// EPOS eMote3_TSTP_MAC IEEE 802.15.4 NIC Mediator Implementation

#include <system/config.h>
#ifndef __no_networking__

#include <machine/cortex_m/machine.h>
#include "../../../include/machine/cortex_m/emote3_tstp_mac.h"
#include <utility/malloc.h>
#include <utility/random.h>
#include <alarm.h>
#include <gpio.h>
#include <timer.h>
#include <tstp.h>

__BEGIN_SYS

// Class attributes
eMote3_TSTP_MAC::Device eMote3_TSTP_MAC::_devices[UNITS];
eMote3_TSTP_MAC::State_Handler eMote3_TSTP_MAC::_scheduled_state;
eMote3_TSTP_MAC * eMote3_TSTP_MAC::_timer_int_requester;

// Methods
eMote3_TSTP_MAC::~eMote3_TSTP_MAC() { db<eMote3_TSTP_MAC>(TRC) << "~Radio(unit=" << _unit << ")" << endl; }

void eMote3_TSTP_MAC::next_state(const State_Handler & s, const Time & when) 
{
    _scheduled_state = s;
    _timer_int_requester = this;
    TSTP_Timer::interrupt(when, _scheduled_state);
}

int eMote3_TSTP_MAC::send(const Address & dst, const Protocol & prot, const void * data, unsigned int size)
{
    if(auto b = alloc(reinterpret_cast<NIC*>(this), dst, prot, 0, 0, size)) {
        return send(b);
    } else {
        return 0;
    }
}

eMote3_TSTP_MAC::Buffer * eMote3_TSTP_MAC::alloc(NIC * nic, const Address & dst, const Protocol & prot, unsigned int once, unsigned int always, unsigned int payload)
{
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::alloc(d=" << dst << ",p=" << hex << prot << dec << ",on=" << once << ",al=" << always << ",ld=" << payload << ")" << endl;

    // TSTP_MAC does not support fragmentation
    int max_data = MTU - always;

    unsigned int buffers = 0;
    for(int size = once + payload; size > 0; size -= max_data, buffers++);
    if(buffers > TX_BUFS) {
        db<eMote3_TSTP_MAC>(WRN) << "eMote3_TSTP_MAC::alloc: sizeof(Network::Packet::Data) > sizeof(NIC::Frame::Data) * TX_BUFS!" << endl;
        return 0;
    }

    Buffer::List pool;

    // Calculate how many frames are needed to hold the transport PDU and allocate enough buffers
    for(int size = once + payload; size > 0; size -= max_data) {
        // Wait for the next buffer to become free and seize it
        for(bool locked = false; !locked; ) {
            locked = _tx_buffer[_tx_cur]->lock();
            if(!locked) ++_tx_cur %= TX_BUFS;
        }

        Buffer * buf = _tx_buffer[_tx_cur];

        db<eMote3_TSTP_MAC>(INF) << "eMote3_TSTP_MAC::alloc[" << _tx_cur << "]" << endl;

        // Initialize the buffer
        auto sz = once + always + payload;
        buf->size(sz);
        buf->nic(nic);
        buf->id(Random::random() & 0x7fff);// TODO
        buf->sfd_time_stamp(TSTP_Timer::now());
        buf->is_tx(true);
        buf->is_frame(true);
        buf->trusted(false);

        ++_tx_cur %= TX_BUFS;

        pool.insert(buf->link());
    }

    return pool.head()->object();
}

eMote3_TSTP_MAC::Buffer * eMote3_TSTP_MAC::alloc_mf(bool all_listen, const Frame_ID & id, const Distance & hint)
{
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::alloc_mf(al=" << all_listen << ",id=" << id << ")" << endl;

    // Wait for the next buffer to become free and seize it
    for(bool locked = false; !locked; ) {
        locked = _tx_buffer[_tx_cur]->lock();
        if(!locked) ++_tx_cur %= TX_BUFS;
    }

    Buffer * buf = _tx_buffer[_tx_cur];

    // Initialize the buffer
    buf->nic(reinterpret_cast<NIC*>(this));
    buf->size(sizeof(Microframe));
    auto mf = new (buf->frame()) Microframe(all_listen, id, N_MICROFRAMES, hint);

    buf->id(mf->id());
    buf->sfd_time_stamp(TSTP_Timer::now());
    buf->is_tx(true);
    buf->is_microframe(true);

    db<eMote3_TSTP_MAC>(INF) << "eMote3_TSTP_MAC::alloc_mf[" << _tx_cur << "]" << endl;

    ++_tx_cur %= TX_BUFS;

    return buf;
}

int eMote3_TSTP_MAC::send(Buffer * buf)
{
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::send(buf=" << buf << ")" << endl;

    _tx_schedule.insert(buf->link2());
    return buf->size();
}

void eMote3_TSTP_MAC::free(Buffer * buf)
{
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::free(buf=" << buf << ")" << endl;

    for(Buffer::Element * el = buf->link(); el; el = el->next()) {
        buf = el->object();

        if(_tx_pending == buf)
            _tx_pending = 0;
        else if(buf->is_tx() and buf->is_frame()) {
            _tx_schedule.remove(buf->link2());
        }

        // Release the buffer to the OS
        buf->unlock();

        db<eMote3_TSTP_MAC>(INF) << "eMote3_TSTP_MAC::free " << buf << endl;
    }
}

void eMote3_TSTP_MAC::reset()
{
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::reset()" << endl;

    // Reset statistics
    new (&_statistics) Statistics;
}

void eMote3_TSTP_MAC::handle_int()
{
    Reg32 irqrf0 = sfr(RFIRQF0);

    if(irqrf0 & INT_FIFOP) { // Frame received
        // Fetch possibly time-sensitive values
        auto sfd = TSTP_Timer::sfd();
        char rssi = xreg(RSSI);
        sfr(RFIRQF0) &= ~INT_FIFOP;
        if(frame_in_rxfifo()) {
            Buffer * buf = 0;

            // NIC received a frame in the RXFIFO, so we need to find an unused buffer for it
            for (auto i = 0u; (i < RX_BUFS) and not buf; ++i) {
                if (_rx_buffer[_rx_cur]->lock()) {
                    buf = _rx_buffer[_rx_cur]; // Found a good one
                } else {
                    ++_rx_cur %= RX_BUFS;
                }
            }

            if (not buf) {
                db<eMote3_TSTP_MAC>(WRN) << "eMote3_TSTP_MAC::handle_int: no buffers left, dropping fifo contents" << endl;
                clear_rxfifo();
            } else {
                // We have a buffer, so we fetch a packet from the fifo
                auto f = reinterpret_cast<unsigned char *>(buf->frame());
                auto sz = copy_from_rxfifo(f);
                buf->size(sz);// - sizeof(CRC)); // CRC Disabled for Multimedia MAC
                buf->sfd_time_stamp(sfd);
                buf->rssi(rssi);

                db<eMote3_TSTP_MAC>(INF) << "eMote3_TSTP_MAC::handle_int[" << _rx_cur << "] => " << buf << endl;

                buf->is_rx(true);
                buf->trusted(false);
                if(sz == sizeof(Microframe)) {
                    if(_rx_state == RX_MF) {
                        process_mf(buf);
                    } else {
                        free(buf);
                    }
                } else {
                    if(_rx_state == RX_DATA) {
                        process_data(buf);
                    } else {
                        free(buf);
                    }
                }
            }
        }
    }

    //db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::int: RFIRQF0 = " << hex << irqrf0 << endl;
}


void eMote3_TSTP_MAC::int_handler(const IC::Interrupt_Id & interrupt)
{
    eMote3_TSTP_MAC * dev = get_by_interrupt(interrupt);

    if(!dev)
        db<eMote3_TSTP_MAC>(WRN) << "eMote3_TSTP_MAC::int_handler: handler not assigned for interrupt " << interrupt << "!" << endl;
    else
        dev->handle_int();
}

void eMote3_TSTP_MAC::process_mf(Buffer * buf)
{
    //_rx_pin.clear();
    //_tx_pin.clear();
    off();
    TSTP_Timer::cancel_interrupt();

    buf->is_microframe(true);

    Microframe * mf = buf->frame()->data<Microframe>();

    long long data_time = buf->sfd_time_stamp() + (static_cast<long long>(mf->count()) + 1ll) * (_mf_period);

    if(_tx_pending and (_tx_pending->id() == mf->id())) {
        free(_tx_pending);
    } else {
        for(auto el = _tx_schedule.head(); el; el = el->next()) {
            if(el->object()->id() == mf->id()) {
                free(el->object());
                break;
            }
        }
    }

    notify(PROTOCOL::TSTP, buf);

    if(buf->relevant()) {
        _receiving_data_id = mf->id();
        free(buf);
        next_state(&trigger_rx_data, data_time - TSTP_Timer::us_to_ts(DATA_LISTEN_MARGIN));
    } else {
        db<eMote3_TSTP_MAC>(INF) << "eMote3_TSTP_MAC::Irrelevant Microframe dropped" << endl;
        free(buf);
        next_state(&trigger_check_tx_schedule, data_time + TSTP_Timer::us_to_ts(SLEEP_PERIOD));
    }
}

void eMote3_TSTP_MAC::process_data(Buffer * buf)
{
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::process_data(buf=" << buf << ")" << endl;

    buf->is_frame(true);
    buf->id(_receiving_data_id);
    notify(PROTOCOL::TSTP, buf);
    free(buf);
    check_tx_schedule();
}

// TSTP MAC State machine
void eMote3_TSTP_MAC::check_tx_schedule()
{
    //db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::check_tx_schedule()" << endl;
    //_rx_pin.clear();
    //_tx_pin.clear();
    lock();
    off();
    auto t = TSTP_Timer::now();
    if(_tx_pending and (TSTP_Timer::us_to_ts(_tx_pending->deadline()) < t)) {
        free(_tx_pending); // sets _tx_pending = 0
    }

    Buffer::Element * head;
    while((not _tx_pending) and (head = _tx_schedule.remove_head())) {
        Buffer * buf = head->object();
        _tx_pending = buf; // it's important here to make _tx_pending == buf before calling free()
        if(TSTP_Timer::us_to_ts(buf->deadline()) <= t) {
            free(buf); // sets _tx_pending = 0
        }
    }

    if(_tx_pending) {
        notify(PROTOCOL::TSTP, _tx_pending);
        if(_tx_pending->destined_to_me()) { // just ACK'ing
            unlock();
            cca();
        } else {
            next_state(&trigger_cca, t + TSTP_Timer::us_to_ts(_tx_pending->offset()));
            unlock();
        }
    } else {
        next_state(&trigger_rx_mf, t + TSTP_Timer::us_to_ts(SLEEP_PERIOD));
        unlock();
    }
}

void eMote3_TSTP_MAC::cca() 
{
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::cca()" << endl;
    auto t0 = TSTP_Timer::now();
    const auto limit = TSTP_Timer::us_to_ts(CCA_TIME);
    bool channel_free = true;
    start_cca();
    while((not cca_valid()) and ((TSTP_Timer::now() - t0) < limit));
    while(channel_free and ((TSTP_Timer::now() - t0) < limit)) {
        channel_free = eMote3_IEEE802_15_4_PHY::cca();
    }
    end_cca();

    if(channel_free) {
        prepare_tx_mf();
    } else {
        rx_mf();
    }
}

void eMote3_TSTP_MAC::prepare_tx_mf() 
{
    //_tx_pin.clear();
    //_rx_pin.clear();
    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::prepare_tx_mf()" << endl;
    auto type = _tx_pending->frame()->header()->type();
    if(type == INTEREST) {
        db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::prepare_tx_mf() : interest message" << endl;
        _sending_microframe = alloc_mf(!_tx_pending->destined_to_me(), _tx_pending->id(), _tx_pending->my_distance());
    }
    else {
        db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::prepare_tx_mf() : response message" << endl;
        _sending_microframe = alloc_mf(false, _tx_pending->id(), _tx_pending->my_distance());
    }
    // TODO: Other message types

    clear_txfifo();
    _mf_time = TSTP_Timer::now();
    tx_mf();
}

void eMote3_TSTP_MAC::tx_mf()
{
    _mf_time += _mf_period;
    if(_sending_microframe->frame()->data<Microframe>()->dec_count() > 1) {
        next_state(&trigger_tx_mf, _mf_time);
    } else {
        next_state(&trigger_tx_data, _mf_time);
    }

    setup_tx(reinterpret_cast<char *>(_sending_microframe->frame()->data<Microframe>()), sizeof(Microframe)); //- sizeof(CRC)); // CRC Disabled for Multimedia MAC
    //_tx_pin.set();
    tx();
    while(not tx_ok());
    clear_txfifo();
    //_tx_pin.clear();
}

void eMote3_TSTP_MAC::tx_data()
{
    if(_tx_pending->destined_to_me()) { // just ACK'ing, no need to actually send data
        off();
        free(_tx_pending);
    } else {
        lock();
        auto hdr = _tx_pending->frame()->header();
        const auto sz = _tx_pending->size();
        auto f = reinterpret_cast<char *>(_tx_pending->frame());

        // TODO: Measure how long the next lines take to execute
        auto ts = TSTP_Timer::now();
        hdr->last_hop_time(ts);
        hdr->elapsed(hdr->elapsed() + TSTP_Timer::ts_to_us(ts - _tx_pending->sfd_time_stamp()));
        setup_tx(f, sz);
        //_tx_pin.set(true);
        tx();
        while(not tx_ok());
        //_tx_pin.set(false);
        clear_txfifo();
        unlock();

        off();
    }

    free(_sending_microframe);

    next_state(&trigger_rx_mf, TSTP_Timer::now() + TSTP_Timer::us_to_ts(SLEEP_PERIOD));

    db<eMote3_TSTP_MAC>(TRC) << "tx_data() done" << endl;
}

void eMote3_TSTP_MAC::rx_mf() 
{
    _rx_state = RX_MF; 
    next_state(&trigger_check_tx_schedule, TSTP_Timer::now() + TSTP_Timer::us_to_ts(RX_MF_TIMEOUT));
    //_rx_pin.set();
    rx(); 
}

void eMote3_TSTP_MAC::rx_data() 
{
    _rx_state = RX_DATA; 
    next_state(&trigger_check_tx_schedule, TSTP_Timer::now() + TSTP_Timer::us_to_ts(RX_DATA_TIMEOUT));
    //_rx_pin.set();
    rx(); 
}

__END_SYS

#endif
