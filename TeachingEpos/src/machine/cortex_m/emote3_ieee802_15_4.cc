// EPOS eMote3_IEEE802_15_4 IEEE 802.15.4 NIC Mediator Implementation

#include <system/config.h>
#ifndef __no_networking__

#include <machine/cortex_m/machine.h>
#include <timer.h>
#include "../../../include/machine/cortex_m/emote3_ieee802_15_4.h"
#include <utility/malloc.h>
#include <utility/random.h>
#include <alarm.h>
#include <gpio.h>

__BEGIN_SYS

// Class attributes
eMote3_IEEE802_15_4::Device eMote3_IEEE802_15_4::_devices[UNITS];

// Methods
eMote3_IEEE802_15_4::~eMote3_IEEE802_15_4()
{
    db<eMote3_IEEE802_15_4>(TRC) << "~Radio(unit=" << _unit << ")" << endl;
}

void eMote3_IEEE802_15_4::address(const Address & address)
{
    _address[0] = address[0];
    _address[1] = address[1];
    ffsm(SHORT_ADDR0) = _address[0];
    ffsm(SHORT_ADDR1) = _address[1];
}

void eMote3_IEEE802_15_4::listen()
{
    // Clear interrupts
    sfr(RFIRQF0) = 0;
    sfr(RFIRQF1) = 0;
    // Enable device interrupts
    xreg(RFIRQM0) = INT_FIFOP;
    xreg(RFIRQM1) = 0;
    // Issue the listen command
    rx();
}

void eMote3_IEEE802_15_4::stop_listening()
{
    // Disable device interrupts
    xreg(RFIRQM0) = 0;
    xreg(RFIRQM1) = 0;
    // Issue the OFF command
    off();
}

int eMote3_IEEE802_15_4::send(const Address & dst, const Protocol & prot, const void * data, unsigned int size)
{
    if(size > MTU) {
        return 0;
    }
    if(auto b = alloc(reinterpret_cast<NIC*>(this), dst, prot, 0, 0, size)) {
        b->frame()->data(data);
        return send(b);
    } else {
        return 0;
    }
}


int eMote3_IEEE802_15_4::receive(Address * src, Protocol * prot, void * data, unsigned int size)
{
    db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::receive(s=" << *src << ",p=" << hex << *prot << dec
        << ",d=" << data << ",s=" << size << ") => " << endl;

    // Wait for a received frame and seize it
    for(bool locked = false; !locked; ) {
        locked = _rx_buffer[_rx_cur]->lock();
        if(!locked) ++_rx_cur;
    }

    // Wait for a complete frame to be received
    while(!_rx_done());

    Buffer * buf = _rx_buffer[_rx_cur];

    if(frame_in_rxfifo()) {
        auto b = reinterpret_cast<unsigned char *>(buf->frame());
        auto sz = copy_from_rxfifo(b + 1);
        b[0] = sz;

        // Disassemble the frame
        Frame * frame = buf->frame();
        *src = frame->src();
        *prot = frame->prot();

        // For the upper layers, size will represent the size of frame->data<T>()
        buf->size(buf->frame()->frame_length() - sizeof(Header) - sizeof(CRC) + sizeof(Phy_Header)); // Phy_Header is included in Header, but is already discounted in frame_length

        // Copy the data
        memcpy(data, frame->data<void>(), (buf->size() > size) ? size : buf->size());

        _statistics.rx_packets++;
        _statistics.rx_bytes += buf->size();

        db<eMote3_IEEE802_15_4>(INF) << "eMote3_IEEE802_15_4::receive done" << endl;

        int tmp = buf->size();

        buf->unlock();

        ++_rx_cur %= RX_BUFS;

        return tmp;
    } else {
        buf->unlock();
        return 0;
    }
}


// Allocated buffers must be sent or release IN ORDER as assumed by the Radio
eMote3_IEEE802_15_4::Buffer * eMote3_IEEE802_15_4::alloc(NIC * nic, const Address & dst, const Protocol & prot, unsigned int once, unsigned int always, unsigned int payload)
{
    db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::alloc(s=" << _address << ",d=" << dst << ",p=" << hex << prot << dec << ",on=" << once << ",al=" << always << ",ld=" << payload << ")" << endl;

    int max_data = MTU - always;

    // TODO: replace with division
    unsigned int buffers = 0;
    for(int size = once + payload; size > 0; size -= max_data, buffers++);
    if(buffers > TX_BUFS) {
//    if((payload + once) / max_data > TX_BUFS) {
        db<eMote3_IEEE802_15_4>(WRN) << "eMote3_IEEE802_15_4::alloc: sizeof(Network::Packet::Data) > sizeof(NIC::Frame::Data) * TX_BUFS!" << endl;
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

        // Initialize the buffer and assemble the IEEE 802.15.4 Frame Header
        auto sz = (size > max_data) ? MTU : size + always;
        new (buf) Buffer(nic, sz, _address, dst, prot, sz);
        if(Traits<eMote3_IEEE802_15_4>::ACK and (dst != broadcast()))
            buf->frame()->ack_request(true);

        db<eMote3_IEEE802_15_4>(INF) << "eMote3_IEEE802_15_4::alloc[" << _tx_cur << "]" << endl;

        ++_tx_cur %= TX_BUFS;

        pool.insert(buf->link());
    }

    return pool.head()->object();
}

int eMote3_IEEE802_15_4::send(Buffer * buf)
{
    int size = 0;

    for(Buffer::Element * el = buf->link(); el; el = el->next()) {
        buf = el->object();
        const bool ack = Traits<eMote3_IEEE802_15_4>::ACK and (buf->frame()->dst() != broadcast());

        db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::send(buf=" << buf << ")" << endl;

        // TODO: Memory in the fifos is padded: you can only write one byte every 4bytes.
        // For now, we'll just copy using the RFDATA register
        char * f = reinterpret_cast<char *>(buf->frame());

        sfr(RFST) = ISFLUSHTX; // Clear TXFIFO
        while(xreg(TXFIFOCNT) != 0);

        // First field is length of MAC
        // CRC is inserted by hardware (assuming auto-CRC is enabled)
        const int crc_size = sizeof(CRC);
        for(int i=0; i < f[0] + 1 - crc_size; i++)
            sfr(RFDATA) = f[i];

        // Trigger an immediate send poll
        bool ok = send_and_wait(ack);

        if(ok) {
            db<eMote3_IEEE802_15_4>(INF) << "eMote3_IEEE802_15_4::send done" << endl;
            _statistics.tx_packets++;
            _statistics.tx_bytes += buf->size();
            size += buf->size();
        } else {
            db<eMote3_IEEE802_15_4>(INF) << "eMote3_IEEE802_15_4::send failed!" << endl;
        }

        buf->unlock();
    }

    return size;
}


void eMote3_IEEE802_15_4::free(Buffer * buf)
{
    db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::free(buf=" << buf << ")" << endl;

    for(Buffer::Element * el = buf->link(); el; el = el->next()) {
        buf = el->object();

        _statistics.rx_packets++;
        _statistics.rx_bytes += buf->size();

        // Release the buffer to the OS
        buf->unlock();

        db<eMote3_IEEE802_15_4>(INF) << "eMote3_IEEE802_15_4::free " << buf << endl;
    }
}

void eMote3_IEEE802_15_4::reset()
{
    db<eMote3_IEEE802_15_4>(TRC) << "Radio::reset()" << endl;

    // Reset statistics
    new (&_statistics) Statistics;
}

bool eMote3_IEEE802_15_4::wait_for_ack()
{
    while(!(sfr(RFIRQF1) & INT_TXDONE));
    sfr(RFIRQF1) &= ~INT_TXDONE;

    if(not Traits<eMote3_IEEE802_15_4>::auto_listen) {
        xreg(RFST) = ISRXON;
    }

    bool acked = false;
    User_Timer timer(0, 2, Traits<eMote3_IEEE802_15_4>::ACK_TIMEOUT);
    while(timer.running() and not (acked = (sfr(RFIRQF0) & INT_FIFOP)));

    return acked;
}

bool eMote3_IEEE802_15_4::send_and_wait(bool ack)
{
    bool do_ack = Traits<eMote3_IEEE802_15_4>::ACK and ack;
    Reg32 saved_filter_settings = 0;
    if(do_ack) {
        saved_filter_settings = xreg(FRMFILT1);
        xreg(RFIRQM0) &= ~INT_FIFOP; // Disable FIFOP int. We'll poll the interrupt flag
        xreg(FRMFILT1) = ACCEPT_FT2_ACK; // Accept only ACK frames now
    }

    bool sent = backoff_and_send();

    if(do_ack) {
        bool acked = sent and wait_for_ack();

        for(auto i = 0u; (i < Traits<eMote3_IEEE802_15_4>::RETRANSMISSIONS) and not acked; i++) {
            db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::retransmitting" << endl;
            sent = backoff_and_send();

            acked = sent and wait_for_ack();
        }

        if(acked) {
            sfr(RFIRQF0) &= ~INT_FIFOP; // Clear FIFOP flag
            clear_rxfifo();
        }

        if(not Traits<eMote3_IEEE802_15_4>::auto_listen) {
            xreg(RFST) = ISRFOFF;
        }

        xreg(FRMFILT1) = saved_filter_settings; // Done with ACKs
        xreg(RFIRQM0) |= INT_FIFOP; // Enable FIFOP int
        return acked;
    }
    else if(sent) {
        while(!(sfr(RFIRQF1) & INT_TXDONE));
        sfr(RFIRQF1) &= ~INT_TXDONE;
    }

    return sent;
}

bool eMote3_IEEE802_15_4::backoff_and_send()
{
    bool ret = true;
    if(Traits<eMote3_IEEE802_15_4>::CSMA_CA) {
        start_cca();

        unsigned int two_raised_to_be = 1;
        unsigned int BE;
        for(BE = 0; BE < Traits<eMote3_IEEE802_15_4>::CSMA_CA_MIN_BACKOFF_EXPONENT; BE++) {
            two_raised_to_be *= 2;
        }

        unsigned int trials;
        for(trials = 0u; trials < Traits<eMote3_IEEE802_15_4>::CSMA_CA_MAX_TRANSMISSION_TRIALS; trials++) {
            const auto ubp = Traits<eMote3_IEEE802_15_4>::CSMA_CA_UNIT_BACKOFF_PERIOD;
            auto delay_time = (Random::random() % (two_raised_to_be - 1)) * ubp;
            delay_time = delay_time < ubp ? ubp : delay_time;

            User_Timer t(0, 2, delay_time);
            while(t.running());
            if(tx_if_cca()) {
                break; // Success
            }
            
            if(BE < Traits<eMote3_IEEE802_15_4>::CSMA_CA_MAX_BACKOFF_EXPONENT) {
                BE++;
                two_raised_to_be *= 2;
            }
        }

        end_cca();

        if(trials >= Traits<eMote3_IEEE802_15_4>::CSMA_CA_MAX_TRANSMISSION_TRIALS) {
            db<eMote3_IEEE802_15_4>(WRN) << "eMote3_IEEE802_15_4::backoff_and_send() FAILED" << endl;
            ret = false;
        }
    }
    else {
        tx();
    }

    return ret;
}

void eMote3_IEEE802_15_4::handle_int()
{
    Reg32 irqrf0 = sfr(RFIRQF0);
    Reg32 irqrf1 = sfr(RFIRQF1);

    if(irqrf0 & INT_FIFOP) { // Frame received
        sfr(RFIRQF0) &= ~INT_FIFOP;
        if(frame_in_rxfifo()) {
            Buffer * buf = 0;

            // NIC received a frame in the RXFIFO, so we need to find an unused buffer for it
            for (auto i = 0u; (i < RX_BUFS) and not buf; ++i) {
                if (_rx_buffer[_rx_cur]->lock()) {
                    db<eMote3_IEEE802_15_4>(INF) << "eMote3_IEEE802_15_4::handle_int: found buffer: " << _rx_cur << endl;
                    buf = _rx_buffer[_rx_cur]; // Found a good one
                } else {
                    ++_rx_cur %= RX_BUFS;
                }
            }

            if (not buf) {
                db<eMote3_IEEE802_15_4>(WRN) << "eMote3_IEEE802_15_4::handle_int: no buffers left" << endl;
                db<eMote3_IEEE802_15_4>(WRN) << "eMote3_IEEE802_15_4::handle_int: dropping fifo contents" << endl;
                clear_rxfifo();
            } else {
                // We have a buffer, so we fetch a packet from the fifo
                auto b = reinterpret_cast<unsigned char *>(buf->frame());
                auto sz = copy_from_rxfifo(b + 1);
                b[0] = sz;
                buf->size(buf->frame()->frame_length() - (sizeof(Header) + sizeof(CRC) - sizeof(Phy_Header))); // Phy_Header is included in Header, but is already discounted in frame_length

                auto * frame = buf->frame();

                db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::int:receive(s=" << frame->src() << ",p=" << hex << frame->header()->prot() << dec
                    << ",d=" << frame->data<void>() << ",s=" << buf->size() << ")" << endl;

                db<eMote3_IEEE802_15_4>(INF) << "eMote3_IEEE802_15_4::handle_int[" << _rx_cur << "]" << endl;

                //IC::disable(_irq);
                if(!notify(frame->header()->prot(), buf)) {// No one was waiting for this frame, so let it free for receive()
                    free(buf);
                }
                // TODO: this serialization is much too restrictive. It was done this way for students to play with
                //IC::enable(_irq);
            }
        }
    }
    db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::int: " << endl << "RFIRQF0 = " << hex << irqrf0 << endl;
    //if(irqrf0 & INT_RXMASKZERO) db<eMote3_IEEE802_15_4>(TRC) << "RXMASKZERO" << endl;
    //if(irqrf0 & INT_RXPKTDONE) db<eMote3_IEEE802_15_4>(TRC) << "RXPKTDONE" << endl;
    //if(irqrf0 & INT_FRAME_ACCEPTED) db<eMote3_IEEE802_15_4>(TRC) << "FRAME_ACCEPTED" << endl;
    //if(irqrf0 & INT_SRC_MATCH_FOUND) db<eMote3_IEEE802_15_4>(TRC) << "SRC_MATCH_FOUND" << endl;
    //if(irqrf0 & INT_SRC_MATCH_DONE) db<eMote3_IEEE802_15_4>(TRC) << "SRC_MATCH_DONE" << endl;
    //if(irqrf0 & INT_SFD) db<eMote3_IEEE802_15_4>(TRC) << "SFD" << endl;
    //if(irqrf0 & INT_ACT_UNUSED) db<eMote3_IEEE802_15_4>(TRC) << "ACT_UNUSED" << endl;

    db<eMote3_IEEE802_15_4>(TRC) << "RFIRQF1 = " << hex << irqrf1 << endl;
    //if(irqrf1 & INT_CSP_WAIT) db<eMote3_IEEE802_15_4>(TRC) << "CSP_WAIT" << endl;
    //if(irqrf1 & INT_CSP_STOP) db<eMote3_IEEE802_15_4>(TRC) << "CSP_STOP" << endl;
    //if(irqrf1 & INT_CSP_MANINT) db<eMote3_IEEE802_15_4>(TRC) << "CSP_MANINT" << endl;
    //if(irqrf1 & INT_RFIDLE) db<eMote3_IEEE802_15_4>(TRC) << "RFIDLE" << endl;
    //if(irqrf1 & INT_TXDONE) db<eMote3_IEEE802_15_4>(TRC) << "TXDONE" << endl;
    //if(irqrf1 & INT_TXACKDONE) db<eMote3_IEEE802_15_4>(TRC) << "TXACKDONE" << endl;
}


void eMote3_IEEE802_15_4::int_handler(const IC::Interrupt_Id & interrupt)
{
    eMote3_IEEE802_15_4 * dev = get_by_interrupt(interrupt);

    db<eMote3_IEEE802_15_4>(TRC) << "Radio::int_handler(int=" << interrupt << ",dev=" << dev << ")" << endl;

    if(!dev)
        db<eMote3_IEEE802_15_4>(WRN) << "Radio::int_handler: handler not assigned!" << endl;
    else
        dev->handle_int();
}

__END_SYS

#endif
