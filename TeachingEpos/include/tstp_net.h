// EPOS TSTP Network component Declarations

#include <nic.h>
#include <tstp.h>
#include <rtc.h>
#include <timer.h>

#ifndef __tstp_net_h
#define __tstp_net_h

__BEGIN_SYS

// TSTP Over Ethernet
class TSTPOE: private TSTP_Common, private NIC::Observer
{
    friend class System;
    template<int unit> friend void call_init();

public:
    static const unsigned int MTU = 1500;
    static const unsigned int TX_DELAY = 0;

    // Buffers received from the NIC
    typedef NIC::Buffer Buffer;

    // TSTPOE observer/d is not conditinal: all packets are passed to TSTP
    typedef Data_Observer<Buffer> Observer;
    typedef Data_Observed<Buffer> Observed;

protected:
    template<unsigned int UNIT = 0>
    TSTPOE(unsigned int nic = UNIT): _nic(nic) {
        db<TSTPOE>(TRC) << "TSTPOE::TSTPOE(nic=" << &_nic << ") => " << this << endl;

        _nic.attach(this, NIC::TSTP);
    }

public:
    ~TSTPOE() {
        db<TSTPOE>(TRC) << "TSTPOE::~TSTPOE(nic=" << &_nic << ") => " << this << endl;

        _nic.detach(this, NIC::TSTP);
    }

    static Buffer * alloc(unsigned int payload) {
        db<TSTPOE>(TRC) << "TSTPOE::alloc(pl=" << payload << ")" << endl;

        Buffer * buf = nic()->alloc(nic(), NIC::Address::BROADCAST, NIC::TSTP, 0, 0, payload);
        if(buf) {
            buf->is_tx(true);
            buf->is_frame(true);
            buf->trusted(false);
        }
        return buf;
    }

    static int send(Buffer * buf) {
        db<TSTPOE>(TRC) << "TSTPOE::send(buf=" << buf << ")" << endl;

        notify(buf); // Let components insert metadata

        auto h = buf->frame()->data<Header>();
        auto t = TSTP_Timer::now();
        h->last_hop_time(t);
        h->elapsed(t - TSTP_Timer::us_to_ts(buf->origin_time()));
        return nic()->send(buf); // implicitly releases the buffer
    }

    static NIC * nic() { return &(_networks[0]->_nic); }
    static const unsigned int mtu() { return MTU; }

    static void attach(Observer * obs) { _observed.attach(obs); }
    static void detach(Observer * obs) { _observed.detach(obs); }
    static bool notify(Buffer * buf) { return _observed.notify(buf); }

private:
    void update(NIC::Observed * obs, NIC::Protocol prot, Buffer * buf) {
        buf->sfd_time_stamp(TSTP_Timer::now());
        db<TSTPOE>(TRC) << "TSTPOE::update(obs=" << obs << ",prot=" << hex << prot << dec << ",buf=" << buf << ")" << endl;
        buf->nic(&_nic);
        buf->destined_to_me(true);
        buf->rssi(0);
        buf->is_rx(true);
        buf->is_frame(true);
        buf->trusted(false);
        notify(buf);
    }

    static void init(unsigned int unit) {
         db<Init, TSTP>(TRC) << "TSTPOE::init(u=" << unit << ")" << endl;

         _networks[unit] = new (SYSTEM) TSTPOE(unit);
     }

protected:
    NIC _nic;

    static TSTPOE * _networks[Traits<NIC>::UNITS];
    static Observed _observed; // shared by all TSTPOE instances, so the default for binding on a unit is for all NICs
};

// TSTP Over TSTP MAC
class TSTPOTM: private TSTP_Common, private NIC::Observer
{
    friend class System;
    template<int unit> friend void call_init();

public:
    static const unsigned int MTU = 127;
    static const unsigned int TX_DELAY = 391; // TODO: confirm this value. TODO: this is machine-dependent

    // Buffers received from the NIC
    typedef NIC::Buffer Buffer;

    // TSTPOTM observer/d is not conditinal: all packets are passed to TSTP
    typedef Data_Observer<Buffer> Observer;
    typedef Data_Observed<Buffer> Observed;

protected:
    template<unsigned int UNIT = 0>
    TSTPOTM(unsigned int nic = UNIT): _nic(nic) {
        db<TSTPOTM>(TRC) << "TSTPOTM::TSTPOTM(nic=" << &_nic << ") => " << this << endl;

        _nic.attach(this, NIC::TSTP);
    }

public:
    ~TSTPOTM() {
        db<TSTPOTM>(TRC) << "TSTPOTM::~TSTPOTM(nic=" << &_nic << ") => " << this << endl;

        _nic.detach(this, NIC::TSTP);
    }

    static Buffer * alloc(unsigned int payload) {
        db<TSTPOTM>(TRC) << "TSTPOTM::alloc(pl=" << payload << ")" << endl;

        return nic()->alloc(nic(), NIC::Address::BROADCAST, NIC::TSTP, 0, 0, payload);
    }

    static int send(Buffer * buf) {
        db<TSTPOTM>(TRC) << "TSTPOTM::send(buf=" << buf << ")" << endl;

        return nic()->send(buf); // implicitly releases the buffer
    }

    static NIC * nic() { return &(_networks[0]->_nic); }
    static const unsigned int mtu() { return MTU; }

    static void attach(Observer * obs) { _observed.attach(obs); }
    static void detach(Observer * obs) { _observed.detach(obs); }
    static bool notify(Buffer * buf) { return _observed.notify(buf); }

private:
    void update(NIC::Observed * obs, NIC::Protocol prot, Buffer * buf) {
        buf->nic(&_nic);
        notify(buf);
    }

    static void init(unsigned int unit) {
         db<Init, TSTP>(TRC) << "TSTPOTM::init(u=" << unit << ")" << endl;

         _networks[unit] = new (SYSTEM) TSTPOTM(unit);
     }

protected:
    NIC _nic;

    static TSTPOTM * _networks[Traits<NIC>::UNITS];
    static Observed _observed; // shared by all TSTPOTM instances, so the default for binding on a unit is for all NICs
};
__END_SYS

#endif
