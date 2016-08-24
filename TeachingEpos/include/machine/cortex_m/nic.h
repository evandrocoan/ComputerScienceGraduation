// EPOS Cortex-M NIC Mediator Declarations

#ifndef __cortex_m_nic_h
#define __cortex_m_nic_h

#include <ieee802_15_4.h>
#include <tstp_mac.h>
#include <system.h>
#include "emote3_ieee802_15_4.h"
#include "emote3_tstp_mac.h"

__BEGIN_SYS

class Cortex_M_IEEE802_15_4: public Traits<Cortex_M_IEEE802_15_4>::MAC
{
    friend class Cortex_M;

private:
    typedef Traits<Cortex_M_IEEE802_15_4>::NICS NICS;
    static const unsigned int UNITS = NICS::Length;

public:
    typedef Traits<Cortex_M_IEEE802_15_4>::MAC MAC;
    typedef MAC::Observed Observed;
    typedef MAC::Observer Observer;

    template<unsigned int UNIT = 0>
    Cortex_M_IEEE802_15_4(unsigned int u = UNIT) {
        _dev = Meta_NIC<NICS>::Get<UNIT>::Result::get(u);
        db<NIC>(TRC) << "NIC::NIC(u=" << UNIT << ",d=" << _dev << ") => " << this << endl;
    }
    ~Cortex_M_IEEE802_15_4() { _dev = 0; }
    
    Buffer * alloc(NIC * nic, const Address & dst, const Protocol & prot, unsigned int once, unsigned int always, unsigned int payload) {
        return _dev->alloc(nic, dst, prot, once, always, payload);
    }
    int send(Buffer * buf) { return _dev->send(buf); }
    void free(Buffer * buf) { _dev->free(buf); }

    int send(const Address & dst, const Protocol & prot, const void * data, unsigned int size) {
        return _dev->send(dst, prot, data, size); 
    }
    int receive(Address * src, Protocol * prot, void * data, unsigned int size) {
        return _dev->receive(src, prot, data, size); 
    }

    const unsigned int mtu() const { return _dev->mtu(); }
    const Address broadcast() const { return _dev->broadcast(); }
    
    const Address & address() { return _dev->address(); }
    void address(const Address & address) { _dev->address(address); }

    const unsigned int channel() { return _dev->channel(); }
    void channel(unsigned int channel) { _dev->channel(channel); }

    const Statistics & statistics() { return _dev->statistics(); }

    void reset() { _dev->reset(); }

    void attach(Observer * obs, const Protocol & prot) { _dev->attach(obs, prot); }
    void detach(Observer * obs, const Protocol & prot) { _dev->detach(obs, prot); }
    void notify(const Protocol & prot, Buffer * buf) { _dev->notify(prot, buf); }

private:
    static void init();

private:
    Meta_NIC<NICS>::Base * _dev;
};

__END_SYS

#endif
