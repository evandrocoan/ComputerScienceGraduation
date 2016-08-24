// EPOS Cortex_M NIC Mediator Initialization

#include <machine/cortex_m/nic.h>

__BEGIN_SYS

template<int unit>
inline static void call_init()
{
    typedef typename Traits<Cortex_M_IEEE802_15_4>::NICS::template Get<unit>::Result NIC;

    // TODO: unit should be reset for each different NIC
    if(Traits<NIC>::enabled)
        NIC::init(unit);

    call_init<unit + 1>();
};

template<>
inline void call_init<Traits<Cortex_M_IEEE802_15_4>::NICS::Length>()
{
};

void Cortex_M_IEEE802_15_4::init()
{
    call_init<0>();
}

__END_SYS
