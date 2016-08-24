// EPOS Network Abstraction Initialization

#include <system/config.h>
#ifndef __no_networking__

#include <network.h>
#include <icmp.h>
#include <udp.h>
#include <tcp.h>
#include <tstp.h>

__BEGIN_SYS

template <int unit>
inline static void call_init()
{
    typedef typename Traits<Network>::NETWORKS::template Get<unit>::Result NET;

    // TODO: unit should be reset for each different NIC
    if(Traits<NET>::enabled)
        NET::init(unit);

    call_init<unit + 1>();
};

template <>
inline void call_init<Traits<Network>::NETWORKS::Length>()
{
};

void Network::init()
{
    db<Init, Network>(TRC) << "Network::init()" << endl;

    call_init<0>();

    // If IP was initialized, initialize also the rest of the stack
    if(Traits<Network>::NETWORKS::Count<IP>::Result) {
        if(Traits<ICMP>::enabled)
            new (SYSTEM) ICMP;
        if(Traits<UDP>::enabled)
            new (SYSTEM) UDP;
        if(Traits<TCP>::enabled)
            new (SYSTEM) TCP;
    }

    // If TSTP's MAC was initialized, initialize also the rest of the stack
    if(Traits<TSTP>::enabled and Traits<Network>::NETWORKS::Count<Traits<TSTP>::MAC>::Result) {
        //if(Traits<TSTP>::Time_Manager != Traits<TSTP>::DISABLED) // TODO: PTS<false> still needs to start the timer and attach itself to the NIC to update origin_time
            new (SYSTEM) TSTP::Time_Manager;
        if(Traits<TSTP>::Security != Traits<TSTP>::DISABLED)
            new (SYSTEM) TSTP::Security;
        if(Traits<TSTP>::Locator != Traits<TSTP>::DISABLED)
            new (SYSTEM) TSTP::Locator;
        if(Traits<TSTP>::Router != Traits<TSTP>::DISABLED)
            new (SYSTEM) TSTP::Router;

        new (SYSTEM) TSTP;
    }

}

__END_SYS

#endif
