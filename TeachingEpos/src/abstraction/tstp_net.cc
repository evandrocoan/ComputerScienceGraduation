// EPOS TSTP Network component Implementation

#include <system/config.h>
#ifndef __no_networking__

#include <tstp_net.h>

__BEGIN_SYS

// Class attributes
TSTPOE * TSTPOE::_networks[];
TSTPOE::Observed TSTPOE::_observed;

TSTPOTM * TSTPOTM::_networks[];
TSTPOTM::Observed TSTPOTM::_observed;

// Methods

__END_SYS

#endif
