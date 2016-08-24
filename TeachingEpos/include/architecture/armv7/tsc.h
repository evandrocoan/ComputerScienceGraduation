// EPOS ARMv7 Time-Stamp Counter Mediator Declarations

#ifndef __armv7_tsc_h
#define __armv7_tsc_h

#include <tsc.h>

__BEGIN_SYS

class ARMv7_TSC: private TSC_Common
{
private:
    static const unsigned int CLOCK = Traits<CPU>::CLOCK;

public:
    using TSC_Common::Hertz;
    using TSC_Common::Time_Stamp;

public:
    ARMv7_TSC() {}

    static Hertz frequency() { return CLOCK; }

    static Time_Stamp time_stamp();
};

__END_SYS

#endif
