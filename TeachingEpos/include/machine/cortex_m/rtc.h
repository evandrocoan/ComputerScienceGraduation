#ifndef __cortex_m_rtc_h
#define __cortex_m_rtc_h

#include <rtc.h>

__BEGIN_SYS

class Cortex_M_RTC: public RTC_Common
{
public:
    Cortex_M_RTC() {}

    static Date date();
    static void date(const Date & d);

    static Second seconds_since_epoch() { return 0; }
};

__END_SYS

#endif
