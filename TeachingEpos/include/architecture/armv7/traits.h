// EPOS ARMv7 Architecture Metainfo
#ifndef __armv7_traits_h
#define __armv7_traits_h

#include <system/config.h>

__BEGIN_SYS

template <> struct Traits<ARMv7>: public Traits<void>
{
    enum {LITTLE, BIG};
    static const unsigned int ENDIANESS         = LITTLE;
    static const unsigned int WORD_SIZE         = 32;
    static const unsigned int CLOCK             = Traits<Build>::MODEL == Traits<Build>::LM3S811 ? 50000000 : 32000000;
    static const bool unaligned_memory_access   = false;
};

template <> struct Traits<ARMv7_MMU>: public Traits<void>
{
};

template <> struct Traits<ARMv7_TSC>: public Traits<void>
{
    static const bool enabled = (Traits<Build>::MACHINE == Traits<Build>::Cortex_A);
};

__END_SYS

#endif
