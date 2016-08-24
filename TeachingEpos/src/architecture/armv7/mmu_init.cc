// EPOS-- ARMv7 MMU Mediator Initialization

#include <mmu.h>
#include <system.h>

extern "C" void * __data_start;
extern "C" void * _edata;
extern "C" void * __bss_start;
extern "C" void * _end;

__BEGIN_SYS

void ARMv7_MMU::init()
{
    db<Init, ARMv7_MMU>(TRC) << "ARMv7_MMU::init()" << endl;
    
    db<Init, ARMv7_MMU>(INF) << "ARMv7_MMU::init::dat.b=" << &__data_start << ",dat.e=" << &_edata << ",bss.b="
                             << &__bss_start << ",bss.e=" << &_end << endl;

    ARMv7_MMU::free(&_end, Memory_Map<Machine>::SYS_STACK - reinterpret_cast<unsigned int>(&_end));
    //TODO: free this initial stack at Thread::init()
}

__END_SYS

