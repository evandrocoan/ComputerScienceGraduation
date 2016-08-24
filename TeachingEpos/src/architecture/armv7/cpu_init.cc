// EPOS ARMv7 CPU Mediator Initialization

#include <cpu.h>
#include <mmu.h>
#include <system.h>
#include <system/info.h>

extern "C" { void __epos_library_app_entry(void); }

__BEGIN_SYS

void ARMv7::init()
{
    db<Init, CPU>(TRC) << "ARMv7_CPU::init()" << endl;

    if(Traits<MMU>::enabled)
        MMU::init();
    else
        db<Init, MMU>(WRN) << "MMU is disabled!" << endl;

//    if(Traits<TSC>::enabled)
//        TSC::init();
}

__END_SYS
