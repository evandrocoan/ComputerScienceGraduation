// EPOS Performance Monitoring Unit Abstraction Declarations

#ifndef __perfmon_h
#define __perfmon_h

#include <pmu.h>

__BEGIN_SYS

class Perf_Mon
{
public:
    typedef PMU::Reg32 Reg32;
    typedef PMU::Reg64 Reg64;
    
public:
    Perf_Mon() { }

	void llc_hit(void) {
		PMU::config(PMU::EVTSEL1, (Intel_Sandy_Bridge_PMU::MEM_LOAD_UOPS_RETIRED_L3_HIT | 
                                  Intel_Sandy_Bridge_PMU::USR | Intel_Sandy_Bridge_PMU::OS |
                                  Intel_Sandy_Bridge_PMU::ENABLE
        ));

		Intel_Sandy_Bridge_PMU::enable_pebs_pmc1();
	}

	Reg64 get_llc_hit(void) {
		return read_pmc1();
	}
    
    void llc_misses(void) {
    	PMU::config(PMU::EVTSEL3, (Intel_Sandy_Bridge_PMU::MEM_LOAD_UOPS_MISC_RETIRED_LLC_MISS | 
                                  Intel_Sandy_Bridge_PMU::USR | Intel_Sandy_Bridge_PMU::OS |
                                  Intel_Sandy_Bridge_PMU::ENABLE
        ));  

		Intel_Sandy_Bridge_PMU::enable_pebs_pmc3();  
	}
    
    Reg64 get_llc_misses(void) {
        return read_pmc3();
    }
   
	void llc_hitm(void) {
		//XSNP_HITM counts the number of retired micro-operations
		//which data soures were Hit Modified responses from shared Last-Level Cache
		PMU::config(PMU::EVTSEL2, (Intel_Sandy_Bridge_PMU::XSNP_HITM | 
                                  Intel_Sandy_Bridge_PMU::USR | Intel_Sandy_Bridge_PMU::OS |
                                  Intel_Sandy_Bridge_PMU::ENABLE
        ));  

		Intel_Sandy_Bridge_PMU::enable_pebs_pmc2();  
	}

	Reg64 get_llc_hitm(void) {
		return read_pmc2();
	}
         
    void instructions_retired(void) {
        Intel_PMU_Version3::enable_fixed_ctr0();
    }
    
    Reg64 get_instructions_retired(void) {
        return Intel_PMU_Version3::rdmsr(PMU::FIXED_CTR0);
    }
    
    void cpu_clk_unhalted_core(void) {
        Intel_PMU_Version3::enable_fixed_ctr1();
    }
    
    Reg64 get_cpu_clk_unhalted_core(void) {
        return Intel_PMU_Version3::rdmsr(PMU::FIXED_CTR1);
    }
    
    void cpu_clk_unhalted_ref(void) {
        Intel_PMU_Version3::enable_fixed_ctr2();
    }
    
    Reg64 get_cpu_clk_unhalted_ref(void) {
        return Intel_PMU_Version3::rdmsr(PMU::FIXED_CTR2);
    }
    

    
public:
  
    void reset_fixed_ctr0(void) {
        Intel_PMU_Version3::disable_fixed_ctr0();
        PMU::reset(PMU::FIXED_CTR0);
        Intel_PMU_Version3::enable_fixed_ctr0();
    }
  
    void reset_fixed_ctr1(void) {
        Intel_PMU_Version3::disable_fixed_ctr1();
        PMU::reset(PMU::FIXED_CTR1);
        Intel_PMU_Version3::enable_fixed_ctr1();
    }
  
    void reset(Reg32 reg) {
        Intel_Core_Micro_PMU::disable(PMU::EVTSEL0);
        PMU::reset(reg + PMU::PMC_BASE_ADDR);
        Intel_Core_Micro_PMU::enable(PMU::EVTSEL0);
    }
    
    void reset_pmc0() {
        PMU::disable(PMU::EVTSEL0);
        PMU::reset(PMU::PMC0 + PMU::PMC_BASE_ADDR);
        PMU::enable(PMU::EVTSEL0);
    }
    
    void reset_pmc1() {
        PMU::disable(PMU::EVTSEL1);
        PMU::reset(PMU::PMC1 + PMU::PMC_BASE_ADDR);
        PMU::enable(PMU::EVTSEL1);
    }
    
    void reset_pmc2() {
        PMU::disable(PMU::EVTSEL2);
        PMU::reset(PMU::PMC2 + PMU::PMC_BASE_ADDR);
        PMU::enable(PMU::EVTSEL2);
    }
    
    void reset_pmc3() {
        PMU::disable(PMU::EVTSEL3);
        PMU::reset(PMU::PMC3 + PMU::PMC_BASE_ADDR);
        PMU::enable(PMU::EVTSEL3);
    }
    
    Reg64 read_pmc0() {
        Reg64 count;
        count = PMU::rdpmc(PMU::PMC0);
        reset_pmc0();
        return count;
    }
    
    Reg64 read_pmc1() {
        Reg64 count;
        count = PMU::rdpmc(PMU::PMC1);
        reset_pmc1();
        return count;
    }
    
    Reg64 read_pmc2() {
        Reg64 count;
        count = PMU::rdpmc(PMU::PMC2);
        reset_pmc2();
        return count;
    }
    
    Reg64 read_pmc3() {
        Reg64 count;
        count = PMU::rdpmc(PMU::PMC3);
        reset_pmc3();
        return count;
    }
    
    Reg64 read() {
          return read_pmc0();
    }
};

__END_SYS

#endif
