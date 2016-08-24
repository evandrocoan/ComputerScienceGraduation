// EPOS eMote3 Dedicated MAC Timer Mediator Implementation

#include <machine/cortex_m/emote3_mac_timer.h>

__BEGIN_SYS

// Class attributes
eMote3_MAC_Timer::Interrupt_Handler eMote3_MAC_Timer::_user_handler;
CPU::Reg32 eMote3_MAC_Timer::_overflow_count_overflow;
CPU::Reg32 eMote3_MAC_Timer::_int_overflow_count_overflow;

__END_SYS
