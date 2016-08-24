// EPOS LM3S811 (Cortex-M3) MCU Mediators Configuration

#ifndef __machine_config_h
#define __machine_config_h

#include <system/meta.h>
#include __APPL_TRAITS_H

#define __CPU_H                 __HEADER_ARCH(cpu)
#define __TSC_H                 __HEADER_ARCH(tsc)
#define __MMU_H                 __HEADER_ARCH(mmu)

#define __MACH_H                __HEADER_MACH(machine)
#define __MODEL_H               __HEADER_MACH(MMOD)
#define __IC_H                  __HEADER_MACH(ic)
#define __TIMER_H               __HEADER_MACH(timer)
#define __RTC_H                 __HEADER_MACH(rtc)
#define __UART_H                __HEADER_MACH(uart)
#define __USB_H                 __HEADER_MACH(usb)
#define __NIC_H                 __HEADER_MACH(nic)
#define __GPIO_H                __HEADER_MACH(gpio)

__BEGIN_SYS

typedef ARMv7                   CPU;
typedef ARMv7_MMU               MMU;
typedef ARMv7_TSC               TSC;

typedef Cortex_M                Machine;
typedef Cortex_M_IC             IC;
typedef Cortex_M_Timer          Timer;
typedef Cortex_M_RTC            RTC;
typedef Cortex_M_UART           UART;
typedef Cortex_M_USB            USB;
typedef IF<Traits<Serial_Display>::enabled, Serial_Display, Cortex_M_Display>::Result Display;
typedef Serial_Keyboard         Keyboard;
typedef Cortex_M_Scratchpad     Scratchpad;
typedef Cortex_M_IEEE802_15_4   NIC;
typedef Cortex_M_GPIO           GPIO;

__END_SYS

#endif
