// EPOS PC Mediators Configuration

#ifndef __pc_config_h
#define __pc_config_h

#include <system/meta.h>
#include __APPL_TRAITS_H

#define __CPU_H         __HEADER_ARCH(cpu)
#define __TSC_H         __HEADER_ARCH(tsc)
#define __MMU_H         __HEADER_ARCH(mmu)
#define __FPU_H         __HEADER_ARCH(fpu)
#define __PMU_H         __HEADER_ARCH(pmu)

#define __MACH_H        __HEADER_MACH(machine)
#define __PCI_H         __HEADER_MACH(pci)
#define __IC_H          __HEADER_MACH(ic)
#define __TIMER_H       __HEADER_MACH(timer)
#define __RTC_H         __HEADER_MACH(rtc)
#define __EEPROM_H      __HEADER_MACH(eeprom)
#define __UART_H        __HEADER_MACH(uart)
#define __DISPLAY_H     __HEADER_MACH(display)
#define __KEYBOARD_H    __HEADER_MACH(keyboard)
#define __SCRATCHPAD_H  __HEADER_MACH(scratchpad)
#define __NIC_H         __HEADER_MACH(nic)
#define __FPGA_H        __HEADER_MACH(fpga)

__BEGIN_SYS

typedef IA32            CPU;
typedef IA32_MMU        MMU;
typedef IA32_TSC        TSC;
typedef IA32_FPU        FPU;
typedef IA32_PMU        PMU;

typedef PC              Machine;
typedef PC_PCI          PCI;
typedef PC_IC           IC;
typedef PC_Timer        Timer;
typedef PC_RTC          RTC;
typedef PC_EEPROM       EEPROM;
typedef PC_UART         UART;
typedef void            USB;
typedef IF<Traits<Serial_Display>::enabled, Serial_Display, PC_Display>::Result Display;
typedef IF<Traits<Serial_Keyboard>::enabled, Serial_Keyboard, PC_Keyboard>::Result Keyboard;
typedef PC_Scratchpad   Scratchpad;
typedef PC_Ethernet     NIC;
typedef PC_FPGA         FPGA;
typedef AES<Traits<AES<0>>::KEY_LENGTH> Cipher;

__END_SYS

#endif
