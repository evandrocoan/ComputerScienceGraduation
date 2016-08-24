// EPOS EPOSMoteIII (Cortex-M4) Mediators Configuration

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
#define __EEPROM_H              __HEADER_MACH(eeprom)
#define __UART_H                __HEADER_MACH(uart)
#define __NIC_H                 __HEADER_MACH(nic)
#define __USB_H                 __HEADER_MACH(usb)
//#define __SPI_H               __HEADER_MACH(spi)
#define __I2C_H                 __HEADER_MACH(i2c)
#define __GPIO_H                __HEADER_MACH(gpio)
//#define __ADC_H               __HEADER_MACH(adc)
//#define __FLASH_H             __HEADER_MACH(flash)

__BEGIN_SYS

typedef ARMv7                   CPU;
typedef ARMv7_MMU               MMU;
typedef ARMv7_TSC               TSC;

typedef Cortex_M                Machine;
typedef Cortex_M_IC             IC;
typedef Cortex_M_Timer          Timer;
typedef Cortex_M_RTC            RTC;
typedef Cortex_M_EEPROM         EEPROM;
typedef Cortex_M_UART           UART;
typedef Cortex_M_USB            USB;
typedef IF<Traits<Serial_Display>::enabled, Serial_Display, Cortex_M_Display>::Result Display;
typedef Serial_Keyboard         Keyboard;
typedef Cortex_M_Scratchpad     Scratchpad;
typedef Cortex_M_IEEE802_15_4   NIC;
typedef Cortex_M_GPIO           GPIO;
typedef Cortex_M_I2C            I2C;
typedef AES<Traits<AES<0>>::KEY_LENGTH> Cipher;

__END_SYS

#endif
