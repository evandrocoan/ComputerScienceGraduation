// EPOS Cortex_M Mediator Declarations

#ifndef __cortex_m_h
#define __cortex_m_h

#include <utility/list.h>
#include <cpu.h>
#include <mmu.h>
#include <tsc.h>
#include <machine.h>
#include <rtc.h>
#include __MODEL_H
#include "info.h"
#include "memory_map.h"
#include "ic.h"

__BEGIN_SYS

class Cortex_M: private Machine_Common, private Cortex_M_Model
{
    friend class Init_System;

public:
    Cortex_M() {}

    static void delay(const RTC::Microsecond & time) {
//        eMote3_GPTM g(3, time_microseconds);
//        g.enable();
//        while(g.running());
    }

    static void panic();
    static void reboot();
    static void poweroff() { reboot(); }

    static unsigned int n_cpus() { return 1; }
    static unsigned int cpu_id() { return 0; }

    typedef Cortex_M_Model::ID ID;
    using Cortex_M_Model::id;

    static void smp_barrier() {};
    static void smp_init(unsigned int) {};
//
//    using Cortex_M_Model::uart_config;
//    using Cortex_M_Model::uart_enable;
//    using Cortex_M_Model::uart_disable;
//
//    using Cortex_M_Model::usb_config;
//    using Cortex_M_Model::usb_enable;
//    using Cortex_M_Model::usb_disable;
//
//    using Cortex_M_Model::gpio_pull_up;
//    using Cortex_M_Model::gpio_pull_down;
//
//    using Cortex_M_Model::radio_enable;
//    using Cortex_M_Model::radio_disable;

private:
    static void init();
};

__END_SYS

#ifdef __TIMER_H
#include __TIMER_H
#endif
#ifdef __RTC_H
#include __RTC_H
#endif
#ifdef __UART_H
#include __UART_H
#endif
#ifdef __USB_H
#include __USB_H
#endif
#ifdef __DISPLAY_H
#include __DISPLAY_H
#endif
#ifdef __GPIO_H
#include __GPIO_H
#endif
#ifdef __NIC_H
#include __NIC_H
#endif

#endif
