// EPOS LM3S811 (Cortex-M3) MCU Initialization

#include <system/config.h>
#include __MODEL_H
#ifdef __lm3s811_h

__BEGIN_SYS

void LM3S811::init()
{
    db<Init, Cortex_M>(TRC) << "LM3S811::init()" << endl;

    // Initialize the clock
    CPU::Reg32 rcc = scr(RCC);

    // bypass PLL and system clock divider while initializing
    rcc |= RCC_BYPASS;
    rcc &= ~RCC_USESYSDIV;
    scr(RCC) = rcc;

    // select the crystal value and oscillator source
    rcc &= ~RCC_XTAL_8192;
    rcc |= RCC_XTAL_6000;
    rcc |= RCC_MOSC;

    // activate PLL by clearing PWRDN and OEN
    rcc &= ~RCC_PWRDN;
    rcc &= ~RCC_OEN;

    // set the desired system divider and the USESYSDIV bit
    rcc |= RCC_SYSDIV_4;
    rcc |= RCC_USESYSDIV;
    scr(RCC) = rcc;

    // wait for the PLL to lock by polling PLLLRIS
    while(!(scr(RIS) & RIS_PLLLRIS));

    // enable use of PLL by clearing BYPASS
    rcc &= ~RCC_BYPASS;
    scr(RCC) = rcc;
}

__END_SYS

#endif
