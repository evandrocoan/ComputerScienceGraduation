// EPOS eMote3 (Cortex-M3) MCU Initialization

#include <system/config.h>
#include __MODEL_H
#ifdef __emote3_h

#include <ic.h>
#include <nic.h>

__BEGIN_SYS

bool eMote3::_init_clock_done = false;

void eMote3::init()
{
    init_clock();

    // Enable alternate interrupt mapping
    scr(I_MAP) |= I_MAP_ALTMAP;

    // Set the vector table offset (must be 512-byte aligned)
    scs(VTOR) = (Traits<Cortex_M>::SYS_CODE) & ~(1 << 29);
}

void eMote3::init_clock()
{
    // Since the clock is configured in traits and never changes,
    // this needs to be done only once, but this method will be 
    // called at least twice during EPOS' initialization 
    // (in eMote3::uart_config() and Cortex_M::init())
    if(_init_clock_done)
        return;

    // Clock setup
    Reg32 clock_val;
    switch(Traits<CPU>::CLOCK)
    {
        case 32000000: clock_val = 0; break;
        case 16000000: clock_val = 1; break;
        case  8000000: clock_val = 2; break;
        case  4000000: clock_val = 3; break;
        case  2000000: clock_val = 4; break;
        case  1000000: clock_val = 5; break;
        case   500000: clock_val = 6; break;
        case   250000: clock_val = 7; break;
        default: while(1) assert(false);
    }

    // Set pins PD6 and PD7 to enable external oscillator
    {
        const auto pin_bit = 1 << 6;
        gpiod(AFSEL) &= ~pin_bit; // Set pin D6 as software-controlled
        gpiod(DIR) &= ~pin_bit; // Set pin D6 as output
        ioc(PD6_OVER) = ANA;
    }
    {
        const auto pin_bit = 1 << 7;
        gpiod(AFSEL) &= ~pin_bit; // Set pin D7 as software-controlled
        gpiod(DIR) &= ~pin_bit; // Set pin D7 as output
        ioc(PD7_OVER) = ANA;
    }

    Reg32 clock_ctrl = scr(CLOCK_CTRL) & ~(SYS_DIV * 7);
    clock_ctrl |= clock_val * SYS_DIV; // Set system clock rate
    clock_ctrl |= AMP_DET; // Enable AMP detect to make sure XOSC starts correctly    
    clock_ctrl |= OSC_PD; // Power down unused oscillator
    clock_ctrl &= ~OSC; // Select 32Mhz oscillator
    clock_ctrl &= ~OSC32K; // Select 32Khz crystal oscillator

    scr(CLOCK_CTRL) = clock_ctrl; // Write back to register

    // Wait until oscillators stabilize
    while((scr(CLOCK_STA) & (STA_OSC | STA_OSC32K)));

    clock_ctrl = scr(CLOCK_CTRL) & ~(IO_DIV * 7);
    scr(CLOCK_CTRL) = clock_ctrl | (clock_val * IO_DIV); // Set IO clock rate

    //while((scr(CLOCK_STA) & (STA_SYNC_32K))); // Contiki includes this line too for some reason 
    while(!(scr(CLOCK_STA) & (STA_SYNC_32K)));

    _init_clock_done = true;
}

__END_SYS
#endif
