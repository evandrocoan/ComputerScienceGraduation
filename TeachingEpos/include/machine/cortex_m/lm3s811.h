// EPOS LM3S811 (Cortex-M3) MCU Mediator Declarations

#ifndef __lm3s811_h
#define __lm3s811_h

#include <cpu.h>

__BEGIN_SYS

class LM3S811
{
protected:
    typedef CPU::Reg32 Reg32;
    typedef CPU::Log_Addr Log_Addr;

public:
    static const unsigned int IRQS = 30;
    static const unsigned int TIMERS = 3;
    static const unsigned int GPIO_PORTS = 5;
    static const bool supports_gpio_power_up = false;

    // Base address for memory-mapped System Control Registers
    enum {
        UART0_BASE      = 0x4000c000,
        UART1_BASE      = 0x4000d000,
        SCR_BASE        = 0x400fe000
    };

    // System Control Registers offsets
    enum {                              // Description                                          Type    Value after reset
        DID0            = 0x000,        // Device Identification 0                              RO      -
        DID1            = 0x004,        // Device Identification 1                              RO      -
        DC0             = 0x008,        // Device Capabilities 0                                RO      0x001f001f
        DC1             = 0x010,        // Device Capabilities 1                                RO      0x001132bf
        DC2             = 0x014,        // Device Capabilities 2                                RO      0x01071013
        DC3             = 0x018,        // Device Capabilities 3                                RO      0xbf0f01ff
        DC4             = 0x01c,        // Device Capabilities 4                                RO      0x0000001f
        PBORCTL         = 0x030,        // Power-On and Brown-Out Reset Control                 R/W     0x00007ffd
        LDOPCTL         = 0x034,        // LDO Power Control                                    R/W     0x00000000
        SRCR0           = 0x040,        // Software Reset Control 0                             R/W     0x00000000
        SRCR1           = 0x044,        // Software Reset Control 1                             R/W     0x00000000
        SRCR2           = 0x048,        // Software Reset Control 2                             R/W     0x00000000
        RIS             = 0x050,        // Raw Interrupt Status                                 RO      0x00000000
        IMC             = 0x054,        // Interrupt Mask Control                               R/W     0x00000000
        MISC            = 0x058,        // Masked Interrupt Status and Clear                    R/W1C   0x00000000
        RESC            = 0x05c,        // Reset Cause  R/W     -
        RCC             = 0x060,        // Run-Mode Clock Configuration                         R/W     0x078e3ac0
        PLLCFG          = 0x064,        // XTAL to PLL Translation                              RO      -
        RCGC0           = 0x100,        // Run Mode Clock Gating Control Register 0             R/W     0x00000040
        RCGC1           = 0x104,        // Run Mode Clock Gating Control Register 1             R/W     0x00000000
        RCGC2           = 0x108,        // Run Mode Clock Gating Control Register 2             R/W     0x00000000
        SCGC0           = 0x110,        // Sleep Mode Clock Gating Control Register 0           R/W     0x00000040
        SCGC1           = 0x114,        // Sleep Mode Clock Gating Control Register 1           R/W     0x00000000
        SCGC2           = 0x118,        // Sleep Mode Clock Gating Control Register 2           R/W     0x00000000
        DCGC0           = 0x120,        // Deep Sleep Mode Clock Gating Control Register 0      R/W     0x00000040
        DCGC1           = 0x124,        // Deep Sleep Mode Clock Gating Control Register 1      R/W     0x00000000
        DCGC2           = 0x128,        // Deep Sleep Mode Clock Gating Control Register 2      R/W     0x00000000
        DSLPCLKCFG      = 0x144,        // Sleep Clock Configuration                            R/W     0x07800000
        CLKVCLR         = 0x150,        // Clock Verification Clear                             R/W     0x00000000
        LDOARST         = 0x160         // Allow Unregulated LDO to Reset the Part              R/W     0x00000000
    };

    // Useful Bits in the Run Mode Clock Gating Control Register 0
    enum RCGC0 {                        // Description                  Type    Value after reset
        RCGC0_WDT       = 1 <<  3,      // Watch Dog Timer              r/w     0
        RCGC0_ADC_125K  = 0 <<  8,      // ADC Max Speed = 125K samp/s  r/w     0
        RCGC0_ADC_250K  = 1 <<  8,      // ADC Max Speed = 250K samp/s  r/w     0
        RCGC0_ADC_500K  = 2 <<  8,      // ADC Max Speed = 500K samp/s  r/w     0
        RCGC0_ADC       = 1 << 16,      // ADC                          r/w     0
        RCGC0_PWM       = 1 << 20       // PWM                          r/w     0
    };

    // Useful Bits in the Run Mode Clock Gating Control Register 1
    enum RCGC1 {                        // Description                  Type    Value after reset
        RCGC1_UART0     = 1 <<  0,      // UART0                        r/w     0
        RCGC1_UART1     = 1 <<  1,      // UART1                        r/w     0
        RCGC1_SSI       = 1 <<  4,      // Synchronous Serial Interface r/w     0
        RCGC1_I2C       = 1 << 12,      // I2C                          r/w     0
        RCGC1_TIMER0    = 1 << 16,      // Timer 2                      r/w     0
        RCGC1_TIMER1    = 1 << 17,      // Timer 2                      r/w     0
        RCGC1_TIMER2    = 1 << 18,      // Timer 2                      r/w     0
        RCGC1_COMP0     = 1 << 24       // Analog Comparator 0          r/w     0
    };

    // Useful Bits in the Run Mode Clock Gating Control Register 2
    enum RCGC2 {                        // Description                  Type    Value after reset
        RCGC2_GPIOA     = 1 <<  0,      // GPIOA                        r/w     0
        RCGC2_GPIOB     = 1 <<  1,      // GPIOB                        r/w     0
        RCGC2_GPIOC     = 1 <<  2,      // GPIOC                        r/w     0
        RCGC2_GPIOD     = 1 <<  3,      // GPIOD                        r/w     0
        RCGC2_GPIOE     = 1 <<  4       // GPIOE                        r/w     0
    };

    // Useful Bits in the Run-Mode Clock Configuration
    enum RCC {                          // Description                                          Type    Value after reset
        RCC_MOSCDIS     = 1 <<  0,      // Main Oscillator Disable                              r/w     0
        RCC_IOSCDIS     = 1 <<  1,      // Internal Oscillator Disable                          r/w     0
        RCC_MOSCVER     = 1 <<  2,      // Main Oscillator Verification Timer                   r/w     0
        RCC_IOSCVER     = 1 <<  3,      // Internal Oscillator Verification Timer               r/w     0
        RCC_OSCSRC      = 0 <<  4,      // Oscillator Source                                    r/w     0
        RCC_MOSC        = 0 <<  4,      // Oscillator Source = Main oscillator                  r/w     0
        RCC_IOSC        = 1 <<  4,      // Oscillator Source = Internal oscillator              r/w     0
        RCC_IOSC4       = 2 <<  4,      // Oscillator Source = Internal oscillator / 4          r/w     0
        RCC_XTAL        = 0x0 << 6,     // Crystal Frequency                                    r/w     0xb
        RCC_XTAL_1000   = 0x0 << 6,     // Crystal Frequency = 1 MHz (no PLL)
        RCC_XTAL_1843   = 0x1 << 6,     // Crystal Frequency = 1.8432 MHz (no PLL)
        RCC_XTAL_2000   = 0x2 << 6,     // Crystal Frequency = 2 MHz (no PLL)
        RCC_XTAL_2457   = 0x3 << 6,     // Crystal Frequency = 2.4576 MHz (no PLL)
        RCC_XTAL_3579   = 0x4 << 6,     // Crystal Frequency = 3.579545 MHz
        RCC_XTAL_3686   = 0x5 << 6,     // Crystal Frequency = 3.6864 MHz
        RCC_XTAL_4000   = 0x6 << 6,     // Crystal Frequency = 4 MHz
        RCC_XTAL_4096   = 0x7 << 6,     // Crystal Frequency = 4.096 MHz
        RCC_XTAL_4915   = 0x8 << 6,     // Crystal Frequency = 4.9152 MHz
        RCC_XTAL_5000   = 0x9 << 6,     // Crystal Frequency = 5 MHz
        RCC_XTAL_5120   = 0xa << 6,     // Crystal Frequency = 5.12 MHz
        RCC_XTAL_6000   = 0xb << 6,     // Crystal Frequency = 6 MHz
        RCC_XTAL_6144   = 0xc << 6,     // Crystal Frequency = 6.144 MHz
        RCC_XTAL_7378   = 0xd << 6,     // Crystal Frequency = 7.3728 MHz
        RCC_XTAL_8000   = 0xe << 6,     // Crystal Frequency = 8 MHz
        RCC_XTAL_8192   = 0xf << 6,     // Crystal Frequency = 8.129 MHz
        RCC_PLLVER      = 1 << 10,      // PLL Verification                                     r/w     0
        RCC_BYPASS      = 1 << 11,      // PLL Bypass                                           r/w     1
        RCC_OEN         = 1 << 12,      // PLL Output Enable                                    r/w     1
        RCC_PWRDN       = 1 << 13,      // PLL Power Down                                       r/w     1
        RCC_PWMDIV      = 1 << 17,      // PWM Unit Clock Divisor                               r/w     0x7
        RCC_USEPWMDIV   = 1 << 20,      // Enable PWM Clock Divisor                             r/w     0
        RCC_USESYSDIV   = 1 << 22,      // Enable System Clock Divider                          r/w     0
        RCC_SYSDIV      = 0x0 << 23,    // System Clock Divisor                                 r/w     0xf
        RCC_SYSDIV_1    = 0x0 << 23,    // System Clock Divisor = 2
        RCC_SYSDIV_4    = 0x3 << 23,    // System Clock Divisor = 4 -> 50 MHz
        RCC_SYSDIV_5    = 0x4 << 23,    // System Clock Divisor = 5 -> 40 HMz
        RCC_SYSDIV_16   = 0xf << 23,    // System Clock Divisor = 16 -> 12.5 HMz
        RCC_ACG         = 1 << 27       // ACG                                                  r/w     0
    };

    // Useful Bits in the Raw Interrupt Status
    enum RIS {                          // Description                                          Type    Value after reset
        RIS_PLLLRIS     = 1 <<  6       // PLL Lock Raw Interrupt Status                        ro      0
    };


    // Base address for memory-mapped System Control Space
    enum {
        SCS_BASE        = 0xe000e000
    };

    // System Control Space offsets
    enum {                              // Description                                          Type    Value after reset
        MCR             = 0x000,        // Master Control Register                              -       0x00000000
        ICTR            = 0x004,        // Interrupt Controller Type Register                   RO      0x????????
        ACTLR           = 0x008,        // Auxiliary Control Register                           R/W     0x????????
        STCTRL          = 0x010,        // SysTick Control and Status Register                  R/W     0x00000000
        STRELOAD        = 0x014,        // SysTick Reload Value Register                        R/W     0x00000000
        STCURRENT       = 0x018,        // SysTick Current Value Register                       R/WC    0x00000000
        IRQ_ENABLE0     = 0x100,        // Interrupt 0-31 Set Enable                            R/W     0x00000000
        IRQ_ENABLE1     = 0x100,        // Inexistent in this model, defined for machine completion
        IRQ_ENABLE2     = 0x100,        // Inexistent in this model, defined for machine completion
        IRQ_DISABLE0    = 0x180,        // Interrupt 0-31 Clear Enable                          R/W     0x00000000
        IRQ_DISABLE1    = 0x180,        // Inexistent in this model, defined for machine completion
        IRQ_DISABLE2    = 0x180,        // Inexistent in this model, defined for machine completion
        IRQ_PEND0       = 0x200,        // Interrupt 0-31 Set Pending                           R/W     0x00000000
        IRQ_PEND1       = 0x200,        // Inexistent in this model, defined for machine completion
        IRQ_PEND2       = 0x200,        // Inexistent in this model, defined for machine completion
        IRQ_UNPEND0     = 0x280,        // Interrupt 0-31 Clear Pending                         R/W     0x00000000
        IRQ_UNPEND1     = 0x280,        // Inexistent in this model, defined for machine completion
        IRQ_UNPEND2     = 0x280,        // Inexistent in this model, defined for machine completion
        IRQ_ACTIVE0     = 0x300,        // Interrupt 0-31 Active Bit                            R/W     0x00000000
        IRQ_ACTIVE1     = 0x300,        // Inexistent in this model, defined for machine completion
        IRQ_ACTIVE2     = 0x300,        // Inexistent in this model, defined for machine completion
        CPUID           = 0xd00,        // CPUID Base Register                                  RO      0x410fc231
        INTCTRL         = 0xd04,        // Interrupt Control and State Register                 R/W     0x00000000
        VTOR            = 0xd08,        // Vector Table Offset Register                         R/W     0x00000000
        AIRCR           = 0xd0c,        // Application Interrupt/Reset Control Register         R/W
        SCR             = 0xd10,        // System Control Register                              R/W     0x00000000
        CCR             = 0xd14,        // Configuration Control Register                       R/W     0x00000000
        SHPR1           = 0xd18,        // System Handlers 4-7 Priority                         R/W     0x00000000
        SHPR2           = 0xd1c,        // System Handlers 8-11 Priority                        R/W     0x00000000
        SHPR3           = 0xd20,        // System Handlers 12-15 Priority                       R/W     0x00000000
        SHCSR           = 0xd24,        // System Handler Control and State Register            R/W     0x00000000
        CFSR            = 0xd28,        // Configurable Fault Status Register                   R/W     0x00000000
        SWTRIG          = 0xf00         // Software Trigger Interrupt Register                  WO      0x00000000
    };

    // Useful Bits in the ISysTick Control and Status Register
    enum STCTRL {                       // Description                                          Type    Value after reset
        ENABLE          = 1 <<  0,      // Enable / disable                                     rw      0
        INTEN           = 1 <<  1,      // Interrupt pending                                    rw      0
        CLKSRC          = 1 <<  2,      // Clock source (0 -> external, 1 -> core)              rw      0
        COUNT           = 1 << 16       // Count underflow                                      ro      0
    };

    // Useful Bits in the Interrupt Control and State Register
    enum INTCTRL {                      // Description                                          Type    Value after reset
        ICSR_ACTIVE     = 1 <<  0,      // Active exceptions (IPSR mirror; 0 -> thread mode)    ro
        ICSR_PENDING    = 1 << 12,      // Pending exceptions (0 -> none)                       ro
        ICSR_ISRPENDING = 1 << 22,      // Pending NVIC IRQ                                     ro
        ICSR_SYSPENDING = 1 << 25       // Clear pending SysTick                                wo
    };

    // Useful Bits in the Application Interrupt/Reset Control Register
    enum AIRCR {                        // Description                                          Type    Value after reset
        VECTRESET       = 1 << 0,       // Reserved for debug                                   wo      0
        VECTCLRACT      = 1 << 1,       // Reserved for debug                                   wo      0
        SYSRESREQ       = 1 << 2,       // System Reset Request                                 wo      0
        VECTKEY         = 1 << 16,      // Register Key                                         rw      0xfa05
                                        // This field is used to guard against accidental 
                                        // writes to this register.  0x05FA must be written 
                                        // to this field in order to change the bits in this
                                        // register. On a read, 0xFA05 is returned.
    };

    // Useful Bits in the Configuration Control Register
    enum CCR {                          // Description                                          Type    Value after reset
        BASETHR         = 1 <<  0,      // Thread state can be entered at any level of int.     rw      0
        MAINPEND        = 1 <<  1,      // SWTRIG can be written to in user mode                rw      0
        UNALIGNED       = 1 <<  3,      // Trap on unaligned memory access                      rw      0
        DIV0            = 1 <<  4,      // Trap on division by zero                             rw      0
        BFHFNMIGN       = 1 <<  8,      // Ignore Precise Data Access Faults for pri -1 and -2  rw      0
        STKALIGN        = 1 <<  9       // Align stack point on exception entry to 8 butes      rw      0
    };

    // Base address for memory-mapped GPIO Ports Registers
    enum {
        GPIOA_BASE      = 0x40004000,   // GPIO Port A
        GPIOB_BASE      = 0x40005000,   // GPIO Port B
        GPIOC_BASE      = 0x40006000,   // GPIO Port C
        GPIOD_BASE      = 0x40007000,   // GPIO Port D
        GPIOE_BASE      = 0x40024000    // GPIO Port E
    };

    // GPIO Ports Registers offsets
    enum {                              // Description                  Type    Value after reset
        DATA		= 0x000,	// Data  	                R/W	0x0000.0000
        DIR		= 0x400,	// Direction    	        R/W	0x0000.0000
        IS		= 0x404,	// Interrupt Sense      	R/W	0x0000.0000
        IBE		= 0x408,	// Interrupt Both Edges 	R/W	0x0000.0000
        IEV		= 0x40c,	// Interrupt Event 	        R/W	0x0000.0000
        IM		= 0x410,	// Interrupt Mask 	        R/W	0x0000.0000
        GRIS		= 0x414,	// Raw Interrupt Status 	RO	0x0000.0000
        MIS		= 0x418,	// Masked Interrupt Status	RO	0x0000.0000
        ICR		= 0x41c,	// Interrupt Clear 	        W1C	0x0000.0000
        AFSEL		= 0x420,	// Alternate Function Select	R/W	-
        DR2R		= 0x500,	// 2-mA Drive Select	        R/W	0x0000.00ff
        DR4R		= 0x504,	// 4-mA Drive Select	        R/W	0x0000.0000
        DR8R		= 0x508,	// 8-mA Drive Select	        R/W	0x0000.0000
        ODR		= 0x50c,	// Open Drain Select	        R/W	0x0000.0000
        PUR		= 0x510,	// Pull-Up Select 	        R/W	0x0000.00ff
        PDR		= 0x514,	// Pull-Down Select 	        R/W	0x0000.0000
        SLR		= 0x518,	// Slew Rate Control Select	R/W	0x0000.0000
        DEN		= 0x51c,	// Digital Enable 	        R/W	0x0000.00ff
        P_EDGE_CTRL     = 0x704,        // Power-up Int. Edge Control   R/W     0x0000.0000
        PI_IEN          = 0x710,	// Power-up Interrupt Enable    R/W	0x0000.0000
        IRQ_DETECT_ACK  = 0x718,        // TODO: document this!
        PeriphID4	= 0xfd0,	// Peripheral Identification 4	RO	0x0000.0000
        PeriphID5	= 0xfd4,	// Peripheral Identification 5 	RO	0x0000.0000
        PeriphID6	= 0xfd8,	// Peripheral Identification 6	RO	0x0000.0000
        PeriphID7	= 0xfdc,	// Peripheral Identification 7	RO	0x0000.0000
        PeriphID0	= 0xfe0,	// Peripheral Identification 0	RO	0x0000.0061
        PeriphID1	= 0xfe4,	// Peripheral Identification 1	RO	0x0000.0000
        PeriphID2	= 0xfe8,	// Peripheral Identification 2	RO	0x0000.0018
        PeriphID3	= 0xfec,	// Peripheral Identification 3	RO	0x0000.0001
        PCellID0	= 0xff0,	// PrimeCell Identification 0	RO	0x0000.000d
        PCellID1	= 0xff4,	// PrimeCell Identification 1	RO	0x0000.00f0
        PCellID2	= 0xff8,	// PrimeCell Identification 2	RO	0x0000.0005
        PCellID3	= 0xffc		// PrimeCell Identification 3	RO	0x0000.00b1
    };

    // Useful Bits in the Alternate Function Select Register
    enum AFSEL {                        // Description                  Type    Value after reset
        AFSEL_ALTP0     = 1 <<  0,      // Pin 0 (0 -> GPIO | 1 -> Alt) r/w     0
        AFSEL_ALTP1     = 1 <<  1,      // Pin 1 (0 -> GPIO | 1 -> Alt) r/w     0
        AFSEL_ALTP2     = 1 <<  2,      // Pin 2 (0 -> GPIO | 1 -> Alt) r/w     0
        AFSEL_ALTP3     = 1 <<  3,      // Pin 3 (0 -> GPIO | 1 -> Alt) r/w     0
        AFSEL_ALTP4     = 1 <<  4,      // Pin 4 (0 -> GPIO | 1 -> Alt) r/w     0
        AFSEL_ALTP5     = 1 <<  5,      // Pin 5 (0 -> GPIO | 1 -> Alt) r/w     0
        AFSEL_ALTP6     = 1 <<  6,      // Pin 6 (0 -> GPIO | 1 -> Alt) r/w     0
        AFSEL_ALTP7     = 1 <<  7       // Pin 7 (0 -> GPIO | 1 -> Alt) r/w     0
    };

    // Useful Bits in the Digital Enable Register
    enum DEN {                        // Description                    Type    Value after reset
        DEN_DIGP0     = 1 <<  0,      // Pin 0 (1 -> Digital Enable)    r/w     1
        DEN_DIGP1     = 1 <<  1,      // Pin 1 (1 -> Digital Enable)    r/w     1
        DEN_DIGP2     = 1 <<  2,      // Pin 2 (1 -> Digital Enable)    r/w     1
        DEN_DIGP3     = 1 <<  3,      // Pin 3 (1 -> Digital Enable)    r/w     1
        DEN_DIGP4     = 1 <<  4,      // Pin 4 (1 -> Digital Enable)    r/w     1
        DEN_DIGP5     = 1 <<  5,      // Pin 5 (1 -> Digital Enable)    r/w     1
        DEN_DIGP6     = 1 <<  6,      // Pin 6 (1 -> Digital Enable)    r/w     1
        DEN_DIGP7     = 1 <<  7       // Pin 7 (1 -> Digital Enable)    r/w     1
    };

    // General Purpose Timer definitions
    class Timer
    {
    protected:
        enum Base
        {
            GPTIMER0_BASE = 0x40030000,
            GPTIMER1_BASE = 0x40031000,
            GPTIMER2_BASE = 0x40032000,
        };

        enum Offset
        {
            //Register Name  Offset  Type  Width  Reset Value
            CFG           =   0x00,  //RW    32    0x00000000
            TAMR          =   0x04,  //RW    32    0x00000000
            TBMR          =   0x08,  //RW    32    0x00000000
            CTL           =   0x0C,  //RW    32    0x00000000
            SYNC          =   0x10,  //RW    32    0x00000000
            IMR           =   0x18,  //RW    32    0x00000000
            RIS           =   0x1C,  //RO    32    0x00000000
            MIS           =   0x20,  //RO    32    0x00000000
            ICR           =   0x24,  //RW    32    0x00000000
            TAILR         =   0x28,  //RW    32    0xFFFFFFFF
            TBILR         =   0x2C,  //RW    32    0x0000FFFF
            TAMATCHR      =   0x30,  //RW    32    0xFFFFFFFF
            TBMATCHR      =   0x34,  //RW    32    0x0000FFFF
            TAPR          =   0x38,  //RW    32    0x00000000
            TBPR          =   0x3C,  //RW    32    0x00000000
            TAPMR         =   0x40,  //RW    32    0x00000000
            TBPMR         =   0x44,  //RW    32    0x00000000
            TAR           =   0x48,  //RO    32    0xFFFFFFFF
            TBR           =   0x4C,  //RO    32    0x0000FFFF
            TAV           =   0x50,  //RW    32    0xFFFFFFFF
            TBV           =   0x54,  //RW    32    0x0000FFFF
            TAPS          =   0x5C,  //RO    32    0x00000000
            TBPS          =   0x60,  //RO    32    0x00000000
            TAPV          =   0x64,  //RO    32    0x00000000
            TBPV          =   0x68,  //RO    32    0x00000000
            PP            =  0xFC0,  //RO    32    0x00000000
        };

        enum CTL
        {
            TBPWML = 1 << 14,   // GPTM Timer B PWM output level
            // 0: Output is unaffected.
            // 1: Output is inverted. RW 0
            TBOTE = 1 << 13,    // GPTM Timer B output trigger enable
            // 0: The ADC trigger of output Timer B is disabled.
            // 1: The ADC trigger of output Timer B is enabled.
            TBEVENT = 1 << 10,  // GPTM Timer B event mode
            // 0x0: Positive edge
            // 0x1: Negative edge
            // 0x2: Reserved
            // 0x3: Both edges RW 0x0
            TBSTALL = 1 << 9,   // GPTM Timer B stall enable
            // 0: Timer B continues counting while the processor is halted by the
            // debugger.
            // 1: Timer B freezes counting while the processor is halted by the
            // debugger. RW 0
            TBEN = 1 << 8,      // GPTM Timer B enable
            // 0: Timer B is disabled.
            // 1: Timer B is enabled and begins counting or the capture logic is
            // enabled based on the GPTMCFG register. RW 0
            TAPWML = 1 << 6,    // GPTM Timer A PWM output level
            // 0: Output is unaffected.
            // 1: Output is inverted. RW 0
            TAOTE = 1 << 5,     // GPTM Timer A output trigger enable
            // 0: The ADC trigger of output Timer A is disabled.
            // 1: The ADC trigger of output Timer A is enabled. RW 0
            TAEVENT = 1 << 2,   // GPTM Timer A event mode
            // 0x0: Positive edge
            // 0x1: Negative edge
            // 0x2: Reserved
            // 0x3: Both edges RW 0x0
            TASTALL = 1 << 1,   // GPTM Timer A stall enable
            // 0: Timer A continues counting while the processor is halted by the
            // debugger.
            // 1: Timer A freezes counting while the processor is halted by the
            // debugger. RW 0
            TAEN = 1 << 0,      // GPTM Timer A enable
            // 0: Timer A is disabled.
            // 1: Timer A is enabled and begins counting or the capture logic is
            // enabled based on the GPTMCFG register.
        };

        enum TAMR
        {
            TAPLO = 1 << 11,    // Legacy PWM operation
            // 0: Legacy operation
            // 1: CCP is set to 1 on time-out. RW 0
            TAMRSU = 1 << 10,   // Timer A match register update mode
            // 0: Update GPTMAMATCHR and GPTMAPR if used on the next
            // cycle.
            // 1: Update GPTMAMATCHR and GPTMAPR if used on the next
            // time-out. If the timer is disabled (TAEN is clear) when this bit is set,
            // GPTMTAMATCHR and GPTMTAPR are updated when the timer is
            // enabled. If the timer is stalled (TASTALL is set), GPTMTAMATCHR
            // and GPTMTAPR are updated according to the configuration of this
            // bit. RW 0
            TAPWMIE = 1 << 9,   // GPTM Timer A PWM interrupt enable
            // This bit enables interrupts in PWM mode on rising, falling, or both
            // edges of the CCP output.
            // 0: Interrupt is disabled.
            // 1: Interrupt is enabled.
            // This bit is valid only in PWM mode. RW 0
            TAILD = 1 << 8,     // GPTM Timer A PWM interval load write
            // 0: Update the GPTMTAR register with the value in the GPTMTAILR
            // register on the next cycle. If the prescaler is used, update the
            // GPTMTAPS register with the value in the GPTMTAPR register on
            // the next cycle.
            // 1: Update the GPTMTAR register with the value in the GPTMTAILR
            // register on the next cycle. If the prescaler is used, update the
            // GPTMTAPS register with the value in the GPTMTAPR register on
            // the next time-out. RW 0
            TASNAPS = 1 << 7,   // GPTM Timer A snap-shot mode
            // 0: Snap-shot mode is disabled.
            // 1: If Timer A is configured in periodic mode, the actual free-running
            // value of Timer A is loaded at the time-out event into the GPTM
            // Timer A (GPTMTAR) register. RW 0
            TAWOT = 1 << 6,     // GPTM Timer A wait-on-trigger
            // 0: Timer A begins counting as soon as it is enabled.
            // 1: If Timer A is enabled (TAEN is set in the GPTMCTL register),
            // Timer A does not begin counting until it receives a trigger from the
            // Timer in the previous position in the daisy-chain. This bit must be
            // clear for GP Timer module 0, Timer A. RW 0
            TAMIE = 1 << 5,     // GPTM Timer A match interrupt enable
            // 0: The match interrupt is disabled.
            // 1: An interrupt is generated when the match value in the
            // GPTMTAMATCHR register is reached in the one-shot and periodic
            // modes. RW 0
            TACDIR = 1 << 4,    // GPTM Timer A count direction
            // 0: The timer counts down.
            // 1: The timer counts up. When counting up, the timer starts from a
            // value of 0x0. RW 0
            TAAMS = 1 << 3,     // GPTM Timer A alternate mode
            // 0: Capture mode is enabled.
            // 1: PWM mode is enabled.
            // Note: To enable PWM mode, the TACM bit must be cleared and the
            // TAMR field must be configured to 0x2. RW 0
            TACMR = 1 << 2,     // GPTM Timer A capture mode
            // 0: Edge-count mode
            // 1: Edge-time mode
            TAMR_TAMR = 1 << 0, // GPTM Timer A mode
            // 0x0: Reserved
            // 0x1: One-shot mode
            // 0x2: Periodic mode
            // 0x3: Capture mode
            // The timer mode is based on the timer configuration defined by bits
            // [2:0] in the GPTMCFG register.
        };
    };

protected:
    LM3S811() {}

    static void reboot() {
        Reg32 val = scs(AIRCR) & (~((-1u / VECTKEY) * VECTKEY));
        val |= SYSRESREQ;
        val |= 0x05fa * VECTKEY;
        scs(AIRCR) = val;
    }

    void uart_config(volatile Log_Addr * base) {
        if(base == reinterpret_cast<Log_Addr *>(UART0_BASE)) {
            scr(RCGC1) |= RCGC1_UART0;                   // Activate UART 0 clock
            scr(RCGC2) |= RCGC2_GPIOA;                   // Activate port A clock
            gpioa(AFSEL) |= (AFSEL_ALTP0 | AFSEL_ALTP1); // Pins A[1:0] are multiplexed between GPIO and UART 0. Select UART.
            gpioa(DEN) |= (DEN_DIGP0 | DEN_DIGP1);       // Enable digital I/O on Pins A[1:0]
        } else {
            scr(RCGC1) |= RCGC1_UART1;                   // Activate UART 1 clock
            scr(RCGC2) |= RCGC2_GPIOB;                   // Activate port B clock
            gpiod(AFSEL) |= (AFSEL_ALTP2 | AFSEL_ALTP3); // Pins D[3:2] are multiplexed between GPIO and UART 1. Select UART.
            gpiod(DEN) |= (DEN_DIGP2 | DEN_DIGP3);       // Enable digital I/O on Pins D[3:2]
        }
    }
    static void uart_enable() {};
    static void uart_disable() {};

    static void usb_config() {};
    static void usb_enable() {};
    static void usb_disable() {};

    static void timer_enable(unsigned int timer) {
        assert(timer < TIMERS);
        scr(RCGC1) |= 1 << timer;
        scr(RCGC2) |= 1 << timer;
    }
    static void timer_disable(unsigned int timer) {
        assert(timer < TIMERS);
        scr(RCGC1) &= ~(1 << timer);
        scr(RCGC2) &= ~(1 << timer);
    }

    void gpio_pull_up(int port, int pin) { gpio(port, PUR) &= 1 << pin; }
    void gpio_pull_down(int port, int pin) { gpio(port, PDR) &= 1 << pin; }

    static void radio_enable() {};
    static void radio_disable() {};

public:
    static volatile Reg32 & scr(unsigned int o) { return reinterpret_cast<volatile Reg32 *>(SCR_BASE)[o / sizeof(Reg32)]; }
    static volatile Reg32 & scs(unsigned int o) { return reinterpret_cast<volatile Reg32 *>(SCS_BASE)[o / sizeof(Reg32)]; }

    static volatile Reg32 & gpio(unsigned int port, unsigned int o) { return reinterpret_cast<volatile Reg32 *>(GPIOA_BASE + 0x1000*(port))[o / sizeof(Reg32)]; }
    static volatile Reg32 & gpioa(unsigned int o) { return reinterpret_cast<volatile Reg32 *>(GPIOA_BASE)[o / sizeof(Reg32)]; }
    static volatile Reg32 & gpiob(unsigned int o) { return reinterpret_cast<volatile Reg32 *>(GPIOB_BASE)[o / sizeof(Reg32)]; }
    static volatile Reg32 & gpioc(unsigned int o) { return reinterpret_cast<volatile Reg32 *>(GPIOC_BASE)[o / sizeof(Reg32)]; }
    static volatile Reg32 & gpiod(unsigned int o) { return reinterpret_cast<volatile Reg32 *>(GPIOD_BASE)[o / sizeof(Reg32)]; }
    static volatile Reg32 & gpioe(unsigned int o) { return reinterpret_cast<volatile Reg32 *>(GPIOE_BASE)[o / sizeof(Reg32)]; }

protected:
    static void init();
};

typedef LM3S811 Cortex_M_Model;

__END_SYS

#endif
