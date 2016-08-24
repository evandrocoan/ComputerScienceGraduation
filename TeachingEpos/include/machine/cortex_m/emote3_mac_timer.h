#ifndef __emote3_mac_timer_h
#define __emote3_mac_timer_h

#include <ic.h>
#include <cpu.h>

__BEGIN_SYS

// Dedicated MAC Timer present in CC2538
class eMote3_MAC_Timer
{
    typedef CPU::Reg8 Reg8;
    typedef CPU::Reg16 Reg16;
    typedef CPU::Reg32 Reg32;

    const static unsigned int CLOCK = 32 * 1000 * 1000; // 32MHz

    public:
    const static long long FREQUENCY = CLOCK;
    static long long frequency() { return FREQUENCY; }

    typedef long long Timestamp;
    typedef Timestamp Microsecond;
    typedef void (* Interrupt_Handler)();

    static Timestamp us_to_ts(Microsecond us) { return us * static_cast<Timestamp>(FREQUENCY / 1000000ll); }
    static Microsecond ts_to_us(Timestamp ts) { return ts / static_cast<Timestamp>(FREQUENCY / 1000000ll); }
    static Microsecond read() { return ts_to_us(read_ts()); }
    static void set(Microsecond time) { set_ts(us_to_ts(time)); }

    private:
    enum {
        MAC_TIMER_BASE = 0x40088800,
    };

    enum {     //Offset   Description                               Type    Value after reset
        CSPCFG = 0x00, // MAC Timer event configuration              RW     0x0
        CTRL   = 0x04, // MAC Timer control register                 RW     0x2
        IRQM   = 0x08, // MAC Timer interrupt mask                   RW     0x0
        IRQF   = 0x0C, // MAC Timer interrupt flags                  RW     0x0
        MSEL   = 0x10, // MAC Timer multiplex select                 RW     0x0
        M0     = 0x14, // MAC Timer multiplexed register 0           RW     0x0
        M1     = 0x18, // MAC Timer multiplexed register 1           RW     0x0
        MOVF2  = 0x1C, // MAC Timer multiplexed overflow register 2  RW     0x0
        MOVF1  = 0x20, // MAC Timer multiplexed overflow register 1  RW     0x0
        MOVF0  = 0x24, // MAC Timer multiplexed overflow register 0  RW     0x0
    };

    enum CTRL {           //Offset   Description                                                             Type    Value after reset
        CTRL_LATCH_MODE = 1 << 3, // 0: Reading MTM0 with MTMSEL.MTMSEL = 000 latches the high               RW      0
                                  // byte of the timer, making it ready to be read from MTM1. Reading
                                  // MTMOVF0 with MTMSEL.MTMOVFSEL = 000 latches the two
                                  // most-significant bytes of the overflow counter, making it possible to
                                  // read these from MTMOVF1 and MTMOVF2.
                                  // 1: Reading MTM0 with MTMSEL.MTMSEL = 000 latches the high
                                  // byte of the timer and the entire overflow counter at once, making it
                                  // possible to read the values from MTM1, MTMOVF0, MTMOVF1, and MTMOVF2.
        CTRL_STATE      = 1 << 2, // State of MAC Timer                                                      RO      0
                                  // 0: Timer idle
                                  // 1: Timer running
        CTRL_SYNC       = 1 << 1, // 0: Starting and stopping of timer is immediate; that is, synchronous    RW      1
                                  // with clk_rf_32m.
                                  // 1: Starting and stopping of timer occurs at the first positive edge of
                                  // the 32-kHz clock. For more details regarding timer start and stop,
                                  // see Section 22.4.
        CTRL_RUN        = 1 << 0, // Write 1 to start timer, write 0 to stop timer. When read, it returns    RW      0
                                  // the last written value.
    };
    enum MSEL {
        MSEL_MTMOVFSEL = 1 << 4, // See possible values below
        MSEL_MTMSEL    = 1 << 0, // See possible values below
    };
    enum MSEL_MTMOVFSEL {
        OVERFLOW_COUNTER  = 0x00,
        OVERFLOW_CAPTURE  = 0x01,
        OVERFLOW_PERIOD   = 0x02,
        OVERFLOW_COMPARE1 = 0x03,
        OVERFLOW_COMPARE2 = 0x04,
    };
    enum MSEL_MTMSEL {
        TIMER_COUNTER  = 0x00,
        TIMER_CAPTURE  = 0x01,
        TIMER_PERIOD   = 0x02,
        TIMER_COMPARE1 = 0x03,
        TIMER_COMPARE2 = 0x04,
    };
    enum {
        INT_OVERFLOW_COMPARE2 = 1 << 5,
        INT_OVERFLOW_COMPARE1 = 1 << 4,
        INT_OVERFLOW_PER      = 1 << 3,
        INT_COMPARE2          = 1 << 2,
        INT_COMPARE1          = 1 << 1,
        INT_PER               = 1 << 0
    };


public:
    static void interrupt(const Microsecond & when, const Interrupt_Handler & h) { interrupt_ts(us_to_ts(when), h); }

    static void interrupt_ts(const Timestamp & when, const Interrupt_Handler & h) {
        int_set(0);
        _user_handler = h;
        reg(MSEL) = (OVERFLOW_COMPARE1 * MSEL_MTMOVFSEL) | (TIMER_COMPARE1 * MSEL_MTMSEL);
        reg(M0) = when;
        reg(M1) = when >> 8;
        reg(MOVF0) = when >> 16;
        reg(MOVF1) = when >> 24;
        reg(MOVF2) = when >> 32;
        _int_overflow_count_overflow = when >> 40ll;

        int_clear();
        Timestamp now = read_ts();
        if((when >> 16ll) > (now >> 16ll)) {
            int_enable(INT_OVERFLOW_COMPARE1 | INT_OVERFLOW_PER);
        } else {
            // If when <= now, this will be the case too, 
            // and interrupt will occur in a little while
            int_enable(INT_COMPARE1 | INT_OVERFLOW_PER);
        }
    }

    static Timestamp last_sfd_ts() {
        reg(MSEL) = (TIMER_CAPTURE * MSEL_MTMSEL);

        Timestamp ts = reg(M0); // M0 must be read first
        ts += reg(M1) << 8;
        ts += static_cast<long long>(reg(MOVF2)) << 32ll;
        ts += static_cast<long long>(reg(MOVF1)) << 24ll;
        ts += static_cast<long long>(reg(MOVF0)) << 16ll;
        ts += static_cast<long long>(_overflow_count_overflow) << 40ll;

        return ts;
    }
    static Timestamp last_sfd() { return ts_to_us(last_sfd_ts()); }

    static void int_clear() {
        reg(IRQF) = 0;
    }
    static void int_disable() {
        reg(IRQM) = INT_OVERFLOW_PER;
    }
    static void int_enable(const Reg32 & interrupt) {
        reg(IRQM) |= interrupt;
    }
private:
    static void int_set(const Reg32 & interrupt) {
        reg(IRQM) = interrupt;
    }

private:
    static void interrupt_handler(const unsigned int & interrupt) {
        Reg32 ints = reg(IRQF);
        int_clear();
        if(ints & INT_OVERFLOW_PER) {
            _overflow_count_overflow++;
            if(_int_overflow_count_overflow == _overflow_count_overflow) {
                int_enable(INT_OVERFLOW_COMPARE1);
            }
        }
        if(ints & INT_OVERFLOW_COMPARE1) {
            int_set(INT_COMPARE1 | INT_OVERFLOW_PER);
        } else if(ints & INT_COMPARE1) {
            int_disable();
            _user_handler();
        }
    }

protected:
    static Interrupt_Handler _user_handler;

    static volatile Reg32 & reg (unsigned int offset) { return *(reinterpret_cast<volatile Reg32*>(MAC_TIMER_BASE + offset)); }
    
    static Reg32 _overflow_count_overflow;
    static Reg32 _int_overflow_count_overflow;

public:

    static Timestamp read_ts() {
        //Reg32 index = reg(MSEL);
        reg(MSEL) = (OVERFLOW_COUNTER * MSEL_MTMOVFSEL) | (TIMER_COUNTER * MSEL_MTMSEL);

        Timestamp ts = reg(M0); // M0 must be read first
        ts += reg(M1) << 8;
        ts += static_cast<long long>(reg(MOVF2)) << 32ll;
        ts += static_cast<long long>(reg(MOVF1)) << 24ll;
        ts += static_cast<long long>(reg(MOVF0)) << 16ll;
        ts += static_cast<long long>(_overflow_count_overflow) << 40ll;

        //reg(MSEL) = index;
        return ts;
    }

    static void set_ts(const Timestamp & t) {
        bool r = running();
        if(r) stop();
        //Reg32 index = reg(MSEL);
        reg(MSEL) = (OVERFLOW_COUNTER * MSEL_MTMOVFSEL) | (TIMER_COUNTER * MSEL_MTMSEL);

        reg(MOVF0) = t >> 16ll;
        reg(MOVF1) = t >> 24ll;
        reg(MOVF2) = t >> 32ll; // MOVF2 must be written last
        _overflow_count_overflow = t >> 40ll;

        reg(M0) = t; // M0 must be written first
        reg(M1) = t >> 8ll;

        //reg(MSEL) = index;
        if(r) start();
    }

    static void config() {
        stop();
        int_set(0);
        int_clear();        
        reg(CTRL) &= ~CTRL_SYNC; // We can't use the sync feature because we want to change
                                 // the count and overflow values when the timer is stopped
        reg(CTRL) |= CTRL_LATCH_MODE; // count and overflow will be latched at once
        IC::int_vector(49, &interrupt_handler);
        IC::enable(33);
        int_enable(INT_OVERFLOW_PER);
    }

    static void start() { reg(CTRL) |= CTRL_RUN; }
    static void stop()  { reg(CTRL) &= ~CTRL_RUN; }

    static bool running() { return reg(CTRL) & CTRL_STATE; }
};

__END_SYS

#endif
