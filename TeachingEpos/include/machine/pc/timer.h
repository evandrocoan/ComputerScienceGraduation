// EPOS PC Timer Mediator Declarations

#ifndef __pc_timer_h
#define __pc_timer_h

#include <cpu.h>
#include <ic.h>
#include <rtc.h>
#include <timer.h>
#include <machine.h>

__BEGIN_SYS

// Intel 8253 (i82C54) Timer (tree counters, only counter 0 available to OS)
class i8253
{
private:
    typedef CPU::Reg8 Reg8;
    typedef TSC::Hertz Hertz;

public:
    // The timer's counter
    typedef CPU::Reg16 Count;

    // Clock input 1.193182 MHz (for all 3 channels)
    static const int CLOCK = 1193182;

    // I/O Ports
    enum {
        CNT_0		= 0x40, // Time keeper
        CNT_1		= 0x41, // Memory refresh
        CNT_2		= 0x42, // Speaker
        CTRL		= 0x43  // Control
    };

    // Control Register Format (8 bits)
    enum {
        SELECT_MASK	= 0xc0, // Select Counter (2 bits)
        SC0	    	= 0x00, // counter 0
        SC1	    	= 0x40, // counter 1
        SC2	    	= 0x80, // counter 2
        RB	    	= 0xc0, // read-back
        RW_MASK		= 0x30, // Read/Write (2 bits)
        LATCH		= 0x00, // latch counter for reading
        MSB	    	= 0x10, // read/write MSB only
        LSB	    	= 0x20, // read/write LSB only
        LMSB		= 0x30,	// read/write LSB then MSB
        MODE_MASK	= 0x0e, // 3 bits
        IOTC		= 0x00, // Interrupt on Terminal Count
        HROS		= 0x02, // Hardware Retriggerable One-Shot
        RG	    	= 0x04, // Rate generator
        CSSW		= 0x06, // Continuous Symmetrical Square Wave
        STS	    	= 0x08, // Software Triggered Strobe
        HTS	    	= 0x0a, // Hardware Triggered Strobe
        COUNT_MODE_MASK	= 0x01, // 1 bit
        BINARY		= 0x00, // Binary count
        BCD		    = 0x01, // BCD count
        DEF_CTRL_C0	= SC0	| LMSB	| CSSW	| BINARY, // Counter 0 default
        DEF_CTRL_C1	= SC1	| MSB	| RG	| BINARY, // Counter 1 default
        DEF_CTRL_C2	= SC2	| LMSB	| IOTC	| BINARY  // Counter 2 default
    };

    // Default Counters as set by the BIOS (16 bits)
    enum {
        DEF_CNT_C0	= 0x0000, // CLOCK/65535 ->     18.2 Hz
        DEF_CNT_C1	= 0x0012, // CLOCK/18    ->  66287.8 Hz
        DEF_CNT_C2	= 0x0533, // CLOCK/1331  ->    896.5 Hz
    };

public:
    i8253() {}

    static Hertz clock() { return CLOCK; }

    static void config(int channel, const Count & count, bool interrupt = true, bool periodic = true)
    {
        if(channel > 2)
            return;

        Reg8 cnt, control;
        switch(channel) {
        case(1):
            cnt = CNT_1;
            control = DEF_CTRL_C1;
        break;
        case(2):
            cnt = CNT_2;
            control = DEF_CTRL_C2;
        break;
        default:
            cnt = CNT_0;
            control = DEF_CTRL_C0;
        }

        CPU::out8(CTRL, control);
        CPU::out8(cnt, count & 0xff);
        CPU::out8(cnt, count >> 8);
    }

    static Count read(int channel) {
        if(channel > 2)
            return 0;

        Reg8 cnt, control;
        switch(channel) {
        case(1):
                cnt = CNT_1;
        control = SC1;
        break;
        case(2):
                cnt = CNT_2;
        control = SC2;
        break;
        default:
            cnt = CNT_0;
            control = SC0;
        }
        control |= LATCH;

        CPU::out8(CTRL, control);
        Count count = CPU::in8(cnt) | (CPU::in8(cnt) << 8);

        return count;
    }

    static void reset(int channel) {
        // 8253 doesn't feature a reset, but a counter can be reset by writing
        // the corresponding control register

        if(channel > 2)
            return;

        Reg8 cnt, ctrl;
        switch(channel) {
        case(1):
            cnt = CNT_1;
            ctrl = DEF_CTRL_C1;
        break;
        case(2):
            cnt = CNT_2;
            ctrl = DEF_CTRL_C2;
        break;
        default:
            cnt = CNT_0;
            ctrl = DEF_CTRL_C0;
        }

        Reg8 control = CPU::in8(ctrl);
        Count count = CPU::in8(cnt) | (CPU::in8(cnt) << 8);
        CPU::out8(ctrl, control);
        CPU::out8(cnt, count & 0xff);
        CPU::out8(cnt, count >> 8);
    }
};


// Intel 8255 Programmable Peripheral Interface (3 x 8-bit ports)
class i8255
{
private:
    typedef CPU::Reg8 Reg8;

public:
    // I/O Ports
    enum {
        PORT_B		= 0x61
    };

    // Port B (status/control)
    enum {
        MEMORY_PARITY           = 0x80, // R/O
        IO_CHECK                = 0x40, // R/O
        I8253_OUT2              = 0x20, // R/O, i8253 CH 2 (speaker)
        I8253_OUT1              = 0x10, // R/O, i8253 CH 1 (DRAM refresh)
        IO_CHECK_ENABLE         = 0x08, // R/W
        MEMORY_PARITY_ENABLE    = 0x04, // R/W
        SPEAKER                 = 0x02, // R/W, speaker enable
        I8253_GATE2             = 0x01  // R/W, i8253 CH 2 (speaker)
    };

public:
    i8255() {}

    static Reg8 port_b() {
        return CPU::in8(PORT_B);
    }

    static void port_b(Reg8 b) {
        return CPU::out8(PORT_B, b);
    }

    static void ms_delay(int milliseconds) {
        for(; milliseconds > 0; milliseconds--) {
            // Disable speaker so we can use channel 2 of i8253
            port_b(port_b() & ~(SPEAKER | I8253_GATE2));

            // Program i8253 channel 2 to count 1 ms
            i8253::config(2, i8253::CLOCK/1000, false, false);

            // Enable i8253 channel 2 counting
            port_b(port_b() | I8253_GATE2);

            // Wait for i8253 counting to finish
            while(!(port_b() & I8253_OUT2));
        }
    }
};


// APIC Timer (Local to each CPU on MP configurations)
class APIC_Timer
{
private:
    typedef TSC::Hertz Hertz;

public:
    // The timer's counter
    typedef CPU::Reg32 Count;

public:
    APIC_Timer() {}

    static Hertz clock() { return CPU::bus_clock(); }

    static void config(int channel, Count count, bool interrupt = true, bool periodic = true) {
        APIC::config_timer(count, interrupt, periodic);
    }

    static void enable(int channel) { APIC::enable_timer(); }
    static void disable(int channel) { APIC::disable_timer(); }

    static Count read(int channel) { return APIC::read_timer(); }

    static void reset(int channel) { APIC::reset_timer(); }
};


// Tick timer used by the system
class PC_Timer: private Timer_Common
{
    friend class PC;
    friend class Init_System;

protected:
    typedef IF<Traits<System>::multicore, APIC_Timer, i8253>::Result Engine;
    typedef Engine::Count Count;
    typedef IC::Interrupt_Id Interrupt_Id;

    static const unsigned int CHANNELS = 3;
    static const unsigned int FREQUENCY = Traits<PC_Timer>::FREQUENCY;

public:
    enum {
        SCHEDULER,
        ALARM,
        USER
    };

    using Timer_Common::Hertz;
    using Timer_Common::Tick;
    using Timer_Common::Microsecond;
    using Timer_Common::Handler;
    using Timer_Common::Channel;

protected:
    PC_Timer(const Hertz & frequency, const Handler & handler, const Channel & channel, bool retrigger = true)
    : _channel(channel), _initial(FREQUENCY / frequency), _retrigger(retrigger), _handler(handler) {
        db<Timer>(TRC) << "Timer(f=" << frequency << ",h=" << reinterpret_cast<void*>(handler)
                       << ",ch=" << channel << ") => {count=" << _initial << "}" << endl;

        if(_initial && (unsigned(channel) < CHANNELS) && !_channels[channel])
            _channels[channel] = this;
        else
            db<Timer>(WRN) << "Timer not installed!"<< endl;

        for(unsigned int i = 0; i < Traits<Machine>::CPUS; i++)
            _current[i] = _initial;
    }

public:
    ~PC_Timer() {
        db<Timer>(TRC) << "~Timer(f=" << frequency() << ",h=" << reinterpret_cast<void*>(_handler)
        	       << ",ch=" << _channel << ") => {count=" << _initial << "}" << endl;

        _channels[_channel] = 0;
    }

    Hertz frequency() const { return (FREQUENCY / _initial); }
    void frequency(const Hertz & f) { _initial = FREQUENCY / f; reset(); }

    Tick read() { return _current[Machine::cpu_id()]; }

    int reset() {
        db<Timer>(TRC) << "Timer::reset() => {f=" << frequency()
        	       << ",h=" << reinterpret_cast<void*>(_handler)
        	       << ",count=" << _current[Machine::cpu_id()] << "}" << endl;

        int percentage = _current[Machine::cpu_id()] * 100 / _initial;
        _current[Machine::cpu_id()] = _initial;

        return percentage;
    }

    void handler(const Handler & handler) { _handler = handler; }

    static void enable() { IC::enable(IC::INT_TIMER); }
    static void disable() { IC::disable(IC::INT_TIMER); }

 private:
    static Hertz count2freq(const Count & c) { return c ? Engine::clock() / c : 0; }
    static Count freq2count(const Hertz & f) { return f ? Engine::clock() / f : 0; }

    static void int_handler(const Interrupt_Id & i);

    static void init();

protected:
    unsigned int _channel;
    Count _initial;
    bool _retrigger;
    volatile Count _current[Traits<Machine>::CPUS];
    Handler _handler;

    static PC_Timer * _channels[CHANNELS];
};


// Timer used by Thread::Scheduler
class Scheduler_Timer: public PC_Timer
{
public:
    Scheduler_Timer(const Microsecond & quantum, const Handler & handler): PC_Timer(1000000 / quantum, handler, SCHEDULER) {}
};

// Timer used by Alarm
class Alarm_Timer: public PC_Timer
{
public:
    Alarm_Timer(const Handler & handler): PC_Timer(FREQUENCY, handler, ALARM) {}
};

// Timer available for users
class User_Timer: public PC_Timer
{
public:
    User_Timer(const Microsecond & time, const Handler & handler, const Channel & channel, bool retrigger = false)
    : PC_Timer(1000000 / time, handler, USER, retrigger) {}
};

// Timer wrapper used by TSTP
class TSTP_Timer
{
    static const unsigned int FREQUENCY = 1;

public:
    typedef long long Time_Stamp;
    typedef Time_Stamp Microsecond;

    static Time_Stamp frequency() { return FREQUENCY; }

    static Time_Stamp now() { return RTC::seconds_since_epoch(); }
    static Time_Stamp sfd() { return now(); }

    void interrupt(const Time_Stamp & when, IC::Interrupt_Handler handler); // Not supported in PC
    void cancel_interrupt(); // Not supported in PC

    TSTP_Timer() { }

    static void start() { }
    static void stop() { }
    static void adjust(const Time_Stamp & t) { } // Not supported in PC

    static Time_Stamp us_to_ts(const Microsecond & us) { return us * frequency() / 1000000ll; }
    static Microsecond ts_to_us(const Time_Stamp & ts) { return ts * 1000000ll / frequency(); }
};
__END_SYS

#endif
