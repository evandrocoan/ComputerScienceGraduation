// EPOS Cortex_M Timer Mediator Declarations

#ifndef __cortex_m_timer_h
#define __cortex_m_timer_h

#include <cpu.h>
#include <ic.h>
#include <rtc.h>
#include <timer.h>
#include <machine.h>
#include <machine/cortex_m/emote3_mac_timer.h>
#include __MODEL_H

__BEGIN_SYS

class Cortex_M_Sys_Tick: public Cortex_M_Model
{
private:
    typedef TSC::Hertz Hertz;

public:
    typedef CPU::Reg32 Count;
    static const unsigned int CLOCK = Traits<CPU>::CLOCK;

protected:
    Cortex_M_Sys_Tick() {}

public:
    static void enable() {
        scs(STCTRL) |= ENABLE;
    }
    static void disable() {
        scs(STCTRL) &= ~ENABLE;
    }

    static Hertz clock() { return CLOCK; }

    static void init(unsigned int f) {
        scs(STCTRL) = 0;
        scs(STCURRENT) = 0;
        scs(STRELOAD) = CLOCK / f;
        scs(STCTRL) = CLKSRC | INTEN;

    }
};


// Tick timer used by the system
class Cortex_M_Timer: private Timer_Common
{
    friend class Cortex_M;
    friend class Init_System;

protected:
    static const unsigned int CHANNELS = 2;
    static const unsigned int FREQUENCY = Traits<Cortex_M_Timer>::FREQUENCY;

    typedef Cortex_M_Sys_Tick Engine;
    typedef Engine::Count Count;
    typedef IC::Interrupt_Id Interrupt_Id;

public:
    using Timer_Common::Hertz;
    using Timer_Common::Tick;
    using Timer_Common::Handler;
    using Timer_Common::Channel;

    // Channels
    enum {
        SCHEDULER,
        ALARM,
        USER
    };

protected:
    Cortex_M_Timer(const Hertz & frequency, const Handler & handler, const Channel & channel, bool retrigger = true)
    : _channel(channel), _initial(FREQUENCY / frequency), _retrigger(retrigger), _handler(handler) {
        db<Timer>(TRC) << "Timer(f=" << frequency << ",h=" << reinterpret_cast<void*>(handler) << ",ch=" << channel << ") => {count=" << _initial << "}" << endl;

        if(_initial && (channel < CHANNELS) && !_channels[channel])
            _channels[channel] = this;
        else
            db<Timer>(WRN) << "Timer not installed!"<< endl;

        for(unsigned int i = 0; i < Traits<Machine>::CPUS; i++)
            _current[i] = _initial;
    }

public:
    ~Cortex_M_Timer() {
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

    static void enable() { Engine::enable(); }
    static void disable() { Engine::disable(); }

private:
    static Hertz count2freq(const Count & c) { return c ? Engine::clock() / c : 0; }
    static Count freq2count(const Hertz & f) { return f ? Engine::clock() / f : 0;}

    static void int_handler(const Interrupt_Id & i);

    static void init();

private:
    unsigned int _channel;
    Count _initial;
    bool _retrigger;
    volatile Count _current[Traits<Machine>::CPUS];
    Handler _handler;

    static Cortex_M_Timer * _channels[CHANNELS];
};

// Timer used by Thread::Scheduler
class Scheduler_Timer: public Cortex_M_Timer
{
private:
    typedef RTC::Microsecond Microsecond;

public:
    Scheduler_Timer(const Microsecond & quantum, const Handler & handler): Cortex_M_Timer(1000000 / quantum, handler, SCHEDULER) {}
};

// Timer used by Alarm
class Alarm_Timer: public Cortex_M_Timer
{
public:
    static const unsigned int FREQUENCY = Timer::FREQUENCY;

public:
    Alarm_Timer(const Handler & handler): Cortex_M_Timer(FREQUENCY, handler, ALARM) {}
};


// User timer
class User_Timer: private Timer_Common, private Cortex_M_Model::Timer, private Cortex_M_Model
{
private:
    const static unsigned int CLOCK = Traits<CPU>::CLOCK;

    typedef CPU::Reg32 Reg32;

public:
    using Timer_Common::Microsecond;
    using Timer_Common::Handler;
    using Timer_Common::Channel;

public:
    User_Timer(const Handler & handler, const Channel & channel, const Microsecond & time, bool retrigger = false)
    : _handler(handler), _channel(channel), _base(reinterpret_cast<Reg32 *>(GPTIMER0_BASE + 0x1000 * channel)) {
        disable();
        Cortex_M_Model::timer_enable(channel);
        reg(CFG) = 0; // 32-bit timer
        reg(TAMR) = retrigger ? 2 : 1; // 2 -> Periodic, 1 -> One-shot
        reg(TAILR) = us2count(time);
        enable();
    }
    ~User_Timer() {
        disable();
        Cortex_M_Model::timer_disable(_channel);
    }

    bool running() { return !reg(Offset::RIS); }

    Microsecond read() { return count2us(reg(TAV)); }

    void enable() {
        reg(Cortex_M_Model::Timer::ICR) = -1; // Clear interrupts
        reg(CTL) |= TAEN; // Enable timer A
    }
    void disable() {
        reg(CTL) &= ~TAEN; // Disable timer A
    }

private:
    volatile Reg32 & reg(unsigned int o) { return _base[o / sizeof(Reg32)]; }

    static Reg32 us2count(const Microsecond & us) { return us * (CLOCK / 1000000); }
    static Microsecond count2us(Reg32 count) { return count / (CLOCK / 1000000); }

private:
    Handler _handler;
    unsigned int _channel;
    Reg32 * _base;
};


// Timer wrapper used by TSTP
class TSTP_Timer
{
    typedef eMote3_MAC_Timer Engine;

public:
    static const long long FREQUENCY = Engine::FREQUENCY;
    static long long frequency() { return FREQUENCY; }
    typedef void (* Interrupt_Handler)();

    typedef Engine::Timestamp Time_Stamp;
    typedef Engine::Microsecond Microsecond;

    static Time_Stamp now() { return read(); }// + _offset; }
    static Time_Stamp sfd() { return Engine::last_sfd_ts(); }// + _offset; }

    static void adjust(const Time_Stamp & offset) { _offset += offset; }
    static void set(const Time_Stamp & value) { _offset = value - read(); }

    static void interrupt(const Time_Stamp & when, Interrupt_Handler handler) { Engine::interrupt_ts(when /*- _offset*/, handler); }
    static void cancel_interrupt() { Engine::int_disable(); }

    TSTP_Timer() { _offset = 0; Engine::config(); }

    static void start() { Engine::start(); }
    static void stop() { Engine::stop(); }

    static Time_Stamp us_to_ts(Microsecond us) { return Engine::us_to_ts(us); }
    static Microsecond ts_to_us(Time_Stamp ts) { return Engine::ts_to_us(ts); }

private:
    static Time_Stamp read() { return Engine::read_ts(); }
    static Time_Stamp _offset;
};
/*
class TSTP_Timer
{
public:
    static const unsigned int FREQUENCY = 1;
    static unsigned int frequency() { return FREQUENCY; }
    typedef void (* Interrupt_Handler)();

    typedef int Time_Stamp;
    typedef int Microsecond;

    static Time_Stamp now() { return 1; }
    static Time_Stamp sfd() { return 1; }

    static void adjust(const Time_Stamp & offset) { }
    static void set(const Time_Stamp & value) { }

    static void interrupt(const Time_Stamp & when, Interrupt_Handler handler) { }
    static void cancel_interrupt() { }

    TSTP_Timer() { }

    static void start() { }
    static void stop() { }

    static Time_Stamp us_to_ts(Microsecond us) { return 1; }
    static Microsecond ts_to_us(Time_Stamp ts) { return 1; }

private:
    static Time_Stamp read() { return 1; }
};
*/

__END_SYS

#endif
