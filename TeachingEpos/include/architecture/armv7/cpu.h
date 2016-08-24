// EPOS ARMv7 CPU Mediator Declarations

#ifndef __armv7_h
#define __armv7_h

#include <cpu.h>

__BEGIN_SYS

class ARMv7: private CPU_Common
{
    friend class Init_System;

public:
    // CPU Native Data Types
    using CPU_Common::Reg8;
    using CPU_Common::Reg16;
    using CPU_Common::Reg32;
    using CPU_Common::Log_Addr;
    using CPU_Common::Phy_Addr;

    // CPU Flags
    typedef Reg32 Flags;
    enum {
        FLAG_THUMB      = 1 << 24,      // Thumb state
        FLAG_Q          = 1 << 27,      // DSP Overflow
        FLAG_V          = 1 << 28,      // Overflow
        FLAG_C          = 1 << 29,      // Carry
        FLAG_Z          = 1 << 30,      // Zero
        FLAG_N          = 1 << 31,      // Negative
        FLAG_DEFAULTS   = FLAG_THUMB
    };

    // Exceptions
    typedef Reg32 Exception_Id;
    enum {                      // Priority
        EXC_RESET       = 1,    // -3 (highest)
        EXC_NMI         = 2,    // -2
        EXC_HARD        = 3,    // -1
        EXC_MPU         = 4,    // programmable
        EXC_BUS         = 5,    // programmable
        EXC_USAGE       = 6,    // programmable
        EXC_SVCALL      = 11,   // programmable
        EXC_DEBUG       = 12,   // programmable
        EXC_PENDSV      = 14,   // programmable
        EXC_SYSTICK     = 15    // programmable
    };

    // CPU Context
    class Context
    {
    public:
        Context(const Log_Addr & entry, const Log_Addr & exit): _psr(FLAG_DEFAULTS), _lr(exit | 1), _pc(entry | 1) {}
//        _r0(0), _r1(1), _r2(2), _r3(3), _r4(4), _r5(5), _r6(6), _r7(7), _r8(8), _r9(9), _r10(10), _r11(11), _r12(12),

        void save() volatile  __attribute__ ((naked));
        void load() const volatile;

        friend Debug & operator<<(Debug & db, const Context & c) {
            db << hex
               << "{r0="  << c._r0
               << ",r1="  << c._r1
               << ",r2="  << c._r2
               << ",r3="  << c._r3
               << ",r4="  << c._r4
               << ",r5="  << c._r5
               << ",r6="  << c._r6
               << ",r7="  << c._r7
               << ",r8="  << c._r8
               << ",r9="  << c._r9
               << ",r10=" << c._r10
               << ",r11=" << c._r11
               << ",r12=" << c._r12
               << ",sp="  << &c
               << ",lr="  << c._lr
               << ",pc="  << c._pc
               << ",psr=" << c._psr
               << "}" << dec;
            return db;
        }

    public:
        Reg32 _psr;
        Reg32 _r0;
        Reg32 _r1;
        Reg32 _r2;
        Reg32 _r3;
        Reg32 _r4;
        Reg32 _r5;
        Reg32 _r6;
        Reg32 _r7;
        Reg32 _r8;
        Reg32 _r9;
        Reg32 _r10;
        Reg32 _r11;
        Reg32 _r12;
        Reg32 _lr;
        Reg32 _pc;
    };

    // I/O ports
    typedef Reg16 IO_Irq;

    // Interrupt Service Routines
    typedef void (ISR)();

    // Fault Service Routines (exception handlers)
    typedef void (FSR)();

public:
    ARMv7() {}

    static Hertz clock() { return _cpu_clock; }
    static Hertz bus_clock() { return _bus_clock; }

    static void int_enable() {
        ASM("cpsie i");
    }
    static void int_disable() {
        ASM("cpsid i");
    }

    static bool int_enabled() {
        return !int_disabled();
    }
    static bool int_disabled() {
        bool disabled;
        ASM("mrs %0, primask" : "=r"(disabled));
        return disabled;
    }

    static void halt() { ASM("wfi"); }

    static void switch_context(Context * volatile * o, Context * volatile n) __attribute__ ((naked));

    static int syscall(void * message);
    static void syscalled();

    static Flags flags() {
        register Reg32 value;
        ASM("mrs %0, xpsr" : "=r"(value) ::);
        return value;
    }
    static void flags(const Flags & flags) {
        ASM("msr xpsr, %0" : : "r"(flags) :);
    }

    static Reg32 sp() {
        Reg32 value;
        ASM("mov %0, sp" : "=r"(value) : : );
        return value;
    }
    static void sp(const Reg32 & sp) {
        ASM("mov sp, %0" : : "r"(sp) : "sp");
        ASM("isb");
    }

    static Reg32 fr() {
        Reg32 value;
        ASM("mov %0, r0" : "=r"(value));
        return value;
    }
    static void fr(const Reg32 & fr) {
        ASM("mov r0, %0" : : "r"(fr) : "r0");
    }

    static Log_Addr ip() // due to RISC pipelining PC is read with a +8 (4 for thumb) offset
    {
        Reg32 value;
        ASM("mov %0, pc" : "=r"(value) : :);
        return value;
    }

    static Reg32 pdp() { return 0; }
    static void pdp(const Reg32 & pdp) {}

    template <typename T>
    static T tsl(volatile T & lock) {
        register T old;
        register T one = 1;
        ASM("1: ldrexb  %0, [%1]        \n"
            "   strexb  r4, %2, [%1]    \n"
            "   cmp     r4, #0          \n"
            "   bne     1b              \n" : "=&r" (old) : "r"(&lock), "r"(one) : "r4" );
        return old;
    }

    template <typename T>
    static T finc(volatile T & value) {
        T old;
        static bool lock = false;
        while(tsl(lock));
        old = value;
        value++;
        lock = false;
        return old;
    }

    template <typename T>
    static T fdec(volatile T & value) {
        T old;
        static bool lock = false;
        while(tsl(lock));
        old = value;
        value--;
        lock = false;
        return old;
    }

    template <typename T>
    static T cas(volatile T & value, T compare, T replacement) {
        static bool lock = false;
        while(tsl(lock));
        if(value == compare)
            value = replacement;
        lock = false;
        return compare;
   }

    static Reg32 htonl(Reg32 v) { return swap32(v); }
    static Reg16 htons(Reg16 v) { return swap16(v); }
    static Reg32 ntohl(Reg32 v) { return swap32(v); }
    static Reg16 ntohs(Reg16 v) { return swap16(v); }

    template<typename ... Tn>
    static Context * init_stack(const Log_Addr & usp, Log_Addr sp, void (* exit)(), int (* entry)(Tn ...), Tn ... an) {
        sp -= sizeof(Context);
        Context * ctx = new(sp) Context(entry, exit);
        init_stack_helper(&ctx->_r0, an ...);
        return ctx;
    }

    template<typename ... Tn>
    static Log_Addr init_user_stack(Log_Addr sp, void (* exit)(), Tn ... an) {
        sp -= sizeof(Context);
        Context * ctx = new(sp) Context(0, exit);
        init_stack_helper(&ctx->_r0, an ...);
        return sp;
    }

public:
    // ARMv7 specific methods
    static unsigned int int_id() { return flags() & 0x3f; }

private:
    template<typename Head, typename ... Tail>
    static void init_stack_helper(Log_Addr sp, Head head, Tail ... tail) {
        *static_cast<Head *>(sp) = head;
        init_stack_helper(sp + sizeof(Head), tail ...);
    }
    static void init_stack_helper(Log_Addr sp) {}

    static void init();

private:
    static unsigned int _cpu_clock;
    static unsigned int _bus_clock;
};

inline CPU::Reg32 htonl(CPU::Reg32 v) { return CPU::htonl(v); }
inline CPU::Reg16 htons(CPU::Reg16 v) { return CPU::htons(v); }
inline CPU::Reg32 ntohl(CPU::Reg32 v) { return CPU::ntohl(v); }
inline CPU::Reg16 ntohs(CPU::Reg16 v) { return CPU::ntohs(v); }

__END_SYS

#endif
