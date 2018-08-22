// EPOS CPU Mediator Common Package

#ifndef __cpu_h
#define __cpu_h

#include <system/config.h>

__BEGIN_SYS

class CPU_Common
{	
protected:
    static const bool BIG_ENDIAN = (Traits<CPU>::ENDIANESS == Traits<CPU>::BIG);

protected:
    CPU_Common() {}

public:
    typedef unsigned char Reg8;
    typedef unsigned short Reg16;
    typedef unsigned long Reg32;
    typedef unsigned long long Reg64;
    typedef unsigned long Reg;

    class Log_Addr
    {
    public:
        Log_Addr() {}
        Log_Addr(const Log_Addr & a) : _addr(a._addr) {}
        Log_Addr(const Reg & a) : _addr(a) {}
        template<typename T>
        Log_Addr(T * a) : _addr(Reg(a)) {}

        operator const Reg &() const { return _addr; }

        template<typename T>
        operator T *() const { return reinterpret_cast<T *>(_addr); }

        template<typename T>
        bool operator==(T a) const { return (_addr == Reg(a)); }
        template<typename T>
        bool operator< (T a) const { return (_addr < Reg(a)); }
        template<typename T>
        bool operator> (T a) const { return (_addr > Reg(a)); }
        template<typename T>
        bool operator>=(T a) const { return (_addr >= Reg(a)); }
        template<typename T>
        bool operator<=(T a) const { return (_addr <= Reg(a)); }

        template<typename T>
        Log_Addr operator-(T a) const { return _addr - Reg(a); }
        template<typename T>
        Log_Addr operator+(T a) const { return _addr + Reg(a); }
        template<typename T>
        Log_Addr & operator+=(T a) { _addr += Reg(a); return *this; }
        template<typename T>
        Log_Addr & operator-=(T a) { _addr -= Reg(a); return *this; }
        template<typename T>
        Log_Addr & operator&=(T a) { _addr &= Reg(a); return *this; }
        template<typename T>
        Log_Addr & operator|=(T a) { _addr |= Reg(a); return *this; }

        Log_Addr & operator[](int i) { return *(this + i); }

        friend OStream & operator<<(OStream & os, const Log_Addr & a) { os << reinterpret_cast<void *>(a._addr); return os; }

    private:
        Reg _addr;
    };

    typedef Log_Addr Phy_Addr;

    typedef unsigned long Hertz;

    class Context;

public:
    static void halt() { for(;;); }

    /**
     * TSL - Test and Set Lock
     * https://en.wikipedia.org/wiki/Test-and-set
     *
     * Some instruction sets have an atomic test-and-set machine language instruction. Examples
     * include x86[3] and IBM System/360 and its successors (including z/Architecture).[4] Those
     * that do not can still implement an atomic test-and-set using a read-modify-write or
     * compare-and-swap instruction.
     *
     * The test and set instruction, when used with boolean values, uses logic like that shown in
     * the following function, except that the function must execute atomically. That is, no other
     * process must be able to interrupt the function mid-execution, thereby seeing a state that
     * only exists while the function executes. That requires hardware support; it cannot be
     * implemented as shown. Nevertheless, the code shown helps to explain the behaviour of
     * test-and-set. NOTE: In this example, 'lock' is assumed to be passed by reference (or by name)
     * but the assignment to 'initial' creates a new value (not just copying a reference).
     *
     *  function TestAndSet(boolean_ref lock) {
     *      boolean initial = lock;
     *      lock = true;
     *      return initial;
     *  }
     *
     * Not only is the code shown not atomic, in the sense of the test-and-set instruction, it also
     * differs from the descriptions of DPRAM hardware test-and-set above. Here, the value being set
     * and the test are fixed and invariant, and the value is updated regardless of the outcome of
     * the test, whereas for the DPRAM test-and-set, the memory is set only when the test succeeds,
     * and the value to set and the test condition are specified by the CPU. Here, the value to set
     * can only be 1, but if 0 and 1 are considered the only valid values for the memory location,
     * and "value is nonzero" is the only allowed test, then this equates to the case described for
     * DPRAM hardware (or, more specifically, the DPRAM case reduces to this under these
     * constraints). From that viewpoint, this can, correctly, be called "test-and-set" in the full,
     * conventional sense of that term. The essential point to note is the general intent and
     * principle of test-and-set: a value is both tested and set in one atomic operation such that
     * no other program thread or process can change the target memory location after it is tested
     * but before it is set. (This is because the location must only be set if it currently has a
     * certain value, not if it had that value sometime earlier.)
     */
    static bool tsl(volatile bool & lock) {
        bool old = lock;
        lock = 1;
        return old;
    }

    static int finc(volatile int & value) {
        int old = value;
        value++;
        return old;
    }

    static int fdec(volatile int & value) {
        int old = value;
        value--;
        return old;
    }

    static int cas(volatile int & value, int compare, int replacement) {
        int old = value;
        if(value == compare) {
            value = replacement;
        }
        return old;
    }

    static Reg32 htonl(Reg32 v) { return (BIG_ENDIAN) ? v : swap32(v); }
    static Reg16 htons(Reg16 v) { return (BIG_ENDIAN) ? v : swap16(v); }
    static Reg32 ntohl(Reg32 v) { return htonl(v); }
    static Reg16 ntohs(Reg16 v) { return htons(v); }

protected:
    static Reg32 swap32(Reg32 v) {
        return (v & 0xff000000) >> 24 | (v & 0x00ff0000) >> 8 | (v & 0x0000ff00) << 8 | (v & 0x000000ff) << 24;
    }

    static Reg16 swap16(Reg16 v) {
        return (v & 0xff00) >> 8 | (v & 0x00ff) << 8;
    }
};

__END_SYS

#ifdef __CPU_H
#include __CPU_H
#endif

__BEGIN_SYS

template<typename T>
inline T align32(const T & addr) { return (addr + 3) & ~3U; }
template<typename T>
inline T align64(const T & addr) { return (addr + 7) & ~7U; }
template<typename T>
inline T align128(const T & addr) { return (addr + 15) & ~15U; }

__END_SYS

#endif
