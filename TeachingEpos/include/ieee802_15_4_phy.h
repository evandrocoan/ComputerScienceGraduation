// EPOS IEEE 802.15.4 PHY Layer Common Package

#ifndef __ieee802_15_4_phy_h
#define __ieee802_15_4_phy_h

#include <cpu.h>
#include <utility/buffer.h>

__BEGIN_SYS

// PHY Layer definitions
class IEEE802_15_4_PHY 
{
    typedef CPU::Reg8 Reg8;

public:
    typedef CPU::Reg16 CRC;

    // The IEEE 802.15.4 PHR
    class Header
    {
    public:
        Header() {};
        Header(Reg8 len) : _frame_length((len & (~0x7f)) ? 0x7f : len) {};

        Reg8 frame_length() const { return _frame_length; }
        void frame_length(Reg8 len) { _frame_length = ((len & (~0x7f)) ? 0x7f : len); }

    protected:
        Reg8 _frame_length;
    } __attribute__((packed, may_alias));

    static const unsigned int MTU = 127;
    typedef unsigned char Data[MTU];
    
    // The IEEE 802.15.4 PHY Frame
    class Frame: public Header
    {
    public:
        Frame() {}
        Frame(Reg8 payload_size) : Header(payload_size) {}
        Frame(const void * dat, Reg8 payload_size) : Header(payload_size)
        {
            memcpy(_data, dat, _frame_length);
        }

        Header * header() { return this; }

        template<typename T>
        T * data() { return reinterpret_cast<T *>(&_data); }

        friend Debug & operator<<(Debug & db, const Frame & f) {
            db << "{" << f._data << "}";
            return db;
        }

    protected:
        Data _data;
    } __attribute__((packed, may_alias));
};

__END_SYS

#endif
