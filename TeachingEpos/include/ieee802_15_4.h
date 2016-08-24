// EPOS IEEE 802.15.4 Mediator Common Package

#include <nic.h>

#ifndef __ieee802_15_4_h
#define __ieee802_15_4_h

#include <cpu.h>
#include <utility/list.h>
#include <utility/observer.h>
#include <utility/buffer.h>
#include <ieee802_15_4_phy.h>

__BEGIN_SYS

class IEEE802_15_4: private NIC_Common, public IEEE802_15_4_PHY
{
public:
    typedef NIC_Common::Address<2> Short_Address;
    typedef NIC_Common::Address<8> Extended_Address;
    typedef Short_Address Address;
    typedef CPU::Reg8 Reg8;
    typedef CPU::Reg16 Reg16;
    typedef IEEE802_15_4_PHY::CRC CRC;

    // Frame types
    enum Frame_Type
    {
        BEACON  = 0,
        DATA    = 1,
        ACK     = 2,
        MAC_CMD = 3,
    };
    enum Addressing_Mode
    {
        ADDR_MODE_NOT_PRESENT = 0,
        ADDR_MODE_SHORT_ADDR  = 2,
        ADDR_MODE_EXT_ADDR    = 4,
    };
    enum
    {
        PAN_ID_BROADCAST = 0xffff
    };

    typedef unsigned short Protocol;
    enum
    {
        IP     = 0x0800,
        ARP    = 0x0806,
        RARP   = 0x8035,
        TSTP   = 0x8401,
        ELP    = 0x8402,
        PTP    = 0x88F7
    };

    typedef IEEE802_15_4_PHY::Header Phy_Header;

    // The IEEE 802.15.4 MHR
    // 802.15.4 headers can have variable format, for now this only
    // supports a simple, fixed format
    // Adding the Protocol field for compatibility with ethernet/nic headers
    class Header : public Phy_Header
    {
    public:
        // Frame Control field that goes inside MHR
        // TODO: This class assumes that the machine is little-endian
        class Frame_Control
        {
            public:
                // TODO: For now, we'll only support data frames
                // TODO: This order assumes that the machine is little-endian
                Frame_Control() :
                    _frame_type(DATA),
                    _security_enabled(0),
                    _frame_pending(0),
                    _ar(0),
                    _pan_id_compression(1),
                    _reserved(0),
                    _dst_addressing_mode(ADDR_MODE_SHORT_ADDR),
                    _frame_version(0),
                    _src_addressing_mode(ADDR_MODE_SHORT_ADDR)
            { }

                bool frame_pending() const { return _frame_pending; }
                unsigned char frame_type() const { return _frame_type; }
                void ack_request(bool val) { _ar = val; }
                bool ack_request() { return _ar; }

            protected:
                // TODO: This order assumes that the machine is little-endian
                unsigned _frame_type : 3;
                unsigned _security_enabled : 1;
                unsigned _frame_pending : 1;
                unsigned _ar : 1;
                unsigned _pan_id_compression: 1;
                unsigned _reserved : 3;
                unsigned _dst_addressing_mode : 2;
                unsigned _frame_version : 2;
                unsigned _src_addressing_mode : 2;
        } __attribute__((packed, may_alias));

        Header() {}
        Header(Reg8 payload_size): Phy_Header(payload_size + sizeof(Header) - sizeof(Phy_Header)) {};
        Header(const Short_Address & src, const Short_Address & dst, const Protocol & prot): Phy_Header(), _frame_control(), _dst(dst), _src(src), _prot(htons(prot)) {}
        Header(Reg8 payload_size, const Short_Address & src, const Short_Address & dst, const Protocol & prot):
            Phy_Header(payload_size+sizeof(Header)-sizeof(Phy_Header)), _frame_control(), _sequence_number(0), _dst_pan_id(PAN_ID_BROADCAST), _dst(dst), _src(src), _prot(htons(prot)) {}

        friend Debug & operator<<(Debug & db, const Header & h) {
            db << "{" << h._dst << "," << h._src << "," << h.prot() << "}";
            return db;
        }

        const Address & src() const { return _src; }
        const Address & dst() const { return _dst; }

        Protocol prot() const { return ntohs(_prot); }

        void sequence_number(Reg8 seq) { _sequence_number = seq; }
        Reg8 sequence_number() { return _sequence_number; }

        bool frame_pending() const { return _frame_control.frame_pending(); }
        unsigned char frame_type() const { return _frame_control.frame_type(); }
        void ack_request(bool val) { _frame_control.ack_request(val); }
        bool ack_request() { return _frame_control.ack_request(); }

    public:
    //protected:
        Frame_Control _frame_control;
        Reg8 _sequence_number;
        Reg16 _dst_pan_id;
        Short_Address _dst;
        Short_Address _src;
        Protocol _prot; // TODO: this is not part of 802.15.4
    } __attribute__((packed, may_alias));

private:
    static const unsigned int HEADER_SIZE = sizeof(Header) - sizeof(Phy_Header);
    static const unsigned int FOOTER_SIZE = sizeof(CRC);

public:
    static const unsigned int MTU = IEEE802_15_4_PHY::MTU - HEADER_SIZE - FOOTER_SIZE;
    typedef unsigned char Data[MTU];


    // The IEEE 802.15.4 Frame
    class Frame: public Header
    {
    public:
        Frame() {}
        Frame(const Address & src, const Address & dst, const Protocol & prot, Reg8 payload_size) 
            : Header(payload_size + FOOTER_SIZE, src, dst, prot) {}
        Frame(const Address & src, const Address & dst, const Protocol & prot, const void * dat, Reg8 payload_size)
            : Header(payload_size + FOOTER_SIZE, src, dst, prot)
        {
            data(dat);
        }

        void data(const void * d) {
            memcpy(_data, d, frame_length() - HEADER_SIZE - FOOTER_SIZE);
        }

        Header * header() { return this; }

        template<typename T>
        T * data() { return reinterpret_cast<T *>(&_data); }

        friend Debug & operator<<(Debug & db, const Frame & f) {
            db << "{" << f.dst() << "," << f.src() << "," << f.prot() << "," << f._data << "}";
            return db;
        }

    protected:
        Data _data;
        CRC _crc;
    } __attribute__((packed, may_alias));

    typedef Frame PDU;

    // Buffers used to hold frames across a zero-copy network stack
    typedef _UTIL::Buffer<NIC, Frame, void> Buffer;

public:
    // Observers of a protocol get a also a pointer to the received buffer
    typedef Data_Observer<Buffer, Protocol> Observer;
    typedef Data_Observed<Buffer, Protocol> Observed;

    // Meaningful statistics for Ethernet
    struct Statistics: public NIC_Common::Statistics
    {
        Statistics(): rx_overruns(0), tx_overruns(0), frame_errors(0), carrier_errors(0), collisions(0) {}

        friend Debug & operator<<(Debug & db, const Statistics & s) {
            db << "{rxp=" << s.rx_packets
               << ",rxb=" <<  s.rx_bytes
               << ",rxorun=" <<  s.rx_overruns
               << ",txp=" <<  s.tx_packets
               << ",txb=" <<  s.tx_bytes
               << ",txorun=" <<  s.tx_overruns
               << ",frm=" <<  s.frame_errors
               << ",car=" <<  s.carrier_errors
               << ",col=" <<  s.collisions
               << "}";
            return db;
        }

        unsigned int rx_overruns;
        unsigned int tx_overruns;
        unsigned int frame_errors;
        unsigned int carrier_errors;
        unsigned int collisions;
    };

protected:
    IEEE802_15_4() {}

public:
    static const unsigned int mtu() { return MTU; }
    static const Address broadcast() { return Address::BROADCAST; }
};

__END_SYS

#endif
