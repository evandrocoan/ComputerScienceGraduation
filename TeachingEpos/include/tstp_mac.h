// EPOS TSTP MAC Mediator Common Package

#include <nic.h>

#ifndef __tstp_mac_h
#define __tstp_mac_h

#include <tstp_common.h>
#include <utility/list.h>
#include <utility/observer.h>
#include <utility/buffer.h>
#include <utility/random.h>

__BEGIN_SYS

class TSTP_MAC: private NIC_Common, public TSTP_Common
{
public:
    //TODO: Traits
    static const unsigned int PERIOD = 225000;
    static const unsigned int Tu = 192; // IEEE 802.15.4 TX Turnaround Time
    static const unsigned int G = Tu + 128; // Tu + 8 / symbol_rate
    //static const unsigned int Ts = 480; // Time to send a single microframe (including PHY headers)
    static const unsigned int Ts = 580 - Tu; // Time to send a single microframe (including PHY headers)
    static const unsigned int MICROFRAME_TIME = Ts;
    static const unsigned int MIN_Ti = 2*Tu; // Minimum time between consecutive microframes
    static const unsigned int RADIO_RADIUS = 17 * 100; //TODO
    static const unsigned int TX_UNTIL_PROCESS_DATA_DELAY = 0;//5100; //TODO
    static const unsigned int DATA_SKIP_TIME = 4500;

    // == Calculated parameters ==
    static const unsigned int N_MICROFRAMES = ((PERIOD / (MIN_Ti + Ts)) > 255) ? 255 : (PERIOD / (MIN_Ti + Ts));
    //static const unsigned int N_MICROFRAMES = 20;
    static const unsigned int Ti = (PERIOD / N_MICROFRAMES) - Ts;
    static const unsigned int TIME_BETWEEN_MICROFRAMES = Ti;
    static const unsigned int DATA_LISTEN_MARGIN = TIME_BETWEEN_MICROFRAMES; // Subtract this amount when calculating time until data transmission
    static const unsigned int RX_MF_TIMEOUT = 2*Ts + 2*TIME_BETWEEN_MICROFRAMES;
    static const unsigned int SLEEP_PERIOD = PERIOD - RX_MF_TIMEOUT;
    static const unsigned int DUTY_CYCLE = (RX_MF_TIMEOUT * 100000) / PERIOD; //ppm

    static const unsigned int RX_DATA_TIMEOUT = DATA_SKIP_TIME + DATA_LISTEN_MARGIN + 4 * (MICROFRAME_TIME + TIME_BETWEEN_MICROFRAMES);
    static const unsigned int CCA_TIME = (2 * MICROFRAME_TIME + TIME_BETWEEN_MICROFRAMES) > 256 ? (2 * MICROFRAME_TIME + TIME_BETWEEN_MICROFRAMES) : 256;

    typedef NIC_Common::CRC16 CRC;
    typedef CPU::Reg16 Frame_ID;
    typedef TSTP_Common::Microsecond Time;

    // Just to comply with EPOS' NIC interface
    typedef NIC_Common::Address<1> Address;
    enum PROTOCOL { 
        IP     = 0x0800,
        ARP    = 0x0806,
        RARP   = 0x8035,
        TSTP   = 0x8401,
        ELP    = 0x8402,
        PTP    = 0x88F7
    };
    typedef NIC_Common::Protocol Protocol;

    // TSTP MAC Frame
    static const unsigned int MTU = 127;// - sizeof(CRC); // CRC Disabled for Multimedia MAC
    typedef unsigned char Data[MTU];

    class Frame: public Header
    {
    public:
        Frame() : Header(INTEREST) {}

        Header * header() { return this; }

        template<typename T>
        T * data() { return reinterpret_cast<T *>(header()); } // Header is shared between MAC and higher layers

        friend Debug & operator<<(Debug & db, const Frame & p) {
            db << "{h=" << reinterpret_cast<const Header &>(p) << ",d=" << p._data << "}";
            return db;
        }

    private:
        Data _data;
        //CRC _crc; // CRC Disabled for Multimedia MAC
    } __attribute__((packed));

    typedef Frame PDU;

    typedef _UTIL::Buffer<NIC, TSTP_MAC::Frame, void> Buffer;

    // Observers of a protocol get a also a pointer to the received buffer
    typedef Data_Observer<Buffer, Protocol> Observer;
    typedef Data_Observed<Buffer, Protocol> Observed;

    typedef NIC_Common::Statistics Statistics;

    typedef unsigned char Count;

    class Microframe {
    public:
        Microframe() {}

        Microframe(bool all_listen, const Frame_ID & id, const Count & count, const Distance & hint) : 
            _al_id((id & 0x7fff) | (static_cast<unsigned int>(all_listen) << 15)), _count(count), _hint(hint) {}

        Microframe(bool all_listen, const Frame_ID & id, const Count & count) : 
            _al_id((id & 0x7fff) | (static_cast<unsigned int>(all_listen) << 15)), _count(count), _hint(0) {}

        Count count() const { return _count; }
        Count dec_count() { Count ret = _count--; return ret; }

        Frame_ID id() const { return _al_id & 0x7fff; }
        void id(Frame_ID id) { _al_id  = all_listen() | (id & 0x7fff); }

        void all_listen(bool all_listen) { _al_id = id() | (1 << 15); }
        bool all_listen() const { return _al_id & ~(0x7fff); }

        Distance hint() const { return _hint; }
        void hint(const Distance & h) { _hint = h; }

        friend OStream & operator<<(OStream & db, const Microframe & m) {
            db << "{al=" << m.all_listen() << ",id=" << m.id() << ",c=" << m._count << ",h=" << m._hint << "}";//<< ",crc=" << m._crc << "}"; // CRC Disabled for Multimedia MAC
            return db;
        }
        friend Debug & operator<<(Debug & db, const Microframe & m) {
            db << "{al=" << m.all_listen() << ",id=" << m.id() << ",c=" << m._count << ",h=" << m._hint << "}";//",crc=" << m._crc << "}"; // CRC Disabled for Multimedia MAC
            return db;
        }
    private:
        unsigned short _al_id; // all_listen : 1 
                               // id : 15
        Count _count;
        Distance _hint;
        //CRC _crc; // CRC Disabled for Multimedia MAC
    } __attribute__((packed));

protected:
    enum STATE {
        CHECK_TX_SCHEDULE,
        SLEEP_S,
        RX_MF,
        SLEEP_UNTIL_DATA,
        RX_DATA,
        OFFSET,
        CCA,
        TX_MF,
        TX_DATA        
    };

    TSTP_MAC() {}

public:
    static const unsigned int mtu() { return MTU; }
    static const Address broadcast() { return Address::BROADCAST; }
};

__END_SYS

#endif
