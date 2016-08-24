// EPOS Trustful SpaceTime Protocol Common Declarations

#ifndef __tstp_common_h
#define __tstp_common_h

#include <utility/geometry.h>
#include <utility/id.h>
#include <rtc.h>

__BEGIN_SYS

class TSTP_Common
{
public:
    static const unsigned int PAN = 10; // Nodes
    static const unsigned int SAN = 100; // Nodes
    static const unsigned int LAN = 10000; // Nodes
    static const unsigned int NODES = Traits<Build>::NODES;

    // Version
    // This field is packed first and matches the Frame Type field in the Frame Control in IEEE 802.15.4 MAC.
    // A version number above 4 renders TSTP into the reserved frame type zone and should avoid interference.
    enum Version {
        V0 = 4
    };

    // Packet Types
    enum Type {
        INTEREST  = 0,
        RESPONSE  = 1,
        COMMAND   = 2,
        CONTROL   = 3
    };

    // Control packet subtypes
    enum Control_Type {
        DH_REQUEST = 0,
        DH_RESPONSE,
        AUTH_REQUEST,
        AUTH_GRANTED,
    };

    // Scale for local network's geographic coordinates
    enum Scale {
        CMx50_8  = 0,
        CM_16    = 1,
        CMx25_16 = 2,
        CM_32    = 3
    };
    static const Scale SCALE = (NODES <= PAN) ? CMx50_8 : (NODES <= SAN) ? CM_16 : (NODES <= LAN) ? CMx25_16 : CM_32;

    // Time
    //typedef RTC::Microsecond Microsecond;
    typedef long long Time; // microseconds
    typedef Time Microsecond; // microseconds
    typedef unsigned long Time_Offset; // microseconds
    typedef unsigned long Time_Stamp; // least significant bytes from TSTP_Timer's time stamp (8 MHz)
    typedef unsigned long Time_Stamp_Offset; // 8 MHz

    // Geographic Coordinates
    template<Scale S>
    struct _Coordinates: public Point<char, 3>
    {
        typedef char Number;

        _Coordinates(Number x = 0, Number y = 0, Number z = 0): Point<Number, 3>(x, y, z) {}
    } __attribute__((packed));
    typedef _Coordinates<SCALE> Coordinates;

    typedef long Distance;

    // Geographic Region in a time interval (not exactly Spacetime, but ...)
    template<Scale S>
    struct _Region: public Sphere<typename _Coordinates<S>::Number>
    {
        typedef typename _Coordinates<S>::Number Number;
        typedef Sphere<Number> Base;

        _Region(const Coordinates & c, const Number & r, const Time & _t0, const Time & _t1): Base(c, r), t0(_t0), t1(_t1) {}

        bool contains(const Coordinates & c, const Time & t) const { return ((Base::center - c) <= Base::radius) && ((t >= t0) && (t <= t1)); }

        friend Debug & operator<<(Debug & db, const _Region & r) {
            db << "{" << reinterpret_cast<const Base &>(r) << ",t0=" << r.t0 << ",t1=" << r.t1 << "}";
            return db;
        }

        Time t0;
        Time t1;
    } __attribute__((packed));
    typedef _Region<SCALE> Region;

    // Packet Header
    template<Scale S>
    class _Header
    {
        // Format
        // Bit   7    5    3  2    0                0         0         0         0         0         0         0         0
        //     +------+----+--+----+----------------+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+
        //     | ver  |type|tr|scal|   confidence   | elapsed |   o.x   |   o.y   |   o.z   |   l.x   |   l.y   |   l.z   |
        //     +------+----+--+----+----------------+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+--- ~ ---+
        // Bits          8                  8            32     8/16/32   8/16/32   8/16/32   8/16/32   8/16/32   8/16/32

    public:
        _Header() {}
        _Header(const Type & t, bool tr = false, unsigned char c = 0, const Coordinates & o = 0, const Coordinates & l = 0, const Time_Stamp & lht = 0, const Time_Stamp_Offset & e = 0, const Version & v = V0)
        : _config(v << 5 | t << 3 | tr << 2 | S), _confidence(c), _origin(o), _last_hop(l), _last_hop_time(lht), _elapsed(e) {}

        Version version() const { return static_cast<Version>((_config >> 5) & 0x07); }
        void version(const Version & v) { _config = (_config & 0x1f) | (v << 5); }

        Type type() const { return static_cast<Type>((_config >> 3) & 0x03); }
        void type(const Type & t) { _config = (_config & 0xe4) | (t << 3); }

        bool time_request() const { return (_config >> 2) & 0x01; }
        void time_request(bool tr) { _config = (_config & 0xfb) | (tr << 2); }

        Scale scale() const { return static_cast<Scale>(_config & 0x03); }
        void scale(const Scale & s) { _config = (_config & 0xfc) | s; }

        Time_Stamp_Offset elapsed() const { return _elapsed; }
        void elapsed(const Time_Stamp_Offset & e) { _elapsed = e; }

        const Coordinates & origin() const { return _origin; }
        void origin(const Coordinates & c) { _origin = c; }

        const Coordinates & last_hop() const { return _last_hop; }
        void last_hop(const Coordinates & c) { _last_hop = c; }

        Time_Stamp last_hop_time() const { return _last_hop_time; }
        void last_hop_time(const Time_Stamp & lht) { _last_hop_time = lht; }

        friend Debug & operator<<(Debug & db, const _Header & h) {
            db << "{v=" << h.version() - V0 << ",t=" << ((h.type() == INTEREST) ? 'I' :  (h.type() == RESPONSE) ? 'R' : (h.type() == COMMAND) ? 'C' : 'P') << ",tr=" << h.time_request() << ",s=" << h.scale() << ",e=" << h._elapsed << ",o=" << h._origin << ",l=" << h._last_hop << "}";
            return db;
        }

    protected:
        unsigned char _config;
        unsigned char _confidence;
        Coordinates _origin;
        Coordinates _last_hop;
        Time_Stamp _last_hop_time;
        Time_Stamp_Offset _elapsed;
    } __attribute__((packed));
    typedef _Header<SCALE> Header;

    // Control Message extended Header
    class Control: public Header
    {
    public:
        unsigned char subtype() { return _type; }

    protected:
        Control(const Control_Type & t, bool tr, unsigned char c, const Coordinates & o, const Coordinates & l, const Time_Stamp & lht, const Time_Stamp_Offset & e)
        : Header(CONTROL, tr, c, o, l, lht, e), _type(t) {}

        friend Debug & operator<<(Debug & db, const Control & m) {
            db << reinterpret_cast<const Header &>(m) << ",t=" << m._type;
            return db;
        }

        unsigned char _type;
    } __attribute__((packed));

    typedef EPOS::S::Diffie_Hellman<16> Diffie_Hellman;
    typedef _UTIL::ID<16> Auth;

    typedef struct OTP { 
        char otp[16]; 
        friend Debug & operator<<(Debug & db, const OTP & o) { return db; }
        bool operator==(const OTP & rhs) {
            for(int i = 0; i < 16; i++) {
                if(rhs.otp[i] != otp[i])
                    return false;
            }
            return true;
        }
    } OTP;


    // TSTP encodes SI Units similarly to IEEE 1451 TEDs
    class Unit
    {
    public:
        // Formats
        // Bit       31                                 16                                     0
        //         +--+----------------------------------+-------------------------------------+
        // Digital |0 | type                             | dev                                 |
        //         +--+----------------------------------+-------------------------------------+

        // Bit       31   29   27     24     21     18     15     12      9      6      3      0
        //         +--+----+----+------+------+------+------+------+------+------+------+------+
        // SI      |1 |NUM |MOD |sr+4  |rad+4 |m+4   |kg+4  |s+4   |A+4   |K+4   |mol+4 |cd+4  |
        //         +--+----+----+------+------+------+------+------+------+------+------+------+
        // Bits     1   2    2     3      3      3      3      3      3      3      3      3


        // Valid values for field SI
        enum {
            DIGITAL = 0 << 31, // The Unit is plain digital data. Subsequent 15 bits designate the data type. Lower 16 bits are application-specific, usually a device selector.
            SI      = 1 << 31  // The Unit is SI. Remaining bits are interpreted as specified here.
        };

        // Valid values for field NUM
        enum {
            I32 = 0 << 29, // Value is an integral number stored in the 32 last significant bits of a 32-bit big-endian integer.
            I64 = 1 << 29, // Value is an integral number stored in the 64 last significant bits of a 64-bit big-endian integer.
            F32 = 2 << 29, // Value is a real number stored as an IEEE 754 binary32 big-endian floating point.
            D64 = 3 << 29, // Value is a real number stored as an IEEE 754 binary64 big-endian doulbe precision floating point.
            NUM = D64      // AND mask to select NUM bits
        };

        // Valid values for field MOD
        enum {
            DIR     = 0 << 27, // Unit is described by the product of SI base units raised to the powers recorded in the remaining fields.
            DIV     = 1 << 27, // Unit is U/U, where U is described by the product SI base units raised to the powers recorded in the remaining fields.
            LOG     = 2 << 27, // Unit is log_e(U), where U is described by the product of SI base units raised to the powers recorded in the remaining fields.
            LOG_DIV = 3 << 27, // Unit is log_e(U/U), where U is described by the product of SI base units raised to the powers recorded in the remaining fields.
            MOD = D64          // AND mask to select MOD bits
        };

        // Masks to select the SI units
        enum {
            SR      = 7 << 24,
            RAD     = 7 << 21,
            M       = 7 << 18,
            KG      = 7 << 15,
            S       = 7 << 12,
            A       = 7 <<  9,
            K       = 7 <<  6,
            MOL     = 7 <<  3,
            CD      = 7 <<  0
        };

        // Typical SI Quantities
        enum Quantity {
             //                        si      | mod       | sr            | rad           |  m            |  kg           |  s            |  A            |  K            |  mol          |  cd
             Length                  = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 1) << 18 | (4 + 0) << 15 | (4 + 0) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0),
             Mass                    = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 0) << 18 | (4 + 1) << 15 | (4 + 0) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0),
             Time                    = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 0) << 18 | (4 + 0) << 15 | (4 + 1) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0),
             Current                 = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 0) << 18 | (4 + 0) << 15 | (4 + 0) << 12 | (4 + 1) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0),
             Electric_Current        = Current,
             Temperature             = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 0) << 18 | (4 + 0) << 15 | (4 + 0) << 12 | (4 + 0) << 9  | (4 + 1) << 6  | (4 + 0) << 3  | (4 + 0),
             Amount_of_Substance     = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 0) << 18 | (4 + 0) << 15 | (4 + 0) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 1) << 3  | (4 + 0),
             Liminous_Intensity      = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 0) << 18 | (4 + 0) << 15 | (4 + 0) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 1),
             Area                    = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 2) << 18 | (4 + 0) << 15 | (4 + 0) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0),
             Volume                  = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 3) << 18 | (4 + 0) << 15 | (4 + 0) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0),
             Speed                   = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 1) << 18 | (4 + 0) << 15 | (4 - 1) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0),
             Velocity                = Speed,
             Acceleration            = 1 << 31 | DIR << 27 | (4 + 0) << 24 | (4 + 0) << 21 | (4 + 1) << 18 | (4 + 0) << 15 | (4 - 2) << 12 | (4 + 0) << 9  | (4 + 0) << 6  | (4 + 0) << 3  | (4 + 0)
         };

        // SI Factors
        typedef char Factor;
        enum {
         // Name           Code         Symbol    Factor
            ATTO        = (8 - 8), //     a       0.000000000000000001
            FEMTO       = (8 - 7), //     f       0.000000000000001
            PICO        = (8 - 6), //     p       0.000000000001
            NANO        = (8 - 5), //     n       0.000000001
            MICRO       = (8 - 4), //     μ       0.000001
            MILI        = (8 - 3), //     m       0.001
            CENTI       = (8 - 2), //     c       0.01
            DECI        = (8 - 1), //     d       0.1
            NONE        = (8    ), //     -       1
            DECA        = (8 + 1), //     da      10
            HECTO       = (8 + 2), //     h       100
            KILO        = (8 + 3), //     k       1000
            MEGA        = (8 + 4), //     M       1000000
            GIGA        = (8 + 5), //     G       1000000000
            TERA        = (8 + 6), //     T       1000000000000
            PETA        = (8 + 7)  //     P       1000000000000000
        };


        template<int N>
        struct Get { typedef typename SWITCH<N, CASE<I32, long, CASE<I64, long long, CASE<DEFAULT, long>>>>::Result Type; };

        template<typename T>
        struct GET { enum { NUM = I32 }; };

    public:
        Unit(unsigned long u) { _unit = u; }

        operator unsigned long() const { return _unit; }

        int sr()  const { return ((_unit & SR)  >> 24) - 4 ; }
        int rad() const { return ((_unit & RAD) >> 21) - 4 ; }
        int m()   const { return ((_unit & M)   >> 18) - 4 ; }
        int kg()  const { return ((_unit & KG)  >> 15) - 4 ; }
        int s()   const { return ((_unit & S)   >> 12) - 4 ; }
        int a()   const { return ((_unit & A)   >>  9) - 4 ; }
        int k()   const { return ((_unit & K)   >>  6) - 4 ; }
        int mol() const { return ((_unit & MOL) >>  3) - 4 ; }
        int cd()  const { return ((_unit & CD)  >>  0) - 4 ; }

        friend Debug & operator<<(Debug & db, const Unit & u) {
            if(u & SI) {
                db << "{SI";
                switch(u & MOD) {
                case DIR: break;
                case DIV: db << "[U/U]"; break;
                case LOG: db << "[log(U)]"; break;
                case LOG_DIV: db << "[log(U/U)]";
                };
                switch(u & NUM) {
                case I32: db << ".I32"; break;
                case I64: db << ".I64"; break;
                case F32: db << ".F32"; break;
                case D64: db << ".D64";
                }
                db << ':';
                if(u.sr())
                    db << "sr^" << u.sr();
                if(u.rad())
                    db << "rad^" << u.rad();
                if(u.m())
                    db << "m^" << u.m();
                if(u.kg())
                    db << "kg^" << u.kg();
                if(u.s())
                    db << "s^" << u.s();
                if(u.a())
                    db << "A^" << u.a();
                if(u.k())
                    db << "K^" << u.k();
                if(u.mol())
                    db << "mol^" << u.mol();
                if(u.cd())
                    db << "cdr^" << u.cd();
            } else
                db << "{D:" << "l=" <<  (u >> 16);
            db << "}";
            return db;
        }

    private:
        unsigned long _unit;
    } __attribute__((packed));

    // SI values (either integer32, integer64, float32, double64)
    template<int NUM>
    class Value
    {
    public:
        Value(long int v): _value(v) {}

        operator long int() { return _value; }

    private:
        long int _value;
    };

    // Precision or Error in SI values, expressed as 10^Error
    typedef char Precision;
    typedef char Error;
};

template<>
struct TSTP_Common::_Coordinates<TSTP_Common::CM_16>: public Point<short, 3>
{
    typedef short Number;

    _Coordinates(Number x = 0, Number y = 0, Number z = 0): Point<Number, 3>(x, y, z) {}
} __attribute__((packed));

template<>
struct TSTP_Common::_Coordinates<TSTP_Common::CMx25_16>: public Point<short, 3>
{
    typedef short Number;

    _Coordinates(Number x = 0, Number y = 0, Number z = 0): Point<Number, 3>(x, y, z) {}
} __attribute__((packed));

template<>
struct TSTP_Common::_Coordinates<TSTP_Common::CM_32>: public Point<long, 3>
{
    typedef long Number;

    _Coordinates(Number x = 0, Number y = 0, Number z = 0): Point<Number, 3>(x, y, z) {}
} __attribute__((packed));

template<>
class TSTP_Common::Value<TSTP_Common::Unit::I64>
{
public:
    Value(long long int v): _value(v) {}

    operator long long int() { return _value; }

public:
    long long int _value;
};

template<>
class TSTP_Common::Value<TSTP_Common::Unit::F32>
{
public:
    Value(float v): _value(v) {}

    operator float() { return _value; }

private:
    float _value;
};

template<>
class TSTP_Common::Value<TSTP_Common::Unit::D64>
{
public:
    Value(double v): _value(v) {}

    operator double() { return _value; }

private:
    double _value;
};

__END_SYS

#endif
