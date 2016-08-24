// EPOS Trustful SpaceTime Protocol Declarations

#ifndef __tstp_h
#define __tstp_h

#include <tstp_common.h>
#include <utility/observer.h>
#include <utility/buffer.h>
#include <utility/hash.h>
#include <utility/id.h>
#include <network.h>
#include <tstp_net.h>
#include <timer.h>
#include <utility/random.h>
#include <diffie_hellman.h>
#include <cipher.h>
#include <semaphore.h>
#include <poly1305.h>

__BEGIN_SYS

typedef Traits<TSTP>::MAC TSTPNIC;

class TSTP_Packet: public TSTP_Common::Header
{
public:
    static const unsigned int MTU = NIC::MTU - sizeof(TSTP_Common::Header);
private:
    typedef unsigned char Data[MTU];
public:
    TSTP_Packet() {}

    TSTP_Common::Header * header() { return this; }

    template<typename T>
    T * data() { return reinterpret_cast<T *>(&_data); }

    friend Debug & operator<<(Debug & db, const TSTP_Packet & p) {
        db << "{h=" << reinterpret_cast<const TSTP_Common::Header &>(p) << ",d=" << p._data << "}";
        return db;
    }

private:
    Data _data;
} __attribute__((packed));

// Time Managers
// Passive Time Synchronization
template <bool do_synchronize = true>
class PTS : public TSTP_Common, private TSTPNIC::Observer
{
    typedef TSTP_Timer Timer;
    static const unsigned int TX_DELAY = TSTPNIC::TX_DELAY; 

    typedef TSTPNIC::Buffer Buffer;
    typedef TSTP_Packet Packet;

    typedef TSTP_Timer::Time_Stamp Time_Stamp;
    static const IF<(static_cast<Time_Stamp>(-1) < static_cast<Time_Stamp>(0)), bool, void>::Result _time_stamp_sign_check; // TSTP_Timer::Time_Stamp must be signed!
    typedef TSTP_Common::Time_Offset Short_Time_Stamp;
    typedef TSTP_Common::Microsecond Time;

    static const typename IF<(do_synchronize and (not EQUAL<TSTPNIC, TSTPOTM>::Result)), void, bool>::Result _time_sync_requirement_check; // Synchronization is only available with TSTP MAC
public:
    PTS() {
        if(!do_synchronize) {
            _synced = true;
        }
        TSTPNIC::attach(this);
    }
    ~PTS() {
        TSTPNIC::detach(this);
    }

    static Time now() { return ts_to_us(Timer::now()); }

    static bool bootstrap() {
        if(do_synchronize) {
            while(!_synced);
        }
        return true; 
    }

    void update(TSTPNIC::Observed * obs, Buffer * buf) {
        db<TSTP>(TRC) << "PTS::update(obs=" << obs << ",buf=" << buf << endl;
        if(buf->is_frame()) {
            auto packet = buf->frame()->data<Packet>();
            if(buf->is_rx()) {
                Time_Stamp tx_delay = us_to_ts(TX_DELAY);
                if(do_synchronize) {
                    switch(packet->type()) {
                    case INTEREST: {
                        Short_Time_Stamp sfd = buf->sfd_time_stamp();
                        Short_Time_Stamp to_set = packet->last_hop_time() + tx_delay;
                        Time_Stamp diff1 = sfd - to_set;
                        Time_Stamp diff2 = to_set - sfd;
                        Time_Stamp adjust = _UTIL::abs(diff1) < _UTIL::abs(diff2) ? diff1 : diff2;
                        //Timer::adjust(adjust);
                        buf->sfd_time_stamp(buf->sfd_time_stamp() + adjust);
                        _synced = true;
                    } break;
                    default: break;
                    }
                }
                buf->origin_time(ts_to_us(buf->sfd_time_stamp() - tx_delay - packet->elapsed()));
            }
        }
        else if(buf->is_rx()) {
            buf->relevant((!_synced) || buf->relevant());
        }
    }

private:
    static Time ts_to_us(const Time_Stamp & ts) { return Timer::ts_to_us(ts); }
    static Time_Stamp us_to_ts(const Time & us) { return Timer::us_to_ts(us); }

    static volatile bool _synced;
};

template<bool S> volatile bool PTS<S>::_synced;


//Locators
class NIC_Locator : public TSTP_Common, private TSTPNIC::Observer {
    typedef TSTPNIC::Buffer Buffer;
public:
    NIC_Locator() {
        TSTPNIC::attach(this);
    }
    ~NIC_Locator() {
        TSTPNIC::detach(this);
    }

    static Coordinates here() { return (TSTPNIC::nic()->address()[sizeof(NIC::Address) - 1] % 2) ? Coordinates(0, 0, 0) : Coordinates(55, 55, 50); }

    static bool bootstrap() {
        return true; 
    }

    void update(TSTPNIC::Observed * obs, Buffer * buf) {
        db<TSTP>(TRC) << "NIC_Locator::update(obs=" << obs << ",buf=" << buf << endl;
    }
};

class HECOPS : public TSTP_Common, private TSTPNIC::Observer {
    typedef TSTPNIC::Buffer Buffer;
    typedef TSTP_Packet Packet;
public:
    HECOPS() {
        TSTPNIC::attach(this);
    }
    ~HECOPS() {
        TSTPNIC::detach(this);
    }

    static Coordinates here() { return Coordinates(0,0,0); }

    static bool bootstrap() {
        return true; 
    }

    void update(TSTPNIC::Observed * obs, Buffer * buf) {
        db<TSTP>(TRC) << "HECOPS::update(obs=" << obs << ",buf=" << buf << endl;
    }
};

class Ultrasound_Locator : public TSTP_Common, private TSTPNIC::Observer {
    typedef TSTPNIC::Buffer Buffer;
    typedef TSTP_Packet Packet;
public:
    Ultrasound_Locator() {
        TSTPNIC::attach(this);
    }
    ~Ultrasound_Locator() {
        TSTPNIC::detach(this);
    }

    static Coordinates here() { return Coordinates(0,0,0); }

    static bool bootstrap() {
        return true; 
    }

    void update(TSTPNIC::Observed * obs, Buffer * buf) {
        db<TSTP>(TRC) << "Ultrasound_Locator::update(obs=" << obs << ",buf=" << buf << endl;
    }
};

// Security
class TSTP_Security : public TSTP_Common, private TSTPNIC::Observer {
    typedef TSTPNIC::Buffer Buffer;
    typedef TSTP_Packet Packet;
public:
    typedef _UTIL::ID<16> ID;

    TSTP_Security() : _sem(0) {
        _instance = this;

        unsigned int i = 0;
        for(Machine::ID mid = Machine::id(); (i < sizeof(Machine::ID)) && (i < sizeof(ID)); i++) {
            _id[i] = mid[i];
        }
        for(; (i < sizeof(ID)); i++) {
            _id[i] = 0;
        }

        auth(_id, &_auth);

        TSTPNIC::attach(this);
    }

    ~TSTP_Security() {
        TSTPNIC::detach(this);
    }

    static bool bootstrap() { return _instance->do_bootstrap(); }

    void update(TSTPNIC::Observed * obs, Buffer * buf); 

    // Only supports Response messages
    void decrypt(Buffer * buf, const OTP & key) { crypt(buf, key, false); }
    // Only supports Response messages
    void encrypt(Buffer * buf, const OTP & key) { crypt(buf, key, true); }

    // Only supports Response messages
    void crypt(Buffer * buf, const OTP & key, bool encrypt); 

private:
    bool do_bootstrap();
    OTP otp(const Diffie_Hellman::Shared_Key & key, const ID & id);

    void auth(const ID & id, Auth * output) {
        const char * in = reinterpret_cast<const char *>(&id);
        _aes.encrypt(in, in, reinterpret_cast<char *>(output));
    }

    static TSTP_Security * _instance;

    ID _id;
    Auth _auth;
    Diffie_Hellman _dh;
    Cipher _aes;
    Poly1305 _poly;
    Diffie_Hellman::Shared_Key _master_secret;
    Semaphore _sem;
    bool _is_sink;
};

// Routers
class Greedy_Geographic_Router : public TSTP_Common, private TSTPNIC::Observer {
    typedef TSTPNIC::Buffer Buffer;
    typedef TSTP_Packet Packet;
public:
    Greedy_Geographic_Router() {
        TSTPNIC::attach(this);
    }

    ~Greedy_Geographic_Router() {
        TSTPNIC::detach(this);
    }

    static bool bootstrap() {
        return true; 
    }

    void update(TSTPNIC::Observed * obs, Buffer * buf) {
        db<TSTP>(TRC) << "Greedy_Geographic_Router::update(obs=" << obs << ",buf=" << buf << endl;
        if(buf->is_tx() and buf->is_frame()) {
            buf->offset((Random::random() % (225000 - 4096)) + 4096);
        }
    }
};

class TSTP: public TSTP_Common, private TSTPNIC::Observer
{
    template<typename> friend class Smart_Data;

    class Disabled : public TSTP_Common {
    public:
        static Coordinates here() { return Coordinates(0,0,0); }
        static bool bootstrap() { return true; }
    };

public:
    typedef TSTPNIC NIC;
    typedef PTS<Traits<TSTP>::Time_Manager == Traits<TSTP>::PTS> Time_Manager;
    typedef SWITCH<Traits<TSTP>::Locator, 
            CASE<Traits<TSTP>::NIC_Locator, NIC_Locator, 
            CASE<Traits<TSTP>::HECOPS, HECOPS, 
            CASE<Traits<TSTP>::Ultrasound_Locator, Ultrasound_Locator, 
            CASE<Traits<TSTP>::DISABLED, Disabled>
            >>>>::Result Locator;
    typedef SWITCH<Traits<TSTP>::Security, 
            CASE<Traits<TSTP>::TSTP_Security, TSTP_Security, 
            CASE<Traits<TSTP>::DISABLED, Disabled>
            >>::Result Security;
    typedef SWITCH<Traits<TSTP>::Router, 
            CASE<Traits<TSTP>::Greedy_Geographic_Router, Greedy_Geographic_Router, 
            CASE<Traits<TSTP>::DISABLED, Disabled>
            >>::Result Router;

    // Buffers received from the NIC
    typedef TSTPNIC::Buffer Buffer;

    // Packet
    typedef TSTP_Packet Packet;
    static const unsigned int MTU = Packet::MTU;

    // TSTP observer/d conditioned to a message's address (ID)
    typedef Data_Observer<Buffer, int> Observer;
    typedef Data_Observed<Buffer, int> Observed;

    // Hash to store TSTP Observers by type
    class Interested;
    typedef Hash<Interested, 10, Unit> Interests;
    class Responsive;
    typedef Hash<Responsive, 10, Unit> Responsives;


    // TSTP Messages
    // Each TSTP message is encapsulated in a single package. TSTP does not need nor supports fragmentation.

    // Interest/Response Modes
    enum Mode {
        // Response
        SINGLE = 0, // Only one response is desired for each interest job (desired, but multiple responses are still possible)
        ALL    = 1, // All possible responses (e.g. from different sensors) are desired
        // Interest
        DELETE = 2  // Revoke an interest
    };

    // Interest Message
    class Interest: public Header
    {
    public:
        Interest(const Region & region, const Unit & unit, const Mode & mode, const Error & precision, const Microsecond & expiry, const Microsecond & period = 0)
        : Header(INTEREST, 0, 0, here(), here(), 0, 0), _region(region), _unit(unit), _mode(mode), _precision(0), _expiry(expiry), _period(period) {}

        const Unit & unit() const { return _unit; }
        const Region & region() const { return _region; }
        Microsecond period() const { return _period; }

        Time_Offset expiry() const { return _expiry; }
        void expiry(const Time_Offset & x) { _expiry = x; }

        Mode mode() const { return static_cast<Mode>(_mode); }
        Error precision() const { return static_cast<Error>(_precision); }

        bool time_triggered() { return _period; }
        bool event_driven() { return !time_triggered(); }

        friend Debug & operator<<(Debug & db, const Interest & m) {
            db << reinterpret_cast<const Header &>(m) << ",u=" << m._unit << ",m=" << ((m._mode == ALL) ? 'A' : 'S') << ",e=" << int(m._precision) << ",x=" << m._expiry << ",re=" << m._region << ",p=" << m._period;
            return db;
        }

    protected:
        Region _region;
        Unit _unit;
        unsigned char _mode : 2;
        unsigned char _precision : 6;
        Time_Offset _expiry;
        Time_Offset _period;
    } __attribute__((packed));

    // Response (Data) Message
    class Response: public Header
    {
    public:
        static const unsigned int HEADERS_SIZE = sizeof(Header) + sizeof(Unit) + sizeof(Error) + sizeof(Time_Offset);
    private:
        typedef unsigned char Data[MTU - HEADERS_SIZE];

    public:
        Response(const Unit & unit, const Error & error = 0, const Time_Offset & expiry = 0)
        : Header(RESPONSE, 0, 0, here(), here(), 0, 0), _unit(unit), _error(error), _expiry(expiry) {}

        const Unit & unit() const { return _unit; }

        Time_Offset expiry() const { return _expiry; }
        void expiry(const Time_Offset & x) { _expiry = x; }

        Error error() const { return _error; }

        template<typename T>
        void value(const T & v) { *reinterpret_cast<Value<Unit::GET<T>::NUM> *>(&_data) = v; }

        template<typename T>
        T value() { return *reinterpret_cast<Value<Unit::GET<T>::NUM> *>(&_data); }

        template<typename T>
        T * data() { return reinterpret_cast<T *>(&_data); }

        friend Debug & operator<<(Debug & db, const Response & m) {
            db << reinterpret_cast<const Header &>(m) << ",u=" << m._unit << ",e=" << int(m._error) << ",x=" << m._expiry << ",d=" << hex << *const_cast<Response &>(m).data<unsigned>() << dec;
            return db;
        }

    protected:
        Unit _unit;
        Error _error;
        Time_Offset _expiry;
        Data _data;
    } __attribute__((packed));

    // Command Message
    class Command: public Header
    {
    private:
        typedef unsigned char Data[MTU - sizeof(Region) - sizeof(Unit)];

    public:
        Command(const Unit & unit, const Region & region)
        : Header(COMMAND, 0, 0, here(), here(), 0, 0), _region(region), _unit(unit) {}

        const Region & region() const { return _region; }
        const Unit & unit() const { return _unit; }

        template<typename T>
        T * command() { return reinterpret_cast<T *>(&_data); }

        template<typename T>
        T * data() { return reinterpret_cast<T *>(&_data); }

        friend Debug & operator<<(Debug & db, const Command & m) {
            db << reinterpret_cast<const Header &>(m) << ",u=" << m._unit << ",reg=" << m._region;
            return db;
        }

    protected:
        Region _region;
        Unit _unit;
        Data _data;
    } __attribute__((packed));

    // Security Bootstrap Control Messages
    class DH_Request: public Control
    {
    public:
        DH_Request(const Coordinates & dst, const Diffie_Hellman::Public_Key & k) 
            : Control(DH_REQUEST, 0, 0, here(), here(), 0, 0), _destination(dst), _public_key(k) { }

        Coordinates destination() { return _destination; }
        void destination(const Coordinates  & d) { _destination = d; }

        Diffie_Hellman::Public_Key key() { return _public_key; } 
        void key(const Diffie_Hellman::Public_Key & k) { _public_key = k; } 

        friend Debug & operator<<(Debug & db, const DH_Request & m) {
            db << reinterpret_cast<const Control &>(m) << ",d=" << m._destination << ",k=" << m._public_key;
            return db;
        }

    private:
        Coordinates _destination;
        Diffie_Hellman::Public_Key _public_key;
    };

    class DH_Response: public Control
    {
    public:
        DH_Response(const Diffie_Hellman::Public_Key & k) 
            : Control(DH_RESPONSE, 0, 0, here(), here(), 0, 0), _public_key(k) { }

        Diffie_Hellman::Public_Key key() { return _public_key; } 
        void key(const Diffie_Hellman::Public_Key & k) { _public_key = k; } 

        friend Debug & operator<<(Debug & db, const DH_Response & m) {
            db << reinterpret_cast<const Control &>(m) << ",k=" << m._public_key;
            return db;
        }

    private:
        Diffie_Hellman::Public_Key _public_key;
    };

    class Auth_Request: public Control
    {
    public:
        Auth_Request(const Auth & a, const OTP & o) 
            : Control(AUTH_REQUEST, 0, 0, here(), here(), 0, 0), _auth(a), _otp(o) { }

        Auth auth() { return _auth; } 
        void auth(const Auth & a) { _auth = a; } 

        OTP otp() { return _otp; } 
        void otp(const OTP & o) { _otp = o; } 

        friend Debug & operator<<(Debug & db, const Auth_Request & m) {
            db << reinterpret_cast<const Control &>(m) << ",a=" << m._auth << ",o=" << m._otp;
            return db;
        }

    private:
        Auth _auth;
        OTP _otp;
    };

    class Auth_Granted: public Control
    {
    public:
        Auth_Granted(const Coordinates & dst, const Auth & a) 
            : Control(AUTH_GRANTED, 0, 0, here(), here(), 0, 0), _destination(dst), _auth(a) { }

        Coordinates destination() { return _destination; }
        void destination(const Coordinates  & d) { _destination = d; }

        Auth auth() { return _auth; } 
        void auth(const Auth & a) { _auth = a; } 

        friend Debug & operator<<(Debug & db, const Auth_Granted & m) {
            db << reinterpret_cast<const Control &>(m) << ",d=" << m._destination << ",a=" << m._auth;
            return db;
        }

    private:
        Coordinates _destination;
        Auth _auth; // TODO
    };

    // Interested (binder between Interest messages and Smart Data)
    class Interested: public Interest
    {
    public:
        template<typename T>
        Interested(T * data, const Region & region, const Unit & unit, const Mode & mode, const Precision & precision, const Microsecond & expiry, const Microsecond & period = 0)
        : Interest(region, unit, mode, precision, expiry, period), _link(this, T::UNIT) {
            db<TSTP>(TRC) << "TSTP::Interested(d=" << data << ",r=" << region << ",p=" << period << ") => " << reinterpret_cast<const Interest &>(*this) << endl;
            _interested.insert(&_link);
            advertise();
        }
        ~Interested() {
            db<TSTP>(TRC) << "TSTP::~Interested(this=" << this << ")" << endl;
            _interested.remove(&_link);
            revoke();
        }

        void advertise() { send(); }
        void revoke() { _mode = DELETE; send(); }

    private:
        void send() {
            db<TSTP>(TRC) << "TSTP::Interested::send() => " << reinterpret_cast<const Interest &>(*this) << endl;
            Buffer * buf = NIC::alloc(sizeof(Interest));
            memcpy(buf->frame()->data<Interest>(), this, sizeof(Interest));
            buf->origin_time(TSTP::now());
            buf->deadline(buf->origin_time() + expiry());
            NIC::send(buf);
        }

    private:
        Interests::Element _link;
    };

    // Responsive (binder between Smart Data (Sensors) and Response messages)
    class Responsive: public Response
    {
    public:
        template<typename T>
        Responsive(T * data, const Unit & unit, const Error & error, const Time & expiry)
        : Response(unit, error, expiry), _size(Response::HEADERS_SIZE + sizeof(typename T::Value)), _link(this, T::UNIT) {
            db<TSTP>(TRC) << "TSTP::Responsive(d=" << data << ",s=" << _size << ") => " << this << endl;
            db<TSTP>(INF) << "TSTP::Responsive() => " << reinterpret_cast<const Response &>(*this) << endl;
            _responsives.insert(&_link);
        }
        ~Responsive() {
            db<TSTP>(TRC) << "TSTP::~Responsive(this=" << this << ")" << endl;
            _responsives.remove(&_link);
        }

        using Header::origin;

        void respond(const Time & expiry) { send(expiry); }

        Time time() { return _time; }
        void time(const Time & t) { _time = t; }

    private:
        void send(const Time & expiry) {
            db<TSTP>(TRC) << "TSTP::Responsive::send(x=" << expiry << ")" << endl;
            Buffer * buf = NIC::alloc(_size);
            memcpy(buf->frame()->data<Response>(), this, _size);
            buf->origin_time(_time);
            buf->frame()->data<Response>()->expiry(expiry);
            buf->deadline(buf->origin_time() + expiry);
            db<TSTP>(INF) << "TSTP::Responsive::send:response=" << this << " => " << reinterpret_cast<const Response &>(*this) << endl;
            NIC::send(buf);
        }

    private:
        unsigned int _size;
        Responsives::Element _link;
        Time _time;
    };

public:
    TSTP() {
        db<TSTP>(TRC) << "TSTP::TSTP()" << endl;
        NIC::attach(this);
    }
    ~TSTP() {
        db<TSTP>(TRC) << "TSTP::~TSTP()" << endl;
        NIC::detach(this);
    }

    static Time now() { return Time_Manager::now(); }
    static Coordinates here() { return Locator::here(); }

    static void attach(Observer * obs, void * subject) { _observed.attach(obs, int(subject)); }
    static void detach(Observer * obs, void * subject) { _observed.detach(obs, int(subject)); }
    static bool notify(void * subject, Buffer * buffer) { return _observed.notify(int(subject), buffer); }

    static void init(unsigned int unit) {
        db<Init, TSTP>(TRC) << "TSTP::init()" << endl;
    }

    static bool bootstrap() {
        if(!Locator::bootstrap()) return false;
        if(!Router::bootstrap()) return false;
        if(!Time_Manager::bootstrap()) return false;
        if(!Security::bootstrap()) return false;
        return true;
    }
private:
    static Coordinates absolute(const Coordinates & coordinates) { return coordinates; }

    void update(NIC::Observed * obs, Buffer * buf) {
        db<TSTP>(TRC) << "TSTP::update(obs=" << obs << ",buf=" << buf << ")" << endl;

        if(buf->is_rx() and buf->is_frame() and buf->destined_to_me()) {
            Packet * packet = buf->frame()->data<Packet>();
            switch(packet->type()) {
            case INTEREST: {
                Interest * interest = reinterpret_cast<Interest *>(packet);
                db<TSTP>(INF) << "TSTP::update:msg=" << interest << " => " << *interest << endl;
                // Check for local capability to respond and notify interested observers
                Responsives::List * list = _responsives[interest->unit()]; // TODO: What if sensor can answer multiple formats (e.g. int and float)
                if(list)
                    for(Responsives::Element * el = list->head(); el; el = el->next()) {
                        Responsive * responsive = el->object();
                        if(interest->region().contains(responsive->origin(), now())) {
                            notify(responsive, buf);
                        }
                    }
            } break;
            case RESPONSE: {
                Response * response = reinterpret_cast<Response *>(packet);
                db<TSTP>(INF) << "TSTP::update:msg=" << response << " => " << *response << endl;
                // Check region inclusion and notify interested observers
                Interests::List * list = _interested[response->unit()];
                if(list)
                    for(Interests::Element * el = list->head(); el; el = el->next()) {
                        Interested * interested = el->object();
                        if(interested->region().contains(response->origin(), buf->origin_time()))
                            notify(interested, buf);
                    }
            } break;
            case COMMAND: {
                Command * command = reinterpret_cast<Command *>(packet);
                db<TSTP>(INF) << "TSTP::update:msg=" << command << " => " << *command << endl;
                // Check for local capability to respond and notify interested observers
                Responsives::List * list = _responsives[command->unit()]; // TODO: What if sensor can answer multiple formats (e.g. int and float)
                if(list)
                    for(Responsives::Element * el = list->head(); el; el = el->next()) {
                        Responsive * responsive = el->object();
                        if(command->region().contains(responsive->origin(), now()))
                            notify(responsive, buf);
                    }
            } break;
            case CONTROL: break;
            }
        }
    }

private:
    static Interests _interested;
    static Responsives _responsives;

    static Observed _observed; // Channel protocols are singletons
};


__END_SYS

#endif
