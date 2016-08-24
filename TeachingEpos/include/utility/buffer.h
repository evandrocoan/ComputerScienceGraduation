// EPOS Buffer Declarations
// This Buffer was designed to move data across a zero-copy communication stack, but can be used for several other purposes

#ifndef __buffer_h
#define __buffer_h

#include <cpu.h>

__BEGIN_UTIL

template<typename Owner, typename Data, typename Shadow = void>
class Buffer: private Data
{
public:
    typedef Simple_List<Buffer<Owner, Data, Shadow> > List;
    typedef typename List::Element Element;

public:
    // This constructor is meant to be used at initialization time to correlate shadow data structures (e.g. NIC ring buffers)
    Buffer(Shadow * s): _lock(false), _owner(0), _shadow(s), _size(sizeof(Data)), _link1(this), _link2(this), 
        _rssi(0), _sfd_time_stamp(0), _id(0), _offset(0), _destined_to_me(true), _deadline(0),
        _origin_time(0), _my_distance(0), _is_tx(false), _is_microframe(false), _relevant(false), _trusted(false) {}

    // These constructors are used whenever a Buffer receives new data
    Buffer(Owner * o, unsigned int s): _lock(false), _owner(o), _size(s), _link1(this), _link2(this),
        _rssi(0), _sfd_time_stamp(0), _id(0), _offset(0), _destined_to_me(true), _deadline(0),
        _origin_time(0), _my_distance(0), _is_tx(false), _is_microframe(false), _relevant(false), _trusted(false) {}
    template<typename ... Tn>
    Buffer(Owner * o, unsigned int s, Tn ... an): Data(an ...), _lock(false), _owner(o), _size(s), _link1(this), _link2(this), 
        _rssi(0), _sfd_time_stamp(0), _id(0), _offset(0), _destined_to_me(true), _deadline(0),
        _origin_time(0), _my_distance(0), _is_tx(false), _is_microframe(false), _relevant(false), _trusted(false) {}

    Data * data() { return this; }
    Data * frame() { return data(); }
    Data * message() { return data(); }

    bool lock() { return !CPU::tsl(_lock); }
    void unlock() { _lock = 0; }

    Owner * owner() const { return _owner; }
    Owner * nic() const { return owner(); }
    void owner(Owner * o) { _owner = o; }
    void nic(Owner * o) { owner(o); }

    Shadow * shadow() const { return _shadow; }
    Shadow * back() const { return shadow(); }

    unsigned int size() const { return _size; }
    void size(unsigned int s) { _size = s; }

    Element * link1() { return &_link1; }
    Element * link() { return link1(); }
    Element * lint() { return link1(); }
    Element * link2() { return &_link2; }
    Element * lext() { return link2(); }

    friend Debug & operator<<(Debug & db, const Buffer & b) {
        db << "{md=" << b._owner << ",lk=" << b._lock << ",sz=" << b._size << ",sd=" << b._shadow << "}";
        return db;
    }

    // Set by any TSTP component
    bool relevant() const { return _relevant; }
    void relevant(bool r) { _relevant = r; }

    // Set by TSTP MAC
    int rssi() const { return _rssi; }
    void rssi(int r) { _rssi = r; }

    // Set by TSTP MAC
    long long sfd_time_stamp() const { return _sfd_time_stamp; }
    void sfd_time_stamp(long long t) { _sfd_time_stamp = t; }

    // Set by TSTP MAC
    unsigned int id() const { return _id; }
    void id(unsigned int i) { _id = i; }

    // Set by TSTP Router
    long long offset() const { return _offset; }
    void offset(long long o) { _offset = o; }

    // Set by TSTP Router
    bool destined_to_me() const { return _destined_to_me; }
    void destined_to_me(bool d) { _destined_to_me = d; }

    // Set by TSTP Router
    long my_distance() const { return _my_distance; }
    void my_distance(long d) { _my_distance = d; }

    // Set by TSTP Time Manager
    long long deadline() const { return _deadline; }
    void deadline(long long t) { _deadline = t; }

    // Set by TSTP Time Manager on reception 
    // Set by TSTP on creation (transmission)
    long long origin_time() const { return _origin_time; }
    void origin_time(long long t) { _origin_time = t; }

    // Set by TSTP MAC or TSTPOE
    bool is_microframe() { return _is_microframe; }
    void is_microframe(bool m) { _is_microframe = m; }
    bool is_frame() { return !is_microframe(); }
    void is_frame(bool m) { is_microframe(!m); }

    // Set by TSTP MAC or TSTPOE
    bool is_tx() { return _is_tx; }
    void is_tx(bool t) { _is_tx = t; }
    bool is_rx() { return !is_tx(); }
    void is_rx(bool t) { is_tx(!t); }

    // Set by TSTP Security
    bool trusted() { return _trusted; }
    void trusted(bool t) { _trusted = t; }

private:
    volatile bool _lock;
    Owner * _owner;
    Shadow * _shadow;
    unsigned int _size;
    Element _link1;
    Element _link2;

    int _rssi;
    long long _sfd_time_stamp;
    unsigned int _id;
    long long _offset;
    bool _destined_to_me;
    long long _deadline;
    long long _origin_time;
    long _my_distance;
    bool _is_tx;
    bool _is_microframe;
    bool _relevant;
    bool _trusted;
};

__END_UTIL

#endif
