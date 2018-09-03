// EPOS Alarm Abstraction Implementation

#include <semaphore.h>
#include <alarm.h>
#include <display.h>
#include <thread.h>

__BEGIN_SYS

// Class attributes
Alarm_Timer * Alarm::_timer;

volatile Alarm::Tick Alarm::_elapsed;
Alarm::Queue Alarm::_request;


// Methods
Alarm::Alarm(const Microsecond & time, Handler * handler, int times, int semaphore)
: _ticks(ticks(time)), _handler(handler), _times(times), _link(this, _ticks), 
    _priority(Thread::running()->priority()),
    _semaphore(),
    _is_semaphore(semaphore)
{
    lock();

    db<Alarm>(TRC) << "Alarm(t=" << time << ",tk=" << _ticks << ",h=" << reinterpret_cast<void *>(handler)
                   << ",x=" << times << ",_is_semaphore="<< semaphore << ") => " << this << endl;

    if(_is_semaphore)
    {
        this->_semaphore = new (kmalloc(sizeof(Semaphore))) Semaphore(0);
    }

    if(_ticks) {
        _request.insert(&_link);
        unlock();
    } else {
        unlock();
        (*handler)();
    }
}


Alarm::~Alarm()
{
    lock();

    db<Alarm>(TRC) << "~Alarm(this=" << this << ")" << endl;

    _request.remove(this);

    unlock();
}


void Alarm::death()
{
    db<Alarm>(TRC) << "Alarm::death()" << endl;
}


// Class methods
void Alarm::delay(const Microsecond & time)
{
    db<Alarm>(TRC) << "Alarm::delay(time=" << time << "), _elapsed=" << _elapsed << endl;

    Tick t = _elapsed + ticks(time);

    Alarm alarm_a(time, &Alarm::death, 1, 1);

    db<Alarm>(TRC) << "DEATH: _elapsed=" << _elapsed << ", t=" << t << ", alarm_a=" << &alarm_a << endl;

    alarm_a._semaphore->lock();

    db<Alarm>(TRC) << "AFTER DEATH: _elapsed=" << _elapsed << ", t=" << t << ", alarm_a=" << &alarm_a << endl;
}


int Alarm::_free_semaphore(Semaphore * semaphore)
{
    db<Alarm>(TRC) << "Alarm::_free_semaphore(semaphore=" << semaphore << "), _elapsed=" << _elapsed << endl;

    semaphore->unlock();

    db<Alarm>(TRC) << "done Alarm::_free_semaphore()!"<< endl;
    return 0;
}


int Alarm::_next_handler(Handler * next_handler_callback)
{
    db<Alarm>(TRC) << "Alarm::_next_handler(next_handler_callback=" << reinterpret_cast<void *>(next_handler_callback) 
            << "), _elapsed=" << _elapsed << endl;

    (*next_handler_callback)();

    db<Alarm>(TRC) << "done Alarm::_next_handler()!"<< endl;
    return 0;
}


void Alarm::handler(const IC::Interrupt_Id & i)
{
    static Tick next_tick;

    static Thread::Priority next_handler_priority;

    static Handler * next_handler_callback;

    static int next_handler_allowed;

    static Semaphore * next_handler_semaphore;

    static int next_handler_is_semaphore;

    lock();

    _elapsed++;

    // if(Traits<Alarm>::visible) {
    //     Display display;
    //     int lin, col;
    //     display.position(&lin, &col);
    //     display.position(0, 79);
    //     display.putc(_elapsed);
    //     display.position(lin, col);
    // }

    // db<Alarm>(TRC) << "Alarm::handler elapsed: " << _elapsed << ", next_tick: " << next_tick 
    //         << ", next_handler_callback: " << reinterpret_cast<void *>(next_handler_callback)
    //         << ", next_handler_priority: " << next_handler_priority
    //         << ", next_handler_allowed: " << next_handler_allowed
    //         << ", _request.size(): " << _request.size() << endl;

    if(next_tick)
        next_tick--;

    if(!next_tick)
    {
        if(next_handler_allowed)
        {
            if(next_handler_is_semaphore)
            {
                db<Alarm>(TRC) << "Alarm::handler(semaphore=" << next_handler_semaphore << ")" << endl;

                Thread * next_handler = new (kmalloc(sizeof(Thread))) 
                        Thread(Thread::Configuration(Thread::WAITING2, next_handler_priority), &Alarm::_free_semaphore, next_handler_semaphore);

                db<Alarm>(TRC) << "AFTER ALARM::_NEXT_HANDLER_SEMAPHORE! next_handler thread: " << next_handler << endl;
            }
            else
            {
                db<Alarm>(TRC) << "Alarm::handler(handler=" << reinterpret_cast<void *>(next_handler_callback) << ")" << endl;

                Thread * next_handler = new (kmalloc(sizeof(Thread))) 
                        Thread(Thread::Configuration(Thread::WAITING2, next_handler_priority), &Alarm::_next_handler, next_handler_callback);

                db<Alarm>(TRC) << "AFTER ALARM::_NEXT_HANDLER! next_handler thread: " << next_handler << endl;
            }
        }

        if(_request.empty())
        {
            next_handler_allowed = 0;
        }
        else
        {
            Queue::Element * e = _request.remove();
            Alarm * alarm = e->object();
            next_tick = alarm->_ticks;

            next_handler_allowed = 1;
            next_handler_callback = alarm->_handler;
            next_handler_priority = alarm->_priority;
            next_handler_semaphore = alarm->_semaphore;
            next_handler_is_semaphore = alarm->_is_semaphore;

            db<Alarm>(TRC) << "Alarm::handler, alarm->_times: " << alarm->_times 
                    << ", alarm: " << alarm
                    << ", next_handler_priority: " << next_handler_priority
                    << ", next_handler_callback: " << reinterpret_cast<void *>(next_handler_callback)
                    << ", next_tick: " << next_tick << endl;

            if(alarm->_times != -1)
                alarm->_times--;

            if(alarm->_times) {
                e->rank(alarm->_ticks);
                _request.insert(e);
            }
        }
    }

    unlock();
}

__END_SYS
