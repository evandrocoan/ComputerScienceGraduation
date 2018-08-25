// EPOS First Thread Initializer

#include <utility/heap.h>
#include <system/kmalloc.h>
#include <thread.h>

extern "C" { void __epos_app_entry(); }

__BEGIN_SYS

int idle_function() 
{
    while(true)
    {
        db<Thread>(TRC) << "idle_function()" << endl;
        db<Thread>(INF) << "THERE ARE NO RUNNABLE THREADS AT THE MOMENT!" << endl;
        db<Thread>(INF) << "HALTING THE CPU ..." << endl;

        CPU::int_enable();
        CPU::halt();
    }

    return 0;
}

class Init_First
{
private:
    typedef CPU::Log_Addr Log_Addr;

public:
    Init_First() {

        db<Init>(TRC) << "Init_First()" << endl;

        if(!Traits<System>::multithread) {
            CPU::int_enable();
            return;
        }

        db<Init>(INF) << "Initializing the first thread: " << endl;

        // If EPOS is not a kernel, then adjust the application entry point to __epos_app_entry,
        // which will directly call main(). In this case, _init will have already been called,
        // before Init_Application, to construct main()'s global objects.
        Thread::_running = new (kmalloc(sizeof(Thread))) Thread(Thread::Configuration(Thread::RUNNING, Thread::NORMAL), reinterpret_cast<int (*)()>(__epos_app_entry));

        db<Init>(INF) << "done!" << endl;

        db<Init>(INF) << "INIT ends here!" << endl;

        db<Init, Thread>(INF) << "Dispatching the first thread: " << Thread::running() << endl;

        Thread* idle = new (kmalloc(sizeof(Thread))) Thread(Thread::Configuration(Thread::RUNNING, Thread::IDLE), &idle_function);

        // Se descomentar essa linha, a thread principal não executa
        // Thread::_ready.insert(&idle->_link);

        This_Thread::not_booting();

        Thread::running()->_context->load();
    }
};

// Global object "init_first" must be constructed last in the context of the
// OS, for it activates the first application thread (usually main()) 
Init_First init_first;

__END_SYS
