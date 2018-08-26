// EPOS System Initializer

#include <utility/random.h>
#include <machine.h>
#include <system.h>

__BEGIN_SYS

class Init_System
{
private:
    static const unsigned int HEAP_SIZE = Traits<System>::HEAP_SIZE;

public:
    Init_System() {
        db<Init>(TRC) << "Init_System()" << endl;

        // Initialize the processor
        db<Init>(INF) << "Initializing the CPU: " << endl;
        CPU::init();
        db<Init>(INF) << "done Initializing the CPU!" << endl;

        // Initialize System's heap
        db<Init>(INF) << "Initializing system's heap: " << endl;
        System::_heap = new (&System::_preheap[0]) Heap(MMU::alloc(MMU::pages(HEAP_SIZE)), HEAP_SIZE);
        db<Init>(INF) << "done Initializing system's heap!" << endl;

        // Initialize the machine
        db<Init>(INF) << "Initializing the machine: " << endl;
        Machine::init();
        db<Init>(INF) << "done Initializing the machine!" << endl;

        // Initialize system abstractions
        db<Init>(INF) << "Initializing system abstractions: " << endl;
        System::init();
        db<Init>(INF) << "done initializing system abstractions!" << endl;

        // Randomize the Random Numbers Generator's seed
        if(Traits<Random>::enabled) {
            db<Init>(INF) << "Randomizing the Random Numbers Generator's seed: " << endl;
            if(Traits<TSC>::enabled)
                Random::seed(TSC::time_stamp());

            if(!Traits<TSC>::enabled)
                db<Init>(WRN) << "Due to lack of entropy, Random is a pseudo random numbers generator!" << endl;
            db<Init>(INF) << "done Randomizing the Random Numbers Generator's seed!" << endl;
        }

        // db<Init>(INF) << "Initialization continues at init_first.cc!" << endl;
    }
};

// Global object "init_system" must be constructed first.
Init_System init_system;

__END_SYS
