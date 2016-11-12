// EPOS Application Initializer

#include <utility/heap.h>
#include <mmu.h>
#include <machine.h>
#include <application.h>

__BEGIN_SYS

class Init_Application
{
private:
    static const unsigned int HEAP_SIZE = Traits<Application>::HEAP_SIZE;

public:
    Init_Application() {
        db<Init>(TRC) << "Init_Application()" << endl;

	// Initialize Application's heap
	db<Init>(INF) << "Initializing application's heap" << endl;
	Application::_heap = new (&Application::_preheap[0]) Heap(MMU::alloc(MMU::pages(HEAP_SIZE)), HEAP_SIZE);

	db<Init>(INF) << "done!" << endl;
    }
};

// Global object "init_application"  must be linked to the application (not
// to the system) and there constructed at first.
Init_Application init_application;

__END_SYS
