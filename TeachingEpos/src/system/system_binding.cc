// EPOS System Binding

#include <machine.h>
#include <display.h>
#include <thread.h>

__USING_SYS;
extern "C" {
    void _panic() { Machine::panic(); }
    void _exit(int s) { Thread::exit(s); }
    void __exit() { Thread::exit(CPU::fr()); }  // must be handled by the Page Fault handler for user-level tasks
    void _print(const char * s) { Display::puts(s); }

    // LIBC Heritage
    void __cxa_pure_virtual() {
        db<void>(ERR) << "Pure Virtual method called!" << endl;
    }
}
