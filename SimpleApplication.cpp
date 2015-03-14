#include <iostream>
#include <queue>
#include "SimpleApplication.h"
#include "Process.h"

void SimpleApplication::run()
{
    // (i) create processes
    Process *p;
    Process *p1 = new Process( 1, 1, new AddressRegion( 100, 150 ),
                               new AddressRegion( 151, 199 ) );
    Process *p1 = new Process( 2, 2, new AddressRegion( 100, 150 ),
                               new AddressRegion( 151, 199 ) );
    Process *p1 = new Process( 3, 3, new AddressRegion( 100, 150 ),
                               new AddressRegion( 151, 199 ) );

    // (ii) create a queue of processes
    std::queue< Process* > readyProcesses;
    // (iii) include processes into the queue
    readyProcesses.push( p1 );
// completar
    // (iv) manipulate queue
    p = readyProcesses.front();
    std::cout << "O primeiro processo da fila tem Id=" << p->getPID()
              << ". Movendo para o final. " << std::endl;
    readyProcesses.push( p );
    readyProcesses.pop();
// completar
    // (v) do something stupid
    int a = 2, b = 0;
    std::cout << a / b << std::endl;
}
