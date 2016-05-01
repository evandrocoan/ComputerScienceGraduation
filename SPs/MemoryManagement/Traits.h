/*
 * File:   Traits.h
 * Author: cancian
 *
 * Created on September 27, 2015, 4:16 PM
 */


/*
 * File:   Traits.h
 * Author: cancian
 *
 * Created on September 27, 2015, 4:16 PM
 */

#ifndef TRAITS_H
#define	TRAITS_H

#include "Debug.h"
#include "CPU.h"
#include "Thread.h"
#include "Model.h"
#include "Scheduler.h"
#include "Process.h"

template<typename T>
struct Traits {
    static const bool enabled = true;
    static const bool debugged = true;
};

template<> struct Traits<Process> {
    static constexpr double timeBetweenCreations = 50.0; // time units
    static constexpr unsigned int minAddressSpace = 10e3; // bytes
    static constexpr unsigned int maxAddressSpace = 200e3; // bytes
};

template<> struct Traits<Debug> { // CHANGE THE DEBUG LEVEL HERE SETTING THE LEVELS YOU WANT TO SHOW
    // debug levels
    static const bool error = true;
    static const bool warning = true;
    static const bool trace = true; //false;
    static const bool info = false; //true;
    static const bool fine = false; //true;
    //
    static const bool showEntityAttributes = false;
    static const bool showListOfEvents = false;
    static const bool pauseOnEveryEvent = false; //true;
};

template<> struct Traits<CPU> {
    static constexpr double context_switch_overhead = 1.0; // time units
    static constexpr double timer_interrupt_period = 100.0; // time units
};

template<> struct Traits<Thread> {
    static constexpr double minCpuBurst = 200.0;   // time units
    static constexpr double maxCpuBurst = 500.0;   // time units
    static constexpr int maxBursts = 5;            // CPUBurst
    static constexpr int minThreadsPerProcess = 1; // threads
    static constexpr int maxThreadsPerProcess = 1; // threads
};

template<> struct Traits<Model> {
    static constexpr double simulationLength = 5000.0; // time units
    static constexpr double firstCreation = 0.0;       // time units
};

template<> struct Traits<MemoryManager> {
    static constexpr unsigned int physicalMemorySize = 1e6; // bytes
};

template<> struct Traits<Scheduler> {
    static constexpr double timeSlice = 300.0; // time units
};


#endif	/* TRAITS_H */
