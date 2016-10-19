#ifndef __system_config_h
#define __system_config_h

#ifndef __DEF
#define __DEF
#endif

//============================================================================
// MACROS
//============================================================================
#define __SYS_NS		System
#define __BEGIN_SYS		namespace __SYS_NS {
#define __END_SYS		}
#define __USING_SYS		using namespace __SYS_NS;

#define __INT_NS		Int
#define __BEGIN_INT		namespace __INT_NS {
#define __END_INT		}
#define __USING_INT		using namespace __INT_NS;
#define __INT(X)		Int::X
#define __REG_INT(M)		__BEGIN_INT class M; __END_INT
#define __EXP_ABS_INT(M)	typedef __INT(M) M;
#define __EXP_ASP_INT(M)	typedef __INT(M) M;

#define __IMP_NS		Imp
#define __BEGIN_IMP		namespace __IMP_NS {
#define __END_IMP		}
#define __USING_IMP		using namespace __IMP_NS;
#define __IMP(X)		Imp::X
#define __REG_IMP(M)		__BEGIN_IMP class M; __END_IMP
#define __EXP_ABS_IMP(M)	typedef Handle<__IMP(M)> M;
#define __EXP_ASP_IMP(M)	typedef __IMP(M) M;

#ifdef	ANALYZE
#define __EXPORT_ABS(F,M)	__REG_INT(M##_##F); __EXP_ABS_INT(M##_##F)
#define __EXPORTN_ABS(F,M)	__REG_INT(M); __EXP_ABS_INT(M)
#else
#define __EXPORT_ABS(F,M)	__REG_IMP(M##_##F); __EXP_ABS_IMP(M##_##F)
#define __EXPORTN_ABS(F,M)	__REG_IMP(M); __EXP_ABS_IMP(M)
#endif

#ifdef	ANALYZE
#define __EXPORT_ASP(F,M)	__REG_INT(M##_##F); __EXP_ASP_INT(M##_##F)
#define __EXPORTN_ASP(F,M)	__REG_INT(M); __EXP_ASP_INT(M)
#else
#define __EXPORT_ASP(F,M)	__REG_IMP(M##_##F); __EXP_ASP_IMP(M##_##F)
#define __EXPORTN_ASP(F,M)	__REG_IMP(M); __EXP_ASP_IMP(M)
#endif

#define __BIND(F,M) 		typedef M##_##F F;
#define __BINDN(F,M)		typedef M F;

#define __UNBIND(F) 		__REG_INT(F); __EXP_ASP_INT(F)

//============================================================================
// SYSTEM NAMESPACE ORGANIZATION
//============================================================================
__BEGIN_SYS
template <class Imp> class Handle;
template <class Imp> class Adapter;
template <class Imp> class Scenario;
template <class Imp> class Config;
__BEGIN_INT __END_INT
__BEGIN_IMP __END_IMP
__END_SYS

//============================================================================
// APPLICATION SPECIFIC CONFIGURATION
//============================================================================
#ifndef ANALYZE
#include CONFIG_KEYS
#assert CONF_FRAMEWORK (Uniform)
#endif

//============================================================================
// FRAMEWORK CONFIGURATION
//============================================================================
#ifndef ANALYZE

#if ! #CONF_FRAMEWORK
#error Configuration: "Framework" not selected!

#elif #CONF_FRAMEWORK (Uniform)
#include <frameworks/uniform.h>
#define __FRAMEWORK_H <frameworks/uniform/handle.h>
#endif

#endif

__BEGIN_SYS

//============================================================================
// ID							
//============================================================================
__EXPORT_ASP(Id, Pointer)
__EXPORT_ASP(Id, Local)
__EXPORT_ASP(Id, Global)
__EXPORT_ASP(Id, Protected)

#ifdef ANALYZE
__UNBIND(Id)
#else

#if #CONF_ID (Not_Needed)
#assert CONF_ID (Pointer)
#endif

#if #CONF_ID (Pointer)
#define	__POINTER_ID_H <aspects/id/pointer.h>
__BIND(Id, Pointer)

#elif #CONF_ID (Local)
#define	__LOCAL_ID_H <aspects/id/local.h>
__BIND(Id, Local)

#elif #CONF_ID (Global)
#define	__GLOBAL_ID_H <aspects/id/global.h>
__BIND(Id, Global)

#elif #CONF_ID (Protected)
#define	__PROTECTED_ID_H <aspects/id/protected.h>
__BIND(Id, Protected)

#else
#error Configuration: "Id" not selected!
#endif	

#endif	

//============================================================================
// THREAD								
//============================================================================
__EXPORT_ABS(Thread, Exclusive)
__EXPORT_ABS(Thread, Cooperative)
__EXPORT_ABS(Thread, Concurrent)

#ifdef ANALYZE
__UNBIND(Thread)
#else

#if #CONF_THREAD (Not_Needed)

#elif #CONF_THREAD (Exclusive)
__BIND (Thread, Exclusive)
#define	__EXCLUSIVE_THREAD_H <abstractions/thread/exclusive.h>

#elif #CONF_THREAD (Cooperative)
__BIND (Thread, Cooperative)
#define	__COOPERATIVE_THREAD_H <abstractions/thread/cooperative.h>

#elif #CONF_THREAD (Concurrent)
__BIND (Thread, Concurrent)
#define	__CONCURRENT_THREAD_H <abstractions/thread/concurrent.h>

#else
#warning Configuration: "Thread" not selected!
#endif

#endif

//============================================================================
// SYNCHRONIZER								
//============================================================================
__EXPORTN_ABS(Synchronizer, Mutex)
__EXPORTN_ABS(Synchronizer, Condition)
__EXPORTN_ABS(Synchronizer, Semaphore)

#ifdef ANALYZE
__UNBIND(Synchronizer)
#else

#if ! #CONF_SYNCHRONIZER
#warning Configuration: "Synchronizer" not selected!
#endif

#if #CONF_SYNCHRONIZER (Mutex)
#if ! #BOUND (Syncrhonizer)
__BINDN (Synchronizer, Mutex)
#assert	BOUND (Syncrhonizer)
#endif
#define	__MUTEX_H <abstractions/synchronizer/mutex.h>
#endif

#if #CONF_SYNCHRONIZER (Condition)
#if ! #BOUND (Syncrhonizer)
__BINDN (Synchronizer, Condition)
#assert	BOUND (Syncrhonizer)
#endif
#define	__CONDITION_H <abstractions/synchronizer/condition.h>
#endif

#if #CONF_SYNCHRONIZER (Semaphore)
#if ! #BOUND (Syncrhonizer)
__BINDN (Synchronizer, Semaphore)
#assert	BOUND (Syncrhonizer)
#endif
#define	__SEMAPHORE_H <abstractions/synchronizer/semaphore.h>
#endif

#endif

//============================================================================
// COMMUNICATOR								
//============================================================================
__EXPORTN_ABS(Communicator, Stream)
__EXPORTN_ABS(Communicator, Port)
__EXPORTN_ABS(Communicator, Mailbox)

#ifdef ANALYZE
__UNBIND(Communicator)
#else

#if ! #CONF_COMMUNICATOR
#warning Configuration: "Communicator" not selected!
#endif

#if #CONF_COMMUNICATOR (Stream)
#if ! #BOUND (Syncrhonizer)
__BINDN (Communicator, Stream)
#assert	BOUND (Syncrhonizer)
#endif
#define	__STREAM_H <abstractions/communicator/stream.h>
#endif

#if #CONF_COMMUNICATOR (Port)
#if ! #BOUND (Syncrhonizer)
__BINDN (Communicator, Port)
#assert	BOUND (Syncrhonizer)
#endif
#define	__PORT_H <abstractions/communicator/port.h>
#endif

#if #CONF_COMMUNICATOR (Mailbox)
#if ! #BOUND (Syncrhonizer)
__BINDN (Communicator, Mailbox)
#assert	BOUND (Syncrhonizer)
#endif
#define	__MAILBOX_H <abstractions/communicator/mailbox.h>
#endif

#endif

//============================================================================
// THREAD_SCHEDULER							
//============================================================================
#ifdef	ANALYZE

typedef class Thread_Scheduler_Int      Thread_Scheduler;
typedef class FCFS_Thread_Scheduler_Int FCFS_Thread_Scheduler;
typedef class RR_Thread_Scheduler_Int   RR_Thread_Scheduler;
typedef class Priority_Thread_Scheduler_Int Priority_Thread_Scheduler;

#else

#define	THREAD_SCHEDULER_FCFS_H		<system/thread_scheduler/fcfs.h>
typedef class FCFS_Thread_Scheduler_Imp FCFS_Thread_Scheduler;

#define	THREAD_SCHEDULER_RR_H		<system/thread_scheduler/rr.h>
typedef class RR_Thread_Scheduler_Imp   RR_Thread_Scheduler;

#define	THREAD_SCHEDULER_PRIORITY_H	<system/thread_scheduler/priority.h>
typedef class Priority_Thread_Scheduler_Imp Priority_Thread_Scheduler;

#if	CONF_THREAD_SCHEDULER		== CONF_NOT_NEEDED

#elif	CONF_THREAD_SCHEDULER		== CONF_THREAD_SCHEDULER_FCFS
#define	THREAD_SCHEDULER_H		THREAD_SCHEDULER_FCFS_H
typedef class FCFS_Thread_Scheduler_Imp Thread_Scheduler;

#elif	CONF_THREAD_SCHEDULER		== CONF_THREAD_SCHEDULER_RR
#define	THREAD_SCHEDULER_H		THREAD_SCHEDULER_RR_H
typedef class RR_Thread_Scheduler_Imp   Thread_Scheduler;

#elif	CONF_THREAD_SCHEDULER		== CONF_THREAD_SCHEDULER_RR
#define	THREAD_SCHEDULER_H		THREAD_SCHEDULER_PRIORITY_H
typedef class Priority_Thread_Scheduler Thread_Scheduler;

#else
#error	Configuration error: "Thread_Scheduler" not selected!
#endif

#endif

//============================================================================
// CPU									
//============================================================================
#ifdef	ANALYZE

typedef class CPU_Int                   CPU;
typedef class Ix86_Int                  Ix86;

#else

#define	CPU_IX86_H			<system/cpu/ix86.h>
typedef class Ix86_Imp                  Ix86; 

#if	CONF_CPU	       		== CONF_NOT_NEEDED

#elif	CONF_CPU			== CONF_CPU_IX86
#define	CPU_H				<ix86.h>
typedef class Ix86_Imp                  CPU;

#else
#error	Configuration error: "CPU" not selected!
#endif

#endif

//============================================================================
// IO_BUS						
//============================================================================
#ifdef	ANALYZE

typedef class IO_Bus_Int                 IO_Bus;
typedef class PCI_Bus_Int                PCI_Bus;

#else

#define	IO_BUS_PCI_H			<system/io_bus/pci.h>
typedef class PCI_Bus_Imp               PCI_Bus;

#if	CONF_IO_BUS	       		== CONF_NOT_NEEDED

#elif	CONF_IO_BUS	       		== CONF_IO_BUS_PCI
#define	IO_BUS_H			IO_BUS_PCI_H
typedef class PCI_Bus_Imp               IO_Bus;

#else
#error	Configuration error: "I/O Bus" not selected!
#endif

#endif

//============================================================================
// NODE									
//============================================================================
#ifdef	ANALYZE

typedef class Node_Int                  Node;
typedef class SP_Node_Int               SP_Node;
typedef class MP_Node_Int               MP_Node;

#else

#define	NODE_SP_H			<system/node/sp.h>
typedef class SP_Node_Imp               SP_Node;

#define	NODE_MP_H			<system/node/mp.h>
typedef class MP_Node_Imp               MP_Node;

#if	CONF_NODE		       	== CONF_NOT_NEEDED

#elif	CONF_NODE	       		== CONF_NODE_SP
#define	NODE_H				<sp_node.h>
typedef class SP_Node_Imp               Node;

#elif	CONF_NODE	       		== CONF_NODE_MP
#define	NODE_H				<mp_node.h>
typedef class MP_Node_Imp               Node;

#else
#error	Configuration error: "Node" not selected!
#endif

#endif

//============================================================================
// TIMER								
//============================================================================
#ifdef	ANALYZE

typedef class Timer_Int                 Timer;
typedef class CPU_Timer_Int             CPU_Timer;

#else

template<class Timer_Imp>               class Timer_Handle;

#define	TIMER_CPU_H			<system/timer/cpu.h>
typedef Timer_Handle<class CPU_Timer_Imp> CPU_Timer;

#if	CONF_TIMER		       	== CONF_NOT_NEEDED

#elif	CONF_TIMER		    	== CONF_TIMER_CPU
#define	TIMER_H				TIMER_CPU_H
typedef Timer_Handle<class CPU_Timer_Imp> Timer;

#else
#error	Configuration error: "Timer" not selected!
#endif

#endif

//============================================================================
// CHRONOMETER								
//============================================================================
#ifdef	ANALYZE

typedef class Chronometer_Int           Chronometer;
typedef class Simple_Chronometer_Int    Simple_Chronometer;

#else

template<class Chronometer_Imp>         class Chronometer_Handle;

#define	CHRONOMETER_SIMPLE_H		<system/chronometer/simple.h>
typedef Chronometer_Handle<class Simple_Chronometer_Imp> Simple_Chronometer;

#if	CONF_CHRONOMETER		== CONF_NOT_NEEDED

#elif	CONF_CHRONOMETER	       	== CONF_CHRONOMETER_SIMPLE
#define	CHRONOMETER_H			CHRONOMETER_SIMPLE_H
typedef Chronometer_Handle<class Simple_Chronometer_Imp> Chronometer;

#else
#error	Configuration error: "Chronometer" not selected!
#endif

#endif

//============================================================================
// NETWORK							
//============================================================================ 
#ifdef	ANALYZE

typedef class Network_Int               Network;
typedef class Myrinet_Int               Myrinet;
typedef class Fast_Ethernet_Int         Fast_Ethernet;

#else

template<class Network_Imp>             class Network_Handle;

#define NETWORK_MYRINET_H		<system/network/myrinet.h>
typedef Network_Handle<class Myrinet_Imp> Myrinet;

#define NETWORK_FAST_ETHERNET_H		<system/network/fast_ethernet.h>
typedef Network_Handle<class Fast_Ethernet_Imp> Fast_Ethernet;

#if	CONF_NETWORK			== CONF_NOT_NEEDED

#elif	CONF_NETWORK			== CONF_NETWORK_MYRINET
#define	NETWORK_H			<myrinet.h>
typedef Network_Handle<class Myrinet_Imp> Network;

#elif	CONF_NETWORK			== CONF_NETWORK_FAST_ETHERNET
#define	NETWORK_H			<fast_ethernet.h>
typedef Network_Handle<class Fast_Ethernet_Imp> Network;

#else
#error	Configuration error: "Network Adapter" not selected!
#endif

#endif

__END_SYS

#ifndef ANALYZE
#include __FRAMEWORK_H
#endif

#endif


