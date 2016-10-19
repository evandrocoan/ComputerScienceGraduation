#ifndef __uniform_h
#define __uniform_h

//============================================================================
// ATOMIC							
//============================================================================
#assert CONF_ATOMIC (System)

#if #CONF_ATOMIC (System)
#define __ATOMIC System_Atomic
#define __SYSTEM_ATOMIC_H <aspects/atomic/system.h>

#elif #CONF_ATOMIC (Class)
#define __ATOMIC Class_Atomic
#define __CLASS_ATOMIC_H <aspects/atomic/class.h>

#elif #CONF_ATOMIC (Object)
#define __ATOMIC Object_Atomic
#define __OBJECT_ATOMIC_H <aspects/atomic/object.h>

// #if ! #CONF_SYNCHRONIZER (Mutex)
// #assert CONF_SYNCHRONIZER (Mutex) 
// #endif

#endif

//============================================================================
// SHARED							
//============================================================================
#assert CONF_SHARED (Referenced)

#if #CONF_SHARED (Referenced)
#define __SHARED Referenced
#define __REFERENCED_H <aspects/shared/referenced.h>

#elif #CONF_SHARED (Enrolled)
#define __SHARED Enrolled
#define __ENROLLED_H <aspects/shared/enrolled.h>
#endif

//============================================================================
// PROTECTED							
//============================================================================
#assert CONF_PROTECTED (Checked)

#if #CONF_PROTECTED (Checked)
#define __PROTECTED Checked
#define __CHECKED_H <aspects/protected/checked.h>

#elif #CONF_PROTECTED (Permitted)
#define __PROTECTED Permitted
#define __PERMITTED_H <aspects/protected/permitted.h>
#endif

//============================================================================
// ALLOCATED							
//============================================================================
#assert CONF_ALLOCATED (False)

#if #CONF_ALLOCATED (Static)
#define __ALLOCATED Static_Allocated
#define __STATIC_ALLOCATED_H <aspects/allocated/static.h>

#elif #CONF_ALLOCATED (Dynamic)
#define __ALLOCATED Dynamic_Allocated
#define __DYNAMIC_ALLOCATED_H <aspects/allocated/dynamic.h>
#endif

//============================================================================
// TIMED							
//============================================================================
#assert CONF_TIMED (False)

#if #CONF_TIMED (Limited)
#define __TIMED Limited
#define __LIMITED_H <aspects/timed/limited.h>

#elif #CONF_TIMED (Delayed)
#define __TIMED Delayed
#define __DELAYED_H <aspects/timed/delayed.h>
#endif

//============================================================================
// DEBUGGED							
//============================================================================
#if #CONF_DEBUGGED (Watched)
#define __DEBUGGED Watched
#define __WATCHED_H <aspects/debugged/watched.h>

#elif #CONF_DEBUGGED (Traced)
#define __DEBUGGED Traced
#define __TRACED_H <aspects/debugged/traced.h>

#elif #CONF_DEBUGGED (Profiled)
#define __DEBUGGED Profiled
#define __PROFILED_H <aspects/debugged/profiled.h>
#endif

#endif
