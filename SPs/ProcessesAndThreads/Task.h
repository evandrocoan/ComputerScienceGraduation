/*
 * Task.h
 *
 */

#ifndef TASK_H_
#define TASK_H_

#include <Queue.h>
#include <ucontext.h>

namespace BOOOS {

class Task : public Queue::Element {
public:
	enum State {
		READY,
		WAITING,
		RUNNING,
		FINISHING
	};

	Task(void (*entry_point)(void), int nargs, void * arg);
	virtual ~Task();

	int tid() { return _tid; }
	State state() { return _state; }

	void pass_to(Task * t, State s = READY);

	void exit(int code);

	static Task * self() { return (Task*) __running; }
	static void init();

private:

	Task();

	static volatile Task * __running;

	State _state;
	int _tid; // task ID
	// ...
};

} /* namespace BOOOS */

#endif /* TASK_H_ */
