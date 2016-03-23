all: test_queue

test_queue: test_queue.o queue.o
	g++ -g -c test_queue.cpp queue.h

test_queue.o: test_queue.cpp
	g++ -g -c test_queue.cpp 

queue.o: queue.h
	g++ -g -c queue.h

#clean:
#	rm *o hello