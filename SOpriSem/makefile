all: SimApp

SimApp: main.o SimpleApplication.o
	g++ -g -o SimApp main.o SimpleApplication.o

main.o: main.cpp
	g++ -g -c main.cpp

SimpleApplication.o: SimpleApplication.cpp SimpleApplication.h
	g++ -g -c SimpleApplication.cpp SimpleApplication.h
	
#clean
#	-rm -f *.o *~