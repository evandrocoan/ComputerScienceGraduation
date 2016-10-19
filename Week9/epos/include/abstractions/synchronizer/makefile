CXX = g++
INCLUDE = /home/guto/thesis/cpp/guest/include
CXXFLAGS = -O -fno-exceptions -fno-rtti -I$(INCLUDE)

all: mutex_test condition_test semaphore_test

mutex_test: mutex_test.cpp *.h
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/mutex_test.key\" $< -o $@ -lpthread

condition_test: condition_test.cpp *.h
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/condition_test.key\" $< -o $@ -lpthread

semaphore_test: semaphore_test.cpp *.h
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/semaphore_test.key\" $< -o $@ -lpthread

analyze: *.cpp *.h
	$(CXX) $(CXXFLAGS) -DANALYZE -c *.cpp                               

clean:
	rm -rf *.o mutex_test condition_test semaphore_test
