CXX = g++
INCLUDE = /home/guto/uml/cpp/guest/include
CXXFLAGS = -O -fno-exceptions -fno-rtti -I$(INCLUDE)

all: exclusive_test cooperative_test concurrent_test

exclusive_test: exclusive_test.cpp *.h 
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/exclusive_test.key\" $< -o $@ -lpthread

cooperative_test: cooperative_test.cpp *.h 
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/cooperative_test.key\" $< -o $@ -lpthread

concurrent_test: concurrent_test.cpp *.h 
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/concurrent_test.key\" $< -o $@ -lpthread

analyze: *.cpp *.h
	$(CXX) $(CXXFLAGS) -DANALYZE -c *.cpp

clean:
	rm -rf *.o exclusive_test cooperative_test concurrent_test
