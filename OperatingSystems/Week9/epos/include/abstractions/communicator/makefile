CXX = g++
INCLUDE = /home/guto/uml/cpp/guest/include
CXXFLAGS = -O -fno-exceptions -fno-rtti -I$(INCLUDE)

all: port_test mailbox_test stream_test

port_test: port_test.cpp *.h
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/port_test.key\" $< -o $@ -lpthread

mailbox_test: mailbox_test.cpp *.h
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/mailbox_test.key\" $< -o $@ -lpthread

stream_test: stream_test.cpp *.h
	$(CXX) $(CXXFLAGS) -DCONFIG_KEYS=\"$(PWD)/stream_test.key\" $< -o $@ -lpthread

analyze: *.cpp *.h
	$(CXX) $(CXXFLAGS) -DANALYZE -c *.cpp                               

clean:
	rm -rf *.o port_test mailbox_test stream_test
