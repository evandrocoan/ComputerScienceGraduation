class Network
{
public:
    Network();
    virtual ~Network();
    virtual void send(int dst, const void* buf, unsigned int len) = 0;
    virtual void receive(int* src, const void* buf, unsigned int* len) = 0;
 
protected:
    // Family commonalities
};

class Ethernet: public Network
{
public:
    Ethernet();
    ~Ethernet();
    void send(int dst, const void* buf, unsigned int len);
    void receive(int* src, const void* buf, unsigned int* len);

private:
    // Implementation
};

class Myrinet: public Network
{
public:
    Myrinet();
    ~Myrinet();
    void send(int dst, const void* buf, unsigned int len);
    void receive(int* src, const void* buf, unsigned int* len);

private:
    // Implementation
};

class Communicator
{
public:
    // Interface

protected:
    Network* net;
    // Other commonalities
};

#include <iostream>
using namespace std;

Network::Network() { cout << "Network()\n"; }
Network::~Network() { cout << "~Network()\n"; }

Ethernet::Ethernet() { cout << "Ethernet()\n"; }
Ethernet::~Ethernet() { cout << "~Ethernet()\n"; }
void Ethernet::send(int, const void*, unsigned)
{ cout << "Ethernet::send()\n"; }
void Ethernet::receive(int* , const void*, unsigned*)
{ cout << "Ethernet::receive()\n"; }

Myrinet::Myrinet() { cout << "Myrinet()\n"; }
Myrinet::~Myrinet() { cout << "~Myrinet()\n"; }
void Myrinet::send(int, const void*, unsigned)
{ cout << "Myrinet::send()\n"; }
void Myrinet::receive(int* , const void*, unsigned*)
{ cout << "Myrinet::receive()\n"; }

int main()
{
    Network* net;
    Ethernet eth0;
    Myrinet myri0;

    net = &eth0;
    net->send(1, 0, 0);    // OK

    net= &myri0;
    net->send(1, 0, 0);    // OK
}
