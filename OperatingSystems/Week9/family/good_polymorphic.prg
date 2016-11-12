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
