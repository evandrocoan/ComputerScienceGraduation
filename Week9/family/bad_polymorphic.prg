class Synchronizer
{
protected:
    Synchronizer();

public:
    virtual ~Synchronizer();
    virtual void p();
    virtual void v();
    virtual void wait();
    virtual void signal();
    virtual void broadcast();
};

class Semaphore: public Synchronizer
{
public:
    Semaphore(int value = 1);
    ~Semaphore();
    void p();
    void v();

private:
    // Implementation declarations
};

class Condition: public Synchronizer
{
public:
    Condition();
    ~Condition();
    void wait();
    void signal();
    void broadcast();

private:
    // Implementation declarations
};
