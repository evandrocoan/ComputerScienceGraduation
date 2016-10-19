class Common
{
protected:
    Common() {}

    // Family commonalities
};

class Member_A: protected Common
{
public:
    virtual ~Member_A();
    virtual int operation1(void);
    virtual void operation2(int i);

private:
    // Implementation declarations
};

class Member_B: public Member_A
{
public:
    void operation2(int i);
 
private:
    // Implementation declarations
};
