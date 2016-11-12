class Common
{
protected:
    Common() {}

    // Family commonalities
};

class Member_A: virtual protected Common
{
public:
    Member_A();
    int operation1(void);

private:
    // Implementation declarations
};

class Member_B: virtual protected Common
{
public:
    Member_B();
    void operation2(int i);

private:
    // Implementation declarations
};

class Member_AB: public Member_A, public Member_B {};
