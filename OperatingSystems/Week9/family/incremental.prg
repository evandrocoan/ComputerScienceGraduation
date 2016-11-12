class Common
{
protected:
    Common() {}

    // Family commonalities
};

class Member_A: protected Common
{
public:
    int operation1(void);

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

typedef Member_B Member_AB;
