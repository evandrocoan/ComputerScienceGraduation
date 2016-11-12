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

class Member_B: protected Common
{
public:
    void operation2(int i);

private:
    // Implementation declarations
};
