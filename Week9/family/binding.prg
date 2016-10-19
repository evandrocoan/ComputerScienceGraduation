namespace Interface
{
    class Family    // Inflated interface
    {
    public:
	Family();
	Family(int i);
	
	int operation1(void);
	void operation2(int i);
    };
}

class Member_A: protected Common    // Partial realization
{
public:
    int operation1(void);
    // ...
};

class Member_B: public Member_A    // Full realization
{
public:
    Member_B(int i = 0);
    void operation2(int i);
    // ...
};

Family instance;
instance.operation1();
instance.operation2(1);
