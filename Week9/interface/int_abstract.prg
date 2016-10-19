namespace Interface
{
    class Abstraction
    {
    public:
	virtual int operation1(void) = 0;
	virtual void operation2(int) = 0;
    };
}

class Abstraction: private Interface::Abstraction
{
public:
    int operation1(void);
    void operation2(int);

private:
    // Implementation declarations
};
