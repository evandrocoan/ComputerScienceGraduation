namespace Interface 
{
    class Abstraction
    {
    protected:
	Abstraction() {}

    private:
	int operation1(void);
	void operation2(int);
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
