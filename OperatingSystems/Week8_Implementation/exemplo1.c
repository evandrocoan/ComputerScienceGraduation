#include <iostream>
#include <string>

class FractionNumber
{
public:
    FractionNumber( long numerator, long denominator )
    {
        this->numerator = numerator;
        this->denominator = denominator;
    }
    
    
    FractionNumber add( FractionNumber &other )
    {
        long mmc = this->denominator * other.denominator;

        FractionNumber resp( this->numerator * mmc / this-> denominator
                    + other.numerator * mmc / other.denominator, mmc );

        return resp;
    }

    FractionNumber operator+( FractionNumber &other )
    {
        long mmc = this->denominator * other.denominator;

        FractionNumber resp( this->numerator * mmc / this-> denominator
                    + other.numerator * mmc / other.denominator, mmc );

        return resp;
    }

    friend std::ostream &operator<<( std::ostream& out, FractionNumber &other )
    {
        return out << other.numerator << "/" << other.denominator;
    }
    
    double evaluate()
    {
        return ((double)numerator)/((double)denominator);
    }
    
    
    
private:
    long numerator;
    long denominator;
    
};

int main(void)
{
    FractionNumber a(2,3);
    FractionNumber b(1,3);
    
    FractionNumber c = a.add(b);
    
    // quero fazer a sobrecarga do operador +, se mandar compilar,
    // vai dar erro por que o operador + nao esta definido para FractionNumber
    FractionNumber c2 = a + b;
    
    std::cout << "a: " << a.evaluate() << " + b: " << b.evaluate() << " c: " << c.evaluate() << std::endl;
    std::cout << "\nc: " << c << " c2: " << c2  << std::endl;
    
    return 0;
}
