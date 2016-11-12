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
    
    FractionNumber operator+( FractionNumber &other )
    {
        std::cout << "\nEstou invocando o novo operator + para o tipo FractionNumber." << std::endl;
        
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

template< typename T >
static T add( T x, T y )
{
    return x + y;
}

int main(void)
{
    int x1 = 2, y1 = 3, z1 = add( x1, y1 );
    double x2 = 2.5, y2 = 3.5, z2 = add( x2, y2 );
    
    FractionNumber a(2,3);
    FractionNumber b(1,3);
    
    FractionNumber z3 = add( a, b );
    
    std::cout << z1 << "\t" << z2 << "\t" << z3 << std::endl;
    
    std::cout << "a: " << a.evaluate() << " + b: " << b.evaluate() << std::endl;
    
    return 0;
}
