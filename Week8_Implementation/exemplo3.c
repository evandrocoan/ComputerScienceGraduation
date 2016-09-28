#include <iostream>
#include <string>


template< int n > struct Factorial
{
    // temos um template recursivo, pegando o campo result do struct factorial
    static const int Result = n * Factorial< n - 1 >::Result;
};

template<> struct Factorial< 0 >
{
    // ainda precisamos do campo Result, por que quando n for igual a 0, ainda precisamos de um
    // campo Result
    static const int Result = 1;
};

template< long num, long den > struct FractionNumber
{
    static const long numerator = num;
    static const long denominator = den;
};

template< typename FN1, typename FN2 > struct FractionNumberAdd
{
    // somente fucntion se o tipeo passado for fraction number
    typedef FractionNumber< FN1::numerator * FN2::denominator +
                            FN2::numerator * FN1::denominator, 
                            FN1::denominator * FN2::denominator > Result;
};


template< typename FN > struct FractionNumberEvaluate
{
    static constexpr double Result = ((double)FN::numerator) /
                                 ((double)FN::denominator);
};

int main(void)
{
    int b = 6;
    std::cout << "Factorial(6): " << Factorial< 6 >::Result << std::endl; 
    
    // é uma possível implementação/chamada daquela struct.
    double r = FractionNumberEvaluate< 
                                       FractionNumberAdd< 
                                                          FractionNumber< 2, 3 >,
                                                          FractionNumber< 1, 3 >
                                                        >::Result
                                       >::Result;
    
    std::cout << "2/3 + 1/3 = " << r << std::endl;
    
    return 0;
}
