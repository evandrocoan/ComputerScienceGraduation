#include <iostream>
#include <string>

template< bool condition, typename Then, typename Else >struct IF
{
    typedef Then Result;
};


template< typename Then, typename Else > struct IF< false, Then, Else >
{
    typedef Else Result;
};


class System
{
    
};

template< typename T> struct Traits
{
    static const bool enabled = true;
};


template<> struct Traits< System >
{
    static constexpr bool doublePrecision = true;
};


class Stub: public IF< ..., Proxy, Adapter > 
{
    
};

int main(void)
{
    // define to tipo da variavel em tempo de compilação dependendo do arquivo de configuração
    // Traits. Muito melhor do que ficar fazendo #ifdef... etc.
    IF< Traits< System >::doublePrecision, double, float>::Result minhaVariavel;
    
    std::cout << "sizeof(minhaVariavel): "<< sizeof(minhaVariavel) << std::endl;
    
    return 0;
}
