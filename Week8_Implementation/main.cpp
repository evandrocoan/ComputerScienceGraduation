


#include <iostream>
#include "abstraction_app.cpp"


int main( void )
{
    AbstractionInterface* abstraction = new Abstraction();
    abstraction->operation();
    
    return 0;
}
