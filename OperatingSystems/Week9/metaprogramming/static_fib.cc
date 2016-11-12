#include <iostream>

using namespace std;

template<int n>
struct Fib 
{ enum { RET = Fib<n-1>::RET + Fib<n-2>::RET };
};

template<>
struct Fib<1>
{ enum { RET = 1 };
};

template<>
struct Fib<0>
{ enum { RET = 0 };
};

int main()
{ 
cout << "Fib(0)= " << Fib<0>::RET << endl;
cout << "Fib(1)= " << Fib<1>::RET << endl;
cout << "Fib(2)= " << Fib<2>::RET << endl;
cout << "Fib(3)= " << Fib<3>::RET << endl;
cout << "Fib(7)= " << Fib<7>::RET << endl;
    return 0;
}

/* Same effect as:
  cout << "Fib(7)= " << 5040 << endl;
*/

