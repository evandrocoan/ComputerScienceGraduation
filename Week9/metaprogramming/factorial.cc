#include <iostream>

template<int n>
struct Factorial
{
  enum {RET = Factorial<n - 1>::RET * n};
};

template<>
struct Factorial<0>
{
  enum {RET = 1};
};

int main()
{
  cout << Factorial<7>::RET << '\n';
}
