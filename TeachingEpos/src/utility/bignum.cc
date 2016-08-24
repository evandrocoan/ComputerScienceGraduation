#include <utility/bignum.h>

__BEGIN_UTIL

template<> const unsigned char Bignum<16>::default_mod[Bignum<16>::sz_word] = {
    0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF,
    0xFD, 0xFF, 0xFF, 0xFF
};
template<> const unsigned char Bignum<16>::default_barrett_u[Bignum<16>::sz_word + Bignum<16>::sz_digit] = {
    17, 0, 0, 0, 
    8, 0, 0, 0, 
    4, 0, 0, 0, 
    2, 0, 0, 0, 
    1, 0, 0, 0
};

// 2^(130) - 5: used by Poly1305
template<> const unsigned char Bignum<17>::default_mod[Bignum<17>::sz_word] = {
    0xFB, 0xFF, 0xFF, 0xFF, 
    0xFF, 0xFF, 0xFF, 0xFF, 
    0xFF, 0xFF, 0xFF, 0xFF, 
    0xFF, 0xFF, 0xFF, 0xFF, 
    0x03, 0x00, 0x00, 0x00
};
// 0x400000000000000000000000000000005000000000000000
template<> const unsigned char Bignum<17>::default_barrett_u[Bignum<17>::sz_word + Bignum<17>::sz_digit] = {
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x50,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x40,
};

template<> const Bignum<16u>::Digit * Bignum<16u>::_mod = 0;
template<> const Bignum<16u>::Digit * Bignum<16u>::_barrett_u = 0;
template<> const Bignum<17u>::Digit * Bignum<17u>::_mod = 0;
template<> const Bignum<17u>::Digit * Bignum<17u>::_barrett_u = 0;

__END_UTIL
