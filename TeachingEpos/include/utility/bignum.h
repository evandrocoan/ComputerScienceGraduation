#ifndef __bignum_h
#define __bignum_h

#include <utility/random.h>

__BEGIN_UTIL

// This class implements a prime finite field (Fp or GF(p))
// It is basically big numbers between 0 and a prime modulo, with + - * / operators
// Primarily meant to be used primarily by asymmetric cryptography (e.g. Diffie_Hellman)
template<unsigned int SIZE = 16>
class Bignum
{
    friend class Diffie_Hellman<SIZE>;
    friend class _SYS::Poly1305;

    typedef unsigned int Digit; 
    typedef unsigned long long Double_Digit;
    static const typename IF<sizeof(Digit) * 2 == sizeof(Double_Digit), bool, void>::Result _check_digits_size; // sizeof(Double_Digit) must be == 2*sizeof(Digit)

    static const unsigned int sz_digit = sizeof(Digit);
    static const unsigned int word = (SIZE + sz_digit - 1) / sz_digit;
    static const unsigned int sz_word = sz_digit * word;
    static const unsigned int double_word = 2 * word;
    static const unsigned int bits_in_digit = sz_digit * 8;

    static const unsigned char default_mod[sz_word];
    static const unsigned char default_barrett_u[sz_word + sz_digit];

    // All operations will be done modulo _mod
    static const Digit * _mod;
    // Auxiliary constant to help multiplication
    static const Digit * _barrett_u;

public:
    // Sets data using a little-endian byte-by-byte representation
    Bignum(const unsigned char * bytes, unsigned int len)
    {
        set_bytes(bytes, len);
        _mod = (Digit *)default_mod; 
        _barrett_u = (Digit *)default_barrett_u; 
    }
    // Sets data using a little-endian byte-by-byte representation
    Bignum(const char * bytes, unsigned int len)
    {
        set_bytes(bytes, len);
        _mod = (Digit *)default_mod; 
        _barrett_u = (Digit *)default_barrett_u; 
    }

    // Convert from unsigned int
    Bignum(unsigned int n = 0) __attribute__((noinline))
    { 
        *this = n;
        _mod = (Digit *)default_mod; 
        _barrett_u = (Digit *)default_barrett_u; 
    }

    unsigned int to_uint() { return data[0]; }

    // Sets data to  a random number, smaller than _mod
    void random() __attribute__((noinline))
    {
        int i;
        for(i=word-1; i>=0 && (_mod[i] == 0); i--) 
            data[i]=0;
        data[i] = Random::random() % _mod[i];
        for(--i;i>=0;i--)
            data[i] = Random::random();
    }

    // Sets data using a little-endian byte-by-byte representation
    unsigned int set_bytes(const unsigned char * values, unsigned int len)
    {
        unsigned int i;
        unsigned int min = len < sz_word ? len : sz_word;
        for(i=0; i<min; i++) {
            byte_data[i] = values[i];
        }
        for(; i<sz_word; i++) {
            byte_data[i] = 0;
        }
        return min;        
    }
    unsigned int set_bytes(const char * values, unsigned int len)
    { return set_bytes((unsigned char*)values, len); }

    // -Sets the module to be applied in every operation on this object
    // -The user must handle allocation
    // -To be used only when a module different than the default is needed
    void set_mod(Digit * mod, Digit * barrett_u)
    {
        _mod = mod; 
        _barrett_u = barrett_u; 
    }   

    friend OStream &operator<<(OStream &out, const Bignum &b) 
    {
        unsigned int i;
        out << '[';
        for(i=0;i<word;i++) {
            out << (unsigned int)b.data[i];
            if(i < word-1)
                out << ", ";
        }
        out << "]";
        return out; 
    }
    friend Debug &operator<<(Debug &out, const Bignum &b) 
    {
        unsigned int i;
        out << '[';
        for(i=0;i<word;i++) {
            out << (unsigned int)b.data[i];
            if(i < word-1)
                out << ", ";
        }
        out << "]";
        return out; 
    }

    // Returns true if this number is even,
    // false otherwise
    bool is_even(){ return !(data[0] % 2); }

    // XOR operator
    void operator^=(const Bignum &b) //__attribute__( ( noinline ) )
    {
        for(unsigned int i=0; i<word; i++)
            data[i] ^= b.data[i];
    }

    // Convert from unsigned int
    void operator=(unsigned int n) //__attribute__( ( noinline ) )
    {
        static const bool shift = sizeof(unsigned int) > sz_digit;
        if(!shift) {
            data[0] = n;
            for(unsigned int i=1;i<word;i++)
                data[i] = 0;
        } else {
            unsigned int i;
            for(i=0; (n != 0) && (i<word); i++) {
                data[i] = n;
                n >>= IF_INT<shift, bits_in_digit, 0>::Result; // Working around wrong compiler warning "right shift count >= width of type"
            }
            for(;i<word;i++)
                data[i] = 0;
        }
    }

    // Copies data only
    void operator=(const Bignum &b) //__attribute__( ( noinline ) )
    {
        for(unsigned int i=0;i<word;i++)
            data[i] = b.data[i];
    }

    // Comparison operators
    bool operator==(const Bignum &b) const //__attribute__( ( noinline ) )
    { return (cmp(data, b.data, word) == 0); }
    bool operator!=(const Bignum &b) const //__attribute__( ( noinline ) )
    { return (cmp(data, b.data, word) != 0); }
    bool operator>=(const Bignum &b) const //__attribute__( ( noinline ) )
    { return (cmp(data, b.data, word) >= 0); }
    bool operator<=(const Bignum &b) const //__attribute__( ( noinline ) )
    { return (cmp(data, b.data, word) <= 0); }
    bool operator>(const Bignum &b) const //__attribute__( ( noinline ) )
    { return (cmp(data, b.data, word) > 0); }
    bool operator<(const Bignum &b) const //__attribute__( ( noinline ) )
    { return (cmp(data, b.data, word) < 0); }

    // data = (data * b.data) % _mod
    void operator*=(const Bignum &b)__attribute__((noinline))
    {
        if(b == 1) return;

        if(Traits<Bignum>::hysterically_debugged) {
            db<Bignum>(TRC) << "Bignum equation:" << endl;
            db<Bignum>(TRC) << *this << endl;
            db<Bignum>(TRC) << "*" << endl;
            db<Bignum>(TRC) << b << endl;
            db<Bignum>(TRC) << "%" << endl;
            db<Bignum>(TRC) << '[';
            for(unsigned int i=0;i<word-1;i++)
                db<Bignum>(TRC) << _mod[i] << ", ";
            db<Bignum>(TRC) << _mod[word-1] << ']' << endl;
            db<Bignum>(TRC) << "==" << endl;
        }

        Digit _mult_result[double_word];

        // _mult_result = data * b.data
        simple_mult(_mult_result, data, b.data, word);

        // Barrett modular reduction
        barrett_reduction(data, _mult_result, word);

        if(Traits<Bignum>::hysterically_debugged) {
            db<Bignum>(TRC) << *this << endl;
        }
    }

    // data = (data + b.data) % _mod
    void operator+=(const Bignum &b)__attribute__((noinline))
    {
        if(Traits<Bignum>::hysterically_debugged) {
            db<Bignum>(TRC) << "Bignum equation:" << endl;
            db<Bignum>(TRC) << *this << endl;
            db<Bignum>(TRC) << "+" << endl;
            db<Bignum>(TRC) << b << endl;
            db<Bignum>(TRC) << "%" << endl;
            db<Bignum>(TRC) << '[';
            for(unsigned int i=0;i<word-1;i++)
                db<Bignum>(TRC) << _mod[i] << ", ";
            db<Bignum>(TRC) << _mod[word-1] << ']' << endl;
            db<Bignum>(TRC) << "==" << endl;
        }
        if(simple_add(data, data, b.data, word))
            simple_sub(data, data, _mod, word);
        if(cmp(data, _mod, word) >= 0)
            simple_sub(data, data, _mod, word);
        if(Traits<Bignum>::hysterically_debugged) {
            db<Bignum>(TRC) << *this << endl;
        }
    }

    // data = (data - b.data) % _mod
    void operator-=(const Bignum &b)__attribute__((noinline))
    {
        if(Traits<Bignum>::hysterically_debugged) {
            db<Bignum>(TRC) << "Bignum equation:" << endl;
            db<Bignum>(TRC) << *this << endl;
            db<Bignum>(TRC) << "-" << endl;
            db<Bignum>(TRC) << b << endl;
            db<Bignum>(TRC) << "%" << endl;
            db<Bignum>(TRC) << '[';
            for(unsigned int i=0;i<word-1;i++)
                db<Bignum>(TRC) << _mod[i] << ", ";
            db<Bignum>(TRC) << _mod[word-1] << ']' << endl;
            db<Bignum>(TRC) << "==" << endl;
        }
        if(simple_sub(data, data, b.data, word))
            simple_add(data, data, _mod, word);
        if(Traits<Bignum>::hysterically_debugged) {
            db<Bignum>(TRC) << *this << endl;
        }
    }

    // Calculates the modular multiplicative inverse of this number
    void inverse() __attribute__((noinline))
    {
        Bignum A(1), u, v, zero(0);
        for(unsigned int i=0;i<word;i++) {
            u.data[i] = data[i];
            v.data[i] = _mod[i];
        }
        *this = 0;
        while(u != zero) {
            while(u.is_even()) {
                u.divide_by_two();
                if(A.is_even())
                    A.divide_by_two();
                else {
                    bool carry = simple_add(A.data, A.data, _mod, word);
                    A.divide_by_two(carry);             
                }
            }
            while(v.is_even()) {
                v.divide_by_two();
                if(is_even())
                    divide_by_two();
                else {
                    bool carry = simple_add(data, data, _mod, word);
                    divide_by_two(carry);
                }
            }
            if(u >= v) {
                u -= v;
                A -= *this;
            } else {
                v-=u;
                *this -= A;
            }
        }
    }

    // Shift left (actually shift right, because of little endianness)
    // - Does not apply modulo
    // - Returns carry bit
    bool multiply_by_two(bool carry = 0)__attribute__((noinline))
    {
        if(Traits<Bignum>::hysterically_debugged) {
            if(!carry)
            {
                db<Bignum>(TRC) << "Bignum equation:" << endl;      
                db<Bignum>(TRC) << *this << endl;
                db<Bignum>(TRC) << "<<" << endl;
                db<Bignum>(TRC) << "[1]" << endl;
                db<Bignum>(TRC) << "%" << endl;
                db<Bignum>(TRC) << '[';
                for(unsigned int i=0;i<word-1;i++)
                    db<Bignum>(TRC) << _mod[i] << ", ";
                db<Bignum>(TRC) << _mod[word-1] << ']' << endl;
                db<Bignum>(TRC) << "==" << endl;
            }
        }
        bool next_carry;
        for(unsigned int i=0;i<word;i++)
        {
            next_carry = data[i] >> (bits_in_digit-1);
            data[i] <<= 1;
            data[i] += (Digit)carry;
            carry = next_carry;
        }
        if(Traits<Bignum>::hysterically_debugged) {
            if(!carry)
                db<Bignum>(TRC) << *this << endl;
        }
        return carry;
    }

    // Shift right (actually shift left, because of little endianness)
    // - Does not apply modulo
    // - Returns carry bit
    bool divide_by_two(bool carry = 0)__attribute__((noinline))
    {
        if(Traits<Bignum>::hysterically_debugged) {
            if(!carry)
            {
                db<Bignum>(TRC) << "Bignum equation:" << endl;      
                db<Bignum>(TRC) << *this << endl;
                db<Bignum>(TRC) << ">>" << endl;
                db<Bignum>(TRC) << "[1]" << endl;
                db<Bignum>(TRC) << "%" << endl;
                db<Bignum>(TRC) << '[';
                for(unsigned int i=0;i<word-1;i++)
                    db<Bignum>(TRC) << _mod[i] << ", ";
                db<Bignum>(TRC) << _mod[word-1] << ']' << endl;
                db<Bignum>(TRC) << "==" << endl;
            }
        }
        bool next_carry;
        for(int i=word-1;i>=0;i--)
        {
            next_carry = data[i] % 2;
            data[i] >>= 1;
            data[i] += (Digit)carry << (bits_in_digit-1);
            carry = next_carry;
        }
        if(Traits<Bignum>::hysterically_debugged) {
            if(!carry)
                db<Bignum>(TRC) << *this << endl;
        }
        return carry;
    }

private:
    // C-like comparison
    // Returns: 
    // 1  if a >  b
    // -1 if a <  b
    // 0  if a == b
    static int cmp(const Digit * a, const Digit * b, int size)
    {
        for(int i=size-1; i>=0; i--) {
            if(a[i] > b[i]) return 1;
            else if(a[i] < b[i]) return -1;
        }
        return 0;
    }

    // res = a - b 
    // returns: borrow bit
    // -No modulo applied
    // -a, b and res are assumed to have size 'size'
    // -a, b, res are allowed to point to the same place
    static bool simple_sub(Digit * res, const Digit * a, const Digit * b, unsigned int size)__attribute__((noinline))
    {
        Double_Digit borrow = 0;
        Double_Digit aux = ((Double_Digit)1) << bits_in_digit;
        for(unsigned int i=0; i<size; i++) {
            Double_Digit anow = a[i];
            Double_Digit bnow = ((Double_Digit)b[i]) + borrow;
            borrow = (anow < bnow);
            if(borrow)
                res[i] = (aux - bnow) + anow;   
            else
                res[i] = anow - bnow;   
        }
        return borrow;
    }

    // res = a + b 
    // returns: carry bit
    // -No modulo applied
    // -a, b and res are assumed to have size 'size'
    // -a, b, res are allowed to point to the same place
    static bool simple_add(Digit * res, const Digit * a, const Digit * b, unsigned int size)__attribute__((noinline))
    {
        bool carry = 0;
        for(unsigned int i=0; i<size; i++) {
            Double_Digit tmp = (Double_Digit)carry + (Double_Digit)a[i] + (Double_Digit)b[i];
            res[i] = tmp;
            carry = tmp >> bits_in_digit;
        }
        return carry;
    }

    // res = (a * b)
    // - Does not apply module
    // - a and b are assumed to be of size 'size'
    // - res is assumed to be of size '2*size'
    static void simple_mult(Digit * res, const Digit * a, const Digit * b, unsigned int size)
    {
        unsigned int i;
        Double_Digit r0=0, r1=0, r2=0;
        for(i=0;i<size*2-1;i++) {
            for(unsigned int j=0;(j<size) && (j<=i);j++) {
                unsigned int k = i - j;
                if(k<size) {
                    Double_Digit aj = a[j];
                    Double_Digit bk = b[k];
                    Double_Digit prod = aj * bk;
                    r0 += (Digit)prod;
                    r1 += (prod >> bits_in_digit) + (r0 >> bits_in_digit);
                    r0 = (Digit)r0;
                    r2 += (r1 >> bits_in_digit);
                    r1 = (Digit)r1;
                }
            }
            res[i] = r0;
            r0 = r1;
            r1 = r2;
            r2 = 0;
        }
        res[i] = r0;
    }

    // res = a % _mod
    // - Intended to be used after a multiplication
    // - res is assumed to be of size 'size'
    // - a is assumed to be of size '2*size'
    void barrett_reduction(Digit * res, const Digit * a, unsigned int size)
    {
        Digit q[size+1];

        // q = floor( ( floor( a/base^(size-1) ) * barrett_u ) / base^(size+1))
        Double_Digit r0=0, r1=0, r2=0;
        unsigned int i;
        for(i=0; i<(2*(size+1))-1 ;i++) {
            for(unsigned int j=0;(j<size+1) && (j<=i);j++) {
                unsigned int k = i - j;
                if(k<size+1) {
                    // shifting left (little endian) size-1 places
                    // a is assumed to have size size*2
                    Double_Digit aj = a[j+(size-1)];
                    Double_Digit bk = _barrett_u[k];
                    Double_Digit prod = aj * bk;
                    r0 += (Digit)prod;
                    r1 += (prod >> bits_in_digit) + (r0 >> bits_in_digit);
                    r0 = (Digit)r0;
                    r2 += r1 >> bits_in_digit;
                    r1 = (Digit)r1;
                }
            }
            // shifting left (little endian) size+1 places
            if(i>=size+1)
                q[i-(size+1)] = r0;
            r0 = r1;
            r1 = r2;
            r2 = 0;
        }
        q[i-(size+1)] = r0;

        Digit r[size+1];
        // r = (q * _mod) % base^(size+1)
        r0=0, r1=0, r2=0;
        for(i=0;i<size+1;i++) {
            for(unsigned int j=0;j<=i;j++) {
                unsigned int k = i - j;
                Double_Digit aj = q[j];
                Double_Digit bk = (k == size ? 0 : _mod[k]);
                Double_Digit prod = aj * bk;
                r0 += (Digit)prod;
                r1 += (prod >> bits_in_digit) + (r0 >> bits_in_digit);
                r0 = (Digit)r0;
                r2 += r1 >> bits_in_digit;
                r1 = (Digit)r1;
            }
            r[i] = r0;
            r0 = r1;
            r1 = r2;
            r2 = 0;
        }

        // r = ((a % base^(size+1)) - r) % base^(size+1)
        simple_sub(r, a, r, size+1);

        // data = r % _mod
        while((r[size] > 0) || (cmp(r, _mod, size) >= 0)) {
            if(simple_sub(r, r, _mod, size))
                r[size]--;
        }

        for(unsigned int i=0;i<size;i++)
            res[i] = r[i];
    }

public:
    union {
        Digit data[word];
        unsigned char byte_data[sz_word];
    };
};

__END_UTIL

#endif
