#ifndef __poly1305_h
#define __poly1305_h

#include <utility/string.h>
#include <utility/bignum.h>
#include <utility/math.h>
#include <cipher.h>

#include <system/config.h>

__BEGIN_SYS

class Poly1305
{
    typedef _UTIL::Bignum<17> Bignum;

public:
    Poly1305(const char k[16], const char r[16]) : _k(k,16), _r(r,16) {
        clamp();
    }
    Poly1305() {}

    void stamp(char out[16], const char nonce[16], const char * message, int message_len) {
        // cr = (c_1 * r^q + c_2 * r^(q-1) + ... + c_q * r^1) % (2^130 - 5)
        Bignum c, cr(0);
        while(message_len > 0) {
            int len = min(16,message_len);
            c.set_bytes(message, len);
            c.byte_data[len] = 1;

            cr += c;
            cr *= _r;

            message += 16;
            message_len -= 16;
        }

        char ciphertext[16];
        Cipher cipher;
        cipher.encrypt(nonce, (char *)_k.byte_data, ciphertext);

        // out = (cr + aes(k,n)) % 2^128
        Bignum::simple_add((Bignum::Digit *)out, (Bignum::Digit *)ciphertext, cr.data, 4);
    }

    bool verify(const char mac[16], const char nonce[16], const char * message, unsigned int message_len) {
        char my_mac[16];
        stamp(my_mac, nonce, message, message_len);
        for(int i = 0; i < 16; i++) {
            if(my_mac[i] != mac[i])
                return false;
        }
        return true;
    }

    void k(const char k1[16]) { _k.set_bytes(k1,16); }
    void r(const char r1[16]) { _r.set_bytes(r1,16); clamp(); }

private:
    void clamp() {
        _r.byte_data[3] &= 15;
        _r.byte_data[7] &= 15;
        _r.byte_data[11] &= 15;
        _r.byte_data[15] &= 15;
        _r.byte_data[4] &= 252;
        _r.byte_data[8] &= 252;
        _r.byte_data[12] &= 252;
    }

    Bignum _k;
    Bignum _r;
};

__END_SYS
#endif
