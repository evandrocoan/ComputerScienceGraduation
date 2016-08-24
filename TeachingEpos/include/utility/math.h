// EPOS Math Utility Declarations

#ifndef __math_h
#define __math_h

#include <system/config.h>

__BEGIN_UTIL

static const float E = 2.71828183;

inline float logf(float num, float base = E, float epsilon = 1e-12)
{
    float integer = 0;
    if (num == 0) return 1;

    if (num < 1  && base < 1) return 0;

    while (num < 1) {
        integer--;
        num *= base;
    }

    while (num >= base) {
        integer++;
        num /= base;
    }

    float partial = 0.5;
    num *= num;
    float decimal = 0.0;
    while (partial > epsilon) {
        if (num >= base) {
            decimal += partial;
            num /= base;
        }
        partial *= 0.5;
        num *= num;
    }

    return (integer + decimal);
}

inline float sqrt(float x)
{
    float xhi = x;
    float xlo = 0;
    float guess = x/2;

    while (guess * guess != x)
    {
        if (guess * guess > x)
            xhi = guess;
        else
            xlo = guess;

        float new_guess = (xhi + xlo) / 2;
        if (new_guess == guess)
            break; // not getting closer
        guess = new_guess;
    }

    return guess;
}

inline float fast_log2(float val)
{
    int * const exp_ptr = reinterpret_cast <int *> (&val);
    int x = *exp_ptr;
    const int log_2 = ((x >> 23) & 255) - 128;
    x &= ~(255 << 23);
    x += 127 << 23;
    (*exp_ptr) = x;

    val = ((-1.0f/3) * val + 2) * val - 2.0f/3;

    return (val + log_2);
}

inline float fast_log(float val)
{
    static const float ln_2 = 0.69314718f;
    return (fast_log2(val) * ln_2);
}

template <typename T>
const T & min(const T & x, const T & y)
{
    return (x <= y) ? x : y;
}

template <typename T>
const T & max(const T & x, const T & y)
{
    return (x > y) ? x : y;
}

template <typename T>
T abs(const T & x)
{
    if(x > 0) return x;
    return -x;
}

__END_UTIL

#endif
