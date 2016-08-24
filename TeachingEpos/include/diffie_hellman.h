#ifndef diffie_hellman_h
#define diffie_hellman_h

#include <system/config.h>
#include <utility/bignum.h>

__BEGIN_SYS

template <unsigned int SECRET_SIZE = Traits<Diffie_Hellman<0>>::SECRET_SIZE>
class Diffie_Hellman
{
	static const unsigned int PUBLIC_KEY_SIZE = 2 * SECRET_SIZE;

    typedef _UTIL::Bignum<SECRET_SIZE> Bignum;

	static const unsigned char default_base_point_x[SECRET_SIZE];
	static const unsigned char default_base_point_y[SECRET_SIZE];

	class ECC_Point
	{
    public:
		ECC_Point() __attribute__((noinline)) {}

		void operator*=(const Bignum &b)__attribute__((noinline))
        {
            // Finding last '1' bit of k
            unsigned int t = Bignum::bits_in_digit;
            int b_len = Bignum::word+1;
            typename Bignum::Digit now; //= x._data[Bignum::word - 1];   
            do {
                now = b.data[(--b_len)-1];
            }while(now == 0);
            assert(b_len > 0);

            bool bin[t]; // Binary representation of now

            ECC_Point pp(*this);

            for(int j=Bignum::bits_in_digit-1;j>=0;j--) {
                if(now%2) 
                    t=j+1;
                bin[j] = now%2;
                now/=2;
            }

            for(int i=b_len-1;i>=0;i--) {
                for(;t<Bignum::bits_in_digit;t++) {
                    jacobian_double();
                    if(bin[t]) {
                        add_jacobian_affine(pp);
                    }
                }
                if(i>0) {
                    now = b.data[i-1];
                    for(int j=Bignum::bits_in_digit-1;j>=0;j--) {
                        bin[j] = now%2;
                        now/=2;
                    }
                    t=0;
                }
            }

            Bignum Z; 
            z.inverse();
            Z = z; 
            Z *= z;

            x *= Z;
            Z *= z;

            y *= Z;
            z = 1;
        }

		friend Debug &operator<<(Debug &out, const ECC_Point &a) {
			out << "{x=" << a.x << ",y=" << a.y << ",z=" << a.z << "}";
			return out;
		}

        Diffie_Hellman::Bignum x, y, z;

	private:
		void jacobian_double()__attribute__((noinline)) {
            Bignum B, C(x), aux(z);

            aux *= z; C -= aux;
            aux += x; C *= aux;
            C *= 3;

            z *= y; z *= 2;

            y *= y; B = y;

            y *= x; y *= 4;

            B *= B; B *= 8;

            x = C; x *= x;
            aux = y; aux *= 2;
            x -= aux;

            y -= x; y *= C;
            y -= B; 
        }
		void add_jacobian_affine(const ECC_Point &b)__attribute__((noinline))
        {
            Bignum A(z), B, C, X, Y, aux, aux2;  

            A *= z;

            B = A;

            A *= b.x;

            B *= z; B *= b.y;

            C = A; C -= x;

            B -= y;

            X = B; X *= B;
            aux = C; aux *= C;

            Y = aux;

            aux2 = aux; aux *= C;
            aux2 *= 2; aux2 *= x;
            aux += aux2; X -= aux;

            aux = Y; Y *= x;
            Y -= X; Y *= B;
            aux *= y; aux *= C;
            Y -= aux;

            z *= C;

            x = X; y = Y;  
        }
	};

public:
    typedef ECC_Point Public_Key;
    typedef Bignum Shared_Key;

	Diffie_Hellman(const unsigned char base_point_data_x[Bignum::word * Bignum::sz_digit], const unsigned char base_point_data_y[Bignum::word * Bignum::sz_digit]) __attribute__((noinline))
	{
		_base_point.x.set_bytes(base_point_data_x, Bignum::word * Bignum::sz_digit);
		_base_point.y.set_bytes(base_point_data_y, Bignum::word * Bignum::sz_digit);
		_base_point.z = 1;
		generate_keypair();
	}

	Diffie_Hellman() __attribute__((noinline))
	{
		_base_point.x.set_bytes(default_base_point_x, Bignum::word * Bignum::sz_digit);
		_base_point.y.set_bytes(default_base_point_y, Bignum::word * Bignum::sz_digit);
		_base_point.z = 1;
		generate_keypair();
	}

	ECC_Point public_key() { return _public; }

	void generate_keypair()
	{
		db<Diffie_Hellman>(TRC) << "Diffie_Hellman::generate_keypair()" << endl;
		_private.random();
		db<Diffie_Hellman>(INF) << "Diffie_Hellman Private: " << _private << endl;
		_public = _base_point;
		db<Diffie_Hellman>(INF) << "Diffie_Hellman Base Point: " << _base_point << endl;
		_public *= _private;
		db<Diffie_Hellman>(INF) << "Diffie_Hellman Public: " << _public << endl;
	}

	Shared_Key shared_key(ECC_Point public_key)__attribute__((noinline))
    {
        db<Diffie_Hellman>(TRC) << "Diffie_Hellman - Calculating key: public_key: " << public_key << endl;
        db<Diffie_Hellman>(TRC) << "private: " << _private << endl;

        public_key *= _private;
        public_key.x ^= public_key.y;

        db<Diffie_Hellman>(INF) << "Diffie_Hellman - Key set: " << public_key.x << endl;
        return public_key.x;
    }

private:
	Bignum _private;
	ECC_Point _base_point;
	ECC_Point _public;
};

__END_SYS

#endif
