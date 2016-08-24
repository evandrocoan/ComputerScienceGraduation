#ifndef __util_id_h
#define __util_id_h

#include <system/config.h>

__BEGIN_UTIL

template<unsigned int SIZE>
class ID {
public:
    ID() {}
    ID(const unsigned char * data, unsigned int data_size = SIZE) {
        unsigned int i = 0;
        for(; (i < SIZE) && (i < data_size); i++) {
            _id[i] = data[i];
        }
        for(; i < SIZE; i++) {
            _id[i] = 0;
        }
    }

    friend OStream & operator<<(OStream & db, const ID & id) {
        db << hex;
        for(unsigned int i = 0; i < SIZE; i++) {
            db << id._id[i];
            if(i < SIZE - 1)
                db << ".";
        }
        db << dec;
        return db;
    }

    unsigned char & operator[](const size_t i) { return _id[i]; }
    const unsigned char & operator[](const size_t i) const { return _id[i]; }

private:
    unsigned char _id[SIZE];
};

__END_UTIL

#endif
