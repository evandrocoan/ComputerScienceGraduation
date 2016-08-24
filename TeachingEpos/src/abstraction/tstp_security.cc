// EPOS Trustful SpaceTime Security Protocol Implementation

#include <system/config.h>
#ifndef __no_networking__

#include <tstp.h>
#include <tstp_mac.h>

__BEGIN_SYS

// Class attributes
TSTP_Security * TSTP_Security::_instance;

// Methods
bool TSTP_Security::do_bootstrap() 
{
    _is_sink = TSTP::here() == Coordinates(0,0,0);

    db<TSTP>(TRC) << "TSTP_Security::bootstrap()" << endl;

    if(_is_sink) {
        Buffer * buf = TSTPNIC::alloc(sizeof(TSTP::DH_Request));
        new (buf->frame()->data<char>()) TSTP::DH_Request(Coordinates(10,10,10), _dh.public_key());
        TSTPNIC::send(buf);
    } else {
        _sem.p();
        Buffer * buf = TSTPNIC::alloc(sizeof(TSTP::Auth_Request));
        new (buf->frame()->data<char>()) TSTP::Auth_Request(_auth, otp(_master_secret, _id));
        TSTPNIC::send(buf);
        _sem.p();
    }

    return true;
}

void TSTP_Security::update(TSTPNIC::Observed * obs, Buffer * buf)
{
    db<TSTP>(TRC) << "TSTP_Security::update(obs=" << obs << ",buf=" << buf << ")" << endl;

    if(buf->is_microframe())
        return;

    TSTP::Header * hdr = buf->frame()->data<Header>();

    if(buf->is_rx() && buf->destined_to_me()) {
        db<TSTP>(TRC) << hdr->type() << endl;
        if(hdr->type() == CONTROL) {
            TSTP::Control * cnt = buf->frame()->data<TSTP::Control>();
            db<TSTP>(TRC) << "TSTP_Security::processing: " << *cnt << endl;
            switch(cnt->subtype()) {
                case DH_REQUEST: {
                    if(!_is_sink) {
                        Buffer * resp = TSTPNIC::alloc(sizeof(TSTP::DH_Response));
                        new (resp->frame()->data<char>()) TSTP::DH_Response(_dh.public_key());
                        TSTPNIC::send(resp);

                        TSTP::DH_Request * dhr = buf->frame()->data<TSTP::DH_Request>();
                        _master_secret = _dh.shared_key(dhr->key());
                        _sem.v();
                    }
                }
                break;
                case DH_RESPONSE: {
                    if(_is_sink) {
                        TSTP::DH_Response * dhr = buf->frame()->data<TSTP::DH_Response>();
                        _master_secret = _dh.shared_key(dhr->key());
                    }
                }
                break;
                case AUTH_REQUEST: {
                    if(_is_sink) {
                        const char sensor_id_1[] = {0x56,0x34,0x12,0,0x54,0x8}; // TODO (students' job: an API to add and manage these IDs properly)

                        ID sensor_id((unsigned char *)sensor_id_1, sizeof(sensor_id_1));

                        TSTP::Auth_Request * aur = buf->frame()->data<TSTP::Auth_Request>();
                        OTP o = otp(_master_secret, sensor_id);                        
                        if(o == aur->otp()) {
                            ID encrypted_id;

                            _aes.encrypt(reinterpret_cast<char *>(&sensor_id), reinterpret_cast<char *>(&o), reinterpret_cast<char *>(&encrypted_id));

                            Buffer * resp = TSTPNIC::alloc(sizeof(TSTP::Auth_Granted));
                            new (resp->frame()->data<char>()) TSTP::Auth_Granted(hdr->origin(), encrypted_id);
                            TSTPNIC::send(resp);
                        }
                    }
                }
                break;
                case AUTH_GRANTED: {
                    if(!_is_sink) {
                        TSTP::Auth_Granted * aug = buf->frame()->data<TSTP::Auth_Granted>();
                        Auth encrypted_id = aug->auth();

                        OTP o = otp(_master_secret, _id);

                        char out[16];
                        _aes.decrypt(reinterpret_cast<char *>(&encrypted_id), reinterpret_cast<char *>(&o), out);

                        bool auth_ok = true;
                        for(int i = 0; i < 16; i++) {
                            if(out[i] != _id[i]) {
                                auth_ok = false;
                                break;
                            }
                        }
                        _sem.v();
                    }
                }
                break;
                default:
                break;
            }
        } else if(hdr->type() == RESPONSE) {
            //decrypt(buf, _master_secret);
        }
    } else if(buf->is_tx() && !buf->trusted()) {
        if(hdr->type() == RESPONSE) {
            //encrypt(buf, _master_secret);
        }
    }
}

// Only supports Response messages
void TSTP_Security::crypt(Buffer * buf, const OTP & key, bool encrypt) 
{
    db<TSTP>(TRC) << "TSTP_Security::crypt(buf=" << buf << ",key=" << key << ",enc=" << encrypt << ")" << endl;
    char * payload = buf->frame()->data<TSTP::Response>()->data<char>();
    char * pkt_end = buf->frame()->data<char>() + buf->size();
    unsigned int size = pkt_end - payload;

    for(unsigned int i = 0; i < size; i+=16) {
        char aux[16];
        for(unsigned int j = i; j < 16; j++) {
            if(i + j > size)
                aux[j] = 0;
            else
                aux[j] = payload[i+j];
        }
        _aes.crypt(aux, reinterpret_cast<const char *>(&key), &(payload[i]), encrypt);
    }
    buf->trusted(true);
}

TSTP_Security::OTP TSTP_Security::otp(const Diffie_Hellman::Shared_Key & key, const ID & id) {
    static const unsigned int max = sizeof(ID) > sizeof(Diffie_Hellman::Shared_Key) ? sizeof(ID) : sizeof(Diffie_Hellman::Shared_Key);
    char MIs[max];
    unsigned int i;
    for(i = 0; i < max; i++) {
        MIs[i] = 0;
        if(i < sizeof(ID)) {
            MIs[i] = id[i];
        }
        if(i < sizeof(Diffie_Hellman::Shared_Key)) {
            MIs[i] ^= key.byte_data[i];
        } 
    }

    OTP out;
    char nonce[16]; // This should be the time. Will set to 0 by now to avoid errors
    for(i = 0; i < 16; i++) {
        nonce[i] = 0;
    }

    _poly.k(reinterpret_cast<const char *>(&id));
    _poly.r(reinterpret_cast<const char *>(key.byte_data));
    _poly.stamp(reinterpret_cast<char *>(&out), nonce, MIs, max);
    return out;
}

__END_SYS

#endif
