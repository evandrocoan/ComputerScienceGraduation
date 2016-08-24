// EPOS Cortex-M IEEE 802.15.4 NIC Mediator Declarations

#ifndef __emote3_ieee802_15_4_h
#define __emote3_ieee802_15_4_h

#include <ic.h>
#include <machine/cortex_m/emote3_ieee802_15_4_phy.h>
#include <ieee802_15_4.h>

__BEGIN_SYS

// CC2538 IEEE 802.15.4 Radio Mediator
class eMote3_IEEE802_15_4: public IEEE802_15_4, public IEEE802_15_4::Observed, private eMote3_IEEE802_15_4_PHY
{
    typedef IEEE802_15_4::CRC CRC;
    static const unsigned int TX_BUFS = Traits<eMote3_IEEE802_15_4>::SEND_BUFFERS;
    static const unsigned int RX_BUFS = Traits<eMote3_IEEE802_15_4>::RECEIVE_BUFFERS;

    static const unsigned int DMA_BUFFER_SIZE = RX_BUFS * sizeof(Buffer) + TX_BUFS * sizeof(Buffer);

    template <int unit> friend void call_init();

    typedef CPU::IO_Irq IO_Irq;
    typedef CPU::Log_Addr Log_Addr;
    typedef CPU::Phy_Addr Phy_Addr;
    typedef MMU::DMA_Buffer DMA_Buffer;
    static const unsigned int MTU = IEEE802_15_4::MTU;
    typedef IEEE802_15_4::Frame Frame;
    typedef IEEE802_15_4::Header Header;

private:
    // Transmit and Receive Ring sizes
    static const unsigned int UNITS = Traits<eMote3_IEEE802_15_4>::UNITS;

    // Interrupt dispatching binding
    struct Device {
        eMote3_IEEE802_15_4 * device;
        unsigned int interrupt;
    };

protected:
    eMote3_IEEE802_15_4(unsigned int unit, IO_Irq irq, DMA_Buffer * dma_buf);

public:
    typedef IEEE802_15_4::Address MAC_Address;
    typedef MAC_Address Address;

    void listen();
    void stop_listening();
    unsigned int channel() { return _channel; }
    void channel(unsigned int c) { if((c > 10) and (c < 27)) { _channel = c; eMote3_IEEE802_15_4_PHY::channel(_channel); } }

    ~eMote3_IEEE802_15_4();

    int send(const Address & dst, const Protocol & prot, const void * data, unsigned int size);
    int receive(Address * src, Protocol * prot, void * data, unsigned int size);

    Buffer * alloc(NIC * nic, const Address & dst, const Protocol & prot, unsigned int once, unsigned int always, unsigned int payload);
    void free(Buffer * buf);
    int send(Buffer * buf);

    const Address & address() { return _address; }
    void address(const Address & address);

    const Statistics & statistics() { return _statistics; }

    void reset();

    static eMote3_IEEE802_15_4 * get(unsigned int unit = 0) { return get_by_unit(unit); }

private:
    unsigned int _channel;
    void handle_int();

    static void int_handler(const IC::Interrupt_Id & interrupt);

    static eMote3_IEEE802_15_4 * get_by_unit(unsigned int unit) {
        if(unit >= UNITS) {
            db<eMote3_IEEE802_15_4>(WRN) << "Radio::get: requested unit (" << unit << ") does not exist!" << endl;
            return 0;
        } else
            return _devices[unit].device;
    }

    static eMote3_IEEE802_15_4 * get_by_interrupt(unsigned int interrupt) {
        for(unsigned int i = 0; i < UNITS; i++)
            if(_devices[i].interrupt == interrupt) {
                return _devices[i].device;
            }

        return 0;
    };

    static void init(unsigned int unit);

    // Send a message and wait for it to be correctly sent
    bool send_and_wait(bool ack);

    bool wait_for_ack();

    bool backoff_and_send();

private:
    volatile bool _acked;
    unsigned int _unit;

    Address _address;
    Statistics _statistics;

    IO_Irq _irq;
    DMA_Buffer * _dma_buf;

    int _rx_cur;
    int _tx_cur;

    Buffer * _rx_buffer[RX_BUFS];
    Buffer * _tx_buffer[TX_BUFS];

    static Device _devices[UNITS];
};

__END_SYS

#endif
