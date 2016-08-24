// EPOS Cortex-M IEEE 802.15.4 NIC Mediator Declarations

#ifndef __emote3_ieee802_15_4_phy_h
#define __emote3_ieee802_15_4_phy_h

#include <ieee802_15_4_phy.h>

__BEGIN_SYS

// CC2538 IEEE 802.15.4 RF Transceiver
class CC2538RF
{
    friend class One_Hop_MAC;
    friend class TSTP_MAC;
protected:
    typedef CPU::Reg8 Reg8;
    typedef CPU::Reg16 Reg16;
    typedef CPU::Reg32 Reg32;

    // Bases
    enum
    {
        FFSM_BASE = 0x40088500,
        XREG_BASE = 0x40088600,
        SFR_BASE  = 0x40088800,
        ANA_BASE  = 0x40088800,
        RXFIFO    = 0x40088000,
        TXFIFO    = 0x40088200,
    };

    // Useful FFSM register offsets
    enum
    {
        SRCRESINDEX = 0x8c,
        PAN_ID0     = 0xc8,
        PAN_ID1     = 0xcc,
        SHORT_ADDR0 = 0xd0,
        SHORT_ADDR1 = 0xd4,
    };

    // ANA_REGS register
    enum
    {
        IVCTRL    = 0x04,
    };

    // Useful XREG register offsets
    enum
    {
        FRMFILT0    = 0x000,
        FRMFILT1    = 0x004,
        SRCMATCH    = 0x008,
        FRMCTRL0    = 0x024,
        FRMCTRL1    = 0x028,
        RXMASKSET   = 0x030,
        FREQCTRL    = 0x03C,
        FSMSTAT1    = 0x04C,
        FIFOPCTRL   = 0x050,
        RXFIRST     = 0x068,
        RXFIFOCNT   = 0x06C,
        TXFIFOCNT   = 0x070,
        RXFIRST_PTR = 0x074,
        RXLAST_PTR  = 0x078,
        RFIRQM0     = 0x08c,
        RFIRQM1     = 0x090,
        CSPT        = 0x194,
        AGCCTRL1    = 0x0c8,
        TXFILTCFG   = 0x1e8,
        FSCAL1      = 0x0b8,
        CCACTRL0    = 0x058,
        TXPOWER     = 0x040,
        RSSI        = 0x060,
        RSSISTAT    = 0x064,
    };

    // Useful SFR register offsets
    enum
    {
        RFDATA  = 0x28,
        RFERRF  = 0x2c,
        RFIRQF1 = 0x30,
        RFIRQF0 = 0x34,
        RFST    = 0x38,
    };


    // Radio commands
    enum
    {
        STXON       = 0xd9,
        SFLUSHTX    = 0xde,
        ISSTART     = 0xe1,
        ISRXON      = 0xe3,
        ISTXON      = 0xe9,
        ISTXONCCA   = 0xea,
        ISSAMPLECCA = 0xeb,
        ISFLUSHRX   = 0xed,
        ISFLUSHTX   = 0xee,
        ISRFOFF     = 0xef,
        ISCLEAR     = 0xff,
    };

    // Useful bits in RSSISTAT
    enum 
    {
        RSSI_VALID = 1 << 0,
    };    
    // Useful bits in XREG_FRMFILT0
    enum
    {
        MAX_FRAME_VERSION = 1 << 2,
        PAN_COORDINATOR   = 1 << 1,
        FRAME_FILTER_EN   = 1 << 0,
    };
    // Useful bits in XREG_FRMFILT1
    enum
    {
        ACCEPT_FT3_MAC_CMD = 1 << 6,
        ACCEPT_FT2_ACK     = 1 << 5,
        ACCEPT_FT1_DATA    = 1 << 4,
        ACCEPT_FT0_BEACON  = 1 << 3,
    };
    // Useful bits in XREG_SRCMATCH
    enum
    {
        SRC_MATCH_EN   = 1 << 0,
    };

    // Useful bits in XREG_FRMCTRL0
    enum
    {
        APPEND_DATA_MODE = 1 << 7,
        AUTO_CRC         = 1 << 6,
        AUTO_ACK         = 1 << 5,
        ENERGY_SCAN      = 1 << 4,
        RX_MODE          = 1 << 2,
        TX_MODE          = 1 << 0,
    };
    enum RX_MODES
    {
        RX_MODE_NORMAL = 0,
        RX_MODE_OUTPUT_TO_IOC,
        RX_MODE_CYCLIC,
        RX_MODE_NO_SYMBOL_SEARCH,
    };

    // Bit set by hardware in FCS field when AUTO_CRC is set
    enum
    {
        AUTO_CRC_OK = 0x80,
    };

    // Useful bits in XREG_FRMCTRL1
    enum
    {
        PENDING_OR         = 1 << 2,
        IGNORE_TX_UNDERF   = 1 << 1,
        SET_RXENMASK_ON_TX = 1 << 0,
    };

    // Useful bits in XREG_FSMSTAT1
    enum
    {
        FIFO        = 1 << 7,
        FIFOP       = 1 << 6,
        SFD         = 1 << 5,
        CCA         = 1 << 4,
        SAMPLED_CCA = 1 << 3,
        LOCK_STATUS = 1 << 2,
        TX_ACTIVE   = 1 << 1,
        RX_ACTIVE   = 1 << 0,
    };

    // Useful bits in SFR_RFIRQF1
    enum
    {
        TXDONE = 1 << 1,
    };

    // RFIRQF0 Interrupts
    enum
    {
        INT_RXMASKZERO      = 1 << 7,
        INT_RXPKTDONE       = 1 << 6,
        INT_FRAME_ACCEPTED  = 1 << 5,
        INT_SRC_MATCH_FOUND = 1 << 4,
        INT_SRC_MATCH_DONE  = 1 << 3,
        INT_FIFOP           = 1 << 2,
        INT_SFD             = 1 << 1,
        INT_ACT_UNUSED      = 1 << 0,
    };

    // RFIRQF1 Interrupts
    enum
    {
        INT_CSP_WAIT   = 1 << 5,
        INT_CSP_STOP   = 1 << 4,
        INT_CSP_MANINT = 1 << 3,
        INT_RFIDLE     = 1 << 2,
        INT_TXDONE     = 1 << 1,
        INT_TXACKDONE  = 1 << 0,
    };

protected:
    volatile Reg32 & ana (unsigned int offset) { return *(reinterpret_cast<volatile Reg32*>(ANA_BASE + offset)); }
    volatile Reg32 & xreg (unsigned int offset) { return *(reinterpret_cast<volatile Reg32*>(XREG_BASE + offset)); }
    volatile Reg32 & ffsm (unsigned int offset) { return *(reinterpret_cast<volatile Reg32*>(FFSM_BASE + offset)); }
    volatile Reg32 & sfr  (unsigned int offset) { return *(reinterpret_cast<volatile Reg32*>(SFR_BASE  + offset)); }

    volatile bool _rx_done() { return (xreg(FSMSTAT1) & FIFOP); }
};

// Standalone IEEE 802.15.4 PHY mediator
class eMote3_IEEE802_15_4_PHY : protected CC2538RF, public IEEE802_15_4_PHY, private Cortex_M_Model
{
    friend class One_Hop_MAC;
    friend class TSTP_MAC;

protected:
    typedef CPU::IO_Irq IO_Irq;
    typedef CPU::Reg8 Reg8;
    typedef CPU::Reg16 Reg16;
    typedef CPU::Reg32 Reg32;

public:
    typedef IEEE802_15_4_PHY::Frame Frame;

    eMote3_IEEE802_15_4_PHY() {
        // Enable clock to the RF CORE module
        Cortex_M_Model::radio_enable();

        // Disable device interrupts
        xreg(RFIRQM0) = 0;
        xreg(RFIRQM1) = 0;

        // Change recommended in the user guide (CCACTRL0 register description)
        xreg(CCACTRL0) = 0xF8;

        // Changes recommended in the user guide (Section 23.15 Register Settings Update)
        xreg(TXFILTCFG) = 0x09;
        xreg(AGCCTRL1) = 0x15;
        ana(IVCTRL) = 0x0b;
        xreg(FSCAL1) = 0x01;

        sfr(RFST) = ISFLUSHTX; // Clear TXFIFO
        sfr(RFST) = ISFLUSHRX; // Clear RXFIFO

        // Reset result of source matching (value undefined on reset)
        ffsm(SRCRESINDEX) = 0;

        // Set FIFOP threshold to maximum
        xreg(FIFOPCTRL) = 0xff;

        // Set TXPOWER (this is the value Contiki uses by default)
        xreg(TXPOWER) = 0xD5;

        rx_mode(RX_MODE_NORMAL);

        // Clear interrupts
        sfr(RFIRQF0) = 0;
        sfr(RFIRQF1) = 0;

        // Clear error flags
        sfr(RFERRF) = 0;
    }

    void off() { sfr(RFST) = ISRFOFF; clear_rxfifo(); sfr(RFIRQF0) = 0; }
    void rx() { sfr(RFST) = ISRXON; }
    void tx() { sfr(RFST) = ISTXON; }
    bool cca() { return xreg(FSMSTAT1) & CCA; }
    bool cca_valid() { return xreg(RSSISTAT) & RSSI_VALID; }
    void start_cca() { rx_mode(RX_MODE_NO_SYMBOL_SEARCH); rx(); }
    void end_cca() { rx_mode(RX_MODE_NORMAL); }
    bool valid_frame() { return frame_in_rxfifo(); }

    void setup_tx(char * f, unsigned int size) {
        if(Traits<eMote3_IEEE802_15_4>::CRC) {
            sfr(RFDATA) = size + sizeof(CRC);
        } else {
            sfr(RFDATA) = size;
        }
        for(auto i=0u; i < size; i++)
            sfr(RFDATA) = f[i];
    }

    volatile bool tx_ok() {
        volatile bool ret = (sfr(RFIRQF1) & INT_TXDONE);
        if(ret) 
            sfr(RFIRQF1) &= ~INT_TXDONE;
        return ret;
    }

    void channel(unsigned int c) { 
        if((c > 10) and (c < 27)) {
            /*
               The carrier frequency is set by programming the 7-bit frequency word in the FREQ[6:0] bits of the
               FREQCTRL register. Changes take effect after the next recalibration. Carrier frequencies in the range
               from 2394 to 2507 MHz are supported. The carrier frequency f C , in MHz, is given by
               f C = (2394 + FREQCTRL.FREQ[6:0]) MHz, and is programmable in 1-MHz steps.
               IEEE 802.15.4-2006 specifies 16 channels within the 2.4-GHz band. These channels are numbered 11
               through 26 and are 5 MHz apart. The RF frequency of channel k is given by Equation 1.
               f c = 2405 + 5(k –11) [MHz] k [11, 26]
               (1)
               For operation in channel k, the FREQCTRL.FREQ register should therefore be set to
               FREQCTRL.FREQ = 11 + 5 (k – 11).
               */
            frequency(11+5*(c-11));
        }
    }

protected:
    bool tx_if_cca() { sfr(RFST) = ISTXONCCA; return (xreg(FSMSTAT1) & SAMPLED_CCA); }
    void rx_mode(RX_MODES m) {
        xreg(FRMCTRL0) = (xreg(FRMCTRL0) & ~(3 * RX_MODE)) | (m * RX_MODE);
    }

    unsigned int copy_from_rxfifo(unsigned char * data) {
        unsigned int sz = sfr(RFDATA); // First field is length of MAC frame
        for(auto i = 0u; i < sz; ++i) { // Copy MAC frame
            data[i] = sfr(RFDATA);
        }
        clear_rxfifo();
        return sz;
    }

    bool frame_in_rxfifo() {
        bool ret = false;
        if(xreg(RXFIFOCNT) > 0) {
            auto rxfifo = reinterpret_cast<volatile unsigned int*>(RXFIFO);
            unsigned char mac_frame_size = rxfifo[0];
            if (mac_frame_size > 127) {
                clear_rxfifo();
                ret = false;
            } else {
                if(Traits<eMote3_IEEE802_15_4>::CRC) {
                    // On RX, last byte in the frame contains info like CRC result
                    // (obs: mac frame is preceeded by one byte containing the frame length, 
                    // so total rxfifo data's size is 1 + mac_frame_size)
                    ret = rxfifo[mac_frame_size] & AUTO_CRC_OK;

                    if(not ret) {
                        clear_rxfifo();
                    }
                } else {
                    ret = true;
                }
            }
        }

        return ret;
    }

    void clear_rxfifo() { sfr(RFST) = ISFLUSHRX; }
    void frequency(unsigned int freq) { xreg(FREQCTRL) = freq; }
    void clear_txfifo() { 
        sfr(RFST) = ISFLUSHTX;
        while(xreg(TXFIFOCNT) != 0);
    }
};

__END_SYS

#endif
