// EPOS eMote3_IEEE802_15_4 IEEE 802.15.4 NIC Mediator Initialization

#include <system/config.h>
#ifndef __no_networking__

#include <system.h>
#include <utility/random.h>
#include <machine/cortex_m/machine.h>
#include "../../../include/machine/cortex_m/emote3_ieee802_15_4.h"

__BEGIN_SYS

eMote3_IEEE802_15_4::eMote3_IEEE802_15_4(unsigned int unit, IO_Irq irq, DMA_Buffer * dma_buf):
    _unit(unit), _irq(irq), _dma_buf(dma_buf), _rx_cur(0), _tx_cur(0)
{
    db<eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4(unit=" << unit << ",irq=" << irq << ")" << endl;

    // Ignore TX underflow to enable writing to TXFIFO through memory
    // TODO: Memory in the fifos is padded: you can only write one byte every 4bytes.
    // For now, we'll just copy using RFDATA register.
	//xreg(FRMCTRL1) |= IGNORE_TX_UNDERF;

    auto log = _dma_buf->log_address();

    for (auto i = 0u; i < TX_BUFS; ++i) {
        _tx_buffer[i] = new (log) Buffer(0);
        log += sizeof(Buffer);
    }
    for (auto i = 0u; i < RX_BUFS; ++i) {
        _rx_buffer[i] = new (log) Buffer(0);
        log += sizeof(Buffer);
    }

    // Set Address
    ffsm(SHORT_ADDR0) = _address[0];
    ffsm(SHORT_ADDR1) = _address[1];
    _address[0] = ffsm(SHORT_ADDR0);
    _address[1] = ffsm(SHORT_ADDR1);

    // Set PAN ID
    //ffsm(PAN_ID0) = DEFAULT_PAN_ID;
    //ffsm(PAN_ID1) = DEFAULT_PAN_ID >> 8;

    // Make this device a pan coordinator
    //xreg(FRMFILT0) |= PAN_COORDINATOR;

    // Enable frame filtering
    xreg(FRMFILT0) |= FRAME_FILTER_EN;
    xreg(FRMFILT1) &= ~ACCEPT_FT2_ACK; // ACK frames are handled only when expected

    // Enable automatic source address matching
    xreg(SRCMATCH) |= SRC_MATCH_EN;

	// Enable auto-CRC
	xreg(FRMCTRL0) |= AUTO_CRC;

    channel(Traits<eMote3_IEEE802_15_4>::DEFAULT_CHANNEL);

    // Enable auto ACK
    if(Traits<eMote3_IEEE802_15_4>::ACK)
        xreg(FRMCTRL0) |= AUTO_ACK;

    // Reset statistics
    reset();

    if(Traits<eMote3_IEEE802_15_4>::auto_listen) {
 	    xreg(FRMCTRL1) |= SET_RXENMASK_ON_TX; // Enter receive mode after TX

        // Enable useful device interrupts
        // WARNING: do not enable INT_TXDONE, because _send_and_wait handles it
        xreg(RFIRQM0) = INT_FIFOP;
        xreg(RFIRQM1) = 0;

        // Enable clock to the RF CORE module
        // Cortex_M_Model::radio_enable(); // already done

        // Issue the listen command
        rx();
    } else {
	    xreg(FRMCTRL1) &= ~SET_RXENMASK_ON_TX; // Do not enter receive mode after TX
    }
}

void eMote3_IEEE802_15_4::init(unsigned int unit)
{
    db<Init, eMote3_IEEE802_15_4>(TRC) << "eMote3_IEEE802_15_4::init(unit=" << unit << ")" << endl;

    // Allocate a DMA Buffer for init block, rx and tx rings
    DMA_Buffer * dma_buf = new (SYSTEM) DMA_Buffer(DMA_BUFFER_SIZE);

    IO_Irq irq = 26;

    // Initialize the device
    eMote3_IEEE802_15_4 * dev = new (SYSTEM) eMote3_IEEE802_15_4(unit, irq, dma_buf);

    // Register the device
    _devices[unit].interrupt = IC::irq2int(irq);
    _devices[unit].device = dev;

    // Install interrupt handler
    IC::int_vector(_devices[unit].interrupt, &int_handler);
    // Enable interrupts for device
    IC::enable(irq);
}

__END_SYS

#endif
