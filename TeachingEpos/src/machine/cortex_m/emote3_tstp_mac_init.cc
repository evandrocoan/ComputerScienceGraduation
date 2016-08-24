// EPOS eMote3 TSTP MAC NIC Mediator Initialization

#include <system/config.h>
#ifndef __no_networking__

#include <system.h>
#include <utility/random.h>
#include <machine/cortex_m/machine.h>
#include "../../../include/machine/cortex_m/emote3_tstp_mac.h"

__BEGIN_SYS

eMote3_TSTP_MAC::eMote3_TSTP_MAC(unsigned int unit, IO_Irq irq, DMA_Buffer * dma_buf):
    _unit(unit), _irq(irq), _dma_buf(dma_buf), _rx_cur(0), _tx_cur(0), _tx_pending(0)
{
    _mf_period = TSTP_Timer::us_to_ts(TIME_BETWEEN_MICROFRAMES + MICROFRAME_TIME) + MF_TX_DELAY;

    db<eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC(unit=" << unit << ",irq=" << irq << ")" << endl;

    auto log = _dma_buf->log_address();

    for (auto i = 0u; i < TX_BUFS; ++i) {
        _tx_buffer[i] = new (log) Buffer(0);
        log += sizeof(Buffer);
    }
    for (auto i = 0u; i < RX_BUFS; ++i) {
        _rx_buffer[i] = new (log) Buffer(0);
        log += sizeof(Buffer);
    }

    // Disable automatic source address matching
    xreg(SRCMATCH) &= ~SRC_MATCH_EN;

    // Disable frame filtering
    xreg(FRMFILT0) &= ~FRAME_FILTER_EN;

    // Disable auto ACK
    xreg(FRMCTRL0) &= ~AUTO_ACK;

    // Disable auto-CRC (for Multimedia MAC)
    xreg(FRMCTRL0) &= ~AUTO_CRC;

    // Do not enter receive mode after TX 
    xreg(FRMCTRL1) &= ~SET_RXENMASK_ON_TX;

    // Disable counting of MAC overflows
    xreg(CSPT) = 0xff;

    // Enable FIFOP (frame received) interrupt
    xreg(RFIRQM0) = INT_FIFOP;
    xreg(RFIRQM1) = 0;

    TSTP_Timer::start();
    // Start state machine
    next_state(&trigger_check_tx_schedule, TSTP_Timer::now() + TSTP_Timer::us_to_ts(SLEEP_PERIOD));
}

void eMote3_TSTP_MAC::init(unsigned int unit)
{
    new (SYSTEM) TSTP_Timer;

    db<Init, eMote3_TSTP_MAC>(TRC) << "eMote3_TSTP_MAC::init(unit=" << unit << ")" << endl;

    // Allocate a DMA Buffer for init block, rx and tx rings
    DMA_Buffer * dma_buf = new (SYSTEM) DMA_Buffer(DMA_BUFFER_SIZE);

    IO_Irq irq = 26;

    // Initialize the device
    eMote3_TSTP_MAC * dev = new (SYSTEM) eMote3_TSTP_MAC(unit, irq, dma_buf);

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
