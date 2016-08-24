// EPOS PC FPGA Mediator Initialization

#include <machine/pc/machine.h>
#include <machine/pc/fpga.h>

__BEGIN_SYS

void PC_FPGA::init()
{
    db<Init, PC_FPGA>(TRC) << "PC_FPGA::init()" << endl;

    // Scan the PCI bus for device
    PC_PCI::Locator loc = PC_PCI::scan(PCI_VENDOR_ID, PCI_DEVICE_ID, 0);
    if(!loc) {
        db<Init, PC_FPGA>(WRN) << "PC_FPGA::init: PCI scan failed!" << endl;
        return;
    }

    // Try to enable IO regions and bus master
    PC_PCI::command(loc, PC_PCI::command(loc) | PC_PCI::COMMAND_MEMORY | PC_PCI::COMMAND_MASTER);

    // Get the config space header and check if we got Regio[0] memory mapped and MASTER
    PC_PCI::Header hdr;
    PCI::header(loc, &hdr);
    if(!hdr) {
        db<Init, PC_FPGA>(WRN) << "PC_FPGA::init: PCI header failed!" << endl;
        return;
    }
    db<Init, PC_FPGA>(INF) << "PC_FPGA::init: PCI header=" << hdr << endl;
    if(!(hdr.command & PC_PCI::COMMAND_MEMORY)) {
        db<Init, PC_FPGA>(WRN) << "PC_FPGA::init: memory unaccessible!" << endl;
        return;
    }
    if(!(hdr.command & PC_PCI::COMMAND_MASTER)) {
        db<Init, PC_FPGA>(WRN) << "PC_FPGA::init: not master capable!" << endl;
        return;
    }
    if(!hdr.region[PCI_REG_CTRL] || !hdr.region[PCI_REG_CTRL].memory) {
        db<Init, PC_FPGA>(WRN) << "PC_FPGA::init: control block unaccessible!" << endl;
        return;
    }

    // Get I/O base port
    Phy_Addr phy_addr = hdr.region[PCI_REG_CTRL].phy_addr;
    Log_Addr log_addr = hdr.region[PCI_REG_CTRL].log_addr;
    unsigned int size = hdr.region[PCI_REG_CTRL].size;
    db<Init, PC_FPGA>(INF) << "PC_FPGA::init: control block of " << size << " bytes at " << phy_addr << "(phy) mapped to " << log_addr << "(log)" << endl;

    // Get I/O irq
    IO_Irq irq = hdr.interrupt_line;
    unsigned int interrupt = IC::irq2int(irq);
    db<Init, PC_FPGA>(INF) << "PC_FPGA::init: PCI interrut pin " << hdr.interrupt_pin << " routed to IRQ " << hdr.interrupt_line << " to trigger INT " << interrupt << endl;

    // Allocate a DMA Buffer for init block, rx and tx rings
    _dma_buf = new (SYSTEM) DMA_Buffer(DMA_BUFFER_SIZE);

    // Initialize the device
    _base = log_addr;
    reset();

    // Install interrupt handler
    IC::int_vector(interrupt, &int_handler);

    // Enable interrupts for device
    IC::enable(interrupt);
}

__END_SYS
