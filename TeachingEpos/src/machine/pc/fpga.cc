// EPOS PC FPGA Mediator Implementation

#include <machine/pc/machine.h>
#include <machine/pc/ic.h>
#include <machine/pc/fpga.h>

__BEGIN_SYS

// Class attributes
CPU::Log_Addr PC_FPGA::Engine::_base;
MMU::DMA_Buffer * PC_FPGA::Engine::_dma_buf;

// Class methods
void PC_FPGA::int_handler(const IC::Interrupt_Id & i)
{
    
}

__END_SYS
