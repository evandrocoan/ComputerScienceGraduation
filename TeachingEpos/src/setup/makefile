# EPOS Setup Makefile

include ../../makedefs

TARGET := $(if $(shell find $(MACH)* 2> /dev/null), $(if $(SETUP_ADDR), install, install.o))

all:		$(TARGET)

cortex_m_setup.o: cortex_m_setup.s
		sed -e 's/MEM_TOP/$(call GETTK,MEM_TOP,$(MACH_TRAITS))/' -i $<
		$(AS) $(ASFLAGS) -o $@ $<

$(MACH)_setup:	$(MACH)_setup.o
		$(LD) $(LDFLAGS) -L$(CCLIB) --omagic --section-start .init=$(SETUP_ADDR) -o $@ $^ -l$(LINIT) -l$(LMACH) -l$(LARCH) -l$(LUTIL) -lgcc

install:	$(MACH)_setup
		$(INSTALL) $< $(IMG)
		
install.o:	$(MACH)_setup.o
		$(INSTALL) $< $(LIB)

clean:
		$(CLEAN) *.o *.s *_setup
