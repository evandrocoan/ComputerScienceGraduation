// EPOS PC EEPROM Mediator Common Declarations

/**
 * EEPROM (also E2PROM) stands for Electrically Erasable Programmable Read-Only Memory and is a type
 * of non-volatile memory used in computers, integrated in microcontrollers for smart cards and
 * remote keyless system, and other electronic devices to store relatively small amounts of data but
 * allowing individual bytes to be erased and reprogrammed.
 * 
 * EEPROMs are organized as arrays of floating-gate transistors. EEPROMs can be programmed and
 * erased in-circuit, by applying special programming signals. Originally, EEPROMs were limited to
 * single byte operations which made them slower, but modern EEPROMs allow multi-byte page
 * operations. It also has a limited life for erasing and reprogramming, now reaching a million
 * operations in modern EEPROMs. In an EEPROM that is frequently reprogrammed while the computer is
 * in use, the life of the EEPROM is an important design consideration.
 * 
 * Flash memory is a type of EEPROM designed for high speed and high density, at the expense of
 * large erase blocks (typically 512 bytes or larger) and limited number of write cycles (often
 * 10,000). There is no clear boundary dividing the two, but the term "EEPROM" is generally used to
 * describe non-volatile memory with small erase blocks (as small as one byte) and a long lifetime
 * (typically 1,000,000 cycles). Many microcontrollers include both: flash memory for the firmware,
 * and a small EEPROM for parameters and history. https://en.wikipedia.org/wiki/EEPROM
 */

#ifndef __pc_eeprom_h
#define __pc_eeprom_h

#include <eeprom.h>
#include "rtc.h"

__BEGIN_SYS

class PC_EEPROM: public EEPROM_Common, private MC146818
{
public:
    typedef EEPROM_Common::Address Address;

public:
    PC_EEPROM() {};

    unsigned char read(const Address & a) { return cmos_read(a); }
    void write(const Address & a, unsigned char d) { cmos_write(a, d); }

    int size() const { return cmos_size(); }
};

__END_SYS

#endif
