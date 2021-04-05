#ifndef EEPROM_programmer_h
#define EEPROM_programmer_h

#include <Arduino.h>

class EEPROM_programmer {
  public:
    EEPROM_programmer(int _shift_data, int _shift_clock, int _shift_latch, int _io_0, int _io_7, int _write_enable, int _num_addresses);
    void set_address(int address, bool output_enable);
    byte read_byte(int address);
    void write_byte(int address, byte data);
    void erase();
    void print_contents();
  private:
    int shift_data;
    int shift_clock;
    int shift_latch;
    int io_0;
    int io_7;
    int write_enable;
    int num_addresses;
};

#endif
