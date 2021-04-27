#ifndef RAM_loader_h
#define RAM_loader_h

#include <Arduino.h>

class RAM_loader {
  public:
    RAM_loader(int* _address_pins, int* _data_pins, int _write_enable);
    void set_address(int address);
    void write_byte(int address, byte data);
    void standby();
  private:
    int write_enable;
    int* data_pins;
    int* address_pins;
};

#endif
