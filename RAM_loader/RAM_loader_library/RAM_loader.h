#ifndef RAM_loader_h
#define RAM_loader_h

#include <Arduino.h>

class RAM_loader {
  public:
    RAM_loader(int* _address_pins, size_t _num_address_pins, int* _data_pins, size_t _num_data_pins, int _write_enable);
    void set_address(int address);
    void write_byte(int address, byte data);
    void standby();
  private:
    int* data_pins;
    size_t num_address_pins;

    int* address_pins;
    size_t num_data_pins;

    int write_enable;
};

#endif
