#include <Arduino.h>
#include "EEPROM_programmer.h"

// constructor
EEPROM_programmer::EEPROM_programmer(int _shift_data, int _shift_clock, int _shift_latch, int _io_0, int _io_7, int _write_enable, int _num_addresses) {
    shift_data = _shift_data;
    shift_clock = _shift_clock;
    shift_latch = _shift_latch;
    io_0 = _io_0;
    io_7 = _io_7;
    write_enable = _write_enable;
    num_addresses = _num_addresses;

    pinMode(shift_data, OUTPUT);
    pinMode(shift_clock, OUTPUT);
    pinMode(shift_latch, OUTPUT);

    pinMode(write_enable, OUTPUT);
    digitalWrite(write_enable, HIGH);
}

void EEPROM_programmer::set_address(int address, bool output_enable) {
    // chop of first 8 bits and then make the most significant bit a 1 or 0 depending on outoutEnable
    shiftOut(shift_data, shift_clock, MSBFIRST, (address >> 8) | (output_enable ? 0x00 : 0x80));
    // do the least significant 8 bits now
    shiftOut(shift_data, shift_clock, MSBFIRST, address); 

    // output the results of the shifting
    digitalWrite(shift_latch, LOW);
    digitalWrite(shift_latch, HIGH);
    digitalWrite(shift_latch, LOW);
}

// read a single byte from the EEPROM
byte EEPROM_programmer::read_byte(int address) {
    // make all the EEPROM I/O pins input
    for (int pin = io_0; pin <= io_7; pin++) {
        pinMode(pin, INPUT);
    }

    // go the the adress with output enabled
    set_address(address, true);

    // shift the data from the I/O pins into a byte of data
    byte data = 0;
    for (int pin = io_7; pin >= io_0; pin--) {
        data = (data << 1) + digitalRead(pin);
    }
    return data;
}

// write a single byte to the EEPROM
void EEPROM_programmer::write_byte(int address, byte data) {
    set_address(address, false);
  
    for (int pin = io_0; pin <= io_7; pin ++) {
        pinMode(pin, OUTPUT);
    }

    // output the correct data on each pin
    for (int pin = io_0; pin <= io_7; pin++) {
        digitalWrite(pin, data & 1); // isolate the lowest bit
        data = data >> 1; // shift by one
    }
    
    // carry out the writing
    delayMicroseconds(1);
    digitalWrite(write_enable, LOW);
    delayMicroseconds(1);
    digitalWrite(write_enable, HIGH);
    delay(10);
}

void EEPROM_programmer::erase() {
    for (int address = 0; address < num_addresses; address++) {
        write_byte(address, 0x00);
    }
}

// neatly print out the contents of the EEPROM
void EEPROM_programmer::print_contents() {
    for (int base = 0; base < 1024; base += 16) {
        byte data[16];
        for (int offset = 0; offset < 16; offset++) {
            data[offset] = read_byte(base + offset);
        }

        char buf[80];
        sprintf(buf, "%03x:  %02x %02x %02x %02x %02x %02x %02x %02x   %02x %02x %02x %02x %02x %02x %02x %02x",
            base, data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15]);

        Serial.println(buf);
    }
}