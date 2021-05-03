#include <Arduino.h>
#include "RAM_loader.h"

// constructor
RAM_loader::RAM_loader(int* _address_pins, size_t _num_address_pins, int* _data_pins, size_t _num_data_pins, int _write_enable) {
    address_pins = _address_pins;
    num_address_pins = _num_address_pins;

    data_pins = _data_pins;
    num_data_pins = _num_data_pins;

    write_enable = _write_enable;

    // set the write enable pin to default high
    pinMode(write_enable, OUTPUT);
    digitalWrite(write_enable, HIGH);
}

// set the adress on the RAM
void RAM_loader::set_address(int address) {
    // set all the adress pins to the appropriate value
    for (int i = 0; i < num_address_pins; i++) {
        digitalWrite(address_pins[i], address & 1); // isolate least significant bit
        address = address >> 1; // shift off that LSB
    }
}


// write a single byte to the RAM
void RAM_loader::write_byte(int address, byte data) {
    // set all the address pins to output mode
    for (int i = 0; i < num_address_pins; i++) {
        pinMode(address_pins[i], OUTPUT);
    }

    // set all the data pins to output mode as well
    for (int i = 0; i < num_data_pins; i++) {
        pinMode(data_pins[i], OUTPUT);
    }

    set_address(address);

    // set all the data pins to the appropriate value
    for (int i = 0; i < num_data_pins; i++) {
        digitalWrite(data_pins[i], data & 1); // isolate the lowest bit
        data = data >> 1; // shift by one
    }
    
    // carry out the writing
    delayMicroseconds(10);
    digitalWrite(write_enable, LOW);
    delayMicroseconds(10);
    digitalWrite(write_enable, HIGH);
    delay(10);
}

// set all the pins to high impedance so they stop interfereing
void RAM_loader::standby() {
    // set all the address pins to input mode
    for (int i = 0; i < num_address_pins; i++) {
        pinMode(address_pins[i], INPUT);
    }

        // set all the address pins to input mode
    for (int i = 0; i < num_data_pins; i++) {
        pinMode(data_pins[i], INPUT);
    }

    pinMode(write_enable, OUTPUT);
    pinMode(write_enable, HIGH);

}