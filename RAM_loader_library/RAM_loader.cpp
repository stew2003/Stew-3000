#include <Arduino.h>
#include "RAM_loader.h"

// constructor
RAM_loader::RAM_loader(int* _address_pins, int* _data_pins, int _write_enable) {
    address_pins = _address_pins;
    data_pins = _data_pins;
    write_enable = _write_enable;

    // set all the address pins to output mode
    for (int i = 0; i < sizeof(address_pins)/sizeof(address_pins[0]); i++) {
        pinMode(address_pins[i], OUTPUT);
    }

    // set all the data pins to output mode as well
    for (int i = 0; i < sizeof(data_pins)/sizeof(data_pins[0]); i++) {
        pinMode(data_pins[i], OUTPUT);
    }

    // set the write enable pin to default high
    pinMode(write_enable, OUTPUT);
    digitalWrite(write_enable, HIGH);
}

// set the adress on the RAM
void RAM_loader::set_address(int address) {
    // set all the adress pins to the appropriate value
    for (int i = 0; i < sizeof(address_pins)/sizeof(address_pins[0]); i++) {
        digitalWrite(address_pins[i], address & 1); // isolate least significant bit
        address = address >> 1; // shift off that LSB
    }
}


// write a single byte to the RAM
void RAM_loader::write_byte(int address, byte data) {
    set_address(address);

    // set all the data pins to the appropriate value
    for (int i = 0; i < sizeof(data_pins)/sizeof(data_pins[0]); i++) {
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
    for (int i = 0; i < sizeof(address_pins)/sizeof(address_pins[0]); i++) {
        pinMode(address_pins[i], OUTPUT);
        digitalWrite(address_pins[i], LOW);
    }

        // set all the address pins to input mode
    for (int i = 0; i < sizeof(data_pins)/sizeof(data_pins[0]); i++) {
        pinMode(data_pins[i], OUTPUT);
        digitalWrite(data_pins[i], LOW);
    }

    pinMode(write_enable, OUTPUT);
    pinMode(write_enable, HIGH);

}