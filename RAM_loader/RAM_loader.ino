#include <RAM_loader.h>

int data_pins[] = { 11, 12, 19, 18, 17, 16, 15, 14 };
int address_pins[] = { 2, 3, 4, 5, 6, 7, 8, 9 };
int write_enable = 10;

RAM_loader* loader;

int instr_index;

void setup() {
  Serial.begin(9600); // set the baud rate
  Serial.println("Ready"); // print "Ready" once

  loader = new RAM_loader(address_pins, sizeof(address_pins) / sizeof(address_pins[0]), data_pins, sizeof(data_pins) / sizeof(data_pins[0]), write_enable);

  instr_index = 0;
}

void loop() {
  // delay until data to arrives
  while (!Serial.available()) {
    loader->standby();
  }

  // serial read section
  while (Serial.available()) {
    byte instr = Serial.read();

    Serial.print("Arduino received: ");
    Serial.print(instr, HEX);
    Serial.print(" at ");
    Serial.println(instr_index, HEX);

    loader->write_byte(instr_index, instr);
    instr_index++;
  }

}
