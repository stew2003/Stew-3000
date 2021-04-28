#include <RAM_loader.h>

int data_pins[] = { 11, 12, 19, 18, 17, 16, 15, 14 };
int address_pins[] = { 2, 3, 4, 5, 6, 7, 8, 9 };

RAM_loader loader(address_pins, data_pins, 10);

int instruction_index;

void setup() {
  Serial.begin(9600); // set the baud rate    
  Serial.println("Ready"); // print "Ready" once

  instr_index = 0;
}

void loop() {
  // delay until data to arrives
  while (!Serial.available()) {}
  
  // serial read section
  while (Serial.available()) {
    byte instr = Serial.read();
    
    Serial.print("Arduino received: ");
    Serial.print(instr, HEX);
    Serial.print(" at ");
    Serial.println(instr_index, HEX); 

    loader.write_byte(instr_index, instr);
    instr_index++;
  }
  
}
