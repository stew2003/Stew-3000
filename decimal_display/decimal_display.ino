#include <EEPROM_programmer.h>

EEPROM_programmer programmer(2, 3, 4, 5, 12, 13, 8192);

// 4-bit hex decoder for common cathode 7-segment display
byte data[] = { 0x7e, 0x30, 0x6d, 0x79, 0x33, 0x5b, 0x5f, 0x70, 0x7f, 0x7b, 0x77, 0x1f, 0x4e, 0x3d, 0x4f, 0x47 };

void setup() {
  Serial.begin(57600);

  // Program data bytes
  Serial.print("Programming EEPROM");
  for (int address = 0; address < sizeof(data); address++) {
    programmer.write_byte(address, data[address]);

    if (address % 64 == 0) {
      Serial.print(".");
    }
  }
  Serial.println(" done");

  // Read and print out the contents of the EERPROM
  Serial.println("Reading EEPROM");
  programmer.print_contents();
}

void loop() {}
