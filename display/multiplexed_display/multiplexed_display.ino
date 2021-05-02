#include <EEPROM_programmer.h>

EEPROM_programmer programmer(2, 3, 4, 5, 12, 13, 8192);

// Bit patterns for the digits 0..9
byte digits[] = {0x7e, 0x30, 0x6d, 0x79, 0x33, 0x5b, 0x5f, 0x70, 0x7f, 0x7b, 0x77, 0x1f, 0x4e, 0x3d, 0x4f, 0x47};

// write the unsigned display
int write_unsigned() {
  int offset = 0;
  int digitIndex;
  for (int place = 0; place < 3; place++) {
    Serial.println("Programming " + String(round(pow(10, place))) + "s place.");

    // the correct place for each of the 256 values
    for(int val = 0; val < 256; val++) {
      digitIndex = (val/round(pow(10, place))) % 10;

      // get rid of leading 0s
      if (place != 0 && digitIndex == 0 && val < round(pow(10, place))) {
        programmer.write_byte(val + offset, 0x00);  
      } else {
        programmer.write_byte(val + offset, digits[digitIndex]);
      }
    }

    offset += 256;
  }

  Serial.println("Programming sign");
  for (int val = 0; val < 256; val++) {
    programmer.write_byte(val + offset, 0);
  }

  offset += 256;
  return offset;
}

// write the two's compliment display
int write_twos_compliment(int offset) {
  for (int place = 0; place < 3; place++) {
    Serial.println("Programming " + String(round(pow(10, place))) + "s place (two's compliment).");
    
    for (int val = -128; val < 128; val++) {
      // casting to byte uses two's compliment value
      programmer.write_byte((byte)val + offset, digits[abs(val)/round(pow(10, place)) % 10]);
    } 

    offset += 256;
  }
  
  Serial.println("Programming sign (two's complement)");
  for (int val = -128; val < 128; val++) {
    if (val < 0) {
      programmer.write_byte((byte)val + offset, 0x01);
    } else {
      programmer.write_byte((byte)val + offset, 0);
    }
  }

  offset += 256;
  return offset;
}

// write the hexadecimal display
void write_hex_display(int offset) {
  for (int place = 0; place < 2; place++) {
    Serial.println("Programming " + String(round(pow(16, place))) + "s place (hex)");

    for (int val = 0; val < 255; val++) {
      // casting to byte uses two's compliment value
      programmer.write_byte(val + offset, digits[val/round(pow(16, place)) % 16]);
    } 

    offset += 256;
  }

  Serial.println("Programming sign");
  for (int place = 0; place < 2; place++) {
    for (int val = 0; val < 256; val++) {
      programmer.write_byte(val + offset, 0);
    }

    offset += 256;
  }
}

void setup() {
  Serial.begin(57600);
  
  int offset = write_unsigned();
//  offset = write_twos_compliment(offset);
//  Serial.println(offset);
//  write_hex_display(offset);
//
  Serial.println("Reading EEPROM");  
  programmer.print_contents();
}

void loop() {}
