#define SHIFT_DATA 2
#define SHIFT_CLOCK 3
#define SHIFT_LATCH 4
#define EEPROM_IO0 5
#define EEPROM_IO7 12
#define WRITE_ENABLE 13
#define EEPROM_SIZE 8192

// output the adress bits using a shift register
void setAddress(int address, bool outputEnable) {
  // chop of first 8 bits and then make the most significant bit a 1 or 0 depending on outoutEnable
  shiftOut(SHIFT_DATA, SHIFT_CLOCK, MSBFIRST, (address >> 8) | (outputEnable ? 0x00 : 0x80));
  // do the least significant 8 bits now
  shiftOut(SHIFT_DATA, SHIFT_CLOCK, MSBFIRST, address); 

  // output the results of the shifting
  digitalWrite(SHIFT_LATCH, LOW);
  digitalWrite(SHIFT_LATCH, HIGH);
  digitalWrite(SHIFT_LATCH, LOW);
}


// read a byte from the EEPROM from the specified address
byte readEEPROM(int address) {
  // make all the EEPROM I/O pins input
  for (int pin = EEPROM_IO0; pin <= EEPROM_IO7; pin += 1) {
    pinMode(pin, INPUT);
  }

  // go the the adress with output enabled
  setAddress(address, true);

  // shift the data from the I/O pins into a byte of data
  byte data = 0;
  for (int pin = EEPROM_IO7; pin >= EEPROM_IO0; pin -= 1) {
    data = (data << 1) + digitalRead(pin);
  }
  return data;
}


/*
 * Write a byte to the EEPROM at the specified address.
 */
void writeEEPROM(int address, byte data) {
  setAddress(address, false);
  
  for (int pin = EEPROM_IO0; pin <= EEPROM_IO7; pin += 1) {
    pinMode(pin, OUTPUT);
  }

  // output the correct data on each pin
  for (int pin = EEPROM_IO0; pin <= EEPROM_IO7; pin += 1) {
    digitalWrite(pin, data & 1); // isolate the lowest bit
    data = data >> 1; // shift by one
  }
  
  delayMicroseconds(1);
  digitalWrite(WRITE_ENABLE, LOW);
  delayMicroseconds(1);
  digitalWrite(WRITE_ENABLE, HIGH);
  delay(10);
}


/*
 * Read the contents of the EEPROM and print them to the serial monitor.
 */
void printContents() {
  for (int base = 0; base < 255; base += 16) {
    byte data[16];
    for (int offset = 0; offset <= 15; offset += 1) {
      data[offset] = readEEPROM(base + offset);
    }

    char buf[80];
    sprintf(buf, "%03x:  %02x %02x %02x %02x %02x %02x %02x %02x   %02x %02x %02x %02x %02x %02x %02x %02x",
            base, data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            data[8], data[9], data[10], data[11], data[12], data[13], data[14], data[15]);

    Serial.println(buf);
  }
}

// 4-bit hex decoder for common cathode 7-segment display
byte data[] = { 0x7e, 0x30, 0x6d, 0x79, 0x33, 0x5b, 0x5f, 0x70, 0x7f, 0x7b, 0x77, 0x1f, 0x4e, 0x3d, 0x4f, 0x47 };

void setup() {

  pinMode(SHIFT_DATA, OUTPUT);
  pinMode(SHIFT_CLOCK, OUTPUT);
  pinMode(SHIFT_LATCH, OUTPUT);
   
  pinMode(WRITE_ENABLE, OUTPUT);
  digitalWrite(WRITE_ENABLE, HIGH);
  
  Serial.begin(57600);

  // Program data bytes
  Serial.print("Programming EEPROM");
  for (int address = 0; address < sizeof(data); address++) {
    writeEEPROM(address, data[address]);

    if (address % 64 == 0) {
      Serial.print(".");
    }
  }
  Serial.println(" done");


  // Read and print out the contents of the EERPROM
  Serial.println("Reading EEPROM");
  printContents();
}

void loop() {}
