#include <RAM_loader.h>

int data_pins[] = { 11, 12, 19, 18, 17, 16, 15, 14 };
int address_pins[] = { 2, 3, 4, 5, 6, 7, 8, 9 };

RAM_loader loader(address_pins, data_pins, 10);

void setup() {
}

void loop() {
  
}
