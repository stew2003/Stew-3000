#include <EEPROM_programmer.h>

EEPROM_programmer programmer(2, 3, 4, 5, 12, 13, 8192);

const int NUM_FLAGS = 4;
//const int NUM_CONDITIONAL_INSTRUCTIONS = 39;
const int NUM_CONDITIONAL_INSTRUCTIONS = 1;

typedef struct conditional_t {
    uint8_t op_code;
    uint8_t (*condition)(uint8_t);
} conditional_t;

uint8_t carry(uint8_t flags) {
    return (flags & 0b0001) ? 1: 0;
}

uint8_t je(uint8_t flags) {
    return (flags & 0b1000) ? 1 : 0;
}

uint8_t jne(uint8_t flags) {
    return je(flags) ? 0 : 1;
}

uint8_t jg(uint8_t flags) {
    int sign_flag = (flags & 0b0100) ? 1 : 0
    int overflow_flag = (flags & 0b0010) ? 1 : 0
    int zero_flag = (flags & 0b1000) ? 1 : 0
    return 
}

uint8_t jge(uint8_t flags) {
    return (~(((flags & 0b0100)) ^ (flags & 0b0010))) ? 1 : 0; 
}

uint8_t jl(uint8_t flags) {
    return jge(flags) ? 0 : 1; 
}

uint8_t jle(uint8_t flags) {
    return jg(flags) ? 0 : 1; 
}

uint8_t ja(uint8_t flags) {
    return (~(flags & 0b0001) & ~(flags & 0b1000)) ? 1 : 0;
}

uint8_t jae(uint8_t flags) {
    return (~(flags & 0b0001)) ? 1 : 0;
}

uint8_t jb(uint8_t flags) {
    return jae(flags) ? 0 : 1;
}

uint8_t jbe(uint8_t flags) {
    return ja(flags) ? 0 : 1;
}

//  0   0   0   0
//  ZF  SF  OF  CF
const conditional_t conditionals[NUM_CONDITIONAL_INSTRUCTIONS] = {
//    // addc $r1, $r2
//    { 0x10, carry }, // addc a, a
//    { 0x11, carry }, // addc a, b
//    { 0x12, carry }, // addc a, c
//    { 0x13, carry }, // addc a, sp
//    { 0x14, carry }, // addc b, a
//    { 0x15, carry }, // addc b, b
//    { 0x16, carry }, // addc b, c
//    { 0x17, carry }, // addc b, sp
//    { 0x18, carry }, // addc c, a
//    { 0x19, carry }, // addc c, b
//    { 0x1a, carry }, // addc c, c
//    { 0x1b, carry }, // addc c, sp
//
//    // addci byte, $r1
//    { 0x1c, carry }, // addci byte, a
//    { 0x1d, carry }, // addci byte, b
//    { 0x1e, carry }, // addci byte, c
//    { 0x1f, carry }, // addci byte, sp
//    
//    // subb $r1, $r2
//    { 0x2d, carry }, // subb b, a
//    { 0x2e, carry }, // subb c, a
//    { 0x2f, carry }, // subb a, b
//    { 0x30, carry }, // subb c, b
//    { 0x31, carry }, // subb a, c
//    { 0x32, carry }, // subb b, c
//    { 0x33, carry }, // subb a, sp
//    { 0x34, carry }, // subb b, sp
//    { 0x35, carry }, // subb c, sp
//
//    // subbi byte, $r1
//    { 0x36, carry }, // subbi byte, a
//    { 0x37, carry }, // subbi byte, b
//    { 0x38, carry }, // subbi byte, c
//    { 0x39, carry }, // subbi byte, sp

//    // je byte
//    { 0xb2, je },

//    // jne byte
//    { 0xb3, jne },
//
    // jg byte
    { 0xb4, jg },
//
//    // jge byte
//    { 0xb5, jge },
//
//    // jl byte
//    { 0xb6, jl },
//
//    // jle byte
//    {0xb7, jle },
//
//    // ja byte
//    { 0xb8, ja },
//
//    // jae byte
//    { 0xb9, jae },
//
//    // jb byte
//    { 0xba, jb },
//
//    // jbe byte
//    { 0xbb, jbe }
};

void program_conditionals() {
    for (int instr_index = 0; instr_index < NUM_CONDITIONAL_INSTRUCTIONS; instr_index++) {
        for (int flags = 0; flags < pow(2, NUM_FLAGS); flags++) {
//            programmer.write_byte(
//                conditionals[instr_index].op_code + (256 * flags),
//                (*conditionals[instr_index].condition)(flags)
//            );
            Serial.print(conditionals[instr_index].op_code + (256 * flags));
            Serial.print(" ");
            Serial.println((*conditionals[instr_index].condition)(flags));
        }
        Serial.println();
    }
}

void setup() {
  Serial.begin(57600);
  Serial.println("Beginning erasing EEPROM.");
//  programmer.erase();
  Serial.println("Finished erasing EEPROM.");
  program_conditionals();
//  programmer.print_contents();
}

void loop() {}
