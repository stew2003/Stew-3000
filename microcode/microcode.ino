#include <EEPROM_programmer.h>
#include <avr/pgmspace.h>

EEPROM_programmer programmer(2, 3, 4, 5, 12, 13, 8192);

#define HLT 0b10000000000000000000000000000000
#define RST 0b01000000000000000000000000000000
#define PCE 0b00100000000000000000000000000000
#define PCO 0b00010000000000000000000000000000
#define PCI 0b00000001000000000000000000000000
#define MI  0b00000000100000000000000000000000
#define RO  0b00011000000000000000000000000000
#define RI  0b00000000010000000000000000000000
#define IO  0b00010100000000000000000000000000
#define II  0b00000000001000000000000000000000
#define AO  0b00011100000000000000000000000000
#define AI  0b00000000000100000000000000000000
#define BO  0b00011010000000000000000000000000
#define BI  0b00000000000010000000000000000000
#define CO  0b00010110000000000000000000000000
#define CI  0b00000000000001000000000000000000
#define TO  0b00011110000000000000000000000000
#define TI  0b00000000000000100000000000000000
#define OI  0b00000000000000010000000000000000
#define LCS 0b00000000000000001000000000000000
#define LCE 0b00000000000000000100000000000000
#define EO  0b00010010000000000000000000000000
#define EI  0b00000000000000000010000000000000
#define FI  0b00000000000000000001000000000000
#define NOT 0b00000000000000000000100000000000
#define AND 0b00000000000000000000010000000000
#define XOR 0b00000000000000000000110000000000
#define OR  0b00000000000000000000001000000000
#define SUM 0b00000000000000000000101000000000
#define TS  0b00000000000000000000000100000000
#define CAR 0b00000000000000000000000010000000
#define SUB 0b00000000000000000000000001000000
//#define PGM 0b00000000000000000000000000100000
//#define SO  0b00000000000000000000000000010000
//#define SI  0b00000000000000000000000000001000  

#define INSTRUCTION_STEPS 8
#define FETCH_STEPS 2
#define NUM_FLAGS 2
#define NUM_INSTRUCTIONS 39
#define NUM_C_INSTRUCTIONS 2
#define NUM_Z_INSTRUCTIONS 2
const int INSTRUCTION_ARR_LENGTH = 1 + (INSTRUCTION_STEPS - FETCH_STEPS);

const uint32_t PROGMEM fetch_cycle[2] = { PCO|MI, RO|II|PCE };

const uint32_t PROGMEM microcode[NUM_INSTRUCTIONS][INSTRUCTION_ARR_LENGTH] = {
  { 0x80, BO|TI,            AO|TS|SUM|EI|FI,         EO|AI,           RST,       0,   0 }, // ADD B
  { 0x81, CO|TI,            AO|TS|SUM|EI|FI,         EO|AI,           RST,       0,   0 }, // ADD C
  { 0xA0, BO|TI,            AO|TS|AND|EI|FI,         EO|AI,           RST,       0,   0 }, // ANA B
  { 0xA1, CO|TI,            AO|TS|AND|EI|FI,         EO|AI,           RST,       0,   0 }, // ANA C
  { 0xE6, PCO|MI,           RO|TI,                   AO|TS|AND|EI|FI, EO|AI|PCE, RST, 0 }, // ANI
  { 0x3D, AO|SUM|SUB|EI|FI, EO|AI,                   RST,             0,         0,   0 }, // DCR A
  { 0x05, BO|SUM|SUB|EI|FI, EO|BI,                   RST,             0,         0,   0 }, // DCR B
  { 0x0D, CO|SUM|SUB|EI|FI, EO|CI,                   RST,             0,         0,   0 }, // DCR C
  { 0x76, HLT,              HLT,                     HLT,             HLT,       0,   0 }, // HLT
  { 0x3C, AO|SUM|CAR|EI|FI, EO|AI,                   RST,             0,         0,   0 }, // INR A
  { 0x04, BO|SUM|CAR|EI|FI, EO|BI,                   RST,             0,         0,   0 }, // INR B
  { 0x0C, CO|SUM|CAR|EI|FI, EO|CI,                   RST,             0,         0,   0 }, // INR C
  { 0xC3, PCO|MI,           RO|PCI,                  RST,             0,         0,   0 }, // JMP
  { 0x3A, PCO|MI,           RO|MI,                   RO|AI|PCE,       RST,       0,   0 }, // LDA
  { 0x78, AO|BI,            RST,                     0,               0,         0,   0 }, // MOV A, B
  { 0x79, AO|CI,            RST,                     0,               0,         0,   0 }, // MOV A, C
  { 0x47, BO|AI,            RST,                     0,               0,         0,   0 }, // MOV B, A
  { 0x41, BO|CI,            RST,                     0,               0,         0,   0 }, // MOV B, C
  { 0x4F, CO|AI,            RST,                     0,               0,         0,   0 }, // MOV C, A
  { 0x48, CO|BI,            RST,                     0,               0,         0,   0 }, // MOV C, B
  { 0x3E, PCO|MI,           RO|AI|PCE,               RST,             0,         0,   0 }, // MVI A
  { 0x06, PCO|MI,           RO|BI|PCE,               RST,             0,         0,   0 }, // MVI B
  { 0x0E, PCO|MI,           RO|CI|PCE,               RST,             0,         0,   0 }, // MVI C
  { 0x00, 0,                0,                       0,               0,         0,   0 }, // NOP
  { 0xB0, BO|TI,            AO|TS|OR|EI|FI,          EO|AI,           0,         0,   0 }, // ORA B
  { 0xB1, CO|TI,            AO|TS|OR|EI|FI,          EO|AI,           0,         0,   0 }, // ORA C
  { 0xF6, PCO|MI,           RO|TI,                   AO|TS|OR|EI|FI,  EO|AI|PCE, 0,   0 }, // ORI
  { 0xD3, AO|OI,            RST,                     0,               0,         0,   0 }, // OUT
  { 0x32, PCO|MI,           RO|MI,                   AO|RI|PCE,       RST,       0,   0 }, // STA
  { 0x90, BO|TI,            AO|TS|SUM|SUB|CAR|EI|FI, EO|AI,           RST,       0,   0 }, // SUB B
  { 0x91, CO|TI,            AO|TS|SUM|SUB|CAR|EI|FI, EO|AI,           RST,       0,   0 }, // SUB C
  { 0xA8, BO|TI,            AO|TS|XOR|EI|FI,         EO|AI,           RST,       0,   0 }, // XRA B
  { 0xA9, CO|TI,            AO|TS|XOR|EI|FI,         EO|AI,           RST,       0,   0 }, // XRA C
  { 0xEE, PCO|MI,           RO|TI,                   AO|TS|XOR|EI|FI, EI|AI|PCE, 0,   0 }, // XRI
  { 0xAA, AO|TI,            TS|NOT|SUB|EI|FI,        EO|AI,           RST,       0,   0 }, // NOT A
  { 0xAB, BO|TI,            TS|NOT|SUB|EI|FI,        EO|BI,           RST,       0,   0 }, // NOT B
  { 0xAC, CO|TI,            TS|NOT|SUB|EI|FI,        EO|CI,           RST,       0,   0 }, // NOT C
  { 0xF0, PCO|MI,           RO|LCE|PCE,              RST,             0,         0,   0 }, // DIC
  { 0xF1, PCO|MI,           RO|LCE|LCS|PCE,          RST,             0,         0,   0 }, // DID 
  { 0xFF, BO|TI,            PCO|MI,                  RO|TS|ADD|EI,    EO|MI,     RO|AI|PCE, RST} //LDB A, B, offset
};

// microcode dependent on the c flag
const uint32_t PROGMEM c_microcode[NUM_C_INSTRUCTIONS][2][INSTRUCTION_ARR_LENGTH] = {
  {
    {0xDA, PCE,    RST,    0,   0, 0, 0 }, // JC - c = 0
    {0xDA, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // JC - c = 1
  },
  {
    {0xD2, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // JNC - c = 0
    {0xD2, PCE,    RST,    0,   0, 0, 0 }, // JNC - c = 1
  }
};

// microcode dependent on the z flag
const uint32_t PROGMEM z_microcode[NUM_Z_INSTRUCTIONS][2][INSTRUCTION_ARR_LENGTH] = {
  {
    {0xCA, PCE,    RST,    0,   0, 0, 0 }, // JZ - z = 0
    {0xCA, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // JZ - z = 1
  },
  {
    {0xC2, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // JNZ - z = 0
    {0xC2, PCE,    RST,    0,   0, 0, 0 }, // JNZ - z = 1
  }
};

// reverse all bits in a byte
byte reverse_bits(byte b) {
   b = (b & 0xF0) >> 4 | (b & 0x0F) << 4;
   b = (b & 0xCC) >> 2 | (b & 0x33) << 2;
   b = (b & 0xAA) >> 1 | (b & 0x55) << 1;
   return b;
}

void program_EEPROM(int num) {
  // write all the unconditional instructions
  for (int instruction_index = 0; instruction_index < NUM_INSTRUCTIONS; instruction_index++) {
    for (int step_index = 0; step_index < INSTRUCTION_STEPS; step_index++) {
      for (int flags = 0; flags < pow(2, NUM_FLAGS); flags++) {
        programmer.write_byte(
          pgm_read_dword_near(&microcode[instruction_index][0]) + (256 * (step_index)) + (2048 * flags), 
          reverse_bits(
            pgm_read_dword_near(step_index < FETCH_STEPS ? &fetch_cycle[step_index]: &microcode[instruction_index][step_index - FETCH_STEPS + 1]) >> (32 - 8*num)
          )
        );
      }
    }
  }

  // write all the conditional instructions dependent on c flag
  for (int instruction_index = 0; instruction_index < NUM_INSTRUCTIONS; instruction_index++) {
    for (int step_index = 0; step_index < INSTRUCTION_STEPS; step_index++) {
      for (int flags = 0; flags < pow(2, NUM_FLAGS); flags++) { // 0 - z = 0, c = 0; 1 - z = 0, c = 1; 2 - z = 1, c = 0; 3 - z = 1, c = 1
        programmer.write_byte(
          pgm_read_dword_near(&c_microcode[instruction_index][0][0]) + (256 * step_index) + (2048 * flags), 
          reverse_bits(
            pgm_read_dword_near(step_index < FETCH_STEPS ? &fetch_cycle[step_index]: &c_microcode[instruction_index][flags & 0b01][step_index - FETCH_STEPS + 1]) >> (32 - 8*num)
          )
        );
      }
    }
  }

  // write all the conditional instructions dependent on z flag
  for (int instruction_index = 0; instruction_index < NUM_INSTRUCTIONS; instruction_index++) {
    for (int step_index = 0; step_index < INSTRUCTION_STEPS; step_index++) {
      for (int flags = 0; flags < pow(2, NUM_FLAGS); flags++) { // 0 - z = 0, c = 0; 1 - z = 0, c = 1; 2 - z = 1, c = 0; 3 - z = 1, c = 1
        programmer.write_byte(
          pgm_read_dword_near(&c_microcode[instruction_index][0][0]) + (256 * step_index) + (2048 * flags), 
          reverse_bits(
            pgm_read_dword_near(step_index < FETCH_STEPS ? &fetch_cycle[step_index]: &z_microcode[instruction_index][flags & 0b10][step_index - FETCH_STEPS + 1]) >> (32 - 8*num)
          )
        );
      }
    }
  }
}


void setup() {
  Serial.begin(57600);
  program_EEPROM(4);
  programmer.print_contents();
}

void loop() {}
