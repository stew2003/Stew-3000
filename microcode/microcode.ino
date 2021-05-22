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
#define SO  0b00000000000000000000000000100000
#define SI  0b00000000000000000000000000010000
#define PGM 0b00000000000000000000000000001000  

const int INSTRUCTION_STEPS = 8;
const int FETCH_STEPS = 2;

const int NUM_FLAGS = 3;
const int FLAG_CONFIGURATIONS = pow(2, NUM_FLAGS);

const int NUM_INSTRUCTIONS = 122;
const int NUM_CONDITIONAL_INSTRUCTIONS = 6;

const int INSTRUCTION_ARR_LENGTH = 1 + (INSTRUCTION_STEPS - FETCH_STEPS);

const uint32_t PROGMEM fetch_cycle[2] = { PCO|MI, RO|II|PCE };

const uint32_t PROGMEM microcode[NUM_INSTRUCTIONS][INSTRUCTION_ARR_LENGTH] = {
  // ADD $r1, $r2: $r2 = $r2 + $r1
  { 0x00, AO|TI, AO|TS|SUM|EI|FI, EO|AI, RST, 0, 0 }, // ADD A, A
  { 0x01, AO|TI, BO|TS|SUM|EI|FI, EO|BI, RST, 0, 0 }, // ADD A, B
  { 0x02, AO|TI, CO|TS|SUM|EI|FI, EO|CI, RST, 0, 0 }, // ADD A, C
  { 0x03, AO|TI, SO|TS|SUM|EI|FI, EO|SI, RST, 0, 0 }, // ADD A, SP
  { 0x04, BO|TI, AO|TS|SUM|EI|FI, EO|AI, RST, 0, 0 }, // ADD B, A
  { 0x05, BO|TI, BO|TS|SUM|EI|FI, EO|BI, RST, 0, 0 }, // ADD B, B
  { 0x06, BO|TI, CO|TS|SUM|EI|FI, EO|CI, RST, 0, 0 }, // ADD B, C
  { 0x07, BO|TI, SO|TS|SUM|EI|FI, EO|SI, RST, 0, 0 }, // ADD B, SP
  { 0x08, CO|TI, AO|TS|SUM|EI|FI, EO|AI, RST, 0, 0 }, // ADD C, A
  { 0x09, CO|TI, BO|TS|SUM|EI|FI, EO|BI, RST, 0, 0 }, // ADD C, B
  { 0x0a, CO|TI, CO|TS|SUM|EI|FI, EO|CI, RST, 0, 0 }, // ADD C, C
  { 0x0b, CO|TI, SO|TS|SUM|EI|FI, EO|SI, RST, 0, 0 }, // ADD C, SP

  // ADDI byte, $r1: $r1 = $r1 + byte
  { 0x0c, PCO|MI, RO|TI, AO|TS|SUM|EI|FI, EO|AI|PCE, RST, 0 }, // ADDI byte, A
  { 0x0d, PCO|MI, RO|TI, BO|TS|SUM|EI|FI, EO|BI|PCE, RST, 0 }, // ADDI byte, B
  { 0x0e, PCO|MI, RO|TI, CO|TS|SUM|EI|FI, EO|CI|PCE, RST, 0 }, // ADDI byte, C
  { 0x0f, PCO|MI, RO|TI, SO|TS|SUM|EI|FI, EO|SI|PCE, RST, 0 }, // ADDI byte, A

  // SUB $r1, $r2: $r2 = $r2 - $r1
  { 0x10, BO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI, RST, 0, 0 }, // SUB B, A
  { 0x11, CO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI, RST, 0, 0 }, // SUB C, A
  { 0x12, AO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI, RST, 0, 0 }, // SUB A, B
  { 0x13, CO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI, RST, 0, 0 }, // SUB C, B
  { 0x14, AO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI, RST, 0, 0 }, // SUB A, C
  { 0x15, BO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI, RST, 0, 0 }, // SUB B, C
  { 0x16, AO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST, 0, 0 }, // SUB A, SP
  { 0x17, BO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST, 0, 0 }, // SUB B, SP
  { 0x18, CO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST, 0, 0 }, // SUB C, SP

  // SUBI byte, $r1: $r1 = $r1 - byte
  { 0x19, PCO|MI, RO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI|PCE, RST, 0 }, // SUB byte, A
  { 0x1a, PCO|MI, RO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI|PCE, RST, 0 }, // SUB byte, B
  { 0x1b, PCO|MI, RO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI|PCE, RST, 0 }, // SUB byte, C
  { 0x1c, PCO|MI, RO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI|PCE, RST, 0 }, // SUB byte, SP

  // AND $r1, $r2: $r2 = $r2 & $r1
  { 0x1d, BO|TI, AO|TS|AND|EI|FI, EO|AI, RST, 0, 0 }, // AND B, A
  { 0x1e, CO|TI, AO|TS|AND|EI|FI, EO|AI, RST, 0, 0 }, // AND C, A
  { 0x1f, AO|TI, BO|TS|AND|EI|FI, EO|BI, RST, 0, 0 }, // AND A, B
  { 0x20, CO|TI, BO|TS|AND|EI|FI, EO|BI, RST, 0, 0 }, // AND C, B
  { 0x21, AO|TI, CO|TS|AND|EI|FI, EO|CI, RST, 0, 0 }, // AND A, C
  { 0x22, BO|TI, CO|TS|AND|EI|FI, EO|CI, RST, 0, 0 }, // AND B, C
  
  // ANI byte, $r1: $r1 = $r1 & byte
  { 0x23, PCO|MI, RO|TI, AO|TS|AND|EI|FI, EO|AI|PCE, RST, 0 }, // ANI byte, A
  { 0x24, PCO|MI, RO|TI, BO|TS|AND|EI|FI, EO|BI|PCE, RST, 0 }, // ANI byte, B
  { 0x25, PCO|MI, RO|TI, CO|TS|AND|EI|FI, EO|CI|PCE, RST, 0 }, // ANI byte, C

  // OR $r1, $r2: $r2 = $r2 & $r1
  { 0x26, BO|TI, AO|TS|OR|EI|FI, EO|AI, RST, 0, 0 }, // OR B, A
  { 0x27, CO|TI, AO|TS|OR|EI|FI, EO|AI, RST, 0, 0 }, // OR C, A
  { 0x28, AO|TI, BO|TS|OR|EI|FI, EO|BI, RST, 0, 0 }, // OR A, B
  { 0x29, CO|TI, BO|TS|OR|EI|FI, EO|BI, RST, 0, 0 }, // OR C, B
  { 0x2a, AO|TI, CO|TS|OR|EI|FI, EO|CI, RST, 0, 0 }, // OR A, C
  { 0x2b, BO|TI, CO|TS|OR|EI|FI, EO|CI, RST, 0, 0 }, // OR B, C
  
  // ORI byte, $r1: $r1 = $r1 & byte
  { 0x2c, PCO|MI, RO|TI, AO|TS|OR|EI|FI, EO|AI|PCE, RST, 0 }, // ORI byte, A
  { 0x2d, PCO|MI, RO|TI, BO|TS|OR|EI|FI, EO|BI|PCE, RST, 0 }, // ORI byte, B
  { 0x2e, PCO|MI, RO|TI, CO|TS|OR|EI|FI, EO|CI|PCE, RST, 0 }, // ORI byte, C

  // XOR $r1, $r2: $r2 = $r2 & $r1
  { 0x2f, BO|TI, AO|TS|XOR|EI|FI, EO|AI, RST, 0, 0 }, // XOR B, A
  { 0x30, CO|TI, AO|TS|XOR|EI|FI, EO|AI, RST, 0, 0 }, // XOR C, A
  { 0x31, AO|TI, BO|TS|XOR|EI|FI, EO|BI, RST, 0, 0 }, // XOR A, B
  { 0x32, CO|TI, BO|TS|XOR|EI|FI, EO|BI, RST, 0, 0 }, // XOR C, B
  { 0x33, AO|TI, CO|TS|XOR|EI|FI, EO|CI, RST, 0, 0 }, // XOR A, C
  { 0x34, BO|TI, CO|TS|XOR|EI|FI, EO|CI, RST, 0, 0 }, // XOR B, C
  
  // XRI byte, $r1: $r1 = $r1 & byte
  { 0x35, PCO|MI, RO|TI, AO|TS|XOR|EI|FI, EO|AI|PCE, RST, 0 }, // XRI byte, A
  { 0x36, PCO|MI, RO|TI, BO|TS|XOR|EI|FI, EO|BI|PCE, RST, 0 }, // XRI byte, B
  { 0x37, PCO|MI, RO|TI, CO|TS|XOR|EI|FI, EO|CI|PCE, RST, 0 }, // XRI byte, C

  // NOT $r1: $r1 = ~$r1
  { 0x38, AO|TI, TS|NOT|SUB|EI|FI, EO|AI, RST, 0, 0 }, // NOT A
  { 0x39, BO|TI, TS|NOT|SUB|EI|FI, EO|BI, RST, 0, 0 }, // NOT B
  { 0x3a, CO|TI, TS|NOT|SUB|EI|FI, EO|CI, RST, 0, 0 }, // NOT C

  // INR $r1: $r1 = $r1 + 1
  { 0x3b, AO|SUM|CAR|EI|FI, EO|AI, RST, 0, 0, 0 }, // INR A
  { 0x3c, BO|SUM|CAR|EI|FI, EO|BI, RST, 0, 0, 0 }, // INR B
  { 0x3d, CO|SUM|CAR|EI|FI, EO|CI, RST, 0, 0, 0 }, // INR C
  { 0x3e, SO|SUM|CAR|EI|FI, EO|SI, RST, 0, 0, 0 }, // INR C

  // DCR $r1: $r1 = $r1 - 1
  { 0x3f, AO|SUM|SUB|EI|FI, EO|AI, RST, 0, 0, 0 }, // DCR A
  { 0x40, BO|SUM|SUB|EI|FI, EO|BI, RST, 0, 0, 0 }, // DCR B
  { 0x41, CO|SUM|SUB|EI|FI, EO|CI, RST, 0, 0, 0 }, // DCR C
  { 0x42, SO|SUM|SUB|EI|FI, EO|SI, RST, 0, 0, 0 }, // DCR A

  // MOV $r1, $r2: $r2 = $r1
  { 0x43, AO|BI, RST, 0, 0, 0, 0 }, // MOV A, B
  { 0x44, AO|CI, RST, 0, 0, 0, 0 }, // MOV A, C
  { 0x45, BO|AI, RST, 0, 0, 0, 0 }, // MOV B, A
  { 0x46, BO|CI, RST, 0, 0, 0, 0 }, // MOV B, C
  { 0x47, CO|AI, RST, 0, 0, 0, 0 }, // MOV C, A
  { 0x48, CO|BI, RST, 0, 0, 0, 0 }, // MOV C, B

  // MVI byte, $r1: $r1 = byte
  { 0x49, PCO|MI, RO|AI|PCE, RST, 0, 0, 0 }, // MVI byte, A
  { 0x4a, PCO|MI, RO|BI|PCE, RST, 0, 0, 0 }, // MVI byte, B
  { 0x4b, PCO|MI, RO|CI|PCE, RST, 0, 0, 0 }, // MVI byte, C

  // LD $r1, $r2: $r2 = RAM[$r1]
  { 0x4c, AO|MI, PGM|RO|AI, RST, 0, 0, 0 }, // LD A, A
  { 0x4d, BO|MI, PGM|RO|AI, RST, 0, 0, 0 }, // LD B, A
  { 0x4e, CO|MI, PGM|RO|AI, RST, 0, 0, 0 }, // LD C, A
  { 0x4f, AO|MI, PGM|RO|BI, RST, 0, 0, 0 }, // LD A, B
  { 0x50, BO|MI, PGM|RO|BI, RST, 0, 0, 0 }, // LD B, B
  { 0x51, CO|MI, PGM|RO|BI, RST, 0, 0, 0 }, // LD C, B
  { 0x52, AO|MI, PGM|RO|CI, RST, 0, 0, 0 }, // LD A, C
  { 0x53, BO|MI, PGM|RO|CI, RST, 0, 0, 0 }, // LD B, C
  { 0x54, CO|MI, PGM|RO|CI, RST, 0, 0, 0 }, // LD C, C

  // ST $r1, $r2: RAM[$r2] = $r1
  { 0x55, AO|MI, PGM|AO|RI, RST, 0, 0, 0 }, //ST A, A
  { 0x56, BO|MI, PGM|AO|RI, RST, 0, 0, 0 }, //ST A, B
  { 0x57, CO|MI, PGM|AO|RI, RST, 0, 0, 0 }, //ST A, C
  { 0x58, AO|MI, PGM|BO|RI, RST, 0, 0, 0 }, //ST B, A
  { 0x59, BO|MI, PGM|BO|RI, RST, 0, 0, 0 }, //ST B, B
  { 0x5a, CO|MI, PGM|BO|RI, RST, 0, 0, 0 }, //ST B, C
  { 0x5b, AO|MI, PGM|CO|RI, RST, 0, 0, 0 }, //ST C, A
  { 0x5c, BO|MI, PGM|CO|RI, RST, 0, 0, 0 }, //ST C, B
  { 0x5d, CO|MI, PGM|CO|RI, RST, 0, 0, 0 }, //ST C, C

  // LDS byte, $r1: $r1 = RAM[SP + byte]
  { 0x5e, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, PGM|RO|AI|PCE, RST }, // LDS byte, A
  { 0x5f, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, PGM|RO|BI|PCE, RST }, // LDS byte, B
  { 0x60, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, PGM|RO|CI|PCE, RST }, // LDS byte, C

  // STS $r1, byte: RAM[SP + byte] = $r1
  { 0x61, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, PGM|AO|RI|PGM|PCE, RST }, // STS A, byte
  { 0x62, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, PGM|BO|RI|PGM|PCE, RST }, // STS B, byte
  { 0x63, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, PGM|CO|RI|PGM|PCE, RST }, // STS A, byte

  // CMP $r1, $r1: $r1 - $r2 and set flags
  { 0x64, BO|TI, AO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0, 0 }, // CMP A, B
  { 0x65, CO|TI, AO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0, 0 }, // CMP A, C
  { 0x66, AO|TI, BO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0, 0 }, // CMP B, A
  { 0x67, CO|TI, BO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0, 0 }, // CMP B, C
  { 0x68, AO|TI, CO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0, 0 }, // CMP C, A
  { 0x69, BO|TI, CO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0, 0 }, // CMP C, B

  // CMPI byte/$r1, $r1/byte: byte/$r1 - $r1/byte and set flags
  { 0x6a, PCO|MI, RO|TI|PCE, AO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0 }, // CMPI A, byte
  { 0x6b, PCO|MI, AO|TI|PCE, RO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0 }, // CMPI byte, A
  { 0x6c, PCO|MI, RO|TI|PCE, BO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0 }, // CMPI B, byte
  { 0x6d, PCO|MI, BO|TI|PCE, RO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0 }, // CMPI byte, B
  { 0x6e, PCO|MI, RO|TI|PCE, CO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0 }, // CMPI C, byte
  { 0x6f, PCO|MI, CO|TI|PCE, RO|TS|SUM|SUB|CAR|EI|FI, RST, 0, 0 }, // CMPI byte, C

  // JMP byte: PC = byte
  { 0x70, PCO|MI, RO|PCI, RST, 0, 0, 0 },

  // CALL byte: SP +=1, PC + 1, Stack[SP] = PC, PC = byte
  { 0x77, PCO|MI, RO|TI, SO|SUM|CAR|EI, EO|SI|MI|PCE, PGM|PCO|RI, TO|PCI },
  
  // RET: PC = Stack[SP], SP -= 1
  { 0x78, SO|MI, PGM|RO|PCI, SO|SUM|SUB|EI, EO|SI, RST, 0 },

  // OUT $r1: Display = $r1
  { 0x79, AO|OI, RST, 0, 0, 0, 0 }, // OUT A
  { 0x7a, BO|OI, RST, 0, 0, 0, 0 }, // OUT B
  { 0x7b, CO|OI, RST, 0, 0, 0, 0 }, // OUT C

  // DIC byte: LCD command byte
  { 0x7c, PCO|MI, RO|LCE|PCE, RST, 0, 0, 0 }, // DIC byte
  
  // DID byte: LCD data byte
  { 0x7d, PCO|MI, RO|LCE|LCS|PCE, RST, 0, 0, 0 }, // DID byte
  
  // HLT: Halt
  { 0x7e, HLT, HLT, HLT, HLT, HLT, HLT }, // HLT
  
  // NOP: No operation
  { 0x7f, 0, 0, 0, 0, 0, 0 },
};

//  0   0   0
//  ZF  SF  OF
const uint32_t PROGMEM conditional_microcode[NUM_CONDITIONAL_INSTRUCTIONS][FLAG_CONFIGURATIONS][INSTRUCTION_ARR_LENGTH] = {
  // JE/JZ byte: ZF ? PC = byte
  {
    { 0x71, PCE,    RST,    0,   0, 0, 0 }, // 000
    { 0x71, PCE,    RST,    0,   0, 0, 0 }, // 001
    { 0x71, PCE,    RST,    0,   0, 0, 0 }, // 010
    { 0x71, PCE,    RST,    0,   0, 0, 0 }, // 011
    { 0x71, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 100
    { 0x71, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 101
    { 0x71, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 110
    { 0x71, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 111
  },
  
  // JNE/JNZ byte: ~ZF ? PC = byte
  {
    { 0x72, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 000
    { 0x72, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 001
    { 0x72, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 010
    { 0x72, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 011
    { 0x72, PCE,    RST,    0,   0, 0, 0 }, // 100
    { 0x72, PCE,    RST,    0,   0, 0, 0 }, // 101
    { 0x72, PCE,    RST,    0,   0, 0, 0 }, // 110
    { 0x72, PCE,    RST,    0,   0, 0, 0 }, // 111
  },

  // JG/JNLE byte: ~(SF ^ OF) & ~ZF ? PC = byte
  {
    { 0x73, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 000
    { 0x73, PCE,    RST,    0,   0, 0, 0 }, // 001
    { 0x73, PCE,    RST,    0,   0, 0, 0 }, // 010
    { 0x73, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 011
    { 0x73, PCE,    RST,    0,   0, 0, 0 }, // 100
    { 0x73, PCE,    RST,    0,   0, 0, 0 }, // 101
    { 0x73, PCE,    RST,    0,   0, 0, 0 }, // 110
    { 0x73, PCE,    RST,    0,   0, 0, 0 }, // 111
  },

  // JGE/JNL byte: ~(SF ^ OF) ? PC = byte
  {
    { 0x74, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 000
    { 0x74, PCE,    RST,    0,   0, 0, 0 }, // 001
    { 0x74, PCE,    RST,    0,   0, 0, 0 }, // 010
    { 0x74, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 011
    { 0x74, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 100
    { 0x74, PCE,    RST,    0,   0, 0, 0 }, // 101
    { 0x74, PCE,    RST,    0,   0, 0, 0 }, // 110
    { 0x74, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 111
  },

  // JL/JNGE byte: SF ^ OF ? PC = byte
  {
    { 0x75, PCE,    RST,    0,   0, 0, 0 }, // 000
    { 0x75, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 001
    { 0x75, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 010
    { 0x75, PCE,    RST,    0,   0, 0, 0 }, // 011
    { 0x75, PCE,    RST,    0,   0, 0, 0 }, // 100
    { 0x75, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 101
    { 0x75, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 110
    { 0x75, PCE,    RST,    0,   0, 0, 0 }, // 111
  },

  // JLE/JNG byte: (SF ^ OF) | ZF ? PC = byte
  {
    { 0x76, PCE,    RST,    0,   0, 0, 0 }, // 000
    { 0x76, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 001
    { 0x76, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 010
    { 0x76, PCE,    RST,    0,   0, 0, 0 }, // 011
    { 0x76, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 100
    { 0x76, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 101
    { 0x76, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 110
    { 0x76, PCO|MI, RO|PCI, RST, 0, 0, 0 }, // 111
  },
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
      for (int flags = 0; flags < FLAG_CONFIGURATIONS; flags++) {
        programmer.write_byte(
          pgm_read_dword_near(&microcode[instruction_index][0]) + (128 * step_index) + (1024 * flags), 
          reverse_bits(
            pgm_read_dword_near(step_index < FETCH_STEPS ? &fetch_cycle[step_index]: &microcode[instruction_index][step_index - FETCH_STEPS + 1]) >> (32 - 8*num)
          )
        );
      }
    }
  }

  // write all the conditional instructions
  for (int instruction_index = 0; instruction_index < NUM_CONDITIONAL_INSTRUCTIONS; instruction_index++) {
    for (int step_index = 0; step_index < INSTRUCTION_STEPS; step_index++) {
      for (int flags = 0; flags < FLAG_CONFIGURATIONS; flags++) {
        programmer.write_byte(
          pgm_read_dword_near(&conditional_microcode[instruction_index][0][0]) + (128 * step_index) + (1024 * flags), 
          reverse_bits(
            pgm_read_dword_near(step_index < FETCH_STEPS ? &fetch_cycle[step_index]: &conditional_microcode[instruction_index][flags][step_index - FETCH_STEPS + 1]) >> (32 - 8*num)
          )
        );
      }
    }
  }
}


void setup() {
  Serial.begin(57600);
  Serial.println("Beginning!");
  program_EEPROM(4);
  programmer.print_contents();
}

void loop() {}
