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
#define ZO  0b00000000000000000000000000000000
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
#define STK 0b00000000000000000000000000001000  

const int INSTRUCTION_STEPS = 16;
const int FETCH_STEPS = 2;

const int NUM_FLAGS = 4;
const int FLAG_CONFIGURATIONS = pow(2, NUM_FLAGS);

const int NUM_INSTRUCTIONS = 162;
const int NUM_CONDITIONAL_INSTRUCTIONS = 39;

const int INSTRUCTION_ARR_LENGTH = 1 + (INSTRUCTION_STEPS - FETCH_STEPS);

const uint32_t PROGMEM fetch_cycle[2] = { PCO|MI, RO|II|PCE };

const uint32_t PROGMEM microcode[NUM_INSTRUCTIONS][INSTRUCTION_ARR_LENGTH] = {
  // add $r1, $r2: $r2 = $r2 + $r1
  { 0x00, AO|TI, AO|TS|SUM|EI|FI, EO|AI, RST }, // add a, a
  { 0x01, AO|TI, BO|TS|SUM|EI|FI, EO|BI, RST }, // add a, b
  { 0x02, AO|TI, CO|TS|SUM|EI|FI, EO|CI, RST }, // add a, c
  { 0x03, AO|TI, SO|TS|SUM|EI|FI, EO|SI, RST }, // add a, sp
  { 0x04, BO|TI, AO|TS|SUM|EI|FI, EO|AI, RST }, // add b, a
  { 0x05, BO|TI, BO|TS|SUM|EI|FI, EO|BI, RST }, // add b, b
  { 0x06, BO|TI, CO|TS|SUM|EI|FI, EO|CI, RST }, // add b, c
  { 0x07, BO|TI, SO|TS|SUM|EI|FI, EO|SI, RST }, // add b, sp
  { 0x08, CO|TI, AO|TS|SUM|EI|FI, EO|AI, RST }, // add c, a
  { 0x09, CO|TI, BO|TS|SUM|EI|FI, EO|BI, RST }, // add c, b
  { 0x0a, CO|TI, CO|TS|SUM|EI|FI, EO|CI, RST }, // add c, c
  { 0x0b, CO|TI, SO|TS|SUM|EI|FI, EO|SI, RST }, // add c, sp
  
  // addi byte, $r1: $r1 = $r1 + byte
  { 0x0c, PCO|MI, RO|TI, AO|TS|SUM|EI|FI, EO|AI|PCE, RST }, // ADDI byte, A
  { 0x0d, PCO|MI, RO|TI, BO|TS|SUM|EI|FI, EO|BI|PCE, RST }, // ADDI byte, B
  { 0x0e, PCO|MI, RO|TI, CO|TS|SUM|EI|FI, EO|CI|PCE, RST }, // ADDI byte, C
  { 0x0f, PCO|MI, RO|TI, SO|TS|SUM|EI|FI, EO|SI|PCE, RST }, // ADDI byte, A

  // sub $r1, $r2: $r2 = $r2 - $r1
  { 0x20, BO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI, RST }, // sub b, a
  { 0x21, CO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI, RST }, // sub c, a
  { 0x22, AO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI, RST }, // sub a, b
  { 0x23, CO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI, RST }, // sub c, b
  { 0x24, AO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI, RST }, // sub a, c
  { 0x25, BO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI, RST }, // sub b, c
  { 0x26, AO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST }, // sub a, sp
  { 0x27, BO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST }, // sub b, sp
  { 0x28, CO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST }, // sub c, sp

  // subi byte, $r1: $r1 = $r1 - byte
  { 0x29, PCO|MI, RO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI|PCE, RST }, // sub byte, a
  { 0x2a, PCO|MI, RO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI|PCE, RST }, // sub byte, b
  { 0x2b, PCO|MI, RO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI|PCE, RST }, // sub byte, c
  { 0x2c, PCO|MI, RO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI|PCE, RST }, // sub byte, sp

  // and $r1, $r2: $r2 = $r2 & $r1
  { 0x3a, BO|TI, AO|TS|AND|EI|FI, EO|AI, RST }, // and b, a
  { 0x3b, CO|TI, AO|TS|AND|EI|FI, EO|AI, RST }, // and c, a
  { 0x3c, AO|TI, BO|TS|AND|EI|FI, EO|BI, RST }, // and a, b
  { 0x3d, CO|TI, BO|TS|AND|EI|FI, EO|BI, RST }, // and c, b
  { 0x3e, AO|TI, CO|TS|AND|EI|FI, EO|CI, RST }, // and a, c
  { 0x3f, BO|TI, CO|TS|AND|EI|FI, EO|CI, RST }, // and b, c
  
  // ani byte, $r1: $r1 = $r1 & byte
  { 0x40, PCO|MI, RO|TI, AO|TS|AND|EI|FI, EO|AI|PCE, RST }, // ani byte, a
  { 0x41, PCO|MI, RO|TI, BO|TS|AND|EI|FI, EO|BI|PCE, RST }, // ani byte, b
  { 0x42, PCO|MI, RO|TI, CO|TS|AND|EI|FI, EO|CI|PCE, RST }, // ani byte, c

  // or $r1, $r2: $r2 = $r2 | $r1
  { 0x43, BO|TI, AO|TS|OR|EI|FI, EO|AI, RST }, // or b, a
  { 0x44, CO|TI, AO|TS|OR|EI|FI, EO|AI, RST }, // or c, a
  { 0x45, AO|TI, BO|TS|OR|EI|FI, EO|BI, RST }, // or a, b
  { 0x46, CO|TI, BO|TS|OR|EI|FI, EO|BI, RST }, // or c, b
  { 0x47, AO|TI, CO|TS|OR|EI|FI, EO|CI, RST }, // or a, c
  { 0x48, BO|TI, CO|TS|OR|EI|FI, EO|CI, RST }, // or b, c
  
  // ori byte, $r1: $r1 = $r1 | byte
  { 0x49, PCO|MI, RO|TI, AO|TS|OR|EI|FI, EO|AI|PCE, RST }, // ori byte, a
  { 0x4a, PCO|MI, RO|TI, BO|TS|OR|EI|FI, EO|BI|PCE, RST }, // ori byte, b
  { 0x4b, PCO|MI, RO|TI, CO|TS|OR|EI|FI, EO|CI|PCE, RST }, // ori byte, c

  // xor $r1, $r2: $r2 = $r2 ^ $r1
  { 0x4c, BO|TI, AO|TS|XOR|EI|FI, EO|AI, RST }, // xor b, a
  { 0x4d, CO|TI, AO|TS|XOR|EI|FI, EO|AI, RST }, // xor c, a
  { 0x4e, AO|TI, BO|TS|XOR|EI|FI, EO|BI, RST }, // xor a, b
  { 0x4f, CO|TI, BO|TS|XOR|EI|FI, EO|BI, RST }, // xor c, b
  { 0x50, AO|TI, CO|TS|XOR|EI|FI, EO|CI, RST }, // xor a, c
  { 0x51, BO|TI, CO|TS|XOR|EI|FI, EO|CI, RST }, // xor b, c
  
  // xri byte, $r1: $r1 = $r1 ^ byte
  { 0x52, PCO|MI, RO|TI, AO|TS|XOR|EI|FI, EO|AI|PCE, RST }, // xri byte, a
  { 0x53, PCO|MI, RO|TI, BO|TS|XOR|EI|FI, EO|BI|PCE, RST }, // xri byte, b
  { 0x54, PCO|MI, RO|TI, CO|TS|XOR|EI|FI, EO|CI|PCE, RST }, // xri byte, c

  // not $r1: $r1 = ~$r1
  { 0x55, AO|TI, TS|NOT|SUB|EI|FI, EO|AI, RST }, // not a
  { 0x56, BO|TI, TS|NOT|SUB|EI|FI, EO|BI, RST }, // not b
  { 0x57, CO|TI, TS|NOT|SUB|EI|FI, EO|CI, RST }, // not c

  // neg $r1: $r1 = ~r1 + 1
  { 0x58, AO|TI, ZO|TS|SUM|SUB|CAR|EI|FI, EO|AI, RST }, // neg a
  { 0x59, BO|TI, ZO|TS|SUM|SUB|CAR|EI|FI, EO|BI, RST }, // neg a
  { 0x5a, CO|TI, ZO|TS|SUM|SUB|CAR|EI|FI, EO|CI, RST }, // neg a

  // inr $r1: $r1 = $r1 + 1
  { 0x5b, AO|SUM|CAR|EI|FI, EO|AI, RST }, // inr a
  { 0x5c, BO|SUM|CAR|EI|FI, EO|BI, RST }, // inr b
  { 0x5d, CO|SUM|CAR|EI|FI, EO|CI, RST }, // inr c
  { 0x5e, SO|SUM|CAR|EI|FI, EO|SI, RST }, // inr sp

  // inr2 $r1: $r1 = $r1 + 2
  { 0x5f, AO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|AI, RST }, // inr2 a
  { 0x60, BO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|BI, RST }, // inr2 b
  { 0x61, CO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|CI, RST }, // inr2 c
  { 0x62, SO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|SI, RST }, // inr2 sp

  // inr3 $r1: $r1 = $r1 + 3
  { 0x63, AO|SUM|CAR|EI, EO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|AI, RST }, // inr2 a
  { 0x64, BO|SUM|CAR|EI, EO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|BI, RST }, // inr2 b
  { 0x65, CO|SUM|CAR|EI, EO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|CI, RST }, // inr2 c
  { 0x66, SO|SUM|CAR|EI, EO|SUM|CAR|EI, EO|SUM|CAR|EI|FI, EO|SI, RST }, // inr2 sp

  // dcr $r1: $r1 = $r1 - 1
  { 0x67, AO|SUM|SUB|EI|FI, EO|AI, RST }, // dcr a
  { 0x68, BO|SUM|SUB|EI|FI, EO|BI, RST }, // dcr b
  { 0x69, CO|SUM|SUB|EI|FI, EO|CI, RST }, // dcr c
  { 0x6a, SO|SUM|SUB|EI|FI, EO|SI, RST }, // dcr sp

  // dcr2 $r1: $r1 = $r1 - 2
  { 0x6b, AO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|AI, RST }, // dcr2 a
  { 0x6c, BO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|BI, RST }, // dcr2 b
  { 0x6d, CO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|CI, RST }, // dcr2 c
  { 0x6e, SO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|SI, RST }, // dcr2 sp

  // dcr3 $r1: $r1 = $r1 - 3
  { 0x6f, AO|SUM|SUB|EI, EO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|AI, RST }, // dcr3 a
  { 0x70, BO|SUM|SUB|EI, EO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|BI, RST }, // dcr3 b
  { 0x71, CO|SUM|SUB|EI, EO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|CI, RST }, // dcr3 c
  { 0x72, SO|SUM|SUB|EI, EO|SUM|SUB|EI, EO|SUM|SUB|EI|FI, EO|SI, RST }, // dcr3 sp

  // mov $r1, $r2: $r2 = $r1
  { 0x73, AO|BI, RST }, // mov a, b
  { 0x74, AO|CI, RST }, // mov a, c
  { 0x75, BO|AI, RST }, // mov b, a
  { 0x76, BO|CI, RST }, // mov b, c
  { 0x77, CO|AI, RST }, // mov c, a
  { 0x78, CO|BI, RST }, // mov c, b
  { 0x79, ZO|AI, RST }, // mov z, a
  { 0x7a, ZO|BI, RST }, // mov z, b
  { 0x7b, ZO|CI, RST }, // mov z, c
  { 0x7c, SO|AI, RST }, // mov sp, a
  { 0x7d, SO|BI, RST }, // mov sp, b
  { 0x7e, SO|BI, RST }, // mov sp, c

  // mvi byte, $r1: $r1 = byte
  { 0x7f, PCO|MI, RO|AI|PCE, RST }, // mvi byte, a
  { 0x80, PCO|MI, RO|BI|PCE, RST }, // mvi byte, b
  { 0x81, PCO|MI, RO|CI|PCE, RST }, // mvi byte, c

  // ld $r1, $r2: $r2 = RAM[$r1]
  { 0x82, AO|MI, STK|RO|AI, RST }, // ld a, a
  { 0x83, BO|MI, STK|RO|AI, RST }, // ld b, a
  { 0x84, CO|MI, STK|RO|AI, RST }, // ld c, a
  { 0x85, AO|MI, STK|RO|BI, RST }, // ld a, b
  { 0x86, BO|MI, STK|RO|BI, RST }, // ld b, b
  { 0x87, CO|MI, STK|RO|BI, RST }, // ld c, b
  { 0x88, AO|MI, STK|RO|CI, RST }, // ld a, c
  { 0x89, BO|MI, STK|RO|CI, RST }, // ld b, c
  { 0x8a, CO|MI, STK|RO|CI, RST }, // ld c, c

  // st $r1, $r2: RAM[$r2] = $r1
  { 0x8b, AO|MI, STK|AO|RI, RST }, // st a, a
  { 0x8c, BO|MI, STK|AO|RI, RST }, // st a, b
  { 0x8d, CO|MI, STK|AO|RI, RST }, // st a, c
  { 0x8e, AO|MI, STK|BO|RI, RST }, // st b, a
  { 0x8f, BO|MI, STK|BO|RI, RST }, // st b, b
  { 0x90, CO|MI, STK|BO|RI, RST }, // st b, c
  { 0x91, AO|MI, STK|CO|RI, RST }, // st c, a
  { 0x92, BO|MI, STK|CO|RI, RST }, // st c, b
  { 0x93, CO|MI, STK|CO|RI, RST }, // st c, c
  { 0x94, ZO|MI, STK|CO|RI, RST }, // st z, a
  { 0x95, ZO|MI, STK|CO|RI, RST }, // st z, b
  { 0x96, ZO|MI, STK|CO|RI, RST }, // st z, c

  // lds byte, $r1: $r1 = RAM[sp + byte]
  { 0x97, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, STK|RO|AI|PCE, RST }, // lds byte, a
  { 0x98, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, STK|RO|BI|PCE, RST }, // lds byte, b
  { 0x99, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, STK|RO|CI|PCE, RST }, // lds byte, c

  // sts $r1, byte: RAM[sp + byte] = $r1
  { 0x9a, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, STK|AO|RI|PCE, RST }, // sts a, byte
  { 0x9b, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, STK|BO|RI|PCE, RST }, // sts b, byte
  { 0x9c, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, STK|CO|RI|PCE, RST }, // sts c, byte
  { 0x9d, PCO|MI, SO|TI, RO|TS|SUM|EI, EO|MI, STK|ZO|RI|PCE, RST }, // sts z, byte
  
  // stsi byte, byte2: RAM [SP + byte2] = byte
  { 0x9e, PCO|MI, RO|TI|PCE, ZO|MI, TO|RI|STK, PCO|MI, SO|TI, RO|TS|SUM|EI, ZO|MI, STK|RO|TI, EO|MI, STK|TO|RI|PCE },

  // cmp $r1, $r2: $r1 - $r2 and set flags
  { 0x9f, BO|TI, AO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp a, b
  { 0xa0, CO|TI, AO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp a, c
  { 0xa1, ZO|TI, AO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp a, z
  { 0xa2, AO|TI, BO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp b, a
  { 0xa3, CO|TI, BO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp b, c
  { 0xa4, ZO|TI, BO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp b, z
  { 0xa5, AO|TI, CO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp c, a
  { 0xa6, BO|TI, CO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp c, b
  { 0xa7, ZO|TI, CO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp c, z
  { 0xa8, AO|TI, ZO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp z, a
  { 0xa9, BO|TI, ZO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp z, b
  { 0xaa, CO|TI, ZO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmp z, c

  // cmpi byte/$r1, $r1/byte: byte/$r1 - $r1/byte and set flags
  { 0xab, PCO|MI, RO|TI|PCE, AO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmpi a, byte
  { 0xac, PCO|MI, AO|TI|PCE, RO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmpi byte, a
  { 0xad, PCO|MI, RO|TI|PCE, BO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmpi b, byte
  { 0xae, PCO|MI, BO|TI|PCE, RO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmpi byte, b
  { 0xaf, PCO|MI, RO|TI|PCE, CO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmpi c, byte
  { 0xb0, PCO|MI, CO|TI|PCE, RO|TS|SUM|SUB|CAR|EI|FI, RST }, // cmpi byte, c

  // jmp byte: pc = byte
  { 0xb1, PCO|MI, RO|PCI, RST },

  // call byte: sp +=1, sp + 1, Stack[sp] = pc, pc = byte
  { 0xbc, PCO|MI, RO|TI, SO|SUM|CAR|EI, EO|SI|MI|PCE|STK, STK|PCO|RI, STK|TO|PCI },
  
  // ret: pc = Stack[sp], sp -= 1
  { 0xbd, SO|MI, STK|RO|PCI, SO|SUM|SUB|EI, EO|SI, RST },

  // out $r1: Decimal Display = $r1
  { 0xbe, AO|OI, RST }, // OUT A
  { 0xbf, BO|OI, RST }, // OUT B
  { 0xc0, CO|OI, RST }, // OUT C

  // outi byte: Decimal Display = byte
  { 0xc1, PCO|MI, RO|OI|PCE, RST },

  // dic byte: LCD command byte
  { 0xc2, PCO|MI, RO|LCE|PCE, RST }, // dic byte
  
  // did byte: LCD data byte
  { 0xc3, PCO|MI, RO|LCE|LCS|PCE, RST }, // did byte

  // dd $r1: Send $r1 to LCD as data
  { 0xc4, AO|LCE|LCS, RST }, // dd a
  { 0xc5, BO|LCE|LCS, RST }, // dd b
  { 0xc6, CO|LCE|LCS, RST }, // dd c
  
  // HLT: Halt
  { 0xc7, HLT, HLT, HLT, HLT, HLT, HLT, HLT, HLT, HLT, HLT, HLT, HLT, HLT, HLT }, // HLT
  
  // NOP: No operation
  { 0xc8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
};

const uint32_t PROGMEM conditional_microcode[NUM_CONDITIONAL_INSTRUCTIONS][2][INSTRUCTION_ARR_LENGTH] = {
  // addc a, a
  {
    { 0x10, AO|TI, AO|TS|SUM|EI|FI,     EO|AI, RST },
    { 0x10, AO|TI, AO|TS|SUM|CAR|EI|FI, EO|AI, RST },
  },
  // addc a, b
  {
    { 0x11, AO|TI, BO|TS|SUM|EI|FI,     EO|BI, RST },
    { 0x11, AO|TI, BO|TS|SUM|CAR|EI|FI, EO|BI, RST },
  },
  // addc a, c
  {
    { 0x12, AO|TI, CO|TS|SUM|EI|FI,     EO|CI, RST },
    { 0x12, AO|TI, CO|TS|SUM|CAR|EI|FI, EO|CI, RST },
  },
  // addc a, sp
  {
    { 0x13, AO|TI, SO|TS|SUM|EI|FI,     EO|SI, RST },
    { 0x13, AO|TI, SO|TS|SUM|CAR|EI|FI, EO|SI, RST },
  },
  // addc b, a
  {
    { 0x14, BO|TI, AO|TS|SUM|EI|FI,     EO|AI, RST },
    { 0x14, BO|TI, AO|TS|SUM|CAR|EI|FI, EO|AI, RST },
  },
  // addc b, b
  {
    { 0x15, BO|TI, BO|TS|SUM|EI|FI,     EO|BI, RST },
    { 0x15, BO|TI, BO|TS|SUM|CAR|EI|FI, EO|BI, RST },
  },
  // addc b, c
  {
    { 0x16, BO|TI, CO|TS|SUM|EI|FI,     EO|CI, RST },
    { 0x16, BO|TI, CO|TS|SUM|CAR|EI|FI, EO|CI, RST },
  },
  // addc b, sp
  {
    { 0x17, BO|TI, SO|TS|SUM|EI|FI,     EO|SI, RST },
    { 0x17, BO|TI, SO|TS|SUM|CAR|EI|FI, EO|SI, RST },
  },
  // addc c, a
  {
    { 0x18, CO|TI, AO|TS|SUM|EI|FI,     EO|AI, RST },
    { 0x18, CO|TI, AO|TS|SUM|CAR|EI|FI, EO|AI, RST },
  },
  // addc c, b
  {
    { 0x19, CO|TI, BO|TS|SUM|EI|FI,     EO|BI, RST },
    { 0x19, CO|TI, BO|TS|SUM|CAR|EI|FI, EO|BI, RST },
  },
  // addc c, c
  {
    { 0x1a, CO|TI, CO|TS|SUM|EI|FI,     EO|CI, RST },
    { 0x1a, CO|TI, CO|TS|SUM|CAR|EI|FI, EO|CI, RST },
  },
  // addc c, sp
  {
    { 0x1b, CO|TI, SO|TS|SUM|EI|FI,     EO|SI, RST },
    { 0x1b, CO|TI, SO|TS|SUM|CAR|EI|FI, EO|SI, RST },
  },
  
  // addci byte, a
  {
    { 0x1c, PCO|MI, RO|TI, AO|TS|SUM|EI|FI,     EO|AI|PCE, RST },
    { 0x1c, PCO|MI, RO|TI, AO|TS|SUM|CAR|EI|FI, EO|AI|PCE, RST },
  },
  // addci byte, b
  {
    { 0x1d, PCO|MI, RO|TI, BO|TS|SUM|EI|FI,     EO|BI|PCE, RST },
    { 0x1d, PCO|MI, RO|TI, BO|TS|SUM|CAR|EI|FI, EO|BI|PCE, RST },
  },
  // addci byte, c
  {
    { 0x1e, PCO|MI, RO|TI, CO|TS|SUM|EI|FI,     EO|CI|PCE, RST },
    { 0x1e, PCO|MI, RO|TI, CO|TS|SUM|CAR|EI|FI, EO|CI|PCE, RST },
  },
  // addci byte, sp
  {
    { 0x1f, PCO|MI, RO|TI, SO|TS|SUM|EI|FI,     SO|BI|PCE, RST },
    { 0x1f, PCO|MI, RO|TI, SO|TS|SUM|CAR|EI|FI, SO|BI|PCE, RST },
  },

  // subb b, a
  {
    { 0x2d, BO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI, RST },
    { 0x2d, BO|TI, AO|TS|SUM|SUB|EI|FI,     EO|AI, RST },
  },
  // subb c, a
  {
    { 0x2e, CO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI, RST },
    { 0x2e, CO|TI, AO|TS|SUM|SUB|EI|FI,     EO|AI, RST },
  },
  // subb a, b
  {
    { 0x2f, AO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI, RST },
    { 0x2f, AO|TI, BO|TS|SUM|SUB|EI|FI,     EO|BI, RST },
  },
  // subb c, b
  {
    { 0x30, CO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI, RST },
    { 0x30, CO|TI, BO|TS|SUM|SUB|EI|FI,     EO|BI, RST },
  },
  // subb a, c
  {
    { 0x31, AO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI, RST },
    { 0x31, AO|TI, CO|TS|SUM|SUB|EI|FI,     EO|CI, RST },
  },
  // subb b, c
  {
    { 0x32, BO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI, RST },
    { 0x32, BO|TI, CO|TS|SUM|SUB|EI|FI,     EO|CI, RST },
  },
  // subb a, sp
  {
    { 0x33, AO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST },
    { 0x33, AO|TI, SO|TS|SUM|SUB|EI|FI,     EO|SI, RST },
  },
  // subb b, sp
  {
    { 0x34, BO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST },
    { 0x34, BO|TI, SO|TS|SUM|SUB|EI|FI,     EO|SI, RST },
  },
  // subb c, sp
  {
    { 0x35, CO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI, RST },
    { 0x35, CO|TI, SO|TS|SUM|SUB|EI|FI,     EO|SI, RST },
  },

  // subbi byte, a
  {
    { 0x36, PCO|MI, RO|TI, AO|TS|SUM|SUB|CAR|EI|FI, EO|AI|PCE, RST },
    { 0x36, PCO|MI, RO|TI, AO|TS|SUM|SUB|EI|FI,     EO|AI|PCE, RST },
  },
  // subbi byte, b
  {
    { 0x37, PCO|MI, RO|TI, BO|TS|SUM|SUB|CAR|EI|FI, EO|BI|PCE, RST },
    { 0x37, PCO|MI, RO|TI, BO|TS|SUM|SUB|EI|FI,     EO|BI|PCE, RST },
  },
  // subbi byte, c
  {
    { 0x38, PCO|MI, RO|TI, CO|TS|SUM|SUB|CAR|EI|FI, EO|CI|PCE, RST },
    { 0x38, PCO|MI, RO|TI, CO|TS|SUM|SUB|EI|FI,     EO|CI|PCE, RST },
  },
  // subbi byte, sp
  {
    { 0x39, PCO|MI, RO|TI, SO|TS|SUM|SUB|CAR|EI|FI, EO|SI|PCE, RST },
    { 0x39, PCO|MI, RO|TI, SO|TS|SUM|SUB|EI|FI,     EO|SI|PCE, RST },
  },

  // je byte: if ZF, PC = byte
  {
    { 0xb2, PCE, RST },
    { 0xb2, PCO|MI, RO|PCI, RST }
  },

  // jne byte: if ~ZF, PC = byte
  {
    { 0xb3, PCE, RST },
    { 0xb3, PCO|MI, RO|PCI, RST }
  },

  // jg byte: if ~(SF ^ OF) & ~ZF, PC = byte
  {
    { 0xb4, PCE, RST },
    { 0xb4, PCO|MI, RO|PCI, RST }
  },

  // jge byte: if ~(SF ^ OF), PC = byte
  {
    { 0xb5, PCE, RST },
    { 0xb5, PCO|MI, RO|PCI, RST }
  },

  // jl byte: if (SF ^ OF), PC = byte
  {
    { 0xb6, PCE, RST },
    { 0xb6, PCO|MI, RO|PCI, RST }
  },

  // jle byte: if (SF ^ OF) | ZF, PC = byte
  {
    { 0xb7, PCE, RST },
    { 0xb7, PCO|MI, RO|PCI, RST }
  },

  // ja byte: if (~CF & ~ZF), PC = byte
  {
    { 0xb8, PCE, RST },
    { 0xb8, PCO|MI, RO|PCI, RST }
  },

  // jae byte: if (~CF), PC = byte
  {
    { 0xb9, PCE, RST },
    { 0xb9, PCO|MI, RO|PCI, RST }
  },

  // jb byte: if (CF), PC = byte
  {
    { 0xba, PCE, RST },
    { 0xba, PCO|MI, RO|PCI, RST }
  },

  // jbe byte: if (CF), PC = byte
  {
    { 0xbb, PCE, RST },
    { 0xbb, PCO|MI, RO|PCI, RST }
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
        programmer.write_byte(
          pgm_read_dword_near(&microcode[instruction_index][0]) + (256 * step_index), 
          reverse_bits(
            pgm_read_dword_near(step_index < FETCH_STEPS ? &fetch_cycle[step_index]: &microcode[instruction_index][step_index - FETCH_STEPS + 1]) >> (32 - 8*num)
          )
        );
      }
    }

  // write all the conditional instructions
  for (int instruction_index = 0; instruction_index < NUM_CONDITIONAL_INSTRUCTIONS; instruction_index++) {
    for (int step_index = 0; step_index < INSTRUCTION_STEPS; step_index++) {
      for (int condition = 0; condition < 2; condition++) {
        programmer.write_byte(
          pgm_read_dword_near(&conditional_microcode[instruction_index][0][0]) + (256 * step_index) + (4096 * condition), 
          reverse_bits(
            pgm_read_dword_near(step_index < FETCH_STEPS ? &fetch_cycle[step_index]: &conditional_microcode[instruction_index][condition][step_index - FETCH_STEPS + 1]) >> (32 - 8*num)
          )
        );
      }
    }
  }
}


void setup() {
  Serial.begin(57600);
  Serial.println("Beginning!");
  programmer.erase(); 
  Serial.println("Finished erasing");
  program_EEPROM(4);
  programmer.print_contents();
}

void loop() {}
