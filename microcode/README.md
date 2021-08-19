# microcode

Below is documentation for the 3000's microinstructions, as well as the full instruction set currently supported on the 3000.

## Microinstructions 

| Instruction | Description |
| ------------- | ------------- |
| `HLT` | Halt (when active this line stops the clock) |
| `RST` | Reset (reset the step counter of the instruction) |
| `PCE` | Program Counter Enable (program counter will increment on next clock pulse) |
| `PCO` | Program Counter Out |
| `PCI` | Program Counter In |
| `MI` | Memory Address Register In |
| `RO` | RAM Out |
| `RI` | RAM In (write to RAM) |
| `IO` | Instruction Register Out (never used) |
| `II` | Instruction Register In |
| `AO` | A-register Out |
| `AI` | A-register In |
| `BO` | B-register Out |
| `BI` | B-register In |
| `CO` | C-register Out |
| `CI` | C-register In |
| `TO` | Temp-register Out |
| `TI` | Temp-register In |
| `OI` | Output-register In (Decimal display) |
| `LCS` | LCD Select (Data vs. Command) |
| `LCE` | LCD Enable (Actively listening to bus) |
| `EO` | ALU Out |
| `EI` | ALU In |
| `FI` | Flags-register In |
| `NOT` | NOT ALU Operation |
| `AND` | AND ALU Operation |
| `XOR` | XOR ALU Operation |
| `OR` | OR ALU Operation |
| `SUM` | ADD ALU Operation |
| `TS` | Select Temp-register as other operand or all 0s |
| `CAR` | Set Carry/Borrow for addition and subtraction |
| `SUB` | SUB ALU operation (add but with 1s compliment (2s compliment comes from setting CAR line)) |
| `STK` | Accessing stack or instruction memory |
| `SO` | Stack pointer out |
| `SI` | Stack pointer in |


## Instruction Set

Note: instructions listed with opcode N/A describe the behavior of a whole class of instructions, which have specific opcodes that correspond to the use of specific registers.

TODO: (stewart) implement the new instructions. Then we can unbold them here and add the opcodes in.

| Instruction | Opcode | Description |
| ------------- | ------------- | ------------- |
| `add $r1, $r2` | N/A | $r2 = $r2 + $r1 |
|  `add a, a` | `0x00` |
|  `add a, b` | `0x01` |
|  `add a, c` | `0x02` |
|  `add a, sp` | `0x03` |
|  `add b, a` | `0x04` |
|  `add b, b` | `0x05` |
|  `add b, c` | `0x06` |
|  `add b, sp` | `0x07` |
|  `add c, a` | `0x08` |
|  `add c, b` | `0x09` |
|  `add c, c` | `0x0a` |
|  `add c, sp` | `0x0b` |
| `addi byte, $r1` | N/A | $r1 = $r1 + byte |
|  `addi byte, a` | `0x0c` |
|  `addi byte, b` | `0x0d` |
|  `addi byte, c` | `0x0e` |
|  `addi byte, sp` | `0x0f` |
| **`addc $r1, $r2`** | N/A | $r2 = $r2 + $r1 + CF |
|  **`addc a, a`** | `0x10` |
|  **`addc a, b`** | `0x11` |
|  **`addc a, c`** | `0x12` |
|  **`addc a, sp`** | `0x13` |
|  **`addc b, a`** | `0x14` |
|  **`addc b, b`** | `0x15` |
|  **`addc b, c`** | `0x16` |
|  **`addc b, sp`** | `0x17` |
|  **`addc c, a`** | `0x18` |
|  **`addc c, b`** | `0x19` |
|  **`addc c, c`** | `0x1a` |
|  **`addc c, sp`** | `0x1b` |
| **`addci byte, $r1`** | N/A | $r1 = $r1 + byte + CF |
|  **`addci byte, a`** | `0x1c` |
|  **`addci byte, b`** | `0x1d` |
|  **`addci byte, c`** | `0x1e` |
|  **`addci byte, sp`** | `0x1f` |
| `sub $r1, $r2` | N/A | $r2 = $r2 - $r1 |
|  `sub b, a` | `0x20` |
|  `sub c, a` | `0x21` |
|  `sub a, b` | `0x22` |
|  `sub c, b` | `0x23` |
|  `sub a, c` | `0x24` |
|  `sub b, c` | `0x25` |
|  `sub a, sp` | `0x26` |
|  `sub b, sp` | `0x27` |
|  `sub c, sp` | `0x28` |
| `subi byte, $r1` | N/A | $r1 = $r1 - byte |
|  `subi byte, a` | `0x29` |
|  `subi byte, b` | `0x2a` |
|  `subi byte, c` | `0x2b` |
|  `subi byte, sp` | `0x2c` |
| **`subb $r1, $r2`** | N/A | $r2 = $r2 - $r1 - CF|
|  **`subb b, a`** | `0x2d` |
|  **`subb c, a`** | `0x2e` |
|  **`subb a, b`** | `0x2f` |
|  **`subb c, b`** | `0x30` |
|  **`subb a, c`** | `0x31` |
|  **`subb b, c`** | `0x32` |
|  **`subb a, sp`** | `0x33` |
|  **`subb b, sp`** | `0x34` |
|  **`subb c, sp`** | `0x35` |
| **`subbi byte, $r1`** | N/A | $r1 = $r1 - byte - CF|
|  **`subbi byte, a`** | `0x36` |
|  **`subbi byte, b`** | `0x37` |
|  **`subbi byte, c`** | `0x38` |
|  **`subbi byte, sp`** | `0x39` |
| `and $r1, $r2` | N/A | $r2 = $r2 & $r1 |
|  `and b, a` | `0x3a` |
|  `and c, a` | `0x3b` |
|  `and a, b` | `0x3c` |
|  `and c, b` | `0x3d` |
|  `and a, c` | `0x3e` |
|  `and b, c` | `0x3f` |
| `ani byte, $r1` | N/A | $r1 = $r1 & byte |
|  `ani byte, a` | `0x40` |
|  `ani byte, b` | `0x41` |
|  `ani byte, c` | `0x42` |
| `or $r1, $r2` | N/A | $r2 = $r2 \| $r1 |
|  `or b, a` | `0x43` |
|  `or c, a` | `0x44` |
|  `or a, b` | `0x45` |
|  `or c, b` | `0x46` |
|  `or a, c` | `0x47` |
|  `or b, c` | `0x48` |
| `ori byte, $r1` | N/A | $r1 = $r1 \| byte |
|  `ori byte, a` | `0x49` |
|  `ori byte, b` | `0x4a` |
|  `ori byte, c` | `0x4b` |
| `xor $r1, $r2` | N/A | $r2 = $r2 ^ $r1 |
|  `xor b, a` | `0x4c` |
|  `xor c, a` | `0x4d` |
|  `xor a, b` | `0x4e` |
|  `xor c, b` | `0x4f` |
|  `xor a, c` | `0x50` |
|  `xor b, c` | `0x51` |
| `xri byte, $r1` | N/A | $r1 = $r1 ^ byte |
|  `xri byte, a` | `0x52` |
|  `xri byte, b` | `0x53` |
|  `xri byte, c` | `0x54` |
| `not $r1` | N/A | $r1 = ~$r1 |
|  `not a` | `0x55` |
|  `not b` | `0x56` |
|  `not c` | `0x57` |
| **`neg $r1`** | N/A | $r1 = ~$r1 + 1 |
| **`neg a`** | `0x58` |
| **`neg b`** | `0x59` |
| **`neg c`** | `0x5a` |
| `inr $r1` | N/A | $r1 = $r1 + 1 |
|  `inr a` | `0x5b` |
|  `inr b` | `0x5c` |
|  `inr c` | `0x5d` |
|  `inr sp` | `0x5e` |
| **`inr2 $r1`** | N/A | $r1 = $r1 + 2 |
|  **`inr2 a`** | `0x5f` |
|  **`inr2 b`** | `0x60` |
|  **`inr2 c`** | `0x61` |
|  **`inr2 sp`** | `0x62` |
| **`inr3 $r1`** | N/A | $r1 = $r1 + 3 |
|  **`inr3 a`** | `0x63` |
|  **`inr3 b`** | `0x64` |
|  **`inr3 c`** | `0x65` |
|  **`inr3 sp`** | `0x66` |
| `dcr $r1` | N/A | $r1 = $r1 - 1 |
|  `dcr a` | `0x67` |
|  `dcr b` | `0x68` |
|  `dcr c` | `0x69` |
|  `dcr sp` | `0x6a` |
| **`dcr2 $r1`** | N/A | $r1 = $r1 - 2 |
|  **`dcr2 a`** | `0x6b` |
|  **`dcr2 b`** | `0x6c` |
|  **`dcr2 c`** | `0x6d` |
|  **`dcr2 sp`** | `0x6e` |
| **`dcr3 $r1`** | N/A | $r1 = $r1 - 3 |
|  **`dcr3 a`** | `0x6f` |
|  **`dcr3 b`** | `0x70` |
|  **`dcr3 c`** | `0x71` |
|  **`dcr3 sp`** | `0x72` |
| `mov $r1, $r2` | N/A | $r2 = $r1 |
|  `mov a, b` | `0x73` |
|  `mov a, c` | `0x74` |
|  `mov b, a` | `0x75` |
|  `mov b, c` | `0x76` |
|  `mov c, a` | `0x77` |
|  `mov c, b` | `0x78` |
|  **`mov z, a`** | `0x79` |
|  **`mov z, b`** | `0x7a` |
|  **`mov z, c`** | `0x7b` |
|  **`mov sp, a`** | `0x7c` |
|  **`mov sp, b`** | `0x7d` |
|  **`mov sp, c`** | `0x7e` |
| `mvi byte, $r1` | N/A | $r1 = byte |
|  `mvi byte, a` | `0x7f` |
|  `mvi byte, b` | `0x80` |
|  `mvi byte, c` | `0x81` |
| `ld $r1, $r2` | N/A | $r2 = RAM[$r1] |
|  `ld a, a` | `0x82` |
|  `ld b, a` | `0x83` |
|  `ld c, a` | `0x84` |
|  `ld a, b` | `0x85` |
|  `ld b, b` | `0x86` |
|  `ld c, b` | `0x87` |
|  `ld a, c` | `0x88` |
|  `ld b, c` | `0x89` |
|  `ld c, c` | `0x8a` |
| `st $r1, $r2` | N/A | RAM[$r2] = $r1 |
|  `st a, a` | `0x8b` |
|  `st a, b` | `0x8c` |
|  `st a, c` | `0x8d` |
|  `st b, a` | `0x8e` |
|  `st b, b` | `0x8f` |
|  `st b, c` | `0x90` |
|  `st c, a` | `0x91` |
|  `st c, b` | `0x92` |
|  `st c, c` | `0x93` |
|  **`st z, a`** | `0x94` |
|  **`st z, b`** | `0x95` |
|  **`st z, c`** | `0x96` |
| `lds byte, $r1` | N/A | $r1 = RAM[sp + byte] |
|  `lds byte, a` | `0x97` |
|  `lds byte, b` | `0x98` |
|  `lds byte, c` | `0x99` |
| `sts $r1, byte` | N/A | RAM[sp + byte] = $r1 |
|  `sts a, byte` | `0x9a` |
|  `sts b, byte` | `0x9b` |
|  `sts c, byte` | `0x9c` |
|  **`sts z, byte`** | `0x9d` |
|  **`stsi byte_1, byte_2`** | `0x9e` | RAM [sp + byte_2] = byte_1
| `cmp $r1, $r2` | N/A | Perform $r1 - $r2 and set flags |
|  `cmp a, b` | `0x9f` |
|  `cmp a, c` | `0xa0` |
|  **`cmp a, z`** | `0xa1` |
|  `cmp b, a` | `0xa2` |
|  `cmp b, c` | `0xa3` |
|  **`cmp b, z`** | `0xa4` |
|  `cmp c, a` | `0xa5` |
|  `cmp c, b` | `0xa6` |
|  **`cmp c, z`** | `0xa7` |
|  **`cmp z, a`** | `0xa8` |
|  **`cmp z, b`** | `0xa9` |
|  **`cmp z, c`** | `0xaa` |
| `cmpi byte/$r1, $r1/byte` | N/A | Perform byte/$r1 - $r1/byte and set flags |
|  `cmpi a, byte` | `0xab` |
|  `cmpi byte, a` | `0xac` |
|  `cmpi b, byte` | `0xad` |
|  `cmpi byte, b` | `0xae` |
|  `cmpi c, byte` | `0xaf` |
|  `cmpi byte, c` | `0xb0` |
| `jmp byte` | `0xb1` | PC = byte |
| `je byte` | `0xb2` | if ZF, PC = byte |
| `jne byte` | `0xb3` | if ~ZF, PC = byte |
| `jg byte` | `0xb4` | if ~(SF ^ OF) & ~ZF, PC = byte |
| `jge byte` | `0xb5` | if ~(SF ^ OF), PC = byte |
| `jl byte` | `0xb6` | if (SF ^ OF), PC = byte |
| `jle byte` | `0xb7` | if (SF ^ OF) \| ZF, PC = byte |
|  **`ja byte`** | `0xb8` | if (~CF & ~ZF), PC = byte |
|  **`jae byte`** | `0xb9` | if (~CF), PC = byte |
|  **`jb byte`** | `0xba` | if (CF), PC = byte |
|  **`jbe byte`** | `0xbb` | if (CF \| ZF), PC = byte|
| `call byte` |`0xbc` | sp += 1, PC += 1, Stack[sp] = PC, PC = byte |
| `ret` | `0xbd` | PC = Stack[sp], sp -= 1 |
| `out $r1` | N/A | Send $r1 to decimal display |
|  `out a` | `0xbe` |
|  `out b` | `0xbf` |
|  `out c` | `0xc0` |
|  **`outi byte`** | `0xc1` | Send byte to decimal display
| `dic byte` | `0xc2` | Send command byte to LCD |
| `did byte` | `0xc3` | Send data byte to LCD |
|  **`dd $r1`** | N/A | Send $r1 to LCD
|  **`dd a`** | `0xc4` |
|  **`dd b`** | `0xc5` |
|  **`dd c`** | `0xc6` |
| `hlt` | `0xc7` | Halt |
| `nop` | `0xc8` | No operation |
