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
| `PGM` | Accessing program or instruction memory |
| `SO` | Stack pointer out |
| `SI` | Stack pointer in |


## Instruction Set

Note: instructions listed with opcode N/A describe the behavior of a whole class of instructions, which have specific opcodes that correspond to the use of specific registers.


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
| `sub $r1, $r2` | N/A | $r2 = $r2 - $r1 |
|  `sub b, a` | `0x10` |
|  `sub c, a` | `0x11` |
|  `sub a, b` | `0x12` |
|  `sub c, b` | `0x13` |
|  `sub a, c` | `0x14` |
|  `sub b, c` | `0x15` |
|  `sub a, sp` | `0x16` |
|  `sub b, sp` | `0x17` |
|  `sub c, sp` | `0x18` |
| `subi byte, $r1` | N/A | $r1 = $r1 - byte |
|  `subi byte, a` | `0x19` |
|  `subi byte, b` | `0x1a` |
|  `subi byte, c` | `0x1b` |
|  `subi byte, sp` | `0x1c` |
| `and $r1, $r2` | N/A | $r2 = $r2 & $r1 |
|  `and b, a` | `0x1d` |
|  `and c, a` | `0x1e` |
|  `and a, b` | `0x1f` |
|  `and c, b` | `0x20` |
|  `and a, c` | `0x21` |
|  `and b, c` | `0x22` |
| `ani byte, $r1` | N/A | $r1 = $r1 & byte |
|  `ani byte, a` | `0x23` |
|  `ani byte, b` | `0x24` |
|  `ani byte, c` | `0x25` |
| `or $r1, $r2` | N/A | $r2 = $r2 | $r1 |
|  `or b, a` | `0x26` |
|  `or c, a` | `0x27` |
|  `or a, b` | `0x28` |
|  `or c, b` | `0x29` |
|  `or a, c` | `0x2a` |
|  `or b, c` | `0x2b` |
| `ori byte, $r1` | N/A | $r1 = $r1 | byte |
|  `ori byte, a` | `0x2c` |
|  `ori byte, b` | `0x2d` |
|  `ori byte, c` | `0x2e` |
| `xor $r1, $r2` | N/A | $r2 = $r2 ^ $r1 |
|  `xor b, a` | `0x2f` |
|  `xor c, a` | `0x30` |
|  `xor a, b` | `0x31` |
|  `xor c, b` | `0x32` |
|  `xor a, c` | `0x33` |
|  `xor b, c` | `0x34` |
| `xri byte, $r1` | N/A | $r1 = $r1 ^ byte |
|  `xri byte, a` | `0x35` |
|  `xri byte, b` | `0x36` |
|  `xri byte, c` | `0x37` |
| `not $r1` | N/A | $r1 = ~$r1 |
|  `not a` | `0x38` |
|  `not b` | `0x39` |
|  `not c` | `0x3a` |
| `inr $r1` | N/A | $r1 = $r1 + 1 |
|  `inr a` | `0x3b` |
|  `inr b` | `0x3c` |
|  `inr c` | `0x3d` |
|  `inr sp` | `0x3e` |
| `dcr $r1` | N/A | $r1 = $r1 - 1 |
|  `dcr a` | `0x3f` |
|  `dcr b` | `0x40` |
|  `dcr c` | `0x41` |
|  `dcr sp` | `0x42` |
| `mov $r1, $r2` | N/A | $r2 = $r1 |
|  `mov a, b` | `0x43` |
|  `mov a, c` | `0x44` |
|  `mov b, a` | `0x45` |
|  `mov b, c` | `0x46` |
|  `mov c, a` | `0x47` |
|  `mov c, b` | `0x48` |
| `mvi byte, $r1` | N/A | $r1 = byte |
|  `mvi byte, a` | `0x49` |
|  `mvi byte, b` | `0x4a` |
|  `mvi byte, c` | `0x4b` |
| `ld $r1, $r2` | N/A | $r2 = RAM[$r1] |
|  `ld a, a` | `0x4c` |
|  `ld b, a` | `0x4d` |
|  `ld c, a` | `0x4e` |
|  `ld a, b` | `0x4f` |
|  `ld b, b` | `0x50` |
|  `ld c, b` | `0x51` |
|  `ld a, c` | `0x52` |
|  `ld b, c` | `0x53` |
|  `ld c, c` | `0x54` |
| `st $r1, $r2` | N/A | RAM[$r2] = $r1 |
|  `st a, a` | `0x55` |
|  `st a, b` | `0x56` |
|  `st a, c` | `0x57` |
|  `st b, a` | `0x58` |
|  `st b, b` | `0x59` |
|  `st b, c` | `0x5a` |
|  `st c, a` | `0x5b` |
|  `st c, b` | `0x5c` |
|  `st c, c` | `0x5d` |
| `lds byte, $r1` | N/A | $r1 = RAM[sp + byte] |
|  `lds byte, a` | `0x5e` |
|  `lds byte, b` | `0x5f` |
|  `lds byte, c` | `0x60` |
| `sts $r1, byte` | N/A | RAM[sp + byte] = $r1 |
|  `sts a, byte` | `0x61` |
|  `sts b, byte` | `0x62` |
|  `sts c, byte` | `0x63` |
| `cmp $r1, $r2` | N/A | Perform $r1 - $r2 and set flags |
|  `cmp a, b` | `0x64` |
|  `cmp a, c` | `0x65` |
|  `cmp b, a` | `0x66` |
|  `cmp b, c` | `0x67` |
|  `cmp c, a` | `0x68` |
|  `cmp c, b` | `0x69` |
| `cmpi byte/$r1, $r1/byte` | N/A | Perform byte/$r1 - $r1/byte and set flags |
|  `cmpi a, byte` | `0x6a` |
|  `cmpi byte, a` | `0x6b` |
|  `cmpi b, byte` | `0x6c` |
|  `cmpi byte, b` | `0x6d` |
|  `cmpi c, byte` | `0x6e` |
|  `cmpi byte, c` | `0x6f` |
| `jmp byte` | `0x70` | PC = byte |
| `je/jz byte` | `0x71` | if ZF, PC = byte |
| `jne/jnz byte` | `0x72` | if ~ZF, PC = byte |
| `jg/jnle byte` | `0x73` | if ~(SF ^ OF) & ~ZF, PC = byte |
| `jge/jnl byte` | `0x74` | if ~(SF ^ OF), PC = byte |
| `jl/jnge byte` | `0x75` | if SF ^ OF, PC = byte |
| `jle/jng byte` | `0x76` | if (SF ^ OF) \| ZF, PC = byte |
| `call byte` |`0x77` | sp += 1, PC += 1, Stack[sp] = PC, PC = byte |
| `ret` | `0x78` | PC = Stack[sp], sp -= 1 |
| `out $r1` | N/A | Send $r1 to decimal display |
|  `out a` | `0x79` |
|  `out b` | `0x7a` |
|  `out c` | `0x7b` |
| `dic byte` | `0x7c` | Send command byte to LCD |
| `did byte` | `0x7d` | Send data byte to LCD |
| `hlt` | `0x7e` | Halt |
| `nop` | `0x7f` | No operation |


| DREAM Instruction | Opcode | Description |
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
| `sub $r1, $r2` | N/A | $r2 = $r2 - $r1 |
|  `sub b, a` | `0x10` |
|  `sub c, a` | `0x11` |
|  `sub a, b` | `0x12` |
|  `sub c, b` | `0x13` |
|  `sub a, c` | `0x14` |
|  `sub b, c` | `0x15` |
|  `sub a, sp` | `0x16` |
|  `sub b, sp` | `0x17` |
|  `sub c, sp` | `0x18` |
| `subi byte, $r1` | N/A | $r1 = $r1 - byte |
|  `subi byte, a` | `0x19` |
|  `subi byte, b` | `0x1a` |
|  `subi byte, c` | `0x1b` |
|  `subi byte, sp` | `0x1c` |
| `and $r1, $r2` | N/A | $r2 = $r2 & $r1 |
|  `and b, a` | `0x1d` |
|  `and c, a` | `0x1e` |
|  `and a, b` | `0x1f` |
|  `and c, b` | `0x20` |
|  `and a, c` | `0x21` |
|  `and b, c` | `0x22` |
| `ani byte, $r1` | N/A | $r1 = $r1 & byte |
|  `ani byte, a` | `0x23` |
|  `ani byte, b` | `0x24` |
|  `ani byte, c` | `0x25` |
| `or $r1, $r2` | N/A | $r2 = $r2 | $r1 |
|  `or b, a` | `0x26` |
|  `or c, a` | `0x27` |
|  `or a, b` | `0x28` |
|  `or c, b` | `0x29` |
|  `or a, c` | `0x2a` |
|  `or b, c` | `0x2b` |
| `ori byte, $r1` | N/A | $r1 = $r1 | byte |
|  `ori byte, a` | `0x2c` |
|  `ori byte, b` | `0x2d` |
|  `ori byte, c` | `0x2e` |
| `xor $r1, $r2` | N/A | $r2 = $r2 ^ $r1 |
|  `xor b, a` | `0x2f` |
|  `xor c, a` | `0x30` |
|  `xor a, b` | `0x31` |
|  `xor c, b` | `0x32` |
|  `xor a, c` | `0x33` |
|  `xor b, c` | `0x34` |
| `xri byte, $r1` | N/A | $r1 = $r1 ^ byte |
|  `xri byte, a` | `0x35` |
|  `xri byte, b` | `0x36` |
|  `xri byte, c` | `0x37` |
| `not $r1` | N/A | $r1 = ~$r1 |
|  `not a` | `0x38` |
|  `not b` | `0x39` |
|  `not c` | `0x3a` |
| `inr $r1` | N/A | $r1 = $r1 + 1 |
|  `inr a` | `0x3b` |
|  `inr b` | `0x3c` |
|  `inr c` | `0x3d` |
|  `inr sp` | `0x3e` |
| **`inr2 $r1`** | N/A | $r1 = $r1 + 2 |
|  **`inr2 a`** | `0x3b` |
|  **`inr2 b`** | `0x3c` |
|  **`inr2 c`** | `0x3d` |
|  **`inr2 sp`** | `0x3e` |
| **`inr3 $r1`** | N/A | $r1 = $r1 + 3 |
|  **`inr3 a`** | `0x3b` |
|  **`inr3 b`** | `0x3c` |
|  **`inr3 c`** | `0x3d` |
|  **`inr3 sp`** | `0x3e` |
| `dcr $r1` | N/A | $r1 = $r1 - 1 |
|  `dcr a` | `0x3f` |
|  `dcr b` | `0x40` |
|  `dcr c` | `0x41` |
|  `dcr sp` | `0x42` |
| **`dcr2 $r1`** | N/A | $r1 = $r1 - 2 |
|  **`dcr a`** | `0x3f` |
|  **`dcr b`** | `0x40` |
|  **`dcr c`** | `0x41` |
|  **`dcr sp`** | `0x42` |
| **`dcr2 $r1`** | N/A | $r1 = $r1 - 3 |
|  **`dcr a`** | `0x3f` |
|  **`dcr b`** | `0x40` |
|  **`dcr c`** | `0x41` |
|  **`dcr sp`** | `0x42` |
| `mov $r1, $r2` | N/A | $r2 = $r1 |
|  `mov a, b` | `0x43` |
|  `mov a, c` | `0x44` |
|  `mov b, a` | `0x45` |
|  `mov b, c` | `0x46` |
|  `mov c, a` | `0x47` |
|  `mov c, b` | `0x48` |
|  **`mov z, a`** | `0x0c` |
|  **`mov z, b`** | `0x0c` |
|  **`mov z, c`** | `0x0c` |
|  **`mov sp, a`** | `0x0c` |
|  **`mov sp, b`** | `0x0c` |
|  **`mov sp, c`** | `0x0c` |
| `mvi byte, $r1` | N/A | $r1 = byte |
|  `mvi byte, a` | `0x49` |
|  `mvi byte, b` | `0x4a` |
|  `mvi byte, c` | `0x4b` |
| `ld $r1, $r2` | N/A | $r2 = RAM[$r1] |
|  `ld a, a` | `0x4c` |
|  `ld b, a` | `0x4d` |
|  `ld c, a` | `0x4e` |
|  `ld a, b` | `0x4f` |
|  `ld b, b` | `0x50` |
|  `ld c, b` | `0x51` |
|  `ld a, c` | `0x52` |
|  `ld b, c` | `0x53` |
|  `ld c, c` | `0x54` |
| `st $r1, $r2` | N/A | RAM[$r2] = $r1 |
|  `st a, a` | `0x55` |
|  `st a, b` | `0x56` |
|  `st a, c` | `0x57` |
|  `st b, a` | `0x58` |
|  `st b, b` | `0x59` |
|  `st b, c` | `0x5a` |
|  `st c, a` | `0x5b` |
|  `st c, b` | `0x5c` |
|  `st c, c` | `0x5d` |
|  **`st z, a`** | `0x0c` |
|  **`st z, b`** | `0x0c` |
|  **`st z, c`** | `0x0c` |
| `lds byte, $r1` | N/A | $r1 = RAM[sp + byte] |
|  `lds byte, a` | `0x5e` |
|  `lds byte, b` | `0x5f` |
|  `lds byte, c` | `0x60` |
| `sts $r1, byte` | N/A | RAM[sp + byte] = $r1 |
|  `sts a, byte` | `0x61` |
|  `sts b, byte` | `0x62` |
|  `sts c, byte` | `0x63` |
|  **`sts z, byte`** | `0x0c` |
|  **`stsi byte_1, byte_2`** | `0x0c` | RAM [sp + byte_2] = byte_1
| `cmp $r1, $r2` | N/A | Perform $r1 - $r2 and set flags |
|  `cmp a, b` | `0x64` |
|  `cmp a, c` | `0x65` |
|  **`cmp a, z`** | `0x0c` |
|  `cmp b, a` | `0x66` |
|  `cmp b, c` | `0x67` |
|  **`cmp b, z`** | `0x0c` |
|  `cmp c, a` | `0x68` |
|  `cmp c, b` | `0x69` |
|  **`cmp c, z`** | `0x0c` |
|  **`cmp z, a`** | `0x0c` |
|  **`cmp z, b`** | `0x0c` |
|  **`cmp z, c`** | `0x0c` |
| `cmpi byte/$r1, $r1/byte` | N/A | Perform byte/$r1 - $r1/byte and set flags |
|  `cmpi a, byte` | `0x6a` |
|  `cmpi byte, a` | `0x6b` |
|  `cmpi b, byte` | `0x6c` |
|  `cmpi byte, b` | `0x6d` |
|  `cmpi c, byte` | `0x6e` |
|  `cmpi byte, c` | `0x6f` |
| `jmp byte` | `0x70` | PC = byte |
| `je byte` | `0x71` | if ZF, PC = byte |
| `jne byte` | `0x72` | if ~ZF, PC = byte |
| `jg byte` | `0x73` | if ~(SF ^ OF) & ~ZF, PC = byte |
| `jge byte` | `0x74` | if ~(SF ^ OF), PC = byte |
| `jl byte` | `0x75` | if (SF ^ OF), PC = byte |
| `jle byte` | `0x76` | if (SF ^ OF) \| ZF, PC = byte |
|  **`ja byte`** | `0x0c` | if (~CF & ~ZF), PC = byte |
|  **`jae byte`** | `0x0c` | if (~CF), PC = byte |
|  **`jb byte`** | `0x0c` | if (CF), PC = byte |
|  **`jbe byte`** | `0x0c` | if (CF \| ZF), PC = byte|
| `call byte` |`0x77` | sp += 1, PC += 1, Stack[sp] = PC, PC = byte |
| `ret` | `0x78` | PC = Stack[sp], sp -= 1 |
| `out $r1` | N/A | Send $r1 to decimal display |
|  `out a` | `0x79` |
|  `out b` | `0x7a` |
|  `out c` | `0x7b` |
|  **`outi byte`** | `0x0c` | Send byte to decimal display
| `dic byte` | `0x7c` | Send command byte to LCD |
| `did byte` | `0x7d` | Send data byte to LCD |
|  **`dd $r1`** | N/A | Send $r1 to LCD
|  **`dd a`** | N/A | Send $r1 to LCD
|  **`dd b`** | N/A | Send $r1 to LCD
|  **`dd c`** | N/A | Send $r1 to LCD
| `hlt` | `0x7e` | Halt |
| `nop` | `0x7f` | No operation |
