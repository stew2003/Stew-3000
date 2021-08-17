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
|  `add a, a` | `OPCODE TBD` |
|  `add a, b` | `OPCODE TBD` |
|  `add a, c` | `OPCODE TBD` |
|  `add a, sp` | `OPCODE TBD` |
|  `add b, a` | `OPCODE TBD` |
|  `add b, b` | `OPCODE TBD` |
|  `add b, c` | `OPCODE TBD` |
|  `add b, sp` | `OPCODE TBD` |
|  `add c, a` | `OPCODE TBD` |
|  `add c, b` | `OPCODE TBD` |
|  `add c, c` | `OPCODE TBD` |
|  `add c, sp` | `OPCODE TBD` |
| `addi byte, $r1` | N/A | $r1 = $r1 + byte |
|  `addi byte, a` | `OPCODE TBD` |
|  `addi byte, b` | `OPCODE TBD` |
|  `addi byte, c` | `OPCODE TBD` |
|  `addi byte, sp` | `OPCODE TBD` |
| **`addc $r1, $r2`** | N/A | $r2 = $r2 + $r1 + CF |
|  **`addc a, a`** | `OPCODE TBD` |
|  **`addc a, b`** | `OPCODE TBD` |
|  **`addc a, c`** | `OPCODE TBD` |
|  **`addc a, sp`** | `OPCODE TBD` |
|  **`addc b, a`** | `OPCODE TBD` |
|  **`addc b, b`** | `OPCODE TBD` |
|  **`addc b, c`** | `OPCODE TBD` |
|  **`addc b, sp`** | `OPCODE TBD` |
|  **`addc c, a`** | `OPCODE TBD` |
|  **`addc c, b`** | `OPCODE TBD` |
|  **`addc c, c`** | `OPCODE TBD` |
|  **`addc c, sp`** | `OPCODE TBD` |
| **`addci byte, $r1`** | N/A | $r1 = $r1 + byte + CF |
|  **`addci byte, a`** | `OPCODE TBD` |
|  **`addci byte, b`** | `OPCODE TBD` |
|  **`addci byte, c`** | `OPCODE TBD` |
|  **`addci byte, sp`** | `OPCODE TBD` |
| `sub $r1, $r2` | N/A | $r2 = $r2 - $r1 |
|  `sub b, a` | `OPCODE TBD` |
|  `sub c, a` | `OPCODE TBD` |
|  `sub a, b` | `OPCODE TBD` |
|  `sub c, b` | `OPCODE TBD` |
|  `sub a, c` | `OPCODE TBD` |
|  `sub b, c` | `OPCODE TBD` |
|  `sub a, sp` | `OPCODE TBD` |
|  `sub b, sp` | `OPCODE TBD` |
|  `sub c, sp` | `OPCODE TBD` |
| `subi byte, $r1` | N/A | $r1 = $r1 - byte |
|  `subi byte, a` | `OPCODE TBD` |
|  `subi byte, b` | `OPCODE TBD` |
|  `subi byte, c` | `OPCODE TBD` |
|  `subi byte, sp` | `OPCODE TBD` |
| **`subb $r1, $r2`** | N/A | $r2 = $r2 - $r1 - CF|
|  **`subb b, a`** | `OPCODE TBD` |
|  **`subb c, a`** | `OPCODE TBD` |
|  **`subb a, b`** | `OPCODE TBD` |
|  **`subb c, b`** | `OPCODE TBD` |
|  **`subb a, c`** | `OPCODE TBD` |
|  **`subb b, c`** | `OPCODE TBD` |
|  **`subb a, sp`** | `OPCODE TBD` |
|  **`subb b, sp`** | `OPCODE TBD` |
|  **`subb c, sp`** | `OPCODE TBD` |
| **`subbi byte, $r1`** | N/A | $r1 = $r1 - byte - CF|
|  **`subbi byte, a`** | `OPCODE TBD` |
|  **`subbi byte, b`** | `OPCODE TBD` |
|  **`subbi byte, c`** | `OPCODE TBD` |
|  **`subbi byte, sp`** | `OPCODE TBD` |
| `and $r1, $r2` | N/A | $r2 = $r2 & $r1 |
|  `and b, a` | `OPCODE TBD` |
|  `and c, a` | `OPCODE TBD` |
|  `and a, b` | `OPCODE TBD` |
|  `and c, b` | `OPCODE TBD` |
|  `and a, c` | `OPCODE TBD` |
|  `and b, c` | `OPCODE TBD` |
| `ani byte, $r1` | N/A | $r1 = $r1 & byte |
|  `ani byte, a` | `OPCODE TBD` |
|  `ani byte, b` | `OPCODE TBD` |
|  `ani byte, c` | `OPCODE TBD` |
| `or $r1, $r2` | N/A | $r2 = $r2 | $r1 |
|  `or b, a` | `OPCODE TBD` |
|  `or c, a` | `OPCODE TBD` |
|  `or a, b` | `OPCODE TBD` |
|  `or c, b` | `OPCODE TBD` |
|  `or a, c` | `OPCODE TBD` |
|  `or b, c` | `OPCODE TBD` |
| `ori byte, $r1` | N/A | $r1 = $r1 | byte |
|  `ori byte, a` | `OPCODE TBD` |
|  `ori byte, b` | `OPCODE TBD` |
|  `ori byte, c` | `OPCODE TBD` |
| `xor $r1, $r2` | N/A | $r2 = $r2 ^ $r1 |
|  `xor b, a` | `OPCODE TBD` |
|  `xor c, a` | `OPCODE TBD` |
|  `xor a, b` | `OPCODE TBD` |
|  `xor c, b` | `OPCODE TBD` |
|  `xor a, c` | `OPCODE TBD` |
|  `xor b, c` | `OPCODE TBD` |
| `xri byte, $r1` | N/A | $r1 = $r1 ^ byte |
|  `xri byte, a` | `OPCODE TBD` |
|  `xri byte, b` | `OPCODE TBD` |
|  `xri byte, c` | `OPCODE TBD` |
| `not $r1` | N/A | $r1 = ~$r1 |
|  `not a` | `OPCODE TBD` |
|  `not b` | `OPCODE TBD` |
|  `not c` | `OPCODE TBD` |
| `inr $r1` | N/A | $r1 = $r1 + 1 |
|  `inr a` | `OPCODE TBD` |
|  `inr b` | `OPCODE TBD` |
|  `inr c` | `OPCODE TBD` |
|  `inr sp` | `OPCODE TBD` |
| **`inr2 $r1`** | N/A | $r1 = $r1 + 2 |
|  **`inr2 a`** | `OPCODE TBD` |
|  **`inr2 b`** | `OPCODE TBD` |
|  **`inr2 c`** | `OPCODE TBD` |
|  **`inr2 sp`** | `OPCODE TBD` |
| **`inr3 $r1`** | N/A | $r1 = $r1 + 3 |
|  **`inr3 a`** | `OPCODE TBD` |
|  **`inr3 b`** | `OPCODE TBD` |
|  **`inr3 c`** | `OPCODE TBD` |
|  **`inr3 sp`** | `OPCODE TBD` |
| `dcr $r1` | N/A | $r1 = $r1 - 1 |
|  `dcr a` | `OPCODE TBD` |
|  `dcr b` | `OPCODE TBD` |
|  `dcr c` | `OPCODE TBD` |
|  `dcr sp` | `OPCODE TBD` |
| **`dcr2 $r1`** | N/A | $r1 = $r1 - 2 |
|  **`dcr2 a`** | `OPCODE TBD` |
|  **`dcr2 b`** | `OPCODE TBD` |
|  **`dcr2 c`** | `OPCODE TBD` |
|  **`dcr2 sp`** | `OPCODE TBD` |
| **`dcr3 $r1`** | N/A | $r1 = $r1 - 3 |
|  **`dcr3 a`** | `OPCODE TBD` |
|  **`dcr3 b`** | `OPCODE TBD` |
|  **`dcr3 c`** | `OPCODE TBD` |
|  **`dcr3 sp`** | `OPCODE TBD` |
| `mov $r1, $r2` | N/A | $r2 = $r1 |
|  `mov a, b` | `OPCODE TBD` |
|  `mov a, c` | `OPCODE TBD` |
|  `mov b, a` | `OPCODE TBD` |
|  `mov b, c` | `OPCODE TBD` |
|  `mov c, a` | `OPCODE TBD` |
|  `mov c, b` | `OPCODE TBD` |
|  **`mov z, a`** | `OPCODE TBD` |
|  **`mov z, b`** | `OPCODE TBD` |
|  **`mov z, c`** | `OPCODE TBD` |
|  **`mov sp, a`** | `OPCODE TBD` |
|  **`mov sp, b`** | `OPCODE TBD` |
|  **`mov sp, c`** | `OPCODE TBD` |
| `mvi byte, $r1` | N/A | $r1 = byte |
|  `mvi byte, a` | `OPCODE TBD` |
|  `mvi byte, b` | `OPCODE TBD` |
|  `mvi byte, c` | `OPCODE TBD` |
| `ld $r1, $r2` | N/A | $r2 = RAM[$r1] |
|  `ld a, a` | `OPCODE TBD` |
|  `ld b, a` | `OPCODE TBD` |
|  `ld c, a` | `OPCODE TBD` |
|  `ld a, b` | `OPCODE TBD` |
|  `ld b, b` | `OPCODE TBD` |
|  `ld c, b` | `OPCODE TBD` |
|  `ld a, c` | `OPCODE TBD` |
|  `ld b, c` | `OPCODE TBD` |
|  `ld c, c` | `OPCODE TBD` |
| `st $r1, $r2` | N/A | RAM[$r2] = $r1 |
|  `st a, a` | `OPCODE TBD` |
|  `st a, b` | `OPCODE TBD` |
|  `st a, c` | `OPCODE TBD` |
|  `st b, a` | `OPCODE TBD` |
|  `st b, b` | `OPCODE TBD` |
|  `st b, c` | `OPCODE TBD` |
|  `st c, a` | `OPCODE TBD` |
|  `st c, b` | `OPCODE TBD` |
|  `st c, c` | `OPCODE TBD` |
|  **`st z, a`** | `OPCODE TBD` |
|  **`st z, b`** | `OPCODE TBD` |
|  **`st z, c`** | `OPCODE TBD` |
| `lds byte, $r1` | N/A | $r1 = RAM[sp + byte] |
|  `lds byte, a` | `OPCODE TBD` |
|  `lds byte, b` | `OPCODE TBD` |
|  `lds byte, c` | `OPCODE TBD` |
| `sts $r1, byte` | N/A | RAM[sp + byte] = $r1 |
|  `sts a, byte` | `OPCODE TBD` |
|  `sts b, byte` | `OPCODE TBD` |
|  `sts c, byte` | `OPCODE TBD` |
|  **`sts z, byte`** | `OPCODE TBD` |
|  **`stsi byte_1, byte_2`** | `OPCODE TBD` | RAM [sp + byte_2] = byte_1
| `cmp $r1, $r2` | N/A | Perform $r1 - $r2 and set flags |
|  `cmp a, b` | `OPCODE TBD` |
|  `cmp a, c` | `OPCODE TBD` |
|  **`cmp a, z`** | `OPCODE TBD` |
|  `cmp b, a` | `OPCODE TBD` |
|  `cmp b, c` | `OPCODE TBD` |
|  **`cmp b, z`** | `OPCODE TBD` |
|  `cmp c, a` | `OPCODE TBD` |
|  `cmp c, b` | `OPCODE TBD` |
|  **`cmp c, z`** | `OPCODE TBD` |
|  **`cmp z, a`** | `OPCODE TBD` |
|  **`cmp z, b`** | `OPCODE TBD` |
|  **`cmp z, c`** | `OPCODE TBD` |
| `cmpi byte/$r1, $r1/byte` | N/A | Perform byte/$r1 - $r1/byte and set flags |
|  `cmpi a, byte` | `OPCODE TBD` |
|  `cmpi byte, a` | `OPCODE TBD` |
|  `cmpi b, byte` | `OPCODE TBD` |
|  `cmpi byte, b` | `OPCODE TBD` |
|  `cmpi c, byte` | `OPCODE TBD` |
|  `cmpi byte, c` | `OPCODE TBD` |
| `jmp byte` | `OPCODE TBD` | PC = byte |
| `je byte` | `OPCODE TBD` | if ZF, PC = byte |
| `jne byte` | `OPCODE TBD` | if ~ZF, PC = byte |
| `jg byte` | `OPCODE TBD` | if ~(SF ^ OF) & ~ZF, PC = byte |
| `jge byte` | `OPCODE TBD` | if ~(SF ^ OF), PC = byte |
| `jl byte` | `OPCODE TBD` | if (SF ^ OF), PC = byte |
| `jle byte` | `OPCODE TBD` | if (SF ^ OF) \| ZF, PC = byte |
|  **`ja byte`** | `OPCODE TBD` | if (~CF & ~ZF), PC = byte |
|  **`jae byte`** | `OPCODE TBD` | if (~CF), PC = byte |
|  **`jb byte`** | `OPCODE TBD` | if (CF), PC = byte |
|  **`jbe byte`** | `OPCODE TBD` | if (CF \| ZF), PC = byte|
| `call byte` |`OPCODE TBD` | sp += 1, PC += 1, Stack[sp] = PC, PC = byte |
| `ret` | `OPCODE TBD` | PC = Stack[sp], sp -= 1 |
| `out $r1` | N/A | Send $r1 to decimal display |
|  `out a` | `OPCODE TBD` |
|  `out b` | `OPCODE TBD` |
|  `out c` | `OPCODE TBD` |
|  **`outi byte`** | `OPCODE TBD` | Send byte to decimal display
| `dic byte` | `OPCODE TBD` | Send command byte to LCD |
| `did byte` | `OPCODE TBD` | Send data byte to LCD |
|  **`dd $r1`** | N/A | Send $r1 to LCD
|  **`dd a`** | `OPCODE TBD` |
|  **`dd b`** | `OPCODE TBD` |
|  **`dd c`** | `OPCODE TBD` |
| `hlt` | `OPCODE TBD` | Halt |
| `nop` | `OPCODE TBD` | No operation |
