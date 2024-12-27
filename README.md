# Netronic TinyBasic for the ELF II
This repository contains a version of the Netronic TinyBasic interpreter,
with the Basic IL (Intermediate Language) in a readable and assemblable format.

#### Tinybasic_IL.asm
When assembled comes close to the original.

#### Tinybasic_IL_Fixed.asm
There have been changes to the Interrupt Service Routine (ISR) for the 1861.

The original version used the R0 (Register 0 - DMA Pointer) to increment the time counter. This could potentially lead to memory corruption issues.

### How to assemble
You will need to use ASL assembler: [wel97459/ASL](https://github.com/wel97459/asl)
```bash
asl -cpu 1802 -x -P -L Tinybasic_IL.asm && p2bin Tinybasic_IL.p Tinybasic_IL.bin
```

### To test 
Use [EMMA 02](https://www.emma02.hobby-site.com/)
* Elf Tab
* Quest Super Elf
* Load the config for Netronic TinyBasic
* **(File->Configuration->Load->Netronic TinyBasic->*)**
* Then for RAM 2 load the the bin file.