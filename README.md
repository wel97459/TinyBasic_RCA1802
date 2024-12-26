# Netronic TinyBasic for the ELF II
This repository contains a version of the Netronic TinyBasic interpreter for the ELF II microcomputer,
with the Basic IL (Intermediate Language) in a readable and assemblable format.

### How to assemble
You will need to use ASL assembler: [wel97459/ASL](https://github.com/wel97459/asl)
```bash
asl -cpu 1802 -x -P -L Tinybasi_IL.asm && p2bin Tinybasi_IL.p Tinybasi_IL.bin
```

