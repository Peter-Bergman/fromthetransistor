function7_code_table_base_integer = {
    "SLLI": 0B0000000,
    "SRLI": 0B0000000,
    "ADD": 0B0000000,
    "SLL": 0B0000000,
    "SLT": 0B0000000,
    "SLTU": 0B0000000,
    "XOR": 0B0000000,
    "SRL": 0B0000000,
    "OR": 0B0000000,
    "AND": 0B0000000,

    "SRAI": 0B0100000,
    "SUB": 0B0100000,
    "SRA": 0B0100000,
}

# Multiply standard does not need a table for function7. Every one of the multiply instructions is a R (register) type instruction.
# That means that they all have a function7. However, they all have the same function7 value: 0B0000001.

# Atomic standard does not need a table for function7. We will instead have a function5 table for it. 
# As you can read in the "RISC-V Instruction Set Manual" v2.2 pdf, atomic instructions have a function5 field and two bits that follow.
# These bits are the aq and rl bits. Read more about them at https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf


function7_code_table_float_standard = {
    "FADD.S": 0B0000000,
    "FSUB.S": 0B0000100,
    "FMUL.S": 0B0001000,
    "FDIV.S": 0B0001100,
    "FSQRT.S": 0B0101100,

    "FSGNJ.S": 0B0010000,
    "FSGNJN.S": 0B0010000,
    "FSGNJX.S": 0B0010000,

    "FMIN.S": 0B0010100,
    "FMAX.S": 0B0010100,
    
    "FCVT.W.S": 0B1100000,
    "FCVT.WU.S": 0B1100000,
    "FMV.X.W": 0B1110000,

    "FEQ.S": 0B1010000,
    "FLT.S": 0B1010000,
    "FLE.S": 0B1010000,

    "FCLASS.S": 0B1110000,

    "FCVT.S.W": 0B1101000,
    "FCVT.W.WU": 0B1101000,

    "FMV.W.X": 0B1111000,
}

