import re
import sys

from register_names import register_names
from function3 import *
from function5 import *
from function7 import *
from opcodes import *

from elfheader import ELFHeader
from elfsectionheader import ELFSectionHeader




# This assembler is for 32 bit RISC-V assembly. It assembles base integer, multiply standard, atomic standard, and float standard. There are no vector or double precision standard instructions included.
# At this point it only supports labels and commands. No sections/segments thus far. No Encoded data thus far. Both of those will change in the near future.




class SymbolTableEntry():

    '''
    An object file's symbol table holds information needed to locate and relocate a program's symbolic definitions and references. A symbol table index is a subscript into this array. Index 0 both designates the first entry in the table and serves as the undefined symbol index.
    Find more at http://www.sco.com/developers/gabi/latest/ch4.symtab.html
    '''

    def __init__(self, st_name, st_value, st_size, st_info, st_other, st_shndx):

        # This member holds an index into the object file's symbol string table, which holds the character representations of the symbol names. If the value is non-zero, it represents a string table index that gives the symbol name. Otherwise, the symbol table entry has no name. 
        self.st_name = st_name

        # This member gives the value of the associated symbol. Depending on the context, this may be an absolute value, an address, and so on.
        self.st_value = st_value

        # Many symbols have associated sizes. For example, a data object's size is the number of bytes contained in the object. This member holds 0 if the symbol has no size or an unknown size. 
        self.st_size = st_size

        # This member specifies the symbol's type and binding attributes. See link in docstring for more.
        self.st_info = st_info

        # This member currently specifies a symbol's visibility. See link in docstring for more.
        self.st_other = st_other

        # Every symbol table entry is defined in relation to some section. This member holds the relevant section header table index. Some section indexes indicate special meanings. 
        self.st_shndx = st_shndx
        






opcode_table = {}
opcode_table.update(opcode_table_base_integer)
opcode_table.update(opcode_table_multiply_standard)
opcode_table.update(opcode_table_atomic_standard)
opcode_table.update(opcode_table_float_standard)

print(opcode_table)

# We start with 2 sections. These are the symbol table and the string table.
sh_num = 2

# The elf header will take up 52 bytes.
# Each section header is 40 bytes.


def first_pass(file_name):
    location_counter = 0
    symbol_table = {}

    with open(file_name) as f:
       while True:
            # We will get one line of source code at a time.
            source_line = f.readline()

            if not source_line:
                break

            # Remove any whitespace at the beginning of the source line
            source_line = re.sub("^\s*", "", source_line)
            
            # Remove any comment from the source line        
            source_line = re.sub("\s*;.*$", "", source_line)
            
            section_definition = re.search("\.section \.?(\w+)")
            if section_definition:
                section_name = section_definition.groups()[0]
                section_name = "." + section_name
                location_counter = 0
                sh_num += 1
                sh_off = 52 + 40 * sh_num
                #Add a string table entry
                #Add a symbol table entry
                continue



            print(section_name, location_counter, ":", source_line) 



            # If there is a label defined, we will store it in label variable. This is a match object
            # If there is no label defined in this line of source code, label will be None
            label_definition = re.search("^\w{1,16}:", source_line)
            if label_definition:
                symbol = label_definition.group(0)
                label_address = location_counter

                # We cannot define a label with the same name as a register. That would be stupid
                assert symbol not in register_names
                
                # You cannot define a label twice. Ha!
                assert symbol not in symbol_table
                
                symbol_table[symbol] = label_address

                source_line = re.sub("^\w{1,16}:\s*", "", source_line)
            
            # If there was nothing assembleable on this line, then we will not increment the location counter by a word (4 bytes)
            if re.search("^\W*\n$", source_line):
                continue
            
            # If there was assembly on this line, then we will add 4 to the value of the location counter.
            # We do this to determine where the next instruction would go in memory. Each instruction is 4 bytes long.
            location_counter += 4

            # This is a 32 bit risc-v assembler. All instructions will be the same length of 4 bytes, one word.
            # That means that we do not have to look at the operator or operands to determine the length of the instruction. 
            # We simply increment the location counter by 4 (referring to 4 bytes) when finding the location where
            # the next instruction will be stored.
    return symbol_table


def second_pass_executable_section(file_descriptor, symbol_table):
    pass







def second_pass(file_name, symbol_table):
    '''
        This is where we actually build the object file using the source code and the symbol table that was generated by the first pass
    '''

    location_counter = 0

    with open(file_name) as f:
        while True:
            source_line = f.readline()
            
            # Determine if this is the last line of the program
            # If there is no more to a file, readline will return an empty string, which is Falsey
            if not source_line:
                break

            # Remove any whitespace at the beginning of the source line
            source_line = re.sub("^\s", "", source_line)

            # If the line has a comment, then store it in the comment variable
            comment = re.search(";.*$", source_line)
            if comment:
                comment = comment.group(0)
                # Remove any comment from the source line        
                source_line = source_line.replace( comment, "" )
            

            # If there is a label definition on the source line, we want to ignore that.
            # We do this by simply removing it from the source_line variable.
            # The label definition will be no help in the second pass.
            source_line = re.sub("^\w{1,16}:\s*", "", source_line)
            

            # If there was nothing assembleable on this line, then we will not increment the location counter by a word (4 bytes)
            # We will also not read for instructions, since that would otherwise throw errors
            if re.search("^\W*\n$", source_line):
                continue

            # Get the Operation of the instruction
            instruction_operation = source_line.split()[0].upper()

            print(location_counter, ":", instruction_operation)

            # Now we need to ensure that the instruction is in the OpCode table
            # opcode will be used to test whether the instruction_operation is in the opcode_table
            # opcode will also be used to hold the bits of the opcode of the instruction
            opcode = opcode_table.get(instruction_operation)
            if not opcode:
                raise ValueError(instruction_operation + " not in opcode table")


            # This array represents the operands expected based on the instruction operation given in the source line.
            # The 0th position is in the array represents the destination.
            # The last three elements of the array represent the inputs.
            # A value of "" means there is no value expected.
            # A value of "r" means that a register is expected.
            # A value of "i" means that an immediate is expected.
            # A value of "u" means that an upper immediate is expected.
            # A value of "f" means that a float register is expected.
            # A value of "fi" means that a float immediate is expected.
            # A value of "rm" means that a three bit field to specify rounding
            # A value of "aqrl" means a two bit field used to specify type of atomicity in atomic instructions
            operands_expected = ["","","",""];
            rtype = False
            itype = False
            stype = False
            utype = False
            btype = False
            jtype = False
            r4type = False
            


            # Now we will determine the function3, function5, and function7 codes, should they exist.
            # Not all instructions have these fields
            # We will also determine the number and type of operands required.
            if instruction_operation in opcode_table_base_integer:
                function3 = function3_code_table_base_integer.get(instruction_operation)
                function7 = function7_code_table_base_integer.get(instruction_operation)
                if function7:
                    # This block is for R type instructions
                    # All and only R type instructions have a function7 value.
                    operands_expected[0] = "r"
                    operands_expected[1] = "r"
                    operands_expected[2] = "r"
                    rtype = True
                 elif instruction_operation in ["LB", "LH", "LW", "LBU", "ADDI", "SLTI", "SLTIU", "XORI", "ORI", "ANDI",]
                    # This block is for I type instructions
                    operands_expected[0] = "r"
                    operands_expected[1] = "r"
                    operands_expected[2] = "i"
                    itype = True

                 elif instruction_operation in ["SW", "SH", "SB"]:
                    # This block is for S type instrucions
                    operands_expected[0] = "r"
                    operands_expected[1] = "r"
                    operands_expected[2] = "i"
                    stype = True

                elif (opcode == 0b0110111 || opcode == 0b0010111):
                    # This block is for U type instructions
                    operands_expected[0] = "r"
                    operands_expected[1] = "u"
                    utype = True

               

                elif instruction_operation in ["BEQ", "BNE", "BLT", "BGE", "BLTU", "BGEU"]
                    # This block is for B type instructions
                    operands_expected[0] = "r"
                    operands_expected[1] = "r"
                    operands_expected[2] = "i"
                    btype = True

                elif instruction_operation == "JAL":
                    # This block is for J type instructions
                    operands_expected[0] = "r"
                    operands_expected[1] = "u"
                    jtype = True

            elif instruction_operation in opcode_table_multiply_standard:
                function3 = function3_code_table_multiply_standard.get(instruction_operation)
                # All multiply instructions have the same function7 value.
                function7 = 0B0000001

                operands_expected = ["r", "r", "r", ""]
                rtype = True

            elif instruction_operation in opcode_table_atomic_standard:
                # All atomic instructions have the same function3 value
                function3 = 0B010
                # All atomic instructions do not have a function7 value. They each instead have a function5 field
                # followed by two bits, aq for acquire access and rl for release access.
                function5 = function5_code_table_atomic_standard.get(instruction_operation)
                
                operands_expected[0] = "r"
                operands_expected[1] = "r"
                if instruction_operation == "LR.W":
                    operands_expected[2] = "aqrl"
                else:
                    operands_expected[2] = "r"
                    operands_expected[3] = "aqrl"
                rtype = True


            elif instruction_operation in opcode_table_float_standard:
                function3 = function3_code_table_float_standard.get(instruction_operation)
                function7 = function7_code_table_float_standard.get(instruction_operation)
                
                if instruction_operation == "FLW" || instruction_operation == "FSW":
                    # This block is for I type instructions and S type instructions
                    # FLW is a the I type and FSW is the S type instruction
                    operands_expected[0] = "f"
                    operands_expected[1] = "r"
                    operands_expected[2] = "i"
                    if instruction_operation == "FSW":
                        stype = True
                    else:
                        itype = True
                
                elif instruction_operation in ["FMADD.S", "FMSUB.S", "FNMSUB.S", "FNMADD.S"]:
                    # This block is for R4 type instructions
                    operands_expected = ["f"] * 4
                    r4type = True

                elif instruction_operation in [ "FADD.S", "FSUB.S", "FMUL.S", "FDIV.S", "FSQRT.S", "FSGNJ.S", "FSGNJN.S", "FSGNJX.S", "FMIN.S", "FMAX.S", "FCVT.W.S",
                        "FCVT.WU.S", "FMV.X.W", "FEQ.S", "FLT.S", "FLE.S", "FCLASS.S", "FCVT.S.W", "FCVT.S.WU", "FMV.W.X" ]:
                    # This block is for R type instructions
                    operands_expected[0] = "f"
                    operadns_expected[1] = "f"
                    operands_expected[2] = "f"
                    rtype = True

                
                
            else:
                raise Exception(instruction_operation, "found in opcode table, but not in any of its component dictionaries.")


            # The line isolates the operands from the instruction_operation.
            # We get the string of the instruction starting just after the space that follows the instruction_operation.
            # Since the comment and any label have already been removed, that leaves us with just the operands separated by commas.
            operands = source_line[len(instruction_operation)+1::]
            # We need to convert the string of comma separated operands into a list of strings, one element per operand.
            operands = operands.split(",")
            # Now it would make sense to remove any whitespace preceding or trailing the operands.
            operands = [ re.sub("^\s*", "", operand) for operand in operands ]
            operands = [ re.sub("\s*$", "", operand) for operand in operands ]

            # Now we have the operands preprocessed the way we would like them.
            # Now would be a good time to confirm that we have the right amount of operands.
            # Many instructions can accept a variable number of instructions, using one of the operands
            # as the destination
            num_operands_given = len(operands)







            location_counter += 4

    # Create an ELF Header
    # First, make the e_ident property
    e_ident = ELFHeader.create_e_ident()
    # If we are just making a relocatable object file, then we don't need a program header. So e_phoff will be 0
    # The property that will need to eventually be changeable will be e_type. We will need to make executable files at some point.
    elfheader = ELFHeader(e_ident)
    


if __name__ == "__main__":
    # need to make an elf header
    # need to make a section header table
    # need to make sections



    input_file = sys.argv[1]
    if sys.argc > 1:
        output_file = 
    symbol_table = first_pass(input_file)
    print("Symbol Table:", symbol_table)
    print()
    
    second_pass(input_file, symbol_table)
