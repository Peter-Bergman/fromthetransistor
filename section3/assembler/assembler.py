#!/bin/python3


import re
import sys
import os

from decode_data import decode_data
from decode_operands import decode_operands
from register_names import register_names, integer_register_names, fp_register_names
from function3 import *
#from function5 import *
from function7 import *
from opcodes import *

from elfheader import ELFHeader
from elfsectionheader import ELFSection
from stringtable import StringTable
from symboltable import SymbolTable, SymbolTableEntry



# This assembler is for 32 bit RISC-V assembly. It assembles base integer, multiply standard, atomic standard, and float standard. There are no vector or double precision standard instructions included.
# At this point it only supports labels and commands. No sections/segments thus far. No Encoded data thus far. Both of those will change in the near future.


opcode_table = {}
opcode_table.update(opcode_table_base_integer)
opcode_table.update(opcode_table_multiply_standard)
opcode_table.update(opcode_table_atomic_standard)
opcode_table.update(opcode_table_float_standard)

print("[+] opcode_table\n", opcode_table)
print("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")


# The elf header will take up 52 bytes.
# Each section header is 40 bytes.

def throw_syntax_error(operands_expected, line, instruction_operation):
                message = "On Line " + str(line)
                message += "\n" + "Expected operands of following format:"
                message += "\n" + str(instruction_operation) + " " + ",".join([ f"[{operand}]" for operand in operands_expected ])
                raise Exception(message)


def first_pass(input_file):
    
    e_shnum = 2 # We start with 2 sections. These are the string table and the symbol table.
    string_table = StringTable()
    symbol_table = SymbolTable()
    sections = [] # This is a list of all of the sections created by a pass through the input_file
    location_counter = 0
    executable_section = False
    uninitialized_section = False
    with open(input_file) as f:
        line = 1
        while True:
            # We will get one line of source code at a time.
            source_line = f.readline()

            if not source_line:
                break

            # Remove any whitespace at the beginning of the source line
            source_line = re.sub("^\s*", "", source_line)
            
            # Remove any comment from the source line        
            source_line = re.sub("\s*;.*$", "", source_line)
            
            section_definition = re.search("\.section \.?(\w+)", source_line)
            if section_definition:

                if "section" in locals():
                    # finish off some of the fields from the previous section
                    section.sh_size = location_counter
                    assert location_counter == len(section.text)
                    section.sh_addr = 0
                    # section.sh_offset will be set once all of the sections have been made and the string table, section headers, and symbol table have definite sizes.
                    section.sh_link = 0
                    section.sh_info = 0
                    section.sh_addralign = 0
                    section.sh_entsize = 0
                    sections.append(section)

                # Now create calculate the fields for the section being defined.
                section_name = section_definition.groups()[0]
                section_name = "." + section_name
                #reset the location counter
                location_counter = 0
                e_shnum += 1
                executable_section = "/*exe*/" in source_line
                uninitialized_section = "/*uninit*/" in source_line
                if executable_section and uninitialized_section:
                    raise Exception(f"On line {line}, Section cannot be both executable and uninitialized")
                elif executable_section:
                    # .text section
                    sh_type = 1 # 1 is PROGBITS. Google for more info
                    sh_flags = 4
                elif uninitialized_section:
                    # .bss section
                    sh_type = 8 # 8 corresponds to NOBITS. We will still have a size for the section, but there will be no string written to the file as section text.
                    sh_flags = 1
                else:
                    # .data section
                    sh_type = 1 # 1 is PROGBITS. Google for more info
                    sh_flags = 1

                            

                # Add a string table entry and get the address for the name.
                sh_name = string_table.give_string_table_location(section_name)
                # First make sure that there is not already a symbol with that name.
                if symbol_table.check_for_symbol_by_name(sh_name):
                    raise Exception(f"You cannot define a symbol twice. But you tried to be a sneaky cunt on line {line}")



                # Add a symbol table entry
                st_name = sh_name
                st_value = 0
                st_size = 0
                st_info = 3 # 3 corresponds to section
                st_other = 0 # 0 is the default
                st_shndx = e_shnum - 1
                symbol_table_entry = SymbolTableEntry(st_name, st_value, st_size, st_info, st_other, st_shndx)
                symbol_table.add_symbol_table_entry(symbol_table_entry)

                #Begin a section header
                section = ELFSection(sh_name, sh_type, sh_flags, "")
                line += 1
                continue



            print(section_name, location_counter, ":", source_line) 



            # If there is a label defined, we will store it in label variable. This is a match object
            # If there is no label defined in this line of source code, label will be None
            label_definition = re.search("^\w{1,16}:", source_line)
            if label_definition:
                st_name = label_definition.group(0)
                st_name = string_table.give_string_table_location(st_name)
                if symbol_table.check_for_symbol_by_name(st_name):
                    raise Exception(f"You cannot define a symbol twice. But you tried to be a sneaky cunt on line {line}")
                label_address = location_counter

                st_size = 0 # We will just leave this as zero. GDB does not even show this, I think.
                
                if executable_section:
                    st_info = 2 # This corresponds with function
                else:
                    st_info = 1 # This corresponds with object

                st_value = location_counter
                st_other = 0
                # We set the section header index to e_shnum - 1 because that will be the index of the section currently being edited.
                st_shndx = e_shnum - 1
                
                #Add the label to the string table
                source_line = re.sub("^\w{1,16}:\s*", "", source_line)
            
            # If there was nothing assembleable on this line, then we will not increment the location counter by a word (4 bytes)
            if re.search("^\W*\n$", source_line):
                line += 1
                continue
            

            if not section_name:
                raise Exception("code outside of exception")


            if executable_section:
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
                # The 0th position in the array represents the destination.
                # The last four elements of the array represent the inputs/operands.
                # A value of "" means there is no value expected.
                # A value of "r" means that a register is expected.
                # A value of "i" means that an immediate is expected.
                # A value of "u" means that an upper immediate is expected.
                # A value of "b" means that a branch immediate is expected.
                # A value of "j" means that a jump immediate is expected.
                # A value of "f" means that a float register is expected.
                # A value of "rm" means that a three bit field to specify rounding
                # A value of "aqrl" means a two bit field used to specify type of atomicity in atomic instructions
                operands_expected = [];
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
                        operands_expected.append("r")
                        operands_expected.append("r")
                        operands_expected.append("r")
                        rtype = True
                    elif instruction_operation in ["LB", "LH", "LW", "LBU", "ADDI", "SLTI", "SLTIU", "XORI", "ORI", "ANDI",]:
                        # This block is for I type instructions
                        operands_expected.append("r")
                        operands_expected.append("r")
                        operands_expected.append("i")
                        itype = True

                    elif instruction_operation in ["SW", "SH", "SB"]:
                        # This block is for S type instrucions
                        operands_expected.append("r")
                        operands_expected.append("r")
                        operands_expected.append("i")
                        stype = True

                    elif (opcode == 0b0110111 or opcode == 0b0010111):
                        # This block is for U type instructions
                        operands_expected.append("r")
                        operands_expected.append("u")
                        utype = True

                   

                    elif instruction_operation in ["BEQ", "BNE", "BLT", "BGE", "BLTU", "BGEU"]:
                        # This block is for B type instructions
                        operands_expected.append("r")
                        operands_expected.append("r")
                        operands_expected.append("b")
                        btype = True

                    elif instruction_operation == "JAL":
                        # This block is for J type instructions
                        operands_expected.append("r")
                        operands_expected.append("j")
                        jtype = True

                elif instruction_operation in opcode_table_multiply_standard:
                    function3 = function3_code_table_multiply_standard.get(instruction_operation)
                    # All multiply instructions have the same function7 value.
                    function7 = 0B0000001

                    operands_expected = "r" * 3
                    rtype = True

                elif instruction_operation in opcode_table_atomic_standard:
                    # All atomic instructions have the same function3 value
                    function3 = 0B010
                    # All atomic instructions do not have a function7 value. They each instead have a function5 field
                    # followed by two bits, aq for acquire access and rl for release access.
                    function5 = function5_code_table_atomic_standard.get(instruction_operation)
                    
                    operands_expected.append("r")
                    operands_expected.append("r")

                    if instruction_operation != "LR.W":
                        operands_expected.append("r")
                    operands_expected.append("aqrl")
                    rtype = True


                elif instruction_operation in opcode_table_float_standard:
                    function3 = function3_code_table_float_standard.get(instruction_operation)
                    function7 = function7_code_table_float_standard.get(instruction_operation)

                    if instruction_operation == "FLW" or instruction_operation == "FSW":
                        # This block is for I type instructions and S type instructions
                        # FLW is an I type and FSW is the S type instruction
                        operands_expected.append("f")
                        operands_expected.append("r")
                        if instruction_operation == "FSW":
                            operands_expected.append("s")
                            stype = True
                        else:
                            operands_expected.append("i")
                            itype = True
                    
                    elif instruction_operation in ["FMADD.S", "FMSUB.S", "FNMSUB.S", "FNMADD.S"]:
                        # This block is for R4 type instructions
                        operands_expected = ["f"] * 4
                        r4type = True

                    elif instruction_operation in ["FCVT.WU.S", "FCVT.W.S", "FMV.X.W", "FCLASS.S"]:
                        # Floating point to integer destination
                        operands_expected = ["r", "f"]
                        rtype=True
                    elif instruction_operation in ["FCVT.S.W", "FMV.W.X", "FCVT.S.WU"]:
                        # Integer to floating point destination
                        operands_expected = ["f", "r"]
                        rtype = True

                    elif instruction_operation == "FSQRT.S":
                        operands_expected = ["f"] * 2
                        rtype=True

                    elif instruction_operation in [ "FADD.S", "FSUB.S", "FMUL.S", "FDIV.S", "FSGNJ.S", "FSGNJN.S", "FSGNJX.S", "FMIN.S", "FMAX.S"]:
                        operands_expected = ["f"] * 3
                        rtype = True

                    elif instruction_operation in ["FEQ.S", "FLT.S", "FLE.S"]:
                        operands_expected = ["r", "f", "f"]
                        rtype = True

                    if not function3:
                        operands_expected.append("rm")
                                 
                else:
                    raise Exception(instruction_operation, "found in opcode table, but not in any of its component dictionaries.")


                # The line isolates the operands from the instruction_operation.
                # We get the string of the instruction starting just after the space that follows the instruction_operation.
                # Since the comment and any label have already been removed, that leaves us with just the operands separated by commas.
                # We do not use replace because that could mess with the labels.
                operands = source_line[len(instruction_operation)+1::]
                # We need to convert the string of comma separated operands into a list of strings, one element per operand.
                operands = operands.split(",")
                # Now it would make sense to remove any whitespace preceding or trailing the operands.
                operands = [ re.sub("^\s*", "", operand) for operand in operands ]
                operands = [ re.sub("\s*$", "", operand) for operand in operands ]

                # Now we have the operands preprocessed the way we would like them.
                # Now would be a good time to confirm that we have the right amount of operands.
                # Usually, assemblers allow a programmer to omit an operand if the first opreand is the same as the destination.
                # We don't do this because it's not necessary.
                num_operands_given = len(operands)
                num_operands_expected = len(operands_expected)
                
                
                if num_operands_given != num_operands_expected:
                    throw_syntax_error(operands_expected, line, instruction_operation)

                string_address = ""
                operands = decode_operands(operands_expected, operands)
                if operands == "throw_syntax_error":
                    throw_syntax_error(operands_expected, line, instruction_operation)
                elif operands == "immediate_too_big":
                    raise Exception(f"immediate_too_big on line {line}")
                elif operands == "upper_immediate_too_precise":
                    raise Exception(f"upper_immediate_too_precise on line {line}\nThe least significant bit should be 0.")
                elif operands == "invalid_rm_flags":
                    raise Exception(f"invalid_rm_flags on line {line}\nFlags must be 3 binary characters, either 0 or 1")
                elif operands == "invalid_aqrl_flags":
                    raise Exception(f"invalid_aqrl_flags on line {line}\nFlags must be 2 binary characters, either 0 or 1")
                elif operands == "bad_branch_immediate":
                    raise Exception(f"bad_branch_immediate on line {line}\nImmediate must be label or even num in range 0 to 4094")
                elif operands == "bad_jump_immediate":
                    raise Exception(f"bad_jump_immediate on line {line}\nImmediate must be label or even num in range 0 to 2^20-2")
                elif operands == []:
                    # This is the case for system calls I think. Check riscv 2.2 pdf
                    pass
                elif re.search("\w", operands[-1]):
                    # This is the case for a lable and thus a relocatable instruction.
                    # We have to get a string table address
                    string_address = string_table.give_string_table_location(operands[-1])
                

                # This variable will be written to the output file, f2
                # We will be editing it below.
                instruction_to_write = ""
                flags = 0 # flags will be a single byte
                if itype:
                    flags += 1
                elif utype:
                    flags += 2
                elif stype:
                    flags += 3
                elif btype:
                    flags += 4
                elif jtype:
                    flags += 5
                else:
                    raise Exception("Relocation Flags Problem. Blow Me!")

                
                # Now that we have all of the operands, we can build the instructions
                if instruction_operation in opcode_table_base_integer:
                    instruction_to_write += opcode
                    # This block is only for base integer instructions

                    if "C" in instruction_operation:
                        raise Exception("No compressed shit, boy")

                    if rtype:
                        instruction_to_write += operands[0]
                        instruction_to_write += function3
                        instruction_to_write += operands[1]
                        instruction_to_write += operands[2]
                        instruction_to_write += function7

                        # have to resolve shamt field
                        pass
                    elif itype:
                        instruction_to_write += operands[0]
                        instruction_to_write += function3
                        instruction_to_write += operands[1]
                        instruction_to_write += operands[2]
                    elif stype:
                        instruction_to_write += operands[2][0:5]
                        instruction_to_write += function3
                        instruction_to_write += operands[0]
                        instruction_to_write += operands[1]
                        instruction_to_write += operands[2][5:]
                    elif btype:
                        instruction_to_write += operands[2][-2] + operands[2][0:4]
                        instruction_to_write += function3
                        instruction_to_write += operands[0]
                        instruction_to_write += operands[1]
                        instruction_to_write += operands[2][4:-2] + operands[2][-1]
                    elif utype:
                        # This is either LUI or AUIPC
                        instruction_to_write += operands[1]
                        instruction_to_write += operands[0]
                    elif jtype:
                        instruction_to_write += operands[0]
                        instruction_to_write += function3
                        instruction_to_write += operands[1][11:19] + operands[1][10] + operands[1][0:10] + operands[1][19]


                else:
                    raise Exception("We are only doing base integer instructions thus far. 810w m3!")

                if string_address != "":
                    # This indicates that the last 3 bits in the flags byte have relevance.
                    flags += 8
                    
                    # We need to add 4 to the location counter to account for the 4 bytes used in the string address
                    # The string address will be directly after the instruction. The flag byte will come directly before the instruction
                    location_counter += 4

                # If the instruction is not relocatable
                else:
                   pass

                # Format of bytes_to_write:
                # flags|32 bit machine code|string_address
                bytes_to_write = chr(flags)
                instruction_to_write += string_address
                #instruction_to_write = flags + instruction_to_write
                
                assert len(instruction_to_write) % 8 == 0
                for byte in range(0, len(instruction_to_write), 8):
                    bytes_to_write += chr( instruction_to_write[byte:byte+8] )

                

                # At this point, we should write the bytes to the file.
                section.text += bytes_to_write
                pass




                # If there was assembly on this line, then we will add 4 to the value of the location counter.
                # We do this to determine where the next instruction would go in memory. Each instruction is 4 bytes long. The flag byte on the end makes it 5 total.
                # If the instruction was relocatable, the 4 bytes for the string_address were already added above.
                location_counter += 5

                # This is a 32 bit risc-v assembler. All instructions will be the same length of 4 bytes, one word.
                # That means that we do not have to look at the operator or operands to determine the length of the instruction. 
                # We simply increment the location counter by 4 (referring to 4 bytes) when finding the location where
                # the next instruction will be stored. We instead increment by 5 because we need a byte for flags needed to indicate whether or not
                # the instruction is relocatable. We will have 4 spare bits for other things in the future.
                # We will increment 4 more bytes if the instruction is in fact relocatable. We need these last four bytes for the string table address of the label.
                # That string will be used to find a symbol table definition when the program is statically linked or dynamically loaded.
            
            elif uninitialized_section:
                data_type = source_line.split()[0].lower()
                if data_type in ["w", "word"]:
                    num_bytes = 4
                elif data_type in ["h", "half"]:
                    num_bytes = 2
                elif data_type in ["b", "byte", "c", "char"]:
                    num_bytes = 1
                elif re.search("^\d*$", data_type):
                    num_bytes = int(data_type)
                else:
                    raise Exception(f"Bitch, I don't know what kind of data you want on line {line}. Just give a data size, either w, h, b, c, or a decimal number of bytes.")
                
                # We do not write any bytes to the uninitialized section. The values would all be 0 anyways. 
                # The section header indicates the size of this section while indicating that there are no bytes written.
                #bytes_to_write = "\x00" * num_bytes
                #section.text += bytes_to_write
                location_counter += num_bytes


            # This block is for creating data sections.
            # We are being picky bastards because why the fuck would a programmer want to put data directly into an executable section.
            # And why the fuck would a programmer want instructions assembled and stored in the data section.
            # Even if they wanted to directly specify the instruction that they were calling, the format of the machine code instructions
            # would probably not match the format required for the ABI. There would be hard-to-debug issues when linking or loading.
            # And if there were assembled instructions in the data sections, it would be assembled in the format needed by the linker or loader, not direct machine code. 
            # No relocation would take place, and flags would mess up execution of the instruction.
            else:
                flags = 0
                data_type = source_line.split()[0].lower()
                if data_type in ["w", "word"]:
                    num_bytes = 4
                    # We do nothing to the flags when it is just a word.
                    pass
                elif data_type in ["h", "half"]:
                    num_bytes = 2
                    flags += 6
                elif data_type in ["b", "byte", "c", "char"]:
                    num_bytes = 1
                    flags += 7
                elif data_type == "s":
                    num_bytes = 0
                    flags += 16

                data = source_line.lstrip(data_type)
                data = data.lstrip()
                decoded_data = decode_data(data, False, num_bytes)

                bytes_to_write = ""
                if data_type == "s":
                    assert len(data) < 2 ** 32
                    num_bytes = len(data)
                    if num_bytes < 8:
                        flags += num_bytes << 5
                        string_length = ""
                    else:
                        string_length = chr(num_bytes)
                    bytes_to_write = chr(flags) + string_length + decoded_data
                else:
                    bytes_to_write = chr(flags) + decoded_data

                # write bytes to file
                section.text += bytes_to_write

                pass




            line += 1
        # Finish off the last section header
        # finish off some of the fields from the previous section
        section.sh_size = location_counter
        assert location_counter == len(section.text)
        section.sh_addr = 0
        # section.sh_offset will be set once all of the sections have been made and the string table, section headers, and symbol table have definite sizes.
        section.sh_link = 0
        section.sh_info = 0
        section.sh_addralign = 0
        section.sh_entsize = 0
        sections.append(section)
        

        # Now we can FUCKING FINALLY build the FUCKING section headers and the FUCKING elf header
        # We will start with the section headers. We just have to finish up the symbol table and string table headers.
        # And then the offset field for all of the users defined sections. And that's FUCKING it.
        string_table.sh_offset = 52 + 40 * 2 + 40 * len(sections)
        string_table.sh_link = 0
        string_table.sh_info = 0
        string_table.sh_addralign = 0
        string_table.sh_entsize = 0
        string_table_header = string_table.write_header_attributes_to_string()

        symbol_table.sh_offset = string_table.offset + len(string_table.section_data)
        symbol_table.sh_link = 0
        symbol_table.sh_info = 0
        symbol_table.sh_addralign = 0
        symbol_table.sh_entsize = 16
        symbol_table.sh_size = symbol_table.calculate_section_size()
        symbol_table_header = symbol_table.write_header_attributes_to_string()


        section_headers_string = string_table_header + symbol_table_header
        section_strings_combined = string_table.section_data + symbol_table.write_to_string()

        current_offset = symbol_table.sh_offset
        for section in sections:
            section.sh_offset = current_offset
            if section.sh_type != 8:
                current_offset += section.sh_size
            # If the section is a NOBITS section, then don't add onto the current_offset
            section_headers += section.write_header_attributes_to_string()
            section_strings_combined += section.text

        assert len(section_headers) + 52 == string_table.sh_offset
        assert len(sections) + 2 == e_shnum

        # Now we just have to make the elf header.
        e_ident = ELFHeader.create_e_ident()
        e_shoff = 52
        e_shentsize = 40
        e_shstrndx = 0 # The string index is the first section header listed
        elf_header = ELFHeader(e_ident=e_ident, e_shoff=e_shoff, e_shentsize=e_shentsize, e_shstrndx=e_shstrndx)
        elf_header_string = elf_header.write_to_string()
        
        
        buffer_to_write = elf_header_string + section_headers_string + section_strings_combined

    return buffer_to_write


def second_pass_executable_section(file_descriptor, symbol_table):
    pass







def second_pass(input_file, symbol_table):
    '''
        This is where we actually build the object file using the source code and the symbol table that was generated by the first pass
    '''

    location_counter = 0

    with open(input_file) as f:
        while True:
            source_line = f.readline()
            
            # Determine if this is the last line of the program
            # If there is no more to a file, readline will return an empty string, which is Falsey
            if not source_line:
                break

            # Remove any whitespace at the beginning of the source line
            source_line = source_line.lstrip()

            # If the line has a comment, then store it in the comment variable
            comment = re.search(";.*$", source_line)
            if comment:
                comment = comment.group(0)
                # Remove any comment from the source line        
                source_line = source_line.replace( comment, "" )


            # If there was nothing assembleable on this line, then we will not increment the location counter by a word (4 bytes)
            # We will also not read for instructions, since that would otherwise throw errors
            if re.search("^\W*\n$", source_line):
                line += 1
                continue









            location_counter += 4

    # Create an ELF Header
    # First, make the e_ident property
    e_ident = ELFHeader.create_e_ident()
    # If we are just making a relocatable object file, then we don't need a program header. So e_phoff will be 0
    # The property that will need to eventually be changeable will be e_type. We will need to make executable files at some point.
    elfheader = ELFHeader(e_ident)
    


if __name__ == "__main__":
    assert len(sys.argv) == 2 
    assert not sys.argv[1].endswith("py")
    # need to make an elf header
    # need to make a section header table
    # need to make sections
    os.mkdir("tmpAssembly")
    with open("./tmpAssembly/tmpInput", "w") as f:
        f.write( sys.stdin.read() )
    
    def remove_tmp():
        os.system("rm -r tmpAssembly")
        
    try:
        output_text = first_pass("tmpAssembly/tmpInput")
    except Exception as e:
        remove_tmp()
        raise e
    with open(sys.argv[1], "w") as f:
        f.write(output_text)

    print("Symbol Table:", symbol_table)
    print()
    
    remove_tmp()
