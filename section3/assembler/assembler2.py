#!/bin/python3

import os
import re
import sys

from elfheader import ELFHeader
from elfsectionheader import ELFSection
from stringtable import StringTable
from symboltable import SymbolTable, SymbolTableEntry

from typing import List

# This assembler is for 32-bit RISC-V assembly.

class AssemblyInstructionOrDirective():
    @staticmethod
    def parse(string_to_parse: str):
        parsed_directive = AssemblyDirective.parse(string_to_parse)
        parsed_instruction = AssemblyInstruction.parse(string_to_parse)
        parse_result = parsed_directive or parsed_instruction or None
        return parse_result

    def serialize(self) -> bytes:
        return NotImplemented

class AssemblyDirective(AssemblyInstructionOrDirective):
    @staticmethod
    def parse(string_to_parse: str):
        parsed_string_directive = StringDirective.parse(string_to_parse)
        parsed_int_directive = IntDirective.parse(string_to_parse)

        parse_result = parsed_string_directive or parsed_int_directive or None
        return parse_result

    def serialize(self) -> bytes:
        return NotImplemented

class StringDirective(AssemblyDirective):
    def __init__(self, string_contents) -> None:
        self.string_contents = string_contents

    @staticmethod
    def parse(string_to_parse: str):
        forbidden_characters = "|".join(['"', '\\', '\n'])
        non_escape_sequence_s_char = f"[^(?:{forbidden_characters})]"

        s_char = "|".join([non_escape_sequence_s_char])
        string_characters = f"(?P<string_contents>{s_char}*)"
        parsed_string_directive = re.fullmatch(f'\\.string \"{string_characters}\"', string_to_parse)
        parse_result = StringDirective(
            parsed_string_directive.groupdict()['string_contents']
        ) if parsed_string_directive else None
        return parse_result

    def serialize(self) -> bytes:
        bytes_to_return = bytes(self.string_contents, "ascii")
        return bytes_to_return

class IntDirective(AssemblyDirective):
    def __init__(self, integer: int) -> None:
        self.integer = integer

    @staticmethod
    def parse(string_to_parse: str):
        parsed_int_directive = re.fullmatch(r'\.int (?P<integer>-?\d+)', string_to_parse)
        parse_result = IntDirective(
            int(parsed_int_directive.groupdict()['integer'])
        ) if parsed_int_directive else None
        return parse_result

    def serialize(self) -> bytes:
        bytes_to_return = self.integer.to_bytes(length=4, byteorder='little', signed=True)
        assert \
            int.from_bytes(bytes_to_return, byteorder='little', signed=True) == \
            self.integer
        return bytes_to_return

class AssemblyInstruction(AssemblyInstructionOrDirective):
    @staticmethod
    def parse(string_to_parse: str):
        pass

    def serialize(self) -> bytes:
        return NotImplemented

AbstractAssemblyFile = List[AssemblyInstructionOrDirective]

def parse_assembly_file(assembly_file_contents: str) -> AbstractAssemblyFile:
    '''
        returns a list of assembly directives and instructions
    '''
    parsed_lines = []
    split_lines = assembly_file_contents.split("\n")
    for line_number, line in enumerate(split_lines):

        # Remove any whitespace at the beginning of the line
        line = re.sub(r"^\s*", "", line)
        # handle blank lines
        if len(line) == 0:
            continue

        assembly_instruction_or_directive = AssemblyInstructionOrDirective.parse(line)
        if assembly_instruction_or_directive:
            parsed_lines.append(assembly_instruction_or_directive)
        else:
            raise Exception(f"Syntax error on line {line_number}")
    return parsed_lines

def serialize_abstract_assembly_as_memory_contents(
        abstract_assembly: AbstractAssemblyFile,
    ) -> bytes:
    serialized_instructions_and_directives = [
        assembly_instruction_or_directive.serialize() for
        assembly_instruction_or_directive in abstract_assembly
    ]
    loadable_memory_contents = b''.join(serialized_instructions_and_directives)
    return loadable_memory_contents

def assemble_string(string_to_assemble: str) -> bytes:
    abstract_assembly = parse_assembly_file(string_to_assemble)
    assembled_string = serialize_abstract_assembly_as_memory_contents(abstract_assembly)
    return assembled_string

def main():
    standard_input = sys.stdin.read()
    assembler_output = assemble_string(standard_input)
    sys.stdout.buffer.write(assembler_output)
    return 0

sample1 = '''.int 17'''

sample2 = '''.string "asdf"'''

sample3 = f"{sample1}\n{sample2}"

sample4 = f"\n{sample3}"

if __name__ == "__main__":
    main()

