from elfsectionheader import ELFSection

class SymbolTable(ELFSection):
    
    def __init__(self, sh_name=".SymbolTable"):
        self.sh_name = sh_name
        self.sh_type = 2 # 2 corresponds to symbol table
        self.sh_flags = 0
        self.addr = 0
        self.entries = [] # This will be a list of SymbolTableEntry objects

    def add_symbol_table_entry(self, entry):
        assert type(entry) == SymbolTableEntry
        self.entries.append(entry)

    def check_for_symbol_by_name(self, symbol_name):
        '''This function returns a boolean value. It accepts an integer parameter. If that integer is the st_name value for any of the entries in the symbol table,
        then True is returned. Otherwise, False is returned.
        '''
        for entry in self.entries:
            if entry.st_name == symbol_name:
                return True
        return False
    
    def calculate_section_size(self):
        return len(self.entries) * 16

    def write_to_string(self):
        buffer_to_write = ""
        for entry in self.entries:
            assert type(entry) == SymbolTableEntry
            buffer_to_write += entry.write_to_string()
        return buffer_to_write


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

    def write_to_string(self):

        buffer_to_write = b""
        buffer_to_write += self.st_name.to_bytes(4,"little")
        buffer_to_write += self.st_value.to_bytes(4,"little")
        buffer_to_write += self.st_size.to_bytes(4,"little")
        buffer_to_write += self.st_info.to_bytes(1,"little")
        buffer_to_write += self.st_other.to_bytes(1,"little")
        buffer_to_write += self.st_shndx.to_bytes(2,"little")
        
        return buffer_to_write.decode()
