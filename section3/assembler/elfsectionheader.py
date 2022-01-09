class ELFSectionHeader():
    # Relocatable files will have elf section headers. Yes, plural.
    # Each section has its own section header. The number of sections is specified in the elf header
    # The elf section header(s) are essentially an array of elf section header entries
    # This class is intended to represent an entry in that array.
    # See https://docs.oracle.com/cd/E19455-01/806-3773/elf-2/index.html for documentation

    def __init__(self, sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size, sh_link, sh_info, sh_addralign, sh_entsize):

        # This member specifies the name of the section. Its value is an index into the section header string table section, giving the location of a null-terminated string.
        self.sh_name = sh_name

        # This member categorizes the sections contents and semantics. 
        self.sh_type = sh_type
        
        # Sections support 1 bit flags that describe miscellaneous attributes. Flag definitions: http://www.sco.com/developers/gabi/latest/ch4.sheader.html#sh_flags
        self.sh_flags = sh_flags

        # If the section will appear in the memory image of a process, this member gives the address at which the section's first byte should reside. Otherwise, the member contains 0.
        self.sh_addr = sh_addr

        # This member's value gives the byte offset from the beginning of the file to the first byte in the section. One section type, SHT_NOBITS, occupies no space in the file and its sh_offset member locates the conceptual location in the file.
        self.sh_offset = sh_offset

        # This member's value gives the section's size in bytes. Unless the section's type is SHT_NOBITS, the section occupies sh_size bytes in the file. A section type of SHT_NOBITS may have a non-zero size, but it occupies no space in the file.
        self.sh_size = sh_size

        # This member holds a section header table index link, whose interpretation depends on the section type. 
        self.sh_link = sh_link

        # This member holds extra information, whose interpretation depends on the section type. If the sh_flags field for this section header includes the attribute SHF_INFO_LINK, then this member represents a section header table index. 
        self.sh_info = sh_info

        # Some sections have address alignment constraints. For example, if a section holds a doubleword, the system must ensure doubleword alignment for the entire section. The value of sh_addr must be congruent to 0, modulo the value of sh_addralign. Currently, only 0 and positive integral powers of two are allowed. Values 0 and 1 mean the section has no alignment constraints. 
        self.sh_addralign = sh_addralign

        # Some sections hold a table of fixed-size entries, such as a symbol table. For such a section, this member gives the size in bytes of each entry. The member contains 0 if the section does not hold a table of fixed-size entries. 
        self.sh_entsize = sh_entsize


class ELFSection():

    def __init__(self, sh_name, sh_type, sh_flags, text):
        self.sh_name = sh_name
        self.sh_type = sh_type
        self.sh_flags = sh_flags
        self.text = text

    def write_header_attributes_to_string(self):

        bytes_to_write = ""
        bytes_to_write += self.sh_name.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_type.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_flags.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_addr.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_offset.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_size.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_link.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_info.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_addralign.to_bytes(4,"little").decode()
        bytes_to_write += self.sh_entsize.to_bytes(4,"little").decode()

        return bytes_to_write
