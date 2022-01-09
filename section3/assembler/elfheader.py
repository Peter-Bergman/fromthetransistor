class ELFHeader():

    def __init__(self, e_ident, e_type=1, e_entry=0, e_phoff=0, e_shoff=0, e_flags=0, e_ehsize=0, e_phentsize=0, e_phnum=0, e_shentsize=0, e_shnum=0, e_shstrndx=0):
        self.e_ident = e_ident[:17]
        self.e_type = e_type # Type of elf file. default of 1 is relocatable file. 2 is executable. 3 is shared object file.
        self.e_machine = 243 # This specifies RISC-V
        self.e_version = 1 # Word
        self.e_entry = e_entry # Entry point of the program. Elf32_Addr
        self.e_phoff = e_phoff # This field is the program header offset. It specifies program header offset in bytes. Elf32_Off
        self.e_shoff = e_shoff # This member holds the section header table's file offset in bytes from the beginning of the file. If the file has no section header table, this member holds zero. Elf32_Off
        self.e_flags = e_flags # This member holds processor-specific flags associated with the file. It is an Elf32_Word
        self.e_ehsize = e_ehsize # This member holds the Elf header's size in bytes. Half
        self.e_phentsize = e_phentsize # This member holds the size in bytes of one entry in the file's program header table; all entries are the same size.
        self.e_phnum = e_phnum # This member holds the number of entries in the program header table. Thus the product of e_phentsize and e_phnum gives the table's size in bytes. If a file has no program header table, e_phnum holds the value of zero. Half
        self.e_shentsize = e_shentsize # This member holds a section header's size in bytes. A section header is one entry in the section header table; all entries are the same size. Half
        self.e_shnum = e_shnum # This member holds the number of entries in the section header table. Thus the product of e_shentsize and e_shnum gives the section header table's size in bytes. If a file has no section header table, e_shnum holds the value of 0. Half
        self.shstrndx = shstrndx # This member holds the section header table index of the entry associated with the section name string table. If the file has no section name string table, this member holds the value SHN_UNDEF (0). Half


    def write_to_string(self):
        assert len(e_ident) == 16

        bytes_to_write = ""
        bytes_to_write += e_ident
        bytes_to_write += self.e_type.to_bytes(2,"little").decode()
        bytes_to_write += self.e_machine.to_bytes(2, "little").decode()
        bytes_to_write += self.e_version.to_bytes(4,"little").decode()
        bytes_to_write += self.e_entry.to_bytes(4,"little").decode()
        bytes_to_write += self.e_phoff.to_bytes(4,"little").decode()
        bytes_to_write += self.e_shoff.to_bytes(4,"little").decode()
        bytes_to_write += self.e_flags.to_bytes(4,"little").decode()
        bytes_to_write += self.e_ehsize.to_bytes(2,"little").decode()
        bytes_to_write += self.e_phentsize.to_bytes(2,"little").decode()
        bytes_to_write += self.e_phnum.to_bytes(2,"little").decode()
        bytes_to_write += self.e_shentsize.to_bytes(2,"little").decode()
        bytes_to_write += self.e_shnum.to_bytes(2,"little").decode()
        bytes_to_write += self.e_shstrndx.to_bytes(2,"little").decode()
        
        return bytes_to_write


    @staticmethod
    def create_e_ident(
        ei_class = 1, # 1 is for 32 bit, 2 is for 64 bit, this is a byte subfield
        ei_data = 1, # 1 is for little endian, 2 is for big endian
        ei_version = 1, # just set it as 1, don't fuck around, tryna be special
        ei_osabi = 3, # 3 is for linux
        ei_abiversion = 129, # We will just call our implementation of linux version 129 because i don't think 129 exists yet.
        ei_pad = "0000000"
        ):
        
        e_ident = "\x7fELF"
        e_ident += chr(ei_class)
        e_ident += chr(ei_data)
        e_ident += chr(ei_version)
        e_ident += chr(ei_osabi)
        e_ident += chr(ei_abiversion)
        e_ident += ei_pad

        return e_ident
