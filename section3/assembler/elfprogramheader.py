class ELFProgramHeader():

    def __init__(self, p_type, p_offset, p_vaddr, p_paddr, p_filesz, p_memsz, p_flags, p_align):

        self.p_type = p_type
        self.p_offset = p_offset
        self.p_vaddr = self.p_vaddr
        self.p_paddr = p_paddr
        self.p_filesz = p_filesz
        self.p_memsz = p_memsz
        self.p_flags = p_flags
        self.p_align = p_align

    def from_bytes(self):
        pass
