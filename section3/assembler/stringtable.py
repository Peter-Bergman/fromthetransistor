from elfsectionheader import ELFSection

class StringTable():

    def __init__(self):
        self.sh_type = 3 # 3 corresponsds to string table section
        self.section_data = "\x00"
        self.sh_size = len(self.section_data)
        self.addr = 0

    def add_string_data(self, string_data):
        self.section_data += string_data
        self.sh_size = len(self.section_data)
    
    def give_string_table_location(self, string):
        '''Needs to return 32 bit integer converted to length 4 string and updated string_table. If the string does not exist in the string table, we will append it to the end of the string table.'''
        string += "\x00"
        if string in self.section_data:
            index = self.section_data.index(string)
        else:
            index = len(self.section_data)
            self.add_string_data(string)
        assert index < 2 ** 32
        index_string = index.to_bytes(4, "little").decode()
        return index_string



