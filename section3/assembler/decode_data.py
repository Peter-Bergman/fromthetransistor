import re

def decode_data(data_string, signed, num_bytes):

    # 0 corresponds to unknown length. This would be a string.
    assert num_bytes >= 0
    has_negative_sign = data_string.startswith("-")
    return_string = ""
    int_value = 0

    if has_negative_sign:
        if not signed:
            return ["negative_value_for_unsigned_field"]
        data_string = data_string.lstrip("-")

    # if the value is hex encoded
    if re.search("^-?0x(\d|[A-F]|[a-f])+$", data_string):
        int_value = int(data_string, 16)
    # if the data_string is octal encoded
    elif re.search("^-?0o[0-7]+$"):
        int_value = int(data_string, 8)
    # if the data_string is decimal encoded
    elif re.search("^-?\d+$"):
        int_value = int(data_string)
    # if the data is binary encoded
    elif re.search("^-?0b[0|1]+$", data_string):
        int_value = int(data_string, 2)
    # if the data is a string
    elif (data_string.startswith("\"") and endswith("\"")) or (data_string.startswith("\'") and data_string.endswith("\'")):
        # We do not need an intermediate int value when the data is a string. 
        # Either way, integers will have to be written to a file as strings.
        return_string = data_string[1:-1] # remove the quotes from the string literal

        if num_bytes != 0 and len(return_string) != num_bytes:

            if len(return_string) < num_bytes:
                return_string += "0" * (num_bytess - len(return_string))
            raise Exception("Expected a char, got a string")

        return return_string
    else:
        return ["error"]

    # Make sure that the number will fit in the designated number of bytes.
    if ( 256 ** num_bytes / 2 < int_value and signed ) or 256 ** num_bytes <= int_value:
        raise Exception("Integer overflow.")

    for byte in range(num_bytes):
        character = chr(int_value % 256)
        return_string += character
        int_value = int_value >> 8

    return return_string
