def decode_operands(operands_expected, operands):
    decoded_operands = []

    for i in range(len(operands_expected)):
        operand = operands[i]
        if operands_expected[i] == "r":
            
            operand = integer_register_names.get(operand)
            if not operand:
                return "throw_syntax_error"
            operand_binary_string = bin(operand).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            operand_binary_string = "0" * (5-len(operand_binary_string)) + operand_binary_string # We need 5 bits for to indicate the register
            decoded_operands.append(operand_binary_string)

        elif operands_expected[i] == "i":

            # First we see if the operand is a constant number
            if re.search("^\d*$", operand):
                operand_binary_string = bin( int(operand) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns  
            elif re.search( "^0x(\d|[a-f])*$", operand.lower() ):
                operand_binary_string = bin( int(operand, 16) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0o(0|1|2|3|4|5|6|7)*$", operand.lower()):
                operand_binary_string = bin( int(operand, 8) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0b(0|1)*$", operand.lower()):
                operand_binary_string = operand.replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            # Then we check for a label passed as the operand.
            elif re.search("^\.?\w*$"):

                operand_binary_string = "0"
                #string_address = give_string_table_location(operand)
                label_operand = operand
            else:
                raise Exception(f"The following operand did not match any immediate parsed style:\n{operand}")


            if len(operand_binary_string) > 12:
                return "immediate_too_big"
            operand_binary_string = "0" * (12-len(operand_binary_string)) + operand_binary_string
            decoded_operands.append(operand_binary_string)


        elif operands_expected[i] == "u":
            # First we see if the operand is a constant number
            if re.search("^\d*$", operand):
                operand_binary_string = bin( int(operand) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns  
            elif re.search( "^0x(\d|[a-f])*$", operand.lower() ):
                operand_binary_string = bin( int(operand, 16) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0o(0|1|2|3|4|5|6|7)*$", operand.lower()):
                operand_binary_string = bin( int(operand, 8) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0b(0|1)*$", operand.lower()):
                operand_binary_string = operand.replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^\.?\w*$"):
                operand_binary_string = "0"
                #string_address = give_string_table_location(operand)
                label_operand = operand
            else:
                raise Exception(f"The following operand did not match any upper immediate parsed style:\n{operand}")
            
            if operand_binary_string != "0" and operand_binary_string[-12:] != "0" * 12:
                return "upper_immediate_too_precise"
            operand_binary_string = "0" * (32-len(operand_binary_string)) + operand_binary_string
            operand_binary_string = operand_binary_string[:20]
            decoded_operands.append(operand_binary_string)


        elif operands_expected[i] == "f":

            operand = fp_register_names.get(operand)
            if not operand:
                return "throw_syntax_error"
            operand_binary_string = bin(operand).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            operand_binary_string = "0" * (5-len(operand_binary_string)) + operand_binary_string # We need 5 bits for to indicate the register
            decoded_operands.append(operand_binary_string)

        elif operands_expected[i] == "rm":
            if re.search("^(0b)?(0|1){3}", operand):
                operand_binary_string = operand.replace("0b","")
            else:
                return "invalid_rm_flags"
            decoded_operands.append(operand_binary_string)

        elif operands_expected[i] == "aqrl":
            if re.search("^(0b)?(0|1){2}", operand):
                operand_binary_string = operand.replace("0b","")
            else:
                return "invalid_aq_rl_flags"
            decoded_operands.append(operand_binary_string)

        elif operands_expected[i] == "b":
            # First we see if the operand is a constant number
            if re.search("^\d*$", operand):
                operand_binary_string = bin( int(operand) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns  
            elif re.search( "^0x(\d|[a-f])*$", operand.lower() ):
                operand_binary_string = bin( int(operand, 16) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0o(0|1|2|3|4|5|6|7)*$", operand.lower()):
                operand_binary_string = bin( int(operand, 8) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0b(0|1)*$", operand.lower()):
                operand_binary_string = operand.replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^\.?\w*$"):
                operand_binary_string = "0"
                #string_address = give_string_table_location(operand)
                label_operand = operand
            else:
                raise Exception(f"The following operand did not match any immediate parsed style:\n{operand}")
            
            if operand_binary_string[-1] != 0 or len(operand_binary_string) > 13:
                return "bad_branch_immediate"
            operand_binary_string = "0" * (13-len(operand_binary_string)) + operand_binary_string
            operand_binary_string = operand_binary_string[:-1]
            decoded_operands.append(operand_binary_string)

        elif operands_expected[i] == "j":
            # First we see if the operand is a constant number
            if re.search("^\d*$", operand):
                operand_binary_string = bin( int(operand) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns  
            elif re.search( "^0x(\d|[a-f])*$", operand.lower() ):
                operand_binary_string = bin( int(operand, 16) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0o(0|1|2|3|4|5|6|7)*$", operand.lower()):
                operand_binary_string = bin( int(operand, 8) ).replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^0b(0|1)*$", operand.lower()):
                operand_binary_string = operand.replace("0b","") # We have to get rid of the 0b prefix that the bin function returns
            elif re.search("^\.?\w*$"):
                operand_binary_string = "0"
                #string_address = give_string_table_location(operand)
                label_operand = operand
            else:
                raise Exception(f"The following operand did not match any jump immediate parsed style:\n{operand}")

            if operand_binary_string[-1] != 0 or len(operand_binary_string) > 21:
                return "bad_jump_immediate"
            operand_binary_string = "0" * (21-len(operand_binary_string)) + operand_binary_string
            operand_binary_string = operand_binary_string[:-1]
            decoded_operands.append(operand_binary_string)

        else:
            raise Exception("Can't interpret operand type", operands_expected[i])
        

        if "label_operand" in locals():
            decoded_operands.append(label_operand)
        return decoded_operands
