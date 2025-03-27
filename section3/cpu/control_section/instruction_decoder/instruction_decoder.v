module instruction_decoder(
	instruction,

	register1,
	register2,
	csr_register,
	immediate,
	store_immediate,
    branch_immediate,
    upper_immediate,
    jump_immediate,
    csr_immediate,
	registerd,
	op_code,
	func3,
	func7,
	read,
	mem_write,
	reg_write,
    csr_immediate_instruction,
	external_call,
	external_break

	// Instruction types include (register, immediate, upper), jump,
	// branch, store, and load.
	// Maybe store and load
);
	
	// ports
	input [31:0]		    instruction;

	output logic [4:0]		register1,
                            // shamt, or shift amount, is the same as the register2 value
				            register2,
				            registerd;
	output logic [11:0]		csr_register;
	output logic [6:0]      op_code,
				            func7;
	output logic [2:0]      func3;
	output logic [31:0]     immediate,
				            store_immediate,
				            branch_immediate,
				            upper_immediate,
				            jump_immediate,
				            csr_immediate;
	output logic [1:0]      read,
				            mem_write,
				            reg_write;
	output logic            csr_immediate_instruction,
				            external_call,
				            external_break;

		
	// internals
	logic		system_instruction,

				csr_instruction,
                csr_write,
                csr_set,
                csr_clear,
                csr_write_immediate,
                csr_set_immediate,
                csr_clear_immediate,

                external_call_or_breakpoint,
				external_instruction,
				branch,
				store,
                load,
                load_byte,
                load_half,
                load_word,
                load_byte_unsigned,
                load_half_unsigned,
                fence;


	// logic

	// We can use an always_comb block to determine if there is an invalid
	// instruction exception.
    always_comb begin

        register1 = instruction [19:15];
        register2 = instruction [24:20];
        registerd = instruction [11:7];
        csr_register = instruction[31:20];

        register1 = instruction [19:15];
        register2 = instruction [24:20];
        registerd = instruction [11:7];
        csr_register = instruction[31:20];

        immediate = 32'(signed'( instruction [31:20] ));
        store_immediate = 32'(signed'( { instruction [31:25], instruction [11:7] } ));
        branch_immediate = 32'(signed'( { instruction [31], instruction [7], instruction [30:25], instruction [11:8], 1'b0 } )); // Going to adjust for instruction alignment in ALU
        upper_immediate = { instruction [31:12], 12'b0 };
        jump_immediate = 32'(signed'( { instruction [31], instruction [19:12], instruction [20], instruction [30:21], 1'b0 } ));
        csr_immediate = 32'(register1);

        op_code = instruction [6:0];
        func3 = instruction [14:12];
        func7 = instruction [31:25];

        branch = op_code == 7'b1100011;
        store = op_code == 7'b0100011;
        fence = op_code == 7'b0001111;

        system_instruction = op_code == 7'b1110011;
        csr_instruction = system_instruction && |func3;
        external_call_or_breakpoint = system_instruction && ~|func3;
        csr_immediate_instruction = csr_instruction && func3[2] == 1'b1;
        external_instruction = system_instruction && !csr_instruction;

        external_call = external_call_or_breakpoint && ~|func7;
        external_break = external_call_or_breakpoint && func7 == 1;

        load = op_code == 7'b0000011;
        load_byte = load & func3 == 0;
        load_half = load & func3 == 1;
        load_word = load & func3 == 2;
        load_byte_unsigned = load & func3 == 4;
        load_half_unsigned = load & func3 == 5;

        read = (op_code == 7'b0000011)
            ? (func3 == 0 || func3 == 3'b100
                ? 2'b01
                : (func3 == 1 || func3 == 3'b101
                    ? 2'b10
                    : (func3 == 3'b010
                        ? 2'b11
                        : 2'b0
                    )
                )
            )
            : 2'b0;
        mem_write = store ? (func3 == 3'b0 ? 2'b01 : (func3 == 3'b001 ? 2'b10 : (func3 == 3'b010 ? 2'b10 : 2'b0))) : 2'b0;
        reg_write =
            |(2'(branch) | mem_write | 2'(fence) | 2'(external_call_or_breakpoint)) ? 0 :
            load_byte ? 1 :
            load_half ? 2 :
            load_word | load_byte_unsigned | load_half_unsigned ? 3 : 3;

        csr_write = csr_instruction && func3 == 1;
        csr_set = csr_instruction && func3 == 2;
        csr_clear = csr_instruction && func3 == 3;
        csr_write_immediate = csr_instruction && func3 == 5;
        csr_set_immediate = csr_instruction && func3 == 6;
        csr_clear_immediate = csr_instruction && func3 == 7;

    end // end always_comb

endmodule
