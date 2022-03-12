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
	write,
	reg_write,
	csr_write_back,
	external_call,
	external_break,

	// Instruction types include (register, immediate, upper), jump,
	// branch, store, and load.
	// Maybe store and load
);
	
	// ports
	input [31:0]		instruction;

	output [4:0]		register1,
				register2, // shamt, or shift amount, is the same as the register2 value
				registerd;
	output [11:0]		csr_register;
	output [6:0]		op_code,
				func7;
	output [2:0]		func3;
	output [31:0]		immediate,
				store_immediate,
				branch_immediate,
				upper_immediate,
				jump_immediate,
				csr_immediate;
	output [1:0]		read,
				write,
				reg_write,
				csr_write_back;
	output			csr_immediate_instruction,
				external_call,
				external_break;

		
	// internals
	wire			system_instruction,
				csr_instruction,
				external_instruction,
				branch,
				store;


	// logic
	assign register1 = instruction [19:15];
	assign register2 = instruction [24:20];
	assign registerd = instruction [11:7];
	assign csr_register = instruction[31:20];

	assign immediate = 32'(signed'( instruction [31:20] ));
	assign store_immediate = 32'(signed'( { instruction [31:25], instruction [11:7] } ));
	assign branch_immediate = 32'(signed'( { instruction [31], instruction [7], instruction [30:25], instruction [11:8], 1'b0 } )); // Going to adjust for instruction alignment in ALU
	assign upper_immediate = { instruction [31:12], 12'b0 };
	assign jump_immediate = 32'(signed'( { instruction [31], instruction [19:12], instruction [20], instruction [30:21], 1'b0 } ));
	assign csr_immediate = 32'(register1);

	assign op_code = instruction [6:0];
	assign func3 = instruction [14:12];
	assign func7 = instruction [31:25];

	assign read = (op_code == 7'b0000011) ? (func3 == 3'b0 || func3 == 3'b100 ? 2'b01 : (func3 == 3'b001 || func3 == 3'b101 ? 2'b10 : (func3 == 3'b010 ? 2'b11 : 2'b0))) : 2'b0;
	assign write = store ? (func3 == 3'b0 ? 2'b01 : (func3 == 3'b001 ? 2'b10 : (func3 == 3'b010 ? 2'b010 : 2'b0))) : 2'b0;
	assign reg_write = !branch && !write;

	assign branch = op_code == 7'b0100011;
	assign store = op_code == 7'b0100011;

	assign system_instruction = op_code == 7'b1110011;
	assign csr_instruction = system_instruction && func3;
	assign external_call_or_bp = system_instruction && !func3;
	assign csr_immediate_instruction = csr_instruction && func3[2] == 1'b1;
	assign external_instruction = system_instruction && !csr_instruction;
	assign csr_read = (csr_instruction && registerd) ? 1'b1 : 1'b0;
	assign csr_writeback = (!csr_instruction) ? 2'd0 : 
		(func3 == 3'b001 || func3 == 3'b101) ? 2'b11 : // readwrite
		(func3 == 3'b010 || func3 == 3'b110) ? 2'b10 : // readset
		(func3 == 3'b011 || func3 == 3'b111) ? 2'b01 : // readclear
		2'd0;
	assign external_call = external_call_or_bp && !func7;
	assign external_break = external_call_or_bp && func7 == 7'b1;

	// We can use an always_comb block to determine if there is an invalid
	// instruction exception.

endmodule
