module instruction_decoder(
	instruction,

	register1,
	register2,
	immediate,
	store_immediate,
	branch_immediate,
	upper_immediate,
	jump_immediate,
	registerd,
	op_code,
	func3,
	func7,

	// Instruction types include (register, immediate, upper), jump,
	// branch, store, and load.
	// Maybe store and load
);
	
	// ports
	input [31:0]		instruction;

	output [4:0]		register1,
				register2, // shamt, or shift amount, is the same as the register2 value
				registerd;
	output [6:0]		op_code,
				func7;
	output [2:0]		func3;
	output [31:0]		immediate,
				store_immediate,
				branch_immediate,
				upper_immediate,
				jump_immediate;
	output [1:0]		read,
				write,
				reg_write;

		
	// internals
	assign branch = op_code == 7'b0100011;


	// logic
	assign register1 = instruction [19:15];
	assign register2 = instruction [24:20];
	assign registerd = instruction [11:7];

	assign immediate = 32'(signed'( instruction [31:20] ));
	assign store_immediate = 32'(signed'( { instruction [31:25], instruction [11:7] } ));
	assign branch_immediate = 32'(signed'( { instruction [31], instruction [7], instruction [30:25], instruction [11:8], 1'b0 } )); // Going to adjust for instruction alignment in ALU
	assign upper_immediate = { instruction [31:12], 12'b0 };
	assign jump_immediate = 32'(signed'( { instruction [31], instruction [19:12], instruction [20], instruction [30:21], 1'b0 } ));

	assign op_code = instruction [6:0];
	assign func3 = instruction [14:12];
	assign func7 = instruction [31:25];

	assign read = (op_code == 7'b0000011) ? (func3 == 3'b0 || func3 == 3'b100 ? 2'b01 : (func3 == 3'b001 || func3 == 3'b101 ? 2'b10 : (func3 == 3'b010 ? 2'b11 : 2'b0))) : 2'b0;
	assign write = (op_code == 7'b0100011) ? (func3 == 3'b0 ? 2'b01 : (func3 == 3'b001 ? 2'b10 : (func3 == 3'b010 ? 2'b010 : 2'b0))) : 2'b0;
	assign reg_write = !branch && !write;

endmodule
