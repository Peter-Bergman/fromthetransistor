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
	input [31:0] instruction;

	output [4:0]		register1,
				register2,
				registerd,
	output [11:0] immediate;
	
	// internals
	// (No internals)


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
	
endmodule
