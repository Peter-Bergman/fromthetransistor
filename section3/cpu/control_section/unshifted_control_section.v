module control_section(
	clk,
	rst,

	instruction_address,
	instruction,
	
	register1,
	register2,

	memory_address,
	memory_data_store,
	memory_read,
	memory_write,
	memory_data_load,
	
	registerd,
);
	// ports
	input				clk;
	output [31:0]			instruction_address;
	input [31:0]			instruction;
	output [31:0]			memory_address,
					memory_data_store;
	input [31:0]			memory_data_load;
	output				memory_read,
					memory_write;

	// internals
	reg [31:0]			pc,

					fetch_decode_pc_reg,
					fetch_decode_instruction_reg,

					decode_execute_pc_reg,
					decode_execute_operand_reg1,
					decode_execute_operand_reg2,
					decode_execute_immediate_reg,
					decode_execute_store_immediate_reg,
					decode_execute_branch_immediate_reg,
					decode_execute_upper_immediate_reg,
					decode_execute_jump_immediate_reg,
	reg [4:0]			decode_execute_destination_reg,
	reg				decode_execute_branch_reg,
	reg [6:0]			decode_execute_op_code_reg;
	reg [2:0]			decode_execute_func3_reg;
	reg [6:0]			decode_execute_func7_reg;
					
	reg [31:0]			execute_memory_access_pc_reg,
					execute_memory_access_branch_result_reg,
					execute_memory_access_alu_result_reg,
	reg [4:0]			execute_memory_access_destination_reg;
	reg				execute_memory_access_write_reg,
					execute_memory_access_read_reg,
					execute_memory_access_branch_reg;

	reg [31:0]			memory_access_write_back_pc_reg,
					memory_access_write_back_data_reg;
	reg [4:0]			memory_access_write_back_destination_reg,

	reg [31:0]			post_write_back_pc_reg;

	reg				branch_signal_reg;

	// alu
	alu arithmetic_logic_unit(
		.op_code (decode_execute_op_code_reg),
		.func3 (decode_execute_func3_reg),
		.func7 (decode_execute_func7_reg),
		.reg1 (decode_execute_operands_reg1),
		.reg2 (decode_execute_operands_reg2),
		.program_counter (decode_execute_pc_reg),
		.immediate (decode_execute_immediate_reg),
		.store_immediate (decode_execute_store_immediate_reg),
		.branch_immediate (decode_execute_branch_immediate_reg),
		.upper_immediate (decode_execute_upper_immediate_reg),
		.jump_immediate (decode_execute_jump_immediate_reg),

		// outputs
		.branch_result (execute_memory_access_branch_result),
		.alu_result (execute_memory_access_alu_result),
		.branch (execute_memory_access_branch),
		// branch_result
		// alu_result
		// branch
	);

	instruction_decoder decoder (
		.instruction (fetch_decode_instruction),
		
		.register1 (select_register1),
		.register2 (select_register2),
		.immediate (decode_execute_immediate),
		.store_immediate (decode_execute_store_immediate),
		.branch_immediate (decode_execute_branch_immediate),
		.upper_immediate (decode_execute_upper_immediate),
		.jump_immediate (decode_execute_jump_immediate),
		.registerd (decode_execute_destination),
	//	.branch (decode_execute_branch), // This line of code is
	//	commented because we get the branch signal from the output of
	//	the execute phase (alu)
		.op_code (decode_execute_op_code),
		.func3 (decode_execute_func3),
		.func7 (decode_execute_func7),
	);

	register_file registers(
		.clk (clk),
		.rst (rst),

		.select_operand1 (select_register1),
		.select_operand2 (select_register2),
		
		.operand_output1 (decode_execute_operand1),
		.operand_output2 (decode_execute_operand2),

		.select_destination (memory_access_write_back_destination),
		.data_in (memory_access_write_back_data),
		// Need to fill this part
		//
		//
		// asdf
		// asdfa
		// sdf
		// asdf
		// asd
		// fa
		// sdf
		// asd
		// fas
		// df
		// Filler text to make this section noticeable!!!!!\
		// !!!!!!!!!!!!!1
		// !!!!!!!!!!!!!!!!!!!!!!!!!!
	);


	// logic
	assign instruction_address = pc;
	always @(posedge clk) begin

		if (post_write_back_pc_reg == memory_access_write_back_pc_reg) begin
			// move everything in the execute_memory_access
			// registers through the memory access stage of the
			// pipeline to the memory_access_write_back registers
		end
		if (memory_access_write_back_pc_reg == execute_memory_access_pc_reg && !branch_signal_reg) begin
			// move decode_execute registers through execute
			// execute phase of the pipeline to
			// execute_memory_access registers
			execute_memory_access_pc_reg <= decode_execute_pc_reg;
			execute_memory_access_branch_result_reg <= execute_memory_access_branch_result;
			execute_memory_access_alu_result_reg <= execute_memory_access_alu_result;
			execute_memory_access_destination_reg <= decode_execute_destination_reg;
			execute_memory_access_write <= decode_execute_write;
			execute_memory_access_read <= decode_execute_read;
			execute_memory_access_branch_reg <= execute_memory_access_branch;
		end
		if (execute_memory_access_pc_reg == decode_execute_pc_reg) begin
			// move fetch_decode registers through fetch phase of
			// the pipeline and into the decode_execute registers
			decode_execute_pc_reg <= fetch_decode_pc_reg;
			decode_execute_operand_reg1 <= decode_execute_operand1;
			decode_execute_operand_reg2 <= decode_execute_operand2;
			decode_execute_immediate_reg <= decode_execute_immediate;
			decode_execute_store_immediate_reg <= decode_execute_store_immediate;
			decode_execute_branch_immediate_reg <= decode_execute_branch_immediate;
			decode_execute_upper_immediate_reg <= decode_execute_upper_immediate;
			decode_execute_jump_immediate_reg <= decode_execute_jump_immediate;
			decode_execute_destination_reg <= decode_execute_destination;
			decode_execute_write_reg <= decode_execute_write;
			decode_execute_read_reg <= decode_execute_read;
			decode_execute_branch_reg <= decode_execute_branch;
			decode_execute_op_code_reg <= decode_execute_op_code;
		end
		if (decode_execute_pc_reg == fetch_decode_pc_reg) begin
			// fetch the instruction in instruction memory
			// with address program counter
			fetch_decode_pc_reg <= pc + 4;
			fetch_decode_instruction_reg <= instruction;
		end
		if (fetch_decode_pc_reg == next_pc) begin
			// get the next program counter value
			// This could be the last pc incremented by 4, or it
			// could be a value from a branch instruction or
			// a value from a jump instruction.
			if (branch_signal_reg) begin
				pc <= calculated_address;
				branch_signal_reg <= 1'b0;
				fetch_decode_pc_reg <= 32'b0;
				decode_execute_pc_reg <= 32'b0;
				execute_memory_access_pc_reg <= 32'b0;
			end else
				pc <= next_pc;
			end
		end
	end

endmodule
