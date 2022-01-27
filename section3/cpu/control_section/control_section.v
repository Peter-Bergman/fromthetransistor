module control_section(
	clk,
	rst,

	instruction_ready,
	instruction_wait,
	instruction,
	
	register1,
	register2,

	memory_address,
	memory_data_store,
	memory_read,
	memory_write,
	memory_data_load,
	memory_wait,
	
	registerd,
);
	// ports
	input				clk;

	output reg			instruction_ready;
	input [31:0]			instruction;
	input				instruction_wait;

	output [31:0]			memory_address,
					memory_data_store;
	input [31:0]			memory_data_load;
	output				memory_read;
	output [1:0]			memory_write;

	// internals
	reg [31:0]			pc;
	
	wire [31:0]			next_pc,

					memory_hazard_check_addend;

	reg				instruction_waiting,
					memory_waiting;

	wire				instrucion_received,
					memory_received,
					can_access_memory,
					can_execute,
					can_decode,
					can_fetch,

					register_read_after_write_hazard,
					main_memory_read_after_write_hazard1,
					main_memory_read_after_write_hazard2,
					main_memory_read_after_write_hazard3;

	wire [4:0]			select_register1,
					select_register2,

					operand_register1,
					operand_register2;
						

	reg [31:0]			fetch_decode_pc_reg,
					fetch_decode_instruction_reg,

					decode_execute_pc_reg,
					decode_execute_operand_reg1,
					decode_execute_operand_reg2,
					decode_execute_immediate_reg,
					decode_execute_store_immediate_reg,
					decode_execute_branch_immediate_reg,
					decode_execute_upper_immediate_reg,
					decode_execute_jump_immediate_reg;
	reg [4:0]			decode_execute_destination_reg,
					decode_execute_shamt;
	reg [6:0]			decode_execute_op_code_reg;
	reg [2:0]			decode_execute_func3_reg;
	reg [6:0]			decode_execute_func7_reg;
	reg [1:0]			decode_execute_write_reg,
					decode_execute_reg_write_reg;
					
	reg [31:0]			execute_memory_access_pc_reg,
					execute_memory_access_store_val_reg;
					execute_memory_access_branch_result_reg,
					execute_memory_access_alu_result_reg;
	reg [4:0]			execute_memory_access_destination_reg;
	reg [1:0]			execute_memory_access_write_reg,
					execute_memory_access_reg_write_reg;
	reg				execute_memory_access_read_reg,
					execute_memory_access_branch_signal_reg;

	reg [31:0]			memory_access_write_back_pc_reg,
					memory_access_write_back_data_reg;
	reg [4:0]			memory_access_write_back_destination_reg;
	reg [1:0]			memory_access_write_back_reg_write_reg;

	reg [31:0]			post_write_back_pc_reg;


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
		.branch (execute_memory_access_branch_signal),
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
		.memory_write (decode_execute_write),
		.reg_write (decode_execute_reg_write),
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

		.write_type (memory_access_write_back_reg_write_reg);
		.select_destination (memory_access_write_back_destination_reg),
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
	assign next_pc = pc + 4;

	assign operand_register1 = fetch_decode_instruction[15:19];
	assign operand_register2 = fetch_decode_instruction[24:20];

	assign register_read_after_write_hazard = 	(

							( ( memory_access_write_back_destination_reg == operand_register1 || memory_access_write_back_destination_reg == operand_register2 ) && 
							memory_access_write_back_pc_reg && memory_access_write_back_destination_reg ) ||
							
							( ( execute_memory_access_destination_reg == operand_register1 || execute_memory_access_destination_reg == operand_register2 ) && 
							execute_memory_access_pc_reg && execute_memory_access_destination_reg ) || 

							( (decode_execute_destination_reg == operand_register1 || decode_execute_destination_reg == operand_register2 ) && 
							decode_execute_pc_reg && decode_execute_destination_reg )

						        ) &&
							fetch_decode_pc_reg;
	
	assign memory_hazard_check_addend = execute_memory_access_write_reg[1] ? ( execute_memory_access_write_reg[0] ? 32'd3 : 32'd1 ) : 32'd0;

	// The following signal, when high, indicates that the instruction in
	// the decode_execute registers is being overwritten in memory.
	assign main_memory_read_after_write_hazard1 = 	execute_memory_access_store_val_reg + memory_hazard_check_addend >= decode_execute_pc_reg - 32'd4 && 
							execute_memory_access_store_val_reg < decode_execute_pc_reg &&
							execute_memory_access_write_reg &&
							execute_memory_access_pc_reg;
	// The following signal, when high, indicates that the instruction ni
	// the fetch_decode registers is being overwritten in memory.
	assign main_memory_read_after_write_hazard2 = 	execute_memory_access_store_val_reg + memory_hazard_check_addend >= fetch_decode_pc_reg - 32'd4 &&
							execute_memory_access_store_val_reg < fetch_decode_pc_reg &&
							execute_memory_access_write_reg &&
							execute_memory_access_pc_reg;
	// The following signal, when high, indicates that the instruction
	// held in memory at the address held in the pc register is being
	// overwritten.
	assign main_memory_read_after_write_hazard3 =	execute_memory_access_store_val_reg + memory_hazard_check_addend >= pc &&
							execute_memory_access_store_val_reg < pc + 32'd4 &&
							execute_memory_access_write_reg &&
							execute_memory_access_pc_reg;

	assign instruction_received = instruction_waiting && !instruction_wait;

	assign memory_read = execute_memory_access_read_reg;
	assign memory_write = execute_memory_access_write_reg;
	assign memory_received = memory_waiting && !memory_wait;

	assign can_access_memory = memory_received || !( memory_write || memory_read );
	assign can_execute = ( can_access_memory || !execute_memory_access_pc_reg ) && !main_memory_access_read_after_write_hazard1 && !execute_memory_access_branch_signal_reg;
	assign can_decode = ( can_access_execute || !decode_execute_pc_reg ) && !register_read_after_write_hazard && !main_memory_read_after_write_hazard2;
	assign can_fetch = instrucion_received && ( !fetch_decode_pc_reg || can_decode ) && !main_memory_read_after_write_hazard3;


	always @(posedge clk) begin

		//if (post_write_back_pc_reg == memory_access_write_back_pc_reg) begin
			// move everything in the execute_memory_access
			// registers through the memory access stage of the
			// pipeline to the memory_access_write_back registers
		//end
		

		// move decode_execute registers through execute
		// execute phase of the pipeline to
		// execute_memory_access registers
		

		if ( can_access_memory ) begin
			memory_access_write_back_pc_reg <= execute_memory_access_pc_reg;
			if ( memory_received ) begin
				memory_access_write_back_data_reg <= memory_data_load;
			end else begin
				memory_access_write_back_data_reg <= execute_memory_access_alu_result_reg;
			end
			memory_access_write_back_destination_reg <= execute_memory_access_destination_reg;
			memory_access_write_back_reg_write_reg <= execute_memory_access_reg_write_reg;

			memory_waiting <= 1'b0;
		end

		if ( can_execute ) begin
			execute_memory_access_pc_reg <= decode_execute_pc_reg;
			execute_memory_access_store_val_reg <= decode_execute_operand_reg2;
			execute_memory_access_branch_result_reg <= execute_memory_access_branch_result;
			execute_memory_access_alu_result_reg <= execute_memory_access_alu_result;
			execute_memory_access_destination_reg <= decode_execute_destination_reg;
			execute_memory_access_write <= decode_execute_write;
			execute_memory_access_read <= decode_execute_read;
			execute_memory_access_branch_signal_reg <= execute_memory_access_branch_signal;

			if ( !decode_execute_pc_reg ) begin
				execute_memory_access_read_reg <= 1'b0;
				execute_memory_access_write_reg <= 2'b0;
			end
		end

		// move fetch_decode registers through fetch phase of
		// the pipeline and into the decode_execute registers
		if ( can_decode ) begin
			decode_execute_pc_reg <= fetch_decode_pc_reg;
			decode_execute_operand_reg1 <= decode_execute_operand1;
			decode_execute_operand_reg2 <= decode_execute_operand2;
			decode_execute_immediate_reg <= decode_execute_immediate;
			decode_execute_store_immediate_reg <= decode_execute_store_immediate;
			decode_execute_branch_immediate_reg <= decode_execute_branch_immediate;
			decode_execute_upper_immediate_reg <= decode_execute_upper_immediate;
			decode_execute_jump_immediate_reg <= decode_execute_jump_immediate;
			decode_execute_destination_reg <= decode_execute_destination;
			decode_execute_shamt_reg <= select_register2;
			decode_execute_write_reg <= decode_execute_write;
			decode_execute_reg_write_reg <= decode_execute_reg_write_reg;
			decode_execute_read_reg <= decode_execute_read;
			decode_execute_op_code_reg <= decode_execute_op_code;
		end

		// fetch the instruction from instruction memory
		// with the address held in the program counter
		if ( can_fetch ) begin
			// WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
			// We need to make sure that the fetch_decode
			// registers can be overwritten. We need to make sure
			// that fetch_decode_pc_reg is equal to 0, or the
			// decode_execute_pc_reg is equal to 0, or the 
			// execute_memory_access_pc_reg is equal to 0, or the 
			// memory_received wire is on.
			fetch_decode_pc_reg <= next_pc;
			fetch_decode_instruction_reg <= instruction;

			pc <= next_pc;
			instruction_ready <= 1'b0;
			instruction_waiting <= 1'b0;
		end else if ( can_decode ) begin // If we can decode but not fetch, then the fetch_decode registers should be nullified.
			fetch_decode_pc_reg <= 32'b0;
		end
		
		// get the next program counter value
		// This could be the last pc incremented by 4, or it
		// could be a value from a branch instruction or
		// a value from a jump instruction. We also have to
		// protect ourselves from branch hazards.
		if (execute_memory_access_branch_signal_reg) begin
			execute_memory_access_branch_signal_reg <= 1'b0;
			fetch_decode_pc_reg <= 32'b0;
			decode_execute_pc_reg <= 32'b0;
			execute_memory_access_pc_reg <= 32'b0;
			execute_memory_access_read_reg <= 1'b0;
			execute_memory_access_write_reg <= 2'b0;

			pc <= execute_memory_access_branch_result_reg;
			instruction_ready <= 1'b0;
			instruction_waiting <= 1'b0;
		end

		// Ensure that we do not have any main memory RAW
		// hazards.
		if ( main_memory_read_after_write_hazard1 ) begin
			// WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
			// We need to make sure that the fetch signals
			// are reset properly.
			decode_execute_pc_reg <= 32'b0;
			fetch_decode_pc_reg <= 32'b0;

			pc <= decode_execute_pc_reg - 4;
			instruction_waiting <= 1'b0;
			instruction_ready <= 1'b0;
		end else if ( main_memory_read_after_write_hazard2 ) begin
			fetch_decode_pc_reg <= 32'b0;
			// WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING 
			// We need to make sure that the fetch signals
			// are reset properly and that
			// decode_execute signals do not go to
			// execute_memory_access
			// and that fetch_decode signals do not get
			// into decode_execute registers.
			pc <= fetch_decode_pc_reg - 4;
			instruction_waiting <= 1'b0;
			instruction_ready <= 1'b0;
		end else if ( main_memory_read_after_write_hazard3 ) begin
			pc <= pc; // This line is redundant.
			instruction_ready <= 1'b0;
			instruction_waiting <= 1'b0;
		end

		// Update the waiting signals if needed.
		if ( instruction_ready && instruction_wait ) begin
			instruction_waiting <= 1'b1;
		end
		if ( (memory_read || memory_write) && memory_wait ) begin
			memory_waiting <= 1'b1;
		end
		if ( !instruction_ready ) begin
			instruction_ready <= 1'b1;
		end

	end // {END OF THE ALWAYS BLOCK}

endmodule
