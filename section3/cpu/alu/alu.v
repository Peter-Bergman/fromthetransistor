module alu(
	op_code,
	func3,
	func7,
	reg1,
	reg2,
	program_counter,
	immediate,
	store_immediate,
	branch_immediate,
	upper_immediate,
	jump_immediate,

	alu_result,
	branch_result,
	branch,
);


always_comb begin
	
	// branch instructions
	if (op_code == 7'b1100011) begin
		case (func3)
			3'b0 : begin // BEQ
				if (reg1 == reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
			end
			3'b001 : begin // BNE
				if (reg1 != reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
			end
			3'b100 : begin // BLT
				if (signed'(reg1) < signed'(reg2))
					branch_result = program_counter + branch_immediate;
					branch = 1;
			end
			3'b101 : begin // BGE
				if (signed'(reg1) >= signed'(reg2))
					branch_result = program_counter + branch_immediate;
					branch = 1;
			end
			3'b110 : begin // BLTU
				if (reg1 < reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
			end
			3'b111 : begin // BGEU
				if (reg1 >= reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
			end
		endcase
	end
	else if (op_code == 7'b0010111) begin // AUIPC
		alu_result = program_counter + upper_immediate;
	end
	else if (op_code == 7'b0110111) begin // LUI
		alu_result = upper_immediate;
	end
	else if (op_code == 7'b1101111) begin // JAL
		branch_result = program_counter + jump_immediate;
	end
	else if (op_code == 7'b1100111 && !func3) begin // JALR
		branch_result = { { reg1 + immediate } [31:1], 1'b0 };
	end
	else if (op_code == 7'b0000011) begin // LB LH LW LBU LHU
		alu_result = reg1 + immediate;
	end
	else if (op_code == 7'b0100011) begin // SB SH SW
		alu_result = reg1 + immediate;
	end
	else if (op_code == 7'b0010111) begin
		case (func3)
			3'b000 : begin // ADDI
				alu_result = reg1 + immediate;
			end
			3'b010 : begin // SLTI
				if (signed'(reg1) < signed'(immediate))
		

end
