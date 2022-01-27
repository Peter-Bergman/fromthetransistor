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
	shamt,

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
		branch = 1;
	end
	else if (op_code == 7'b1100111 && !func3) begin // JALR
		branch_result = { { reg1 + immediate } [31:1], 1'b0 };
		branch = 1;
	end
	else if (op_code == 7'b0000011) begin // LB LH LW LBU LHU
		alu_result = reg1 + immediate;
	end
	else if (op_code == 7'b0100011) begin // SB SH SW
		alu_result = reg1 + immediate;
	end
	else if (op_code == 7'b0010011) begin
		case (func3)
			3'b000 : begin // ADDI
				alu_result = reg1 + immediate;
			end
			3'b010 : begin // SLTI
				if (signed'(reg1) < signed'(immediate)) begin
					alu_result = 32'b1;
				end else begin
					alu_result = 32'b0;
				end
			end
			3'b011 : begin //SLTIU
				if (reg1 < immediate) begin
					alu_result = 32'b1;
				end else begin
					alu_result = 32'b0;
				end
			end
			3'b100 : begin // XORI
				alu_result = reg1 ^ immediate;
			end
			3'b110 : begin // ORI
				alu_result = reg1 | immediate;
			end
			3'b111 : begin // ANDI
				alu_result = reg1 & immediate;
			end
			3'b001 : begin // SLLI
				alu_result = reg1 << shamt;
			3'b101 : begin // SRLI SRAI
				if (func7 == 7'b0) begin // SRLI
					alu_result = reg1 >> shamt;
				end else if (func7 == 7'b0100000) begin // SRAI
					alu_result = reg1 >>> shamt;
				end
			end
		endcase
	end
	else if (op_code == 7'b0110011) begin
		case (func3)
			3'b000 : begin // ADD SUB
				if (func7 == 7'b0) begin // ADD
					alu_result = reg1 + reg2;
				end else if (func7 == 7'b0100000) begin // SUB
					alu_result = reg1 - reg2;
				end
			end
			3'b001 : begin // SLL
				alu_result = reg1 << reg2[4:0];
			end
			3'b010 : begin // SLT
				if (signed'(reg1) < signed'(reg2)) begin
			    		alu_result = 32'b1;
				end else begin
					alu_result = 32'b0;
				end
			end
			3'b011 : begin // SLTU
				if (reg1 < reg2) begin
					alu_result = 32'b1;
				end else begin
					alu_result = 32'b0;
				end
			end
			3'b100 : begin // XOR
				alu_result = reg1 ^ reg2;
			end
			3'b101 : begin // SRL SRA
				if (func7 == 7'b0) begin
					alu_result = reg1 >> reg2[4:0]; // SRL
				end else if (func7 == 7'b0100000) begin
					alu_result = reg1 >>> reg2[4:0]; // SRA
				end
			end
			3'b110 : begin // OR
				alu_result = reg1 | reg2;
			end
			3'b111 : begin // AND
				alu_result = reg1 & reg2;
			end
		endcase
	end

					
		

end
