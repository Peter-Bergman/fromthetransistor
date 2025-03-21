module alu(
	input [6:0] op_code,
	input [2:0] func3,
	input [6:0] func7,
	input [31:0] reg1,
	input [31:0] reg2,
	input [31:0] program_counter,
	input [31:0] immediate,
	input [31:0] store_immediate,
	input [31:0] branch_immediate,
	input [31:0] upper_immediate,
	input [31:0] jump_immediate,
	input [4:0] shamt,

	output logic [31:0] alu_result,
	output logic [31:0] branch_result,
	output logic branch,
    output logic exception_caught
);

always_comb begin
    branch_result = 0;
    branch = 0;
    alu_result = 0;
    exception_caught = 1;
	
	// branch instructions
	if (op_code == 7'b1100011) begin
		case (func3)
			3'b0 : begin // BEQ
				if (reg1 == reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
                    alu_result = 0;
                    exception_caught = 0;
			end
			3'b001 : begin // BNE
				if (reg1 != reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
                    alu_result = 0;
                    exception_caught = 0;
			end
			3'b100 : begin // BLT
				if (signed'(reg1) < signed'(reg2))
					branch_result = program_counter + branch_immediate;
					branch = 1;
                    alu_result = 0;
                    exception_caught = 0;
			end
			3'b101 : begin // BGE
				if (signed'(reg1) >= signed'(reg2))
					branch_result = program_counter + branch_immediate;
					branch = 1;
                    alu_result = 0;
                    exception_caught = 0;
			end
			3'b110 : begin // BLTU
				if (reg1 < reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
                    alu_result = 0;
                    exception_caught = 0;
			end
			3'b111 : begin // BGEU
				if (reg1 >= reg2)
					branch_result = program_counter + branch_immediate;
					branch = 1;
                    alu_result = 0;
                    exception_caught = 0;
			end
            default: begin
                branch_result = 0;
                branch = 0;
                alu_result = 0;
                exception_caught = 1;
            end
		endcase
	end
	else if (op_code == 7'b0010111) begin // AUIPC
        branch_result = 0;
        branch = 0;
		alu_result = program_counter + upper_immediate;
        exception_caught = 0;
	end
	else if (op_code == 7'b0110111) begin // LUI
        branch_result = 0;
        branch = 0;
		alu_result = upper_immediate;
        exception_caught = 0;
	end
	else if (op_code == 7'b1101111) begin // JAL
		branch_result = program_counter + jump_immediate;
		branch = 1;
		alu_result = 0;
        exception_caught = 0;
	end
	else if (op_code == 7'b1100111 && func3 == 3'b000) begin // JALR
		branch_result = { { reg1 + immediate } [31:1], 1'b0 };
		branch = 1;
		alu_result = 0;
        exception_caught = 0;
	end
	else if (op_code == 7'b0000011) begin // LB LH LW LBU LHU
        branch_result = 0;
        branch = 0;
		alu_result = reg1 + immediate;
        exception_caught = 0;
	end
	else if (op_code == 7'b0100011) begin // SB SH SW
        branch_result = 0;
        branch = 0;
		alu_result = reg1 + immediate;
        exception_caught = 0;
	end
	else if (op_code == 7'b0010011) begin
		case (func3)
			3'b000 : begin // ADDI
                branch_result = 0;
                branch = 0;
				alu_result = reg1 + immediate;
                exception_caught = 0;
			end
			3'b010 : begin // SLTI
				if (signed'(reg1) < signed'(immediate)) begin
                    branch_result = 0;
                    branch = 0;
					alu_result = 32'b1;
                    exception_caught = 0;
				end else begin
                    branch_result = 0;
                    branch = 0;
					alu_result = 32'b0;
                    exception_caught = 0;
				end
			end
			3'b011 : begin //SLTIU
				if (reg1 < immediate) begin
                    branch_result = 0;
                    branch = 0;
					alu_result = 32'b1;
                    exception_caught = 0;
				end else begin
                    branch_result = 0;
                    branch = 0;
					alu_result = 32'b0;
                    exception_caught = 0;
				end
			end
			3'b100 : begin // XORI
                branch_result = 0;
                branch = 0;
				alu_result = reg1 ^ immediate;
                exception_caught = 0;
			end
			3'b110 : begin // ORI
                branch_result = 0;
                branch = 0;
				alu_result = reg1 | immediate;
                exception_caught = 0;
			end
			3'b111 : begin // ANDI
                branch_result = 0;
                branch = 0;
				alu_result = reg1 & immediate;
                exception_caught = 0;
			end
			3'b001 : begin // SLLI
                branch_result = 0;
                branch = 0;
				alu_result = reg1 << shamt;
                exception_caught = 0;
            end
			3'b101 : begin // SRLI SRAI
				if (func7 == 7'b0) begin // SRLI
                    branch_result = 0;
                    branch = 0;
					alu_result = reg1 >> shamt;
                    exception_caught = 0;
				end else if (func7 == 7'b0100000) begin // SRAI
                    branch_result = 0;
                    branch = 0;
					alu_result = reg1 >>> shamt;
                    exception_caught = 0;
				end
			end
		endcase
	end
	else if (op_code == 7'b0110011) begin
		case (func3)
			3'b000 : begin // ADD SUB
				if (func7 == 7'b0) begin // ADD
                    branch_result = 0;
                    branch = 0;
					alu_result = reg1 + reg2;
                    exception_caught = 0;
				end else if (func7 == 7'b0100000) begin // SUB
                    branch_result = 0;
                    branch = 0;
					alu_result = reg1 - reg2;
                    exception_caught = 0;
				end
			end
			3'b001 : begin // SLL
                branch_result = 0;
                branch = 0;
				alu_result = reg1 << reg2[4:0];
                exception_caught = 0;
			end
			3'b010 : begin // SLT
				if (signed'(reg1) < signed'(reg2)) begin
                    branch_result = 0;
                    branch = 0;
			    	alu_result = 32'b1;
                    exception_caught = 0;
				end else begin
                    branch_result = 0;
                    branch = 0;
					alu_result = 32'b0;
                    exception_caught = 0;
				end
			end
			3'b011 : begin // SLTU
				if (reg1 < reg2) begin
                    branch_result = 0;
                    branch = 0;
					alu_result = 32'b1;
                    exception_caught = 0;
				end else begin
                    branch_result = 0;
                    branch = 0;
					alu_result = 32'b0;
                    exception_caught = 0;
				end
			end
			3'b100 : begin // XOR
                branch_result = 0;
                branch = 0;
				alu_result = reg1 ^ reg2;
                exception_caught = 0;
			end
			3'b101 : begin // SRL SRA
				if (func7 == 7'b0) begin
                    branch_result = 0;
                    branch = 0;
					alu_result = reg1 >> reg2[4:0]; // SRL
                    exception_caught = 0;
				end else if (func7 == 7'b0100000) begin
                    branch_result = 0;
                    branch = 0;
					alu_result = reg1 >>> reg2[4:0]; // SRA
                    exception_caught = 0;
				end
			end
			3'b110 : begin // OR
                branch_result = 0;
                branch = 0;
				alu_result = reg1 | reg2;
                exception_caught = 0;
			end
			3'b111 : begin // AND
                branch_result = 0;
                branch = 0;
				alu_result = reg1 & reg2;
                exception_caught = 0;
			end
		endcase
	end
end

endmodule
