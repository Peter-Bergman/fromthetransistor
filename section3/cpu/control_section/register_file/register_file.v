module register_file(
	clk,
	rst,
	select_operand1,
	select_operand2,
	select_destination,
	data_in,
	operand_output1,
	operand_output2,
);

	// ports
	input [31:0] 				select_operand1,
						select_operand2,
						select_destination,
						data_in;

	output [31:0] 				operand_output1,
			  			operand_output2;

	// internal variables
	reg [31:0] 				reg_file [31:0];
	reg [31:0]				operand_output1_reg,
						operand_output2_reg;

	// logic
	always @(posedge clk) begin
		// This block resets all of the registers to 0.
		// The first issue noticed would probably be a
		// segmentation fault.
		if (rst == 1) begin
			for (int i = 0; i < 32; i++) begin
				reg_file[i] <= 0;
			end
		end
		else begin

			// The following two blocks are for the case
			// when the x0 register is referenced. The x0
			// register will always have the value of 0.
			if (select_operand1 == 5'b0) begin
				operand_output1_reg <= 32'h0;
			end
			if (select_operand2 == 5'b0) begin
				operand_output2_reg <= 32'h0;
			end

			// The following block is for write commands.
			// Since the x0 register is a constant equal to 0, the
			// value stored in the x0 register cannot be
			// overwritten.
			if (select_destination != 5'b0) begin
				reg_file [select_destination] <= data_in;
			end
		end
	end

endmodule
