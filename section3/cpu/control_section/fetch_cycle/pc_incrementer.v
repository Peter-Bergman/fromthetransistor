module pc_incrementer(
	input clk,
	input program_counter,
	
	output program_counter_new,
)

	reg program_counter_new_reg [31:0];
	assign program_counter_new = program_counter_new_reg;

	always @(posedge clk) begin
		program_counter_new_reg <= program_counter + 4;
	end
endmodule
