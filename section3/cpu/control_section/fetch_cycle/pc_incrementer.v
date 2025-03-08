module pc_incrementer(
	clk,
	program_counter,
	
	program_counter_new
);

    // ports
    input           clk;

    input [31:0]    program_counter;

    output [31:0]   program_counter_new;



	reg [31:0] program_counter_new_reg;
	assign program_counter_new = program_counter_new_reg;

	always @(posedge clk) begin
		program_counter_new_reg <= program_counter + 4;
	end
endmodule
