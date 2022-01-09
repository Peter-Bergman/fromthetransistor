module instruction_fetcher(
	clk,
	program_counter,
	address_selector,
	instruction_recieve,
	instruction
)
	
	// ports
	input				clk;
	input [31:0]			program_counter,
					instruction_recieve;
	output [31:0]			address_selector;
	output				address_updated;
	
	// internals
	reg [31:0]			last_address;

	// logic
	always @(posedge clk) begin
		if (program_counter != last_address) begin
			address_updated <= 1'b1;
			last_address <= program_counter;
		end else begin
			address_updated <= 1'b0;
		end
	end

endmodule
