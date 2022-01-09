module sram(
	clk,
	addr,
	data_in,
	data_out,
	read,
	write,
	chip_select,


);

parameter ADDRESS = 20;
parameter DATA = 8;
parameter DEPTH = 20;

// ports
input [DATA-1:0]			data_in;
output [DATA-1:0]			data_out;
input [ADDRESS-1:0]			addr;
input					chip_select,
					write,
					read,
					clk;

// internal variables
reg [DATA-1:0] SRAM [DEPTH-1:0];

// logic
always @(posedge clk) begin
	if (chip_select == 1) begin
		if (write == 1'b1 && read == 1'b0) begin
			SRAM [ADDRESS] = data_in;
		end
		else if (read == 1'b1 && write == 1'b0) begin
			data_out = SRAM [ADDRESS];
		end
	end
	else;
end

endmodule
