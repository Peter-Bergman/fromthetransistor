module blinkingLED(
	input clock,
	input reset,
	output led
);
reg [15:0] counter;
assign led = counter[15];
always @(posedge clock or reset) begin
	counter <= counter + 1;
end
endmodule;

