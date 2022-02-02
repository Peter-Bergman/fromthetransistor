module CSR_Register_File(
	clk,
	rst,

	read,
	read_address,
	read_data,
	write_back,
	write_back_address,
	write_back_data
);

	// ports
	input			read;
	input [11:0]		read_address;
	output [31:0]		read_data;

	input [1:0]		write_back;
	input [11:0]		write_back_address;
	input [31:0]		write_back_data;


	// internals
	reg [31:0]		cycle,
				cycleh,
				time_,
				timeh,
				instret,
				instreth;
	
	wire			has_read_permission,
				has_write_permission;

	// logic
	
	// For now, we will just give the user permission to whatever they
	// wantt until we set up infrastructure for an operating environment.
	has_read_permission = 1'b1;
	has_write_permission = 1'b1;

	always_ff @ (posedge clk) begin


		if (read) begin
			if (read_permission) begin
				case (read_address)
					12'hC01 : 
						read_data <= cycle;
					12'hC02 :
						read_data <= time_;
					12'hC03 :
						read_data <= instret;
					12'hC80 :
						read_data <= cycleh;
					12'hC81 :
						read_data <= timeh;
					12'hC82 :
						read_data <= instreth;
					default :
						// Invalid address trap
				endcase
			end else begin
				// This is where we would handle a permission
				// error. The running user program is trying
				// to read CSRs that it is not allowed to
				// read.
				read_data <= 32'b0;
			end

		end // end of the if (read) block


		if (write_back == 2'b01) begin
			// clear
		end else if (write_back == 2'b10) begin
			// set
		end else if (write_back == 2'b11) begin
			// write
			if (write_back_permission) begin
				case (write_back_address)
						12'hC01 : 
							cycle <= write_back_data;
						12'hC02 :
							time_ <= write_back_data;
						12'hC03 :
							instret <= write_back_data;
						12'hC80 :
							cycleh <= write_back_data;
						12'hC81 :
							timeh <= write_back_data;
						12'hC82 :
							instreth <= write_back_data;
						default :
							// Invalid address trap
				endcase
			end // end of the write_back_permission block

		end // end of the write_back blocks

		if (rst) begin
			cycle <= 32'b0;
			time_ <= 32'b0;
			instret <= 32'b0;
			cycleh <= 32'b0;
			timeh <= 32'b0;
			instreth <= 32'b0;
		end
	end // end of the always_ff block

endmodule
