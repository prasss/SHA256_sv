`timescale 1 ns / 1 ps

`define HALF_CLK_PERIOD  10
`define RESET_PERIOD 	 400
`define DELAY 	    		 200

module sha256_wrapper_tb;

	logic reset_n = 0;					//variable for the reset signal
	logic [5:0] address = 6'd0;		//variable for storing the address offset to be accessed
	logic wren = 0;						//variable for the write enable signal
	logic rden = 0;						//variable for the read enable signal
	logic [31:0] read_reg = 32'd0;	//register for reading data
	logic [31:0] write_reg = 32'd0;	//register for writing data
		

	//Clock generation
	logic local_clk = 0;
	integer i = 0;
	initial 
		begin: clock_generation_process
		$display("Clock intialised!");
		for(i = 0; i < 600; i = i+1)
			begin
					#`HALF_CLK_PERIOD local_clk = ~local_clk;
			end
		 
			end
			
		
	//Reset and Start generation
	logic local_reset_n = 0; logic start = 0;
	logic [31:0] hash_op [7:0];
	logic [255:0] final_hash = 0;
	integer j = 0;
		initial
			begin
				#`RESET_PERIOD local_reset_n = 1'b1;
				start = 0;
				wren = 0;
				rden = 0;

				#`DELAY;
				start = 1;		//setting start signal to 1 to initiate the hashing process
				#`DELAY;
				
				//setting the message through write_reg
				write_reg = 32'b00000000011000010110001001100011;	//	8 bits 0 + 24 bits data
				address = 6'd8;
				wren = 1;			//write enable set high
				rden = 0;
				
				#`DELAY;
				
				wren = 0;
				rden = 0;
				
				//setting the Control register to bit 0 to high for the start bit
				write_reg = 32'd1;	//	last bit is 1 for start bit to be set high
				address = 6'd0;
				wren = 1;				//write enable set high
				rden = 0;
				
				#`DELAY;
						
				//delay to enable the Hash calculation to run completely before reading the status and data
				for(j = 0; j < 32; j = j+1) begin
					#`DELAY;
				end
						
				//getting the Status register bit 0 value
				read_reg = 32'd0;	//	last bit is 1 for start bit to be set high
				address = 6'd4;
				wren = 0;
				rden = 1;			//read enable set high

				
				#`DELAY;
				
				$display("read_reg[0]: %x", read_reg[0]);
				
				//Below, all the Hash output values are obtained through their offset address values, the extra delays
				if(read_reg[0])
					begin
						address = 6'd12;
						#`DELAY;
						hash_op[0] = read_reg;
						
						address = 6'd16;
						#`DELAY;
						hash_op[1] = read_reg;
						
						address = 6'd20;
						#`DELAY;
						hash_op[2] = read_reg;
						
						address = 6'd24;
						#`DELAY;
						hash_op[3] = read_reg;
						
						address = 6'd28;
						#`DELAY;
						hash_op[4] = read_reg;
						
						address = 6'd32;
						for(j = 0; j < 8; j = j+1) begin
						#`DELAY;
						end
						hash_op[5] = read_reg;
						
						address = 6'd36;
						#`DELAY;
						hash_op[6] = read_reg;
						
						address = 6'd40;
						#`DELAY;
						hash_op[7] = read_reg;
					end
				
				//concatinating data to form the final hash output value
				final_hash = {hash_op[0], hash_op[1], hash_op[2], hash_op[3], hash_op[4], hash_op[5], hash_op[6], hash_op[7]};
				
				#`DELAY;
				
				$display("hash_op: %x", final_hash);
				
				start = 0;			//setting start signal to 0 to stop the hashing process
				wren = 0;
				rden = 0;
				
				#`DELAY;
				
				$stop();
			end
			

	sha256_wrapper uut2 (.clk(local_clk), .reset_n(local_reset_n),
								.address(address), .wren(wren), .rden(rden), 
								.read_reg(read_reg),	.write_reg(write_reg));

endmodule
