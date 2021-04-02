`timescale 1 ns / 1 ps

`define HALF_CLK_PERIOD  10
`define RESET_PERIOD 	 400
`define DELAY 	    		 200

module sha256_process_tb;

	logic [255:0] hash_op;
	logic done = 0;

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
	integer j = 0;
		initial
			begin
				$display("Inside the reset and start block...");
				#`RESET_PERIOD local_reset_n = 1'b1;
				start = 0;
				for(j = 0; j < 32; j = j+1) begin
					#`DELAY if(j % 8 == 0) start = ~start;
				end
				$stop();
			end
	

	sha256_process uut2 (.clk(local_clk), .reset_n(local_reset_n), .start(start), .hash_op(hash_op), .done(done));

endmodule
