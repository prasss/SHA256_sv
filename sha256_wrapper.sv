//reg 0: 32 bit Control register - Address offset - 0   		(bit 0 is the Start bit)
//reg 1: 32 bit Status register - Address offset - 4			(bit 0 is the Status bit)
//reg 2: 32 bit Data_input register - Address offset - 8		(only 24 bits valid)
//reg 3: 32 bit Data_output register Address offset - 	12		(8 * 32 bit registers)
//reg 4: 32 bit Data_output register - Address offset - 	16
//reg 5: 32 bit Data_output register - Address offset - 	20
//reg 6: 32 bit Data_output register - Address offset - 	24
//reg 7: 32 bit Data_output register - Address offset - 	28
//reg 8: 32 bit Data_output register - Address offset - 	32
//reg 9: 32 bit Data_output register - Address offset - 	36
//reg 10: 32 bit Data_output register - Address offset - 40



package wrapper_state_machine_definitions;

	enum logic [2:0] {__RESET = 3'b000, __IDLE = 3'b001, __FETCH = 3'b010, __FIRST = 3'b011, __PROC_1 = 3'b100, __PROC_2 = 3'b101, __FINAL = 3'b110} state;
	
endpackage


module sha256_wrapper	#(parameter PADDING_SIZE = 512)
			(input logic clk, reset_n,
			input logic [5:0] address,
			input logic wren, rden,
			output logic [31:0] read_reg,
			input logic [31:0] write_reg);

	import state_machine_definitions::*;
	
	logic start = 0; 
	logic [23:0] message = 24'd0;
	logic done = 0;
	logic [511:0] padding;
	
	logic [31:0] register_writing [1:0];
	logic [31:0] register_reading[8:0];
	
	integer idx;
	
	always_ff@(posedge clk)
			if(reset_n != 0 && wren)
				case(address)
					6'd0: begin 
								register_writing[0] <= write_reg; 			//Control register
								
								if(register_writing[0][0] == 1) begin
									start <= register_writing[0][0];			//bit 0 of control register is the start bit
								end
							end		
					
					6'd8: begin //Input data register (3 Ascii letters message i.e. 24 bits)
									register_writing[1] <= write_reg; 		
									message <= register_writing[1][23:0];
							end		
					
				endcase
			else
				begin
					//else set the previous values again
					register_writing[0] <= register_writing[0];
					register_writing[1] <= register_writing[1];
				end
				
	always_latch
		if(rden)
			case(address)

					6'd4 : begin read_reg = register_reading[0]; end		//status register
					
					6'd12: begin read_reg = register_reading[1]; end	//start of the Data output section
					6'd16: begin read_reg = register_reading[2]; end
					6'd20: begin read_reg = register_reading[3]; end
					6'd24: begin read_reg = register_reading[4]; end
					6'd28: begin read_reg = register_reading[5]; end
					6'd32: begin read_reg = register_reading[6]; end
					6'd36: begin read_reg = register_reading[7]; end
					6'd40: begin read_reg = register_reading[8]; end		
									
				endcase
			else
				read_reg = 0;
		
	sha256_padding uut1 (.message(message), .padding(padding));
	
	
	logic [31:0] a, b, c, d, e, f, g, h;
	logic [31:0] h1, h2, h3, h4, h5, h6, h7, h8;
	logic [255:0] hash_op;

	integer localIndex = 0;
	logic [7:0] sc = 8'd0;	//state counter
	logic [31:0] W [63:0];
	logic [31:0] t1;
	logic [31:0] t2;
	logic [31:0] cK;		//current K value in use
	logic [31:0] tempW;		//current W value temp
	logic [31:0] Wt_15;		
	logic [31:0] Wt_2;	
	
	

	logic [31:0] lsigma1, lsigma0, usigma1, usigma0, choice, majority;
	
	integer i = 0, j = 0;
	
	logic [2047:0] kt = {32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 
								32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5, 32'hd807aa98, 32'h12835b01, 
								32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 
								32'hc19bf174, 32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 
								32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da, 32'h983e5152, 
								32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 
								32'h06ca6351, 32'h14292967, 32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 
								32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85, 
								32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 
								32'hd6990624, 32'hf40e3585, 32'h106aa070, 32'h19a4c116, 32'h1e376c08, 
								32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 
								32'h682e6ff3, 32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 
								32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2};

	logic [255:0] ht = {32'h6a09e667, 32'hbb67ae85, 32'h3c6ef372, 32'ha54ff53a, 32'h510e527f, 
							  32'h9b05688c, 32'h1f83d9ab, 32'h5be0cd19};

	logic [3:0] signal_reg = 4'b0000;
	
	
	//Start signal detection
	always_ff@(posedge clk)
		begin : start_detection
			if(reset_n == 1'b0)
				signal_reg <= 4'b0000;
			else begin
				signal_reg <= {signal_reg[2:0],start};
				end
		end
			
	logic signal_start;
	//here the signal_start variable is populated according to the signal rise pattern
	assign signal_start = (signal_reg == 4'b0011)? 1'b1:1'b0; 

	
	always_comb begin
	
		//First we need to prepare the Array of 32bit values from the 512 bits
		for(i = 0; i < 4; i = i + 1) 
		begin
			for(j = 0; j < 16; j = j + 1) 
			
			begin

				if(i == 0) begin
					W[i*16+j] = padding[((PADDING_SIZE-1)-j*32) -: 32];	//store j0->j15 while i = 0 i.e. first 16 values
					$display("F16 - W[%0d]: %h",j,W[i*16+j]);
				end
				else begin
					
					Wt_15 = W[(i*16+j) - 15];
					Wt_2 = W[(i*16+j) - 2];

					//lsigma0 = ROR(1, 7) XOR ROR(1, 18) XOR SHR(1, 3)
					lsigma0 = {Wt_15[6:0],Wt_15[31:7]} ^ {Wt_15[17:0],Wt_15[31:18]} ^ (Wt_15 >> 3);

					//lsigma1 = ROR(1, 17) XOR ROR(1, 19) XOR SHR(1, 10)
					lsigma1 = {Wt_2[16:0],Wt_2[31:17]} ^ {Wt_2[18:0],Wt_2[31:19]} ^ (Wt_2 >> 10);

					//we need to calculate each W value as per the algorithm
					W[i*16+j] = lsigma1 + W[(i*16+j)-16] + lsigma0 + W[(i*16+j) -7];
					
					$display("W[%0d]: %h",i*16+j,W[i*16+j]);
				end
			end
		end
	end
	
	
	always_comb begin

		//choice - Ch(E, F, G) = (E AND F) XOR ((NOT E) AND G)
		choice = (e & f) ^ (~e & g);

		//majority - Ma(A, B, C) = (A AND B) XOR (A AND C) XOR (B AND C)
		majority = (a & b) ^ (a & c) ^ (b & c);
				
		//usigma0 = (A >>> 2) XOR (A >>> 13) XOR (A >>> 22)
		usigma0 = {a[1:0],a[31:2]} ^ {a[12:0],a[31:13]} ^ {a[21:0],a[31:22]};

		//usigma1 = (E >>> 6) XOR (E >>> 11) XOR (E >>> 25)
		usigma1 = {e[5:0],e[31:6]} ^ {e[10:0],e[31:11]} ^ {e[24:0],e[31:25]};
		
		t1 = (h + usigma1 + choice + cK + tempW);
		t2 = (usigma0 + majority);
	end
	
	
	//state machine
	always_ff@(posedge clk) 
	begin : state_machine
			if(reset_n == 1'b0)
				begin
					//All the required variables and registers for the SHA256 algorithm are initialised
					a <= 32'd0; b <= 32'd0; c <= 32'd0; d <= 32'd0; e <= 32'd0; f <= 32'd0; g= 32'd0; h= 32'd0;
					h1 <= 32'd0; h2 <= 32'd0; h3 <= 32'd0; h4 <= 32'd0; h5= 32'd0; h6= 32'd0; h7= 32'd0; h8= 32'd0;
					sc <=  8'd0;  cK <=  32'd0; tempW <= 32'd0;
					
					h1 <= ht[255:224]; h2 <= ht[223:192]; h3 <= ht[191:160];	h4 <= ht[159:128]; h5 <= ht[127:96];
					h6 <= ht[95:64]; h7 <= ht[63:32]; h8 <= ht[31:0]; done <= 0;
					
					state	<= __RESET;
				end
			else
				case(state)
					__RESET: begin	
							state	<= __IDLE;
							end
				
					__IDLE: begin
							
							if(signal_start && done == 0)
							begin
								state	<= __FIRST;
							end
							else
								state	<= __IDLE;							
							end
				
					__FIRST: begin	
							//In the __FIRST state, the initial values for the SHA256 algorithm are populated
							a <= h1;
							b <= h2;
							c <= h3;
							d <= h4;
							e <= h5;
							f <= h6;
							g <= h7;
							h <= h8;

							$display("First - h1: %h, h2: %h, h3: %h, h4: %h, h5: %h, h6: %h, h7: %h, h8: %h",h1, h2, h3, h4, h5, h6, h7, h8);		
								
							state	  <= __PROC_1;
						end
					__PROC_1: begin
							//In the __PROC_1 state, the K and W values are populated iteratively according to sc (0 -> 63)
							cK[31:0] <= kt[2047-sc*32-: 32];
							tempW[31:0] <= W[sc];

							state	  <= __PROC_2;
							end
							
					__PROC_2: begin
					
							$display("sc: %0d",sc);
							//In the __PROC_2 state, the individual values from a -> h are populated iteratively and sc is incremented by 1
						
							h <= g;
							g <= f;
							f <= e;
							e <= d + t1;
							d <= c;
							c <= b;
							b <= a;
							a <= t1 + t2;
							sc <= sc + 1;							
							
							$display("a: %h, b: %h, c: %h, d: %h, e: %h, f: %h, g: %h, h: %h\n\n",a, b, c, d, e, f, g, h);
							
							if (sc == 63) begin
								state	  <= __FINAL;
								end
							else
								state	  <= __PROC_1;
						end
					__FINAL: begin
							h1 <= a + h1;
							h2 <= b + h2;
							h3 <= c + h3;
							h4 <= d + h4;
							h5 <= e + h5;
							h6 <= f + h6;
							h7 <= g + h7;
							h8 <= h + h8;
							
							$display("Final- a: %h, b: %h, c: %h, d: %h, e: %h, f: %h, g: %h, h: %h",a, b, c, d, e, f, g, h);

							done <= 1;	//the done signal is used as part of the status register as well as for stopping execution in the IDLE state					
							state	  <= __IDLE;
						end

					default:
						begin
							//default state
						end
		endcase
	end

	//The registers are set for reading
	//first the status register is set
	assign register_reading[0] = {31'd0,done};
	
	//later the individual registers for the Sha256 hash output is set
	assign register_reading[1] = h1;
	assign register_reading[2] = h2;
	assign register_reading[3] = h3;
	assign register_reading[4] = h4;
	assign register_reading[5] = h5;
	assign register_reading[6] = h6;
	assign register_reading[7] = h7;
	assign register_reading[8] = h8;
	
endmodule
