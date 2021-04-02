`timescale 1 ns / 10 ps

module sha256_padding_tb;
	logic [511:0] padding;
	logic [23:0] message;

	sha256_padding uut (.message(message), .padding(padding));

	initial begin
    	assign message = 24'b011000010110001001100011;
	$display(message);
    	$display("FINISHED padding...");
    	$finish;
	end
endmodule
			
