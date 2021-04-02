`define LEN_SIZE 64

module sha256_padding #(parameter MSG_SIZE = 24,
			parameter PADDING_SIZE = 512)
			(input logic [MSG_SIZE-1:0] message,
			output logic [PADDING_SIZE-1:0] padding);

	localparam zeros_till_len = PADDING_SIZE - MSG_SIZE - `LEN_SIZE - 1;
	localparam zeros_in_len = `LEN_SIZE - $bits(MSG_SIZE);

	assign padding = { message, 1'b1, {zeros_till_len{1'b0}}, {zeros_in_len{1'b0}}, MSG_SIZE};
endmodule