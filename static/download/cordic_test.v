module cordic_tb();
    reg clock = 0;
    reg reset = 0;
    reg start = 0;
    wire [31:0] cos_out;
    wire [31:0] sin_out;
    
    wire [31:0] angle;
    
    cordic cordic(
        .angle(angle),
        
        .clock(clock),
        .reset(reset),
        .start(start),
        .angle_in(32'hc0000000),
        .cos_out(cos_out),
        .sin_out(sin_out)
    );
    
    always #5 clock = ~clock;
    
    initial begin
        $display("c r s cos      sin");
        $display("- - - -------- --------");
        $monitor("%b %b %b %h %h %h", clock, reset, start, cos_out, sin_out, angle);
        
        #12 reset = 1;
        #15 reset = 0;
        #20 start = 1;
        #10 start = 0;
        
        #330 $finish;
    end
endmodule
