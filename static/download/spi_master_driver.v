// A driver to communicate with a slave device over the SPI interface.
//
// A command-based interface is provided by the driver. Three commands are provided:
//   enable: enable the slave by bringing SCS low
//   disable: disable the slave by bringing SCS high
//   transfer: exchange a bit of data with the slave.
// 
// Typical usage is to wait until the "idle" output goes high, then assert one
// of "do_enable", "do_disable" or "do_transfer" for one cycle. The "ack" output
// will go high for one cycle when the command is completed.
module spi_master_driver(
    // Global synchronisation signals
    clock,   // Input; clock signal; time period is specified by the CLOCK_PERIOD_NS parameter
    reset_n, // Input; active-low asynchronous reset
    
    // SPI signals
    scs,  // Output; SPI chip select / slave select
    sck,  // Output: SPI clock
    mosi, // Output: SPI data (master out / slave in)
    miso, // Input: SPI data (master in / slave out)
    
    // Control signals
    idle,        // Output; high if the driver is ready to accept a command
    do_enable,   // Input; when idle is high, bringing do_enable high will begin an "enable" command
    do_disable,  // Input; when idle is high, bringing do_disable high will begin a "disable" command
    do_transfer, // Input; when idle is high, bringing do_transfer high will begin a "transfer" command
    ack,         // Output; upon completion of a command, this signal will be brought high for one clock cycle
    
    // Data signals
    wdata, // Input; data bit to send; must be stable on the same clock tick that do_transfer is asserted for
    rdata  // Output: most recent data ready; will be ready on the same clock tick that ack is asserted
);


    //////////////////// BEGIN INPUT PARAMETERS ////////////////////

    // Time period of 'clock' input, in nanoseconds.
    parameter CLOCK_PERIOD_NS = 20;

    // Minimum SCS active setup time before SCK posedge, in nanoseconds.
    // i.e. minimum time between SCS going low and SCK going high.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TSS_NS = 30;
    
    // Minimum SCS active hold time after SCK posedge, in nanoseconds.
    // i.e. minimum time between SCK going high and SCS going high.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TSH_NS = 30;

    // Minimum SCS inactive setup time before SCK posedge, in nanoseconds.
    // i.e. minimum time between SCS going high and SCK going high.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TNS_NS = 30;

    // Minimum SCS inactive hold time after SCK posedge, in nanoseconds.
    // i.e. minimum time between SCK going high and SCS going low.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TNH_NS = 30;
    
    // Minimum SCS inactive time, in nanoseconds.
    // i.e. minimum time between SCK going high after one operation is completed
    //  and it going low again before the next operation starts.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TN_NS = 120;
    
    // Minimum SCK high pulse width, in nanoseconds.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TCH_NS = 18;
    
    // Minimum SCK low pulse width, in nanoseconds.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TCL_NS = 24;
    
    // Minimum MOSI setup time before SCK posedge, in nanoseconds.
    // i.e. minimum time between MOSI being changed and SCK going high.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TDS_NS = 8;
    
    // Minimum MOSI hold time after SCK posedge, in nanoseconds.
    // i.e. minimum time between SCK going high and MOSI being changed.
    // Max value: CLOCK_PERIOD_NS * (2^TIMER_BITS - 1)
    parameter TDH_NS = 2;
    
    // Width of timer counter registers. This should be scaled in accomodation
    // with the timing values; specifically, a register of this width should be
    // large enough each of the *_CLOCKS values calculated below.
    parameter TIMER_BITS = 4;

    ///////////////////// END INPUT PARAMETERS /////////////////////
    
    
    // Timing values from above, converted from nanoseconds to numbers of clock ticks.
    parameter TSS_CLOCKS = (TSS_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TSH_CLOCKS = (TSH_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TNS_CLOCKS = (TNS_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TNH_CLOCKS = (TNH_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TN_CLOCKS = (TN_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TCH_CLOCKS = (TCH_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TCL_CLOCKS = (TCL_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TDS_CLOCKS = (TDS_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;
    parameter TDH_CLOCKS = (TDH_NS + CLOCK_PERIOD_NS - 1) / CLOCK_PERIOD_NS;

    // State machine constants.
    parameter STATE_BITS = 3;
    parameter ST_IDLE = 3'd0;
    parameter ST_ENABLE = 3'd1;
    parameter ST_DISABLE = 3'd2;
    parameter ST_TRANSFER_1 = 3'd3;
    parameter ST_TRANSFER_2 = 3'd4;
    parameter ST_TRANSFER_3 = 3'd5;

    // Ports
    input clock;
    input reset_n;
    output scs;
    output sck;
    output mosi;
    input miso;
    output idle;
    input do_enable;
    input do_disable;
    input do_transfer;
    output ack;
    input wdata;
    output rdata;
    
    // Generate idle and ack signals based on the current and next state.
    wire idle = state == ST_IDLE;
    wire ack = (state_next == ST_IDLE) && (state != ST_IDLE);
        
    // High if OK to bring SCS low.
    wire scs_deassert_ready = (clocks_since_sck_assert >= TNH_CLOCKS)
                            & (clocks_since_scs_assert >= TN_CLOCKS);
    
    // High if OK to bring SCS high.
    wire scs_assert_ready = (clocks_since_sck_assert >= TSH_CLOCKS);
    
    // High if OK to bring SCK high.
    wire sck_assert_ready = (clocks_since_sck_deassert >= TCL_CLOCKS)
                          & (clocks_since_scs_deassert >= TSS_CLOCKS)
                          & (clocks_since_scs_assert >= TNS_CLOCKS)
                          & (clocks_since_mosi_change >= TDS_CLOCKS);
    
    // High if OK to bring SCK low.
    wire sck_deassert_ready = (clocks_since_sck_assert >= TCH_CLOCKS);
    
    // High if OK to change MOSI.
    wire mosi_change_ready = (clocks_since_sck_assert >= TDH_CLOCKS);
    
    // Registers
    reg scs;       // SPI slave select output
    reg sck;       // SPI clock output
    reg mosi;      // SPI data output
    reg [STATE_BITS-1:0] state; // Current state
    reg rdata;     // Data read from slave
    reg wdata_buf; // Caches data to be written between do_transfer assertion and actual output of data
    reg [TIMER_BITS-1:0] clocks_since_sck_assert;   // Counts clock cycles since SCK was last brought high
    reg [TIMER_BITS-1:0] clocks_since_sck_deassert; // Counts clock cycles since SCK was last brought low
    reg [TIMER_BITS-1:0] clocks_since_scs_assert;   // Counts clock cycles since SCS was last brought high
    reg [TIMER_BITS-1:0] clocks_since_scs_deassert; // Counts clock cycles since SCS was last brought low
    reg [TIMER_BITS-1:0] clocks_since_mosi_change;  // Counts clock cycles since MOSI was last changed
    
    // State machine outputs
    reg scs_next;
    reg sck_next;
    reg mosi_next;
    reg [STATE_BITS-1:0] state_next;
    reg rdata_next;
    reg wdata_buf_next;
    
    // Register control logic
    always @(posedge clock or negedge reset_n) begin
        if (~reset_n) begin
            scs <= 1'b1;
            sck <= 1'b0;
            mosi <= 1'b0;
            state <= ST_IDLE;
            rdata <= 1'b0;
            wdata_buf <= 1'b0;
            clocks_since_sck_assert <= 0;
            clocks_since_sck_deassert <= 0;
            clocks_since_scs_assert <= 0;
            clocks_since_scs_deassert <= 0;
            clocks_since_mosi_change <= 0;
        end
        
        else begin
            scs <= scs_next;
            sck <= sck_next;
            mosi <= mosi_next;
            state <= state_next;
            rdata <= rdata_next;
            wdata_buf <= wdata_buf_next;
            
            if (sck_next & ~sck)
                clocks_since_sck_assert <= 0;
            else if (clocks_since_sck_assert != (1 << TIMER_BITS) - 1)
                clocks_since_sck_assert <= clocks_since_sck_assert + 1;
            
            if (~sck_next & sck)
                clocks_since_sck_deassert <= 0;
            else if (clocks_since_sck_deassert != (1 << TIMER_BITS) - 1)
                clocks_since_sck_deassert <= clocks_since_sck_deassert + 1;
            
            if (scs_next & ~scs)
                clocks_since_scs_assert <= 0;
            else if (clocks_since_scs_assert != (1 << TIMER_BITS) - 1)
                clocks_since_scs_assert <= clocks_since_scs_assert + 1;
            
            if (~scs_next & scs)
                clocks_since_scs_deassert <= 0;
            else if (clocks_since_scs_deassert != (1 << TIMER_BITS) - 1)
                clocks_since_scs_deassert <= clocks_since_scs_deassert + 1;
            
            if (mosi_next != mosi)
                clocks_since_mosi_change <= 0;
            else if (clocks_since_mosi_change != (1 << TIMER_BITS) - 1)
                clocks_since_mosi_change <= clocks_since_mosi_change + 1;
        end
    end
    
    // Finite state machine
    always @* begin
        scs_next = scs;
        sck_next = sck;
        mosi_next = mosi;
        state_next = state;
        rdata_next = rdata;
        wdata_buf_next = wdata_buf;
        
        case (state)
            ST_IDLE: begin
                // Accept commands and dispatch to appropriate state.
                if (do_enable) begin
                    state_next = ST_ENABLE;
                end
                else if (do_disable) begin
                    state_next = ST_DISABLE;
                end
                else if (do_transfer) begin
                    wdata_buf_next = wdata;
                    state_next = ST_TRANSFER_1;
                end
            end
            
            ST_ENABLE: begin
                // Bring SCS low when ready.
                if (scs_deassert_ready) begin
                    scs_next = 1'd0;
                    state_next = ST_IDLE;
                end
            end
            
            ST_DISABLE: begin
                // Bring SCS high when ready.
                if (scs_assert_ready) begin
                    scs_next = 1'd1;
                    state_next = ST_IDLE;
                end
            end
            
            ST_TRANSFER_1: begin
                // Set MOSI when ready.
                if (mosi_change_ready) begin
                    mosi_next = wdata_buf;
                    state_next = ST_TRANSFER_2;
                end
            end
            
            ST_TRANSFER_2: begin
                // Bring SCK high when ready.
                if (sck_assert_ready) begin
                    sck_next = 1'd1;
                    state_next = ST_TRANSFER_3;
                    rdata_next = miso; // latch data-in into rdata register
                end
            end
            
            ST_TRANSFER_3: begin
                // Bring SCK low when ready.
                if (sck_deassert_ready) begin
                    sck_next = 1'd0;
                    state_next = ST_IDLE;
                end
            end
        endcase
    end
endmodule
