`include "./control_section/control_section.v"
`include "./mmu/sram/sram.v"


module motherboard(
    input wire clk,
    input wire rst
);

    // === Parameters ===
    parameter ADDRESS_WIDTH = 20;
    parameter DATA_WIDTH = 8;
    parameter DEPTH = 1 << ADDRESS_WIDTH;

    // === Wires between CPU and SRAM ===
    wire [31:0] memory_address;
    wire [31:0] memory_data_store;
    wire [31:0] memory_data_load;
    wire [1:0]  memory_read;
    wire [1:0]  memory_write;
    wire        memory_wait;

    wire        instruction_ready;
    wire        instruction_wait;
    wire [31:0] instruction;

    // === Wires for SRAM I/O ===
    wire [ADDRESS_WIDTH-1:0] sram_addr;
    wire [DATA_WIDTH-1:0]    sram_data_in;
    wire [DATA_WIDTH-1:0]    sram_data_out;
    wire                     sram_read;
    wire                     sram_write;
    wire                     sram_chip_select;

    // === CPU Instance ===
    control_section cpu (
        .clk(clk),
        .rst(rst),

        .instruction_ready(instruction_ready),
        .instruction_wait(instruction_wait),
        .instruction(instruction),

        .memory_address(memory_address),
        .memory_data_store(memory_data_store),
        .memory_read(memory_read),
        .memory_write(memory_write),
        .memory_data_load(memory_data_load),
        .memory_wait(memory_wait)
    );

    // === SRAM Instance ===
    sram #(
        .ADDRESS(ADDRESS_WIDTH),
        .DATA(DATA_WIDTH),
        .DEPTH(DEPTH)
    ) ram (
        .clk(clk),
        .addr(sram_addr),
        .data_in(sram_data_in),
        .data_out(sram_data_out),
        .read(sram_read),
        .write(sram_write),
        .chip_select(sram_chip_select)
    );

    // === Stateless SRAM Access Wiring ===

    // Simple policy: SRAM is selected whenever there's read or write request or instruction fetch
    assign sram_chip_select = instruction_ready | (|memory_read) | (|memory_write);

    // SRAM address comes from CPU's memory address, word-aligned
    assign sram_addr = memory_address[ADDRESS_WIDTH-1:0];

    // Only use the lower 8 bits for single-byte SRAM access
    assign sram_data_in = memory_data_store[7:0];

    // Currently forwarding one byte as instruction — you may expand to full word access later
    assign instruction = {4{sram_data_out}};  // placeholder for 32-bit instruction
    assign instruction_wait = 1'b0;

    assign memory_data_load = {4{sram_data_out}};  // placeholder for 32-bit data load
    assign memory_wait = 1'b0;

    // Single-byte SRAM interface: always read/write when requested
    assign sram_read = instruction_ready | (|memory_read);
    assign sram_write = |memory_write;

endmodule
