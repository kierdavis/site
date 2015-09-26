---
title: Embedded APL Part 1: Design
---

From the start this looked like it would be a major project, so a fair bit of planning was involved.

The hardware features that would need to be accessed included:

  * UART, for debugging.
  * VGA and/or an LCD screen, for graphical output.
  * USB, to connect a USB keyboard.
  * off-chip SDRAM, since the FPGA only contains about 20 kB of memory which must also hold the firmware ROM, video memory and bitmap font ROM.
  * SD card to hold software.

Optional peripherals that were also present on my development board that may be relevant this application include an Ethernet port & controller IC and a real-time clock.

It was obvious that some form of CPU would be needed; the system was too complex to be implemented entirely in HDL. The requirements of the CPU were:

  * at least 32-bit, to make implementing APL's arithmetic operations simpler, especially in the more complex list-based operations.
  * ability to connect custom peripherals to the processor (not just those selected from a standard set).
  * ideally, some hardware acceleration for floating point operations.
  * above all, must fit into the low-end FPGA I was using for development (an Altera Cyclone II EP2C8Q208C8).

I initially experimented with some existing CPU architectures:

  * [Nios II][nios] is Altera's proprietary 32-bit CPU. A full toolchain including a C compiler is included as part of the Quartus II distribution, as well as Qsys (a GUI tool to aid building a system-on-a-chip from resuable components). Whilst it appeared powerful, I struggled to get my testing attempts to work; I was also not comfortable with the non-free nature of the CPU.
  * [OpenRISC][or] is a fully open source CPU architecture standard with a number of mature implementations (such as [OR1200][or1200] and [mork1x][mor1kx]). This processor satisified all of my requirements and seemed like a good fit. After attempting to figure my way around the many parameters and ports provided the implementations, I came across [ORPSOC][orpsoc], a set of tools similar to Qsys designed to aid in interconnecting a number of on-chip components. Whilst my initial tests worked, I found I was fighting a losing battle against delay slots, long recompilation times and a lack of a working C compiler (neither the LLVM or GCC ports worked correctly on my computer).

[nios]: https://www.altera.com/products/processors/overview.html
[or]: http://opencores.org/or1k/Main_Page
[or1200]: http://opencores.org/or1k/OR1200_OpenRISC_Processor
[mor1kx]: https://github.com/openrisc/mor1kx
[orpsoc]: https://github.com/olofk/fusesoc

Eventually I decided to write my own CPU. The final product was `k9m100`, a CPU designed to have a small instruction set (to reduce size as well as the complexity of the instruction decoder) whilst still maintaining high instruction throughput. The CPU also utilises a simpler memory bus than the Avalon bus used in Nios and the Wishbone bus used in OpenRISC. A separate bus is provided for IO to reduce the 
