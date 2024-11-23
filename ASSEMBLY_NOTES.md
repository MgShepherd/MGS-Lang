
## Assembly Notes

Arm Processor uses 64 bit registers. 

### Registers

There are 31 general purpose registers which act as very fastmemory located directly on the CPU. These registers are X0-X30

There is a Stack Pointer SP which is used to help manage stack based memory.

There is a link register LR which is used to hold the return address of a function, allowing you to continue from the place you were in the program when you call another function

We also have the Program Counter PC which is the memory address of the instruction we are currently executing.

Each register is 64 bits, but sometimes we don't need to use all 64 bits. If we operate on each of the 64 bit registers as if they are 32 bits by replace X with W (e.g. the 32 bit version of register X0 is W0)

Using registers in this way allows us to use the lower 32 bits of the register and will set the upper 32 bits to zero.

### Instructions

Each ARM instruction is 32 bits long. To enable all the instruction information to fit into 32 bits, there is a specific structure used for each one.

5 bits are used to specify a register. Each instruction requires a maximum of 3 registers, meaning a maximum of 15 bits.

Each instruction can work with either the 32 or 64 bit version of registers, but you cannot mix 64 and 32 bit in the same instruction.

Each instruction takes 3 clock cycles to execute - one to load the instruction from memory, one to decode the instruction and one to execute. However, the ARM processor tries to reduce this down to just one clock cycle by operating on multiple instructions at the same time. Some instructions can however take longer than just a single clock Cycle

### Memory

Programs we write get stored in memory and then loaded to be executed. The memory will hold the program as well any data or variables which are associated with it. In modern ARM computers, memory addresses are 64 bits, the same as the CPU registers.

This can pose a problem when writing problems as we need to specify a 64 bit location in just a 32 bit instruction - there are a few techniques which can help us with this.


