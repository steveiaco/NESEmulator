#include "cpu6502.h"

void cpu6502::write(uint16_t addr, uint8_t data)
{
	bus->write(addr, data);
}

uint8_t cpu6502::read(uint16_t addr)
{
	return bus->read(addr, false);
}

uint8_t cpu6502::fetch()
{
	if (lookup[opcode].addrmode != cpu6502::IMP)
	{
		fetched = read(addr_abs);
	}
	return fetched;
}

cpu6502::cpu6502()
{
	using a = cpu6502;
	lookup =
	{
		{ "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
		{ "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
	};
}

void cpu6502::clock()
{
	// If cycles is equal to zero, then the current opcode is finished processing and we can move onto loading and executing the next one
	if (cycles == 0) 
	{
		// We read the place in memory in which the program counter is pointing to, and store that value (it represents the next opcode to process)
		opcode = read(pc);

		// Increment program counter
		pc++;

		// Store the number of cycles this next opcode will take to complete
		cycles = lookup[opcode].cycles;

		// Change the addressing mode to the one specified in the opcode
		uint8_t needsAdditionalCycle1 = (this->*lookup[opcode].addrmode)();

		// Execute the operation
		uint8_t needsAdditionalCycle2 = (this->*lookup[opcode].operate)();

		// If both the addressing mode and opcode execution specify they need an additional cycle, then we add one
		cycles += (needsAdditionalCycle1 & needsAdditionalCycle2);
	}

	// Decrement the number of cycles left to complete the current opcode being executed
	cycles--;
}


/** ADDRESSING MODES **/

/* NON-INDEXED, NON-MEMORY MODES */

// Implied addressing / Accumulator
uint8_t cpu6502::IMP()
{
	// In certain cases we will have accumulator addressing, we will catch this using the implied addressing method
	// Therefore, every time we use implied addressing, we copy the contents of the accumulator into our fetched variable
	fetched = a;
	return 0;
}

// Immediate addressing
uint8_t cpu6502::IMM()
{
	// The data is supplied as part of the instruction, therefore the value after the opcode is the data
	addr_abs = pc++;
	return 0;

}

/* NON-INDEXED MEMORY MODES */

// Relative addressing, ONLY USED FOR BRANCHING
uint8_t cpu6502::REL()
{
	// program counter points to the relative offset
	addr_rel = read(pc);
	pc++;

	// if the offset is negative, then fill the most significant bits with 0
	if (addr_rel & 0x80)
		addr_rel |= 0xFF00;
	return 0;
}

// Absolute addressing
uint8_t cpu6502::ABS()
{
	// 6502 is little endian, so we read the little end, and then the big end
	uint8_t little_end = read(pc++);
	uint8_t big_end = read(pc++);

	// then we concatenate them together to form a 16 bit word
	addr_abs = (big_end << 8) | little_end;
}

// Zero page addressing
uint8_t cpu6502::ZP0()
{
	// zero page addressing allows us to skip reading the high address bits, as we assume the data we need is in the first page

	// first we read the contents of the low byte
	addr_abs = read(pc);
	pc++;

	// make sure the most siginificant byte is zeroed out
	addr_abs &= 0x00FF;
	return 0;
}

// Indirect addressing
uint8_t cpu6502::IND()
{
	// 6502 is little endian
	// here we are effectively reading a pointer
	uint8_t little_end = read(pc++);	// represents the location on the page
	uint8_t big_end = read(pc++);		// represents the page

	uint16_t ptr16 = (big_end << 8) | little_end;

	// BUG: there is a bug in the 6502 cpu, we must emulate it here
	// When the vector of an indirect address begins at the last byte of a page, the second byte is fetched from the beginning of that page rather than the beginning of the next.
	if (little_end == 0xFF)
	{
		addr_abs = (read(ptr16 & 0xFF00) << 8) | read(ptr16);
	}
	else
	{
		addr_abs = (read(ptr16 + 1) << 8) | read(ptr16);
	}

	return 0;
}

/* INDEXED MEMORY MODES */

// Absolute Indexed X
uint8_t cpu6502::ABX()
{
	// 6502 is little endian, so we read the little end, and then the big end
	uint8_t little_end = read(pc++);
	uint8_t big_end = read(pc++);

	// then we concatenate them together to form a 16 bit word
	addr_abs = ((big_end << 8) | little_end) + x;

	// if we change page, then we must signal that we might need an extra cpu cycle
	if ((big_end << 8) != (addr_abs & 0xFF00))
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

// Absolute Indexed Y
uint8_t cpu6502::ABY()
{
	// 6502 is little endian, so we read the little end, and then the big end
	uint8_t little_end = read(pc++);
	uint8_t big_end = read(pc++);

	// then we concatenate them together to form a 16 bit word
	addr_abs = ((big_end << 8) | little_end) + y;

	// if we change page, then we must signal that we might need an extra cpu cycle
	if ((big_end << 8) != (addr_abs & 0xFF00))
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

// Zero Page Indexed X
uint8_t cpu6502::ZPX()
{
	addr_abs = read(pc++) + x;

	// If we overflow into the next page, then wrap around
	addr_abs &= 0x00FF;
	return 0;	
}

// Zero Page Indexed Y
uint8_t cpu6502::ZPY()
{
	addr_abs = read(pc++) + y;

	// If we overflow into the next page, then wrap around
	addr_abs &= 0x00FF;
	return 0;
}

// Indexed Indirect
uint8_t cpu6502::IZX()
{
	return uint8_t();
}

// Indirect Indexed
uint8_t cpu6502::IZY()
{
	return uint8_t();
}

/* OPCODE IMPLEMENTATIONS */

#pragma region Arithmetic

// Add with carry
uint8_t cpu6502::ADC()
{
	fetch();

	// carry out operation
	uint16_t t = a + fetched + GetFlag(C);

	// Get rid of the most significant byte and assign the new accumulator value
	a = t & 0x00FF;

	// Check for negative sign
	SetFlag(N, t & 0x80);

	// Check for overflow
	SetFlag(V, ((a ^ t) & ~(a ^ fetched)) & 0x80);

	// Check for zero
	SetFlag(Z, !(t & 0x00FF));

	// Check for carry
	SetFlag(C, t & 0xFF00);

	return 1;
}

// Subtract with carry
uint8_t cpu6502::SBC()
{
	fetch();

	//invert the bits of the fetched value (to facilitate two's compliment)
	uint16_t inv = fetched ^ 0xFF;

	uint16_t t = a + inv + GetFlag(C);

	// Get rid of the most significant byte and assign the new accumulator value
	a = t & 0x00FF;

	// Check for negative sign
	SetFlag(N, t & 0x80);

	// Check for overflow
	SetFlag(V, ((a ^ t) & ~(a ^ fetched)) & 0x80);

	// Check for zero
	SetFlag(Z, !(t & 0x00FF));

	// Check for carry
	SetFlag(C, t & 0xFF00);

	return uint8_t();
}

#pragma endregion

#pragma region Logical operations

// Bitwise AND with accumulator
uint8_t cpu6502::AND()
{
	fetch();

	// Perform AND operation
	a = a & fetched;

	// Check for flags that may need to be set
	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0x00);

	// AND may require an additional clock cycle if a page boundary is crossed
	return 1;
}

// Bitwise OR with accumulator
uint8_t cpu6502::ORA()
{
	fetch();

	// Perform AND operation
	a = a | fetched;

	// Check for flags that may need to be set
	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0x00);

	// AND may require an additional clock cycle if a page boundary is crossed
	return 1;
}

// Bitwise XOR
uint8_t cpu6502::ORA()
{
	fetch();

	// Perform AND operation
	a = a ^ fetched;

	// Check for flags that may need to be set
	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0x00);

	// AND may require an additional clock cycle if a page boundary is crossed
	return 1;
}



// Arithmetic Shift Left
uint8_t cpu6502::ASL()
{
	fetch();

	// Shift left by 1
	uint16_t temp = fetched << 1;

	SetFlag(N, temp & 0x80);
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	// Set Carry to the value of the most significant bit of the fetched byte 
	SetFlag(C, temp & 0x0100);

	if (lookup[opcode].addrmode == &cpu6502::IMP)
	{
		a = temp & 0x00FF;
	}
	else
	{
		write(addr_abs, temp & 0x00FF);
	}

	return 0;
}

// Logical Shift right
uint8_t cpu6502::LSR()
{
	fetch();

	// Shift left by 1
	uint16_t temp = fetched >> 1;

	SetFlag(N, temp & 0x80);
	SetFlag(Z, (temp & 0x00FF) == 0x00);
	// Set Carry to the value of the most significant bit of the fetched byte 
	SetFlag(C, temp & 0x0100);

	if (lookup[opcode].addrmode == &cpu6502::IMP)
	{
		a = temp & 0x00FF;
	}
	else
	{
		write(addr_abs, temp & 0x00FF);
	}

	return 0;
}

#pragma endregion

#pragma region Branch operations

// Branch on plus (positive)
uint8_t cpu6502::BPL()
{
	if (!GetFlag(N))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}

// Branch on minus (negative)
uint8_t cpu6502::BMI()
{
	if (GetFlag(N))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}

// Branch on overflow clear
uint8_t cpu6502::BVS()
{
	if (!GetFlag(V))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}

// Branch on overflow set
uint8_t cpu6502::BVS()
{
	if (GetFlag(V))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}

// Branch on Carry Clear
uint8_t cpu6502::BCC()
{
	if (!GetFlag(C))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}

// Branch on Carry Set
uint8_t cpu6502::BCS()
{
	if (GetFlag(C))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}

// Branch on not equal
uint8_t cpu6502::BEQ()
{
	if (!GetFlag(Z))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}


// Branch on equal
uint8_t cpu6502::BEQ()
{
	if (GetFlag(Z))
	{
		// If we branch, we take one extra cycle
		cycles++;

		addr_abs = pc + addr_rel;

		// If the address of the branch destination and the address of the instruction after the branch instruction are on a different page, then add one cycle
		if ((addr_abs & 0xFF00) ^ (pc & 0xFF00))
		{
			cycles++;
		}

		pc = addr_abs;
	}

	return 0;
}
#pragma endregion

uint8_t cpu6502::BIT()
{
	fetch();

	uint8_t t = fetched & a;

	SetFlag(Z, t == 0x00);
	SetFlag(N, fetched & (1 << 7));
	SetFlag(V, fetched & (1 << 6));
	return 0;
}