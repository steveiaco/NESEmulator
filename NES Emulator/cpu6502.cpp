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


#pragma region ADDRESSING MODES

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

#pragma endregion

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

// Bitwise OR
uint8_t cpu6502::ORA()
{
	fetch();

	// Perform OR operation
	a = a | fetched;

	// Check for flags that may need to be set
	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0x00);

	// OR may require an additional clock cycle if a page boundary is crossed
	return 1;
}

// Bitwise XOR
uint8_t cpu6502::EOR()
{
	fetch();

	// Perform XOR operation
	a = a ^ fetched;

	// Check for flags that may need to be set
	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0x00);

	// XOR may require an additional clock cycle if a page boundary is crossed
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

	// Shift right by 1
	SetFlag(C, fetched & 0x0001);

	uint16_t temp = fetched >> 1;

	SetFlag(N, temp & 0x0080);
	SetFlag(Z, (temp & 0x00FF) == 0x0000);

	// Set Carry to the value of the most significant bit of the fetched byte 
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

// rotate bits left
uint8_t cpu6502::ROL()
{
	fetch();

	uint16_t temp = (fetched << 1) | GetFlag(C);

	SetFlag(C, temp & 0xFF00);
	SetFlag(Z, (temp & 0x00FF) == 0);
	SetFlag(N, temp & 0x0080);

	if (lookup[opcode].addrmode == &cpu6502::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);

	return 0;
}

// rotate bits right
uint8_t cpu6502::ROR()
{
	fetch();

	uint16_t temp = (fetched >> 1) | (GetFlag(C) << 7);
	

	SetFlag(C, fetched & 0x0001);
	SetFlag(Z, (temp & 0x00FF) == 0);
	SetFlag(N, temp & 0x0080);

	if (lookup[opcode].addrmode == &cpu6502::IMP)
		a = temp & 0x00FF;
	else
		write(addr_abs, temp & 0x00FF);

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
uint8_t cpu6502::BVC()
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
uint8_t cpu6502::BNE()
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

#pragma region Stack

// In this implementation, the stack pointer always points to empty space on the stack

// Push accumulator to stack
uint8_t cpu6502::PHA()
{
	// 0x0100 represents the page the stack is hardcoded to live
	write(STACK_PAGE + stkp, a);
	stkp--;;
	return 0;
}

// Push processor status to stack
uint8_t cpu6502::PHP()
{
	write(STACK_PAGE + stkp, status);

	stkp--;

	return 0;
}

// Pop stack value to accumulator
uint8_t cpu6502::PLA()
{
	stkp++;

	// 0x0100 represents the page the stack is hardcoded to live
	a = read(STACK_PAGE + stkp);

	// Check for flags that may need to be set
	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0x00);

	return 0;
}

// Pop process status into the cpu status flags
uint8_t cpu6502::PLP()
{
	stkp++;

	// 0x0100 represents the page the stack is hardcoded to live
	status = read(STACK_PAGE + stkp);

	SetFlag(U, 1);

	return 0;
}

#pragma endregion

#pragma region Reset and Interrupts

// reset cpu to initial state
void cpu6502::reset()
{
	a = 0;
	x = 0;
	y = 0;
	stkp = 0xFD; //todo why is fd instead of ff?
	status = 0x00 | U;
	addr_rel = 0;
	addr_abs = 0;
	fetched = 0;

	// when the 6502 is reset, it always looks at 0xFFFC to find the program start address
	uint16_t low = read(RESET_VECTOR);
	uint16_t high = read(RESET_VECTOR + 1);

	// set the program counter to the address we read from 0xFFFC-0xFFFD
	pc = (high << 8) | low;


	cycles = 8;
}

// interrupt request
void cpu6502::irq()
{
	if (GetFlag(I) == 0)
	{
		// write the program counter to the stack
		// start with the high part
		write(STACK_PAGE + stkp, (pc >> 8) & 0x00FF);
		stkp--;
		write(STACK_PAGE + stkp, pc & 0x00FF);
		stkp--;

		SetFlag(B, 0);
		SetFlag(U, 1);
		SetFlag(I, 1);

		// write the cpu status to the stack
		write(STACK_PAGE + stkp, status);
		stkp--;

		// read the interrupt memory location
		pc = (read(M_INTERRUPT_VECTOR + 1) << 8) | read(M_INTERRUPT_VECTOR);

		cycles = 7;
	}
}

// non-maskable interrupt request
void cpu6502::nmi()
{
	// write the program counter to the stack
	// start with the high part
	write(STACK_PAGE + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(STACK_PAGE + stkp, pc & 0x00FF);
	stkp--;

	SetFlag(B, 0);
	SetFlag(U, 1);
	SetFlag(I, 1);

	// write the cpu status to the stack
	write(STACK_PAGE + stkp, status);
	stkp--;

	// read the interrupt memory location
	pc = (read(NM_INTERRUPT_VECTOR + 1) << 8) | read(NM_INTERRUPT_VECTOR);

	cycles = 8;
}

// Return the state of the cpu to what it was before the interrupt
uint8_t cpu6502::RTI()
{
	// start off by popping the status off of the stack
	stkp++;
	status = read(STACK_PAGE + stkp);
	status &= ~B;
	status &= ~U;

	// then read the low part of the program counter
	stkp++;
	uint8_t low = read(STACK_PAGE + stkp);

	// then the high part of the program counter
	stkp++;
	uint8_t high = read(STACK_PAGE + stkp);

	// now we can restore the program counter
	pc = (high << 8) | low;

	return 0;
}

// Return from subroutine
uint8_t cpu6502::RTS()
{
	// then read the low part of the program counter
	stkp++;
	uint8_t low = read(STACK_PAGE + stkp);

	// then the high part of the program counter
	stkp++;
	uint8_t high = read(STACK_PAGE + stkp);

	// now we can restore the program counter
	pc = (high << 8) | low;

	pc++;

	return 0;
}

// break, interrupt coming from program
uint8_t cpu6502::BRK()
{
	pc++;

	SetFlag(I, true);

	// write the high part of the PC to the stack
	write(STACK_PAGE + stkp, (pc >> 8) & 0x00FF);
	stkp--;

	// write the low part to the stack
	write(STACK_PAGE + stkp, pc & 0x00FF);
	stkp--;

	// set the break flag to 1, this only shows in the status byte written to the stack (only happens with PHP and BRK according to the NESDEV wiki)
	SetFlag(B, true);

	// write the status to the stack
	write(STACK_PAGE + stkp, status);

	// set the break flag back to 0
	SetFlag(B, false);

	// read the low and high byte and combine them into a 16 bit word for the pc
	pc = read(M_INTERRUPT_VECTOR) | (read(M_INTERRUPT_VECTOR + 1) << 8);
}

#pragma endregion

#pragma region Flag Operations

// Sets the Z flag as though the value in the address tested were ANDed with the accumulator. S and V match bits 7 and 6 at the fetched address
uint8_t cpu6502::BIT()
{
	fetch();

	uint8_t t = fetched & a;

	SetFlag(Z, t == 0x00);
	SetFlag(N, fetched & (1 << 7));
	SetFlag(V, fetched & (1 << 6));
	return 0;
}

// clear carry bit
uint8_t cpu6502::CLC()
{
	SetFlag(C, false);
	return 0;
}

// clear decimal bit
uint8_t cpu6502::CLD()
{
	SetFlag(D, false);
	return 0;
}

// clear interrupt disable bit
uint8_t cpu6502::CLI()
{
	SetFlag(I, false);
	return 0;
}

// clear overflow bit
uint8_t cpu6502::CLV()
{
	SetFlag(V, false);
	return 0;
}

// set carry flag
uint8_t cpu6502::SEC()
{
	SetFlag(C, true);
	return 0;
}

// set decimal flag
uint8_t cpu6502::SED()
{
	SetFlag(D, true);
	return 0;
}

// set interrupt disable flag
uint8_t cpu6502::SEI()
{
	SetFlag(I, true);
	return 0;
}

#pragma endregion


// compare memory to accumulator, subtraction (a - m)
uint8_t cpu6502::CMP()
{
	fetch();

	uint8_t temp = a - fetched;

	SetFlag(N, a >= fetched);
	SetFlag(Z, a == fetched);
	SetFlag(C, temp & 0x80);

	return 1;
}

// compare x register to memory
uint8_t cpu6502::CPX()
{
	fetch();

	uint8_t temp = x - fetched;

	SetFlag(N, x >= fetched);
	SetFlag(Z, x == fetched);
	SetFlag(C, temp & 0x80);

	return 1;
}

// compare y register to memory
uint8_t cpu6502::CPY()
{
	fetch();

	uint8_t temp = y - fetched;

	SetFlag(N, y >= fetched);
	SetFlag(Z, y == fetched);
	SetFlag(C, temp & 0x80);

	return 1;
}

// decrement memory, subtracts one from the value held at a specific memory location
uint8_t cpu6502::DEC()
{
	fetch();
	
	uint8_t decremented = fetched - 1;

	write(addr_abs, decremented);

	SetFlag(N, decremented & 0x80);
	SetFlag(Z, decremented == 0);

	return 0;
}

// decrement the x register
uint8_t cpu6502::DEX()
{
	--x;

	SetFlag(N, x & 0x80);
	SetFlag(Z, x == 0);

	return 0;
}

// decrement the y register
uint8_t cpu6502::DEY()
{
	--y;

	SetFlag(N, y & 0x80);
	SetFlag(Z, y == 0);

	return 0;
}

// increment the data at memory location
uint8_t cpu6502::INC()
{
	fetch();

	uint8_t incremented = fetched + 1;

	write(addr_abs, incremented);

	SetFlag(N, incremented & 0x80);
	SetFlag(Z, incremented == 0);

	return 0;
}

// increment the x register
uint8_t cpu6502::INX()
{
	++x;

	SetFlag(N, x & 0x80);
	SetFlag(Z, x == 0);

	return 0;
}

// increment the y register
uint8_t cpu6502::INY()
{
	++y;

	SetFlag(N, y & 0x80);
	SetFlag(Z, y == 0);

	return 0;
}

// jump to address specified
uint8_t cpu6502::JMP()
{
	pc = addr_abs;
	return 0;
}

// jump to subroutine
uint8_t cpu6502::JSR()
{
	--pc;

	// write the high part of the pc to the stack
	write(STACK_PAGE + stkp, (pc >> 8) & 0x00FF);
	stkp--;

	// write the low part of the pc to the stack
	write(STACK_PAGE + stkp, pc & 0x00FF);
	stkp--;

	// this can only be called in absolute mode, so no need to fetch
	pc = addr_abs;

	return 0;

}

// load to accumulator
uint8_t cpu6502::LDA()
{
	fetch();
	a = fetched;

	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0);

	return 1;
}

// load to x register
uint8_t cpu6502::LDX()
{
	fetch();
	x = fetched;

	SetFlag(N, x & 0x80);
	SetFlag(Z, x == 0);

	return 1;
}

// load to y register
uint8_t cpu6502::LDY()
{
	fetch();
	y = fetched;

	SetFlag(N, y & 0x80);
	SetFlag(Z, y == 0);

	return 1;
}

// no operation
uint8_t cpu6502::NOP()
{
	// Many illegal op codes route to this one, and can cause different behavior
	switch (opcode)
	{
	case 0x1C:
	case 0x3C:
	case 0x5C:
	case 0x7C:
	case 0xDC:
	case 0xFC:
		return 1;
		break;
	}
	return 0;
}

// store the contents of the accumulator into memory
uint8_t cpu6502::STA()
{
	write(addr_abs, a);
	return 0;
}

// store the contents of the x register into memory
uint8_t cpu6502::STX()
{
	write(addr_abs, x);
	return 0;
}

// store the contents of the y register into memory
uint8_t cpu6502::STY()
{
	write(addr_abs, y);
	return 0;
}

// copies the contents of the accumulator into the x register
uint8_t cpu6502::TAX()
{
	x = a;

	SetFlag(N, x & 0x80);
	SetFlag(Z, x == 0);

	return 0;
}

// copies the contents of the accumulator into the y register
uint8_t cpu6502::TAY()
{
	y = a;

	SetFlag(N, y & 0x80);
	SetFlag(Z, y == 0);

	return 0;
}

// copy stack pointer into x register
uint8_t cpu6502::TSX()
{
	x = stkp;

	SetFlag(N, x & 0x80);
	SetFlag(Z, x == 0);

	return 0;
}

// copy the contents of the x register into the accumulator
uint8_t cpu6502::TXA()
{
	a = x;

	SetFlag(N, x & 0x80);
	SetFlag(Z, x == 0);

	return 0;
}

// transfer x to stack pointer
uint8_t cpu6502::TXS()
{
	stkp = x;
	return 0;
}

// transfer y into a
uint8_t cpu6502::TYA()
{
	a = y;

	SetFlag(N, a & 0x80);
	SetFlag(Z, a == 0);

	return 0;
}

// all illegal opcodes go here
uint8_t cpu6502::XXX()
{
	return 0;
}