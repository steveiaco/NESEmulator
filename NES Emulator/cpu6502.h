#pragma once
#include <cstdint>
#include <vector>
#include "Bus.h"

class cpu6502
{
public: 
	
	/* Constructors and destructor */
	
	cpu6502();
	~cpu6502();

	/* Functions */

	// Setup bus
	void ConnectBus(Bus* b) { bus = b; }


	void clock();
	void reset();
	void irq();	// Interrupt request signal
	void nmi();	// Non-maskable interrupt request signal


	/* Member variables */

	uint8_t a = 0x00;	// Accumulator Register
	uint8_t x = 0x00;	// X Register
	uint8_t y = 0x00;	// Y Register
	uint8_t stkp = 0x00;	// Stack Pointer
	uint16_t pc = 0x00;		// Program Counter

	// Stores flag states
	uint8_t status = 0x00;	// Status Register

public: 
	
	/* Enums */

	enum FLAGS6502 
	{
		C = (1 << 0), // Carry Bit
		Z = (1 << 1), // Zero
		I = (1 << 2), // Disable interrupts
		D = (1 << 3), // Decimal mode
		B = (1 << 4), // Break
		U = (1 << 5), // Unused
		V = (1 << 6), // Overflow
		N = (1 << 7), // Negative
	};

	/* Structs */

	struct INSTRUCTION 
	{
		std::string name;
		uint8_t(cpu6502::*operate)(void) = nullptr;  // Pointer to one of the Opcode functions
		uint8_t(cpu6502::*addrmode)(void) = nullptr; // Pointer to one of the Addressing mode functions
		uint8_t cycles = 0;
	};

	//todo maybe change this to a map
	std::vector<INSTRUCTION> lookup;
	
private:

	/* Functions */

	uint8_t GetFlag(FLAGS6502 f);
	void SetFlag(FLAGS6502 f, bool v);

	// Read and write to bus
	void write(uint16_t addr, uint8_t data);
	uint8_t read(uint16_t addr);

	// Fetches data depending on the addressing mode of the current instruction byte.
	uint8_t fetch();

	// Addressing modes
	uint8_t IMP();	uint8_t IMM();
	uint8_t ZP0();	uint8_t ZPX();
	uint8_t ZPY();	uint8_t REL();
	uint8_t ABS();	uint8_t ABX();
	uint8_t ABY();	uint8_t IND();
	uint8_t IZX();	uint8_t IZY();

	// Opcodes
	uint8_t ADC();	uint8_t AND();	uint8_t ASL();	uint8_t BCC();
	uint8_t BCS();	uint8_t BEQ();	uint8_t BIT();	uint8_t BMI();
	uint8_t BNE();	uint8_t BPL();	uint8_t BRK();	uint8_t BVC();
	uint8_t BVS();	uint8_t CLC();	uint8_t CLD();	uint8_t CLI();
	uint8_t CLV();	uint8_t CMP();	uint8_t CPX();	uint8_t CPY();
	uint8_t DEC();	uint8_t DEX();	uint8_t DEY();	uint8_t EOR();
	uint8_t INC();	uint8_t INX();	uint8_t INY();	uint8_t JMP();
	uint8_t JSR();	uint8_t LDA();	uint8_t LDX();	uint8_t LDY();
	uint8_t LSR();	uint8_t NOP();	uint8_t ORA();	uint8_t PHA();
	uint8_t PHP();	uint8_t PLA();	uint8_t PLP();	uint8_t ROL();
	uint8_t ROR();	uint8_t RTI();	uint8_t RTS();	uint8_t SBC();
	uint8_t SEC();	uint8_t SED();	uint8_t SEI();	uint8_t STA();
	uint8_t STX();	uint8_t STY();	uint8_t TAX();	uint8_t TAY();
	uint8_t TSX();	uint8_t TXA();	uint8_t TXS();	uint8_t TYA();

	uint8_t XXX();



	/* Member variables */

	uint8_t fetched = 0x00;		// represents the working input value to the ALU
	uint16_t temp = 0x0000;		// temp variable
	uint16_t addr_abs = 0x0000;	// all memory addresses end up in here
	uint16_t addr_rel = 0x00;	// offset used for computing relative addresses
	uint8_t opcode = 0x00;		// the instruction byte
	uint8_t cycles = 0;			// the number of cycles the current instruction has remaining
	uint32_t clock_count = 0;	// accumulation of the number of clock ticks

	Bus* bus = nullptr;

};

