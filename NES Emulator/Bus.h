#pragma once
#include <cstdint>
#include "cpu6502.h"
#include <array>

class Bus
{
public: // Constructors and destructor

	Bus();
	~Bus();

public:	// Devices on the bus

	// CPU
	cpu6502 cpu;

	// Placeholder RAM
	std::array<uint8_t, 64 * 1024> ram;

public: // Bus read and write functions

	void write(uint16_t addr, uint8_t data);
	uint8_t read(uint16_t addr, bool bReadOnly = false);
};

