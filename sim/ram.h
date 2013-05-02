// Definition of RAM (random access machine) class
//
// g++ (Ubuntu 4.4.1-4ubuntu9) 4.4.1 on
// Linux 2.6.31-23-generic 
//
// Editor:  tabstop=2, cols=80
//
// 5/11 - KS, hanged initialisation of RAM to use a given memory size.
// 5/11 - added MUL and JMZ, KS
//
// TODO:  assumes mem file lists locations in order.  Make into dict.
//

#ifndef __RAM_H_
#define __RAM_H_

#include <vector>
using namespace std;

#include "inst.h"

class RAM {
public:
	// Constructors
	RAM();
	//RAM(int pSize, int mSize);

	// Initialize RAM with hardwired program and memory
	// pc is set to 1 and ac is set to 0
	void init();

	// Initialize RAM with program in file with the name pInput
	// and initial memory configuration in the file with name mInput
	// pc is set to 1 and ac is set to 0.  programSize is set to the number
	// of instructions read.
	void init( string pFile, string mFile, int mSize );

	// simulate execution of RAM with given program and memory configuration.
	// Notes:
	//    1. Program may not terminate (if HLT is not executed)
	//    2. Currently no error checking is performed.  Checks for valid program 
	//       and memory addresses and illegal opcodes should be provided.
	void execute();

		// returns # of actual instructions or spaces.
		// Remember, 1-based
	int programSize() const { return program.size()-1 ; }
	int memorySize() const { return memory.size()-1 ; }
	void memorySize( int size )
	{ memory.resize( size+1 ) ; } // remember, 1-based index

	// Dump memory contents
	void dump();

private:
	vector<int> memory ;
	vector<Instruction> program ;
	int pc;
	int ac;
};

#endif //__RAM_H_
