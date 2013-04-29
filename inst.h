// definition of an instruction
//
// g++ (Ubuntu 4.4.1-4ubuntu9) 4.4.1 on
// Linux 2.6.31-23-generic 
//
// Editor:  tabstop=2, cols=80
//
// 5/11 - add constructors to Instruction, KS
// 5/11 - added MUL and JMZ, KS
// 

#ifndef INSTHEADER
#define INSTHEADER

enum OPCODES
{ ILL, LDA, LDI, STA, STI, ADD, SUB, MUL, JMP, JMI, JMZ, JMN, CAL, HLT } ;
	// ILL, an illegal (nonexistent) instruction

enum MEM_LOCATIONS
{ SP=1 };

struct Instruction {
	OPCODES opcode;
	int operand;
	Instruction( OPCODES code ) : opcode(code) {}
	Instruction( OPCODES code, int o ) : opcode(code), operand(o) {}
};

#endif
