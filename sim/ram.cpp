// Implementation of RAM class
//
// g++ (Ubuntu 4.4.1-4ubuntu9) 4.4.1 on
// Linux 2.6.31-23-generic 
//
// Editor:  tabstop=2, cols=80
//
// 5/21/11 - KS, added CAL instruction
// 5/11 - KS, hanged initialisation of RAM to use a given memory size.
// 5/11 - added MUL and JMZ, KS
//
// TODO:  assumes mem file lists locations in order.  Make into dict(?)
//


#include <iostream>
#include <fstream>
#include <string>
#include <stdlib.h>
using namespace std;

#include "ram.h"
#include "inst.h"

// Constructor
RAM::RAM() {
	pc = 1;  ac = 0;
		// indexing for RAM memory starts at 1; waste a space
	memory.push_back( 0 ) ;
		// indexing for RAM program starts at 1; waste a space
	program.push_back( Instruction( ILL, 0 )) ;
}

//RAM::RAM(int pSize, int mSize)
//{
//	memory = new int[memorySize+1];
//		// indexing for RAM memory starts at 1
//		// indexing for RAM program starts at 1
//	program = new Instruction[ programSize+1 ];
//	pc = 1;  ac = 0;
//	for( int i=0; i<=memorySize; i++ )
//		memory[i] = 0;
//}

void RAM::init()
	// Initialize RAM with hardwired program and memory
	// pc is set to 1 and ac is set to 0.
{
	program.push_back( Instruction( LDA, 3 ));
	program.push_back( Instruction( SUB, 4 ));
	program.push_back( Instruction( JMZ, 7 ));
	program.push_back( Instruction( LDA, 1 ));
	program.push_back( Instruction( STA, 5 ));
	program.push_back( HLT );
	program.push_back( Instruction( LDA, 2 ));
	program.push_back( Instruction( STA, 5 ));
	program.push_back( HLT );

	memory.push_back( 0 );
	memory.push_back( 1 );
	memory.push_back( 2 );
	memory.push_back( 1 );
	memory.push_back( 3 );
}

// Initialize RAM with program in file with the name pInput
// and initial memory configuration in the file with name mInput
// pc is set to 1 and ac is set to 0.  programSize is set to the number
// of instructions read.
void RAM::init( string pInput, string mInput, int req_size ) {
	// Initialize Memory
	int addr, value, cnt=1;
	
	string buf ;

	////   Initialize memory   //////////////////////
	ifstream mFile( mInput.c_str() );
	if( ! mFile ) {
		cerr << "Error: memory file " << mInput << " not found" << endl;
		exit(1);
	}

	while( mFile >> addr >> value ) {
		memory.push_back( value ) ;
		if( addr < 1 || addr > memorySize() ) {
			cerr << "Error:  illegal memory location.  " << endl;
			cerr << "Mem map must list locations in order" << endl;
			exit(1);
		}
		getline( mFile, buf );  // flush line (possibly contains comment)
	}
	mFile.close() ;

		// extend to requested size
	if( req_size > memorySize() )
		memorySize( req_size ) ; 

	////   Initialize program   //////////////////////

	ifstream pFile( pInput.c_str() );
	if (! pFile) {
		cerr << "Error: program file " << pInput << " not found" << endl;
		exit(1);
	}

	OPCODES code ;
	int operand ;

	while( pFile >> buf ) {
		if( buf[0] == ';' ) {       // comment
			getline( pFile, buf );    // flush to end of line
			continue ;	// nothing to store
		}
		else if( buf == "LDA" ) {
			code = LDA;
			pFile >> operand; 
			getline( pFile, buf ); } 
		else if( buf == "LDI" ) {
			code = LDI;
			pFile >> operand; 
			getline( pFile, buf ); }
		else if( buf == "STA" ) {
			code = STA;
			pFile >> operand;
			getline( pFile, buf ); }
		else if( buf == "STI" ) {
			code = STI;
			pFile >> operand; 
			getline( pFile, buf ); }
		else if( buf == "ADD" ) {
			code = ADD;
			pFile >> operand; 
			getline( pFile, buf ); }
		else if( buf == "SUB" ) {
			code = SUB;
			pFile >> operand; 
			getline( pFile, buf ); }
		else if( buf == "MUL" ) {
			code = MUL;
			pFile >> operand; 
			getline( pFile, buf ); }
		else if( buf == "JMP" ) {
			code = JMP;
			pFile >> operand;
			getline( pFile, buf ); }
		else if( buf == "JMI" ) {
			code = JMI;
			pFile >> operand;
			getline( pFile, buf ); }
		else if( buf == "JMZ" ) {
			code = JMZ;
			pFile >> operand;
			getline( pFile, buf ); }
		else if( buf == "JMN" ) {
			code = JMN;
			pFile >> operand;
			getline( pFile, buf ); }
		else if( buf == "CAL" ) {
			code = CAL;
			pFile >> operand;
			getline( pFile, buf ); }
		else if( buf == "HLT" ) {
			code = HLT;
			operand = -1 ;
			getline( pFile, buf ); }
		else {
			cerr << "Error:  Illegal Instruction: " << buf << endl;
			exit( 1 );
		}	// if

		program.push_back( Instruction( code, operand )) ;

	}	// while tokens 
} // Init( f, f )

// simulate execution of RAM with given program and memory configuration.
// Notes:
//    1. Program may not terminate (if HLT is not executed)
//    2. Currently no error checking is performed.  Checks for valid program 
//       and memory addresses and illegal opcodes should be provided.
void RAM::execute()
{
	int x;
	OPCODES op;
	bool halted = false;

		// start at first instruction
	pc = 1 ;

	while (!halted) {
		op = program[pc].opcode;
		switch (op) {
		case LDA:
			x = program[pc].operand;
			ac = memory[x];
			pc++;
			break;

		case LDI:
			x = program[pc].operand;
			ac = memory[memory[x]];
			pc++;
			break;

		case STA:
			x = program[pc].operand;
			memory[x] = ac;
			pc++;
			break;

		case STI:
			x = program[pc].operand;
			memory[memory[x]] = ac;
			pc++;
			break;

		case ADD:
			x = program[pc].operand;
			ac = ac + memory[x];
			pc++;
			break;

		case SUB:
			x = program[pc].operand;
			ac = ac - memory[x];
			pc++;
			break;

		case MUL:
			x = program[pc].operand;
			ac = ac * memory[x];
			pc++;
			break;

		case JMP:
			x = program[pc].operand;
			pc = x;
			break;

		case JMI:
			x = program[pc].operand;
			pc = memory[x];
			break;

		case JMZ:
			x = program[pc].operand;
			if (ac == 0)
				pc = x;
			else
				pc++;
			break;

		case JMN:
			x = program[pc].operand;
			if (ac < 0)
				pc = x;
			else
				pc++;
			break;

		case CAL :
				// NOTE  it is assumed that SP is in memory[0],
				//  that it points to the *last* location in the current (new)
				//  frame, and that the return address is stored there.
			x = program[pc].operand;
			memory[ memory[ SP ]] = pc+1 ;	// set the return address
			pc = x ;	// jump to the function
			break ;

		case HLT:
			halted = true ;
			break;

		default :  // shouldn't be here
			halted = true;
			fprintf( stderr, "Error:  Got opcode %d.\n\n", op );
		}	// switch
	}	// while not halted
} // execute()

// Dump memory contents

void RAM::dump()
{
	cout << "RAM Memory Contents" << endl;
	cout << endl;  
	for( int i=1; i<=memorySize(); i++ )
		cout << i << "   " << memory[i] << endl;
}

