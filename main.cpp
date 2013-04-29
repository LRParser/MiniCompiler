// RAM interpreter
// Purpose: To simulate the execution of a RAM (random access machine)
// Author: Jeremy Johnson
// Date: 9/25/00
//
// g++ (Ubuntu 4.4.1-4ubuntu9) 4.4.1 on
// Linux 2.6.31-23-generic 
//
// Editor:  tabstop=2, cols=80
//

#include <iostream>
#include <string>
#include <cstdlib>

using namespace std;

#include <unistd.h>
#include <stdio.h>
#include "ram.h"
#include "inst.h"

const char *OPT_FLAGS = "m:" ;

void usage( char* ) ;

int main( int argc, char** argv )
{
	int mSize = 0 ;
	string pName, mName ;

	RAM M;  // The (only) machine

	int c ;
	while( (c=getopt( argc, argv, OPT_FLAGS )) != -1 ) {
		switch( c ) {
			case 'm' :  mSize = atoi( optarg ) ; break ;
			case '?' :  usage( argv[0] ) ; exit( 1 ) ; break ;
		}
	}
				
	if( argc < optind + 2 ) {
		usage( argv[0] ) ;
		exit( 1 ) ;
	}

	pName = argv[optind] ;
	mName = argv[optind+1] ;

	M.init( pName, mName, mSize );  // Initialize RAM with program in pName and
	                            // initial memory configuration in mName

	cout << "Initial Memory Configuration" << endl;
	M.dump();

	M.execute();  // Execute RAM with given program and memory configuration

	cout << endl << "Final Memory Configuration" << endl;
	M.dump();
}

void usage( char *progname ) {
	fprintf( stderr, "Usage:\n" ) ;
	fprintf( stderr, "   %s [-m memSize] progFile memFile\n", progname ) ;
}
