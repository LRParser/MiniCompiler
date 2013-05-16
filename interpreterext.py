#!/usr/bin/python
#
# interpreterext.py -
#		A python implementation of the mini language, with user-defined
#		functions
#
# Kurt Schmidt
# 7/07
#
# EDITOR:  cols=80, tabstop=2
#
# NOTES:
#		the display() method everybody has is just to graphically spit the
#		actual parse tree to the screen
#
#		The grammar can be found in programext.py  (probably should be here)
#
#

import sys
from programext import *


# Debug Flag
DEBUG = None

######   LEXER   ###############################
# Note:  This is precisely the same lexer that exp1 uses.  Could've pulled
# it out to a different file.

from ply import lex

tokens = (
	'PLUS',
	'MINUS',
	'TIMES',
	'LPAREN',
	'RPAREN',
	'SEMICOLON',
	'COMMA',
	'NUMBER',
	'ASSIGNOP',
	'WHILE',
	'DO',
	'OD',
	'IF',
	'THEN',
	'ELSE',
	'FI',
	'DEFINE',
	'PROC',
	'END',
	'IDENT'
)

	# These are all caught in the IDENT rule, typed there.
reserved = {
		'while' : 'WHILE',
		'do'		: 'DO',
		'od'		: 'OD',
		'if'		: 'IF',
		'then'	: 'THEN',
		'else'	: 'ELSE',
		'fi'		: 'FI',
		'define': 'DEFINE',
		'proc'	: 'PROC',
		'end'		: 'END'
		}

# Now, this section.  We have a mapping, REs to token types (please note
# the t_ prefix).  They simply return the type.

	# t_ignore is special, and does just what it says.  Spaces and tabs
t_ignore = ' \t'

	# These are the simple maps
t_PLUS		= r'\+'
t_MINUS   = r'-'
t_TIMES		= r'\*'
t_LPAREN	= r'\('
t_RPAREN	= r'\)'
t_ASSIGNOP = r':='
t_SEMICOLON = r';'
t_COMMA		= r','

def t_IDENT( t ):
	#r'[a-zA-Z_][a-zA-Z_0-9]*'
	r'[a-zA-Z]+'
	t.type = reserved.get( t.value, 'IDENT' )    # Check for reserved words
	return t

def t_NUMBER( t ) :
	r'[0-9]+'

		# t.value holds the string that matched.  Dynamic typing - no unions
	t.value = int( t.value )
	return t

	# These are standard little ditties:
def t_newline( t ):
  r'\n+'
  t.lexer.lineno += len( t.value )

  # Error handling rule
def t_error( t ):
  print "Illegal character '%s' on line %d" % ( t.value[0], t.lexer.lineno )
  return t
  #t.lexer.skip( 1 )

lex.lex()

#-----   LEXER (end)   -------------------------------


######   YACC   #####################################

import ply.yacc as yacc

	# create a function for each production (note the prefix)
	# The rule is given in the doc string
def get_compile_functions(program):

  def translate():
    log.info('Translating Program')
    instructions = program.translate()
    symOut = open('symOut.txt','w')
    for inst in instructions :
      symOut.write(inst.symbolicStr() + '\n')
    return instructions

  def optimize(instructions_input):
    log.info('Optimizing Program')
    return program.optimize(instructions_input)


  def link(instructions_input, outfile):
    log.info('Linking Program')
    instructions = program.link(instructions_input)
    linkedOut = open(outfile,'w')
    for inst in instructions :
      linkedOut.write(str(inst)+'\n')
    return instructions

  def memdump(outfile):
    log.info('Printing memory table')
    memOut = open(outfile,'w')
    memLines = program.getMemoryTable()

    for line in memLines :
      memOut.write(str(line)+'\n')

  return translate, optimize, link, memdump

def p_program( p ) :
  'program : stmt_list'

  mainIdent = "main"
  mainArgs = [0]
  mainParams = ["n"]
  returnStmt = AssignStmt("return",Number(0))

  defineStmts = StmtList()
  otherStmts = StmtList()

  for stmt in p[1].sl :
    if(isinstance(stmt,DefineStmt)) :
      defineStmts.insert(stmt)
    else :    
      otherStmts.append(stmt)

  otherStmts.append(returnStmt)

  mainProc = Proc(mainParams,otherStmts)
  defineMainStmt = DefineStmt(mainIdent,mainProc)
  defineStmts.append(defineMainStmt)

  mainFunCall = FunCall(mainIdent,mainArgs)
  assignStmt = AssignStmt("return",mainFunCall)
  #otherStmts.insert(assignStmt) # Call main as 1st thing

  allStmts = StmtList()
  allStmts.insert(defineStmts)
  allStmts.insert(assignStmt)
#  allStmts.insert(otherStmts)
  P = Program(allStmts)
#  P = Program( p[1] )
  #P.display()
  #print 'Running Program'
  #P.eval()
  #P.dump()

  translate, optimize, link, memdump = get_compile_functions(P)

  #first run without optimizations
  instructions = translate()
  link(instructions, "program-non-opt.txt")
  memdump("mem-dump.txt")

  GLOBAL_SYMBOL_TABLE.clear()
  LABEL_FACTORY.count = 0
  TEMP_VARIABLE_FACTORY.count = 0

  #now optimize (requires re-translate)
  instructions = translate()
  instructions = optimize(instructions)
  link(instructions, "program-opt.txt")
  memdump("mem-dump-opt.txt")


def p_stmt_list( p ) :
 '''stmt_list : stmt SEMICOLON stmt_list
       | stmt'''
 if len( p ) == 2 :  # single stmt => new list
   p[0] = StmtList()
   p[0].insert( p[1] )
 else :  # we have a stmtList, keep adding to front
   p[3].insert( p[1] )
   p[0] = p[3]

def p_stmt( p ) :
	'''stmt : assign_stmt
				| while_stmt
				| if_stmt
				| define_stmt'''
	p[0] = p[1]

def p_add( p ) :
	'expr : expr PLUS term'
	p[0] = Plus( p[1], p[3] )

def p_sub( p ) :
	'expr : expr MINUS term'
	p[0] = Minus( p[1], p[3] )

def p_expr_list( p ) :
  '''expr_list : expr COMMA expr_list
              | expr'''
  if len( p ) == 2 :  # single expr => new list
    p[0] = [ p[1] ]
  else :  # we have a expr_list, keep adding to front
    p[3].insert( 0, p[1] )
    p[0] = p[3]

def p_expr_term( p ) :
	'expr : term'
	p[0] = p[1]

def p_mult( p ) :
	'''term : term TIMES fact'''
	p[0] = Times( p[1], p[3] )

def p_term_fact( p ) :
	'term : fact'
	p[0] = p[1]

def p_fact_expr( p ) :
	'fact : LPAREN expr RPAREN'
	p[0] = p[2]

def p_fact_NUM( p ) :
	'fact : NUMBER'
        log.debug("p_fact_NUMBER")
	p[0] = Number( p[1] )

def p_fact_IDENT( p ) :
	'fact : IDENT'
	log.debug("p_fact_IDENT")
        p[0] = Ident( p[1] )

def p_fact_funcall( p ) :
	'fact : func_call'
	p[0] = p[1]

def p_assn( p ) :
	'assign_stmt : IDENT ASSIGNOP expr'
	p[0] = AssignStmt( p[1], p[3] )

def p_while( p ) :
	'while_stmt : WHILE expr DO stmt_list OD'
	p[0] = WhileStmt( p[2], p[4] )

def p_if( p ) :
	'if_stmt : IF expr THEN stmt_list ELSE stmt_list FI'
	p[0] = IfStmt( p[2], p[4], p[6] )

def p_def( p ) :
  'define_stmt : DEFINE IDENT PROC LPAREN param_list RPAREN stmt_list END'
  p[0] = DefineStmt( p[2], Proc( p[5], p[7] ))

def p_param_list( p ) :
  '''param_list : IDENT COMMA param_list
              | IDENT'''
  if len( p ) == 2 :  # single param => new list
    p[0] = [ p[1] ]
  else :  # we have a param_list, keep adding to front
    p[3].insert( 0, p[1] )
    p[0] = p[3]

def p_func_call( p ) :
  'func_call : IDENT LPAREN expr_list RPAREN'
  p[0] = FunCall( p[1], p[3] )


# Error rule for syntax errors
def p_error( p ):
	print "Syntax error in input!", str( p )
	sys.exit( 2 )

	# now, build the parser
yacc.yacc()


######   MAIN   #################################

def _debugMessage(message):
    """Will write a debug message to the screen.
       Args:
            message: Data to be printed out for debug.
       Returns:
            NONE
    """
    if DEBUG:
        print message


def test_scanner(data) :
    """ Test the lexer to make sure we
    don't have any invalid tokens.

    :param data: string data from either
                 a file or text input.
    """
    lex.input(data)

    # attempt to get that first token
    tok = lex.token()
    while tok:
        tok = lex.token()


def test_parser(data) :
    """ Test method for the parser.

    :param data: string data from either
                 a file or text input.
    """
    yacc.parse(data)


def main() :
    """ Main method.
        Will process file input or text input
        and will execute the scanner and the parser.
    """
    data = []
    nArgs = len(sys.argv) - 1
    if nArgs == 1:
        # One argument, hopefully it a filename.
        # If not, it will error out when attempting
        # to open.
        try:
            fSpec = sys.argv[1]
            _debugMessage("Reading %s" % fSpec)
            data = open(fSpec, 'r').read()
        except Exception as e:
            print "({0}): {1}".format(type(e), e.message)
    elif nArgs < 1:
        # No Arguments, just enter manual mode.
        data=sys.stdin.read()

    _debugMessage("Input program is: ")
    _debugMessage(data)
    _debugMessage("End input program")
    _debugMessage("Call lexer")
    test_scanner(data)
    _debugMessage("Call parser")
    test_parser(data)

if __name__ == '__main__':
    main()
