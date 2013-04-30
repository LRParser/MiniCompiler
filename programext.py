#!/usr/bin/python
#
# exp.py - Classes to represent underlying data structures for the grammar
#	 below, for the mini-compiler.
#
# Kurt Schmidt
# 8/07
#
# DESCRIPTION:
#		Just a translation of the C++ implementation by Jeremy Johnson (see
#		programext.cpp)
#
# EDITOR: cols=80, tabstop=2
#
# NOTES
#	environment:
#		a dict
#
#		Procedure calls get their own environment, can not modify enclosing env
#
#	Grammar:
#		program: stmt_list 
#		stmt_list:  stmt ';' stmt_list 
#		    |   stmt  
#		stmt:  assign_stmt 
#		    |  define_stmt 
#		    |  if_stmt 
#		    |  while_stmt 
#		assign_stmt: IDENT ASSIGNOP expr
#		define_stmt: DEFINE IDENT PROC '(' param_list ')' stmt_list END
#		if_stmt: IF expr THEN stmt_list ELSE stmt_list FI
#		while_stmt: WHILE expr DO stmt_list OD
#		param_list: IDENT ',' param_list 
#		    |      IDENT 
#		expr: expr '+' term   
#		    | expr '-' term   
#		    | term            
#		term: term '*' factor   
#		    | factor            
#		factor:     '(' expr ')'  
#		    |       NUMBER 
#		    |       IDENT 
#		    |       funcall 
#		funcall:  IDENT '(' expr_list ')'
#		expr_list: expr ',' expr_list 
#		    |      expr 
#

import sys

####  CONSTANTS   ################

	# the variable name used to store a proc's return value
returnSymbol = 'return'

tabstop = '  ' # 2 spaces

######   CLASSES   ##################

class Expr :
	'''Virtual base class for expressions in the language'''

	def __init__( self ) :
		raise NotImplementedError(
			'Expr: pure virtual base class.  Do not instantiate' )

	def eval( self, nt, ft ) :
		'''Given an environment and a function table, evaluates the expression,
		returns the value of the expression (an int in this grammar)'''

		raise NotImplementedError(
			'Expr.eval: virtual method.  Must be overridden.' )

	def display( self, nt, ft, depth=0 ) :
		'For debugging.'
		raise NotImplementedError(
			'Expr.display: virtual method.  Must be overridden.' )

class Number( Expr ) :
	'''Just integers'''

	def __init__( self, v=0 ) :
		self.value = v
	
	def eval( self, nt, ft ) :
		return self.value

	def display( self, nt, ft, depth=0 ) :
		print "%s%i" % (tabstop*depth, self.value)

class Ident( Expr ) :
	'''Stores the symbol'''

	def __init__( self, name ) :
		self.name = name
	
	def eval( self, nt, ft ) :
		return nt[ self.name ]

	def display( self, nt, ft, depth=0 ) :
		print "%s%s" % (tabstop*depth, self.name)


class Times( Expr ) :
	'''expression for binary multiplication'''

	def __init__( self, lhs, rhs ) :
		'''lhs, rhs are Expr's, the operands'''

		# test type here?
		# if type( lhs ) == type( Expr ) :
		self.lhs = lhs
		self.rhs = rhs
	
	def eval( self, nt, ft ) :
		return self.lhs.eval( nt, ft ) * self.rhs.eval( nt, ft )

	def display( self, nt, ft, depth=0 ) :
		print "%sMULT" % (tabstop*depth)
		self.lhs.display( nt, ft, depth+1 )
		self.rhs.display( nt, ft, depth+1 )
		#print "%s= %i" % (tabstop*depth, self.eval( nt, ft ))


class Plus( Expr ) :
	'''expression for binary addition'''

	def __init__( self, lhs, rhs ) :
		self.lhs = lhs
		self.rhs = rhs
	
	def eval( self, nt, ft ) :
		return self.lhs.eval( nt, ft ) + self.rhs.eval( nt, ft )

	def display( self, nt, ft, depth=0 ) :
		print "%sADD" % (tabstop*depth)
		self.lhs.display( nt, ft, depth+1 )
		self.rhs.display( nt, ft, depth+1 )
		#print "%s= %i" % (tabstop*depth, self.eval( nt, ft ))


class Minus( Expr ) :
	'''expression for binary subtraction'''

	def __init__( self, lhs, rhs ) :
		self.lhs = lhs
		self.rhs = rhs
	
	def eval( self, nt, ft ) :
		return self.lhs.eval( nt, ft ) - self.rhs.eval( nt, ft )

	def display( self, nt, ft, depth=0 ) :
		print "%sSUB" % (tabstop*depth)
		self.lhs.display( nt, ft, depth+1 )
		self.rhs.display( nt, ft, depth+1 )
		#print "%s= %i" % (tabstop*depth, self.eval( nt, ft ))


class FunCall( Expr ) :
	'''stores a function call:
	  - its name, and arguments'''
	
	def __init__( self, name, argList ) :
		self.name = name
		self.argList = argList
	
	def eval( self, nt, ft ) :
		return ft[ self.name ].apply( nt, ft, self.argList )

	def display( self, nt, ft, depth=0 ) :
		print "%sFunction Call: %s, args:" % (tabstop*depth, self.name)
		for e in self.argList :
			e.display( nt, ft, depth+1 )


#-------------------------------------------------------

class Stmt :
	'''Virtual base class for statements in the language'''

	def __init__( self ) :
		raise NotImplementedError(
			'Stmt: pure virtual base class.  Do not instantiate' )

	def eval( self, nt, ft ) :
		'''Given an environment and a function table, evaluates the expression,
		returns the value of the expression (an int in this grammar)'''

		raise NotImplementedError(
			'Stmt.eval: virtual method.  Must be overridden.' )

	def display( self, nt, ft, depth=0 ) :
		'For debugging.'
		raise NotImplementedError(
			'Stmt.display: virtual method.  Must be overridden.' )


class AssignStmt( Stmt ) :
	'''adds/modifies symbol in the current context'''

	def __init__( self, name, rhs ) :
		'''stores the symbol for the l-val, and the expressions which is the
		rhs'''
		self.name = name
		self.rhs = rhs
	
	def eval( self, nt, ft ) :
		nt[ self.name ] = self.rhs.eval( nt, ft )

	def display( self, nt, ft, depth=0 ) :
		print "%sAssign: %s :=" % (tabstop*depth, self.name)
		self.rhs.display( nt, ft, depth+1 )


class DefineStmt( Stmt ) :
	'''Binds a proc object to a name'''

	def __init__( self, name, proc ) :
		self.name = name
		self.proc = proc

	def eval( self, nt, ft ) :
		ft[ self.name ] = self.proc

	def display( self, nt, ft, depth=0 ) :
		print "%sDEFINE %s :" % (tabstop*depth, self.name)
		self.proc.display( nt, ft, depth+1 )


class IfStmt( Stmt ) :

	def __init__( self, cond, tBody, fBody ) :
		'''expects:
		cond - expression (integer)
		tBody - StmtList
		fBody - StmtList'''
		
		self.cond = cond
		self.tBody = tBody
		self.fBody = fBody

	def eval( self, nt, ft ) :
		if self.cond.eval( nt, ft ) > 0 :
			self.tBody.eval( nt, ft )
		else :
			self.fBody.eval( nt, ft )

	def display( self, nt, ft, depth=0 ) :
		print "%sIF" % (tabstop*depth)
		self.cond.display( nt, ft, depth+1 )
		print "%sTHEN" % (tabstop*depth)
		self.tBody.display( nt, ft, depth+1 )
		print "%sELSE" % (tabstop*depth)
		self.fBody.display( nt, ft, depth+1 )


class WhileStmt( Stmt ) :

	def __init__( self, cond, body ) :
		self.cond = cond
		self.body = body

	def eval( self, nt, ft ) :
		while self.cond.eval( nt, ft ) > 0 :
			self.body.eval( nt, ft )

	def display( self, nt, ft, depth=0 ) :
		print "%sWHILE" % (tabstop*depth)
		self.cond.display( nt, ft, depth+1 )
		print "%sDO" % (tabstop*depth)
		self.body.display( nt, ft, depth+1 )

#-------------------------------------------------------

class StmtList :
	'''builds/stores a list of Stmts'''

	def __init__( self ) :
		self.sl = []
	
	def insert( self, stmt ) :
		self.sl.insert( 0, stmt )
	
	def eval( self, nt, ft ) :
		for s in self.sl :
			s.eval( nt, ft )
	
	def display( self, nt, ft, depth=0 ) :
		print "%sSTMT LIST" % (tabstop*depth)
		for s in self.sl :
			s.display( nt, ft, depth+1 )


class Proc :
	'''stores a procedure (formal params, and the body)

	Note that, while each function gets its own environment, we decided not to
	allow side-effects, so, no access to any outer contexts.  Thus, nesting
	functions is legal, but no different than defining them all in the global
	environment.  Further, all calls are handled the same way, regardless of
	the calling environment (after the actual args are evaluated); the proc
	doesn't need/want/get an outside environment.'''

	def __init__( self, paramList, body ) :
		'''expects a list of formal parameters (variables, as strings), and a
		StmtList'''

		self.parList = paramList
		self.body = body

	def apply( self, nt, ft, args ) :
		newContext = {}

		# sanity check, # of args
		if len( args ) is not len( self.parList ) :
			print "Param count does not match:"
			sys.exit( 1 )

		# bind parameters in new name table (the only things there right now)
			# use zip, bastard
		for i in range( len( args )) :
			newContext[ self.parList[i] ] = args[i].eval( nt, ft )

		# evaluate the function body using the new name table and the old (only)
		# function table.  Note that the proc's return value is stored as
		# 'return in its nametable

		self.body.eval( newContext, ft )
		if newContext.has_key( returnSymbol ) :
			return newContext[ returnSymbol ]
		else :
			print "Error:  no return value"
			sys.exit( 2 )
	
	def display( self, nt, ft, depth=0 ) :
		print "%sPROC %s :" % (tabstop*depth, str(self.parList))
		self.body.display( nt, ft, depth+1 )


class Program :
	
	def __init__( self, stmtList ) :
		self.stmtList = stmtList
		self.nameTable = {}
		self.funcTable = {}
	
	def eval( self ) :
		self.stmtList.eval( self.nameTable, self.funcTable )
	
	def dump( self ) :
		print "Dump of Symbol Table"
		print "Name Table"
		for k in self.nameTable :
			print "  %s -> %s " % ( str(k), str(self.nameTable[k]) )
		print "Function Table"
		for k in self.funcTable :
			print "  %s" % str(k)

	def display( self, depth=0 ) :
		print "%sPROGRAM :" % (tabstop*depth)
		self.stmtList.display( self.nameTable, self.funcTable )

