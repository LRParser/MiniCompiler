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
import logging


logging.basicConfig(
   format = "%(levelname) -4s %(message)s",
   level = logging.DEBUG
)

log = logging.getLogger('programext')

####  CONSTANTS   ################

# the variable name used to store a proc's return value
returnSymbol = 'return'

tabstop = '  ' # 2 spaces
######   OPCODES   ##################

LD = 'LD'
ST = 'ST'
ADD = 'ADD'
SUB = 'SUB'
MUL = 'MUL'
JMP = 'JMP'
JMI = 'JMI'
JMN = 'JMN'
JMZ = 'JMZ'
CAL = 'CAL'
HLT = 'HLT'

######   SYMBOL TABLE ENTRY TYPE#####
CONST = "const"
VAR = "var"
TEMP = "temp"

###### ADDRESS CONSTANTS ############

UNKNOWN = "?" 

######   CLASSES   ##################
class MachineCode(object):

    def __init__(self, opcode=None, operand=None):
        self.opcode = opcode
        self.operand = operand

    def __str__(self):
        return "%s %s" % (self.opcode, self.operand)

class Label(object):

    def __init__(self, label=None):
        self.label = label

    def __str__(self):
        return str(self.label)

    def __eq__(self,other) :
        if isinstance(other,Label) :
            return self.label == other.label
        else :
            return False

    def __ne__(self,other) :
        return not __eq__(self,other)

    def __hash__(self) :
        return hash(self.label)


class LabelFactory ( object ) :
    def __init__( self ) :
        self.__labels = list()
        self.count = 0

    def get_label( self ):
        newLabel = Label("L" + str(self.count))
        self.count = self.count + 1
        return newLabel

LABEL_FACTORY = LabelFactory()
        

class TempVariable(Label):

    def __init__(self, number):
        super(TempVariable, self).__init__("T%s" % number)

    def __eq__(self, other) :
        return super(TempVariable,self).__eq__(other)

    def __ne__(self, other) :
        return not self.__eq__(other)

    def __hash__(self) :
        return super(TempVariable,self).__hash__()


class TempVariableFactory(object):

    def __init__(self):
        self.__temps = list()
        self.count = 0

    def get_temp(self):
        temp = TempVariable(self.count)
        SymbolTableUtils.createOrGetSymbolTableReference(temp,temp.label,TEMP)        
        self.count = self.count + 1
        return temp

TEMP_VARIABLE_FACTORY = TempVariableFactory()

class SymbolTableEntry(object):

    def __init__(self, value=None, entryType=None, address=UNKNOWN):
        """ Class representing a Symbol Table Entry

        :param value: Value of the entry
        :param entryType: Type, Constant, Variable, Temp
        :param address: Memory address where located.
        """
        self.value = value
        self.entryType = entryType
        self.address = address

    def __str__(self):
        return "Value: %s Type: %s Address: %s" % (self.value, self.entryType, self.address)

class SymbolTable(dict) :

        def __init__(self) :
            super(SymbolTable,self).__init__()

        def dump(self) :
            log.debug("Dumping entries in symbol table: ")
            self.summarize()
            for entry in self :
                log.debug("Name: %s %s" % (entry, self[entry]))

        def iterate(self, entryType) :
            return filter(lambda x: x.entryType == entryType, self.itervalues())    

        def countOf(self, entryType) :
            return len(filter(lambda x: x.entryType == entryType, self.itervalues()))

        def summarize(self) :
            log.debug("Num_Vars = %d, Num_Consts = %d, Num_Temps = %d" % ( self.countOf(VAR), self.countOf(CONST), self.countOf(TEMP) ) )

"This symbol table should contain: {Label : SymbolTableEntry}"
GLOBAL_SYMBOL_TABLE = SymbolTable()

class SymbolTableUtils :
        
        @staticmethod
        def createOrGetSymbolTableReference(key, entryVal, entryType ) :
                entry = None
                if key in GLOBAL_SYMBOL_TABLE:
                        log.debug("Found %s in the symbol table" % key)
                        entry = GLOBAL_SYMBOL_TABLE[key]
                else:
                        log.debug("Didn't find %s in the symbol table" % key)
                        GLOBAL_SYMBOL_TABLE.dump()
                        log.debug("Putting %s into symbol table" % key)
                        entry = SymbolTableEntry(entryVal, entryType)
                        GLOBAL_SYMBOL_TABLE[key] = entry
                return entry

### Linker Code ###

class Linker(object) :

    @staticmethod
    def linkAddressesToSymbolTable(symbolTable) :
        currentAddr = 1
        for var in symbolTable.iterate(VAR) :
            var.address = currentAddr
            currentAddr = currentAddr + 1

        for const in symbolTable.iterate(CONST) :
            const.address = currentAddr
            currentAddr = currentAddr + 1

        for temp in symbolTable.iterate(TEMP) :
            temp.address = currentAddr
            currentAddr = currentAddr + 1

class Expr(object) :
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

        def translate( self, nt=None, ft=None ) :
		'For debugging.'
		raise NotImplementedError(
			'Expr.display: virtual method.  Must be overridden.' )


class Number( Expr ) :
	'''Just integers'''

	def __init__( self, v=0 ) :
		self.value = v
                self.tempAddr = TEMP_VARIABLE_FACTORY.get_temp()

	def eval( self, nt, ft ) :
		return self.value

	def display( self, nt, ft, depth=0 ) :
		print "%s%i" % (tabstop*depth, self.value)

        def __str__(self):
                return str(self.value)

        def __eq__(self,other):
                if not(isinstance(other,Number)) :
                    return False
                else :
                    return self.value == other.value

        def __ne__(self,other):
                return not self.__eq__(other)

        def __hash__(self):
                return hash(self.value)

        def translate( self, nt=None, ft=None ) :
                #check to see if number is in the symbol table
                log.debug("Entering translate method for Number %s" % self)

                entry = SymbolTableUtils.createOrGetSymbolTableReference(self,self.value,CONST)

                instructions = list()
                instructions.append(MachineCode(LD,self))
                instructions.append(MachineCode(ST,self.tempAddr))

                return instructions


class Ident( Expr ) :
	'''Stores the symbol'''

	def __init__( self, name ) :
		self.name = name

	def eval( self, nt, ft ) :
		return nt[ self.name ]

	def display( self, nt, ft, depth=0 ) :
		print "%s%s" % (tabstop*depth, self.name)

        def __str__(self):
                return self.name

        def __eq__(self, other):
                log.debug("Calling __eq__ on %s and %s" % (self, other))
                if(isinstance(other,Ident)) :
                        return self.name == other.name
                else :
                        return False

        def __ne__(self, other):
                return not self.__eq__(other)

        def __hash__(self):
                return self.name.__hash__()


        def translate( self, nt=None, ft=None ) :
                #check to see if Ident is in the symbol table
                log.debug("Entering translate method for Ident: %s", self)
                entry = SymbolTableUtils.createOrGetSymbolTableReference(self,self.name,VAR)
                instructions = list()
                instructions.append(MachineCode(LD,entry.address))

                return instructions

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

        def translate(self, nt, ft ) :
                log.debug("Entering translate method for Times")
                raise Exception("Not implemented")
                instructions = list()
                return instructions

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

        def translate(self, nt, ft ) :
                log.debug("Entering translate method for Plus")
                raise Exception("Not implemented")
	
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

        def translate( self, nt, ft ) :
                log.debug("Entering translate method for Minus")
                raise Exception("Not implemented")
                instructions = list()
                return instructions()

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

        def translate( self, nt, ft ) :
                raise Exception("Functions not supported by mini compiler")

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

        def __str__( self ) :
                return str(self.name)+" = "+str(self.rhs)

        def __eq__( self, other ) :
                if(isinstance(other,AssignStmt)) :
                    return self.name == other.name
                else :
                    return False

        def __ne__( self, other ) :
            return not (self.__eq__(other))
        
        def __hash__( self ) :
            return hash(self.name)

        def translate(self, nt, ft) :
                '''Produces (unlinked) machine code to load the locates of RHS via LD, and store into location of LHS via LD'''
                log.debug("Entering translate method for AssignStmt: %s" % self)
                instructions = list()
               
                # First, execute the code corresponding to the RHS
                rhsCode = self.rhs.translate(nt,ft)
                instructions.append(rhsCode)
                log.debug("RHS of AssignStmt translated")
                
                # The value computed by the RHS is now in the accumulator. First, ensure the Ident on LHS is in the symbol table for later linking
                entry = SymbolTableUtils.createOrGetSymbolTableReference(self,self.name,VAR)          

                # First, ensure the Ident is in the symbol table. Then, store the value in the accumulator in the memory address pointed to by the Ident on the LHS
                ldCode = MachineCode(LD,rhsCode[-1].operand)
                instructions.append(ldCode) 
                assignCode = MachineCode(ST,self.name)
                instructions.append(assignCode)
                log.debug("LHS of AssignStmt translated")
      
                GLOBAL_SYMBOL_TABLE.dump()
          
                return instructions

class DefineStmt( Stmt ) :
	'''Binds a proc object to a name'''

	def __init__( self, name, proc ) :
		self.name = name
		self.proc = proc

	def eval( self, nt, ft ) :
		ft[ self.name ] = self.proc

        def translate(self, nt, ft) :
                raise Exception("Functions not supported for mini compiler")

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

        def translate( self, nt, ft ) :
            instructions = list()

            # translate the conditional expression
            (condCode, storageLocation) = translate(self.cond, nt, ft)
            for instr in condCode :
                instructions.append(instr)

            # load result of condConde into accumulator
            instructions.append(MachineCode(LD, storageLocation))

            # if result is false (<= 0), jump over trueBody
            falseBodyLabel = LABEL_FACTORY.get_label()
            instructions.append(MachineCode(JMN, falseBodyLabel))
            instructions.append(MachineCode(JMZ, falseBodyLabel))

            # translate the trueBody
            (trueBody, storageLocation) = translate(self.tBody, nt, ft)
            for instr in trueBody :
                instructions.append(instr)

            # load the result of trueBody
            instructions.append(MachineCode(LD, storageLocation))

            # jump over the falseBody
            nextStatement = LABEL_FACTORY.get_label()
            instructions.append(MachineCode(JMP, nextStatement))

            # insert the falseBodyLabel
            instructions.append(falseBodyLabel)

            # translate the flaseBody
            (falseBody, storageLocation) = translate(self.fBody, nt, ft)
            for instr in falseBody :
                instructions.append(instr)

            # load resulf of falseBody
            instructions.append(MachineCode(LD, storageLocation))

            # insert the nextStatement label
            instructions.append(nextStatement)
                
            return instructions


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

        def translate( self, nt, ft) :
            instructions = list()

            # insert the loopBeginlabel
            loopBeginLabel = LABEL_FACTORY.get_label()
            instructions.append(loopBeginLabel)

            # translate the body of the conditional
            (condBody, storageLocation) = self.cond.translate(nt, ft)

            for instr in condBody :
                instructions.append(instr)

            # print to log the result of condBody
            log.debug(instr)

            # load the result of condBody
            instructions.append(MachineCode(LD, storageLocation))

            # if the result is false (<= 0), jump to loopEndLabel
            loopEndLabel = LABEL_FACTORY.get_label()
            instructions.append(MachineCode(JMN, loopEndLabel))
            instructions.append(MachineCode(JMZ, loopEndLabel))

            # translate the loopBody
            (loopBody, storageLocation) = self.body.translate(nt, ft)
            for instr in loopBody :
                instructions.append(instr)

            # load the result of loopBody
            instructions.append(MachineCode(LD, storageLocation))

            # go back to the begining of the loop
            instructions.append(MachineCode(JMP, loopBeginLabel))

            # insert the loopEndLabel
            instructions.append(loopEndLabel)

            return instructions

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
        
        def translate( self, nt, ft ) :
                instructions = list()
                for s in self.sl :
                        instructions.append(s.translate( nt, ft ))
                return instructions

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

        def translate( self ) :
                nestedStmtCode = self.stmtList.translate(self.nameTable, self.funcTable)
                flattenedStmtCode = list(self.flattenList(nestedStmtCode))
                return flattenedStmtCode

        def link( self, machineCode ) :
            Linker.linkAddressesToSymbolTable(GLOBAL_SYMBOL_TABLE)
            for key in GLOBAL_SYMBOL_TABLE.iterkeys() :
                log.debug(key)
            for line in machineCode :
                log.debug("Looking to link operand %s " % line.operand)
                line.operand = GLOBAL_SYMBOL_TABLE[line.operand].address
            return machineCode 


        def compile( self ) :
            machineCode = self.translate()
            machineCode = self.optimize(machineCode)
            machineCode = self.link(machineCode)
            return machineCode

        def performPeepholeOptimization(self, machineCode ) :
                '''Removes redundant LD statements when desired value is already in accumulator'''
                optimizedCode = list()

                for i in range(len(machineCode)) :
                    prevInstr = machineCode[i-1]
                    currentInstr = machineCode[i]
                    if(prevInstr.opcode == ST and currentInstr.opcode == LD and (prevInstr.operand == currentInstr.operand)) :
                        log.debug("Removing redundant LD statement")
                    else :
                        optimizedCode.append(currentInstr)

                return optimizedCode                         

        def optimize( self, machineCode ) :
                optimizedCode = self.performPeepholeOptimization(machineCode)
                return optimizedCode

        def flattenList( self, iterableList ) :
                iterator = iter(iterableList)
                for element in iterator :
                        if isinstance(element, (list, tuple)) :
                            for nestedElement in self.flattenList(element):
                                yield nestedElement
                        else :
                            yield element

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
