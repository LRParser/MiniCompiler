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

def log_inst(name, inst_list):
    log.debug("Print out instructions for %s" % name)
    for i in inst_list:
        log.debug(i)

####  CONSTANTS   ################

# the variable name used to store a proc's return value
returnSymbol = 'return'

tabstop = '  ' # 2 spaces

MEMORY_SIZE = 100

######   OPCODES   ##################

LD = 'LDA'
ST = 'STA'
ADD = 'ADD'
SUB = 'SUB'
MUL = 'MUL'
JMP = 'JMP'
JMI = 'JMI'
JMN = 'JMN'
JMZ = 'JMZ'
CAL = 'CAL'
HLT = 'HLT'
LDA = 'LDA'
STA = 'STA'
LDI = 'LDI'
STI = 'STI'
######   SYMBOL TABLE ENTRY TYPE#####
CONST = "const"
VAR = "var"
TEMP = "temp"

###### ADDRESS CONSTANTS ############

UNKNOWN = "?"
SPADDR = "1"
FPADDR = '2'
FPBADDR = '3'
TEMP_REG = '4'
######   CLASSES   ##################

class MachineCode(object):

    def __init__(self, opcode=None, operand=None, label=None):
        self.opcode = opcode
        self.operand = operand
        self.label = label

    @property
    def is_jump(self):
        if 'JM' in self.opcode:
            return True
        elif 'CAL' in self.opcode:
            return True
        else:
            return False

    def __str__(self) :
        return self.symbolicStr()

    def symbolicStr(self):
        if (self.label is not None) :
            if (self.opcode is HLT) :
                return "%s: %s" % (self.label, self.opcode)
            else :
                return "%s: %s %s" % (self.label, self.opcode, self.operand)
        elif (self.operand is None) :
            return "    %s" % (self.opcode)
        else:
            return "    %s %s" % (self.opcode, self.operand)


class Label(object):

    def __init__(self, label=None):
        self.label = label

    def __str__(self):
        return str(self.label)

    def __eq__(self,other) :
        if isinstance(other,Label) :
            return self.label == other.label
        elif isinstance(other,str) :
            return self.label == other
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

class ProcLabelFactory ( object ) :
    def __init__ ( self ) :
        self.__labels = list()
        self.count = 0

    def get_label ( self ) :
        text = "P"+str(self.count)
        newLabel = Label(text)
        log.debug("Log the label: "+str(text))
        self.count = self.count + 1
        return newLabel

PROC_LABEL_FACTORY = ProcLabelFactory()


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
#        SymbolTableUtils.createOrGetSymbolTableReference(temp,temp.label,TEMP)
        self.count = self.count + 1
        return temp

class ActivationRecord(object):

    def __init__(self):
        self.rel_mem_addr = 0
        self.temp_factory = TempVariableFactory()

        self.on_stack = dict()

        # Decleare some 'constants'
        self.RETURN_VALUE = 'return_value'
        self.PREV_FP = 'prev_fp'
        self.RETURN_ADDR = 'return_addr'

    def __inc(self):
        self.rel_mem_addr += 1

    def get_size(self):
        '''Get the size of the activation record.  Useful for determing where
        the SP and FP should go
        '''
        return self.rel_mem_addr + 1

    def store_parameters(self, previousAR, parameterList):
        '''Generates the machine code to store the parameters from the previous
        Activation Record
        '''

        if not parameterList:
            #no parameters
            return None
        else:
            instructions = list()

            for param in parameterList:
                #Load from the previous AR
                instructions.extend(previousAR.load_stack_var(param))

                #create space on the stack
                self.alloc_param(param)

                #and store the new param
                instructions.exend(self.set_stack_var(param))



    def store_prev_fp(self):
        '''Generate the RAL code to store the previous FP
        '''
        instructions = list()

        make_inst = self.__make_inst_list(instructions)

        #load previous FP
        make_inst(LDA, FPADDR)

        #Store it
        return instructions.extend(self.set_stack_var(self.PREV_FP))


    def load_prev_fp(self):
        "Return RAL code to load the previous FP into the ACC"
        return self.load_stack_var(self.PREV_FP)



    def alloc_temp(self):
        "Alloc a temporary and return a string representing the temp"

        temp = self.temp_factory.get_temp()

        #Store the temp variable and it's location
        self.alloc_param(temp)

        return temp

    def alloc_param(self, param):
        "alloc space on the stack for a param"
        log.debug("Alloc room on the AR for param: %s" % param)
        self.on_stack[param] = self.rel_mem_addr

        self.__inc()

    def param_exists(self, param):
        if param in self.on_stack:
            return True
        else:
            return False

    def get_offset(self, var):
        self.__assert_var(var)
        log.debug("Offset to %s is %s" % (var, self.on_stack[var]))
        return self.on_stack[var]


    def __assert_var(self, var):
        assert (var in self.on_stack), ("Stack var: %s not found!" % var)

    def load_stack_var(self, var):
        "Generate the machine code to load the var from the stack into the ACC"

        self.__assert_var(var)

        make_inst = self.__make_inst_list(list())

        make_inst(LDA, FPADDR)
        #create a number...
        num = Number(self.get_offset(var))
        entry = \
        SymbolTableUtils.createOrGetSymbolTableReference(num,num.value,CONST)

        make_inst(SUB, num)
        make_inst(STA, FPBADDR)
        return make_inst(LDI, FPBADDR)

    def load_stack_or_heap_var(self, var):
        '''Should only really be used if you think you have a heap var,
        otherwise, stick with load stack var
        '''
        inst = list()

        make_inst = self.__make_inst_list(inst)

        try:
            inst.extend(self.load_stack_var(var))
        except AssertionError:
            #not found on stack, right now assuming it's a global
            # But we may have to search up the ARs...
            log.debug("Not found on stack: %s" % var)
            inst.append(MachineCode(LDA, var))

        return inst

    def set_stack_var(self, var):
        '''Generates the machine code to store a stack var.  Assumes the value to
        set is in the ACC'''

        self.__assert_var(var)

        make_inst = self.__make_inst_list(list())

        #first save of the value to set
        make_inst(STA, TEMP_REG)
        #Look up the var, but first get the FP
        make_inst(LDA, FPADDR)
        #calc offset
        num = Number(self.get_offset(var))
        entry = \
        SymbolTableUtils.createOrGetSymbolTableReference(num,num.value,CONST)
        make_inst(SUB, num)
        #store offset in FPBADDR
        make_inst(STA, FPBADDR)
        #now load back the value into the ACC
        make_inst(LDA, TEMP_REG)
        #now store that into the value FP points to
        return make_inst(STI, FPBADDR)

    def store_return_value(self):
        "Assumes return is in ACC"
        return self.set_stack_var(self.RETURN_VALUE)

    def load_return_value(self):
        return self.load_stack_var(self.RETURN_VALUE)

    def store_return_addr(self):
        "Assumes return addr is in ACC"
        return self.set_stack_var(self.RETURN_ADDR)

    def jump_to_return_addr(self):
        "Produce the RAL code to jump to the return addr"
        make_inst = self.__make_inst_list(list())

        num = Number(self.get_offset(self.RETURN_ADDR))
        entry = SymbolTableUtils.createOrGetSymbolTableReference(num, num.value, CONST)
        make_inst(LDA, FPADDR)
        make_inst(SUB, num)
        make_inst(STA, FPBADDR)
        make_inst(LDI, FPBADDR)
        make_inst(STA, FPBADDR)
        return make_inst(JMI, FPBADDR)


    def __make_inst_list(self, the_list):
        '''Return a function that takes machine code operators and operands and
        appends them to a list, which is a lexical closure.

        Basically, I was tired of typing MachineCode(blah, blah) each time ;)
        '''
        def machine_code_append(code, operand):
            the_list.append(MachineCode(code, operand))
            return the_list

        return machine_code_append


    def alloc_epilogue(self):
        'Should be called after the proc is translated'
        self.alloc_param(self.RETURN_VALUE)
        self.alloc_param(self.PREV_FP)
        self.alloc_param(self.RETURN_ADDR)

    def prologue_update_fp_sp(self):
        'Sets the fp to the sp and sets sp to the return addr'
        make_inst = self.__make_inst_list(list())

        #
        # Store previous FP
        #
        make_inst(LDA, FPADDR).extend(self.set_stack_var(self.PREV_FP))
        #
        # Update FP
        #
        #load SP
        make_inst(LDA, SPADDR)
        #sub one to point to new AR
        num = Number(1)
        entry = \
        SymbolTableUtils.createOrGetSymbolTableReference(num,num.value,CONST)
        make_inst(SUB, num)
        #Store that ADDR into the FP
        make_inst(STA, FPADDR)
        #
        # Update SP
        #
        #FP is already in ACC, so get offset to ret addr
        num = Number(self.get_offset(self.RETURN_ADDR))
        entry = \
        SymbolTableUtils.createOrGetSymbolTableReference(num,num.value,CONST)
        make_inst(SUB, num)
        #Update the SP to point to the ret. addr
        return make_inst(STA, SPADDR)

    def epilogue_update_fp_sp(self):
        'Sets the fp to the sp and sets sp to the return addr'
        make_inst = self.__make_inst_list(list())

        #
        # Update SP
        #
        #load FP
        make_inst(LDA, FPADDR)
        #add one to point to end of the old AR
        num = Number(1)
        entry = \
        SymbolTableUtils.createOrGetSymbolTableReference(num,num.value,CONST)
        make_inst(ADD, num)
        #Store that ADDR into the FP
        make_inst(STA, SPADDR)
        #
        # Update FP
        #
        #FP load previous FP
        make_inst(LDA, self.get_offset(self.PREV_FP))
        #Update the FP to point to the previous FP
        return make_inst(STA, FPADDR)

    def call(self, function_label):
        "Assumes SP already points to ret address"
        make_inst = self.__make_inst_list(list())
        return make_inst(CAL, function_label)


class NoOp(MachineCode):

    def __init__(self, label=None):
        MachineCode.__init__(self, None, None, label)


class SymbolTableEntry(object):

    def __init__(self, value=None, entryType=None, address=UNKNOWN, label=None):
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

    @staticmethod
    def getMemoryTable(linkedSymbolTable) :
        '''Takes linked machine code and generates a memory table that already contains values of constants'''

        log.debug("Generating memory table")
        currentAddr = 1

        # TODO: Refactor to use a lambda function to iterate by address
        # Except for constants, we we initialize all values to 0

        memLines = list()

        def pretty_print(count):

            def f(value,typeof):
                addr = count['currentAddr']
                memLines.append("%-3d %-3s ; %s" % (addr, value, typeof) )
                count['currentAddr'] = addr + 1

            return f

        count = dict()
        count['currentAddr'] = 1

        memdump = pretty_print(count)

        #set the known registers
        memdump(MEMORY_SIZE-1, "SP")
        memdump(MEMORY_SIZE-1, "FP")
        memdump(0, "FPB")
        memdump(0, "TEMP")

        for register in range(6): #10 registers total
            memdump('0', 'register')

        for const in linkedSymbolTable.iterate(CONST) :
            memdump(const.value, const)

        for var in linkedSymbolTable.iterate(VAR) :
            memdump(0, var)

        for temp in linkedSymbolTable.iterate(TEMP) :
            memdump(0, temp)

        for i in range(count['currentAddr'], MEMORY_SIZE):
            memdump(0, "FREE STACK")

        log.debug("Memory table generated")
        return memLines

### Linker Code ###

class Linker(object) :

    @staticmethod
    def linkAddressesToSymbolTable(symbolTable) :
        #start at an offset from the registers
        currentAddr = 11
        for const in symbolTable.iterate(CONST) :
            const.address = currentAddr
            currentAddr = currentAddr + 1
            log.debug("Found const: %s" % const)

        for var in symbolTable.iterate(VAR) :
            var.address = currentAddr
            currentAddr = currentAddr + 1

        for temp in symbolTable.iterate(TEMP) :
            temp.address = currentAddr
            currentAddr = currentAddr + 1

        log.debug("linkaddressesToSymbolTable currentaddr: %d" % currentAddr)

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

    def translate( self, nt=None, ft=None, ar = None ) :
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

    def __str__(self):
        return str(self.value)

    def __eq__(self,other):
        if isinstance(other,Number) :
            return self.value == other.value
        else :
            return False

    def __ne__(self,other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.value)

    def translate( self, nt=None, ft=None, ar = None ) :
        #check to see if number is in the symbol table
        log.debug("Entering translate method for Number %s" % self)

        entry = SymbolTableUtils.createOrGetSymbolTableReference(self,self.value,CONST)

        instructions = list()

        return (instructions, self, ar)


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
        if(isinstance(other,Ident)) :
            return self.name == other.name
        else :
            return False

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return hash(self.name)


    def translate( self, nt=None, ft=None, ar = None ) :
        #check to see if Ident is in the symbol table
        log.debug("Entering translate method for Ident: %s", self)
        entry = ar.alloc_param(self.name) # SymbolTableUtils.createOrGetSymbolTableReference(self.name,self.name,VAR)
        instructions = list()
        #instructions.append(MachineCode(LD,self.name))

        return (instructions, self.name, ar)

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

    def translate(self, nt, ft, ar ) :
        log.debug("Entering translate method for Times")

        instructions = list()

        # Get the Left Hand Side.
        (lhsCode, lhsStorageLocation, lhsActivationRecord) = self.lhs.translate(nt,ft,ar)
        for instr in lhsCode :
            instructions.append(instr)

        # Get the Right Hand Side.
        (rhsCode, rhsStorageLocation, rhsActivationRecord) = self.rhs.translate(nt,ft,ar)
        for instr in rhsCode :
            instructions.append(instr)

        # load the result of lhs into the accumulator, then subtract the rhs
        resultStorageLocation = ar.alloc_temp()
        instructions.extend(ar.load_stack_or_heap_var(lhsStorageLocation))
        instructions.append(MachineCode(STA, TEMP_REG))
        instructions.extend(ar.load_stack_or_heap_var(rhsStorageLocation))
        instructions.append(MachineCode(MUL, TEMP_REG))
        instructions.extend(ar.set_stack_var(resultStorageLocation))

        for val in instructions:
            log.debug("%s %s " % (val.opcode, val.operand))

        log.debug("Times translated")
        log_inst("Times", instructions)
        return (instructions, resultStorageLocation, ar)

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

    def translate(self, nt, ft, ar ) :
        log.debug("Entering translate method for Plus")

        instructions = list()

        # Get the Left Hand Side.
        (lhsCode, lhsStorageLocation, lhsActivationRecord) = self.lhs.translate(nt,ft,ar)
        for instr in lhsCode :
            instructions.append(instr)

        # Get the Right Hand Side.
        (rhsCode, rhsStorageLocation, rhsActivationRecord) = self.rhs.translate(nt,ft,ar)
        for instr in rhsCode :
            instructions.append(instr)

        # load the result of lhs into the accumulator, then subtract the rhs
        resultStorageLocation = TEMP_VARIABLE_FACTORY.get_temp()
        instructions.append(MachineCode(LD, lhsStorageLocation))
        instructions.append(MachineCode(ADD, rhsStorageLocation))
        instructions.append(MachineCode(ST, resultStorageLocation))

        for val in instructions:
            log.debug("%s %s " % (val.opcode, val.operand))

        log.debug("Plus translated")
        return (instructions, resultStorageLocation, ar)

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

    def translate( self, nt, ft, ar ) :
        log.debug("Entering translate method for Minus")

        instructions = list()

        # Get the Left Hand Side.
        (lhsCode, lhsStorageLocation, lhsActivationRecord) = self.lhs.translate(nt,ft,ar)
        for instr in lhsCode :
            instructions.append(instr)

        # Get the Right Hand Side.
        (rhsCode, rhsStorageLocation, rhsActivationRecord) = self.rhs.translate(nt,ft,ar)
        for instr in rhsCode :
            instructions.append(instr)

        # load the result of lhs into the accumulator, then subtract the rhs
        resultStorageLocation = ar.alloc_temp()

        #load rhs and store that in temp
        instructions.extend(ar.load_stack_or_heap_var(rhsStorageLocation))
        instructions.append(MachineCode(STA, TEMP_REG))

        #load LHS
        instructions.extend(ar.load_stack_or_heap_var(lhsStorageLocation))

        #sub the AC from RHS (TEMP_REG)
        instructions.append(MachineCode(SUB, TEMP_REG))

        instructions.extend(ar.set_stack_var(resultStorageLocation))

        for val in instructions:
            log.debug("%s %s " % (val.opcode, val.operand))

        log.debug("Minus translated")
        log_inst("minus", instructions)
        return (instructions, resultStorageLocation, ar)

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

    def translate_arguments(self, proc_to_call, new_ar, nt, ft, at):

        log.debug("Entering translate arguments")

        #check the number args == num of params
        proc_to_call.check_arg_list(self.argList)

        instructions = list()

        if (not self.argList):
            return instructions

        for arg, param in zip(self.argList, proc_to_call.parList):
            log.debug("Translating param: %s" % param)

            arg_inst, return_val, ar = arg.translate(nt, ft, at)

            # Add the argument instructions
            instructions.extend(arg_inst)

            instructions.extend(ar.load_stack_or_heap_var(return_val))
            # set that in th new AR
            new_ar.set_stack_var(param)


        return instructions

    def translate( self, nt, ft, ar ) :
        log.debug("Entering translate for FunCall: %s" % self.name)

        proc_to_call = ft[ self.name ]
        new_ar = proc_to_call.ar

        instructions = list()
        #
        # Prologue
        #
        #First update fp and sp
        instructions.extend(new_ar.prologue_update_fp_sp())

        #store the parameters
        #instructions.extend(self.translate_arguments(proc_to_call, new_ar, nt, ft, ar))

        #call the function
        instructions.extend(new_ar.call(ft[self.name].label))

        #
        # Epilogue
        #
        # The return value is already stored and the jump has already been
        # translated

        # load the return value into the ACC
        instructions.extend(new_ar.load_return_value())


        # and store in the caller's ar
        return_val = ar.alloc_temp()
        ar.set_stack_var(return_val)

        #pop the AR
        instructions.extend(new_ar.epilogue_update_fp_sp())

        log_inst("translated funcall", instructions)

        return (instructions, return_val, ar)


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
        return str(self.name)

    def __eq__( self, other ) :
        if(isinstance(other,AssignStmt)) :
            return self.name == other.name
        elif(isinstance(other,str)) :
            return self.name == other
        else :
            log.debug("Returning false")
            return False

    def __ne__( self, other ) :
        return not (self.__eq__(other))

    def __hash__( self ) :
        return hash(self.name)

    def translate(self, nt, ft, ar) :
        '''Produces (unlinked) machine code to load the locates of RHS via LD, and store into location of LHS via LD'''
        log.debug("Entering translate method for AssignStmt: %s" % self)
        instructions = list()

        # First, execute the code corresponding to the RHS
        (rhsCode, rhsStorageLocation, ar) = self.rhs.translate(nt,ft,ar)
        for instr in rhsCode :
            instructions.append(instr)
            # instructions.append(rhsCode)
        log.debug("RHS of AssignStmt translated")
        log.debug("RHS storage: %s" % rhsStorageLocation)

        # The value computed by the RHS is now in the accumulator. Make sure it
        # exists on the AR
        if not ar.param_exists(self.name):
            ar.alloc_param(self.name)


        # First, ensure the Ident is in the symbol table. Then, store the value in the accumulator in the
        # memory address pointed to by the Ident on the LHS
        instructions.extend(ar.load_stack_or_heap_var(rhsStorageLocation))

        #now set the LHS
        instructions.extend(ar.set_stack_var(self.name))

        log.debug("LHS of AssignStmt translated")
        log_inst("assignStmt translation", instructions)

        GLOBAL_SYMBOL_TABLE.dump()

        return (instructions, self.name, ar)

class DefineStmt( Stmt ) :
    '''Binds a proc object to a name'''

    def __init__( self, name, proc ) :
        log.debug("DefineStmt Ctor for: "+str(name))
        self.name = name
        self.proc = proc
        self.proc.label = name

    def eval( self, nt, ft ) :
        self.__add_self_to_function_table(ft)

    def translate(self, nt, ft, ar) :

        log.debug("Entering translate for Define Stmt with name: "+str(self.name))

        self.__add_self_to_function_table(ft)

        instructions = self.proc.translate(nt, ft)
        #instructions.append(NoOp(self.proc.label))
        if(instructions[0].label is not None) :
            raise Exception("About to wipe out a label, previous value was: "+str(instructions[0].label)+" new value would be: "+str(self.proc.label))
        instructions[0].label = self.proc.label

        log.debug("Labeled %s with %s" % (instructions[0], self.proc.label))

        log_inst("translated defineStmt", instructions)

        return (instructions, None, None)

    def display( self, nt, ft, depth=0 ) :
        print "%sDEFINE %s :" % (tabstop*depth, self.name)
        self.proc.display( nt, ft, depth+1 )


    def __add_self_to_function_table(self, ft):
        ft[ self.name ] = self.proc

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

    def translate( self, nt, ft, ar ) :
        instructions = list()

        # translate the conditional expression
        (condCode, storageLocation, condActivationRecord) = self.cond.translate( nt, ft, ar)
        for instr in condCode :
            instructions.append(instr)

        # load result of condConde into accumulator
        instructions.append(MachineCode(LD, storageLocation))

        # if result is false (<= 0), jump over trueBody
        falseBodyLabel = LABEL_FACTORY.get_label()
        instructions.append(MachineCode(JMN, falseBodyLabel))
        instructions.append(MachineCode(JMZ, falseBodyLabel))

        # translate the trueBody
        (trueBody, storageLocation, trueBodyActivationRecord) = self.tBody.translate(nt, ft, ar)
        for instr in trueBody :
            instructions.append(instr)

        # load the result of trueBody
        instructions.append(MachineCode(LD, storageLocation))

        # jump over the falseBody
        nextStatement = LABEL_FACTORY.get_label()
        instructions.append(MachineCode(JMP, nextStatement))

        # translate the falseBody
        (falseBody, storageLocation, falseBodyActivationRecord) = self.fBody.translate(nt, ft, ar)
        if not falseBody:
            #insert NOOP
            instructions.append(NoOp(falseBodyLabel))
        else:
            falseBody[0].label = falseBodyLabel

            for instr in falseBody :
                instructions.append(instr)

        # load resulf of falseBody
        instructions.append(MachineCode(LD, storageLocation))

        # insert the nextStatement label
        instructions.append(NoOp(nextStatement))

        return (instructions,storageLocation, ar)


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

    def translate( self, nt, ft, ar) :
        log.debug("Entering translate for WhileStmt")
        instructions = list()

        # insert the loopBeginlabel
        loopBeginLabel = LABEL_FACTORY.get_label()

        # translate the body of the conditional
        (condBody, storageLocation, condActivationRecord) = self.cond.translate(nt, ft, ar)

        log.debug("Condition returned %s storage: %s" % (condBody, storageLocation))

        if len(condBody) is not 0:
            condBody[0].label = loopBeginLabel

            log.debug("Replaced machine code with: %s " % condBody[0])

            for instr in condBody :
                instructions.append(instr)
                # print to log the result of condBody
                log.debug(instr)

            # load the result of condBody
            instructions.extend(ar.load_stack_var(storageLocation))

        else:
            #point the label to the first instruction
            load_list = ar.load_stack_var(storageLocation)
            load_list[0].label = loopBeginLabel
            instructions.extend(load_list)
            log.debug("Loop begin: %s" % instructions[0])

        # if the result is false (<= 0), jump to loopEndLabel
        loopEndLabel = LABEL_FACTORY.get_label()
        instructions.append(MachineCode(JMN, loopEndLabel))
        instructions.append(MachineCode(JMZ, loopEndLabel))

        # translate the loopBody
        (loopBody, storageLocation, loopBodyActivationRecord) = self.body.translate(nt, ft, ar)
        for instr in loopBody :
            instructions.append(instr)

        # load the result of loopBody
        instructions.extend(ar.load_stack_var(storageLocation))

        # go back to the begining of the loop
        instructions.append(MachineCode(JMP, loopBeginLabel))

        # insert the loopEndLabel
        instructions.append(NoOp(loopEndLabel))

        log_inst("while", instructions)

        return (instructions,storageLocation, ar)

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

    def append(self, stmt ) :
        self.sl.append(stmt)

    def eval( self, nt, ft ) :
        for s in self.sl :
            s.eval( nt, ft )

    def translate( self, nt, ft, ar ) :

        instructions = list()
        for s in self.sl :
            (s.instructions, s.storageLocation, activationRecord) = s.translate( nt, ft, ar)
            lastStorageLocation = s.storageLocation

            for instr in s.instructions :
                instructions.append(instr)
        return (instructions,lastStorageLocation, ar)


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
        self.ar = ActivationRecord()
        self.label = None

    def apply( self, nt, ft, args ) :
        newContext = {}

        self.check_arg_list(args)

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

    def translate( self, nt, ft):

        log.debug("Entering translate for Proc")
        log.debug("FT for proc:")
        for (i, function) in enumerate(ft):
            log.debug("%s: %s (%s)" % (i, function, ft[function].label))

        #first create the parameter list on the AR.
        for param in self.parList:
            log.debug("Param: %s" % param)
            self.ar.alloc_param(param)

        (instructions, lastStorageLocation, returnAR) = self.body.translate(nt, ft, self.ar)

        log_inst("Translated body", instructions)

        self.ar = returnAR

        #alloc the epliogue on the AR
        self.ar.alloc_epilogue()

        instructions.extend(self.ar.jump_to_return_addr())

        log_inst("translated proc", instructions)

        return instructions





    def display( self, nt, ft, depth=0 ) :
        print "%sPROC %s :" % (tabstop*depth, str(self.parList))
        self.body.display( nt, ft, depth+1 )

    def check_arg_list(self, args):
        # sanity check, # of args
        def get_len(args):
            if not args:
                return 0
            else:
                return len(args)

        assert (get_len(args) == get_len(self.parList)), "Param count does not match"


class Program :

    def __init__( self, stmtList ) :
        self.stmtList = stmtList
        self.nameTable = {}
        self.funcTable = {}
        self.ar = ActivationRecord()

    def eval( self ) :
        self.stmtList.eval( self.nameTable, self.funcTable )

    def remove_no_ops(self, instructions):

        for i,item in enumerate(list(instructions)):
            if isinstance(item, NoOp):
                log.debug("found Noop at %s" % i)
                if item.label is not None:
                    log.debug("Had label %s" % item.label)
                    try :
                        instructions[i+1].label = item.label
                        instructions[i] = None
                    except :
                        pass

        return [x for x in instructions if x is not None]


    def __init_sp_fp(self):
        "generate the RAL code to init the SP and FP"

    def call_main(self):

        main_func = None

        for s in self.stmtList.sl:
            if isinstance(s,DefineStmt) and "main" in s.name:
                main_func = s
                break

        assert (main_func), "main function not defined"

        m = FunCall("main", None)

        code, x, y = m.translate(self.nameTable, self.funcTable, self.ar)

        return code

    def callImplMain( self ) :

        m = FunCall("implMain", None)

        (code, storageLocation, activationRecord) = m.translate(self.nameTable, self.funcTable, self.ar)

        return code


    def translate( self ) :

        # create define statement for implicit main

        implMain = DefineStmt("implMain", Proc( list(), self.stmtList))

        # translate the define statement into RAL code, which adds implMain to the funcTable
        (implMainCode, lastStorageLocation, activationRecord) = implMain.translate(self.nameTable, self.funcTable, self.ar)

        trampoline = self.callImplMain()
        trampoline.append(MachineCode(HLT))

        trampoline.extend(list(self.flattenList(implMainCode)))
        trampoline = self.remove_no_ops(trampoline)

        log_inst("translated program", trampoline)

        return trampoline

    def link( self, machineCode ) :
        Linker.linkAddressesToSymbolTable(GLOBAL_SYMBOL_TABLE)
        for key in GLOBAL_SYMBOL_TABLE.iterkeys() :
            log.debug(key)
        for line in machineCode :

            try:
                linkedAddr = GLOBAL_SYMBOL_TABLE[line.operand].address
                line.operand = linkedAddr

                log.debug("Linked operand %s to address: %s" % (line.operand, linkedAddr))

            except KeyError:
                pass

            log.debug("Linked MachineCode: %s" % line)

        for inst in machineCode:
            if ('CAL' in str(inst)):
                log.debug("trying to resolve %s" % inst)
                for i, item in enumerate(machineCode):
                    if str(inst.operand) == str(item.label):
                        log.debug("Found target of call: %s" % inst)
                        inst.operand = i + 1
                        log.debug(inst)

            elif (inst.is_jump):
                for i, item in enumerate(machineCode):
                    if inst.operand == item.label:
                        log.debug("Found target of jump: %s" % inst)
                        inst.operand = i + 1
                        log.debug(inst)

        return machineCode

    def getMemoryTable( self ) :
        return SymbolTableUtils.getMemoryTable(GLOBAL_SYMBOL_TABLE)


    def compile( self ) :
        machineCode = self.translate()
        machineCode = self.optimize(machineCode)
        machineCode = self.link(machineCode)
        # Print memory table

        Linker.getMemoryTable(GLOBAL_SYMBOL_TABLE)

        return machineCode

    def performPeepholeOptimization(self, machineCode ) :
        '''Removes redundant LD statements when desired value is already in accumulator'''
        optimizedCode = list()

        for i in range(len(machineCode)) :
            prevInstr = machineCode[i-1]
            currentInstr = machineCode[i]
            if(prevInstr.opcode == ST and currentInstr.opcode == LD and (prevInstr.operand == currentInstr.operand)) :
                if (currentInstr.label is None):
                    log.debug("Removing redundant LD statement")
                else:
                    optimizedCode[-1].label = currentInstr.label
                    log.debug("Moved label %s" % currentInstr.label)
            else :
                optimizedCode.append(currentInstr)

        return optimizedCode

    def performConstantFolding(self, machineCode ) :
        '''Extra credit optimization recommended by TA; pre-computes the values at compile time where possible; decreases execution time at cost of memory footprint'''
        log.debug("Trying to perform folding")

        optimizedCode = list()

        log.debug("Pre-optimization length is: %d" % len(machineCode))
        for i in range(2, len(machineCode)) :
            # Try to find 3 instructions in form LDA CONST, ADD CONST, STA TEMP
            currentInstr = machineCode[i]
            instrMinus1 = machineCode[i-1]
            instrMinus2 = machineCode[i-2]

            if(currentInstr.opcode == ST and (instrMinus1.opcode == ADD or instrMinus1.opcode == SUB or instrMinus1.opcode == MUL) and instrMinus2.opcode == LD) :
                if(isinstance(currentInstr.operand,TempVariable) and isinstance(instrMinus1.operand,Number) and isinstance(instrMinus2.operand,Number)) :
                    # Create a new constant that holds the computed result
                    foldedConst = None
                    if(instrMinus1.opcode == ADD) :
                        foldedConst = Number(instrMinus2.operand.value + instrMinus1.operand.value)
                    elif(instrMinus1.opcode == SUB) :
                        foldedConst = Number(instrMinus2.operand.value - instrMinus1.operand.value)
                    else :
                        foldedConst = Number(instrMinus2.operand.value * instrMinus1.operand.value)

                    entry = SymbolTableUtils.createOrGetSymbolTableReference(foldedConst,foldedConst.value,CONST)
                    machineCode[i-2] = MachineCode(LD, foldedConst, instrMinus2.label)
                    machineCode[i-1] = NoOp()
                    machineCode[i] = NoOp()

                    # Remove temp var from memory/symbol table

                    del GLOBAL_SYMBOL_TABLE[currentInstr.operand]

                    log.debug("Found folding candidate")

        for inst in machineCode :
            if not isinstance(inst, NoOp) :
                optimizedCode.append(inst)

        machineCode = optimizedCode

        log.debug("Post-optimization length is: %d" % len(machineCode))

        return machineCode

    def optimize( self, machineCode ) :
        log.debug("Optimizing")
        optimizedCode = self.performPeepholeOptimization(machineCode)
        optimizedCode = self.performConstantFolding(optimizedCode)
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
