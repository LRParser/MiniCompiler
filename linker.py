from programext import *

class Linker(object) :

    @staticmethod
    def linkAddressesToSymbolTable(symbolTable) :
        currentAddr = 2
        for const in symbolTable.iterate(CONST) :
            const.address = currentAddr
            currentAddr = currentAddr + 1

        for var in symbolTable.iterate(VAR) :
            var.address = currentAddr
            currentAddr = currentAddr + 1

        for temp in symbolTable.iterate(TEMP) :
            temp.address = currentAddr
            currentAddr = currentAddr + 1


    @staticmethod
    def linkSymbolicRALToRAL(symbolTable, machineCode) :
        for key in symbolTable.iterkeys() :
            log.debug(key)
        for line in machineCode :
            log.debug("Going to link: "+str(line))
            if(line.operand is None or line.is_jump) :
                # No need to link the HLT instruction
                continue
            elif(line.operand == SPADDR) :
                line.operand = 1
            else :
                linkedAddr = symbolTable[line.operand].address
                line.operand = linkedAddr
            log.debug("Linked operand %s to address: %s" % (line.operand, linkedAddr))

        for inst in machineCode:
            if (inst.is_jump):
                for i, item in enumerate(machineCode):
                    if inst.operand == item.label:
                        log.debug("Found jump")
                        inst.operand = i + 1

        return machineCode


