import programext
import interpreterext
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
    def indexOfLDOFP(machineCode) :
        i = 0
        for line in machineCode :
            if(line.operand == LDOFP) :
                return i
            else :
                return -1

    @staticmethod
    def containsLDOFP(machineCode) :
        idx = Linker.indexOfLDOFP(machineCode)
        while(idx != -1) :
            log.debug("Index at: "+str(idx))        

    @staticmethod
    def translateLDOFP(machineCode) :
        while(Linker.containsLDOFP(machineCode)) :
           log.debug("Translation needed") 


    @staticmethod
    def linkSymbolicRALToRAL(symbolTable, machineCode) :
        for key in symbolTable.iterkeys() :
            log.debug(key)

        Linker.translateLDOFP(machineCode)

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


