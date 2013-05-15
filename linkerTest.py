from programext import *
from interpreterext import *
import unittest
import sys
import logging

class LinkerTests(unittest.TestCase) :

    def test_LDO(self) :
        machineCode = list()
        machineCode.append(MachineCode(LDOFP,1))
        machineCode = Linker.linkSymbolicRALToRAL(GLOBAL_SYMBOL_TABLE,machineCode)

        # Print each line of the generated linked RAL
        containsLDOFP = False
        for line in machineCode :
            if(line.opcode == LDOFP) :
                containsLDOFP = True
            log.debug(line)

        self.assertFalse(containsLDOFP)

if __name__ == '__main__':
    unittest.main()

