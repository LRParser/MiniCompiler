from programext import *
import unittest

class LinkerTests(unittest.TestCase) :

    def test_LDO(self) :
        machineCode = list()
        machineCode.append(MachineCode(LDOFP,1))
        self.assertTrue(True)

if __name__ == '__main__':
    unittest.main()

