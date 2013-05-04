import sys
import os
import subprocess

test_dir = 'test/SampleInputs1'
answers_dir = 'test/answers1'
output_dir = 'test/output1'

interpreter = 'python interpreterext.py'

tests = os.listdir(test_dir)

#Create the output dir, which will be cleaned on 'make clean'
os.makedirs(output_dir)

for test in tests:
    print("Running test: %s/%s" % ( test_dir, test))
    os.system('%s < ./%s/%s > %s/%s' % (interpreter,test_dir,
                                       test,output_dir,test))
