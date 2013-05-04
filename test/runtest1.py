import sys
import os
import subprocess

test_dir = 'test/SampleInputs1'
answers_dir = 'test/answers1'
output_dir = 'test/output1'

interpreter = 'python interpreterext.py'

sim = './ram'

tests = os.listdir(test_dir)

#Create the output dir, which will be cleaned on 'make clean'
os.makedirs(output_dir)

for test in tests:
    print("Running test: %s/%s" % ( test_dir, test))
    os.system('%s < ./%s/%s > %s/%s' % (interpreter,test_dir,
                                       test,output_dir,test))

    # Compare simulator output for both optimized and non-optimized code
    os.system('%s program-non-opt.txt mem-dump.txt > %s/%s.program-non-opt-sim-output.txt' % (sim,output_dir,test) )
    os.system('%s program-opt.txt mem-dump-opt.txt > %s/%s.program-opt-sim-output.txt' % (sim,output_dir,test) )

    # Copy the generated machine code and memory tables for additional diff checks
    os.system('cp symOut.txt %s/%s.symOut.txt' % (output_dir,test) )
    os.system('cp program-non-opt.txt %s/%s.program-non-opt.txt' % (output_dir,test) )
    os.system('cp mem-dump.txt %s/%s.mem-dump.txt' % (output_dir,test) )
    os.system('cp program-opt.txt %s/%s.program-opt.txt' % (output_dir,test) )
    os.system('cp mem-dump-opt.txt %s/%s.mem-dump-opt.txt' % (output_dir,test) )

 
