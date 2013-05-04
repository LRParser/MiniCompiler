#!/bin/sh
test_dir="test/SampleInputs1/"

if [ -z $1 ]
then
    echo "usage: $0 testname"
    exit
elif [ $1 == "--list" ]
then
    ls $test_dir
    exit
fi
make compile < $1
echo "******************************"
echo "Running Simulator"
make run
echo "******************************"
echo "Running Optimized Simulator"
make run-op
echo "******************************"
echo "Original Program"
cat $1
