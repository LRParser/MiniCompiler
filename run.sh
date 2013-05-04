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
make compile < $test_dir/$1
echo "******************************"
echo "Original Program"
cat $test_dir/$1
