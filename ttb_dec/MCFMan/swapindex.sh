#!/bin/bash

if [ a$1 = a ]
then
echo usage: swapindex file1 file2 ... filen
echo usage: swapindex *.top
exit -1
fi

for i in $*
do
sed -i 's/#\( *index  *[0-9]*\)  *\([^ ]*\)/# \2 \1/' $i
done

