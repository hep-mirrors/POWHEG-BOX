#!/bin/bash

# example of a run script for a parallel run on ncores cpu's
ncores=7


function runthem {
for i in $(seq 1 $ncores)
do
echo $i | $prg > $logfile-$i.log 2>&1 &
done
}


prg=../pwhg_main
runthem


