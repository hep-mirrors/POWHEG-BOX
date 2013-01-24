#!/bin/bash

# this function does a single step of grid improvement

cat powheg.input-save | sed 's/ncall2.*/ncall2 0/' > powheg.input

echo 1 | ../pwhg_main > run-0.log 2>&1


cat powheg.input-save | sed 's/nubound.*/nubound 0/' > powheg.input

# compute in parallel upper bounding grid
for i in {1..10}
do
echo $i | ../pwhg_main > run-$i.log 2>&1 &
done

wait

cat powheg.input-save | sed 's/numevts.*/numevts 0/' > powheg.input

# compute in parallel upper bounds for event generation
for i in {1..10}
do
echo $i | ../pwhg_main > run-$i.log 2>&1 &
done

wait

cat powheg.input-save > powheg.input

# Generate events in parallel
for i in {1..10}
do
echo $i | ../pwhg_main > run-$i.log 2>&1 &
done



