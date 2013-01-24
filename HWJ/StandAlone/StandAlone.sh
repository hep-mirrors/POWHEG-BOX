#!/bin/bash

# if a .f90 file consists of single module, copy it into
# the GoSamlib directory with the name of the module it defines
for file in `find ./GoSam_POWHEG/ -path "*.f90"`
do

nmodules=`grep -i '^ *end *module ' $file | wc -l`

if [ "$nmodules" = "1" ]
then
    modname=`grep -i '^ *end *module ' $file | sed 's/^ *end *module //'`
    \cp $file GoSamlib/$modname.f90
elif  [ "$nmodules" = "0" ]
then
:
else
    echo $file
    echo 'more than 1 module per file! exiting'
    exit -1
fi

done

# copy all needed fortran and include files from the contrib directory
# to the GoSamlib subdirectory

contrib=/home/gionata/Programmi/gosam-contrib-1.0/

for file in  `find $contrib -path "*.[hf]"`  `find $contrib -path "*.[hf]90"`
do
\cp $file GoSamlib/
done

# fix msamurai.f90 (it is a DOS file, and makedepf90 does not handle that)

cd GoSamlib

mv msamurai.f90 msamurai.f90-dos

tr -d '\r' < msamurai.f90-dos > msamurai.f90

# create the dependencies file

makedepf90 *.f90 > deps.txt

cd ..

cat Makefile.nodeps GoSamlib/deps.txt > Makefile.deps


# this should be it!