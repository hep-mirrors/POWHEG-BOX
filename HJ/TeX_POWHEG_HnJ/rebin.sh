#!/bin/bash

# Run this before making figures; it rebins all given files and tags.
# More tags with different binnings can be given.


gfortran -o rebin rebin.f

rebinex=`dirname $0`/rebin

cd plots

for dir in H_dij_2_bornzerodamp HJ_dij_2_bornzerodamp  HJ_dij_2_bornzerodamp_bornonly HJ_dij_2_bornzerodamp_runningsc HJ_dij_2_bornzerodamp_runningsc_bornonly H_dij_2_bornzerodamp HJJ_dij_2_bornzerodamp  HJJ_dij_2_bornzerodamp_bornonly HJJ_dij_2_bornzerodamp_runningsc HJJ_dij_2_bornzerodamp_runningsc_bornonly
do

cd $dir


for file in LHEF_analysis.top  pwgNLO.top  pwgPOWHEG+PYTHIA-output_had.top  pwgPOWHEG+PYTHIA-output_nohad.top pwgPOWHEG+PYTHIA-output_nohad_scalup.top
do


if [ -e $file ]
then

cp $file rebinned.top

echo rebinning $file

for tag in H-pt-020 H-pt-050 H-pt-100 j1-pt-020 j1-pt-050 j1-pt-100  j2-pt-020 j2-pt-050 j2-pt-100  j3-pt-020 j3-pt-050 j3-pt-100
do

echo $tag
../../$rebinex <<EOF
$tag
0
10
20
30
40
50
60
80
100
120
140
160
200
240
280
320
360
400
400
rebinned.top
EOF
mv -f rebinned_rebinned.top rebinned.top
done

for tag in j1-ptzoom-020 j1-ptzoom-050 j1-ptzoom-100  j2-ptzoom-020 j2-ptzoom-050 j2-ptzoom-100  j3-ptzoom-020 j3-ptzoom-050 j3-ptzoom-100
do
echo $tag
../../$rebinex <<EOF
$tag
0.10000000E+01
0.30000000E+01
0.50000000E+01
0.70000000E+01
0.90000000E+01
0.11000000E+02
0.13000000E+02
0.15000000E+02
0.17000000E+02
0.19000000E+02
0.21000000E+02
0.25000000E+02
0.29000000E+02
0.33000000E+02
0.37000000E+02
0.43000000E+02
0.49000000E+02
0.55000000E+02
0.61000000E+02
0.67000000E+02
0.75000000E+02
0.83000000E+02
0.91000000E+02
0.99000000E+02
0.10900000E+03
0.11900000E+03
0.12900000E+03
0.13900000E+03
0.15100000E+03
0.15100000E+03
rebinned.top
EOF
mv -f rebinned_rebinned.top rebinned.top
done

for tag in ptrel1-020 ptrel1-050 ptrel1-100 ptrel2-020 ptrel2-050 ptrel2-100
do
echo $tag
../../$rebinex <<EOF
$tag
 0.00000000E+00
 0.10000000E+01
 0.20000000E+01
 0.30000000E+01
 0.50000000E+01
 0.75000000E+01
 0.10000000E+02
 0.12500000E+02
 0.15000000E+02
 0.17500000E+02
 0.20000000E+02
 0.20000000E+02
rebinned.top
EOF
mv -f rebinned_rebinned.top rebinned.top
done


newfile=`echo $file | sed 's/\.top/_rebinned.top/'`

mv rebinned.top $newfile

fi


done

cd ../

done

cd ../

\rm rebin


