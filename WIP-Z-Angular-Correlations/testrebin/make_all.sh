#/bin/bash

# TeV
rm z-PYTHIA-alone-output.top
ln -s ../testrun-tev/z-PYTHIA-alone-output.top 

(cd ../; g77  filetop_rebin_A_PYT-alo.f completetop.f )
../a.out
mv  z-PYTHIA-alone-rebinned.top z-PYTHIA-alone-TeV-rebinned.top
td -deps z-PYTHIA-alone-TeV-rebinned.top

echo "===============> gv z-PYTHIA-alone-TeV-rebinned.eps"
gv z-PYTHIA-alone-TeV-rebinned.eps


rm z-pwhgalone-output.top
ln -s ../testrun-tev/zborn-pwhgalone-output.top z-pwhgalone-output.top 

(cd ../; g77  filetop_rebin_A_POW-alo.f completetop.f )
../a.out
mv  z-pwhgalone-rebinned.top zborn-pwhgalone-TeV-rebinned.top
td -deps zborn-pwhgalone-TeV-rebinned.top

echo "===============> gv zborn-pwhgalone-TeV-rebinned.eps"
gv zborn-pwhgalone-TeV-rebinned.eps


rm z-POWHEG+PYTHIA-output.top 
ln -s ../testrun-tev/zborn-POWHEG+PYTHIA-output.top z-POWHEG+PYTHIA-output.top 

(cd ../; g77  filetop_rebin_A_POW+PYT.f completetop.f )
../a.out
mv  z-POWHEG+PYTHIA-rebinned.top zborn-POWHEG+PYTHIA-TeV-rebinned.top
td -deps zborn-POWHEG+PYTHIA-TeV-rebinned.top

echo "===============> gv zborn-POWHEG+PYTHIA-TeV-rebinned.eps"
zborn-POWHEG+PYTHIA-TeV-rebinned.eps 


rm z-pwhgalone-output.top
ln -s ../testrun-tev/z-pwhgalone-output.top 
(cd ../; g77  filetop_rebin_A_POW-alo.f completetop.f )
../a.out
mv  z-pwhgalone-rebinned.top z-pwhgalone-TeV-rebinned.top
td -deps z-pwhgalone-TeV-rebinned.top

echo "===============> gv z-pwhgalone-TeV-rebinned.eps"
 gv z-pwhgalone-TeV-rebinned.eps

rm z-POWHEG+PYTHIA-output.top
ln -s ../testrun-tev/z-POWHEG+PYTHIA-output.top 

(cd ../; g77  filetop_rebin_A_POW+PYT.f completetop.f )
../a.out
mv  z-POWHEG+PYTHIA-rebinned.top z-POWHEG+PYTHIA-TeV-rebinned.top
td -deps z-POWHEG+PYTHIA-TeV-rebinned.top

echo "===============> gv z-POWHEG+PYTHIA-TeV-rebinned.eps"
gv z-POWHEG+PYTHIA-TeV-rebinned.eps

#LHC

rm z-PYTHIA-alone-output.top
ln -s ../testrun-tev/z-PYTHIA-alone-output.top 

(cd ../; g77  filetop_rebin_A_PYT-alo.f completetop.f )
../a.out
mv  z-PYTHIA-alone-rebinned.top z-PYTHIA-alone-LHC-rebinned.top
td -deps z-PYTHIA-alone-LHC-rebinned.top

echo "===============> gv z-PYTHIA-alone-LHC-rebinned.eps"
gv z-PYTHIA-alone-LHC-rebinned.eps

rm z-pwhgalone-output.top
ln -s ../testrun-tev/zborn-pwhgalone-output.top z-pwhgalone-output.top 

(cd ../; g77  filetop_rebin_A_POW-alo.f completetop.f )
../a.out
mv  z-pwhgalone-rebinned.top zborn-pwhgalone-LHC-rebinned.top
td -deps zborn-pwhgalone-LHC-rebinned.top

echo "===============> gv zborn-pwhgalone-LHC-rebinned.eps"
zborn-pwhgalone-LHC-rebinned.eps

rm z-POWHEG+PYTHIA-output.top 
ln -s ../testrun-tev/zborn-POWHEG+PYTHIA-output.top z-POWHEG+PYTHIA-output.top 

(cd ../; g77  filetop_rebin_A_POW+PYT.f completetop.f )
../a.out
mv  z-POWHEG+PYTHIA-rebinned.top zborn-POWHEG+PYTHIA-LHC-rebinned.top
td -deps zborn-POWHEG+PYTHIA-LHC-rebinned.top

echo "===============> gv zborn-POWHEG+PYTHIA-LHC-rebinned.eps"
gv zborn-POWHEG+PYTHIA-LHC-rebinned.eps

rm z-pwhgalone-output.top
ln -s ../testrun-tev/z-pwhgalone-output.top 
(cd ../; g77  filetop_rebin_A_POW-alo.f completetop.f )
../a.out
mv  z-pwhgalone-rebinned.top z-pwhgalone-LHC-rebinned.top
td -deps z-pwhgalone-LHC-rebinned.top

echo "===============> gv z-pwhgalone-LHC-rebinned.eps"
gv z-pwhgalone-LHC-rebinned.eps

rm z-POWHEG+PYTHIA-output.top
ln -s ../testrun-tev/z-POWHEG+PYTHIA-output.top 

(cd ../; g77  filetop_rebin_A_POW+PYT.f completetop.f )
../a.out
mv  z-POWHEG+PYTHIA-rebinned.top z-POWHEG+PYTHIA-LHC-rebinned.top
td -deps z-POWHEG+PYTHIA-LHC-rebinned.top

echo "===============> gv z-POWHEG+PYTHIA-LHC-rebinned.eps"
gv z-POWHEG+PYTHIA-LHC-rebinned.eps


rm z-POWHEG+PYTHIA-output.top
rm z-pwhgalone-output.top
rm z-PYTHIA-alone-output.top
rm *.completed 