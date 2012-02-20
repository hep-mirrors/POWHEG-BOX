#!/bin/bash


cd plots

for dir in H_dij_2_bornzerodamp HJ_dij_2_bornzerodamp  HJ_dij_2_bornzerodamp_bornonly HJ_dij_2_bornzerodamp_runningsc HJ_dij_2_bornzerodamp_runningsc_bornonly H_dij_2_bornzerodamp HJJ_dij_2_bornzerodamp  HJJ_dij_2_bornzerodamp_bornonly HJJ_dij_2_bornzerodamp_runningsc HJJ_dij_2_bornzerodamp_runningsc_bornonly
do

cd $dir

\rm  LHEF_analysis_rebinned.top  pwgNLO_rebinned.top  pwgPOWHEG+PYTHIA-output_had_rebinned.top  pwgPOWHEG+PYTHIA-output_nohad_rebinned.top pwgPOWHEG+PYTHIA-output_nohad_scalup_rebinned.top pwgPOWHEG+PYTHIA-output_had_scalup_rebinned.top

cd ../

done


cd ../


\rm rebin
