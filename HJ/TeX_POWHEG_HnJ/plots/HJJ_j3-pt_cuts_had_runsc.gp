reset

set terminal epslatex color
set output 'data.tex'

iindex1 = "j3-ptzoom-020"
iindex2 = "j3-ptzoom-050"
iindex3 = "j3-ptzoom-100"



dir = "HJJ_dij_2_bornzerodamp_runningsc/" 
NLO = dir."pwgNLO_rebinned.top"
LHE = dir."LHEF_analysis_rebinned.top"
PYSCALUP  = dir."pwgPOWHEG+PYTHIA-output_nohad_scalup_rebinned.top"
PY  = dir."pwgPOWHEG+PYTHIA-output_nohad_rebinned.top"
HAD = dir."pwgPOWHEG+PYTHIA-output_had_rebinned.top"

epsfile = "HJJ_j3-pt_cuts_had_runsc.eps"

pasteLHNLO = '< paste '.LHE.' '.NLO
pastePYNLO = '< paste '.PY.' '.NLO

set multiplot

set origin 0.0,0.0
set size 1,1

set lmargin at screen 0.25
set rmargin at screen 0.95
set tmargin at screen 0.95
set bmargin at screen 0.2

#set key at 1.2, 0.03 spacing 1.25
set key spacing 1.6

set datafile fortran
#set style data lines
#set style data xyerrorbars
set style data xyerrorlines

#set label '${\mathrm d}\sigma/{\mathrm d}p_{\scriptscriptstyle T}^{\scriptscriptstyle j_3}$ [pb/GeV]' at screen 0.12 , 0.38 rotate 
#set ytics("$0$" 0,"$0.2$" 0.2,"$0.4$" 0.4)
#set label '$p_{\scriptscriptstyle T}{\scriptscriptstyle j_1} > 20$ GeV' at 0.7,0.15  


#set xtics format ""

set xrange [0:120] 
set yrange [5e-5:0.5]
set format y "$10^{%L}$"

set macros
range1 = "(($1+$2)/2):3:1:2:($3-$4):($3+$4)"
range2 = "(($1+$2)/2):($3/$7):1:2:($3/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"

set log y
set xlabel '$p_{\scriptscriptstyle T}^{\scriptscriptstyle  j_3}$ [GeV]'

plot PY  index iindex1 using @range1 lw 3  title 'PY',\
     PYSCALUP index iindex1 using @range1 lc rgb "dark-green" lw 3  title 'PYscalup',\
     HAD index iindex1 using @range1 lw 3   title 'HAD'
#     HAD index iindex1 using (($1+$2)/2):3 with lines title 'HAD'     

plot PY  index iindex2 using @range1 lw 3  title '',\
     PYSCALUP index iindex2 using @range1 lc rgb "dark-green" lw 3  title '' ,\
     HAD index iindex2 using @range1 lw 3   title ''

plot PY  index iindex3 using @range1 lw 3  title '',\
     PYSCALUP index iindex3 using @range1 lc rgb "dark-green" lw 3  title '',\
     HAD index iindex3 using @range1 lw 3   title ''



unset multiplot

shell1 = "dvips -E tot.dvi -o " . epsfile
shell2 = "bbox " . epsfile
shell3 = "epstopdf " . epsfile

set output
!latex tot.tex
system shell1
system shell2
system shell3


