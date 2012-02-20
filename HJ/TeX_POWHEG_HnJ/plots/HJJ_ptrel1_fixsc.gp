reset

set terminal epslatex color
set output 'data.tex'

iindex1 = "ptrel1-020"

dir = "HJJ_dij_2_bornzerodamp/" 
NLO = dir."pwgNLO_rebinned.top"
LHE = dir."LHEF_analysis_rebinned.top"
PY  = dir."pwgPOWHEG+PYTHIA-output_nohad_rebinned.top"

epsfile = "HJJ_ptrel1_fixsc.eps"

pasteLHNLO = '< paste '.LHE.' '.NLO
pastePYNLO = '< paste '.PY.' '.NLO

set multiplot

set origin 0.0,0.0
set size 1,1

set lmargin at screen 0.25
set rmargin at screen 0.95
set tmargin at screen 0.95
set bmargin at screen 0.4

#set key at 1.2, 0.03 spacing 1.25
set key spacing 1.6
set datafile fortran
#set style data lines
set style data xyerrorbars

set label '${\mathrm d}\sigma/{\mathrm d}p_{\scriptscriptstyle T}^{\scriptscriptstyle {\rm rel},j_1}$ [pb/GeV]' at screen 0.12 , 0.45 rotate
#set ytics("$0$" 0,"$0.2$" 0.2,"$0.4$" 0.4)
set label '$p_{\scriptscriptstyle T}^{\scriptscriptstyle j_1} > 20$ GeV' at 6.5,2  
set label '$p_{\scriptscriptstyle T}^{\scriptscriptstyle j_2} > 20$ GeV' at 6.5,0.4 


set xtics format ""

set xrange [0:20] writeback
set yrange [3e-4:10]
set format y "$10^{%L}$"


set macros
range1 = "(($1+$2)/2):3:1:2:($3-$4):($3+$4)"
range2 = "(($1+$2)/2):($3/$7):1:2:($3/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"

set log y
unset xtics
plot LHE index iindex1 using @range1 lw 3  title 'LHE',\
     PY  index iindex1 using @range1 lc rgb "dark-green" lw 3  title 'PY',\
     NLO index iindex1 using @range1 lw 3  title 'NLO'


set tmargin at screen 0.4
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 
#set ylabel 'ratio to NLO' 
set label '\#/NLO' at screen 0.13 , 0.16 rotate
set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics
#set xtics("" 0,"1" 1,"2" 2)
set xlabel '$p_{\scriptscriptstyle T}^{\scriptscriptstyle {\rm rel},j_1}$ [GeV]'


plot pasteLHNLO index iindex1 using @range2 lw 3 ,\
     pastePYNLO index iindex1 using @range2 lc rgb "dark-green" lw 3 ,\
     1 with lines lt 2 lc 3


unset multiplot

shell1 = "dvips -E tot.dvi -o " . epsfile
shell2 = "bbox " . epsfile
shell3 = "epstopdf " . epsfile

set output
!latex tot.tex
system shell1
system shell2
system shell3

