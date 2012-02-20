reset

set terminal epslatex color
set output 'data.tex'

iindex1 = "H-y-020"
iindex2 = "H-y-050"
iindex3 = "H-y-100"

dir = "HJJ_dij_2_bornzerodamp_runningsc/" 
NLO = dir."pwgNLO_rebinned.top"
LHE = dir."LHEF_analysis_rebinned.top"
PY  = dir."pwgPOWHEG+PYTHIA-output_nohad_rebinned.top"

epsfile = "HJJ_H-y_runsc.eps"

pasteLHNLO = '< paste '.LHE.' '.NLO
pastePYNLO = '< paste '.PY.' '.NLO

set multiplot

set origin 0.0,0.0
set size 1,1

set lmargin at screen 0.25
set rmargin at screen 0.95
set tmargin at screen 0.95
set bmargin at screen 0.4

set key at 1.2, 0.007 spacing 1.25

set datafile fortran
#set style data lines
set style data xyerrorbars

#set label '${\mathrm d}\sigma/{\mathrm d}y_{\scriptscriptstyle H}$ [pb]' at screen 0.12 , 0.50 rotate
#set ytics("$0$" 0,"$0.2$" 0.2,"$0.4$" 0.4)
#set label '$p_{{\scriptscriptstyle Tj}} > 20$ GeV' at 0.7,0.15  


set xtics format ""

set xrange [-4:4] writeback
set yrange [5e-4:2]
set format y "$10^{%L}$"

set macros
range1 = "(($1+$2)/2):3:1:2:($3-$4):($3+$4)"
range2 = "(($1+$2)/2):($3/$7):1:2:($3/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"

set log y
unset xtics
plot LHE index iindex1 using @range1 lw 3  title 'LHE',\
     PY  index iindex1 using @range1 lc rgb "dark-green" lw 3  title 'PY',\
     NLO index iindex1 using @range1 lw 3  title 'NLO'

plot LHE index iindex2 using @range1 lw 3  title '',\
     PY  index iindex2 using @range1 lc rgb "dark-green" lw 3  title '',\
     NLO index iindex2 using @range1 lw 3  title ''

plot LHE index iindex3 using @range1 lw 3  title '',\
     PY  index iindex3 using @range1 lc rgb "dark-green" lw 3  title '',\
     NLO index iindex3 using @range1 lw 3  title ''


set tmargin at screen 0.4
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 
#set ylabel 'ratio to NLO' 
#set label '\#/NLO' at screen 0.14 , 0.16 rotate
set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics
#set xtics("" 0,"1" 1,"2" 2)
set xlabel '$y_{\scriptscriptstyle H}$'


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


