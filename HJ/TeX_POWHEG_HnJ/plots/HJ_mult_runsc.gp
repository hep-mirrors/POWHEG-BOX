reset

set terminal epslatex color
set output 'data.tex'

iindex1 = "Njet-020"
iindex2 = "Njet-050"
iindex3 = "Njet-100"

dir = "HJ_dij_2_bornzerodamp_runningsc/" 
NLO = dir."pwgNLO_rebinned.top"
LHE = dir."LHEF_analysis_rebinned.top"
PY  = dir."pwgPOWHEG+PYTHIA-output_nohad_rebinned.top"

epsfile = "HJ_mult_runsc.eps"

pasteLHNLO = '< paste '.LHE.' '.NLO
pastePYNLO = '< paste '.PY.' '.NLO

set multiplot

set origin 0.0,0.0
set size 1,1

set lmargin at screen 0.25
set rmargin at screen 0.48
set tmargin at screen 0.94
set bmargin at screen 0.5

set key spacing 1.25

set datafile fortran
#set style data lines
set style data xyerrorbars

#set label '$\sigma$ [pb]' at screen 0.14 , 0.65 rotate
#set ytics("$0$" 0,"$0.2$" 0.2,"$0.4$" 0.4)
set label '$p_{\scriptscriptstyle T}^{\scriptscriptstyle j}\hskip -0.15cm >\hskip -0.15cm 20\,$GeV' at 0.54,0.15  


set xtics format ""

set xrange [0.45:2.55] writeback
set yrange [0.03:7]
set format y "$10^{%L}$"

set macros
range1 = "(($1+$2)/2):3:1:2:($3-$4):($3+$4)"
range2 = "(($1+$2)/2):($3/$7):1:2:($3/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"

set log y
unset xtics
plot LHE index iindex1 using @range1 lw 3  title '',\
     PY  index iindex1 using @range1 lc rgb "dark-green" lw 3  title '',\
     NLO index iindex1 using @range1 lw 3  title ''

set tmargin at screen 0.5
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 

#set ylabel 'ratio to NLO' 
#set label 'ratio to NLO' at screen 0.14 , 0.18 rotate
set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics("" 0,"1" 1,"2" 2)

plot pasteLHNLO index iindex1 using @range2 lw 3 ,\
     pastePYNLO index iindex1 using @range2 lc rgb "dark-green" lw 3 ,\
     1 with lines lt 2 lc 3


########################################


set lmargin at screen 0.48
set rmargin at screen 0.71
set tmargin at screen 0.94
set bmargin at screen 0.5

set key spacing 1.25

set datafile fortran
#set style data lines
set style data xyerrorbars

set ytics(" " 0," " 0.2," " 0.4)

set xtics format ""

set xrange [0.45:2.55] writeback
set yrange [0.03:7]
unset ytics

set log y
unset ylabel 
unset xlabel 
unset xtics

set label '$p_{\scriptscriptstyle T}^{\scriptscriptstyle j}\hskip -0.15cm >\hskip -0.15cm 50\,$GeV' at 0.54,0.15 

plot LHE index iindex2 using @range1 lw 3  title '',\
     PY  index iindex2 using @range1 lc rgb "dark-green" lw 3  title '',\
     NLO index iindex2 using @range1 lw 3  title ''

set tmargin at screen 0.5
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 


#set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics("" 0,"1" 1,"2" 2)
set xlabel '$N_{\rm jets}$'

plot pasteLHNLO index iindex2 using @range2 lw 3 ,\
     pastePYNLO index iindex2 using @range2 lc rgb "dark-green" lw 3 ,\
     1 with lines lt 2 lc 3





########################################


set lmargin at screen 0.71
set rmargin at screen 0.94
set tmargin at screen 0.94
set bmargin at screen 0.5

set key spacing 1.25

set datafile fortran
#set style data lines
set style data xyerrorbars

set ytics(" " 0," " 0.2," " 0.4)

set xtics format ""

set xrange [0.45:2.55] writeback
set yrange [0.03:7]
unset ytics

set log y
unset ylabel 
unset xlabel 
unset xtics
#set ytics(" " 1)

set label '$p_{\scriptscriptstyle T}^{\scriptscriptstyle j}\hskip -0.15cm >\hskip -0.2cm 100\,$GeV' at 0.54,0.15 

plot LHE index iindex3 using @range1 lw 3  title 'LHE' ,\
     PY  index iindex3 using @range1 lc rgb "dark-green" lw 3  title 'PY',\
     NLO index iindex3 using @range1 lw 3  title 'NLO'

set tmargin at screen 0.5
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 


#set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics("" 0,"1" 1,"2" 2)
unset ytics

plot pasteLHNLO index iindex3 using @range2 lw 3 ,\
     pastePYNLO index iindex3 using @range2 lc rgb "dark-green" lw 3 ,\
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


