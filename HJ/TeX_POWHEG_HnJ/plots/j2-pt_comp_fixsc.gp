reset

set terminal epslatex color
set output 'data.tex'

iindex1 = "j2-pt-020"

dirHJ = "HJ_dij_2_bornzerodamp/" 
dirHJJ = "HJJ_dij_2_bornzerodamp/" 
LHEHJ = dirHJ."LHEF_analysis_rebinned.top"
LHEHJJ = dirHJJ."LHEF_analysis_rebinned.top"

epsfile = "j2-pt_comp_fixsc.eps"

pasteLH = '< paste '.LHEHJJ.' '.LHEHJ

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

set label '${\mathrm d}\sigma/{\mathrm d}p_{\scriptscriptstyle T}^{\scriptscriptstyle j_2}$ [pb/GeV]' at screen 0.12 , 0.45 rotate
#set ytics("$0$" 0,"$0.2$" 0.2,"$0.4$" 0.4)
set label '$p_{\scriptscriptstyle T}^{\scriptscriptstyle j_1} > 20$ GeV' at 50,0.00002  


set xtics format ""

set xrange [20:400] writeback
set yrange [1e-6:0.3]
set format y "$10^{%L}$"


set macros
range1 = "(($1+$2)/2):3:1:2:($3-$4):($3+$4)"
range2 = "(($1+$2)/2):($3/$7):1:2:($3/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"

set log y
unset xtics
plot LHEHJJ  index iindex1 using @range1 lw 3  title 'LHE $Hjj$',\
     LHEHJ index iindex1 using @range1 lc rgb "dark-green" lw 3  title 'LHE $Hj$'
     


set tmargin at screen 0.4
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 
#set ylabel 'ratio to NLO' 
set label '\#/$Hj$' at screen 0.13 , 0.20 rotate
set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics
#set xtics("" 0,"1" 1,"2" 2)
set xlabel '$p_{\scriptscriptstyle T}^{\scriptscriptstyle j_2}$ [GeV]'


plot pasteLH index iindex1 using @range2 lw 3 ,\
     1 with lines lt 2  lc rgb "dark-green"


unset multiplot

shell1 = "dvips -E tot.dvi -o " . epsfile
shell2 = "bbox " . epsfile
shell3 = "epstopdf " . epsfile

set output
!latex tot.tex
system shell1
system shell2
system shell3


