reset

set terminal epslatex color
set output 'data.tex'

iindex1 = "H-pt-020"

dirH   = "H_dij_2_bornzerodamp/"
dirHJ  = "HJ_dij_2_bornzerodamp_runningsc/"
dirHqT = "HqT/"

LHE="LHEF_analysis_rebinned.top"
HqT="HqTspectrum-11_rebinned.top"
#HqTnorebin="HqTspectrum-11.top"

LHEH   = dirH.LHE
LHEHJ  = dirHJ.LHE
HqTH   = dirHqT.HqT

epsfile = "H-pt_comp_runsc.eps"

pasteLHEHJ_HqT = '< pastegnudata [H-pt-020] '.LHEHJ.' '.HqTH
pasteLHEH_HqT  = '< pastegnudata [H-pt-020] '.LHEH.' '.HqTH


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

#set label '${\mathrm d}\sigma/{\mathrm d}p_{\scriptscriptstyle T}^{\scriptscriptstyle H}$ [pb/GeV]' at screen 0.12 , 0.45 rotate 
#set ytics("$0$" 0,"$0.2$" 0.2,"$0.4$" 0.4)
set label '$p_{\scriptscriptstyle T}^{j_1} > 20$ GeV' at 50,0.0007  


set xtics format ""

set xrange [0:400] writeback
set yrange [1e-4:0.5]
set format y "$10^{%L}$"


set macros

# NNLO HqT / NLO POWHEG
kfact = 1.32

range1 = "(($1+$2)/2):3:1:2:($3-$4):($3+$4)"
range1kfac = "(($1+$2)/2):($3*kfact):1:2:(($3-$4)*kfact):(($3+$4)*kfact)"
range2 = "(($1+$2)/2):($3/$7):1:2:($3/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"
range3 = "(($1+$2)/2):($3/kfact/$7):1:2:($3/kfact/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3/kfact/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"
range4 = "(($1+$2)/2):($3*kfact/$7):1:2:($3*kfact/$7)*(1-(($4/$3)**2+($8/$7)**2)**0.5):($3*kfact/$7)*(1+(($4/$3)**2+($8/$7)**2)**0.5)"


set log y
#unset xtics
plot LHEHJ index iindex1 using @range1 lw 3  title 'LHE $Hj$',\
     LHEH index iindex1 using @range1kfac lc rgb "dark-green" lw 3  title '$K_{\rm fac} \times$ LHE $H$',\
     HqTH index iindex1 using @range1 lw 3  title '\mbox{{\tt HqT}}'

#     HqTH index iindex1 using (($1+$2)/2):($3) with lines title 'HqT'
#     HqTH index iindex1 using (($1+$2)/2):($3) with lines title 'HqT'
#     LHEHJJ  index iindex1 using @range1 lw 3  title 'LHE HJJ',\


set tmargin at screen 0.4
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 
#set ylabel 'ratio to NLO' 
#set label '\#/HqT' at screen 0.13 , 0.16 rotate
set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics
#set xtics("" 0,"1" 1,"2" 2)
set xlabel '$p_{\scriptscriptstyle T}^{\scriptscriptstyle H}$ [GeV]'


     
plot pasteLHEHJ_HqT index 0 using @range2 lw 3 ,\
     pasteLHEH_HqT  index 0 using @range4 lc rgb "dark-green" lw 3 ,\
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


