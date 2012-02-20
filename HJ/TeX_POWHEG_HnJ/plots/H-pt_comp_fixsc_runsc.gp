reset

set terminal epslatex color
set output 'data.tex'

iindex1 = "H-pt-020"

dirH   = "H_dij_2_bornzerodamp/"
dirHJfix  = "HJ_dij_2_bornzerodamp/"
dirHJrun  = "HJ_dij_2_bornzerodamp_runningsc/"
dirHqT = "HqT/"

LHE="LHEF_analysis_rebinned.top"
HqT="HqTspectrum-11_rebinned.top"
#HqTnorebin="HqTspectrum-11.top"

LHEH   = dirH.LHE
LHEHJfix  = dirHJfix.LHE
LHEHJrun  = dirHJrun.LHE
HqTH   = dirHqT.HqT

epsfile = "H-pt_comp_fixsc_runsc.eps"

pasteLHEHJfix_HqT = '< pastegnudata [H-pt-020] '.LHEHJfix.' '.HqTH
pasteLHEHJrun_HqT = '< pastegnudata [H-pt-020] '.LHEHJrun.' '.HqTH
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

set label '${\mathrm d}\sigma/{\mathrm d}p_{\scriptscriptstyle T}^{\scriptscriptstyle H}$ [pb/GeV]' at screen 0.14 , 0.5 rotate 
#set ytics("$0$" 0,"$0.2$" 0.2,"$0.4$" 0.4)
set label '$p_{\scriptscriptstyle T}^{j_1} > 20$ GeV' at 50,0.0007  
set label '$K_{\rm fac}=1.32$' at 270, 0.002


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
plot LHEHJfix index iindex1 using @range1 lw 3  title 'LHE $Hj$  $\mu=m_{\scriptscriptstyle H}$',\
     LHEHJrun index iindex1 using @range1 lc rgb "dark-green" lw 3  title 'LHE $Hj$ $\mu=p_{\scriptscriptstyle T}^{\scriptscriptstyle \rm UB}$',\
     LHEH index iindex1 using @range1kfac lw 3  title '$K_{\rm fac} \times$ LHE $H$',\
     HqTH index iindex1 using @range1 lw 3  title '\mbox{{\tt HqT}}'

#     HqTH index iindex1 using (($1+$2)/2):($3) with lines title 'HqT'
#     HqTH index iindex1 using (($1+$2)/2):($3) with lines title 'HqT'
#     LHEHJfixJ  index iindex1 using @range1 lw 3  title 'LHE HJJ',\


set tmargin at screen 0.4
set bmargin at screen 0.2
set nolog y
set yrange [0:2]
set xrange restore
set format x

set key off

unset label 
#set ylabel 'ratio to NLO' 
set label '\mbox{\#/{\tt HqT}}' at screen 0.14 , 0.21 rotate
set ytics("0" 0,"0.5" 0.5,"1" 1,"1.5" 1.5," " 2)
set xtics
#set xtics("" 0,"1" 1,"2" 2)
set xlabel '$p_{\scriptscriptstyle T}^{\scriptscriptstyle H}$ [GeV]'


     
plot pasteLHEHJfix_HqT index 0 using @range2 lw 3 ,\
     pasteLHEHJrun_HqT index 0 using @range2 lc rgb "dark-green" lw 3 ,\
     pasteLHEH_HqT  index 0 using @range4 lw 3 ,\
     1 with lines lt 2 lc 4


     
unset multiplot

shell1 = "dvips -E tot12.dvi -o " . epsfile
shell2 = "bbox " . epsfile
shell3 = "epstopdf " . epsfile

set output
!latex tot12.tex
system shell1
system shell2
system shell3


