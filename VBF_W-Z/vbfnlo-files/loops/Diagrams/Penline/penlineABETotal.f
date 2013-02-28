       subroutine penlineABETotal(p1,p2,p3,p4,p5,barpsi_p5,psi_p1,mup2,m
     -   up3,mup4,alpha,musqIn,ngluon,posgluon,gaugetest,comp,resultgau
     -   ge,result,resultgaugeb,resultb,Div)
c ************************************************************************************
c ************************************************************************************
c Author: Francisco Campanario
c E-mail: francam@particle.uni-karlsruhe.de
c Date: 1/4/2008
c Modified:30/7/2008
       IMPLICIT NONE
       Real*8   p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
       Complex*16 barpsi_p5(2),psi_p1(2),mup2(0:3),mup3(0:3),mup4(0:3) 
       Complex*16 result(8),resultg(8)
       Complex*16 resultgauge(3),resultgaugeb(3),resultb 
       Real*8 musq,musqIn,P(554)
       Integer alpha,comp,gaugetest,ngluon,posgluon, Div
       Complex*16 F(448),K(4)
       common/Ffunctionsmmm/F
       common/Pfunctionsmmm/P
       common/Kfunctionsmmm/K
       SAVE/Ffunctionsmmm/
       SAVE/Pfunctionsmmm/
       SAVE/Kfunctionsmmm/
       Common/musqInv/musq
       musq=musqIn
c************************************************************************************
       If(Div.eq.0) then
       call penlineABEmmm(p1,p2,p3,p4,p5,barpsi_p5,psi_p1,mup2,m
     -   up3,mup4,alpha,musq,ngluon,posgluon,gaugetest,comp,resultgau
     -   ge,result,resultgaugeb,resultb) 
       else
       call penlineABEmmmDiv(p1,p2,p3,p4,p5,barpsi_p5,psi_p1,mup2,m
     -   up3,mup4,alpha,musq,ngluon,posgluon,gaugetest,comp,resultgau
     -   ge,result,resultgaugeb,resultb,Div) 

       endif

       End
