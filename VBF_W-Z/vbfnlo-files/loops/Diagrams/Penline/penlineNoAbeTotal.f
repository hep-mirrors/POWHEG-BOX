       subroutine penlineNoAbeTotal(p1,p2,p3,p4,p5,barpsi_p4,psi_p1,mup2
     -   ,mup3,mup5,alpha,musqIn,gaugetest,comp,resultgauge,result,Div)
c ************************************************************************************
c ************************************************************************************
c Author: Francisco Campanario
c E-mail: francam@particle.uni-karlsruhe.de
c Date: 3/7/2008
c Modified:4/8/2008
c ******************************************************************************
       IMPLICIT NONE
       Real*8   p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
       Complex*16 barpsi_p4(2),psi_p1(2),mup2(0:3),mup3(0:3),mup5(0:3) 
       Complex*16 result
       Complex*16 resultgauge(3),F(280)
       Real*8 musq
       Integer alpha,comp,gaugetest,Div
       Real*8 musqIn,P(685) 

       common/FfunctionsNOABEmmm/F,P
       SAVE/FfunctionsNOABEmmm/
       Common/musqInv/musq
       musq=musqIn
       If(Div.eq.0) then

       call penlineNoAbemmm(p1,p2,p3,p4,p5,barpsi_p4,psi_p1,mup2
     -   ,mup3,mup5,alpha,musq,gaugetest,comp,resultgauge,result)

       else
       call penlineNoAbemmmDiv(p1,p2,p3,p4,p5,barpsi_p4,psi_p1,mup2
     -   ,mup3,mup5,alpha,musq,gaugetest,comp,resultgauge,result,Div)

       endif

       End
