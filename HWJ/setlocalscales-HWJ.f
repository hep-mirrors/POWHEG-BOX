      subroutine setlocalscales(iuborn,imode,rescfac)
c returns the rescaling factor including sudakov form factors and
c coupling rescaling, for born (imode=1) and NLO corrections (imode=2)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      include 'pwhg_st.h'
      include 'pwhg_flg.h'
      include 'pwhg_math.h'
      integer iuborn,imode
      real * 8 rescfac,expsudakov,expsud,sudakov,pwhg_alphas
      real * 8 ptw2,mw2,mu2,alphas,b0,optw2,omw2,orescfac
      integer oimode
      save optw2,omw2,orescfac,oimode
      data optw2/-1d0/
      logical ini
      data ini/.true./
      save ini
      real * 8 powheginput,factsc2min,frensc2min,as,y,b1,tmp
      save factsc2min,frensc2min,b0,b1
      if(ini) then
         factsc2min = powheginput("#factsc2min")
         frensc2min = powheginput("#frensc2min")
         if(factsc2min.lt.0) factsc2min = 0
         if(frensc2min.lt.0) frensc2min = 1
         call getq2min(1,tmp)
         write(*,*) ' ***** minimum Q of pdf:',sqrt(tmp)
         b0=(33d0-2d0*st_nlight)/(12*pi)
         b1=(153d0-19d0*st_nlight)/(24*pi**2)
         ini = .false.
      endif

      rescfac = 1

c$$$      if(flg_minlo_real) then
c$$$         if(nlegborn.eq.5) then
c$$$c We are doing W or Z
c$$$            ptw2 = (kn_cmpreal(1,3)+kn_cmpreal(1,4))**2
c$$$     1           + (kn_cmpreal(2,3)+kn_cmpreal(2,4))**2
c$$$            mw2  = (kn_cmpreal(0,3)+kn_cmpreal(0,4))**2
c$$$     1           - (kn_cmpreal(3,3)+kn_cmpreal(3,4))**2 - ptw2
c$$$         else
c$$$c We are doing the Higgs
c$$$            ptw2 = kn_cmpreal(1,3)**2+kn_cmpreal(2,3)**2
c$$$            mw2 =  kn_cmpreal(0,3)**2-kn_cmpreal(3,3)**2 - ptw2
c$$$         endif
c$$$      else
c$$$         if(nlegborn.eq.5) then
c$$$c We are doing W or Z
c$$$            ptw2 = (kn_cmpborn(1,3)+kn_cmpborn(1,4))**2
c$$$     1           + (kn_cmpborn(2,3)+kn_cmpborn(2,4))**2
c$$$            mw2  = (kn_cmpborn(0,3)+kn_cmpborn(0,4))**2
c$$$     1           - (kn_cmpborn(3,3)+kn_cmpborn(3,4))**2 - ptw2
c$$$         else
c$$$c We are doing the Higgs
c$$$            ptw2 = kn_cmpborn(1,3)**2+kn_cmpborn(2,3)**2
c$$$            mw2 =  kn_cmpborn(0,3)**2-kn_cmpborn(3,3)**2 - ptw2
c$$$         endif
c$$$      endif
      if(flg_minlo_real) then
         ptw2 = (kn_cmpreal(1,3)+kn_cmpreal(1,4)+kn_cmpreal(1,5))**2
     $        + (kn_cmpreal(2,3)+kn_cmpreal(2,4)+kn_cmpreal(2,5))**2
         mw2  = (kn_cmpreal(0,3)+kn_cmpreal(0,4)+kn_cmpreal(0,5))**2
     $        - (kn_cmpreal(3,3)+kn_cmpreal(3,4)+kn_cmpreal(3,5))**2 
     $        - ptw2
      else
         ptw2 = (kn_cmpborn(1,3)+kn_cmpborn(1,4)+kn_cmpborn(1,5))**2
     $        + (kn_cmpborn(2,3)+kn_cmpborn(2,4)+kn_cmpborn(2,5))**2
         mw2  = (kn_cmpborn(0,3)+kn_cmpborn(0,4)+kn_cmpborn(0,5))**2
     $        - (kn_cmpborn(3,3)+kn_cmpborn(3,4)+kn_cmpborn(3,5))**2 
     $        - ptw2
      endif
         

      if(imode.eq.oimode.and.ptw2.eq.optw2.and.mw2.eq.omw2) then
         rescfac=orescfac
         return
      else
         optw2=ptw2
         omw2=mw2
         oimode=imode
      endif
c      if(st_renfact**2*ptw2.lt.frensc2min*st_lambda5MSB**2) then
c         rescfac=0
c         return
c      endif

      as=pwhg_alphas(mw2,st_lambda5MSB,st_nlight)
      y = -as*b0*log(st_renfact**2*ptw2/mw2)
      if(y.ge.1) then
         rescfac = 0d0
         return
      endif
      
      if(ptw2.gt.mw2) then
         rescfac = 1d0
         expsud = 0d0
      else
c Call a Sudakov for a quark flavour
c$$$         if(nlegborn.eq.5) then
c$$$            rescfac = sudakov(ptw2,mw2,ptw2,1)**2
c$$$            expsud = 2 * expsudakov(ptw2,mw2,ptw2,1)
c$$$         else
c$$$            rescfac = sudakov(ptw2,mw2,ptw2,0)**2
c$$$            expsud = 2 * expsudakov(ptw2,mw2,ptw2,0)
c$$$         endif
c     Call a Sudakov for a quark flavour
         rescfac = sudakov(ptw2,mw2,ptw2,1)**2
         expsud = 2 * expsudakov(ptw2,mw2,ptw2,1)
      endif
c as reweighting
      mu2=ptw2*st_renfact**2
      st_mufact2=max(ptw2*st_facfact**2,factsc2min)

      alphas=as/(1-y)-as**2*b1*log(1-y)/(b0*(1-y)**2) !  <=============================
c         if(mu2.gt.4) then
c            tmp=pwhg_alphas(mu2,st_lambda5MSB,st_nlight)
c            if(abs(1-alphas/tmp).gt.0.01) then
c               write(*,*) sqrt(mu2),tmp/alphas
c            endif
c         endif
      
      if(imode.eq.2) then
         rescfac = rescfac * (alphas/st_alpha)**2
      else
         rescfac = rescfac * (alphas/st_alpha)
         if(.not.flg_bornonly) then
            rescfac = rescfac * 
     1           (1+alphas*(expsud+b0*log(mu2/st_muren2)))
         endif
      endif
      orescfac=rescfac
      end



C ------------------------------------------------ C
C - Inputs:                                      - C
C - *******                                      - C
C - q2h  - Upper node scale / bound on Sudakov   - C
C - q2l  - Lower node scale / bound on Sudakov   - C
C - flav - flavour index for the evolving parton - C
C -                                              - C
C - Outputs:                                     - C
C - ********                                     - C
C - sudakov - The Sudakov form factor.           - C
C -                                              - C
C ------------------------------------------------ C
      function sudakov(q20,q2h,q2l,flav)
      implicit none
      real * 8 sudakov,q2h,q2l,q20
      integer flav
      include 'pwhg_st.h'
      include 'pwhg_flg.h'
      include 'pwhg_math.h'
      real * 8 lam2,b0,y,as,pwhg_alphas
      logical isQuark
      real * 8 theExponentN,theExponentD
      real * 8 powheginput
      logical ini,sudscalevar
      data ini/.true./
      save ini,sudscalevar,b0

      if(ini) then
         if(powheginput("#sudscalevar").eq.1) then
            sudscalevar = .true.
         else
            sudscalevar = .false.
         endif
         b0 = (11d0*Ca-2d0*st_nlight)/(12*pi)
         ini = .false.
      endif
      if(sudscalevar) then
         lam2=st_lambda5MSB**2 / st_renfact**2
      else
         lam2=st_lambda5MSB**2
      endif


C     -- running coupling coefficients 
      as=pwhg_alphas(q2h,st_lambda5MSB,st_nlight)
      y = -as*b0*log(q2l*st_renfact**2/q2h)
      if (y .ge. 1) then 
         sudakov = 0d0 
         return 
      endif

      if(sudscalevar) then
         if(q2h.le.q20*st_renfact**2) then
            sudakov=1
            return
         endif
      else
         if(q2l.ge.q2h.or.q2h.le.q20) then
            sudakov=1
            return
         endif
      endif
      if(flav.eq.0) then
         isQuark=.false.
      else
         isQuark=.true.
      endif
      if(q2l.le.q20) then
         if(flg_minlo_nnll) then
            call sudakov_exponent(q20,q2h,q2h,theExponentN,
     $           isQuark,1,.true.)
         else
            call sudakov_exponent(q20,q2h,q2h,theExponentN,
     $           isQuark,2,.true.)
         endif
         sudakov=exp(theExponentN)
      else
         if(flg_minlo_nnll) then
            call sudakov_exponent(q20,q2h,q2h,theExponentN,
     $           isQuark,1,.true.)
            call sudakov_exponent(q20,q2l,q2l,theExponentD,
     $           isQuark,1,.true.)
         else
            call sudakov_exponent(q20,q2h,q2h,theExponentN,
     $           isQuark,2,.true.)
            call sudakov_exponent(q20,q2l,q2l,theExponentD,
     $           isQuark,2,.true.)
         endif
         sudakov=exp(theExponentN-theExponentD)
      endif
      end


C ------------------------------------------------ C
C - Inputs:                                      - C
C - *******                                      - C
C - q2h  - Upper node scale / bound on Sudakov   - C
C - q2l  - Lower node scale / bound on Sudakov   - C
C - flav - flavour index for the evolving parton - C
C -                                              - C
C - Outputs:                                     - C
C - ********                                     - C
C - expsudakov - The Sudakov form factor's expon - C
C -              -ent MODULO a factor of minus   - C
C -              alphaS, integrated with alphaS  - C
C -              fixed. Summed over with the     - C
C -              relevant alphaS factors this is - C
C -              used in compensating the NLO    - C
C -              correction induced when the     - C
C -              Sudakov multiplies the Born.    - C
C -                                              - C
C ------------------------------------------------ C
      function expsudakov(q20,q2h,q2l,flav)
      implicit none
      real * 8 expsudakov,q2h,q2l,q20
      integer flav
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      include 'pwhg_flg.h'
      real * 8 b0,c,b,lam2,logf,m2,a1,b1,tmp
      real * 8 powheginput
      logical ini,sudscalevar
      data ini/.true./
      save ini,sudscalevar

      if(ini) then
         if(powheginput("#sudscalevar").eq.1) then
            sudscalevar = .true.
         else
            sudscalevar = .false.
         endif
         ini = .false.
      endif

c      if(q20.le.lam2.or.q2l.lt.lam2.or.q2h.lt.lam2) then
cc in this case everything is zero, irrelevant
c         expsudakov=0
c         return
c      endif
      b0=(33-2*st_nlight)/12d0
      if(sudscalevar) then
         logf = log(st_renfact)
         m2 = q20*st_renfact**2
      else
         logf = 0
         m2 = q20
      endif
      if(q2h.le.m2.or.flg_bornonly) then
         expsudakov=0
         return
      endif
      if(flav.eq.0) then
         c=3
         b=b0/3  - logf
      else
         c=4d0/3
         b=3d0/4 - logf
      endif
      expsudakov=
     1     c/pi*(0.25d0*log(q2h/m2)**2 - b*log(q2h/m2))
c alterantively
c$$$      b0=(33-2*st_nlight)/(12d0*pi)
c$$$      if(flav.eq.0) then
c$$$         a1=3
c$$$         b1=-2*pi*b0
c$$$      else
c$$$         a1=4d0/3
c$$$         b1=-2
c$$$      endif
c$$$      a1=a1/(2*pi)
c$$$      b1=b1/(2*pi)
c$$$      b1=b1+2*a1*logf
c$$$      tmp=a1*0.5d0*log(q2h/m2)**2+b1*log(q2h/m2)
c$$$      if(abs(tmp/expsudakov-1).gt.1.d-5) then
c$$$         write(*,*) ' expsudakov:',tmp,expsudakov
c$$$      endif
      end

      subroutine sudakov_exponent(q2l,q2h,m2,theExponent,isQuark,
     $                            theAccuracy,fixed_nf)
      implicit none
      real *8 q2l,q2h,m2,theExponent
      logical isQuark,fixed_nf
      integer theAccuracy
      call sudakov_exponent_alt(q2l,q2h,theExponent,isQuark,
     $                            theAccuracy)
      end
      
   




C ********* DDT / Ellis-Veseli / Nason-Ridolfi Sudakov ************ C
C -                                                               - C
C - Output:                                                       - C
C - ========                                                      - C
C - The value of the Sudakov exponent defined as the integral,    - C
C - from  Log [ ql^2/Lambda^2 ]  up to Log [  qh^2/Lambda^2 ], of - C
C -                                                               - C
C -    d Log[ q^2/Lambda^2 ]                                      - C
C -  - {                                                          - C
C -      aSBar*A1*Log[m^2/q^2] + aSBar^2*A2*Log[m^2/q^2]          - C
C -    + aSBar*B1              + aSBar^2*B2                       - C
C -    }                                                          - C
C -                                                               - C
C - where aSBar = aS/2/Pi.                                        - C
C -                                                               - C
C - For m2=qh2, except for an overall factor of two this is the   - C
C - Sudakov form factor of eq. 32 in the Ellis-Veseli paper - in  - C
C - that paper they have two quark lines to consider while here   - C
C - we only want to consider one line at a time. The factor of    - C
C - two is manifest in the code below as our A1, A2, B1, B2       - C
C - coefficients defined to be HALF of the Ellis-Veseli ones.     - C
C -                                                               - C
C - The m2 dependence is a relic of the Nason & Ridolfi form of   - C
C - the Sudakov form factor, which has the numerator in the large - C
C - log equal to mZZ but the upper bound on the Sudakov integral  - C
C - is Q^2. There doesn't seem to be any problem arising when     - C
C - you just call the routine with m^2=Q^2, but maybe if this     - C
C - gets resolved I can re-do the mathematica integral.           - C
C -                                                               - C
C - The analytic integral was done in Mathematica assuming no     - C
C - flavour thresholds. When q^2 is below the b or c quark        - C 
C - flavour thresholds a numerical integration is done instead    - C
C - using dgauss. The numerical integration and analytic results  - C
C - agree very well above these thresholds - try resetting        - C
C - debuggingEpsilon below.                                       - C
C -                                                               - C 
C - For the default values of A1, A2, B1, B2 in the code below    - C
C - the Sudakov should correspond to that of Nason and Ridolfi,   - C
C - which has an effective B2 term by virtue of the fact that     - C
C - the CMW alpha_S is used to multiply the leading & subleading  - C
C - term. At least with the calculation done in the way it is we  - C
C - can easily play around with the coefficients.                 - C 
C -                                                               - C 
C - To Use:                                                       - C
C - =======                                                       - C
C - q2l = The scale of the lower  clustered node.                 - C
C - q2h = The scale of the higher clustered node.                 - C
C - m2  = The boson mass squared (argument of the                 - C
C -       log in the exponent of N.R. eq 4.8).                    - C
C - theExponent                                                   - C 
C -     = The value of the curly brackets in N.R. eq 4.8.         - C
C - isQuark = .true. for a quark propagator                       - C 
C - theAccuracy = 0 for 1-loop alphaS and A2=B2=0,                - C 
C -             = 1 for 2-loop alphaS and Powheg A & B coeffs     - C 
C -             = 2 for 2-loop alphaS and NLL A & B coeffs        - C 
C -                                                               - C 
C - Notes:                                                        - C
C - ======                                                        - C
C - Details for the integration in the sudakov exponent can be    - C
C - found in the Mathematica notebook: menlops/DDT_exponent.nb .  - C 
C -                                                               - C 
C - The Mathematica notebook shows plots in which the 5-flavour   - C
C - and 4-flavour alphaS differ by <2% at pT=3 GeV and 4% at      - C
C - pT=2 GeV. Using the C.M.W. alphaS (aS -> aS*(1+aS*K/2*pi))    - C
C - increases these differences but they remain small: 2% at pT=4 - C
C - GeV and 6% at pT=2 GeV. Note well that since the program      - C
C - matches alphaS at flavour thresholds, not                     - C
C - alphaS*(1+alphaS*K/2*pi), since K too actually depends on     - C
C - the number of flavours, this means the 3,4 and 5 flavour      - C
C - C.M.W. alphaS*(1+alphaS*K/2*pi) DO NOT match at the flavour   - C
C - thresholds in pT! Whereas alphaS 4 and 5 flavour couplings    - C
C - match at 5 GeV, the nf dependence of K means that the 4 and 5 - C
C - flavour alphaS*(1+alphaS*K/2*pi) actually meet at about 9 GeV - C
C - instead.                                                      - C
C -                                                               - C
C ***************************************************************** C
      subroutine sudakov_exponent_alt(q2l,q2h,theExponent,
     $     isquark,theAccuracy)
      implicit none 
      include 'pwhg_st.h'
      logical isQuark 
      integer theAccuracy
      real * 8 q20,q2h,q2l,theExponent
      real * 8 b0,be1,be2
      real * 8 A1,A2,A3
      real * 8 B1,B2
      real * 8 y, as
      real * 8 ff0,ff1,ff2
      real * 8 Ca,Cf,nf,K,logf,q2l_lcl  
      real * 8 EulerGamma,Pi,zeta3  
      real * 8 f0,f1,f2 
      real * 8 pwhg_alphas,powheginput
      external pwhg_alphas,powheginput
      logical ini,sudscalevar
      data ini/.true./
      save ini,sudscalevar

      if(ini) then
         if(powheginput("#sudscalevar").eq.1) then
            sudscalevar = .true.
         else
            sudscalevar = .false.
         endif
         ini = .false.
      endif

      if(sudscalevar) then
         logf = log(st_renfact)
         q2l_lcl = q2l*st_renfact**2
      else
         logf = 0d0
         q2l_lcl = q2l 
      endif


C     -- some constants (should be somewhere else prob...) 
      Ca = 3d0 
      Cf = 4d0/3d0 
      nf = st_nlight
      zeta3 = 1.2020569031595942854d0 
      EulerGamma = 0.57721566490153286061d0 
      Pi = 3.141592653589793238d0 
      K = (67d0/18d0-Pi**2/6d0)*CA-5d0/9d0*nf
         
C     -- running coupling coefficients 
      b0 = (11d0*Ca-2d0*nf)/(12*pi)
      be1 = (153d0 - 19d0*nf) / Pi / 2d0 / (33d0 - 2*nf) 
      be2 = 0d0                 ! not needed 
      

      as=pwhg_alphas(q2h,st_lambda5MSB,st_nlight)
      y = -as*b0*log(q2l_lcl/q2h)
      if (y .ge. 1) then 
         write(*,*) '-------> y',y
         theExponent = -100d0 
         return 
      endif

C     -- Sudakov exponent coefficients 
      if (isQuark) then 
         A1 = Cf  
         A2 = Cf*K
         A3 = 0d0 ! not needed 
         B1 = -3d0/2*Cf 
         B2    = (     Cf**2 * (Pi**2-3d0/4-12*zeta3)
     $               + Cf*CA * (11*Pi**2/9-193d0/12+6*zeta3)
     $           + Cf*nf*0.5 * (17d0/3-4d0/9*Pi**2)
     $           ) / 2d0

      else
         A1 =  CA
         A2 =  CA*K 
         A3 = 0d0 ! not needed 
         B1 = -2*Pi*b0 
         B2    = (       CA*CA * (23d0/6+22*Pi*Pi/9-6*zeta3)
     $               + 4*Cf*nf*0.5d0
     $               - CA*nf*0.5*(2d0/3+8d0*Pi*Pi/9)
     $               - 11d0/2*CA*Cf
     $           ) / 2d0

      endif

      B2     = B2 + 4*A1**2*zeta3 ! <--- qT space conversion

C     -- add logf dependence 
      B2 = B2 + 2d0*A2*logf +  4d0*pi*b0*A1*logf**2 
      B2 = B2 + (st_bornorder-1)*2*b0**2*logf
      A2 = A2 + 4d0*pi*A1*b0*logf 
      B1 = B1 + 2d0*A1*logf 

C     -- switch off some coefficients according to accuracy wanted 
      if(theAccuracy.eq.0) then
         A2  = 0d0 ! NLL coefficient
         B2  = 0d0 ! NNLL coefficient
      elseif(theAccuracy.eq.2) then
         B2  = 0d0 ! NNLL coefficient
      elseif(theAccuracy.eq.3) then
         B1  = 0d0 ! NLL coefficient
         A2  = 0d0 ! NLL coefficient
         B2  = 0d0 ! NNLL coefficient
      endif
      A1 = A1/(2d0*Pi)  
      B1 = B1/(2d0*Pi)  
      A2 = A2/(2d0*Pi)**2  
      B2 = B2/(2d0*Pi)**2

      ff0 = f0(y,A1,b0)
      ff1 = f1(y,A1,A2,B1,b0,be1) 
      ff2 = f2(y,A1,A2,A3,B1,B2,b0,be1,be2) 
!      ff2 = f2(y,0d0,0d0,0d0,0d0,B2,b0,be1,be2) ! keep only B2 term ...? 

      theExponent = 1d0/as*ff0+ff1+as*ff2 

      end

      function f0(y,A1,b0) 
      implicit none 
      real * 8 y,A1,b0,f0
      f0 = A1/b0**2*(y+Log(1d0-y))
      end


      function f1(y,A1,A2,B1,b0,be1) 
      implicit none 
      real * 8 y,A1,A2,B1,b0,be1,f1
      real * 8 omy,lomy
      omy = 1d0-y 
      lomy=log(omy)
      f1=A1*be1/b0**2*(0.5d0*lomy**2+y/(omy)+lomy/(omy))
      f1=f1-A2/b0**2*(lomy+y/(omy))+B1/b0*lomy
      end


      function f2(y,A1,A2,A3,B1,B2,b0,be1,be2) 
      implicit none 
      real * 8 y,A1,A2,A3,B1,B2,b0,be1,be2,f2
      real * 8 omy,bbe1,lomy
      omy = 1d0-y 
      lomy=log(omy)
      bbe1 = be1*b0
      f2 = -B2/b0*y/omy+B1*bbe1/b0**2*(y+lomy)/omy
     .     -A3/2d0*(y/omy/b0)**2
     .     +A2*bbe1/2d0/b0**3*(y*(3d0*y-2d0)+(4d0*y-2d0)*lomy)
     .     /omy**2
     .     + A1/2d0/b0**4/omy**2*(
     .     bbe1**2*(1d0-2d0*y)*lomy**2
     .     +2d0*(b0*be2*omy**2+bbe1**2*y*omy)*lomy
     .     -3d0*b0*be2*y**2+(bbe1*y)**2+2d0*b0*be2*y
     .     )
      end


      subroutine findNearestNeighbours
      write(*,*) ' Should not be here'
      call exit(-1)
      end
