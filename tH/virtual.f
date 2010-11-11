c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point
c     The factor  (4*Pi)^ep/Gamma(1-ep) IS NOT RETURNED by this subroutine
c     and it's thought as factorized in front of the real counterterms too.

      subroutine setvirtual(p,vflav,virtual)
      implicit none
      include 'nlegborn.h'
      include 'loopints.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'   
      include 'PhysPars.h'
      integer nleg
      parameter (nleg=nlegborn)
      real * 8 p(0:3,nleg), pphy(0:3,nleg)
      integer vflav(nleg)
      real * 8 virtual
      real * 8 ampborn2,bmunu(0:3,0:3)
      external dotp
      integer mu
      real * 8 Mtop, Mch, s, t, muR, muF, A, B, GF, alphas
      real * 8 DOne, DTwo, DThree, C1, C2, C3, C4, C5, C6, C7, C8, B1, B2, B3, B4, B5, Rescht
      real * 8 Log1, Log2, Log3
      real * 8 C4term, C7term, C8term
      real * 8 C1M1, scaswitch, scalogs, muFlog
      integer i
      real * 8 mh,mt
      complex * 16 cspen
      external cspen
c  no muR dependence implemented yet!
c  include the loopfun.F file!!!!
      mh=ph_mCH
      mt=ph_mT
      
      Mtop = ph_mT
      Mch = ph_mCH
      A = ph_A
      B = ph_B
      GF = ph_GF
      alphas = st_alpha

	
	
c initialize the scales here
      musc=sqrt(st_muren2)
      muF = sqrt(st_mufact2)

c  make sure that p1 is the b quark      
      if (vflav(1).eq.5) then
      	do i=1,nleg
		do mu=0,3
		pphy(mu,i)= p(mu,i)
		enddo
	enddo
      
      elseif (vflav(2).eq.5) then
	do mu=0,3
	pphy(mu,1) = p(mu,2)
	pphy(mu,2) = p(mu,1)
	pphy(mu,3) = p(mu,3)
	pphy(mu,4) = p(mu,4)
	enddo
      endif
c  compute the Mandelstams

          
      s = (pphy(0,1)+pphy(0,2))**2  - ( (pphy(1,1)+pphy(1,2))**2 + (pphy(2,1)+pphy(2,2))**2 + (pphy(3,1)+pphy(3,2))**2 )
      
      t = (pphy(0,2)-pphy(0,4))**2  - ( (pphy(1,2)-pphy(1,4))**2 + (pphy(2,2)-pphy(2,4))**2 + (pphy(3,2)-pphy(3,4))**2 ) 
     
      call myCget(s, mh**2, mt**2, 0d0, 0d0, mt**2)
      C4term = dReal(xc0)
	
      call myCget(t,0d0, mt**2, 0d0, mt**2, mt**2)
      C7term = dReal(xc0)
	
      call myCget(mh**2,0d0, mh**2 + mt**2 - s - t, 0d0, mt**2, mt**2)
      C8term = dReal(xc0)
c the rest of the D scalar integrals     

      DOne =    (alphas**2*(A**2 + B**2)*NC*(-1 + NC**2)*(mt**6 + 2*mh**4*(mt**2 - t) + mt**2*(s + t)**2 - t*(s + t)**2 - mt**4*(4*s + t) + 
     -      2*mh**2*(-mt**4 + mt**2*s + t*(s + t)))*((-5*Pi**2)/12. - CLog(Cmplx((-mh**2 + mt**2)/mt**2))**2 + 
     -      2*CLog(Cmplx(-s/mt**2))*CLog(Cmplx((mt**2 - t)/mt**2)) - 2*dReal(CSpen(DCmplx((-mh**2 + t)/(-mt**2 + t))))))/
     -  (2.*s*(mt**2 - t)*(-mt**2 + t))
     
    	DTwo  = -(alphas**2*(A**2 + B**2)*(-1 + NC**2)*(-mh**2 + s + t)*(2*mh**4 + mt**4 - mt**2*s + (s + t)**2 - 2*mh**2*(mt**2 + s + t))*
     -     ((-5*Pi**2)/12. - CLog(Cmplx((-mh**2 + mt**2)/mt**2))**2 + 2*CLog(Cmplx(-s/mt**2))*CLog(Cmplx(-(mh**2 - s - t)/mt**2)) - 
     -       2*dReal(CSpen(DCmplx((mt**2 - s - t)/(mh**2 - s - t))))))/(2.*NC*s*(mt**2 - t)*(mh**2 - s - t))
	
	DThree =(alphas**2*(A**2 + B**2)*(-1 + NC**2)*(-mh**2 + s + t)*
     -    (2*mt**6 + 2*mh**4*(mt**2 - t) - t*(s + t)**2 - mt**4*(4*s + 3*t) + mt**2*(s**2 + 2*s*t + 2*t**2) + 
     -      2*mh**2*(-mt**4 + mt**2*s + t*(s + t)))*(-Pi**2/12. - CLog(Cmplx((-mh**2 + mt**2)/mt**2))**2 + 
     -      2*CLog(Cmplx((mt**2 - t)/mt**2))*CLog(Cmplx((-mh**2 + s + t)/mt**2)) - 2*dReal(CSpen(DCmplx(1 - (-mh**2 + mt**2)/(mt**2 - t)))) - 
     -      2*dReal(CSpen(DCmplx(1 - (-mh**2 + mt**2)/(-mh**2 + s + t))))))/(2.*NC*s*(mt**2 - t)*(mh**2 - s - t)*(-mt**2 + t))
     
     	C8 = (alphas**2*(A**2 + B**2)*C8term*(-1 + NC**2)*(-mt**2 + s + t)*
     -    (2*mh**4 + 2*mt**4 + (s + t)**2 - 2*mh**2*(mt**2 + s + t) - mt**2*(2*s + t)))/(2.*NC*s*(mt**2 - t))
	
	
	C1 =  -(alphas**2*(A**2 + B**2)*(-1 + NC**2)*(2*mh**4*(1 + NC**2) + mt**4*(1 + NC**2) - mt**2*s + (1 + NC**2)*(s + t)**2 - 
     -       2*mh**2*(1 + NC**2)*(mt**2 + s + t))*CLog(Cmplx(-s/mt**2))**2)/(4.*NC*s*(mt**2 - t))
	
	C4 = -(alphas**2*(A**2 + B**2)*C4term*(-1 + NC**2)*(2*mh**10*(-1 + NC**2) - 
     -       (mt**2 - s)**3*(mt**4*(-1 + NC**2) + mt**2*(1 - 2*NC**2)*s - (-1 + NC**2)*s**2) - 
     -       2*(mt**2 - s)*s*(mt**4 - mt**2*NC**2*s - (-1 + NC**2)*s**2)*t - (-1 + NC**2)*(mt**2 + s)*(mt**4 + s**2)*t**2 - 
     -       2*mh**8*(-1 + NC**2)*(4*(mt**2 + s) + t) + mh**6*
     -        (13*mt**4*(-1 + NC**2) + mt**2*((-11 + 12*NC**2)*s + 6*(-1 + NC**2)*t) + (-1 + NC**2)*(13*s**2 + 8*s*t + t**2)) + 
     -       mh**2*(5*mt**8*(-1 + NC**2) + (-1 + NC**2)*s**2*(s + t)*(5*s + 3*t) + mt**6*((11 - 12*NC**2)*s + 2*(-1 + NC**2)*t) + 
     -          mt**4*(-2*s**2 + 4*NC**2*s*t + 3*(-1 + NC**2)*t**2) + mt**2*s*((5 - 2*NC**2)*s**2 + 2*(-1 + 3*NC**2)*s*t + 4*(-1 + NC**2)*t**2)) + 
     -       mh**4*(-11*mt**6*(-1 + NC**2) + mt**4*((-4 + 3*NC**2)*s - 6*(-1 + NC**2)*t) - (-1 + NC**2)*s*(11*s**2 + 12*s*t + 3*t**2) + 
     -          mt**2*(t*(10*s + 3*t) - 3*NC**2*(s**2 + 4*s*t + t**2)))))/(2.*NC*s*(mh**4 + (mt**2 - s)**2 - 2*mh**2*(mt**2 + s))*(mt**2 - t))


	C7 = (alphas**2*(A**2 + B**2)*C7term*(-1 + NC**2)*(2*mt**8 + 2*mh**4*(mt**2 - t)**2 + t**2*(s + t)**2 + mt**4*(s + t)*(3*s + 5*t) - 
     -      mt**6*(4*s + 5*t) - mt**2*t*(2*s**2 + 6*s*t + 3*t**2) - 2*mh**2*(mt**2 - t)*(mt**4 - t*(s + t))))/(2.*NC*s*(mt**2 - t)**2)


	C6 =  -(alphas**2*(A**2 + B**2)*(-1 + NC**2)*(mh**2 - t)*(mt**6*(-2 + NC**2) + 2*mh**4*(-1 + NC**2)*(mt**2 - t) - 
     -       2*mh**2*(-1 + NC**2)*(mt**2 - s - t)*(mt**2 + t) - (-1 + NC**2)*t*(s + t)**2 + mt**4*(-4*(-1 + NC**2)*s - (-3 + NC**2)*t) + 
     -       mt**2*((-1 + NC**2)*s**2 + 2*(-1 + NC**2)*s*t + (-2 + NC**2)*t**2))*
     -     (-CLog(Cmplx((-mh**2 + mt**2)/mt**2))**2 + CLog(Cmplx((mt**2 - t)/mt**2))**2 - dReal(CSpen(DCmplx(mh**2/mt**2))) + 
     -       dReal(CSpen(DCmplx(t/mt**2)))))/(2.*NC*s*(mt**2 - t)**2*(-mh**2 + t))

	C2 = (alphas**2*(A**2 + B**2)*mt**2*(-1 + NC**2)*(mt**2 - s - t)*(-mh**2 + s + t)*
     -    (Pi**2/12. + CLog(Cmplx(mt**2/(-mh**2 + s + t)))**2/2. - dReal(CSpen(DCmplx((-mh**2 - mt**2 + s + t)/(-mh**2 +s+ t))))**2))/
     -  (2.*NC*s*(mt**2 - t)*(mh**2 - s - t))
     
     
     	C3 =  (alphas**2*(A**2 + B**2)*NC*(-1 + NC**2)*(2*mh**4 + mt**4 + (s + t)**2 - 2*mh**2*(mt**2 + s + t))*
     -    (Pi**2/12. + CLog(Cmplx(mt**2/(mt**2 - t)))**2/2. - dReal(CSpen(DCmplx(-(t/(mt**2 - t)))))**2))/(2.*s*(-mt**2 + t))


	C5 = (alphas**2*(A**2 + B**2)*(-1 + NC**2)*(-mt**2 + s + t)*(2*mh**4 + mt**4 - mt**2*s + (s + t)**2 - 2*mh**2*(mt**2 + s + t))*
     -    (-CLog(Cmplx((-mh**2 + mt**2)/mt**2))**2 + CLog(Cmplx((-mh**2 + s + t)/mt**2))**2 - dReal(CSpen(DCmplx(mh**2/mt**2))) + 
     -      dReal(CSpen(DCmplx((mh**2 + mt**2 - s - t)/mt**2)))))/(2.*NC*s*(mt**2 - t)*(mt**2 - s - t))
	
	
	B1 =  (2*alphas**2*(A**2 + B**2)*mt**2*(-1 + NC**2)**2*(mh**2*(mt**2 - t) + t*(-mt**2 + s + t))*(2 - CLog(Cmplx(s/mt**2))))/
     -  (NC*(mh**4 + (mt**2 - s)**2 - 2*mh**2*(mt**2 + s))*(mt**2 - t))
     
     	B2 =   -(alphas**2*(A**2 + B**2)*mt**2*(-1 + NC**2)*(mh**4*(mt**2 - t)*(mt**2*(-1 + NC**2) - (-3 + NC**2)*t) + 
     -       mh**2*(2*mt**2 - s - 2*t)*t*(mt**2*(-1 + NC**2) - (1 + NC**2)*t) - (mt**2 - s - t)*t**2*(mt**2*(-1 + NC**2) - (1 + NC**2)*t))*
     -     (2 + ((mt**2 - t)*CLog(Cmplx((mt**2 - t)/mt**2)))/t))/(2.*NC*(mh**2 - t)*(mt**2 - t)**3*t)
     
     
        B3 = 0d0
     
     	B4 =  (-2*alphas**2*(A**2 + B**2)*(-1 + NC**2)*(-(mt**10*(-1 + NC**2)*(s - 2*t)) + 2*mh**8*(-1 + NC**2)*(mt**2 - t)*t - 
     -      (-1 + NC**2)*s**2*t**2*(s + t)**2 + mt**8*(3*(-1 + NC**2)*s**2 + (10 - 9*NC**2)*s*t - 2*(-1 + NC**2)*t**2) + 
     -      mt**6*s*(-3*(-1 + NC**2)*s**2 + (-13 + 10*NC**2)*s*t + (-14 + 13*NC**2)*t**2) + 
     -      mt**4*s*((-1 + NC**2)*s**3 - 3*(-2 + NC**2)*s**2*t + 2*(7 - 6*NC**2)*s*t**2 - 10*(-1 + NC**2)*t**3) - 
     -      mt**2*s*t*(s**3 + (5 - 4*NC**2)*s**2*t - 8*(-1 + NC**2)*s*t**2 - 3*(-1 + NC**2)*t**3) + 
     -      mh**6*(-2*mt**6*(-1 + NC**2) + 2*(-1 + NC**2)*t**2*(3*s + t) + mt**4*((-2 + NC**2)*s - 2*(-1 + NC**2)*t) + 
     -         mt**2*t*((4 - 3*NC**2)*s + 2*(-1 + NC**2)*t)) + 
     -      mh**4*(3*mt**8*(-1 + NC**2) + mt**6*((1 + NC**2)*s + 4*(-1 + NC**2)*t) + mt**2*s*t*(-3*s + (-7 + 6*NC**2)*t) - 
     -         (-1 + NC**2)*t**2*(7*s**2 + 6*s*t + t**2) + mt**4*((3 - NC**2)*s**2 + (12 - 13*NC**2)*s*t - 6*(-1 + NC**2)*t**2)) + 
     -      mh**2*(-(mt**10*(-1 + NC**2)) + 2*(-1 + NC**2)*s*t**2*(s + t)*(2*s + t) + mt**8*((-9 + 8*NC**2)*s - 6*(-1 + NC**2)*t) + 
     -         mt**6*((6 - 4*NC**2)*s**2 + (6 - 7*NC**2)*s*t + 8*(-1 + NC**2)*t**2) + 
     -         mt**2*t*((2 + NC**2)*s**3 + 2*(5 - 4*NC**2)*s**2*t - 8*(-1 + NC**2)*s*t**2 + (-1 + NC**2)*t**3) - 
     -         mt**4*((8*s - t)*t*(s + 2*t) + NC**2*(s**3 - 6*s**2*t - 17*s*t**2 + 2*t**3)))))/
     -  (NC*s*(mh**4 + (mt**2 - s)**2 - 2*mh**2*(mt**2 + s))*(mt**2 - t)**3)
     
     	B5 =  (alphas**2*(A**2 + B**2)*mt**2*(-1 + NC**2)**2*(mh**2*(mt**2 - t) + t*(-mt**2 + s + t))*
     -    (-2*mh**6 + 2*mh**4*(2*(mt**2 + s) + t) - mh**2*(3*mt**4 + 2*s**2 - mt**2*(s - 2*t) + s*t + t**2) + 
     -      (mt**2 - s)*(mt**4 - mt**2*s + t*(-s + t)))*(2 + ((-mh**2 + mt**2)*CLog(Cmplx((-mh**2 + mt**2)/mt**2)))/mh**2))/
     -  (NC*s*(mh**4 + (mt**2 - s)**2 - 2*mh**2*(mt**2 + s))*(mh**2 - t)*(mt**2 - t)**2)
     
     
     	Log1 =  (alphas**2*(A**2 + B**2)*(-1 + NC**2)*(-mt**2 + s + t)**2*
     -    CLog(Cmplx((-mh**2 + s + t)/mt**2)))/(NC*s*(mt**2 - t))
     
	Log2 = -((alphas**2*(A**2 + B**2)*NC*(-1 + NC**2)*(-mt**2 + s + t)**2*
     -      CLog(Cmplx((mt**2 - t)/mt**2)))/(s*(mt**2 - t)))

    
	Log3 = -((alphas**2*(A**2 + B**2)*NC*(-1 + NC**2)*(-mt**2 + s + t)**2*
     -		CLog(Cmplx(-s/mt**2)))/(s*(mt**2 - t)))

		
	Rescht =  (alphas**2*(A**2 + B**2)*(-1 + NC**2)*
     -    (48*mh**4*(-1 + NC**2)*(mt**2 - t)*t**2 + 
     -      6*mh**2*((-7 + 9*NC**2)*t**3*(s + t) + mt**6*((-1 + NC**2)*s - (1 + NC**2)*t) - 
     -         mt**2*t**2*((7 - 3*NC**2)*s + 3*(1 + NC**2)*t) - mt**4*t*(s - 3*NC**2*s + (-11 + 5*NC**2)*t)) + 
     -      t*(mt**8*(15 + NC*(-20 + 19*NC)) + (9 + NC*(-20 + 13*NC))*t**2*(s + t)**2 + 
     -         2*mt**2*t*((3 + (20 - 31*NC)*NC)*s**2 + 6*(3 + 2*(5 - 8*NC)*NC)*s*t + 
     -            (3 + (40 - 53*NC)*NC)*t**2) + 
     -         mt**4*((-15 + NC*(-20 + 49*NC))*s**2 + 6*(7 + NC*(-20 + 23*NC))*s*t + 
     -            24*(1 + NC*(-5 + 6*NC))*t**2) - 2*mt**6*(27*t + NC*((-20 + 34*NC)*s + 5*(-8 + 7*NC)*t)))))/
     -  (12.*NC*s*(mt**2 - t)**3*t)
	    
** remove the single pole coef which was multiplied by the eps part of born, because POWHEG has (C2/eps^2+C1/eps)born(d)
**form     
      	C1M1 = (alphas**2*(A**2 + B**2)*CF*(-mt**2 + s + t)**2*
     -    (15. + 20.*NC - 37*NC**2 - 12*CLog(Cmplx((-mh**2 + s + t)/mt**2)) + 
     -      12*NC**2*(CLog(Cmplx(s/mt**2)) + CLog(Cmplx(1 - t/mt**2)))))/(6.*s*(mt**2 - t))
	

      scaswitch =  (2*alphas**2*(A**2 + B**2)*CF*NC*
     -    (2*mh**4*(mt**2 - t) + 2*mh**2*(-mt**4 + t*(s + t)) + 
     -      (mt**2 - s - t)*(mt**4 - mt**2*s + t*(s + t)))*
     -    (((1 - 3*NC**2)*CLog(Cmplx(musc**2/mt**2))**2)/(4.*NC) + 
     -      (CLog(Cmplx(musc**2/mt**2))*(15 + 20.*NC - 37*NC**2 - 
     -           12*CLog(Cmplx((-mh**2 + s + t)/mt**2)) + 
     -           12*NC**2*(CLog(Cmplx(s/mt**2)) + CLog(Cmplx(1 - t/mt**2)))))/(12.*NC)))/
     -  (s*(mt**2 - t)**2)
      

c	no log for the yuk
c      scalogs =  (alphas**2*(A**2 + B**2)*(-1 + NC**2)*(2 + 11*NC - 2*6.d0)*
c     -    (2*mh**4*(mt**2 - t) + 2*mh**2*(-mt**4 + t*(s + t)) + 
c     -      (mt**2 - s - t)*(mt**4 - mt**2*s + t*(s + t)))*CLog(Cmplx(musc**2/mt**2)))/
c     -  (6.*s*(mt**2 - t)**2)
     
c	yuk for running masses     
      scalogs =    (alphas**2*(A**2 + B**2)*(-1 + NC**2)*(-9 + 2*NC*(-5 + 10*NC))*
     -    (2*mh**4*(mt**2 - t) - (mt**2 - s - t)*(-mt**4 + mt**2*s - t*(s + t)) + 2*mh**2*(-mt**4 + t*(s + t)))*
     -    CLog(Cmplx(muSc**2/mt**2)))/(6.*NC*s*(mt**2 - t)**2)
     
c    explicit muF dependence from the strong coupling renormalization
      muFlog = -(alphas**2*(A**2 + B**2)*(-12 + 11*NC)*(-1 + NC**2)*
     -     (2*mh**4*(mt**2 - t) + 2*mh**2*(-mt**4 + t*(s + t)) + (mt**2 - s - t)*(mt**4 - mt**2*s + t*(s + t)))*
     -     CLog(Cmplx(muF**2/muSc**2)))/(6.*s*(mt**2 - t)**2)
         
      
      virtual = (DOne + DTwo + DThree + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 
     #+ B1 + B2 + B3 + B4 + B5 + Rescht + Log1 + Log2 + Log3 + C1M1 + scaswitch + scalogs + muFlog)/(4*NC*(NC**2-1d0))

      virtual = 4*sqrt(2d0)*GF*virtual / (st_alpha/(2*pi))

      end


