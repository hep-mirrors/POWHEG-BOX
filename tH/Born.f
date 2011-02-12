c      include'myalphas.F'

      subroutine setborn(p,bflav,born,bornjk,bmunu)
      implicit none
      include '../include/pwhg_math.h'
      include 'nlegborn.h'      
      include '../include/pwhg_flst.h'
      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 p(0:3,nlegs),bornjk(nlegs,nlegs)
      integer bflav(nlegs)
      real * 8 bmunu(0:3,0:3,nlegs),bbmunu(0:3,0:3),born,colcf
      integer j,k,mu,nu

c Colour factors for colour-correlated Born amplitudes;
      call compborn(p,bflav,born,bbmunu)
      
      do j=1,nlegs
c         if(abs(bflav(j)).le.6) then
c this shortens the loop, since only over colored particles	 
            if(bflav(j).eq.0) then
c	    	write(*,*)'bmunu filled for j = ', j
               do mu=0,3
                  do nu=0,3
                     bmunu(mu,nu,j)=bbmunu(mu,nu)
c	this should only happen twice for my case! j=2 and j=1		     
                  enddo
               enddo
	      else 
	       do mu=0,3
                  do nu=0,3
                     bmunu(mu,nu,j)=0		     
                  enddo
               enddo 
            endif  
	    
	   if (bflav(1).eq.5) then                
            do k=j+1,nlegs
               if (((j.eq.1).and.(k.eq.2)).or.
     #                 ((j.eq.2).and.(k.eq.4))) then   
                  colcf = CA/2d0            
               else if ((j.eq.1).and.(k.eq.4)) then
	       	  colcf = CF - CA/2d0	 
	       else
                  colcf = 0
               endif
               bornjk(j,k)=born*colcf
               bornjk(k,j)=bornjk(j,k)
            enddo
	   else
	    do k=j+1,nlegs
               if (((j.eq.1).and.(k.eq.2)).or.
     #                 ((j.eq.1).and.(k.eq.4))) then   
                  colcf = CA/2d0            
               else if ((j.eq.2).and.(k.eq.4)) then
	       	  colcf = CF - CA/2d0	 
	       else
                  colcf = 0
               endif
               bornjk(j,k)=born*colcf
               bornjk(k,j)=bornjk(j,k)
            enddo
	    	    
	   endif
	    
c         endif
      enddo
      end


c     b g -> H^{-} t
      subroutine compborn(p,bflav,born,bmunu)
      implicit none
      include '../include/pwhg_math.h'
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_st.h'
      include 'PhysPars.h' 
      integer nleg
      parameter (nleg=nlegborn)
      real * 8 p(0:3,nleg)
      integer bflav(nleg)
      real * 8 pin(0:3,nleg)
      real * 8 amp2,born,bmunu(0:3,0:3)
      real * 8 gmunu(0:3,0:3)
      integer i,j,k,mu,nu, inlo 
      real * 8 s,t, Mch, Mtop , A, B, GF, alphas 
      real * 8 lam_qcd, lam_dummy, acc, mcq, mbq, mtq, qr
      integer nq, inicoup
      real * 8 RUNM_EXT
      external ALSINI_RUNM


      Mch = ph_mCH
      Mtop = ph_mT

      GF = ph_GF
      alphas = st_alpha
     
      if(inicoup.eq.0) then
      
      qr = Sqrt(st_muren2)
      
      lam_qcd = 0.226D0  ! for LO: 0.165 cteq6, 0.146 cteq 5
      
      
      acc = 1.e-8
      mcq = 1.5              
c      mbq = 4.d0                  
c     mtq = 172.0  
c changed for testing      
      mbq = ph_mb                  
      mtq = ph_mT                                                ! top quark decoupled from alphas

                                         ! top quark with finite mass for running Yukawas
      nq  = 6
      call ALSINI_RUNM(acc,lam_qcd,mcq,mbq,mtq,nq)

      write(*,*)'ren scale: ', qr 	
      write(*,*)'mt(qr): ',  RUNM_EXT(qr,6,2), ' mb(qr): ', RUNM_EXT(qr,5,2)


      ph_A = RUNM_EXT(qr,6,2)/ph_tanb
      ph_B = RUNM_EXT(qr,5,2)*ph_tanb
            
      inicoup = 1
      
      endif
      
c  use pin so as not to change p by some mistake
      A = ph_A
      B = ph_B

      
      if (bflav(1).eq.5) then	
      	do mu=0,3
         	pin(mu,1) = p(mu,1)
         	pin(mu,2) = p(mu,2)
         	pin(mu,3) = p(mu,3)
         	pin(mu,4) = p(mu,4)
      	enddo
	
      elseif (bflav(1).eq.0)then
      	do mu=0,3
         	pin(mu,1) = p(mu,2)
        	pin(mu,2) = p(mu,1)
         	pin(mu,3) = p(mu,3)
         	pin(mu,4) = p(mu,4)
      	enddo
	      
      endif	
      
      
c     compute the Mandeltam variables, s = (b + g)**2, t = (g - top)**2   
      
      s = (pin(0,1)+pin(0,2))**2  - ( (pin(1,1)+pin(1,2))**2 + (pin(2,1)+pin(2,2))**2 + (pin(3,1)+pin(3,2))**2 )
      
      t = (pin(0,2)-pin(0,4))**2  - ( (pin(1,2)-pin(1,4))**2 + (pin(2,2)-pin(2,4))**2 + (pin(3,2)-pin(3,4))**2 ) 
      
c     compute the simple born  with color sum and average and spin average   
      born =      (2*Sqrt(2d0)*alphas*(A**2 + B**2)*GF*Pi*
     -    (2*Mch**4*(Mtop**2 - t) + 2*Mch**2*(-Mtop**4 + t*(s + t)) + 
     -      (Mtop**2 - s - t)*(Mtop**4 - Mtop**2*s + t*(s + t))))/
     -  (NC*s*(Mtop**2 - t)**2)
      
	
   
c     compute the metric tensor
      do mu = 0,3
      	do nu = 0,3
		if (mu.eq.nu)then
			if (mu.eq.0) then
				gmunu(mu,nu) = 1d0
			else
                           gmunu(mu,nu) = -1d0
			endif
		else
                   gmunu(mu,nu) = 0d0
		endif	
c	test		
c	write(*,*)'g mu nu (', mu, ',', nu, ')= ', gmunu(mu,nu)
	enddo      
      enddo
      
c     compute the spin correlated born     
      do mu=0,3
         do nu=0,3
	 
            bmunu(mu,nu)= -((Sqrt(2d0)*alphas*(A**2 + B**2)*GF*Pi*
     -      (s*(Mtop**2 - t)*(-Mtop**2 + s + t)**2*gmunu(mu,nu) + 
     -        2*(-((Mtop**2 - t)*(Mtop**4 - Mch**2*(Mtop**2 + s - t) - Mtop**2*t + s*(s + t))*pin(mu,1)) - 
     -           s*(Mch**2*(Mtop**2 + s - t) + t*(s + t) - Mtop**2*(2*s + t))*pin(mu,4))*pin(nu,2) + 
     -        4*(Mch**2 - Mtop**2)*((Mtop**2 - t)*pin(mu,1) - s*pin(mu,4))*
     -         ((Mtop**2 - t)*pin(nu,1) - s*pin(nu,4)) + 
     -        2*pin(mu,2)*(-((Mtop**2 - t)*(Mtop**4 - Mch**2*(Mtop**2 + s - t) - Mtop**2*t + s*(s + t))*
     -              pin(nu,1)) - 2*s*(Mtop**2 - t)*(-Mch**2 + s + t)*pin(nu,2) - 
     -           s*(Mch**2*(Mtop**2 + s - t) + t*(s + t) - Mtop**2*(2*s + t))*pin(nu,4))))/
     -    (NC*s**2*(Mtop**2 - t)**2))
	  
	    
         enddo
      enddo
      end

   
      subroutine resonances_lh
c     Set up the resonances whose mass must be preserved
c     on the Les Houches interface.

      end

      subroutine borncolour_lh
      implicit none
c Sets up the colour for the given flavour configuration
c already filled in the Les Houches interface.
c In case there are several colour structure, one
c should pick one with a probability proportional to
c the value of the corresponding cross section, for the
c kinematics defined in the Les Houches interface

C We deal here with the CKM matrix elements for VBF Higgs boson production
      include '../include/LesHouches.h'
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'

c the H-
         icolup(1,3)=0
         icolup(2,3)=0

      if(idup(1).eq.5) then
c the b
         icolup(1,1)=501
         icolup(2,1)=0
c the g
         icolup(1,2)=502
         icolup(2,2)=501
c the t
         icolup(1,4)=502
         icolup(2,4)=0
      else
c the b
         icolup(1,2)=501
         icolup(2,2)=0
c the g
         icolup(1,1)=502
         icolup(2,1)=501
c the t
         icolup(1,4)=502
         icolup(2,4)=0
      endif

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DOUBLE PRECISION FUNCTION XITER_ORIG(Q,XLB1,NF1,XLB,NF2,ACC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      B0(NF)=33.D0-2.D0*NF
      B1(NF)=6.D0*(153.D0-19.D0*NF)/B0(NF)**2
      ALS2(NF,X,XLB)=12.D0*PI/(B0(NF)*LOG(X**2/XLB**2))
     .              *(1.D0-B1(NF)*LOG(LOG(X**2/XLB**2))
     .              /LOG(X**2/XLB**2))
      AA(NF)=12D0*PI/B0(NF)
      BB(NF)=B1(NF)/AA(NF)
      XIT(A,B,X)=A/2.D0*(1D0+SQRT(1D0-4D0*B*LOG(X)))
      PI=4.D0*ATAN(1.D0)
      XLB2=XLB
      II=0
1     II=II+1
      X=LOG(Q**2/XLB2**2)
      ALP=ALS2(NF1,Q,XLB1)
      A=AA(NF2)/ALP
      B=BB(NF2)*ALP
      XX=XIT(A,B,X)
      XLB2=Q*DEXP(-XX/2.D0)
      Y1=ALS2(NF1,Q,XLB1)
      Y2=ALS2(NF2,Q,XLB2)
      DY=ABS(Y2-Y1)/Y1
      IF(DY.GE.ACC) GOTO 1
      XITER_ORIG=XLB2
      RETURN
      END
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine ALSINI_RUNM(acc,xlambda_in,amc_in,amb_in,amt_in,n0_in)

      implicit none

      integer n0_in,i
      real*8  acc,xlambda_in,amc_in,amb_in,amt_in,xlb(6),XITER_ORIG

      real*8             xlb1(6),xlb2(6)
      COMMON/ALSLAM_RUNM/xlb1   ,xlb2

      integer n0
      real*8          xlambda,amc,amb,amt
      COMMON/ALS_RUNM/xlambda,amc,amb,amt,n0
ctp [begin]
c               fill the common input als by arguments
      xlambda = xlambda_in
      amc = amc_in
      amb = amb_in
      amt = amt_in
      n0  = n0_in
ctp [end]      
c               compute the different values of lambda_qcd 
      xlb1(1)=0d0
      xlb1(2)=0d0
      xlb2(1)=0d0
      xlb2(2)=0d0

      if(n0.eq.3)then
         xlb(3)=xlambda
         xlb(4)=xlb(3)*(xlb(3)/amc)**(2.d0/25.d0)
         xlb(5)=xlb(4)*(xlb(4)/amb)**(2.d0/23.d0)
         xlb(6)=xlb(5)*(xlb(5)/amt)**(2.d0/21.d0)
      elseif(n0.eq.4)then
         xlb(4)=xlambda
         xlb(5)=xlb(4)*(xlb(4)/amb)**(2.d0/23.d0)
         xlb(3)=xlb(4)*(xlb(4)/amc)**(-2.d0/27.d0)
         xlb(6)=xlb(5)*(xlb(5)/amt)**(2.d0/21.d0)
      elseif(n0.eq.5)then
         xlb(5)=xlambda
         xlb(4)=xlb(5)*(xlb(5)/amb)**(-2.d0/25.d0)
         xlb(3)=xlb(4)*(xlb(4)/amc)**(-2.d0/27.d0)
         xlb(6)=xlb(5)*(xlb(5)/amt)**(2.d0/21.d0)
      elseif(n0.eq.6)then
         xlb(6)=xlambda
         xlb(5)=xlb(6)*(xlb(6)/amt)**(-2.d0/23.d0)
         xlb(4)=xlb(5)*(xlb(5)/amb)**(-2.d0/25.d0)
         xlb(3)=xlb(4)*(xlb(4)/amc)**(-2.d0/27.d0)
      endif
      
      do i=1,6
         xlb1(i)=xlb(i)
      end do
      
      if(n0.eq.3)then
         xlb(3)=xlambda
         xlb(4)=xlb(3)*(xlb(3)/amc)**(2.d0/25.d0)
     .                *(2.d0*log(amc/xlb(3)))**(-107.d0/1875.d0)
         xlb(4)=xiter_orig(amc,xlb(3),3,xlb(4),4,acc)
         xlb(5)=xlb(4)*(xlb(4)/amb)**(2.d0/23.d0)
     .                *(2.d0*log(amb/xlb(4)))**(-963.d0/13225.d0)
         xlb(5)=xiter_orig(amb,xlb(4),4,xlb(5),5,acc)
         xlb(6)=xlb(5)*(xlb(5)/amt)**(2.d0/21.d0)
     .                *(2.d0*log(amt/xlb(5)))**(-321.d0/3381.d0)
         xlb(6)=xiter_orig(amt,xlb(5),5,xlb(6),6,acc)
      elseif(n0.eq.4)then
         xlb(4)=xlambda
         xlb(5)=xlb(4)*(xlb(4)/amb)**(2.d0/23.d0)
     .                *(2.d0*log(amb/xlb(4)))**(-963.d0/13225.d0)
         xlb(5)=xiter_orig(amb,xlb(4),4,xlb(5),5,acc)
         xlb(3)=xlb(4)*(xlb(4)/amc)**(-2.d0/27.d0)
     .                *(2.d0*log(amc/xlb(4)))**(107.d0/2025.d0)
         xlb(3)=xiter_orig(amc,xlb(4),4,xlb(3),3,acc)
         xlb(6)=xlb(5)*(xlb(5)/amt)**(2.d0/21.d0)
     .                *(2.d0*log(amt/xlb(5)))**(-321.d0/3381.d0)
         xlb(6)=xiter_orig(amt,xlb(5),5,xlb(6),6,acc)
      elseif(n0.eq.5)then
         xlb(5)=xlambda
         xlb(4)=xlb(5)*(xlb(5)/amb)**(-2.d0/25.d0)
     .                *(2.d0*log(amb/xlb(5)))**(963.d0/14375.d0)
         xlb(4)=xiter_orig(amb,xlb(5),5,xlb(4),4,acc)
         xlb(3)=xlb(4)*(xlb(4)/amc)**(-2.d0/27.d0)
     .                *(2.d0*log(amc/xlb(4)))**(107.d0/2025.d0)
         xlb(3)=xiter_orig(amc,xlb(4),4,xlb(3),3,acc)
         xlb(6)=xlb(5)*(xlb(5)/amt)**(2.d0/21.d0)
     .                *(2.d0*log(amt/xlb(5)))**(-321.d0/3381.d0)
         xlb(6)=xiter_orig(amt,xlb(5),5,xlb(6),6,acc)
      elseif(n0.eq.6)then
         xlb(6)=xlambda
         xlb(5)=xlb(6)*(xlb(6)/amt)**(-2.d0/23.d0)
     .                *(2.d0*log(amt/xlb(6)))**(321.d0/3703.d0)
         xlb(5)=xiter_orig(amt,xlb(6),6,xlb(5),5,acc)
         xlb(4)=xlb(5)*(xlb(5)/amb)**(-2.d0/25.d0)
     .                *(2.d0*log(amb/xlb(5)))**(963.d0/14375.d0)
         xlb(4)=xiter_orig(amb,xlb(5),5,xlb(4),4,acc)
         xlb(3)=xlb(4)*(xlb(4)/amc)**(-2.d0/27.d0)
     .                *(2.d0*log(amc/xlb(4)))**(107.d0/2025.d0)
         xlb(3)=xiter_orig(amc,xlb(4),4,xlb(3),3,acc)
      endif

      do i=1,6
         xlb2(i)=xlb(i)
      end do
      
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function RUNM_EXT(q,nf,n)
      
      implicit none 
      
      integer nf,n,nn,n0,i
      real*8  q,zeta3,amsb,amc,amb,amt
     &       ,x,xk,xkfac,xmsb,pi,q0,nnlo
     &       ,B0,B1,B2,G0,G1,G2,C1,C2,TRAN,cq
     &       ,ALPHAS_RUNM

      parameter (nn=6)
      parameter (zeta3 = 1.202056903159594d0)
      real*8    am(nn),ymsb(nn)

      integer    n0a
      real*8          xlambda,amca,amba,amta
      COMMON/ALS_RUNM/xlambda,amca,amba,amta,n0a

      B0(nf)=(33.d0-2.d0*nf)/12d0
      B1(nf) = (102d0-38d0/3d0*nf)/16d0
      B2(nf) = (2857d0/2d0-5033d0/18d0*nf+325d0/54d0*nf**2)/64d0
      G0(nf) = 1d0
      G1(nf) = (202d0/3d0-20d0/9d0*nf)/16d0
      G2(nf) = (1249d0-(2216d0/27d0+160d0/3d0*zeta3)*nf
     .       - 140d0/81d0*nf**2)/64d0
      C1(nf,n) = dmin1( dble(n-1), 1.D0) 
     .          *( G1(nf)/B0(nf) - B1(nf)*G0(nf)/B0(nf)**2 )
      C2(nf,n) = dmax1( dble(n-2), 0.D0)
     .          *( ((G1(nf)/B0(nf) - B1(nf)*G0(nf)/B0(nf)**2)**2
     .       + G2(nf)/B0(nf) + B1(nf)**2*G0(nf)/B0(nf)**3
     .       - B1(nf)*G1(nf)/B0(nf)**2 - B2(nf)*G0(nf)/B0(nf)**2)/2d0 )
      TRAN(x,xk,n)=1d0+4d0/3d0*ALPHAS_RUNM(x,n)/pi
     .              +xk*(ALPHAS_RUNM(x,n)/pi)**2
      CQ(x,nf,n)=(2d0*B0(nf)*x)**(G0(nf)/B0(nf))
     .            *(1d0+C1(nf,n)*x+C2(nf,n)*x**2)

      pi=4d0*atan(1d0)
ctp [begin]
      amsb = .3d0
      amc  = amca
      amb  = amba 
      amt  = amta 
ctp [end]
      if (n.lt.3) then 
         nnlo = 0
      else if (n.eq.3) then 
         nnlo = 1
      end if 

      am(1) = 0
      am(2) = 0
      am(3) = amsb
      am(4) = amc
      am(5) = amb
      am(6) = amt
      
      xk = 16.11d0
      do i=1,nf-1
         xk = xk - 1.04d0*(1.d0-am(i)/am(nf))
      end do
      
      if(nf.ge.4)then
         xmsb = am(nf)/TRAN(am(nf),0d0,n)
      else
         xmsb = 0
      endif
      ymsb(3) = amsb
      if(nf.eq.3)then
         ymsb(4) = ymsb(3)*CQ(ALPHAS_RUNM(am(4),n)/pi,3,n)/
     .                     CQ(ALPHAS_RUNM(1.d0 ,n)/pi,3,n)
         ymsb(5) = ymsb(4)*CQ(ALPHAS_RUNM(am(5),n)/pi,4,n)/
     .                     CQ(ALPHAS_RUNM(am(4),n)/pi,4,n)
         ymsb(6) = ymsb(5)*CQ(ALPHAS_RUNM(am(6),n)/pi,5,n)/
     .                     CQ(ALPHAS_RUNM(am(5),n)/pi,5,n)
      elseif(nf.eq.4)then
         ymsb(4) = xmsb
         ymsb(5) = ymsb(4)*CQ(ALPHAS_RUNM(am(5),n)/pi,4,n)/
     .                     CQ(ALPHAS_RUNM(am(4),n)/pi,4,n)
         ymsb(6) = ymsb(5)*CQ(ALPHAS_RUNM(am(6),n)/pi,5,n)/
     .                     CQ(ALPHAS_RUNM(am(5),n)/pi,5,n)
      elseif(nf.eq.5)then
         ymsb(5) = xmsb
         ymsb(4) = ymsb(5)*CQ(ALPHAS_RUNM(am(4),n)/pi,4,n)/
     .                     CQ(ALPHAS_RUNM(am(5),n)/pi,4,n)
         ymsb(6) = ymsb(5)*CQ(ALPHAS_RUNM(am(6),n)/pi,5,n)/
     .                     CQ(ALPHAS_RUNM(am(5),n)/pi,5,n)
      elseif(nf.eq.6)then
         ymsb(6) = xmsb
         ymsb(5) = ymsb(6)*CQ(ALPHAS_RUNM(am(5),n)/pi,5,n)/
     .                     CQ(ALPHAS_RUNM(am(6),n)/pi,5,n)
         ymsb(4) = ymsb(5)*CQ(ALPHAS_RUNM(am(4),n)/pi,4,n)/
     .                     CQ(ALPHAS_RUNM(am(5),n)/pi,4,n)
      endif
      if(q.lt.amc)then
         n0=3
         q0 = 1.d0
      elseif(q.le.amb)then
         n0=4
         q0 = amc
      elseif(q.le.amt)then
         n0=5
         q0 = amb
      else
         n0=6
         q0 = amt
      endif
      if(nnlo.eq.1.and.nf.gt.3)then
         xkfac = TRAN(am(nf),0d0,n)/TRAN(am(nf),xk,n)
      else
         xkfac = 1d0
      endif
      RUNM_EXT = ymsb(n0)*CQ(ALPHAS_RUNM(q,n)/pi,n0,n)/
     .                    CQ(ALPHAS_RUNM(q0,n)/pi,n0,n)
     .       * xkfac
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision function ALPHAS_RUNM(q,n)

      implicit none

      integer n,i,nf
      real*8  q,B0,B1,ALS1,ALS2,pi,x,xlb(6)
      
      real*8             xlb1(6),xlb2(6)
      COMMON/ALSLAM_RUNM/xlb1   ,xlb2

      integer n0
      real*8          xlambda,amc,amb,amt
      COMMON/ALS_RUNM/xlambda,amc,amb,amt,n0

      B0(nf)=33.d0-2.d0*nf
      B1(nf)=6.d0*(153.d0-19.d0*nf)/B0(nf)**2
      ALS1(nf,x)=12.d0*pi/(B0(nf)*log(x**2/xlb(nf)**2))
      ALS2(nf,x)=12.d0*pi/(B0(nf)*log(x**2/xlb(nf)**2))
     .          *(1.d0-B1(nf)*log(log(x**2/xlb(nf)**2))
     .           /log(x**2/xlb(nf)**2))
      pi=4.d0*atan(1.d0)
      if (n.eq.1) then
         do i=1,6
            xlb(i)=xlb1(i)
         end do
      else if (n.gt.1) then 
         do i=1,6
            xlb(i)=xlb2(i)
         end do
      else 
         print*, " ALPHAS_RUNM: order not set correctly ",n
         stop
      end if

      if(q.lt.amc)then
       nf=3
      elseif(q.le.amb)then
       nf=4
      elseif(q.le.amt)then
       nf=5
      else
       nf=6
      endif

      if (n.eq.1) then
         ALPHAS_RUNM = ALS1(nf,q)
      else if (n.gt.1) then 
         ALPHAS_RUNM = ALS2(nf,q)
      else 
         print*, " ALPHAS_RUNM: order not set correctly ",n
         stop
      endif

      return
      end 

