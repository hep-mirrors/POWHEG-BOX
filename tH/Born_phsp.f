      subroutine born_phsp(xborn)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      include '../include/pwhg_math.h'
      include 'PhysPars.h'
      real * 8 xborn(ndiminteg-3)     
      real * 8 Mch, Mch2, Mtop, Mtop2, tau, taumax, taumin, xjac, s,
     1   y, ymax, ymin , t, tmin, tmax, t1, t2, beta, vec(3)      
      integer mu,k,j
      logical ini
      data ini/.true./
      save ini
      real * 8 mass      
      external mass      
      logical check
      parameter(check=.false.)
      logical BW
      parameter (BW=.false.)
      real * 8 epsilon
      parameter (epsilon=1d-10)
      real * 8 testCM(0:3), testLab(0:3)
      real * 8 ehiggs,etop,ptop,cth,sth,ltau


      Mch = ph_mCH
      Mtop = ph_mT
      
      Mch2 = Mch**2d0
      Mtop2 = Mtop**2d0
      
            
c      if (ph_mCH.lt.ph_mT)  then
c      	write(*,*) 'ATTENTION: choose a charged Higgs mass GREATER than '
c	write(*,*) 'the top mass !!! '
c	stop
c      endif
      
      

      if(ini) then
c     set initial- and final-state masses for Born and real
         do k=1,nlegborn
	 	
            if( (k.eq.1).or.(k.eq.2) ) then
	    	kn_masses(k)=0
	    elseif(k.eq.3) then
	    	kn_masses(k) = Mch 
	    elseif(k.eq.4) then
	    	kn_masses(k) = Mtop
	    endif
         enddo
      endif
      xjac = 1d0
      taumin = (Mtop+Mch)**2d0/kn_sbeams
      taumax = 1d0
      ltau = log(taumin) + log(taumax/taumin)*xborn(1)
      xjac = xjac*log(taumax/taumin)
      tau=exp(ltau)
      xjac=xjac*tau
      
      
      s = kn_sbeams*tau
      kn_sborn = s
      

      ymax = -log(tau)/2d0
      ymin = log(tau)/2d0
            
      
      y = ymin + (ymax-ymin)*xborn(2) 
      xjac = xjac*(ymax-ymin)
      
      t1 = Mtop2 + Mch2 - s
      t2 = Sqrt( (s-Mtop2-Mch2)**2d0 - 4*Mtop2*Mch2 )
      tmax = (t1 + t2)/2
      tmin = (t1 - t2)/2
      t = tmin + (tmax-tmin)*xborn(3)
      xjac = xjac*(tmax-tmin)           

c     supply 2 pi for azimuthal phi integration 
c     phi fixed to 0, generated randomly afterwards by POWHEG 
      xjac=xjac*2*pi

c     factor for the two-body phase space
      xjac=xjac/(16*pi**2*s)
      
      kn_jacborn = xjac
      
c--------------------------------------------------------c      
c-----------------     Build kinematics	-----------------c
c--------------------------------------------------------c 
      
      kn_xb1=sqrt(tau)*exp(y)
      kn_xb2=tau/kn_xb1

c---------   Build kinematics in the CM frame -----------c

c initial state particles
      kn_cmpborn(0,1)=sqrt(s)/2
      kn_cmpborn(0,2)=kn_cmpborn(0,1)
      kn_cmpborn(3,1)=kn_cmpborn(0,1)
      kn_cmpborn(3,2)=-kn_cmpborn(0,2)
      kn_cmpborn(1,1)=0
      kn_cmpborn(1,2)=0
      kn_cmpborn(2,1)=0
      kn_cmpborn(2,2)=0  
      
c  final state particles, arbitrarily aligned on the x axis
      ehiggs=(s-Mtop2+Mch2)/(2*sqrt(s))
      ptop=sqrt((s-mtop2-mch2)**2-4*mtop2*mch2)/(2*sqrt(s))
      etop=sqrt(s)-ehiggs
      cth=(-(t-mtop2)/sqrt(s)-etop)/ptop
      sth=sqrt(1-cth**2)
      kn_cmpborn(0,3) = ehiggs
      kn_cmpborn(1,3) = -sth*ptop
      kn_cmpborn(2,3) = 0d0
      kn_cmpborn(3,3) = -cth*ptop
      
      kn_cmpborn(0,4) = etop
      kn_cmpborn(1,4) = sth*ptop
      kn_cmpborn(2,4) = 0d0
      kn_cmpborn(3,4) = cth*ptop
      
      beta=(kn_xb1-kn_xb2)/(kn_xb1+kn_xb2)
      vec(1)=0
      vec(2)=0
      vec(3)=1
c      call mboost(nlegborn-2,vec,beta,kn_cmpborn(0,3),kn_pborn(0,3))   ????
      call mboost(nlegborn,vec,beta,kn_cmpborn,kn_pborn)
            
      kn_minmass =  Mtop + Mch
      
      end


      function mass(p)
      implicit none
      real * 8 p(0:3),mass
      mass = sqrt(abs(p(0)**2-p(1)**2-p(2)**2-p(3)**2))
      end



      subroutine born_suppression(fact)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      logical ini
      data ini/.true./
      real * 8 fact,pt2,pt2supp,powheginput,pt
      save ini,pt2supp,pt     
c CAVEAT!!!  process dependent subroutine
      fact=1
      end


      subroutine set_fac_ren_scales(muf,mur)
      implicit none
      include 'PhysPars.h'
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      real * 8 muf,mur
      muf = (ph_mT + ph_mCH)/2d0
      mur = (ph_mT + ph_mCH)/2d0
c      muf =  ph_mT
c      mur =  ph_mT
      
      end
