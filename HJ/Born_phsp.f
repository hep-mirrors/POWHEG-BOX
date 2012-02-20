      subroutine born_phsp(xborn)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'      
      include 'coupl.inc'
      include 'pwhg_math.h'
      include 'PhysPars.h'
      real * 8 xborn(ndiminteg-3)
      real * 8 m2,xjac,taumin,tau,y,beta,vec(3),cth,s,
     # z,zhigh,zlow,khiggs,cthmax,bwcutoff
      integer mu,k
      logical ini
      data ini/.true./
      save ini
      real * 8 powheginput
      external powheginput
      if(ini) then
c     set initial- and final-state masses for Born and real
         do k=1,nlegborn
            kn_masses(k)=0
         enddo
         kn_masses(nlegreal)=0
         ph_HmHw=hmass*hwidth
         ph_Hmass2=hmass**2
         bwcutoff=powheginput("bwcutoff")
         ph_Hmass2low=max(hmass-bwcutoff*hwidth,0d0)**2
         ph_Hmass2high=min(hmass+bwcutoff*hwidth,sqrt(kn_sbeams))**2
         ini=.false.
      endif
      zlow=atan((ph_Hmass2low  - ph_Hmass2)/ph_HmHw)
      zhigh=atan((min(ph_Hmass2high,kn_sbeams)  - ph_Hmass2)/ph_HmHw)
      z=zlow+(zhigh-zlow)*xborn(1)
      xjac=zhigh-zlow
      m2=ph_HmHw*tan(z)+ph_Hmass2
c     The BW integrates to Pi ==> divide by Pi
      xjac=xjac/pi
c     assign the Higgs boson mass
      kn_masses(3)=sqrt(m2)
c d x1 d x2 = d tau d y;
      taumin=( sqrt(m2+kn_ktmin**2) + kn_ktmin )**2/kn_sbeams
      tau=exp(log(taumin)*(1-xborn(2)**2))
      xjac=xjac*tau*abs(log(taumin))*2*xborn(2)
      s=kn_sbeams*tau
      kn_sborn=s
c compute H momentum in partonic cm
      khiggs=(s-m2)/(2*sqrt(s))
c ymax=|log(tau)|/2
      y=-(1-2*xborn(3))*log(tau)/2
      xjac=-xjac*log(tau)
      cthmax=sqrt(1-(kn_ktmin/khiggs)**2)
      z=1-2*xborn(4)
      xjac=xjac*2
      cth=1.5d0*(z-z**3/3)
      xjac=xjac*1.5d0*(1-z**2)
      cth=cth*cthmax
c      kn_born_pt2=(1-cth**2)*khiggs**2
      xjac=xjac*cthmax
c supply 2 pi for azimuthal integration (not performed)
      xjac=xjac*2*pi
      xjac=xjac*(s-m2)/s/(8*(2*pi)**2)
c
      kn_jacborn=xjac
c Build kinematics
c velocity of H in partonic CM
      kn_cmpborn(1,3)=sqrt(1-cth**2)*khiggs
      kn_cmpborn(2,3)=0
      kn_cmpborn(3,3)=cth*khiggs
      kn_cmpborn(0,3)=sqrt(m2+khiggs**2)
      kn_xb1=sqrt(tau)*exp(y)
      kn_xb2=tau/kn_xb1
      kn_cmpborn(1,4)=-kn_cmpborn(1,3)
      kn_cmpborn(2,4)=-kn_cmpborn(2,3)
      kn_cmpborn(3,4)=-kn_cmpborn(3,3)
      kn_cmpborn(0,4)=khiggs
      kn_cmpborn(0,1)=sqrt(kn_sborn)/2
      kn_cmpborn(0,2)=kn_cmpborn(0,1)
      kn_cmpborn(3,1)=kn_cmpborn(0,1)
      kn_cmpborn(3,2)=-kn_cmpborn(0,2)
      kn_cmpborn(1,1)=0
      kn_cmpborn(1,2)=0
      kn_cmpborn(2,1)=0
      kn_cmpborn(2,2)=0      
c now boost everything along 3
      beta=(kn_xb1-kn_xb2)/(kn_xb1+kn_xb2)
      vec(1)=0
      vec(2)=0
      vec(3)=1
      call mboost(nlegborn-2,vec,beta,kn_cmpborn(0,3),kn_pborn(0,3))
      do mu=0,3
         kn_pborn(mu,1)=kn_xb1*kn_beams(mu,1)
         kn_pborn(mu,2)=kn_xb2*kn_beams(mu,2)
      enddo
c      call checkmomzero(nlegborn,kn_pborn)
c      call checkmass(2,kn_pborn(0,3))

c minimal final state mass 
c      kn_minmass=sqrt(ph_Hmass2low)
      kn_minmass=kn_ktmin + sqrt(kn_ktmin**2 + ph_Hmass2low)
      end


      subroutine born_suppression(fact)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      include 'coupl.inc'
      include 'pwhg_flg.h'
      real * 8 fact,pt2,ptmin
      ptmin=20
      if(flg_weightedev) then
         pt2=kn_cmpborn(1,4)**2+kn_cmpborn(2,4)**2
         fact=pt2/(pt2+ptmin**2)
      else
         fact=1
      endif
      end


      subroutine set_fac_ren_scales(muf,mur)
      implicit none
      include 'coupl.inc'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      real * 8 muf,mur
      logical ini
      data ini/.true./
      logical runningscales
      real * 8 pt2
      real * 8 powheginput
      external powheginput      
      save ini,runningscales

      if (ini) then
         if (powheginput("#runningscales").eq.1) then
            runningscales=.true.
            write(*,*) '****************************************'
            write(*,*) '****************************************'
            write(*,*) '**   mur=pt  used for Bbar function   **'
            write(*,*) '**   muf=pt  used for Bbar function   **'
            write(*,*) '****************************************'
            write(*,*) '****************************************'
         else
            runningscales=.false.
         endif
         ini=.false.
      endif
      
      if (runningscales) then
         pt2=kn_pborn(1,4)**2+kn_pborn(2,4)**2
         mur=max(sqrt(pt2),1d0)
         muf=mur
      else
         muf=hmass
         mur=hmass
      endif
      end


