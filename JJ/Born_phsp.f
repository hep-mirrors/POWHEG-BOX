      subroutine born_phsp(xborn)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      include '../include/pwhg_math.h'
      include 'PhysPars.h'

      real * 8 xborn(ndiminteg-3)
      real * 8 vec(3),phidec
      integer mu,k
      logical ini
      data ini/.true./
      save ini

      real *8 xx(ndiminteg-3),xplus,xminus,cth1
      real *8 tau,tau_min,tau_max,tmp,ycm,ycm_min,ycm_max
      real *8 jacb,shat,s_had,beta
      integer ixx_tau,ixx_ycm,ixx_cth1
      parameter(
     #ixx_tau  =1,
     #ixx_ycm  =2,
     #ixx_cth1 =3)
      integer psgen
      parameter (psgen=0)
c     Parameter to generate phase space
c     psgen: <=0: importance sampling; 1: flat in tau,cth1
c     psgen=0:     flat in 1/tau, flat in cth1bar
c     psgen=1:     flat in tau, flat in cth1bar
      
      if(ini) then
         do k=1,nlegborn
            kn_masses(k)=0
         enddo
         kn_masses(nlegreal)=0
         ini=.false.
         write(*,*) " *****************************"   
         write(*,*) " ****    WARNING     *********"    
         write(*,*) "    taumin=0.2 in Born_phsp   " 
         write(*,*) "     kn_minmass=100d0         " 
         write(*,*) " *****************************"    
      endif

c     local copy of variables
c     xx(1) -> tau
c     xx(2) -> ycm
c     xx(3) -> cth1
      do k=1,ndiminteg-3
         xx(k)=xborn(k)
      enddo

      s_had=kn_sbeams
      jacb=1d0
      
      tau_min=0.2 !: da controllare
      tau_max=1d0
      if(psgen.eq.0)then
c     imp. sampling (flat in 1/tau )
         tmp=1d0/tau_max + (1d0/tau_min-1d0/tau_max)*xx(ixx_tau)
         tau=1d0/tmp
         jacb=jacb * tau**2 * (1d0/tau_min-1d0/tau_max)
      elseif(psgen.eq.1) then
c     uniform generation
         tau=tau_min + (tau_max-tau_min)*xx(ixx_tau)
         jacb=jacb * (tau_max-tau_min)
      else
         write(*,*) 'Wrong psgen in gen_born_vars'
         call exit(1)
      endif
      ycm_min=  log(tau)/2
      ycm_max= -log(tau)/2
      ycm = ycm_min + xx(ixx_ycm)*(ycm_max-ycm_min)
      jacb=jacb * (ycm_max-ycm_min)

      shat=tau*s_had

c     th1 is the angle of 1st outgoing particle wrt +z axis
      if(psgen.eq.0)then
c     uniform generation
         cth1=-1d0+xx(ixx_cth1)*2d0 
         jacb=jacb * 2d0 
      elseif(psgen.eq.1) then
c     uniform generation
         cth1=-1d0+xx(ixx_cth1)*2d0 
         jacb=jacb * 2d0 
      else
         write(*,*) 'Wrong psgen in gen_born_vars'
         call exit(1)
      endif
 
c     born phase space: physical phase space
      jacb=jacb /16d0/pi   ! *( d_phi/(2d0*pi) )

c     Feynman x's and Mandelstam invariants
      xplus=sqrt(tau) * exp(ycm)
      xminus=sqrt(tau) * exp(-ycm)

cccccccccccccccccccccccccccc
c     assign born jacobian and default kinematics variables
      kn_jacborn=jacb
      kn_born_pt2=0d0
      kn_cthdec=cth1
      phidec=0d0
c     With this choice px_3 is always positive, but at the
c     end the whole event will be randomly rotated around z-axis
      kn_sborn=shat
cccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccc
c     compute momenta
cccccccccccccccccccccccccccccc

c Build kinematics
      kn_xb1=xplus
      kn_xb2=xminus

c initial state particles
      kn_cmpborn(0,1)=sqrt(shat)/2
      kn_cmpborn(0,2)=kn_cmpborn(0,1)
      kn_cmpborn(3,1)=kn_cmpborn(0,1)
      kn_cmpborn(3,2)=-kn_cmpborn(0,2)
      kn_cmpborn(1,1)=0
      kn_cmpborn(1,2)=0
      kn_cmpborn(2,1)=0
      kn_cmpborn(2,2)=0  

c final state particles 
      kn_cmpborn(0,3)=sqrt(shat)/2
      kn_cmpborn(0,4)=kn_cmpborn(0,3)

      kn_cmpborn(1,3)=sqrt(1-kn_cthdec**2)*sin(phidec)*kn_cmpborn(0,3)
      kn_cmpborn(2,3)=sqrt(1-kn_cthdec**2)*cos(phidec)*kn_cmpborn(0,4)
      kn_cmpborn(3,3)=kn_cthdec*kn_cmpborn(0,3) 

      kn_cmpborn(1,4)=-kn_cmpborn(1,3)
      kn_cmpborn(2,4)=-kn_cmpborn(2,3)
      kn_cmpborn(3,4)=-kn_cmpborn(3,3)

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
      call checkmomzero(nlegborn,kn_pborn)
c      call checkmass(2,kn_pborn(0,3))

c     !ER: for now, just use the main relevant scale of the process
c     (needed to do log-scale graphs)
      kn_minmass=100d0  !ER: ora e' completamente arbitrario

      end

      subroutine born_suppression(fact)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_kn.h'
      real * 8 fact
      logical ini
      data ini/.true./
      real *8 ptb
      if (ini) then
         write(*,*) '**************************'
         write(*,*) 'Born pt > 100 GeV in  born_suppression'
         write(*,*) '**************************'
         ini=.false.
      endif
      ptb=sqrt(dabs(kn_cmpborn(1,3)**2+kn_cmpborn(2,3)**2))
      fact=1.
      if(ptb.lt.100d0) then
         fact=0.
      endif
      end


      subroutine set_fac_ren_scales(muf,mur)
      implicit none
      include 'PhysPars.h'
      include 'nlegborn.h'
      include '../include/pwhg_kn.h'
      real * 8 muf,mur
      logical ini
      data ini/.true./
      real *8 muref
      real *8 dotp
      external dotp
      if (ini) then
         write(*,*) '*************************************'
         write(*,*) '    Factorization and renormalization '
         write(*,*) '    scales set to 100 GeV         '
         write(*,*) '*************************************'
         ini=.false.
      endif
      muref=100. !ER: completamente arbitrario
      muf=muref
      mur=muref
c     CAVEAT:
c     Never tried to set mu_r != mu_f

      end

