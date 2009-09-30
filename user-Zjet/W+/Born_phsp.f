      subroutine born_phsp(xborn)
      implicit none
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      include '../include/pwhg_math.h'
      include 'PhysPars.h'
      real * 8 xborn(ndiminteg-3)
      real * 8 m2,xjac,tau,y,beta,vec(3),cth,cthdec,phidec,s,
     #     z,zhigh,zlow
      integer mu,k,j
      logical ini
      data ini/.true./
      save ini
      if(ini) then
         do k=1,nlegborn
            kn_masses(k)=0
         enddo
         kn_masses(nlegreal)=0
         ini=.false.
      endif
c Phase space:
c 1 /(16 pi S) d m^2 d cth d y
      xjac=1d0/kn_sbeams/(16*pi)
      zlow=atan((ph_Wmass2low  - ph_Wmass2)/ph_WmWw)
      zhigh=atan((min(ph_Wmass2high,kn_sbeams)  - ph_Wmass2)/ph_WmWw)
      z=zlow+(zhigh-zlow)*xborn(1)
      xjac=xjac*(zhigh-zlow)
      m2=ph_WmWw*tan(z)+ph_Wmass2
c d m^2 jacobian
      xjac=xjac*ph_WmWw/cos(z)**2
c d x1 d x2 = d tau d y;
      tau=m2/kn_sbeams
      s=kn_sbeams*tau
      kn_sborn=s
c ymax=|log(tau)|/2
      y=-(1-2*xborn(2))*log(tau)/2
      xjac=-xjac*log(tau)
      z=1-2*xborn(3)
      xjac=xjac*2
      cth=1.5d0*(z-z**3/3)
      xjac=xjac*1.5d0*(1-z**2)
      kn_born_pt2=0d0
      
c
      cthdec=cth
      phidec=0d0
      kn_cthdec=cthdec
      kn_jacborn=xjac
c Build kinematics
      kn_xb1=sqrt(tau)*exp(y)
      kn_xb2=tau/kn_xb1
c decay products in their rest frame
      kn_cmpborn(0,3)=sqrt(m2)/2
      kn_cmpborn(0,4)=kn_cmpborn(0,3)
      kn_cmpborn(3,3)=kn_cthdec*kn_cmpborn(0,3)
      kn_cmpborn(1,3)=sqrt(1-kn_cthdec**2)*cos(phidec)*kn_cmpborn(0,3)
      kn_cmpborn(2,3)=sqrt(1-kn_cthdec**2)*sin(phidec)*kn_cmpborn(0,3)
      kn_cmpborn(1,4)=-kn_cmpborn(1,3)
      kn_cmpborn(2,4)=-kn_cmpborn(2,3)
      kn_cmpborn(3,4)=-kn_cmpborn(3,3)
c initial state particles
      kn_cmpborn(0,1)=sqrt(s)/2
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

c CAVEAT!!!
c minimal final state mass (after init_couplings, Wmass is defined
      kn_minmass=ph_Wmass

      end






      subroutine born_suppression(fact)
      implicit none
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      logical ini
      data ini/.true./
      real * 8 fact,pt2,pt2supp,powheginput,pt
      save ini,pt2supp,pt     
c CAVEAT!!!  questa sub e' process dependent!  Dove metterla? 
      if (ini) then
         pt = powheginput("#ptsupp")         
         ini = .false.
         pt2supp = pt**2
      endif
      if (pt.lt.0) then
         fact=1d0
      else         
         fact=1d0
c         pt2=kn_pborn(1,5)**2+kn_pborn(2,5)**2
c         fact=pt2/(pt2+pt2supp)         
c      if (pt2.gt.10) then
c         fact = 1d0
c      else
c         fact = 0d0
c      endif
      endif
      end


      subroutine set_fac_ren_scales(muf,mur)
      implicit none
      include 'PhysPars.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      real * 8 muf,mur
      muf=ph_Wmass
      mur=ph_Wmass
      end
