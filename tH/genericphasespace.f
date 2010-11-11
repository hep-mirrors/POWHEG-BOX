      subroutine phasespaceregular(xx)
      implicit none
      include 'nlegborn.h'
      real * 8 xx(ndiminteg)
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      include '../include/pwhg_math.h'
      include 'PhysPars.h'
      real * 8 jac,minmass,mtstar,ptstar(0:3),vec(3),beta,mch,mt,tmp,m
      integer i,mu
      jac=1
      mch=kn_masses(3)
      mt=kn_masses(4)
      minmass=mch+mt
      call x1x2phspace(kn_sbeams,minmass,xx(1),kn_x1,kn_x2,kn_sreal,jac)
c mass of the H+ b~ system
      m=sqrt(kn_sreal)
      call lorentian(xx(3),mt,ph_twidth,mch,m-mt,mtstar,jac)
c should be d mstar^2/(2 pi)
      jac = jac * mtstar/pi
c generate t tstar system (CM)
      call twobody0(xx(4),m,mt,mtstar,kn_cmpreal(0,4),
     1     ptstar,jac)
c now two body decay of tstar, in tstar rest frame
      call twobody1(xx(5),mtstar,mch,0d0,
     1     kn_cmpreal(0,3),kn_cmpreal(0,5),jac)
c boost them in cm
      vec(1)=ptstar(1)/ptstar(0)
      vec(2)=ptstar(2)/ptstar(0)
      vec(3)=ptstar(3)/ptstar(0)
      beta=sqrt(vec(1)**2+vec(2)**2+vec(3)**2)
      vec(1)=vec(1)/beta
      vec(2)=vec(2)/beta
      vec(3)=vec(3)/beta
      call mboost(1,vec,beta,kn_cmpreal(0,3),kn_cmpreal(0,3))
      call mboost(1,vec,beta,kn_cmpreal(0,5),kn_cmpreal(0,5))
      do i=1,2
         do mu=1,2
            kn_cmpreal(mu,i) = 0
         enddo
      enddo
      kn_cmpreal(0,1) = sqrt(kn_sreal)/2
      kn_cmpreal(3,1) = kn_cmpreal(0,1)
      kn_cmpreal(0,2) = kn_cmpreal(0,1)
      kn_cmpreal(3,2) =-kn_cmpreal(0,1)
      vec(1) = 0
      vec(2) = 0
      vec(3) = 1
      beta=(kn_x1-kn_x2)/(kn_x1+kn_x2)
      call mboost(5,vec,beta,kn_cmpreal(0,1),kn_preal(0,1))
      kn_jacborn = jac
      end
      

      subroutine x1x2phspace(sbeams,minmass,xx,x1,x2,s,jac)
      implicit none
      real * 8 sbeams,minmass,xx(2),x1,x2,s,jac,tau,taumin,y
      taumin=minmass**2/sbeams
      tau=exp(log(taumin)*(1-xx(1)**2))
      jac=jac*tau*abs(log(taumin))*2*xx(1)
      s=sbeams*tau
      y=-(1-2*xx(2))*log(tau)/2
      jac=-jac*log(tau)
      x1=sqrt(tau)*exp(y)
      x2=tau/x1
      end

      subroutine twobody0(xx,m,m1,m2,p1,p2,xjac)
      implicit none
      include '../include/pwhg_math.h'
      real * 8 xx(1),m,m1,m2,p1(0:3),p2(0:3),xjac
      real * 8 k,beta,vec(3),stheta,ctheta
      if(m.le.m1+m2) then
         xjac=0
         return
      endif
      k=sqrt((m-(m1+m2))*(m+m1+m2)*(m-(m1-m2))*(m+m1-m2))/(2*m)
      ctheta=1-2*xx(1)
      stheta=sqrt(abs((1-ctheta)*(1+ctheta)))
      p1(3)=ctheta*k
      p1(1)=stheta*k
      p1(2)=0
      p2(1)=-p1(1)
      p2(2)=-p1(2)
      p2(3)=-p1(3)
      p1(0)=sqrt(m1**2+k**2)
      p2(0)=sqrt(m2**2+k**2)
      xjac=xjac/(8*pi)*2*k/m
      end

      subroutine twobody1(xx,m,m1,m2,p1,p2,xjac)
      implicit none
      include '../include/pwhg_math.h'
      real * 8 xx(2),m,m1,m2,p1(0:3),p2(0:3),xjac
      real * 8 k,beta,vec(3),stheta,ctheta,phi
      if(m.le.m1+m2) then
         xjac=0
         return
      endif
      k=sqrt((m-(m1+m2))*(m+m1+m2)*(m-(m1-m2))*(m+m1-m2))/(2*m)
      ctheta=1-2*xx(1)
      stheta=sqrt(abs((1-ctheta)*(1+ctheta)))
      phi=2*pi*xx(2)
      p1(3)=ctheta*k
      p1(1)=stheta*k*cos(phi)
      p1(2)=stheta*k*sin(phi)
      p2(1)=-p1(1)
      p2(2)=-p1(2)
      p2(3)=-p1(3)
      p1(0)=sqrt(m1**2+k**2)
      p2(0)=sqrt(m2**2+k**2)
      xjac=xjac/(8*pi)*2*k/m
      end


      subroutine lorentian(x,m0,gam,mmin,mmax,m,jac)
      implicit none
      real * 8 x,m0,gam,mmin,mmax,m,jac
      real * 8 atanmin,atanmax,atax
      atanmin=atan((mmin-m0)*2/gam)
      atanmax=atan((mmax-m0)*2/gam)
      atax=atanmin+x*(atanmax-atanmin)
      jac=jac*(atanmax-atanmin)
      m=tan(atax)*gam/2+m0
      jac=jac*gam/2*(1+((m-m0)*2/gam)**2)
      end

