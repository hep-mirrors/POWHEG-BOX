      implicit none
      real * 8 p1(0:3),p2(0:3)
      common/momenta/p1,p2
      integer j
      real * 8 xl,xu,acc
      integer ndim,ncall,itmx,nprn
      common/bveg1/xl(10),xu(10),acc,ndim,ncall,itmx,nprn
      real * 8 av,sd,chi2,av1
      real * 8 ffmm,ffmm0,softintmm,softintmm0,
     #     ffm,ffm0,softintm,softintm0,random
      external ffmm,ffmm0,softintmm,softintmm0,
     #     ffm,ffm0,softintm,softintm0,random
      real * 8 pi
      parameter (pi=3.141592653589793d0)

 1    continue
      p1(0)=0
      p2(0)=0
      do j=1,3
         p1(j)=cos(pi*random())
         p2(j)=cos(pi*random())
         p1(0)=p1(0)+p1(j)**2
         p2(0)=p2(0)+p2(j)**2
      enddo
      p1(0)=sqrt(p1(0)+random())
      p2(0)=sqrt(p2(0)+random())
      xl(1)=0
      xl(2)=0
      xu(1)=1
      xu(2)=1
      ndim=2
      ncall=1000000
      itmx=5
      nprn=0
           
      call vegas(ffmm0,av,sd,chi2)
      av1=softintmm0(p1,p2)
      write(*,*) 'test 1 ',av/av1,av-av1,sd/abs(av1),chi2

      call vegas(ffmm,av,sd,chi2)
      av1=softintmm(p1,p2)
      write(*,*) 'test 2 ',av/av1,av-av1,sd/abs(av1),chi2

      call vegas(ffm,av,sd,chi2)
      av1=softintm(p1)
      write(*,*) 'test 3 ',av/av1,av-av1,sd/abs(av1),chi2


      call vegas(ffm0,av,sd,chi2)
      av1=softintm0(p1)
      write(*,*) 'test 4 ',av/av1,av-av1,sd/abs(av1),chi2

      goto 1
      end

      

      function softintmm(p1,p2)
c               / 
c               |           d phi                             p1.p2  
c softintmm= -2 | d cos th  ----- log(sin th sin phi) k0^2 ----------
c               |             pi                           p1.k  p2.k
c               /
c p1^2>0, p2^2>0.
c The range in phi is 0<phi<pi.
      implicit none
      real * 8 p1(0:3),p2(0:3), softintmm
      real * 8 i,z,zm,zp,z0,z1,a,b,x0,c,alph,kh(3),gh(3),k2,g2,kg
      integer j
      real * 8 ddilog
      external ddilog
      i(z)=-1d0/2*log((z-zm)*(zp-z)/((zp+z)*(z+zm)))**2
     # -2*ddilog(2*zm/(zp-zm)*(zp-z)/(zm+z))
     # -2*ddilog(-2*zp/(zp-zm)*(zm+z)/(zp-z))
      do j=1,3
         kh(j)=p1(j)/p1(0)
         gh(j)=p2(j)/p2(0)
      enddo
      k2=0
      g2=0
      kg=0
      do j=1,3
         k2=k2+kh(j)**2
         g2=g2+gh(j)**2
         kg=kg+kh(j)*gh(j)
      enddo
      a=k2+g2-2*kg
      x0=-(kg-g2)/a
      b=a*x0**2+2*(kg-g2)*x0+g2
      c=sqrt(b/(4*a))
      alph=sqrt(b)/2
      zp=(1+sqrt(1-b))/(2*alph)
      zm=(1-sqrt(1-b))/(2*alph)
      z0=(sqrt(x0**2+4*c**2)-x0)/c/2
      z1=(sqrt((1-x0)**2+4*c**2)+(1-x0))/c/2
      if(z1.lt.z0) then
         write(*,*) ' ha '
      endif
      softintmm=-2*(i(z1)-i(z0))*(-(1-kg)
     #  /(alph**2*sqrt(a)*2*(zp**2-zm**2)))
      end

      function softintmm0(p1,p2)
c            / 
c            |           d phi        p1.p2  
c softintmm= | d cos th  ----- k0^2 ----------
c            |             pi       p1.k  p2.k
c            /
c the range in phi is 0<phi<pi.
      implicit none
      real * 8 p1(0:3),p2(0:3), softintmm0
      real * 8 kh(3),gh(3),k2,g2,kg,beta
      integer j
      real * 8 beta1, dotp
      do j=1,3
         kh(j)=p1(j)/p1(0)
         gh(j)=p2(j)/p2(0)
      enddo
      k2=0
      g2=0
      kg=0
      do j=1,3
         k2=k2+kh(j)**2
         g2=g2+gh(j)**2
         kg=kg+kh(j)*gh(j)
      enddo
      beta=sqrt(1-(1-k2)*(1-g2)/(1-kg)**2)

c      beta1=sqrt(1-dotp(p1,p1)*dotp(p2,p2)/dotp(p1,p2)**2)
c      write(*,*) 'ratio ',beta/beta1

      softintmm0=log((1+beta)/(1-beta))/beta
      end

      function ffmm(x,w)
      implicit none
      real * 8 x(2),w,ffmm
      real * 8 p1(0:3),p2(0:3)
      common/momenta/p1,p2
      real * 8 pi
      parameter (pi=3.141592653589793d0)
      integer j
      real * 8 cth,sth,xjac,ph,k2,g2,k,g,kg,cosal,sinal,l(0:3),sph
      real * 8 dotp
      external dotp      
      cth=1-2*x(1)
      xjac=2
      sth=sqrt(1-cth**2)
      ph=pi*x(2)
c no jacobian, we divide by pi
      k2=0
      g2=0
      kg=0
      do j=1,3
         k2=k2+(p1(j)/p1(0))**2
         g2=g2+(p2(j)/p2(0))**2
         kg=kg+p1(j)*p2(j)/(p1(0)*p2(0))
      enddo
      k=sqrt(k2)
      g=sqrt(g2)
      cosal=kg/(k*g)
      sinal=sqrt(1-cosal**2)


c      sph = sin(ph)
c      l(0) = 1
c      l(1) = sth*sph
c      l(2) = sth*cos(ph)
c      l(3) = cth
c      write(*,*) (1-k*cth)*p1(0)/dotp(p1,l)
c      write(*,*) (1-g*(cosal*cth+sinal*sth*cos(ph)))*p2(0)/dotp(p2,l)

      ffmm=-2*xjac*log(sth*sin(ph))*(1-kg)/
     # ((1-k*cth)*(1-g*(cosal*cth+sinal*sth*cos(ph))))
      end

      function ffmm0(x,w)
      implicit none
      real * 8 x(2),w,ffmm0
      real * 8 p1(0:3),p2(0:3)
      common/momenta/p1,p2
      real * 8 pi
      parameter (pi=3.141592653589793d0)
      integer j
      real * 8 cth,sth,xjac,ph,k2,g2,k,g,kg,cosal,sinal
      cth=1-2*x(1)
      xjac=2
      sth=sqrt(1-cth**2)
      ph=pi*x(2)
c no jacobian, we divide by pi
      k2=0
      g2=0
      kg=0
      do j=1,3
         k2=k2+(p1(j)/p1(0))**2
         g2=g2+(p2(j)/p2(0))**2
         kg=kg+p1(j)*p2(j)/(p1(0)*p2(0))
      enddo
      k=sqrt(k2)
      g=sqrt(g2)
      cosal=kg/(k*g)
      sinal=sqrt(1-cosal**2)
      ffmm0=xjac*(1-kg)/
     # ((1-k*cth)*(1-g*(cosal*cth+sinal*sth*cos(ph))))
      end


      function softintm(p)
c               /                                              2
c               |           d phi                             p  
c softintm = -2 | d cos th  ----- log(sin th sin phi) l0^2  -------
c               |             pi                            (p.l)^2
c               /
c the range in phi is 0<phi<pi.
      implicit none
      real * 8 p(0:3),softintm
      real * 8 beta2,beta
      integer j
      beta2=0
      do j=1,3
         beta2=beta2+(p(j)/p(0))**2
      enddo
      beta=sqrt(beta2)
      softintm=2*log((1+beta)/(1-beta))/beta
      end

      function softintm0(p)
c            /                         2
c            |           d phi        p
c softintmm= | d cos th  ----- l0^2 -------
c            |             pi       (p.l)^2
c            /
c the range in phi is 0<phi<pi.
      implicit none
      real * 8 p(0:3),softintm0
      softintm0=2
      end

      function ffm(x,w)
      implicit none
      real * 8 x(2),w,ffm
      real * 8 p1(0:3),p2(0:3)
      common/momenta/p1,p2
      real * 8 pi
      parameter (pi=3.141592653589793d0)
      integer j
      real * 8 cth,sth,xjac,ph,beta2,beta
      cth=1-2*x(1)
      xjac=2
      sth=sqrt(1-cth**2)
      ph=pi*x(2)
c no jacobian, we divide by pi
      beta2=0
      do j=1,3
         beta2=beta2+(p1(j)/p1(0))**2
      enddo
      beta=sqrt(beta2)
      ffm=-2*xjac*log(sth*sin(ph))*(1-beta2)/(1-beta*cth)**2
      end

      function ffm0(x,w)
      implicit none
      real * 8 x(2),w,ffm0
      real * 8 p1(0:3),p2(0:3)
      common/momenta/p1,p2
      real * 8 pi
      parameter (pi=3.141592653589793d0)
      integer j
      real * 8 cth,sth,xjac,ph,beta2,beta
      cth=1-2*x(1)
      xjac=2
      sth=sqrt(1-cth**2)
      ph=pi*x(2)
c no jacobian, we divide by pi
      beta2=0
      do j=1,3
         beta2=beta2+(p1(j)/p1(0))**2
      enddo
      beta=sqrt(beta2)
      ffm0=xjac*(1-beta2)/(1-beta*cth)**2
      end




      function random()
      real * 8 random
      real * 8 saverandom
      logical fixed
      COMMON/tmpfixed/fixed
      data fixed/.false./
      save saverandom
      if(fixed) then
         random=saverandom
         return
      endif
      call rm48(random,1)
      saverandom=random
      end


      subroutine resetrandom
      call RM48IN(54217137,0,0)
      end



      function dotp(p1,p2)
      implicit none
      real * 8 dotp,p1(0:3),p2(0:3)
      dotp = (p1(0)*p2(0) - p1(3)*p2(3)) - p1(1)*p2(1) - p1(2)*p2(2)
      end
