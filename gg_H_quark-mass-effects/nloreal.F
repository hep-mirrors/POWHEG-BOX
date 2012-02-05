      subroutine agg(s,t,u,agg2)
      implicit none
      double precision s,t,u,agg2
      double complex bdva2, bdva4

      agg2 = abs(bdva2(s,t,u))**2+abs(bdva2(u,s,t))**2+
     &       abs(bdva2(t,u,s))**2+abs(bdva4(s,t,u))**2


      end

      subroutine aqqbar(s,aqqbar2)
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      double precision aqqbar2
      double complex ampl
      double precision s,t,u,realfac
      double precision m0,m12,y0,y12
      double precision s0,t0,u0,s12,t12,u12
      double complex bdvd12,bdvd0,fermion,scalar
      integer i

      ampl = dcmplx(0d0)

      do i=1,afer
         m12 = mfer(i)
         s12 = s/m12**2
         y12 = m12**2/mh2
         fermion = lambdafer(i)*trfer(i)*y12*bdvd12(s12,y12)
         ampl = ampl + fermion
      end do

      if (flg_lhscalars.ne.1) then
       do i=1,asca
        m0 = msca(i)
        s0 = s/m0**2
        y0 = m0**2/mh2
        scalar = lambdasca(i)*(mmaa**2/m0**2)*trsca(i)*y0*bdvd0(s0,y0)
        ampl = ampl + scalar
       end do
      else
         realfac = -(mh2-s)/(2*(mh2))
         do i=1,asca
            m0 = msca(i)
            y0 = m0**2/mh2
            scalar = realfac*lambdasca(i)*trsca(i)*(mmaa/m0)**2*
     &           (-1d0/3d0)
            ampl = ampl + scalar
         end do
      endif

      aqqbar2 = abs(ampl)**2

      end

      double complex function bdva2(s,t,u)
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      double precision s,t,u,m0,m12,realfac
      double precision y0,y12
      double precision s0,t0,u0,s12,t12,u12
      double complex bdvb12,bdvb0,fermion,scalar
      external bdvb12,bdvb0
      integer i

      bdva2 = dcmplx(0d0)
      do i=1,afer
         m12 = mfer(i)
         s12 = s/m12**2
         t12 = t/m12**2
         u12 = u/m12**2
         y12 = m12**2/mh2

         fermion = lambdafer(i)*trfer(i)*y12**2*
     &        (bdvb12(s12,t12,u12,y12)+bdvb12(s12,u12,t12,y12))

         bdva2 = bdva2 + fermion
      end do

      if (flg_lhscalars.ne.1) then
         do i=1,asca
            m0 = msca(i)
            s0 = s/m0**2
            t0 = t/m0**2
            u0 = u/m0**2
            y0 = m0**2/mh2
            scalar = lambdasca(i)*(mmaa**2/m0**2)*trsca(i)*y0**2*
     &           (bdvb0(s0,t0,u0,y0)+bdvb0(s0,u0,t0,y0))

            bdva2 = bdva2 + scalar
         end do
      else
         realfac = s**2/(4*(s+t+u)**2)
         do i=1,asca
            m0 = msca(i)
            y0 = m0**2/mh2
            scalar = realfac*lambdasca(i)*trsca(i)*(mmaa/m0)**2*
     &           (-1d0/3d0)
            bdva2 = bdva2 + scalar
         end do
      endif

      return
      end

      double complex function bdva4(s,t,u)
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      double precision s,t,u,m0,m12,y0,y12,realfac
      double precision s0,t0,u0,s12,t12,u12
      double complex bdvc12,bdvc0,fermion,scalar
      external bdvc12,bdvc0
      integer i

      bdva4 = dcmplx(0d0)

      do i=1,afer
      m12 = mfer(i)
      s12 = s/m12**2
      t12 = t/m12**2
      u12 = u/m12**2
      y12 = m12**2/mh2
      fermion = lambdafer(i)*trfer(i)*y12**2*
     &     (bdvc12(s12,t12,u12,y12)+bdvc12(t12,u12,s12,y12)+
     &      bdvc12(u12,s12,t12,y12))

      bdva4 = bdva4 + fermion
      end do


      if (flg_lhscalars.ne.1) then
         do i=1,asca
            m0 = msca(i)
            s0 = s/m0**2
            t0 = t/m0**2
            u0 = u/m0**2
            y0 = m0**2/mh2
            scalar = lambdasca(i)*(mmaa**2/m0**2)*trsca(i)*y0**2*
     &      (bdvc0(s0,t0,u0,y0)+bdvc0(t0,u0,s0,y0)+bdvc0(u0,s0,t0,y0))

            bdva4 = bdva4 + scalar
         end do
      else
         realfac = 1d0/4d0
         do i=1,asca
            m0 = msca(i)
            y0 = m0**2/mh2
            scalar = realfac*lambdasca(i)*trsca(i)*(mmaa/m0)**2*
     &           (-1d0/3d0)
            bdva4 = bdva4 + scalar
         end do
      endif

      return
      end



      double complex function bdvb12(s,t,u,y12)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,y12,m2
      double complex xs,xt,x12
      double complex bdvbigb12, bdvh3, reduced
      external bdvbigb12, bdvh3, reduced

      x12 = reduced(1d0/y12)
      xs = reduced(s)
      xt = reduced(t)
      m2 = y12*mh2

      bdvb12= bdvbigb12(s,t,u,y12) +
     &       s/4d0*(0.5d0*log(x12)**2-0.5d0*log(xs)**2)
     &       -(s/2d0-s**2/(s+u))*(0.5d0*log(x12)**2-0.5d0*log(xt)**2)
     &       -s/8d0*bdvh3(s,u,t,m2)+s/4d0*bdvh3(t,s,u,m2)


      return
      end

      double complex function bdvb0(s,t,u,y0)
      implicit none
      double precision s,t,u,y0
      double complex bdvbigb0
      external bdvbigb0

      bdvb0= -0.5d0*bdvbigb0(s,t,u,y0) 

      return
      end


      double complex function bdvbigb12(s,t,u,y12)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,m12, y12, m2
      double complex xs,xt,x12,r1,r2
      double complex bdvh3, reduced
      external bdvh3, reduced

      x12 = reduced(1d0/y12)
      xs = reduced(s)
      xt = reduced(t)
      m2 = y12*mh2

      if (y12.lt.0.25d0) then
         r1 = dsqrt(1d0-4d0*y12)
      else
         r1 = cdsqrt(dcmplx(1d0-4d0*y12))
      end if
      if ((t.lt.0.d0).or.(t.gt.4d0)) then
         r2 = dsqrt(1d0-4d0/t)
      else
         r2 = cdsqrt(dcmplx(1d0-4d0/t))
      end if


      bdvbigb12= s*(t-s)/(s+t) +
     &     2d0*(t*u**2+2d0*s*t*u)/(s+u)**2*(r1*log(x12)-r2*log(xt))
     &     -(1d0+t*u/s)*0.5d0*log(x12)**2+0.5d0*log(xs)**2
     &     -2d0*(2d0*s**2/(s+u)**2 -1d0-t*u/s)*
     &                0.5d0*(log(x12)**2-log(xt)**2)
     &     +0.5d0*(t*u/s+3d0)*bdvh3(s,u,t,m2)-bdvh3(t,s,u,m2)


      return
      end

      double complex function bdvbigb0(s,t,u,y0)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u, m0,y0,m2
      double complex xs,xt,x0, r1, r2
      double complex bdvh3, reduced
      external bdvh3, reduced

      x0 = reduced(1d0/y0)
      xs = reduced(s)
      xt = reduced(t)
      m2 = y0*mh2

      if (y0.lt.0.25d0) then
         r1 = dsqrt(1d0-4d0*y0)
      else
         r1 = cdsqrt(dcmplx(1d0-4d0*y0))
      end if
      if ((t.lt.0.d0).or.(t.gt.4d0)) then
         r2 = dsqrt(1d0-4d0/t)
      else
         r2 = cdsqrt(dcmplx(1d0-4d0/t))
      end if


      bdvbigb0= s*(t-s)/(s+t) 
     &    +2d0*(t*u**2+2d0*s*t*u)/(s+u)**2*(r1*log(x0)-r2*log(xt))
     &     -(1d0+t*u/s)*0.5d0*log(x0)**2+0.5d0*log(xs)**2
     &     -2d0*(2d0*s**2/(s+u)**2 -1d0-t*u/s)*
     &                0.5d0*(log(x0)**2-log(xt)**2)
     &     +0.5d0*(t*u/s+3d0)*bdvh3(s,u,t,m2)-bdvh3(t,s,u,m2)

      return
      end


      double complex function bdvc12(s,t,u,y12)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,m12, y12, m2
      double complex xs,x12
      double complex bdvbigc12, bdvh3, reduced
      external bdvbigc12, bdvh3, reduced

      x12 = reduced(1d0/y12)
      xs = reduced(s)
      m2 = y12*mh2

      bdvc12= bdvbigc12(s,t,u,y12) +
     &    0.5d0/y12*0.5d0*(log(x12)**2-log(xs)**2)+
     &    0.25d0/y12*bdvh3(s,u,t,1d0)

      return
      end

      double complex function bdvc0(s,t,u, y0)
      implicit none
      double precision s,t,u, y0
      double complex bdvbigc0
      external bdvbigc0

      bdvc0 = -0.5d0*bdvbigc0(s,t,u, y0)

      return
      end



      double complex function bdvbigc12(s,t,u,y12)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,y12,m2
      double complex xs,x12
      double complex bdvh3,reduced
      external bdvh3, reduced

      x12 = reduced(1d0/y12)
      xs = reduced(s)
      m2 = y12*mh2

      bdvbigc12 = -2d0*s-2d0*0.5d0*(log(x12)**2-log(xs)**2)-
     &            bdvh3(u,s,t,1d0)

      return
      end

      double complex function bdvbigc0(s,t,u, y0)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,y0,m2
      double complex xs,x0
      double complex bdvh3, reduced
      external bdvh3, reduced

      x0 = reduced(1d0/y0)
      xs = reduced(s)
      m2 = y0*mh2
      bdvbigc0 = -2d0*s-2d0*0.5d0*(log(x0)**2-log(xs)**2)-
     &            bdvh3(u,s,t,1d0)
      
      return
      end


      double complex function bdvd12(s,y12)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,m12, y12, m2
      double complex xs,x12
      double complex bdvbigd12, reduced
      external bdvbigd12, bdvh3, reduced
      
      x12 = reduced(1d0/y12)
      xs = reduced(s)
      m2 = y12*mh2

      bdvd12= bdvbigd12(s,y12) -
     &        2d0*(0.5d0*(log(x12)**2-log(xs)**2))


      return
      end

      double complex function bdvd0(s, y0)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u, y0
      double complex bdvbigd0
      external bdvbigd0

      bdvd0 = -0.5d0*bdvbigd0(s, y0)

      return
      end



      double complex function bdvbigd12(s,y12)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,y12,m2
      double complex xs,x12,r1,r2
      double complex reduced
      external reduced

      x12 = reduced(1d0/y12)
      xs = reduced(s)
      m2 = y12*mh2

      if (y12.lt.0.25d0) then
         r1 = dsqrt(1d0-4d0*y12)
      else
         r1 = cdsqrt(dcmplx(1d0-4d0*y12))
      end if
      if ((s.lt.0.d0).or.(s.gt.4d0)) then
         r2 = dsqrt(1d0-4d0/s)
      else
         r2 = cdsqrt(dcmplx(1d0-4d0/s))
      end if

      bdvbigd12 = 4d0 + 4d0*s/(1d0/y12-s)*(r1*log(x12)-r2*log(xs))+
     &            8d0/(1d0/y12-s)*0.5d0*(log(x12)**2-log(xs)**2)

      return
      end

      double complex function bdvbigd0(s, y0)
      implicit none
      include 'PhysPars.h'
      double precision s,t,u,y0,m2
      double complex xs,x0,r1,r2
      double complex bdvh3, reduced
      external bdvh3, reduced

      x0 = reduced(1d0/y0)
      xs = reduced(s)
      m2 = y0*mh2

      if (y0.lt.0.25d0) then
         r1 = dsqrt(1d0-4d0*y0)
      else
         r1 = cdsqrt(dcmplx(1d0-4d0*y0))
      end if
      if ((s.lt.0.d0).or.(s.gt.4d0)) then
         r2 = dsqrt(1d0-4d0/s)
      else
         r2 = cdsqrt(dcmplx(1d0-4d0/s))
      end if

      bdvbigd0  = 4d0 + 4d0*s/(1d0/y0-s)*(r1*log(x0)-r2*log(xs))+
     &            8d0/(1d0/y0-s)*0.5d0*(log(x0)**2-log(xs)**2)

      return
      end



      double complex function bdvh3(s,t,u,m2)
      implicit none
      double precision s,t,u,m2
      double complex bdvi3,w3
      external bdvi3,w3

      bdvh3 = -w3(t,s,u,(s+t+u), 1d0)

c      if (1>0) print*,'h3=',bdvh3,s,t,u
c      bdvh3 = dcmplx(0d0)

c      bdvh3 = bdvi3(s,t,u,t) + bdvi3(s,t,u,u) - bdvi3(s,t,u,s+t+u)

      return
      end

      double complex function w3(s,t,u,v,m2)
      implicit none
      double precision s,t,u,v,xf,xh,tau,m2
      double complex ei3
      external ei3

      w3 = ei3(s,t,u,v,m2) -ei3(s,t,u,s,m2) - ei3(s,t,u,u,m2)
c      w3 = dcmplx(0d0)

      return
      end


c      subroutine eei3(s,t,u,varg,mbsq,ei3)
      double complex function ei3(s,t,u,varg,mbsq)
C     ehsv:EqnA.21
      implicit none
c      double complex ei3
      double precision pi
      double precision s,t,u,varg,rat,al,be,ga,r,theta,phi,cosphi
      double precision arg1,arg2,arg3,arg4,ddilog,mbsq
      double complex eli2,zth,zph,CLI2

      external cli2,eli2

      pi = 3.141592653589793d0
      rat=4d0/varg
      if (rat .lt. 0d0) then

           be=0.5d0*(1d0+dsqrt(1d0+4d0*t/(u*s)))
           ga=0.5d0*(1d0+dsqrt(1d0-rat))
           arg1=ga/(ga+be-1d0)
           arg2=(ga-1d0)/(ga+be-1d0)
           arg3=(be-ga)/be
           arg4=(be-ga)/(be-1d0)
           ei3=2d0/(2d0*be-1d0)
     .     *(-ddilog(arg1)+ddilog(arg2)+ddilog(arg3)-ddilog(arg4)
     .     +0.5d0*(dlog(be)**2-dlog(be-1d0)**2)
     .     +dlog(ga)*dlog((ga+be-1d0)/be)
     .     +dlog(ga-1d0)*dlog((be-1d0)/(ga+be-1d0)))
      elseif (rat .gt. 1d0) then
           be=0.5d0*(1d0+dsqrt(1d0+4d0*t/(u*s)))
           al=dsqrt(rat-1d0)
           r=dsqrt((al**2+1d0)/(al**2+(2d0*be-1d0)**2))
           cosphi=r*(al**2+2d0*be-1d0)/(1d0+al**2)
           if (cosphi.gt.1) then
              if (cosphi.lt.1.000001) then
                 write(18,*) "Detected rounding error - cosphi: "
                 write(18,*) cosphi
                 cosphi = 1.d0
              else
                 write(18,*) "Rounding error seems too large! Aborting!"
                 write(18,*) cosphi
                 stop
              endif
           endif
           phi=acos(cosphi)
           theta=acos(r*(al**2-2d0*be+1d0)/(1d0+al**2))
           zth=r*dcmplx(cos(theta),sin(theta))
           zph=r*dcmplx(cos(phi),sin(phi))
           ei3=2d0/(2d0*be-1d0)
     .     *(2d0*dble(eli2(zth))-2d0*dble(eli2(zph))
     .     +(phi-theta)*(phi+theta-pi))
      else
           be=0.5d0*(1d0+dsqrt(1d0+4d0*t/(u*s)))
           ga=0.5d0*(1d0+dsqrt(1d0-rat))
           arg1=ga/(ga+be-1d0)
           arg2=(ga-1d0)/(ga+be-1d0)
           arg3=ga/(ga-be)
           arg4=(ga-1d0)/(ga-be)
      
           ei3=2d0/(2d0*be-1d0)
     .     *(-ddilog(arg1)+ddilog(arg2)+ddilog(arg3)-ddilog(arg4)
     .     +dlog(ga/(1d0-ga))*dlog((ga+be-1d0)/(be-ga))
     .     -dcmplx(0d0,1d0)*pi*dlog((ga+be-1d0)/(be-ga)))
      endif

      return
      end
