      subroutine phi1_2m_nobw_ktmin(m2,x3,xth,xphi,s3min,p1,p2,p3,wt)
c     massive particle p1 decaying into p2 mass m2 and p3 minimum mass-squared s3min,
c     with invariant mass of particle three s3 integrated over.
c     vectors returned p2 and p3 are in the same frame as p1 is supplied
c     Expression evaluate is 
c     ds2 d^4 p2 d^4 p3 (2 pi)^4 delta(p1-p2-p3)/(2 pi)^6
c     delta(p2^2-s2) delta(p3^2-s3)
      implicit none
      include 'pwhg_math.h'
      double precision p1(4),p2(4),p3(4),p3cm(4)
      double precision x3,xth,xphi,costh,sinth,phi,cphi,sphi
      double precision wt,wt0,w3
      double precision s3max,s3min
      double precision m1,m2,m3,s1,s2,s3,lambda
      integer j
      parameter(wt0=1d0/8d0/pi)
      real * 8 z,mod_p2,sinth_min,th_min,max_costh

      wt=0d0
      s1=p1(4)**2-p1(1)**2-p1(2)**2-p1(3)**2  
      if (s1 .lt. 0d0) stop
      m1=dsqrt(s1)
      s2=m2**2
      s3max=(m1-m2)**2
      if (s3min .gt. s3max) stop
      w3=s3max-s3min
      s3=s3max*x3+s3min*(1d0-x3)

      m3=dsqrt(s3)
ccccccccccccccccccccccccccccccccccccccccccccccc
c     modified by Carlo Oleari
c     costh=2d0*xth-1d0     ! original line. Superseded by the following lines
c     mod_p2 is the modulus of the vector p2 or p3, one recoiling against the other
      mod_p2=sqrt((s1-s2-s3)**2-4d0*s2*s3)/(2*m1)
      sinth_min = kn_ktmin/mod_p2
      th_min = arcsin(sinth_min)
      max_costh = cos(th_min)
c      costh=2d0*xth-1d0      
      costh=(max_costh+1)*xth-1d0      
c     we divide by two, since this factor is already included in wt0
      w3 = w3/2*(max_costh+1)

c      z = 2d0*xth-1d0      
c      costh=1.5d0*(z-z**3/3)
c     w3 = w3*1.5d0*(1-z**2)
ccccccccccccccccccccccccccccccccccccccccccccccc


      phi=2d0*pi*xphi
      sinth=dsqrt(1d0-costh**2)
      cphi=dcos(phi)
      sphi=dsin(phi)
      lambda=((s1-s2-s3)**2-4d0*s2*s3)
      if (lambda .lt. 0d0) then
      write(6,*) 'lambda in phi1_2m',lambda
      write(6,*) 's1 in phi1_2m',s1
      write(6,*) 's2 in phi1_2m',s2
      write(6,*) 's3 in phi1_2m',s3
      write(6,*) 'm1 in phi1_2m',m1
      write(6,*) 'm2 in phi1_2m',m2
      write(6,*) 'm3 in phi1_2m',m3
      write(6,*) 'x3 in phi1_2m',x3
      stop
      endif
      lambda=dsqrt(lambda)
      wt=wt0*w3*lambda/s1

      

      p3cm(4)=m1/2d0*(s1+s3-s2)/s1
      p3cm(1)=m1/2d0*lambda/s1*sinth*sphi
      p3cm(2)=m1/2d0*lambda/s1*sinth*cphi
      p3cm(3)=m1/2d0*lambda/s1*costh
      call boost(m1,p1,p3cm,p3)
      do j=1,4
      p2(j)=p1(j)-p3(j)
      enddo
      if (  (p1(4) .lt. 0d0) 
     & .or. (p2(4) .lt. 0d0) 
     & .or. (p3(4) .lt. 0d0)) then  
      write(6,*) 'p1(4)',p1(4)
      write(6,*) 'p2(4)',p2(4)
      write(6,*) 'p3(4)',p3(4)
      write(6,*) 'p1sq',p1(4)**2-p1(1)**2-p1(2)**2-p1(3)**2,s1
      write(6,*) 'p2sq',p2(4)**2-p2(1)**2-p2(2)**2-p2(3)**2,s2
      write(6,*) 'p3sq',p3(4)**2-p3(1)**2-p3(2)**2-p3(3)**2,s3
      write(6,*) 'in phi1_2m.f'
      endif
      return
      end



