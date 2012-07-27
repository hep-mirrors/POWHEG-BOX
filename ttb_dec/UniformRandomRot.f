      program testuniform
c this test is based upon the fact that the fraction of points
c falling at a certain distance from a given matrix is independent
c from the given matrix.
      implicit none
      real * 8 r(3,3),r0(3,3),distmat,res
      integer j,n
      parameter (n=100000000)
c First try the indentity
      r0(1,1)=1
      r0(2,2)=1
      r0(3,3)=1
 1    continue
      res=0
      do j=1,n
         call uniformrot(r)
         if(distmat(r,r0).lt.0.2) then
            res=res+1
         endif
      enddo
      write(*,*) res/n, '+-',sqrt(res)/n
      call uniformrot(r0)
      goto 1
      end

      function distmat(r1,r2)
      implicit none
      real * 8 distmat,r1(3,3),r2(3,3)
      real * 8 dist
      integer j,k
      dist = 0
      do j=1,3
         do k=1,3
            dist = dist + (r1(j,k)-r2(j,k))**2
         enddo
      enddo
      distmat = dist
      end
      
      subroutine uniformrot(R)
      implicit none
c     Generate a uniformly distributed rotation
      real * 8 r(3,3)
      real * 8 pi
      parameter (pi=3.141592653589793d0)
      real * 8 costh,sinth,phi,gamma,sing,cosg,norm
      real * 8 random
      external random
      costh=2*random()-1
      sinth=sqrt(abs(1-costh**2))
      phi=2*pi*random()
c First axis in random direction
      r(1,1)=costh
      r(2,1)=sinth*sin(phi)
      r(3,1)=sinth*cos(phi)
c now pick a vector orthogonal to the first axis
      if(costh.gt.0.5d0) then
         norm=sqrt(r(1,1)**2+r(2,1)**2)
         r(1,2)=r(2,1)/norm
         r(2,2)=-r(1,1)/norm
         r(3,2)=0
      else
         norm=sqrt(r(2,1)**2+r(3,1)**2)
         r(1,2)=0
         r(2,2)=r(3,1)/norm
         r(3,2)=-r(2,1)/norm
      endif
c Now totate r(:,2) around r(:,1) of an arbitrary angle
      gamma = 2*pi * random()
      sing = sin(gamma)
      cosg = cos(gamma)
      call mrotate(r(:,1),sing,cosg,r(:,2))
c Last axis is cross product of 1 and 2
      r(1,3)=r(2,1)*r(3,2)-r(3,1)*r(2,2)
      r(2,3)=r(3,1)*r(1,2)-r(1,1)*r(3,2)
      r(3,3)=r(1,1)*r(2,2)-r(2,1)*r(1,2)
      end



