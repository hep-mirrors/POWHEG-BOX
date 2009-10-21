      implicit none
      real * 8 k1(0:3),k2(0:3),q2,s,xicut,n1,n2
      real * 8 intf,intf0,random
      integer j
      external intf,intf0,random
 1    continue
      n1=0
      n2=0
      do j=1,3
         k1(j)=random()
         k2(j)=random()
         n1=n1+k1(j)**2
         n2=n2+k2(j)**2
      enddo
      k1(0)=sqrt(n1)
      k2(0)=sqrt(n2)
      q2=random()
      s=random()
      xicut=random()
      write(*,*) intf(k1,k2,q2,s,xicut)/intf0(k1,k2,q2,s,xicut)
      goto 1
      end


c I_0(k,m)
      function Int0(k,m)
      implicit none
      real * 8 Int0, k(0:3),m(0:3)
      real * 8 kh(0:3),mh(0:3)
      integer j
      real * 8 dotp
      external ddilog,dotp
      do j=0,3
         kh(j)=k(j)/k(0)
         mh(j)=m(j)/m(0)
      enddo
      Int0=log( dotp(kh,mh)**2/dotp(mh,mh) )
      end


c I_ep(k,m)
      function Intep(k,m)
      implicit none
      real * 8 Intep, k(0:3),m(0:3)
      real * 8 kh(0:3),mh(0:3),khmh,b
      integer j
      real * 8 ddilog,dotp
      external ddilog,dotp
      do j=0,3
         kh(j)=k(j)/k(0)
         mh(j)=m(j)/m(0)
      enddo
      b=sqrt(1-dotp(mh,mh))
      khmh=dotp(kh,mh)
      Intep=-2*(log((1-b)/(1+b))**2/4+log(khmh/(1+b))*log(khmh/(1-b))
     # +ddilog(1-khmh/(1+b))+ddilog(1-khmh/(1-b)))
      end


      function Intf(k1,k2,q2,s,xic)
      implicit none
      real * 8 Intf,k1(0:3),k2(0:3),q2,s,xic
      real * 8 m(0:3)
      real * 8 pi
      parameter (pi=3.141592653589793d0)
      integer j
      real * 8 Int0,Intep
      do j=0,3
         m(j)=k1(j)+k2(j)
      enddo
      Intf=-pi**2/6
     # +1d0/2*log(q2/(s*xic**2))**2-1d0/2*
     # (Int0(k1,m)+Int0(k2,m))*log(q2/(s*xic**2))
     # -1d0/2*(Intep(k1,m)+Intep(k2,m))
      end


      function intf0(k1,k2,q2,s,xicut)
      implicit none
      real * 8 intf0,k1(0:3),k2(0:3),q2,s,xicut
      real * 8 eij,kij
      real * 8 ddilog,dotp
      external ddilog,dotp
      kij=dotp(k1,k2)
      eij=k1(0)*k2(0)
      intf0=1d0/2*log(xicut**2*s/q2)**2
     #          +log(xicut**2*s/q2)*log(kij/(2*eij))
     #          -ddilog(kij/(2*eij))+1d0/2*log(kij/(2*eij))**2
     #          -log(1-kij/(2*eij))*log(kij/(2*eij))
      
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
      dotp = p1(0)*p2(0) - p1(1)*p2(1) - p1(2)*p2(2) - p1(3)*p2(3)
      end

