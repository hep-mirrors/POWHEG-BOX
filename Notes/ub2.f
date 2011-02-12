      function u(xx,w)
      implicit none
      real * 8 u,xx(2),w
      real * 8  csi,y,kt2
      real * 8 s,b0,lam2,pt2,csimax
      common/ccc/s,b0,lam2,pt2,csimax
      csi=xx(1)
      y=xx(2)
      kt2=s/2*csi**2*(1-y)
      if(kt2.lt.pt2) then
         u=0
         return
      endif
      u=1/(csi**2*(1-y))/(1-kt2/s/csi)**2
c      u=1/(csi**3*(1-y))/(1-kt2/s/csi)**3
      end


      implicit none
      real * 8 xl,xu,acc
      integer ndim,ncall,itmx,nprn
      common/bveg1/xl(10),xu(10),acc,ndim,ncall,itmx,nprn
      real * 8 avgi,sd,chi2a,exact,exact1
      real * 8 s,b0,lam2,pt2,csimax
      common/ccc/s,b0,lam2,pt2,csimax
      real * 8 xm,p
      real * 8 u,ddilog
      external u,ddilog
      s=200
      b0=32
      lam2=0.2
      pt2=3.2
      csimax=0.4
      xl(1)=0
      xl(2)=-1
      xu(1)=csimax
      xu(2)=1
      ndim=2
      ncall=100000
      itmx=10
      nprn=1
      
      call vegas(u,avgi,sd,chi2a)

      xm=csimax
      p=sqrt(pt2/s)
      exact=(log(xm-xm**2)+(2*xm-2)*log(xm)-2*log(1-xm)*xm-2)/xm/2.d+0
     1   -(p*log(xm-p**2)+(2*p*log(p)-2*log(1-p)*p-2)*xm-2*p*log(p))
     2   /(p*xm)/2.d+0
      exact=exact*2

c      exact=
c     12*((2*log(k)*log(xm-k**2)-log(k)**2)/2.d+0-(2*log(k)*log(1-k**2/xm
c     1   )+ddilog(k**2/xm))/2.d+0-log(1-k)*log(k)-ddilog(1-k))

      write(*,*) ' exact/vegas',exact/avgi,'+-',sd/exact

      end

      

