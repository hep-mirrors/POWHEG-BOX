      function u(xx,w)
      implicit none
      real * 8 u,xx(2),w
      real * 8  csi,y,kt2
      real * 8 s,b0,lam2,pt2
      common/ccc/s,b0,lam2,pt2
      csi=xx(1)
      y=xx(2)
      kt2=s/2*csi**2*(1-y)
      if(kt2.lt.pt2) then
         u=0
         return
      endif
      u=1/(csi*(1-y))*1/b0/log(kt2/lam2)
      end

      implicit none
      real * 8 xl,xu,acc
      integer ndim,ncall,itmx,nprn
      common/bveg1/xl(10),xu(10),acc,ndim,ncall,itmx,nprn
      real * 8 csimax,avgi,sd,chi2a,exact,exact1
      real * 8 s,b0,lam2,pt2
      common/ccc/s,b0,lam2,pt2
      real * 8 u
      external u
      s=100
      b0=32
      lam2=0.2
      pt2=4
      csimax=0.7
      xl(1)=0
      xl(2)=-1
      xu(1)=csimax
      xu(2)=1
      ndim=2
      ncall=100000
      itmx=10
      nprn=1
      
      call vegas(u,avgi,sd,chi2a)

      exact=1/(2*b0)*( log(csimax**2*s/lam2)*log(log(csimax**2*s/lam2)/
     # log(pt2/lam2))-log(csimax**2*s/pt2))

      exact1=1/(2*b0)*(log(csimax**2*s/lam2)*(log(log(csimax**2*s/lam2))
     # -1 ) -   log(pt2/lam2) * (log(log(pt2/lam2))-1)
     # - log(csimax**2*s/pt2)*log(log(pt2/lam2))  )

      write(*,*) ' exact/vegas',exact1/avgi,'+-',sd/exact,exact/exact1

      end

