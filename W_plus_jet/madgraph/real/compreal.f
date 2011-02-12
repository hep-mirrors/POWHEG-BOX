      subroutine compreal(p,flav,amp2)
      real * 8 p(0:3,1: 6)
      integer flav( 6)
      real * 8 amp2
      real * 8 madp(0:3,1: 6)
      integer madflav( 6),perm( 6)
      logical flavequiv_perm
      external flavequiv_perm
      integer i, mu

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbc_epvebb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbc_epvedb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbc_epvesb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbdx_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbdx_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbsx_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbsx_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_epvebb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_epvedb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_epvesb(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxbx_epvecxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxbx_epveuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epvedbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epvegg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epvesbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxc_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxcx_epvecxcx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxcx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxcx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxcx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxd_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxd_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxdx_epvecxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxdx_epvecxdx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxdx_epvecxdx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxdx_epveuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epvecxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epveuxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxsx_epvecxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxsx_epveuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epvedbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epvegg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epvesbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epveucx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_epveuxux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scb_epvebb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scb_epvedb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scb_epvesb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epvedbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epvegg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epvesbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scbx_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scc_epvecb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scc_epvecd(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scc_epvecs(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scd_epvedd(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epveddx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epvegg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scdx_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_epvebg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_epvedg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_epvesg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scs_epveds(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scs_epvess(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epvedsx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epvegg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epvessx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scsx_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epvecb(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epvecb(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epvecb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epvecd(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epvecs(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epvecs(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epveub(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epveub(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epveub(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epveud(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epveus(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epveus(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxbx_epvecxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxbx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxbx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxbx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxdx_epvecxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxdx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxdx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxsx_epvecxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxsx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxsx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdbx_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdbx_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdc_epvedd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdsx_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdu_epvedd(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxb_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxb_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxbx_epvecxbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxbx_epvecxdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxbx_epvecxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxbx_epveuxbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epveddx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epvegg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxc_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxcx_epvecxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxcx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxcx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxdx_epvecxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxdx_epveuxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epvecxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epveuxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxs_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxsx_epvecxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxsx_epvecxsx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epveddx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epvegg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epveucx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxux_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxux_epveuxux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epvecxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epveuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_epvebg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_epvedg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_epvesg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epvecxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epveuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgsx_epvecxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgsx_epveuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epvebg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epvedg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epvesg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssc_epveds(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssc_epvess(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssdx_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sssx_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sssx_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssu_epveds(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssu_epvess(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxb_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxb_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxbx_epvecxbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxbx_epveuxbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epvedsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epvegg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epvessx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxc_epveuxc(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxcx_epvecxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxcx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxcx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxd_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxdx_epvecxdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxdx_epvecxsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxg_epvecxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxg_epveuxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxs_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxs_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxsx_epvecxsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxsx_epveuxsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epveccx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epvedsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epvegg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epvessx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epveucx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxu_epveuux(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxux_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxux_epveuxux(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_epvebb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_epvedb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_epvesb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epvedbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epvegg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epvesbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epveucx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epvecb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epvecb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epvecb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epvecd(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epvecs(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epvecs(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epveub(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epveub(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epveub(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epveud(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epveus(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epveus(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sud_epvedd(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epveddx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epvegg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epveucx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epvebg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epvedg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epvesg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sus_epveds(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sus_epvess(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epvebbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epveccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epvedsx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epvegg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epvessx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epveucx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Susx_epveuux(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_epveub(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_epveud(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_epveus(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_epveuxux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxdx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxdx_epveuxux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxsx_epveuxcx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxsx_epveuxux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epvecxb(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epvecxd(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epvecxs(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epveuxb(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epveuxd(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=12
      madflav(5)=-2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epveuxs(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbb_emvexcb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbb_emvexub(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbc_emvexcc(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbc_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbc_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbc_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexdxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexsxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbcx_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbd_emvexcb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbd_emvexcd(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbd_emvexcd(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbd_emvexub(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbdx_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbdx_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_emvexcg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_emvexug(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbs_emvexcb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbs_emvexub(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_emvexuu(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexdxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexsxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_emvexuxc(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxcx_emvexbxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxcx_emvexdxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxcx_emvexsxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxd_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxd_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxs_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxs_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_emvexbxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_emvexdxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_emvexsxbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scb_emvexcc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scb_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scb_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scb_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sccx_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scd_emvexcc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scd_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scd_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scs_emvexcc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scs_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scs_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexdxb(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexsxb(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxb_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxbx_emvexbxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxbx_emvexdxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxbx_emvexsxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxc_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxcx_emvexcxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxcx_emvexcxdx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxcx_emvexcxsx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexddx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxd_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxdx_emvexdxdx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_emvexbxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_emvexdxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_emvexsxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexdxs(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexssx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxs_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxsx_emvexdxsx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxsx_emvexsxsx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexcxbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexcxbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexcxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexcxdx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexcxsx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexcxsx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexuxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexuxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexuxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexuxdx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexuxsx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_emvexuxsx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdb_emvexcb(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdb_emvexcd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdb_emvexcd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdb_emvexub(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdbx_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdbx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdc_emvexcc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdc_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdc_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexddx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdcx_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdd_emvexcd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdd_emvexud(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_emvexcg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_emvexug(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sds_emvexcd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sds_emvexcs(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdsx_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdu_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdu_emvexuu(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_emvexddx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_emvexuxc(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxb_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxb_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxcx_emvexdxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxs_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxux_emvexdxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_emvexcg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_emvexug(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_emvexbxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_emvexdxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_emvexsxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_emvexcg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_emvexug(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgs_emvexcg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgs_emvexug(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_emvexbxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_emvexdxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_emvexsxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssb_emvexcb(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssb_emvexub(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssbx_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssbx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssc_emvexcc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssc_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssc_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexdxs(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexssx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexucx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sscx_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssd_emvexcd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssd_emvexcs(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssdx_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssg_emvexcg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssg_emvexug(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sss_emvexcs(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sss_emvexus(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sssx_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sssx_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssu_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssu_emvexuu(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexdxs(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexssx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssux_emvexuxc(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxcx_emvexdxsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxcx_emvexsxsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxd_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxs_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxs_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxux_emvexdxsx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxux_emvexsxsx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_emvexuu(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sud_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sud_emvexuu(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sus_emvexuc(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sus_emvexuu(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexdxb(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexsxb(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_emvexuxc(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_emvexbxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_emvexdxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_emvexsxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexcxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexcxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexcxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexcxdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexcxsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexcxsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexuxbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexuxbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexuxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexuxdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexuxsx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_emvexuxsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_emvexddx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_emvexuxc(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxdx_emvexdxdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-5
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_emvexbxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_emvexdxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_emvexsxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=5
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexbbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=1
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexdxs(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=0
      madflav(6)=0
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexgg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexssx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-2
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexuux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=4
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxs_emvexuxc(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-1
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxsx_emvexdxsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-3
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxsx_emvexsxsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexcbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexcdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=4
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexcsx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexubx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexudx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_emvexusx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-5
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_emvexuxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-1
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_emvexuxdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-2
      madflav(3)=11
      madflav(4)=-12
      madflav(5)=-2
      madflav(6)=-3
      if (flavequiv_perm( 6,flav,madflav,perm)) then
      do i=1, 6 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_emvexuxsx(madp,amp2)
      return
      endif

      write(*,*) 'ERROR: the flavour list', flav
      write(*,*) 'is not in the list of MADGRAPH routines'
      call exit(1)
      end



