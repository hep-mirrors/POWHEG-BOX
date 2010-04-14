      subroutine compborn(p,flav,amp2)
      real * 8 p(0:3,1: 4)
      integer flav( 4)
      real * 8 amp2
      real * 8 madp(0:3,1: 4)
      integer madflav( 4),perm( 4)
      logical flavequiv_perm
      external flavequiv_perm
      integer i, mu

      madflav(1)=1
      madflav(2)=2
      madflav(3)=2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdu_ud(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdu_ud(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdu_ud(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdux_uxd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdux_uxd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdux_uxd(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdxu_udx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdxu_udx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdxu_udx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdxux_uxdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdxux_uxdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sdxux_uxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgg_gg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgg_uux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgg_uux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgg_uux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=2
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgu_ug(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=1
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgu_ug(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=3
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgu_ug(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgux_uxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgux_uxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sgux_uxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sud_ud(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sud_ud(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sud_ud(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sudx_udx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sudx_udx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sudx_udx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sug_ug(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=1
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sug_ug(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=3
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Sug_ug(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=2
      madflav(3)=2
      madflav(4)=2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suu_uu(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=1
      madflav(3)=1
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suu_uu(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=3
      madflav(3)=3
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suu_uu(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_ddx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_ddx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_ddx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_ddx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_ddx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_ddx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_gg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_gg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_gg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_uux(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_uux(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suux_uux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=-2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxd_uxd(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=-2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxd_uxd(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=-1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxd_uxd(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=-2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxdx_uxdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=-2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxdx_uxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxdx_uxdx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-2
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxg_uxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-1
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxg_uxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-3
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxg_uxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_ddx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_ddx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_ddx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_ddx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_ddx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_ddx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_gg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_gg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_gg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_uux(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_uux(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxu_uux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxux_uxux(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxux_uxux(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,i)=p(mu,perm(i))
         enddo
      enddo

      call Suxux_uxux(madp,amp2)
      return
      endif

      write(*,*) 'ERROR: the flavour list', flav
      write(*,*) 'is not in the list of MADGRAPH routines'
      call exit(1)
      end

