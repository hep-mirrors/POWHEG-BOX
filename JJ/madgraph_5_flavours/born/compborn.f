      subroutine compborn(p,flav,amp2)
      real * 8 p(0:3,1: 4)
      integer flav( 4)
      real * 8 amp2
      real * 8 madp(0:3,1: 4)
      integer madflav( 4),perm( 4)
      logical flavequiv_perm
      external flavequiv_perm
      integer i, mu

      madflav(1)=5
      madflav(2)=5
      madflav(3)=5
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbb_bb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_bbx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_gg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uux(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=0
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_gb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=2
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ub(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=4
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ub(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=1
      madflav(3)=1
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ub(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=3
      madflav(3)=3
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ub(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxb(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_bbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_gg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uux(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-5
      madflav(3)=-5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxbx_bxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=0
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_gbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ubx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=4
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ubx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=1
      madflav(3)=1
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ubx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=3
      madflav(3)=3
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ubx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=2
      madflav(4)=4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_uc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_uc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=4
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_uc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_uc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=4
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_uc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_uc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxc(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=4
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=4
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=0
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_gb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=0
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_gbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_bbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_uux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ug(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=4
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=2
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ub(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=4
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ub(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=5
      madflav(3)=1
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ub(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=5
      madflav(3)=3
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ub(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=2
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ubx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=4
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ubx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-5
      madflav(3)=1
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ubx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-5
      madflav(3)=3
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ubx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=2
      madflav(4)=4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_uc(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_uc(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_uc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=4
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_uc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=4
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_uc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_uc(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=2
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=4
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=4
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ug(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=4
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_uu(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=4
      madflav(3)=4
      madflav(4)=4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_uu(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_bbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_bbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_bbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_bbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_gg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_uux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_uux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=-2
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxb(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=-4
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=5
      madflav(3)=-1
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=5
      madflav(3)=-3
      madflav(4)=5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxb(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=-2
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=-4
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-5
      madflav(3)=-1
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-5
      madflav(3)=-3
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=-2
      madflav(4)=4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxc(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=-2
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxc(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=-2
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxc(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=-4
      madflav(4)=1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxc(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=-4
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxc(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=-1
      madflav(4)=3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxc(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=-2
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=-2
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=-2
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=-4
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=-4
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-1
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-2
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-4
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_bbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_bbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_bbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=5
      madflav(4)=-5
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_bbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=3
      madflav(4)=-3
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=2
      madflav(4)=-2
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=1
      madflav(4)=-1
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_gg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=0
      madflav(4)=0
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_uux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_uxux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=-4
      if (flavequiv_perm( 4,flav,madflav,perm)) then
      do i=1, 4 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
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
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_uxux(madp,amp2)
      return
      endif

      write(*,*) 'ERROR: the flavour list', flav
      write(*,*) 'is not in the list of MADGRAPH routines'
      call exit(1)
      end

