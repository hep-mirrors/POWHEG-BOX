      subroutine compreal(p,flav,amp2)
      real * 8 p(0:3,1: 5)
      integer flav( 5)
      real * 8 amp2
      real * 8 madp(0:3,1: 5)
      integer madflav( 5),perm( 5)
      logical flavequiv_perm
      external flavequiv_perm
      integer i, mu

      madflav(1)=5
      madflav(2)=5
      madflav(3)=0
      madflav(4)=5
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbb_gbb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_gbbx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_ggg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uuxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uuxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uuxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_uuxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=5
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_bbbx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=0
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_ggb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_uuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_uuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_uuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_uuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=2
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ugb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=4
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ugb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=1
      madflav(3)=1
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ugb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=3
      madflav(3)=3
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_ugb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxgb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxgb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxgb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_uxgb(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_gbbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_ggg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-5
      madflav(3)=0
      madflav(4)=-5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxbx_gbxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=5
      madflav(4)=-5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_bbxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=0
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_ggbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=2
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ugbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=4
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ugbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=1
      madflav(3)=1
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ugbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=3
      madflav(3)=3
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_ugbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_uuxc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_uuxc(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_uuxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_uuxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_uuxc(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_uuxc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=2
      madflav(4)=4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_ucg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=2
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_ucg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=4
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_ucg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=2
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_ucg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=4
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_ucg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=1
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_ucg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxcg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxcg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxcg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxcg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxcg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_uxcg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=4
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=4
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=1
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_ucxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=5
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_bbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=0
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_ggb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_uuxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_uuxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_uuxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_uuxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=5
      madflav(4)=-5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_bbxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=0
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_ggbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_uuxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_uuxc(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_uuxc(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_uuxc(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_uuxc(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_uuxc(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_uuxc(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_uuxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_gbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_ggg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_uuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_uuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_uuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_uuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=2
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ubbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=4
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ubbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=1
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ubbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=3
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ubbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=2
      madflav(4)=4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=2
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=2
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=4
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=4
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=1
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=2
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ugg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=4
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ugg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=1
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ugg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=3
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_ugg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=2
      madflav(4)=2
      madflav(5)=-2
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uuux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=4
      madflav(4)=4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uuux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=1
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uuux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=3
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uuux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-2
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uuxux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uuxux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uuxux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uuxux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxgg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=2
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ugb(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=4
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ugb(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=5
      madflav(3)=1
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ugb(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=5
      madflav(3)=3
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_ugb(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=2
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ugbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=4
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ugbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-5
      madflav(3)=1
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ugbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-5
      madflav(3)=3
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_ugbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=2
      madflav(4)=4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_ucg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=2
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_ucg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=2
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_ucg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=4
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_ucg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=4
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_ucg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=1
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_ucg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=2
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=2
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=2
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=4
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=4
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=1
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_ucxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ubbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=4
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ubbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=1
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ubbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=3
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ubbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=4
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uccx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=4
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uccx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=1
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ugg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=4
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ugg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=1
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ugg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=3
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_ugg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=2
      madflav(5)=-2
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=4
      madflav(4)=4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uuux(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=1
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uuux(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=3
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_uuux(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=2
      madflav(3)=2
      madflav(4)=2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_uug(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=4
      madflav(3)=4
      madflav(4)=4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_uug(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=1
      madflav(3)=1
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_uug(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=3
      madflav(3)=3
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_uug(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ccxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_gbbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_gbbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_gbbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_gbbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ggg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ggg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ggg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ggg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_uuxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_uuxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_uuxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=-2
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxgb(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=-4
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxgb(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=5
      madflav(3)=-1
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxgb(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=5
      madflav(3)=-3
      madflav(4)=0
      madflav(5)=5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_uxgb(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=-2
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=-4
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-5
      madflav(3)=-1
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-5
      madflav(3)=-3
      madflav(4)=0
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_uxgbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=-2
      madflav(4)=4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxcg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=-2
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxcg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=-2
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxcg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=-4
      madflav(4)=1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxcg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=-4
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxcg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=-1
      madflav(4)=3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_uxcg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=-2
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=-2
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=-2
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=-4
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=-4
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-1
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_uxcxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=-2
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uuxux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uuxux(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uuxux(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uuxux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-2
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-4
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-1
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-3
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxbbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-2
      madflav(4)=4
      madflav(5)=-4
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-2
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-2
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-4
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxccx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-4
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxccx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-1
      madflav(4)=3
      madflav(5)=-3
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-2
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxgg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-4
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxgg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-1
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxgg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-3
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxgg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ccxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_gbbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_gbbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_gbbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=0
      madflav(4)=5
      madflav(5)=-5
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_gbbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ggg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ggg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ggg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=0
      madflav(4)=0
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ggg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_uuxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-2
      madflav(3)=-2
      madflav(4)=-2
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_uxuxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-4
      madflav(3)=-4
      madflav(4)=-4
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_uxuxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-1
      madflav(3)=-1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_uxuxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-3
      madflav(3)=-3
      madflav(4)=-3
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_uxuxg(madp,amp2)
      return
      endif

      write(*,*) 'ERROR: the flavour list', flav
      write(*,*) 'is not in the list of MADGRAPH routines'
      call exit(1)
      end

