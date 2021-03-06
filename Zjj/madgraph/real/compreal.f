      subroutine compreal(p,flav,amp2)
      double precision p(0:3,1: 7)
      integer flav( 7)
      double precision amp2
      double precision madp(0:3,1: 7)
      integer madflav( 7),perm( 7)
      logical flavequiv_perm
      external flavequiv_perm
      integer i, mu

      madflav(1)=5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbb_epembbg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epemggg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbbx_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbd_epemdbg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbd_epemdbg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbdx_epemdxbg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbdx_epemdxbg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_epembbbx(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_epembgg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_epemddxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_epemddxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_epemuuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbg_epemuuxb(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_epemubg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbu_epemubg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_epemuxbg(madp,amp2)
      return
      endif

      madflav(1)=5
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbux_epemuxbg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epemggg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxb_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxbx_epembxbxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxd_epemdbxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxd_epemdbxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxdx_epemdxbxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxdx_epemdxbxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epembbxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-5
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epembxgg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epemddxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epemddxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epemuuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxg_epemuuxbx(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epemubxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxu_epemubxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_epemuxbxg(madp,amp2)
      return
      endif

      madflav(1)=-5
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sbxux_epemuxbxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scg_epemuuxc(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scu_epemucg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scux_epemuxcg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxg_epemuuxcx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxu_epemucxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Scxux_epemuxcxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdb_epemdbg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdb_epemdbg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdbx_epemdbxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdbx_epemdbxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdd_epemddg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdd_epemddg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemggg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemggg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemssxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemssxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sddx_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemdbbx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemdbbx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemdddx(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemdddx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemdgg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemdgg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemdssx(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdg_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sds_epemdsg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdsx_epemdsxg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdu_epemudg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdu_epemudg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdu_epemudg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdu_epemudg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=1
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdux_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxb_epemdxbg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxb_epemdxbg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxbx_epemdxbxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxbx_epemdxbxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemggg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemggg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemssxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemssxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxd_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxdx_epemdxdxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxdx_epemdxdxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemddxdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemddxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemdxbbx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemdxbbx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemdxgg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemdxgg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemdxssx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxg_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxs_epemdxsg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxsx_epemdxsxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxu_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxux_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=-1
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxux_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxux_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sdxux_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_epembbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_epembgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_epemddxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_epemddxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_epemuuxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgb_epemuuxb(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epembbxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-5
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epembxgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epemddxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epemddxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epemuuxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgbx_epemuuxbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgc_epemuuxc(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgcx_epemuuxcx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemdbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemdbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemdddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemdddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemdgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemdgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemdssx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgd_epemuuxd(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemddxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemddxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemdxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemdxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemdxgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-3
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemdxgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemdxssx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgdx_epemuuxdx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgg_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgs_epemddxs(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgsx_epemddxsx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemubbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemubbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemuccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=0
      madflav(7)=0

      if (flavequiv_perm( 7,flav,madflav,perm)) then

c$$$      write(*,*) '---------------------'
c$$$      write(*,*) flav
c$$$      write(*,*) madflav
c$$$      write(*,*) perm


      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemugg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemugg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=2
      madflav(7)=-2
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemuuux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_epemuuux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-2
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuuxux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuuxux(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxbbx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxccx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxgg(madp,amp2)
      return
      endif

      madflav(1)=0
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_epemuxgg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssd_epemdsg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssdx_epemdxsg(madp,amp2)
      return
      endif

      madflav(1)=3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssg_epemddxs(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxd_epemdsxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-1
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxdx_epemdxsxg(madp,amp2)
      return
      endif

      madflav(1)=-3
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Ssxg_epemddxsx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_epemubg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sub_epemubg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epemubxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Subx_epemubxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suc_epemucg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sucx_epemucxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sud_epemudg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sud_epemudg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sud_epemudg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sud_epemudg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sudx_epemudxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemubbx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemubbx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemuccx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemuddx(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemugg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemugg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=2
      madflav(7)=-2
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemuuux(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sug_epemuuux(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_epemuug(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suu_epemuug(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemccxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemccxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemggg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemggg(madp,amp2)
      return
      endif

      madflav(1)=2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_epemuxbg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxb_epemuxbg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_epemuxbxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-5
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxbx_epemuxbxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxc_epemuxcg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxcx_epemuxcxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxd_epemuxdg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxdx_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxdx_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-1
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxdx_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-3
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxdx_epemuxdxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=-2
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuuxux(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuuxux(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxbbx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=5
      madflav(7)=-5
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxbbx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=4
      madflav(7)=-4
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxccx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=1
      madflav(7)=-1
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=3
      madflav(7)=-3
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxddx(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxgg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=0
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_epemuxgg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=5
      madflav(6)=-5
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epembbxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemccxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemccxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=1
      madflav(6)=-1
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=3
      madflav(6)=-3
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemddxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemggg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=0
      madflav(6)=0
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemggg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_epemuuxg(madp,amp2)
      return
      endif

      madflav(1)=-2
      madflav(2)=-2
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-2
      madflav(6)=-2
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo
      call Suxux_epemuxuxg(madp,amp2)
      return
      endif

      madflav(1)=-4
      madflav(2)=-4
      madflav(3)=-11
      madflav(4)=11
      madflav(5)=-4
      madflav(6)=-4
      madflav(7)=0
      if (flavequiv_perm( 7,flav,madflav,perm)) then
      do i=1, 7 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxux_epemuxuxg(madp,amp2)
      return
      endif

      write(*,*) 'ERROR: the flavour list', flav
      write(*,*) 'is not in the list of MADGRAPH routines'
      call exit(1)
      end

