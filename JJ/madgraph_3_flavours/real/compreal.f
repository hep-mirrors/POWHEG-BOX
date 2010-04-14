      subroutine compreal(p,flav,amp2)
      real * 8 p(0:3,1: 5)
      integer flav( 5)
      real * 8 amp2
      real * 8 madp(0:3,1: 5)
      integer madflav( 5),perm( 5)
      logical flavequiv_perm
      external flavequiv_perm
      integer i, mu

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

      call Sdg_uuxd(madp,amp2)
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

      call Sdg_uuxd(madp,amp2)
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

      call Sdg_uuxd(madp,amp2)
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

      call Sdu_udg(madp,amp2)
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

      call Sdu_udg(madp,amp2)
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

      call Sdu_udg(madp,amp2)
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

      call Sdux_uxdg(madp,amp2)
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

      call Sdux_uxdg(madp,amp2)
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

      call Sdux_uxdg(madp,amp2)
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

      call Sdxg_uuxdx(madp,amp2)
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

      call Sdxg_uuxdx(madp,amp2)
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

      call Sdxg_uuxdx(madp,amp2)
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

      call Sdxu_udxg(madp,amp2)
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

      call Sdxu_udxg(madp,amp2)
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

      call Sdxu_udxg(madp,amp2)
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

      call Sdxux_uxdxg(madp,amp2)
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

      call Sdxux_uxdxg(madp,amp2)
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

      call Sdxux_uxdxg(madp,amp2)
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

      call Sgd_uuxd(madp,amp2)
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

      call Sgd_uuxd(madp,amp2)
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

      call Sgd_uuxd(madp,amp2)
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

      call Sgdx_uuxdx(madp,amp2)
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

      call Sgdx_uuxdx(madp,amp2)
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

      call Sgdx_uuxdx(madp,amp2)
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
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgu_uddx(madp,amp2)
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

      call Sgu_uddx(madp,amp2)
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

      call Sgu_uddx(madp,amp2)
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
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Sgux_uxddx(madp,amp2)
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

      call Sgux_uxddx(madp,amp2)
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

      call Sgux_uxddx(madp,amp2)
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

      call Sud_udg(madp,amp2)
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

      call Sud_udg(madp,amp2)
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

      call Sud_udg(madp,amp2)
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

      call Sudx_udxg(madp,amp2)
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

      call Sudx_udxg(madp,amp2)
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

      call Sudx_udxg(madp,amp2)
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

      call Sug_uddx(madp,amp2)
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

      call Sug_uddx(madp,amp2)
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

      call Sug_uddx(madp,amp2)
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
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suux_ddxg(madp,amp2)
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

      call Suux_ddxg(madp,amp2)
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

      call Suux_ddxg(madp,amp2)
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

      call Suux_ddxg(madp,amp2)
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

      call Suux_ddxg(madp,amp2)
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

      call Suux_ddxg(madp,amp2)
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

      call Suxd_uxdg(madp,amp2)
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

      call Suxd_uxdg(madp,amp2)
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

      call Suxd_uxdg(madp,amp2)
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

      call Suxdx_uxdxg(madp,amp2)
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

      call Suxdx_uxdxg(madp,amp2)
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

      call Suxdx_uxdxg(madp,amp2)
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
      madflav(4)=1
      madflav(5)=-1
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxg_uxddx(madp,amp2)
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

      call Suxg_uxddx(madp,amp2)
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

      call Suxg_uxddx(madp,amp2)
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
      madflav(3)=1
      madflav(4)=-1
      madflav(5)=0
      if (flavequiv_perm( 5,flav,madflav,perm)) then
      do i=1, 5 
         do mu=0,3
            madp(mu,perm(i))=p(mu,i)
         enddo
      enddo

      call Suxu_ddxg(madp,amp2)
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

      call Suxu_ddxg(madp,amp2)
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

      call Suxu_ddxg(madp,amp2)
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

      call Suxu_ddxg(madp,amp2)
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

      call Suxu_ddxg(madp,amp2)
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

      call Suxu_ddxg(madp,amp2)
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

