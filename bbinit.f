      subroutine bbinit
      implicit none
      include 'include/pwhg_flst.h'
      include 'include/pwhg_flg.h'
      include 'include/pwhg_rad.h'
      integer iupperisr,iupperfsr
      common/cupper/iupperisr,iupperfsr
      integer iret,iun,n
      real * 8 sigtot,errtot,signeg,errneg,sigbtl,errbtl,sigrm,errrm,
     #         xint,fracneg
      real * 8 btilde,sigremnant
      integer ncall1,ncall2,itmx1,itmx2
      real * 8 xx(ndiminteg),xgrid(0:50,ndiminteg),ymax(50,ndiminteg)
     #                  ,xgridrm(0:50,ndiminteg),ymaxrm(50,ndiminteg)
     #                  ,xmmm(0:50,ndiminteg),xmmmrm(0:50,ndiminteg)
      integer ifold(ndiminteg),ifoldrm(ndiminteg)
      common/cgengrids/xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm
      logical negflag
      common /cbbarra/negflag
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer j,mcalls,icalls,imode,iunstat
      real * 8 powheginput
      external btilde,sigremnant,powheginput

c select which upper bounding function form
      iupperisr=powheginput("#iupperisr")
      if(iupperisr.lt.0) iupperisr=1
      iupperfsr=powheginput("#iupperfsr")
      if(iupperfsr.lt.0) iupperfsr=2
c
      flg_bornonly=.false.
      if (powheginput("#bornonly").eq.1) flg_bornonly=.true.

      do j=1,ndiminteg
         ifold(j)=1
      enddo
      call newunit(iunstat)
      open(unit=iunstat,file=pwgprefix(1:lprefix)//'stat.dat',
     #  status='unknown')
      ncall1=powheginput('ncall1')
      ncall2=powheginput('ncall2')
      itmx1=powheginput('itmx1')
      itmx2=powheginput('itmx2')
      call loadgrids(sigbtl,errbtl,sigrm,errrm,fracneg,iret,
     #               xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm)
      if(iret.ne.0) then
c debug
c         goto 111
         call init_hist
         negflag=.false.
         write(*,*)
         write(*,*)' POWHEG: initialization'
         write(*,*)' Computing the integral of the absolute value'
         write(*,*)' of the cross section to set up the adaptive grid'
         call newunit(iun)
         open(unit=iun,file=pwgprefix(1:lprefix)//'btlgrid.top',
     #        status='unknown')
         write(*,*)' result +- errtot (picobarn) for each iteration'
         flg_nlotest=.false.
         imode=0
         call mint(btilde,ndiminteg,ncall1,itmx1,ifold,imode,iun,
     #        xgrid,xint,ymax,sigbtl,errbtl)
         close(iun)
c set  up the folding here, if required
         ifold(ndiminteg-2) = powheginput("foldcsi")
         ifold(ndiminteg-1) = powheginput("foldy")
         ifold(ndiminteg)   = powheginput("foldphi")
c
         if(.not.flg_bornonly) then
            negflag=.true.
            write(*,*)
            write(*,*)' POWHEG: Computing negative weight contribution'
     #             //' to inclusive cross section'
            flg_nlotest=.false.
            imode=1
            call mint(btilde,ndiminteg,ncall2,itmx2,ifold,imode,iun,
     #      xgrid,xint,ymax,signeg,errneg)
            write(*,*) 'neg. weights:', -signeg,' +-', errneg
         endif
         negflag=.false.
         write(*,*)' POWHEG: Computing positive weight contribution'
     #           //' to inclusive cross section' 
         flg_nlotest=.true.
         imode=1
         call mint(btilde,ndiminteg,ncall2,itmx2,ifold,imode,iun,
     #        xgrid,xint,ymax,sigbtl,errbtl)
         flg_nlotest=.false.
         write(*,*) 'pos. weights:', sigbtl,' +-', errbtl
         sigbtl=sigbtl-signeg
         errbtl=sqrt(errbtl**2+errneg**2)
         write(*,*) 'pos+neg', sigbtl,' +-', errbtl
c Now compute the remnant contributions
c No folding for remnants:
 111     do j=1,ndiminteg
            ifoldrm(j)=1
         enddo
         if((flg_withreg.or.flg_withdamp).and..not.flg_bornonly) then
            write(*,*)' Computing the integral of the absolute value of'
            write(*,*)' the remnant cross section' 
            write(*,*)' to set up the adaptive grid'
            negflag=.false.
            flg_nlotest=.false.
            call newunit(iun)
            open(unit=iun,file=pwgprefix(1:lprefix)//'rmngrid.top',
     #        status='unknown')
            imode=0
            call mint(sigremnant,ndiminteg,ncall1,itmx1,ifoldrm,imode,
     #         iun,xgridrm,xint,ymaxrm,sigrm,errrm)
            close(iun)
            write(*,*)' POWHEG: Computing remnant'//
     #                ' and/or regular remnants'
            flg_nlotest=.true.
            imode=1
            call mint(sigremnant,ndiminteg,ncall2,itmx2,ifoldrm,imode,
     #         iun,xgridrm,xint,ymaxrm,sigrm,errrm)
            flg_nlotest=.false.
            write(*,*) 'remnants', sigrm,' +-', errrm
         else
            sigrm=0
            errrm=0
         endif
         sigtot=sigrm+sigbtl
         errtot=sqrt(errbtl**2+errrm**2)
         fracneg=signeg/sigtot
         write(*,*) 'Total', sigtot,' +-', errtot
c        
         call storegrids(sigbtl,errbtl,sigrm,errrm,fracneg,
     #            xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm)
c Output NLO histograms
         if (powheginput('#testplots').eq.1d0) then
            open(unit=99,file=pwgprefix(1:lprefix)//'NLO.top')
            n=powheginput('itmx2')*powheginput('ncall2')
            call pwhgtopout(n)
            close(99)
         endif
      else
         write(*,*)
     #     ' stored grids successfully loaded'
         negflag=.false.
         sigtot=sigbtl+sigrm
         errtot=sqrt(errbtl**2+errrm**2)
      endif
      write(*,*) ' Total cross section in pb', sigtot,' +-',errtot
      write(*,*) ' Estimated fraction of neg. weights', fracneg
      write(iunstat,*) ' Total cross section in pb', sigtot,' +-',errtot
      write(iunstat,*) ' Estimated fraction of neg. weights', fracneg
      if(flg_withreg.or.flg_withdamp) then
         write(*,*) ' Positive remnant cross section in pb',
     #      sigrm,' +-',errrm
         write(iunstat,*) ' Positive remnant cross section in pb',
     #      sigrm,' +-',errrm
      endif
      close(iunstat)
      call flush
c initialize gen
      negflag=.false.
      call gen(btilde,ndiminteg,xgrid,ymax,xmmm,ifold,0,
     #    mcalls,icalls,xx)
      call do_maxrat(mcalls,icalls)
c print statistics
      call gen(btilde,ndiminteg,xgrid,ymax,xmmm,ifold,3,
     #    mcalls,icalls,xx)
      if(xx(1).gt.0) write(*,*) 'POWHEG: efficiency in the generation'
     # //' of the Born variables =',xx(1)
      if(flg_withreg.or.flg_withdamp) then
c initialize gen
         call gen(sigremnant,ndiminteg,xgridrm,ymaxrm,xmmmrm,ifoldrm,
     #            0,mcalls,icalls,xx)
CAVEAT    100 ???
         do j=1,min(powheginput('nubound'),100d0)
            call gen(sigremnant,ndiminteg,xgridrm,ymaxrm,xmmmrm,ifoldrm,
     #            1,mcalls,icalls,xx)
         enddo
c     print statistics
         call gen(sigremnant,ndiminteg,xgridrm,ymaxrm,xmmmrm,ifoldrm,
     #            3,mcalls,icalls,xx)
         if(xx(1).gt.0) then
            write(*,*) 'POWHEG: efficiency in the generation'
     #                   //' of the remnant variables =',xx(1)
         endif
      endif
c fill radiation common block with cross sections
      rad_sigtot=sigtot
      rad_sigtoterr=errtot 
      rad_sigbtl=sigbtl
      rad_sigrm=sigrm
      end


      subroutine gen_btilde(mcalls,icalls)
      implicit none
      include 'include/pwhg_flst.h'
      integer mcalls,icalls
      real * 8 xgrid(0:50,ndiminteg),ymax(50,ndiminteg)
     #        ,xgridrm(0:50,ndiminteg),ymaxrm(50,ndiminteg)
     #        ,xmmm(0:50,ndiminteg),xmmmrm(0:50,ndiminteg)
      integer ifold(ndiminteg),ifoldrm(ndiminteg)
      common/cgengrids/xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm
      logical negflag
      common /cbbarra/negflag
      real * 8 xx(ndiminteg)      
      real * 8 btilde
      external btilde
      negflag=.false.
      call gen(btilde,ndiminteg,xgrid,ymax,xmmm,ifold,1,
     #    mcalls,icalls,xx)
      end

      subroutine gen_sigremnant
      implicit none
      include 'include/pwhg_flst.h'
      real * 8 xgrid(0:50,ndiminteg),ymax(50,ndiminteg)
     #        ,xgridrm(0:50,ndiminteg),ymaxrm(50,ndiminteg)
     #        ,xmmm(0:50,ndiminteg),xmmmrm(0:50,ndiminteg)
      integer ifold(ndiminteg),ifoldrm(ndiminteg)
      common/cgengrids/xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm
      logical negflag
      common /cbbarra/negflag
      real * 8 xx(ndiminteg)
      integer mcalls,icalls
      real * 8 sigremnant
      external sigremnant
      negflag=.false.
      call gen(sigremnant,ndiminteg,xgridrm,ymaxrm,xmmmrm,ifoldrm,1,
     #    mcalls,icalls,xx)
      end


      subroutine storegrids(sigbtl, errbtl, sigrm, errrm, fracneg,
     #            xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm)
      implicit none
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      real * 8 sigbtl,errbtl,sigrm,errrm,fracneg
      real * 8 xgrid(0:50,ndiminteg),ymax(50,ndiminteg)
     #        ,xgridrm(0:50,ndiminteg),ymaxrm(50,ndiminteg)
     #        ,xmmm(0:50,ndiminteg),xmmmrm(0:50,ndiminteg)
      integer nbins
      parameter (nbins=50)
      integer ifold(ndiminteg),ifoldrm(ndiminteg)
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer j,k,iun
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'grid.dat',
     #     form='unformatted',status='unknown')
      write(iun) ((xgrid(j,k),k=1,ndiminteg),j=0,nbins)
      write(iun) ((ymax(j,k),k=1,ndiminteg),j=1,nbins)
      write(iun) ((xmmm(j,k),k=1,ndiminteg),j=1,nbins)
      write(iun) ((xgridrm(j,k),k=1,ndiminteg),j=0,nbins)
      write(iun) ((ymaxrm(j,k),k=1,ndiminteg),j=1,nbins)
      write(iun) ((xmmmrm(j,k),k=1,ndiminteg),j=1,nbins)
      write(iun) (ifold(k),k=1,ndiminteg)
      write(iun) (ifoldrm(k),k=1,ndiminteg)
      write(iun) kn_sbeams, pdf_ih1, pdf_ih2, pdf_ndns1, pdf_ndns2
      write(iun) sigbtl, errbtl, sigrm, errrm, fracneg
      close(iun)
      end

      subroutine loadgrids(sigbtl,errbtl,sigrm,errrm,fracneg,iret,
     #               xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm)
      implicit none
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      real * 8 sigbtl,errbtl,sigrm,errrm,fracneg
      real * 8 xgrid(0:50,ndiminteg),ymax(50,ndiminteg)
     #        ,xgridrm(0:50,ndiminteg),ymaxrm(50,ndiminteg)
     #        ,xmmm(0:50,ndiminteg),xmmmrm(0:50,ndiminteg)
      integer ifold(ndiminteg),ifoldrm(ndiminteg)
      integer iret
c
      integer ios
      integer nbins
      parameter (nbins=50)
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      real * 8 shx
      integer ih1x, ih2x, ndns1x, ndns2x
      integer j,k,iun
      real * 8 powheginput
      external powheginput
      if(powheginput('use-old-grid').ne.1) then
         iret=1
         return
      endif
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'grid.dat',
     #     form='unformatted',status='old',iostat=ios)
      if(ios.ne.0) then
         iret=-1
         return
      endif
      read(iun,iostat=ios) ((xgrid(j,k),k=1,ndiminteg),j=0,nbins)
      read(iun,iostat=ios) ((ymax(j,k),k=1,ndiminteg),j=1,nbins)
      read(iun,iostat=ios) ((xmmm(j,k),k=1,ndiminteg),j=1,nbins)
      read(iun,iostat=ios) ((xgridrm(j,k),k=1,ndiminteg),j=0,nbins)
      read(iun,iostat=ios) ((ymaxrm(j,k),k=1,ndiminteg),j=1,nbins)
      read(iun,iostat=ios) ((xmmmrm(j,k),k=1,ndiminteg),j=1,nbins)
      read(iun,iostat=ios) (ifold(k),k=1,ndiminteg)
      read(iun,iostat=ios) (ifoldrm(k),k=1,ndiminteg)
      read(iun,iostat=ios) shx, ih1x, ih2x, ndns1x, ndns2x
      if(shx.ne.kn_sbeams.or.ih1x.ne.pdf_ih1.or.ih2x.ne.pdf_ih2
     #  .or.ndns1x.ne.pdf_ndns1.or.ndns2x.ne.pdf_ndns2.or.ios.ne.0) then
         iret=-1
         close(iun)
         return
      endif
      read(iun) sigbtl, errbtl, sigrm, errrm, fracneg
      close(iun)
      iret=0
      end
