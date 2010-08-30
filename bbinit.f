      subroutine bbinit
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_flg.h'
      include 'include/pwhg_rad.h'
      integer iret,iun
      real * 8 sigbtl,errbtl,sigrm,errrm,
     #         xint
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
      call loadgrids(iret,xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm)
      if(iret.ne.0) then
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
         negflag=.false.
         if(flg_withnegweights) then
            write(*,*)' POWHEG: Computing pos.+|neg.| '
     1           //' weight contribution to inclusive cross section' 
         else
            write(*,*)' POWHEG: Computing positive weight'
     1           //' contribution to inclusive cross section' 
         endif
         flg_nlotest=.true.
         imode=1
c Totals will also be made available in the rad_tot???btl variables.
c The output in sigbtl is: positive weight only (flg_withnegweights=.false.)
c                          pos-|neg|            (flg_withnegweights=.true.)
c Results in rad_tot???btl do not depend upon flg_withnegweights
         call resettotals
         call mint(btilde,ndiminteg,ncall2,itmx2,ifold,imode,iun,
     #        xgrid,xint,ymax,sigbtl,errbtl)
         call finaltotals
c finalize btilde output in histograms
         call pwhgaddout
         flg_nlotest=.false.
         write(*,*) 'btilde pos.   weights:', rad_totposbtl,' +-',
     1        rad_etotposbtl
         write(*,*) 'btilde |neg.| weights:', rad_totnegbtl,' +-',
     1        rad_etotnegbtl
         write(*,*) 'btilde total (pos.-|neg.|):', rad_totbtl,' +-',
     1        rad_etotbtl
         write(iunstat,*) 'btilde pos.   weights:', rad_totposbtl,' +-',
     1        rad_etotposbtl
         write(iunstat,*) 'btilde |neg.| weights:', rad_totnegbtl,' +-',
     1        rad_etotnegbtl
         write(iunstat,*) 'btilde Total (pos.-|neg.|):', rad_totbtl,
     1        ' +-',rad_etotbtl
c Now compute the remnant contributions
c No folding for remnants:
         do j=1,ndiminteg
            ifoldrm(j)=1
         enddo
         if((flg_withreg.or.flg_withdamp).and..not.flg_bornonly) then
            write(*,*) ' Computing the integral of the'//
     #           ' remnant cross section' 
            write(*,*) ' to set up the adaptive grid'
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
c add finalized remnant contributions in histograms
            call pwhgaddout
            flg_nlotest=.false.
         else
            sigrm=0
            errrm=0
         endif
         rad_totrm=sigrm
         rad_etotrm=errrm
c rad_totgen is used for the generation of the events.
c btilde and remnant event are chosen in proportion to
c rad_totbtlgen and rad_totrm.
         if(flg_withnegweights) then
            rad_totbtlgen=rad_totabsbtl
            rad_etotbtlgen=rad_etotabsbtl
         else
c notice: this is correct only if the negative fraction is
c negligible
            rad_totbtlgen=rad_totbtl
            rad_etotbtlgen=rad_etotbtl
         endif
         rad_totgen=rad_totrm+rad_totbtlgen
         rad_etotgen=sqrt(rad_etotbtlgen**2+rad_etotrm**2)
         rad_tot=rad_totrm+rad_totbtl
         rad_etot=sqrt(rad_etotbtl**2+rad_etotrm**2)
         
c        
         call storegrids(xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm)
c Output NLO histograms
         if (powheginput('#testplots').eq.1d0) then
            open(unit=99,file=pwgprefix(1:lprefix)//'NLO.top')
            call pwhgtopout
            close(99)
         endif

         if(flg_withreg.or.flg_withdamp) then
            write(iunstat,*) ' Remnant cross section in pb',
     1           rad_totrm,'+-',rad_etotrm
         endif
         
         write(iunstat,*)' total (btilde+remnants) cross section in pb',
     1        rad_tot,'+-',rad_etot
         
         write(iunstat,*) ' negative weight fraction:',
     1        rad_totnegbtl/(2*rad_totnegbtl+rad_tot)
      else
         write(*,*)
     #     ' stored grids successfully loaded'
         negflag=.false.
         write(*,*) 'btilde pos.   weights:', rad_totposbtl,' +-',
     1        rad_etotposbtl
         write(*,*) 'btilde |neg.| weights:', rad_totnegbtl,' +-',
     1        rad_etotnegbtl
         write(*,*) 'btilde total (pos.-|neg.|):', rad_totbtl,' +-',
     1        rad_etotbtl
      endif

      if(flg_withreg.or.flg_withdamp) then
         write(*,*) ' Remnant cross section in pb',
     1        rad_totrm,'+-',rad_etotrm
      endif

      write(*,*) ' total (btilde+remnants) cross section in pb',
     1     rad_tot,'+-',rad_etot

      write(*,*) ' negative weight fraction:',
     1     rad_totnegbtl/(2*rad_totnegbtl+rad_tot)


      close(iunstat)
      call flush
c initialize gen; the array xmmm is set up at this stage.
      negflag=.false.
      call gen(btilde,ndiminteg,xgrid,ymax,xmmm,ifold,0,
     #    mcalls,icalls,xx)
c compute normalization of upper bounding function for radiation
      call do_maxrat(mcalls,icalls)
c print statistics
      call gen(btilde,ndiminteg,xgrid,ymax,xmmm,ifold,3,
     #    mcalls,icalls,xx)
      if(xx(1).gt.0) write(*,*) 'POWHEG: efficiency in the generation'
     # //' of the Born variables =',xx(1)
      if((flg_withreg.or.flg_withdamp).and..not.flg_bornonly) then
c initialize gen for remnants
         call gen(sigremnant,ndiminteg,xgridrm,ymaxrm,xmmmrm,ifoldrm,
     #            0,mcalls,icalls,xx)
c     save random number seeds
         call randomsave
c generate few events from remnants, just to determine the generation efficiency
         do j=1,int(min(powheginput('nubound'),10d0))
            call gen(sigremnant,ndiminteg,xgridrm,ymaxrm,xmmmrm,ifoldrm,
     #            1,mcalls,icalls,xx)
         enddo
c     restore  random number seeds
         call randomrestore
c     print statistics
         call gen(sigremnant,ndiminteg,xgridrm,ymaxrm,xmmmrm,ifoldrm,
     #            3,mcalls,icalls,xx)
         if(xx(1).gt.0) then
            write(*,*) 'POWHEG: efficiency in the generation'
     #                   //' of the remnant variables =',xx(1)
         endif
      endif
      end


      subroutine gen_btilde(mcalls,icalls)
      implicit none
      include 'nlegborn.h'
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
      include 'nlegborn.h'
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


      subroutine storegrids(xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #                ifold,ifoldrm)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_rad.h'
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
      write(iun)
     1     rad_totbtl,rad_etotbtl,
     2     rad_totabsbtl,rad_etotabsbtl,
     3     rad_totposbtl,rad_etotposbtl,
     4     rad_totnegbtl,rad_etotnegbtl,
     5     rad_totrm,rad_etotrm,
     6     rad_totbtlgen,rad_etotbtlgen,
     7     rad_totgen,rad_etotgen,
     8     rad_tot,rad_etot
       close(iun)
      end

      subroutine loadgrids(iret,xgrid,ymax,xmmm,xgridrm,ymaxrm,xmmmrm,
     #           ifold,ifoldrm)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_rad.h'
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
      read(iun)
     1     rad_totbtl,rad_etotbtl,
     2     rad_totabsbtl,rad_etotabsbtl,
     3     rad_totposbtl,rad_etotposbtl,
     4     rad_totnegbtl,rad_etotnegbtl,
     5     rad_totrm,rad_etotrm,
     6     rad_totbtlgen,rad_etotbtlgen,
     7     rad_totgen,rad_etotgen,
     8     rad_tot,rad_etot
      close(iun)
      close(iun)
      iret=0
      end
