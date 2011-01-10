      subroutine bbinit
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_flg.h'
      include 'include/pwhg_rnd.h'
      include 'include/pwhg_rad.h'
      integer iret1,iret2,iun
      real * 8 sigbtl,errbtl,sigrm,errrm,
     #         xint,xintrm,totneg
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
      integer j,k,mcalls,icalls,imode,iunstat
      real * 8 powheginput
      external btilde,sigremnant,powheginput
      do j=1,ndiminteg
         do k=0,50
            xgrid(k,j)=0
            xgridrm(k,j)=0
            xmmm(k,j)=0
            xmmmrm(k,j)=0
         enddo
         do k=1,50
            ymax(k,j)=0
            ymaxrm(k,j)=0
         enddo
         ifold(j)=1
         ifoldrm(j)=1
      enddo
      call newunit(iunstat)
      if(rnd_cwhichseed.eq.'none') then
         open(unit=iunstat,file=pwgprefix(1:lprefix)//'stat.dat',
     1        status='unknown')
      else
         open(unit=iunstat,file=pwgprefix(1:lprefix)//'stat-'//
     1        rnd_cwhichseed//'.dat',status='unknown')
      endif
      ncall1=powheginput('ncall1')
      ncall2=powheginput('ncall2')
      itmx1=powheginput('itmx1')
      itmx2=powheginput('itmx2')
      iret1=0
      iret2=0
      call loadgrids(iret1,xgrid,ymax,xgridrm,ymaxrm,
     2     ifold,ifoldrm)
      if(iret1.ne.0) call loadxgrid(iret2,xgrid,xint,xgridrm,xintrm)
      if(iret2.ne.0) then
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
     #         iun,xgridrm,xintrm,ymaxrm,sigrm,errrm)
            close(iun)
         endif
         call storexgrid(xgrid,xint,xgridrm,xintrm)
         write(*,*)' Importance sampling x grids generated and stored'
      endif
      if(ncall2.eq.0) then
         write(*,*) ' ncall2 set to 0; nothing else to do'
         call exit(0)
      endif
c importance sampling grids have been initialized with default seed;
c they must all be the same. Now set current seed, if required
      if(rnd_cwhichseed.ne.'none')
     1     call setrandom(rnd_initialseed,rnd_i1,rnd_i2)
      if(iret1.ne.0) then
         if (powheginput('#testplots').eq.1d0) call init_hist
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
         call writetotals(6,rad_totbtl,'btilde')
         call writetotals(iunstat,rad_totbtl,'btilde')
c Now compute the remnant contributions
         call resettotalsrmn
         if((flg_withreg.or.flg_withdamp).and..not.flg_bornonly) then
            write(*,*)' POWHEG: Computing remnant'//
     #                ' and/or regular remnants'
            flg_nlotest=.true.
            imode=1
            call mint(sigremnant,ndiminteg,ncall2,itmx2,ifoldrm,imode,
     #         iun,xgridrm,xintrm,ymaxrm,sigrm,errrm)
c add finalized remnant contributions in histograms
            call pwhgaddout
            flg_nlotest=.false.
         endif
         call finaltotalsrmn
         if(flg_withdamp) then
            call writetotals(6,rad_totrem,'remnant')
            call writetotals(iunstat,rad_totrem,'remnant')
         endif
         if(flg_withreg) then
            call writetotals(6,rad_totreg,'regular')
            call writetotals(iunstat,rad_totreg,'regular')
         endif
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
c notice: negative weights in regular or dump remnants are tolerated
c only if withnegweights is set; 
         rad_totgen=rad_totposrem+rad_totposreg+rad_totbtlgen
         rad_etotgen=sqrt(rad_etotrem**2
     1        +rad_etotreg**2+rad_etotbtlgen**2)
         rad_tot=rad_totrem+rad_totreg+rad_totbtl
         rad_etot=sqrt(rad_etotrem**2
     1        +rad_etotreg**2+rad_etotbtl**2)
         
c        
         call storegrids(xgrid,ymax,xgridrm,ymaxrm,ifold,ifoldrm,ncall2)
c Output NLO histograms
         if (powheginput('#testplots').eq.1d0) then
            if(rnd_cwhichseed.eq.'none') then
               open(unit=99,file=pwgprefix(1:lprefix)//'NLO.top')
            else
               open(unit=99,file=pwgprefix(1:lprefix)//'NLO-'
     1              //rnd_cwhichseed//'.top')
            endif
            call pwhgtopout
            close(99)
         endif
         
         write(iunstat,*) ' total cross section: ',rad_tot,'+-',
     1        rad_etot
         totneg=rad_totnegbtl+rad_totnegrem+rad_totnegreg
         write(iunstat,*) ' negative weight fraction:',
     1        totneg/(2*totneg+rad_tot)
      else
         write(*,*)
     #     ' stored grids successfully loaded'
         negflag=.false.
         
         call writetotals(6,rad_totbtl,'btilde')
         if(flg_withdamp) then
            call writetotals(6,rad_totrem,'remnant')
         endif
         if(flg_withreg) then
            call writetotals(6,rad_totreg,'regular')
         endif
      endif
      close(iunstat)
      call flush
      write(*,*) ' total cross section: ',rad_tot,'+-',
     1     rad_etot
      totneg=rad_totnegbtl+rad_totnegrem+rad_totnegreg
      write(*,*) ' negative weight fraction:',
     1     totneg/(2*totneg+rad_tot)
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


      subroutine writetotals(unit,rtot,name)
      integer unit
      real * 8 rtot(2,6)
      character *(*) name
         write(unit,*) name//' pos.   weights:', rtot(1,3),
     1     ' +-',rtot(2,3)
         write(unit,*) name//' |neg.| weights:', rtot(1,4),
     1        ' +-',rtot(2,4)
         write(unit,*) name//' total (pos.-|neg.|):', rtot(1,1),
     1        ' +-',rtot(2,1)
      
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


      subroutine storegrids(xgrid,ymax,xgridrm,ymaxrm,
     #                ifold,ifoldrm,ncall2)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_rnd.h'
      real * 8 xgrid(0:50,ndiminteg),ymax(50,ndiminteg)
     #        ,xgridrm(0:50,ndiminteg),ymaxrm(50,ndiminteg)
      integer nbins
      parameter (nbins=50)
      integer ifold(ndiminteg),ifoldrm(ndiminteg),ncall2
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer j,k,iun
      call newunit(iun)
      if(rnd_cwhichseed.eq.'none') then
         open(unit=iun,file=pwgprefix(1:lprefix)//'grid.dat',
     #     form='unformatted',status='unknown')
      else
         open(unit=iun,
     1        file=pwgprefix(1:lprefix)//'grid-'//rnd_cwhichseed//
     2        '.dat',form='unformatted',status='unknown')
      endif
      write(iun) ((xgrid(j,k),k=1,ndiminteg),j=0,nbins)
      write(iun) ((ymax(j,k),k=1,ndiminteg),j=1,nbins)
      write(iun) ((xgridrm(j,k),k=1,ndiminteg),j=0,nbins)
      write(iun) ((ymaxrm(j,k),k=1,ndiminteg),j=1,nbins)
      write(iun) (ifold(k),k=1,ndiminteg)
      write(iun) (ifoldrm(k),k=1,ndiminteg)
      write(iun) ncall2
      write(iun) kn_sbeams, pdf_ih1, pdf_ih2, pdf_ndns1, pdf_ndns2
      write(iun) ((rad_totarr(j,k),j=1,2),k=1,15)
      close(iun)
      end

      subroutine loadgrids(iret,xgrid,ymax,xgridrm,ymaxrm,
     #           ifold,ifoldrm)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_rnd.h'
      real * 8 xgrid(0:50,ndiminteg),ymax(50,ndiminteg)
     #        ,xgridrm(0:50,ndiminteg),ymaxrm(50,ndiminteg)
      real * 8 xxgrid(0:50,ndiminteg),xymax(50,ndiminteg)
     #        ,xxgridrm(0:50,ndiminteg),xymaxrm(50,ndiminteg)
      integer ntot
      parameter (ntot=15)
      real * 8 tot(2,ntot)
      integer ifold(ndiminteg),ifoldrm(ndiminteg)
      integer iifold(ndiminteg),iifoldrm(ndiminteg)
      integer iret,jfound
c
      integer ios
      integer nbins
      parameter (nbins=50)
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      character * 4 chseed
      real * 8 shx
      integer ih1x, ih2x, ndns1x, ndns2x
      integer j,k,iun,jfile,nfiles,ncall2
      logical lpresent,manyfiles,filefound
      real * 8 powheginput
      external powheginput
      if(powheginput('use-old-grid').eq.0) then
         iret=1
         return
      endif
      iret=0
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'grid.dat',
     #     form='unformatted',status='old',iostat=ios)
      if(ios.eq.0) then
         nfiles=1
      else
         if(rnd_cwhichseed.ne.'none') then
c Are we required to generate a grid file or to load a bunch of them?
c See if the grid file exists already
            inquire(file=pwgprefix(1:lprefix)//'grid-'//
     1           rnd_cwhichseed//'.dat',exist=lpresent)
            if(.not.lpresent) then
               iret=-1
               return
            endif
         endif
         nfiles=9999
         manyfiles=.true.
      endif
c Try to open and merge a set of grid files, generated with different
c random seeds
      filefound=.false.
      jfound=0
      do jfile=1,nfiles
         if(manyfiles) then
            write(chseed,'(i4)') jfile
            do k=1,4
               if(chseed(k:k).eq.' ') chseed(k:k)='0'
            enddo
            inquire(file=pwgprefix(1:lprefix)//'grid-'//
     1           chseed//'.dat',exist=lpresent)
            if(.not.lpresent) goto 111
            open(unit=iun,file=pwgprefix(1:lprefix)//'grid-'//
     1           chseed//'.dat',
     2           form='unformatted',status='old',iostat=ios)
            if(ios.ne.0) then
               iret=-1
               return
            else
               write(*,*) ' Opened ',pwgprefix(1:lprefix)//'grid-'//
     1              chseed//'.dat'
            endif
         endif
         filefound=.true.
         read(iun,iostat=ios) ((xxgrid(j,k),k=1,ndiminteg),j=0,nbins)
         if(ios.ne.0) goto 998
         read(iun,iostat=ios) ((xymax(j,k),k=1,ndiminteg),j=1,nbins)
         if(ios.ne.0) goto 998
         read(iun,iostat=ios) ((xxgridrm(j,k),k=1,ndiminteg),j=0,nbins)
         if(ios.ne.0) goto 998
         read(iun,iostat=ios) ((xymaxrm(j,k),k=1,ndiminteg),j=1,nbins)
         if(ios.ne.0) goto 998
         read(iun,iostat=ios) (iifold(k),k=1,ndiminteg)
         if(ios.ne.0) goto 998
         read(iun,iostat=ios) (iifoldrm(k),k=1,ndiminteg)
         if(ios.ne.0) goto 998
         read(iun,iostat=ios) ncall2
         if(ios.ne.0) goto 998
         read(iun,iostat=ios) shx, ih1x, ih2x, ndns1x, ndns2x
         if(ios.ne.0) goto 998
         if(shx.ne.kn_sbeams.or.ih1x.ne.pdf_ih1.or.ih2x.ne.pdf_ih2
     1      .or.ndns1x.ne.pdf_ndns1.or.ndns2x.ne.pdf_ndns2.or.ios.ne.0)
     2        goto 998
         read(iun,iostat=ios) ((tot(k,j),k=1,2),j=1,ntot)
         if(ios.ne.0) goto 998
         jfound=jfound+1
         if(jfile.lt.2) then
            do k=1,ndiminteg
               do j=0,nbins
                  xgrid(j,k)=xxgrid(j,k)
                  xgridrm(j,k)=xxgridrm(j,k)
               enddo
               ifold(k)=iifold(k)
               ifoldrm(k)=iifoldrm(k)
            enddo
            do k=1,ndiminteg
               do j=1,nbins
                  ymax(j,k)=xymax(j,k)
                  ymaxrm(j,k)=xymaxrm(j,k)
               enddo
            enddo
            do k=1,2
               do j=1,ntot
                  rad_totarr(k,j)=tot(k,j)
               enddo
            enddo
         else
            do k=1,ndiminteg
               do j=0,nbins
                  if(xgrid(j,k).ne.xxgrid(j,k).or.
     1                 xgridrm(j,k).ne.xxgridrm(j,k)) then
                     write(*,*) ' error loading grids: '
                     write(*,*)  pwgprefix(1:lprefix)//'grid-'//
     1          rnd_cwhichseed//'.dat does not have the same importance'
                    write(*,*) 'sampling grid as',pwgprefix(1:lprefix)//
     1                    'grid.dat'
                     call exit(-1)
                  endif
               enddo
               if(ifold(k).ne.iifold(k)
     1              .or.ifoldrm(k).ne.ifoldrm(k)) then
                  write(*,*) ' error loading grids: '
                  write(*,*)  pwgprefix(1:lprefix)//'grid-'//
     1                 rnd_cwhichseed//
     2                 '.dat does not have the same folding as'
                  write(*,*) ,pwgprefix(1:lprefix)//'grid.dat'
                  call exit(-1)
               endif
            enddo
            do k=1,ndiminteg
               do j=1,nbins
                  ymax(j,k)=max(ymax(j,k),xymax(j,k))
                  ymaxrm(j,k)=max(ymaxrm(j,k),xymaxrm(j,k))
               enddo
            enddo
            do j=1,ntot
               rad_totarr(2,j)=
     1              sqrt((rad_totarr(2,j)**2*(jfound-1)**2+tot(2,j)**2)
     2              /jfound**2+(jfound-1)*(rad_totarr(1,j)-tot(1,j))**2/
     3              (jfound**3*ncall2))
               rad_totarr(1,j)=
     1              (rad_totarr(1,j)*(jfound-1)+tot(1,j))/jfound
            enddo
         endif
         close(iun)
 111     continue
      enddo
      if(filefound) return
 998  continue
      close(iun)
      iret=-1
      end

      subroutine storexgrid(xgrid,xint,xgridrm,xintrm)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_rad.h'
      real * 8 xgrid(0:50,ndiminteg),xgridrm(0:50,ndiminteg),
     1     xint,xintrm
      integer nbins
      parameter (nbins=50)
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer j,k,iun
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'xgrid.dat',
     #     form='unformatted',status='unknown')
      write(iun) ((xgrid(j,k),k=1,ndiminteg),j=0,nbins),xint
      write(iun) ((xgridrm(j,k),k=1,ndiminteg),j=0,nbins),xintrm
      write(iun) kn_sbeams, pdf_ih1, pdf_ih2, pdf_ndns1, pdf_ndns2
      close(iun)
      end

      subroutine loadxgrid(iret,xgrid,xint,xgridrm,xintrm)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_rad.h'
      real * 8 xgrid(0:50,ndiminteg),xgridrm(0:50,ndiminteg),
     1     xint,xintrm
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
      if(powheginput('use-old-grid').eq.0) then
         iret=1
         return
      endif
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'xgrid.dat',
     #     form='unformatted',status='old',iostat=ios)
      if(ios.ne.0) then
         iret=-1
         return
      endif
      read(iun,iostat=ios) ((xgrid(j,k),k=1,ndiminteg),j=0,nbins),xint
      read(iun,iostat=ios) ((xgridrm(j,k),k=1,ndiminteg),j=0,nbins),
     1     xintrm
      read(iun,iostat=ios) shx, ih1x, ih2x, ndns1x, ndns2x
      if(shx.ne.kn_sbeams.or.ih1x.ne.pdf_ih1.or.ih2x.ne.pdf_ih2
     #  .or.ndns1x.ne.pdf_ndns1.or.ndns2x.ne.pdf_ndns2.or.ios.ne.0) then
         iret=-1
         close(iun)
         return
      endif
      close(iun)
      iret=0
      end