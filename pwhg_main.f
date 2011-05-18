      implicit none
      include 'LesHouches.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_rad.h'
      include 'pwhg_st.h'
      include 'pwhg_kn.h'
      include 'pwhg_rnd.h'
      integer j,iun,nev
      real * 8 weight
      real * 8 powheginput
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer ios
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      integer iseed,n1,n2
      nev=powheginput('numevts')
      call newunit(iun)
c The following allows to perform multiple runs with
c different random seeds in the same directory.
c If manyseeds is set to 1, the program asks for an integer j;
c The file 'pwgprefix'seeds.dat at line j is read, and the
c integer at line j is used to initialize the random
c sequence for the generation of the event.
c The event file is called 'pwgprefix'events-'j'.lhe
      if(powheginput("#manyseeds").eq.1) then
         open(unit=iun,status='old',
     1        file=pwgprefix(1:lprefix)//'seeds.dat')
         do j=1,1000000
            read(iun,*,iostat=ios)  rnd_initialseed
            if(ios.ne.0) goto 10
         enddo
 10      continue
         rnd_numseeds=j-1
         write(*,*) 'enter which seed'
         read(*,*) rnd_iwhichseed
         if(rnd_iwhichseed.gt.rnd_numseeds) then
            write(*,*) ' no more than ',rnd_numseeds, ' seeds in ',
     1           pwgprefix(1:lprefix)//'seeds.dat'
            call exit(-1)
         endif
         rewind(iun)
         do j=1,rnd_iwhichseed
c Commented line to be used instead, for testing that manyseed runs
c yield the same results as single seed runs, provided the total number
c of calls is the same.
c            read(iun,*) rnd_initialseed,rnd_i1,rnd_i2
            read(iun,*) rnd_initialseed
            rnd_i1=0
            rnd_i2=0
         enddo
         close(iun)
         write(rnd_cwhichseed,'(i4)') rnd_iwhichseed
         do j=1,4
            if(rnd_cwhichseed(j:j).eq.' ') rnd_cwhichseed(j:j)='0'
         enddo
      else
         rnd_cwhichseed='none'
      endif
      if (powheginput('#testplots').eq.1d0) WHCPRG='NLO   '
      call pwhginit
      if(nev.gt.0) then
         if(rnd_cwhichseed.ne.'none') then
            write(*,*) pwgprefix(1:lprefix)//'events-'//
     1           rnd_cwhichseed//'.lhe', rnd_iwhichseed,rnd_initialseed
            open(unit=iun,status='new',file=pwgprefix(1:lprefix)
     1           //'events-'//rnd_cwhichseed//'.lhe')
         else
            open(unit=iun,status='new',
     1           file=pwgprefix(1:lprefix)//'events.lhe')
         endif
      else
         write(*,*) ' No events requested'
         goto 999
      endif
      call lhefwritehdr(iun)
      if (powheginput('#testplots').eq.1d0) then
         call init_hist 
c     let the analysis subroutine know that it is run by this program
         WHCPRG='LHE   '
      endif
c if we are using manyseeds, and iseed is given, it means that we want
c to examine that event in particular
      if(rnd_cwhichseed.ne.'none') then
         iseed=powheginput('#iseed')
         n1=powheginput('#rand1')
         n2=powheginput('#rand2')
         if(iseed.ge.0.and.n1.ge.0.and.n2.ge.0)
     1        call setrandom(iseed,n1,n2)
      endif
      call resetcnt
     1       ('upper bound failure in inclusive cross section')
      call resetcnt
     1       ('vetoed calls in inclusive cross section')
      call resetcnt(
     1 'upper bound failures in generation of radiation')
      call resetcnt('vetoed radiation')
      do j=1,nev
         call pwhgevent
c         write(*,*)  j,' / ',nev,' pt = ',sqrt(st_muren2)
         call lhefwritev(iun)
         if(idwtup.eq.3) then
            weight=rad_totgen*xwgtup
         elseif(idwtup.eq.-4) then
            weight=xwgtup
         else
            write(*,*) ' only 3 and -4 are allowed for idwtup'
            call exit(-1)
         endif
         if(kn_csi.eq.0d0) then
            call analysis_driver(weight,0)
         else
            call analysis_driver(weight,1)
         endif
         call pwhgaccumup
      enddo
      if (powheginput('#testplots').eq.1d0) then
         if(rnd_cwhichseed.eq.'none') then
            open(unit=99,file=pwgprefix(1:lprefix)//
     1           'pwhgalone-output.top')
         else
            open(unit=99,file=pwgprefix(1:lprefix)//
     1           'pwhgalone-output'//rnd_cwhichseed//'.top')
         endif
         call pwhgsetout
         call pwhgtopout
         close(99)
      endif
      call lhefwritetrailer(iun)
      close(iun)
 999  continue
      call newunit(iun)
      if(rnd_cwhichseed.eq.'none') then
         open(unit=iun,file=pwgprefix(1:lprefix)//'counters.dat'
     1     ,status='unknown')
      else
         open(unit=iun,file=pwgprefix(1:lprefix)//'counters'//
     1        rnd_cwhichseed//'.dat',status='unknown')
      endif
      call printcnt(iun)
      close(iun)
      end
