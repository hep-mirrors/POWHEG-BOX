      implicit none
      include 'include/LesHouches.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_st.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rnd.h'
      integer j,iun,nev
      real * 8 weight
      real * 8 powheginput
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer ios
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
            read(iun,*) rnd_initialseed
         enddo
         close(iun)
         write(rnd_cwhichseed,'(i4)') rnd_iwhichseed
         do j=1,4
            if(rnd_cwhichseed(j:j).eq.' ') rnd_cwhichseed(j:j)='0'
         enddo
      else
         rnd_cwhichseed='none'
      endif
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
         call exit(0)
      endif
      call lhefwritehdr(iun)
      call init_hist 
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
