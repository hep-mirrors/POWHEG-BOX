      implicit none
      include 'include/LesHouches.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_st.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_flg.h'
      integer j,iun,nev
      real * 8 powheginput
      character * 20 pwgprefix
      character * 40 file
      character * 4 cwhichseed
      integer lprefix
      integer lfile,iwhichseed,iseed
      common/cpwgprefix/pwgprefix,lprefix
      nev=powheginput('numevts')
      call pwhginit
      call newunit(iun)
c The following allows to perform multiple runs with
c different random seeds in the same directory.
c If manyseeds is set to 1, the program asks for an integer j;
c The file 'pwgprefix'seeds.dat at line j is read, and the
c integer at line j is used to initialize the random
c sequence for the generation of the event.
c The event file is called 'pwgprefix'events-'j'.lhe
      if(powheginput("#manyseeds").eq.1) then
         write(*,*) 'enter which seed'
         read(*,*) iwhichseed
         open(unit=iun,status='old',
     1        file=pwgprefix(1:lprefix)//'seeds.dat')
         do j=1,iwhichseed
            read(iun,*) iseed
         enddo
         close(iun)
         call setrandom(iseed,0,0)
         write(cwhichseed,'(i4)') iwhichseed
         file=pwgprefix(1:lprefix)//'events-'//cwhichseed//'.lhe'
         do j=1,len(file)
            if(file(j:j).ne.' ') then
               lfile=j
            endif
         enddo
         do j=1,lfile
            if(file(j:j).eq.' ') file(j:j)='0'
         enddo
         write(*,*) file,iwhichseed,iseed
         open(unit=iun,status='new',file=file)
      else
         open(unit=iun,status='new',
     1        file=pwgprefix(1:lprefix)//'events.lhe')
      endif
      call lhefwritehdr(iun)
      call init_hist 
      do j=1,nev
         call pwhgevent
c         write(*,*)  j,' / ',nev,' pt = ',sqrt(st_muren2)
         call lhefwritev(iun)
         if(flg_debug) call lhefwritextra(iun)
         if(kn_csi.eq.0d0) then
            call analysis_driver(rad_sigtot,0)
         else
            call analysis_driver(rad_sigtot,1)
         endif
         call pwhgaccumup
      enddo
      if (powheginput('#testplots').eq.1d0) then
         open(unit=99,file=pwgprefix(1:lprefix)//'pwhgalone-output.top')
         call pwhgsetout
         call pwhgtopout
         close(99)
      endif
      call lhefwritetrailer(iun)
      close(iun)
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'counters.dat'
     #     ,status='unknown')
      call printcnt(iun)
      close(iun)
      end
