      subroutine opencount(maxev)
      implicit none
      integer maxev
      character * 30 file
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer ios
      character * 7 string
      real * 8 powheginput
      external powheginput
      integer nev
c     this call is necessary to read the prefix of the file
      nev=powheginput('numevts')
      maxev=0
      file=pwgprefix(1:lprefix)//'events.lhe'
      open(unit=97,file=file,status='old',iostat=ios)
      if(ios.ne.0) then
         write(*,*)' enter name of event file'
         read(*,'(a)') file
         open(unit=97,file=file,status='old')
      endif
 1    continue
      read(unit=97,fmt='(a)',end=2) string
      if(string.eq.'</event') then
         maxev=maxev+1
         goto 1
      endif
      goto 1
 2    continue
      write(*,*) ' found ',maxev,' events in file ',file
      if (maxev.eq.0) then
         write(*,*) ' NO EVENTS!! Program exits'
         call exit(3)
      endif
      rewind(97)
      end
