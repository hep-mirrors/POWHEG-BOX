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
      maxev=0
      file='pwgevents.lhe'
      open(unit=97,file=file,status='old',iostat=ios)
      if(ios.ne.0) then
         write(*,*)' enter name of event file'
         read(*,'(a)') file
         open(unit=97,file=file,status='old')
      endif
      write(*,*) ' Opened event file ',file
      write(*,*) ' Counting events in ', file
      write(*,*) ' This may take some time...'
 1    continue
      read(unit=97,fmt='(a)',end=2) string
      if(string.eq.'</event') then
         maxev=maxev+1
         goto 1
      endif
      goto 1
 2    continue
      write(*,*) ' Found ',maxev,' events in file ',file
      if (maxev.eq.0) then
         write(*,*) ' NO EVENTS!! Program exits'
         call exit(3)
      endif
      rewind(97)
      end
