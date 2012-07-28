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
      integer  lenocc,j,ifile
      external powheginput,lenocc
      integer nev
      maxev=0
      if(powheginput('#manyseeds').eq.1) then
         write(*,*) 'enter file number'
         read(*,*) ifile
      else
         ifile = -1
      endif
      if(ifile.eq.-1) then
         file=pwgprefix(1:lprefix)//'events.lhe'
      else
         file=pwgprefix(1:lprefix)//'events-0000.lhe'
         write(file(lprefix+8:lprefix+8),'(i4)') ifile
         do j=lprefix+8,lprefix+11
            if(file(j:j).eq.' ') file(j:j) = '0'
         enddo
      endif
      open(unit=97,file=file,status='old',iostat=ios)
      if(ios.ne.0) then
         write(*,*)' file not found:',file(1:lenocc(file))
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
