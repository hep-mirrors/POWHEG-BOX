      program leshouchesanal
      implicit none
      include 'include/LesHouches.h'
      integer j,nev
      call opencount(nev)
      call upinit
      call init_hist 
      do j=1,nev
         call upevnt
         call lhuptohepevt(j)
         call analysis(xsecup)
         call pwhgaccumup
         if (mod(j,20000).eq.0) then
            write(*,*) "# of events processed =",j
            open(unit=99,file='LHEF_analysis.top')
            call pwhgtopout(j)
            close(99)
         endif
      enddo
      open(unit=99,file='LHEF_analysis.top')
      call pwhgtopout(nev)
      close(99)
      end
      
      subroutine UPINIT
      implicit none
      call lhefreadhdr(97)
      end
      
      subroutine opencount(maxev)
      implicit none
      include 'include/hepevt.h'
      character * 50 file
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer ios
      character * 8 string
      real * 8 powheginput
      external powheginput
      integer nev,maxev
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
      if(string.eq.'</event>') then
         maxev=maxev+1
         goto 1
      endif
      goto 1
 2    continue
      write(*,*) ' found ',maxev,' events in file'
      rewind(97)
      end

      subroutine UPEVNT
      call lhefreadev(97)
      end

      subroutine lhuptohepevt(n)
      implicit none
      include 'include/hepevt.h'
      include 'include/LesHouches.h'
      integer ihep,mu,n
      
      nhep=nup
      nevhep=n
      do ihep=1,nhep
         isthep(ihep)=istup(ihep)
         idhep(ihep)=idup(ihep)
         do mu=1,5
            phep(mu,ihep)=pup(mu,ihep)
         enddo
      enddo
      end
