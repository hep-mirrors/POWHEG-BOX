      PROGRAM HWIGPR
C---COMMON BLOCKS ARE INCLUDED AS FILE herwig6510.h
      INCLUDE 'herwig6510.inc'
      include 'include/LesHouches.h'
      integer n
      logical uevent 
      parameter (uevent=.false.)

C---PROCESS; set to negative for user supplied me
      iproc=-1                  ! Les Houches interface
C--- Opens input file and counts number of events, setting MAXEV;
c    MAXEV must be set before HWIGIN call.
      call opencount
C---INITIALISE OTHER COMMON BLOCKS
      CALL HWIGIN
C---USER CAN RESET PARAMETERS AT
C   THIS POINT, OTHERWISE DEFAULT
C   VALUES IN HWIGIN WILL BE USED.
c      PTMIN=100.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ptrms=2.5d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      maxpr=2
      if(.not.uevent) PRSOF=0d0
c----DO NOT USE SOFT ME CORRECTION     
      SOFTME=.FALSE.
C---COMPUTE PARAMETER-DEPENDENT CONSTANTS
      CALL HWUINC
C---CALL HWUSTA TO MAKE ANY PARTICLE STABLE
      CALL HWUSTA('PI0     ')
      CALL HWUSTA('HIGGS   ')
C---USER'S INITIAL CALCULATIONS
      CALL HWABEG
C---INITIALISE ELEMENTARY PROCESS
      CALL HWEINI
C---LOOP OVER EVENTS
      DO N=1,maxev
C---INITIALISE EVENT
      CALL HWUINE
C---GENERATE HARD SUBPROCESS
      CALL HWEPRO
      if(nup.eq.0) goto 111
C---GENERATE PARTON CASCADES
      CALL HWBGEN
C---DO HEAVY OBJECT DECAYS
      CALL HWDHOB
C---DO CLUSTER FORMATION
      CALL HWCFOR
C---DO CLUSTER DECAYS
      CALL HWCDEC
C---DO UNSTABLE PARTICLE DECAYS
      CALL HWDHAD
C---DO HEAVY FLAVOUR HADRON DECAYS
      CALL HWDHVY
C---ADD SOFT UNDERLYING EVENT IF NEEDED
      CALL HWMEVT
C---FINISH EVENT
      CALL HWUFNE    
C---USER'S EVENT ANALYSIS
      CALL HWANAL
      if (mod(nevhep,20000).eq.0) then
         write(*,*) "# of events processed =",nevhep
         call hwaend
      endif
      ENDDO
 111  continue
C---TERMINATE ELEMENTARY PROCESS
      CALL HWEFIN
C---USER'S TERMINAL CALCULATIONS
      CALL HWAEND
      END

      subroutine UPINIT
      implicit none
      call lhefreadhdr(97)
      end

      subroutine opencount
c      implicit none
c      include 'include/hepevt.h'
      INCLUDE 'herwig6510.inc'
      character * 50 file
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer ios
      character * 6 string
      real * 8 powheginput
      external powheginput
      integer nev
c     this call is necessary to read the prefix of the file
      nev=powheginput('maxev')
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
      if(string.eq.'<event') then
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


      subroutine hwabeg
      call init_hist
      end

      subroutine hwaend
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      open(unit=99,file=pwgprefix(1:lprefix)//'POWHEG+HERWIG-output.top'
     #     ,status='unknown')
      call topout
      close(99)
      end
      

      subroutine hwanal
      INCLUDE 'herwig6510.inc'
      include 'include/LesHouches.h'
      if (ierror.ne.0) then
         return
      endif
      xwgtup=xwgtup*xsecup(1)
      call analysis(xwgtup)
      end
