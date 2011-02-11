      PROGRAM HWIGPR
C---COMMON BLOCKS ARE INCLUDED AS FILE HERWIG65.INC
      INCLUDE '../HERWIG65.INC'
      include 'LesHouches.h'
      integer n
      real *8 powheginput
      external powheginput
      integer hdecaymode
      logical uevent 
      parameter (uevent=.true.)
c     we need to tell to the analysis file which program is running it
      character * 6 WHCPRG
      integer iun
      common/cWHCPRG/WHCPRG
      WHCPRG='HERWIG'
C---PROCESS; set to negative for user supplied me
      hdecaymode=powheginput('hdecaymode')
      if ((hdecaymode.lt.-1).or.(hdecaymode.gt.12)) then
         write(*,*) "Higgs decay channel not allowed"
         stop
      endif
      if (hdecaymode.eq.-1) then
         iproc=-1600
      else
         iproc=-1600-hdecaymode ! Les Houches interface
      endif
C--- Opens input file and counts number of events, setting MAXEV;
c    MAXEV must be set before HWIGIN call.
      call opencount(MAXEV)
C---INITIALISE OTHER COMMON BLOCKS
      CALL HWIGIN
C---USER CAN RESET PARAMETERS AT
C   THIS POINT, OTHERWISE DEFAULT
C   VALUES IN HWIGIN WILL BE USED.
      MAXPR=2

      ptrms=2.5d0
      write(*,*)
      write(*,*) '*******************************************'
      write(*,*) '*******************************************'
      write(*,*) ' INITIAL p_T SPREADING OF ',ptrms,' GEV    '
      write(*,*) '*******************************************'
      write(*,*) '*******************************************'
      write(*,*)
      if(.not.uevent) then
         PRSOF=0d0
         write(*,*)
         write(*,*) '*******************************************'
         write(*,*) '*******************************************'
         write(*,*) ' NO UNDERLYING EVENT  WILL BE GENERATED    '
         write(*,*) '*******************************************'
         write(*,*) '*******************************************'
         write(*,*)   
      else
         write(*,*)
         write(*,*) '*******************************************'
         write(*,*) '*******************************************'
         write(*,*) ' UNDERLYING EVENT  WILL BE GENERATED     '
         write(*,*) '*******************************************'
         write(*,*) '*******************************************'
         write(*,*)   
      endif
c----DO NOT USE SOFT ME CORRECTION     
      SOFTME=.FALSE.
C---COMPUTE PARAMETER-DEPENDENT CONSTANTS
      CALL HWUINC
C---CALL HWUSTA TO MAKE ANY PARTICLE STABLE
      CALL HWUSTA('PI0     ')
      CALL HWUSTA('TAU-    ')
      CALL HWUSTA('TAU+    ')
C---USER'S INITIAL CALCULATIONS
      CALL HWABEG
C---INITIALISE ELEMENTARY PROCESS
      CALL HWEINI
C---LOOP OVER EVENTS
      DO N=1,MAXEV
C---  INITIALISE EVENT
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
      if((nevhep.gt.0).and.(mod(nevhep,20000).eq.0)) then
         write(*,*) "# of events processed =",nevhep
         call hwaend
      endif
      ENDDO
 111  continue
C---  TERMINATE ELEMENTARY PROCESS
      CALL HWEFIN
      write(*,*) 'At the end NEVHEP is ',nevhep
C---USER'S TERMINAL CALCULATIONS
      CALL HWAEND
      
c      call newunit(iun)
c      open(unit=iun,file='HERWIGcounters.dat',status='unknown')
c      call printcnt(iun)
c      close(iun)

      END

      subroutine UPINIT
      implicit none
      call lhefreadhdr(97)
      end

      subroutine UPEVNT
      include 'LesHouches.h'
      real *8 powheginput
      external powheginput
      integer hdecaymode
      call lhefreadev(97)
      hdecaymode=powheginput('hdecaymode')
      if (hdecaymode.eq.-1) then
         istup(3)=2    ! needed in order not to decay the Higss boson 
      endif
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
      call pwhgsetout
      call pwhgtopout
      close(99)
      end
      

      subroutine hwanal
      INCLUDE '../HERWIG65.INC'
      include 'LesHouches.h'
      integer hdecaymode
      real *8 powheginput
      external powheginput
      if (ierror.ne.0) then
         return
      endif
      if(idwtup.eq.3) xwgtup=xwgtup*xsecup(1)
      hdecaymode=powheginput('hdecaymode')
      if (hdecaymode.gt.0) then
      xwgtup=xwgtup*brhig(hdecaymode)
      endif
      call analysis(xwgtup)
      call pwhgaccumup 
      end
