      PROGRAM HWIGPR
C---COMMON BLOCKS ARE INCLUDED AS FILE HERWIG65.INC
      INCLUDE '../HERWIG65.INC'
      include '../include/LesHouches.h'
      integer n
      logical uevent
      parameter (uevent=.true.)
c     we need to tell to the analysis file which program is running it
      character * 6 WHCPRG
      integer iun
      common/cWHCPRG/WHCPRG
      WHCPRG='HERWIG'
C---PROCESS; set to negative for user supplied me
      iproc=-1                  ! Les Houches interface
C--- Opens input file and counts number of events, setting MAXEV;
c    MAXEV must be set before HWIGIN call.
      call opencount(MAXEV)
C---INITIALISE OTHER COMMON BLOCKS
      CALL HWIGIN
C---USER CAN RESET PARAMETERS AT
C   THIS POINT, OTHERWISE DEFAULT
C   VALUES IN HWIGIN WILL BE USED.
c      PTMIN=100.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      print *
      print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      ptrms=2.5d0
      print *,'Initial pt-spreading=',ptrms,' GeV'
      print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      print *
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      maxpr=2
      if(.not.uevent) PRSOF=0d0
c----DO NOT USE SOFT ME CORRECTION     
      SOFTME=.FALSE.
C---COMPUTE PARAMETER-DEPENDENT CONSTANTS
      CALL HWUINC
C---CALL HWUSTA TO MAKE ANY PARTICLE STABLE
      CALL HWUSTA('PI0     ')
c      CALL HWUSTA('HIGGS   ')
C---USER'S INITIAL CALCULATIONS
      CALL HWABEG
C---INITIALISE ELEMENTARY PROCESS
      CALL HWEINI
C---LOOP OVER EVENTS
      DO N=1,MAXEV
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
      if((nevhep.gt.0).and.(mod(nevhep,20000).eq.0)) then
         write(*,*) "# of events processed =",nevhep
         call hwaend
      endif
      ENDDO
 111  continue
C---TERMINATE ELEMENTARY PROCESS
      CALL HWEFIN
      write(*,*) 'At the end NEVHEP is ',nevhep
C---USER'S TERMINAL CALCULATIONS
      CALL HWAEND
      END

      subroutine UPINIT
      implicit none
      call lhefreadhdr(97)
      end

      subroutine UPEVNT
      implicit none
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
      call pwhgsetout
      call pwhgtopout
      close(99)
      end
      

      subroutine hwanal
      INCLUDE '../HERWIG65.INC'
      include '../include/LesHouches.h'
c     check parameters
      logical verbose
      parameter (verbose=.false.)

      if (ierror.ne.0) then
         if(verbose) then
            write(*,*) 'Killed event'
            write(*,*) 'Scalup= ',scalup
            call HWUPUP         !hepeup
            call hwuepr         !all the event
         endif
         return
      endif
      xwgtup=xwgtup*xsecup(1)
      call analysis(xwgtup)
      call pwhgaccumup 
      end
