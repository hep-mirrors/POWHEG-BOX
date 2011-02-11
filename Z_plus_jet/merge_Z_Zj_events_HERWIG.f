      subroutine SMC_initialize
C---COMMON BLOCKS ARE INCLUDED AS FILE HERWIG65.INC
      include '../HERWIG65.INC'
      include 'LesHouches.h'
      integer n
      logical uevent 
      parameter (uevent=.false.)
c     we need to tell to the analysis file which program is running it
      character * 6 WHCPRG
      integer iun
      common/cWHCPRG/WHCPRG
      WHCPRG='HERWIG'
c     set conventinally lprup(1) to 666
      lprup(1)=666
C---PROCESS; set to negative for user supplied me
      iproc=-1                  ! Les Houches interface
c    MAXEV must be set before HWIGIN call.
      MAXEV=1000000
C---INITIALISE OTHER COMMON BLOCKS
      CALL HWIGIN
C---USER CAN RESET PARAMETERS AT
C   THIS POINT, OTHERWISE DEFAULT
C   VALUES IN HWIGIN WILL BE USED.
c      PTMIN=100.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ptrms=2.5d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      maxpr=0
      maxer = 1000000
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
      end

      subroutine SMC_hadronize(iun,end_of_file)
      implicit none
      include 'LesHouches.h'
      integer iun
      logical end_of_file
      end_of_file = .false.
      call lhefreadev(iun)
      if(nup.eq.0) then
c     reached the last event         
         end_of_file = .true.
         return
      endif
C---  INITIALISE EVENT
         CALL HWUINE
C---GENERATE HARD SUBPROCESS
         CALL HWEPRO
         if(nup.eq.0) goto 777
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
 777     continue
      end

      subroutine SMC_finalize
      implicit none
C---  TERMINATE ELEMENTARY PROCESS
      CALL HWEFIN
C---  USER'S TERMINAL CALCULATIONS
      CALL HWAEND
      end

      subroutine UPINIT
      implicit none
      end

      subroutine UPEVNT
      implicit none
      end


      subroutine hwabeg
      implicit none
      end


      subroutine hwaend
      implicit none
      end
      
      subroutine pdfset
      implicit none
      end
      
      subroutine structm
      implicit none
      end


      subroutine hwanal
      INCLUDE '../HERWIG65.INC'
      real * 8 ptZ_had,ptj1
      common/cptZ/ptZ_had,ptj1
      if (ierror.ne.0) then
c     the event has been killed. Return ptZ_had < 0
         ptZ_had=-1
         ptj1=-1
         return         
      else
         call pre_analysis(1d0)
      endif
      end



      subroutine running_prog(stringa)
      implicit none
      character * 2 stringa
      stringa = 'HW'
      end
