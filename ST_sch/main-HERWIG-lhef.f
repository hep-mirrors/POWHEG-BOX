      PROGRAM HWIGPR
C---COMMON BLOCKS ARE INCLUDED AS FILE herwig6510.h
      INCLUDE 'HERWIG65.INC'
      include '../include/LesHouches.h'
c     local
      integer n
c     external
      real *8 powheginput
      external powheginput
c     underlying event
      logical uevent
      parameter (uevent=.true.)
c     we need to tell to the analysis file which program is running it
      character * 6 WHCPRG
      integer iun
      common/cWHCPRG/WHCPRG
      WHCPRG='HERWIG'

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Read hard event from LH interface

c     set iproc to negative for user supplied me
      iproc=-1
C     opens input file and counts number of events, setting MAXEV;
c     MAXEV must be set before HWIGIN call.
      call opencount(MAXEV)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C---INITIALISE OTHER COMMON BLOCKS
      CALL HWIGIN
C---USER CAN RESET PARAMETERS AT
C   THIS POINT, OTHERWISE DEFAULT
C   VALUES IN HWIGIN WILL BE USED.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Useful settings to interface POWHEG with HERWIG


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     to use external PDF
c$$$      MODPDF(1)=10050
c$$$      MODPDF(2)=10050
c$$$      AUTPDF(1)='HWLHAPDF'
c$$$      AUTPDF(2)='HWLHAPDF'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     do not use soft me correction
      SOFTME=.FALSE.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     tolerate 2% of killed events (default is 1%)
      MAXER=MAXEV/50
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     underlying event
      if(.not.uevent) PRSOF=0d0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!
c     intrinsic pt
      ptrms=2.5d0
      write(*,*) '**************************'
      write(*,*) 'Initial pt-spreading=',ptrms,' GeV'
      write(*,*) '**************************'
!!!!!!!!!!!!!!!!!!!!!!!!!!

c     number of events printed on the shell
      maxpr=2

c     do not print vertexes
      prvtx=.false.

c$$$c     HERWIG masses
c$$$      RMASS(6) = powheginput('topmass')
c$$$      RMASS(12)=RMASS(6)
c$$$      RMASS(2)=     powheginput('#mup_MC')
c$$$      if(RMASS(2).le.0d0)   RMASS(2)=0.32d0     !as mcnlo input file default
c$$$      RMASS(1)=     powheginput('#mdown_MC')
c$$$      if(RMASS(1).le.0d0)   RMASS(1)=0.32d0     !as mcnlo input file default
c$$$      RMASS(4)=     powheginput('#mcharm_MC')
c$$$      if(RMASS(4).le.0d0)   RMASS(4)=1.55d0     !as mcnlo input file default
c$$$      RMASS(3)=     powheginput('#mstrange_MC')
c$$$      if(RMASS(3).le.0d0)   RMASS(3)=0.5d0      !as mcnlo input file default
c$$$      RMASS(5)=     powheginput('#mbottom_MC')
c$$$      if(RMASS(5).le.0d0)   RMASS(5)=4.95d0     !as mcnlo input file default
c$$$      do n=1,5
c$$$         RMASS(n+6)=RMASS(n)    !antiparticles
c$$$      enddo

c$$$c     HERWIG ckm matrix: for consistency it should be the
c$$$c     same as the one used in POWHEG
c$$$      VCKM(1,1)=powheginput('CKM_ud')
c$$$      VCKM(1,2)=powheginput('CKM_us')
c$$$      VCKM(1,3)=powheginput('CKM_ub')
c$$$      VCKM(2,1)=powheginput('CKM_cd')
c$$$      VCKM(2,2)=powheginput('CKM_cs')
c$$$      VCKM(2,3)=powheginput('CKM_cb')
c$$$      VCKM(3,1)=powheginput('CKM_td')
c$$$      VCKM(3,2)=powheginput('CKM_ts')
c$$$      VCKM(3,3)=powheginput('CKM_tb')

c     set stable lighter b-flavoured states: see the following calls to HWUSTA

c     top decay:
c     relevant only when POWHEG is run with spin correlations switched off;
c     in that case, force t->e ve b decay.
c     see the following calls to HWMODK
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C---COMPUTE PARAMETER-DEPENDENT CONSTANTS
      CALL HWUINC
C---CALL HWUSTA TO MAKE ANY PARTICLE STABLE
      CALL HWUSTA('PI0     ')

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     the following HWUSTA calls are exactly as in MC@NLO.
c     They are needed to analize single-top events in a reasonable simple way
c     (see the analize subroutine), but, strictly speaking, they are not necessary
c     for the program.
      CALL HWUSTA('B+      ')
      CALL HWUSTA('B-      ')
      CALL HWUSTA('B_D0    ')
      CALL HWUSTA('B_DBAR0 ')
      CALL HWUSTA('B_S0    ')
      CALL HWUSTA('B_SBAR0 ')
      CALL HWUSTA('SIGMA_B+')
      CALL HWUSTA('SIGMA_B-')
      CALL HWUSTA('XI_B0   ')
      CALL HWUSTA('XI_B+   ')
      CALL HWUSTA('XI_B-   ')
      CALL HWUSTA('B_C+    ')
      CALL HWUSTA('B_C-    ')
      CALL HWUSTA('UPSLON1S')
      CALL HWUSTA('SGM_BBR+')
      CALL HWUSTA('SGM_BBR-')
      CALL HWUSTA('LMD_BBR0')
      CALL HWUSTA('OMEGA_B-')
      CALL HWUSTA('XI_BBAR0')
      CALL HWUSTA('OMG_BBR+')
      CALL HWUSTA('LMBDA_B0')

c     Needed to force t->e ve b decay.
      CALL HWMODK(6,ONE,100,12,-11,5,0,0)
      CALL HWMODK(-6,ONE,100,-12,11,-5,0,0)
c     call HWIODK to see the HERWIG decay table
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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
      if (nevhep.gt.0.and.mod(nevhep,20000).eq.0) then
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



c$$$C-----------------------------------------------------------------------
c$$$      SUBROUTINE STRUCTM(X,QSCA,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
c$$$C-----------------------------------------------------------------------
c$$$C     DUMMY SUBROUTINE: DELETE IF YOU USE PDFLIB CERN-LIBRARY
c$$$C     PACKAGE FOR NUCLEON STRUCTURE FUNCTIONS
c$$$C-----------------------------------------------------------------------
c$$$      IMPLICIT NONE
c$$$      DOUBLE PRECISION X,QSCA,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU
c$$$      WRITE (6,10)
c$$$  10  FORMAT(/10X,'STRUCTM CALLED BUT NOT LINKED')
c$$$      STOP
c$$$      END
c$$$C-----------------------------------------------------------------------
c$$$
c$$$
c$$$C----------------------------------------------------------------------
c$$$      SUBROUTINE PDFSET(PARM,VAL)
c$$$C----------------------------------------------------------------------
c$$$C     DUMMY SUBROUTINE: DELETE AND SET MODPDF(I)
c$$$C     IN MAIN PROGRAM IF YOU USE PDFLIB CERN-LIBRARY
c$$$C     PACKAGE FOR NUCLEON STRUCTURE FUNCTIONS
c$$$C----------------------------------------------------------------------
c$$$      IMPLICIT NONE
c$$$      DOUBLE PRECISION VAL(20)
c$$$      CHARACTER*20 PARM(20)
c$$$      WRITE (6,10)
c$$$   10 FORMAT(/10X,'PDFSET CALLED BUT NOT LINKED')
c$$$      STOP
c$$$      END
c$$$C-----------------------------------------------------------------------




      subroutine UPINIT
      implicit none
      call lhefreadhdr(97)
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
      call pwhgsetout
      call pwhgtopout
      close(99)
      end


      subroutine hwanal
      INCLUDE 'HERWIG65.INC'
      include '../include/LesHouches.h'
c     check parameters
      logical verbose
      parameter (verbose=.false.)

      if (ierror.ne.0) then
         if(verbose) then
            write(*,*) 'Killed event'
            write(*,*) 'Scalup= ',scalup
            write(*,*) idup(1),idup(2),nup
c$$$            if(scalup.gt.4.95d0) then
c$$$               write(*,*) 'KILLED AN EVENT WITH scalup>4.95'
c$$$            endif

c$$$            call HWUPUP         !hepeup
c$$$            call hwuepr         !all the event
         endif
         return
      endif
      
      if(idwtup.eq.3) xwgtup=xwgtup*xsecup(1)
      call analysis(xwgtup)
      call pwhgaccumup 
      end


