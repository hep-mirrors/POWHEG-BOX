      subroutine SMC_initialize
      implicit none
      include '../include/LesHouches.h'
c     pythia common blocks
      real * 8 parp,pari
      integer mstp,msti
      common/pypars/mstp(200),parp(200),msti(200),pari(200)
      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      integer MDCY,MDME,KFDP
      double precision brat
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      integer pycomp
      external pycomp
c     New standard event common
      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
     &     JMOHEP,JDAHEP
      double precision phep,vhep
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &     JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),
     &     VHEP(4,NMXHEP)
c     multiple interactions
      logical mult_inter
      parameter (mult_inter=.false.)
c     to use the proper analysis procedure
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      WHCPRG='PYTHIA'
c     set conventinally lprup(1) to 666
      lprup(1)=666      

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     multiple interactions
c     (MI can increase a lot the execution time)
      if(.not.mult_inter) mstp(81)=0   !No Multiple interactions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     photon radiation off leptons
      mstj(41)=3                !No photon radiations off leptons
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

c      mstp(61)=0                !No IS shower
c      mstp(71)=0                !No FS shower
c      mstp(91)=0                !No Primordial kt
c      mstp(131)=0               !No Pile Up
c      mstp(111)=0               !No hadronization


c     Make PI0 stable as in pythia default
      mdcy(pycomp(111),1)=0
c     Make tau stable to use our analysis routine
      mdcy(pycomp(15),1)=0

ccccccccccccccccccccccccccccccccccccccccccccc
c Set up PYTHIA to accept user processes
      call PYINIT('USER','','',0d0)

c     maximum number of errors before pythia aborts (def=10)
      mstu(22)=10
c     number of warnings printed on the shell
      mstu(26)=20


C---USER'S INITIAL CALCULATIONS
      call PYABEG
ccccccccccccccccccccccccccccccccccccccccccccc
      end

      subroutine SMC_hadronize(iun,end_of_file)
      implicit none
      include '../include/LesHouches.h'
      integer iun
      logical end_of_file

      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
     &     JMOHEP,JDAHEP
      double precision phep,vhep
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &   JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)

      integer iev,maxpr,temp
      data iev/0/
      data maxpr/2/
      save iev,maxpr
      integer iunit
      common/ciunit/iunit

      iunit = iun   ! set unit that upevnt will read
      end_of_file = .false.
      iev=iev+1
      call pyevnw
      if(nup.eq.0) then
c     reached the last event         
         end_of_file = .true.
         return
      endif
      
c     Convert from PYJETS event record to HEPEVT event record
      temp=nevhep
      call pyhepc(1)
      nevhep=temp
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     Print out the event record

      if(iev.le.maxpr) then
c     print the even
c            call pystat(2)      ! print cross sections, widths, branchings,...
c            CALL PYLIST(7)      ! print the HEPEUP common block
         CALL PYLIST(5)          ! print the HEPEVT common block
c            CALL PYLIST(2)      ! print the event
c            call PYLIST(1)      ! as PYLIST(2) but with less information
      endif
cccccccccccccccccccccccccccccccccccccccccccccccccccc
      call PYANAL
      end

      subroutine SMC_finalize
      implicit none
      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
     &     JMOHEP,JDAHEP
      double precision phep,vhep
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &   JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      call PYAEND
      end




      subroutine UPINIT
      implicit none
      end

      subroutine UPEVNT
c     !ER: cambiato
      implicit none
      include '../include/LesHouches.h'
      integer iunit
      common/ciunit/iunit
      real * 8 mass_elect
      common /cmass_elect/mass_elect
      call lhefreadev(iunit)
      idprup=lprup(1)
      if (mass_elect.ne.0d0) then
         call momenta_reshuffle(3,4,5,mass_elect)
      endif
      end

      subroutine upveto
c pythia routine to abort event
      end

      subroutine pyabeg
      implicit none
      end

      subroutine pyaend
      implicit none
      end
      
      subroutine pdfset
      implicit none
      end
      
      subroutine structm
      implicit none
      end

      subroutine structp
      implicit none
      end


      subroutine pyanal
      implicit none
      include '../include/LesHouches.h'

      real * 8 ptZ_had,ptj1
      common/cptZ/ptZ_had,ptj1

      integer mint
      double precision vint
      COMMON/PYINT1/MINT(400),VINT(400)

      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)

      logical verbose
      parameter (verbose=.true.)

c$$$      integer iunit
c$$$      common/ciunit/iunit

c     'error code' in pythia is mint(51).ne.0
c     This condition does not exclude all possible errors, and
c     does not exclude warnings !!!!
c      if(mint(51).ne.0) then
c         if(verbose) then
c            write(*,*) 'Killed event'
c            write(*,*) 'Scalup= ',scalup
c            call pylist(7)      !hepeup
c            write(*,*) 'current error: mstu(22),mstu(23),mstu(24) ',
c     $           mstu(22),mstu(23),mstu(24)
c         endif
c     the event has been killed. Return ptZ_had < 0
c         ptZ_had=-1
c         ptj1=-1
c         return
c      endif

c$$$      call PYLIST(1) 
c$$$      CALL PYLIST(5)


      call pre_analysis(1d0)


c$$$      print*, '>>>> iunit ',iunit
c$$$      print*, 'ptz_had',ptz_had
      end




      subroutine running_prog(stringa)
      implicit none
      character * 2 stringa
      stringa = 'PY'
      end
