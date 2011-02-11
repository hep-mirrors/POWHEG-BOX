      program main_pythia
      implicit none
      include 'LesHouches.h'
      include 'hepevt.h'
c      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
c     &     JMOHEP,JDAHEP
c      double precision phep,vhep
c      PARAMETER (NMXHEP=4000)
c      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
c     &     JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),
c     &     VHEP(4,NMXHEP)
      real * 8 parp,pari
      integer mstp,msti
      common/pypars/mstp(200),parp(200),msti(200),pari(200)
      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      integer maxev
      common/mcmaxev/maxev
      integer MDCY,MDME,KFDP
      double precision brat
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      integer pycomp
      external pycomp
      integer iev,temp,i
      external pydata
      integer msel,mselpd,msub,kfin
      real *8 ckin
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      real * 8 powheginput
      external powheginput
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      real * 8  Zmass,Zwidth,Zmass2low,Zmass2high,pbeam1,pbeam2
      integer vdecaymode
      character *10 part1,part2 

      WHCPRG='PYTHIA'

C---MAX NUMBER OF EVENTS THIS RUN
      MAXEV=powheginput('numevts')
C---BEAM PARTICLES 
      if(powheginput('ih1').eq.1) then
      PART1='P+'
      elseif (powheginput('ih1').eq.-1) then
      PART1='PBAR-'
      endif
      if(powheginput('ih2').eq.1) then
      PART2='P+'
      elseif (powheginput('ih2').eq.-1) then
      PART2='PBAR-'
      endif
C---BEAM MOMENTA
      PBEAM1=powheginput('ebeam1')
      PBEAM2=powheginput('ebeam2')
      ebmup(1)=PBEAM1
      ebmup(2)=PBEAM2
c---select process
c
      vdecaymode=powheginput('vdecaymode')
      lprup(1)=10000+vdecaymode

      MSEL=11
    
      do i=174,189
         mdme(i,1)=0
      enddo
      
      mdme(171+vdecaymode,1)=1
      Zmass = 91.188d0
      Zwidth = 2.486d0
      Zmass2low = (Zmass-10*Zwidth)**2
      Zmass2high = (Zmass+10*Zwidth)**2

      CKIN(1)=sqrt(Zmass2low)
      CKIN(2)=sqrt(Zmass2high)

      mstu(101)=2 
      paru(103)=(137.035999679)**-1
      paru(102)=abs(1d0-(80.398d0/Zmass)**2)

      
c PARAMETERS
      mstj(41)=3 !No photon radiations off leptons
c     mstp(61)=0 !No IS shower
c     mstp(71)=0 !No FS shower
c      mstp(81)=0 !No Multiple interactions (MI increases the execution time)
c     mstp(91)=0  !No Primordial kt
c     mstp(131)=0 !No Pile Up
c     mstp(111)=0               !No hadronization
      
c     Make PI0 stable as in herwig default
      mdcy(pycomp(111),1)=0
c     Make tau stable
      mdcy(pycomp(15),1)=0
C---PROCESS; 
c Set up PYTHIA to accept user processes
      call PYINIT('CMS',PART1,PART2,PBEAM1+PBEAM2)
      
      call PYABEG
      nevhep=0
      do iev=1,maxev
         call pyevnw
c         if(nup.eq.0) goto 111
c     Convert from PYJETS event record to HEPEVT event record
         temp=nevhep
         call pyhepc(1)
         nevhep=temp
C     Print out the event record
         IF (IEV.le.6) THEN 
c     list the event
c            CALL PYLIST(7)      ! print the HEPEUP common block
c            CALL PYLIST(5)      ! print the HEPEVT common block

            CALL PYLIST(5)      ! print the event

c     call PYLIST(1) ! as PYLIST(2) but with less information
         ENDIF
         
         call PYANAL
         
         IF(MOD(IEV,20000).EQ.0) THEN
            WRITE(*,*)'# of events processed=',NEVHEP
            CALL PYAEND
         ENDIF          
      enddo
 111  continue
      write(*,*) 'At the end NEVHEP is ',nevhep
      call PYAEND2
      END


      subroutine UPINIT
      implicit none
      end


      subroutine UPEVNT
      implicit none
      end


      subroutine upveto
c pythia routine to abort event
      end


      subroutine pyabeg
      call init_hist
      end

c      subroutine pyabeg
c      implicit none
c      include 'hepevt.h'
c      nevhep=0
c      call abegin
c      end

      subroutine pyaend
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      open(unit=99,file=pwgprefix(1:lprefix)//'PYTHIA-alone-output.top'
     #     ,status='unknown')
      call pwhgsetout
      call pwhgtopout
      close(99)
      end


      subroutine pyaend2
      implicit none
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      real * 8 parp,pari
      integer mstp,msti
      common/pypars/mstp(200),parp(200),msti(200),pari(200)
      open(unit=99,file=pwgprefix(1:lprefix)//'PYTHIA-alone-output.top'
     #     ,status='unknown')
      call pwhgsetout
      call pwhgrescale(pari(1)*1d9)
      call pwhgtopout
      close(99)
      end


      subroutine pyanal
      implicit none
      include 'LesHouches.h'
      include 'hepevt.h'
c      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
c     &     JMOHEP,JDAHEP
c      double precision phep,vhep
c      PARAMETER (NMXHEP=4000)
c      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
c     &   JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      integer MDCY,MDME,KFDP
      double precision brat
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      integer pycomp
      external pycomp
      nevhep=nevhep+1
      xwgtup=1d0
      call analysis(xwgtup)
      call pwhgaccumup 
      end

