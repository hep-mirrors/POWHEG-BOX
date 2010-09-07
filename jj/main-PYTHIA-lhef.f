      program main_pythia
      implicit none
      include '../include/LesHouches.h'
      include '../include/hepevt.h'
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
      real * 8 powheginput
      external powheginput
      integer hdecaymode
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      
      WHCPRG='PYTHIA'
      
c PARAMETERS
c     mstj(41)=3 !No photon radiations off leptons
c     mstp(61)=0 !No IS shower
c     mstp(71)=0 !No FS shower
      mstp(81)=0 !No Multiple interactions (MI increases the execution time)
c     mstp(91)=0  !No Primordial kt
c     mstp(131)=0 !No Pile Up
c     mstp(111)=0               !No hadronization
      
c     Make PI0 stable as in herwig default
      mdcy(pycomp(111),1)=0
c     Make tau stable
      mdcy(pycomp(15),1)=0
C---PROCESS; 
c      hdecaymode=powheginput('hdecaymode')
c      if ((hdecaymode.lt.-1).or.(hdecaymode.gt.12)) then
c         write(*,*) "Higgs decay mode not allowed"
c         stop
c      endif   
      hdecaymode=-1

c     choose Higgs decay channel
      if (hdecaymode.eq.-1) then
         mdcy(pycomp(25),1)=0
      else   
         mdcy(pycomp(25),1)=1
         if (hdecaymode.gt.0) then
            do i=210,288
               if (mdme(i,1).ne.-1) mdme(i,1)=0
            enddo
            if (hdecaymode.eq.12) then
               mdme(223,1)=1
            elseif(hdecaymode.eq.11) then
               mdme(225,1)=1
            elseif(hdecaymode.eq.10) then
               mdme(226,1)=1
            elseif(hdecaymode.eq.7) then
               mdme(218,1)=1
            elseif(hdecaymode.eq.8) then
               mdme(219,1)=1
            elseif(hdecaymode.eq.9) then
               mdme(220,1)=1   
            else
               mdme(209+hdecaymode,1)=1
            endif
         endif      
      endif
      
c Set up PYTHIA to accept user processes
      call PYINIT('USER','','',0d0)
      
      call PYABEG
      nevhep=0
      do iev=1,maxev
         call pyevnw
         if(nup.eq.0) goto 111
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
      call PYAEND
      END


      subroutine UPINIT
      implicit none
      integer maxev
      common/mcmaxev/maxev      
C--- Opens input file and counts number of events, setting MAXEV;
      call opencount(maxev)
      rewind(97)
      call lhefreadhdr(97)
      end


      subroutine countevents
      implicit none
      character * 6 string
      integer maxev
      common/mcmaxev/maxev
 1    continue
      read(unit=97,fmt='(a)',end=2) string
      if(string.eq.'<event') then
         maxev=maxev+1
         goto 1
      endif
      goto 1
 2    continue
      write(*,*) ' found ',maxev,' events in file'
      end


      subroutine UPEVNT
      include '../include/LesHouches.h'

      LOGICAL MASSIVE_FS_PARTONS,DEBUGGING
      INTEGER IXX,JXX,NFS
      REAL*8  PARTON_MOM(4,5)
      REAL*8  PTOT(4),PIN(5),POUT(5),MPOUT(5)
      REAL*8  MODP2(5),MASS2(5),SHAT
      REAL*8  SCALING,THRESHOLD,TMP1,TMP2,NUM,DENOM,DIFF

      logical ini
      save ini
      data ini/.true./
      call lhefreadev(97)

      if (ini) then
         ini = .false.
         write(*,*)
         write(*,*) '*****************************'
         write(*,*) '*****************************'
         write(*,*) '*                           *'
         write(*,*) '*  main-HERWIG-lhef: UPEVNT *'
         write(*,*) '*                           *'
         write(*,*) '*  Initialization finished  *'
         write(*,*) '*                           *'
         write(*,*) '*****************************'
         write(*,*) '*****************************'
         write(*,*)
      endif


C - If there are more than 4 partons i.e. 2 or more final state
C - partons and if we are going to be doing rescaling (option
C - MASSIVE_FS_PARTONS=.TRUE.) then we
C - start here:
      MASSIVE_FS_PARTONS=.TRUE.
      DEBUGGING=.FALSE.           ! Suppress debugging output
      IF(NUP.GE.4.AND.MASSIVE_FS_PARTONS.EQV..TRUE.) THEN

C - Initialise incoming, outgoing and total momentum vectors
         DO IXX=1,4
            PTOT(IXX)=0d0
            PIN(IXX) =0d0
            POUT(IXX)=0d0
         ENDDO

C - Calculate initial incoming, outgoing and total momentum in the lab
         DO IXX=1,NUP
            IF(ISTUP(IXX).EQ.-1) THEN
               DO JXX=1,4
                  PTOT(JXX)=PTOT(JXX)+PUP(JXX,IXX)
                  PIN(JXX) =PIN(JXX) +PUP(JXX,IXX)
               ENDDO
            ELSE
               DO JXX=1,4
                  PTOT(JXX)=PTOT(JXX)-PUP(JXX,IXX)
                  POUT(JXX)=POUT(JXX)+PUP(JXX,IXX)
               ENDDO
            ENDIF
         ENDDO

C - Calculate the partonic COM energy
         SHAT=POUT(4)*POUT(4)-POUT(1)*POUT(1)
     $       -POUT(2)*POUT(2)-POUT(3)*POUT(3)
         PIN(5) =SQRT(SHAT)
         POUT(5)=SQRT(SHAT)

C - Show debugging output
         IF(DEBUGGING.OR.(ABS(PTOT(1)).GT.1D-5).OR.
     $                   (ABS(PTOT(2)).GT.1D-5).OR.
     $                   (ABS(PTOT(3)).GT.1D-5).OR.
     $                   (ABS(PTOT(4)).GT.1D-5)) THEN
            WRITE(6,*) ''
            WRITE(6,*) 'UPEVNT: pre rescaling debugging output'
            WRITE(6,*) 'PTOT = ',PTOT(1),PTOT(2),PTOT(3),PTOT(4)
            WRITE(6,*) 'PIN  = ',PIN(1) ,PIN(2) ,PIN(3) ,PIN(4) ,PIN(5)
            WRITE(6,*) 'POUT = ',POUT(1),POUT(2),POUT(3),POUT(4),POUT(5)
            DO IXX=1,NUP
               WRITE(6,*) 
     $           IXX,IDUP(IXX),
     $           PUP(1,IXX),PUP(2,IXX),PUP(3,IXX),PUP(4,IXX),PUP(5,IXX),
     $           PYMASS(ABS(IDUP(IXX)))
              
            ENDDO
         ENDIF

C - Initialise the vectors of |p|, masses and the counters for the
C - threshold energy and number of final state particles 
         NFS=0
         THRESHOLD=0
         DO IXX=1,NUP
            MODP2(IXX)=0
            MASS2(IXX)=0
         ENDDO

C - Work out the momenta and their magnitudes^2, & the HW masses^2 in
C - the partonic COM frame: PARTON_MOM(JXX,IXX), MODP2(IXX),MASS2(IXX)
         DO IXX=1,NUP
            CALL HWULF4(POUT,PUP(1,IXX),PARTON_MOM(1,IXX))
            IF(ISTUP(IXX).NE.-1) THEN
               NFS=NFS+1
               THRESHOLD=THRESHOLD+PYMASS(ABS(IDUP(IXX)))
            ENDIF
            DO JXX=1,3
               MODP2(IXX)=MODP2(IXX)+PARTON_MOM(JXX,IXX)**2
            ENDDO
            MASS2(IXX)=PYMASS(ABS(IDUP(IXX)))**2
         ENDDO

C - If the threshold condition is met then we attempt to solve for the
C - final state momentum rescaling factor using the Newton Raphson method
         SCALING=1D0               ! Initialisation of rescaling factor
         IF(SQRT(SHAT).GE.THRESHOLD) THEN
            TMP1=1D0 ! Initial guess solution (no rescaling: rescaling = 1)
            DIFF=1D0 ! Initialisation of the convergence measure
            DO WHILE(ABS(DIFF/TMP1).GT.1D-10)
               NUM=-SQRT(SHAT)     ! Initialisation of the root equation
               DENOM=0D0           ! Initialisation of its derivative 
               DO IXX=1,NUP
                  IF(ISTUP(IXX).NE.-1) THEN
                     NUM=NUM+SQRT(TMP1**2*MODP2(IXX)+MASS2(IXX))
                     DENOM=DENOM+TMP1*MODP2(IXX)
     $                          /SQRT(TMP1**2*MODP2(IXX)+MASS2(IXX))
                  ENDIF
               ENDDO
               TMP2=TMP1-NUM/DENOM ! New best guess
               DIFF=TMP2-TMP1      ! To test convergence   
               TMP1=TMP2           ! Next trial value equals new best guess
               IF(TMP1.LE.0) THEN
                  WRITE(6,*) 'main-PYTHIA-lhef UPEVNT warning:'
                  WRITE(6,*) 'Negative value encountered in solving'
                  WRITE(6,*) 'for the momentum rescaling factor.'
                  WRITE(6,*) 'Momenta will not be rescaled to the HW'
                  WRITE(6,*) 'mass shell values'
                  GOTO 300
               ENDIF
            ENDDO
            SCALING=TMP1
         ENDIF
C - Rescale the momenta
         DO IXX=1,NUP
            IF(ISTUP(IXX).NE.-1) THEN
               DO JXX=1,3
                  PARTON_MOM(JXX,IXX)=SCALING*PARTON_MOM(JXX,IXX)
               ENDDO
               PARTON_MOM(4,IXX)=
     $              SQRT(PARTON_MOM(1,IXX)*PARTON_MOM(1,IXX)
     $                  +PARTON_MOM(2,IXX)*PARTON_MOM(2,IXX)
     $                  +PARTON_MOM(3,IXX)*PARTON_MOM(3,IXX)
     $                  +MASS2(IXX))
            ENDIF
         ENDDO
C - Boost everything back to the lab frame
 300     DO JXX=1,3
            MPOUT(JXX)=-POUT(JXX)
         ENDDO
         MPOUT(4)=POUT(4)
         MPOUT(5)=POUT(5)
         DO IXX=1,NUP
            CALL HWULF4(MPOUT,PARTON_MOM(1,IXX),PUP(1,IXX))
C - Calculate the mass components as E^2-|p|^2
            PUP(5,IXX)=(PUP(4,IXX)+PUP(3,IXX))*(PUP(4,IXX)-PUP(3,IXX))
     $                - PUP(1,IXX)*PUP(1,IXX) - PUP(2,IXX)*PUP(2,IXX)
C - Sometimes the near massless particles can experience rounding
C - errors in the calculation of their mass components, almost always
C - the initial state ones. This is dealt with here:
            IF(PUP(5,IXX).LT.0D0) THEN
               IF(DEBUGGING.OR.PUP(5,IXX).LT.-1D-6) THEN
               WRITE(6,*) 'main-PYTHIA-lhef: UPEVNT warning'
               WRITE(6,*) 'PUP(JXX,',IXX,') has E^2-|p|^2 = ',PUP(5,IXX)
               WRITE(6,*) 'Setting PUP(JXX,IXX) = -SQRT(',PUP(5,IXX),')'
               ENDIF
               PUP(5,IXX)=SQRT(-PUP(5,IXX))
            ELSE
               PUP(5,IXX)=SQRT( PUP(5,IXX))
            ENDIF
         ENDDO

C - Initialise incoming, outgoing and total momentum vectors
         DO IXX=1,4
            PTOT(IXX)=0d0
            PIN(IXX) =0d0
            POUT(IXX)=0d0
         ENDDO

C - Calculate initial incoming, outgoing and total momentum in the lab
         DO IXX=1,NUP
            IF(ISTUP(IXX).EQ.-1) THEN
               DO JXX=1,4
                  PTOT(JXX)=PTOT(JXX)+PUP(JXX,IXX)
                  PIN(JXX) =PIN(JXX) +PUP(JXX,IXX)
               ENDDO
            ELSE
               DO JXX=1,4
                  PTOT(JXX)=PTOT(JXX)-PUP(JXX,IXX)
                  POUT(JXX)=POUT(JXX)+PUP(JXX,IXX)
               ENDDO
            ENDIF
         ENDDO

C - Recalculate the partonic COM energy
         SHAT=POUT(4)*POUT(4)-POUT(1)*POUT(1)
     $       -POUT(2)*POUT(2)-POUT(3)*POUT(3)
         PIN(5) =SQRT(SHAT)
         POUT(5)=SQRT(SHAT)

C - Show debugging output
         IF(DEBUGGING.OR.(ABS(PTOT(1)).GT.1D-5).OR.
     $                   (ABS(PTOT(2)).GT.1D-5).OR.
     $                   (ABS(PTOT(3)).GT.1D-5).OR.
     $                   (ABS(PTOT(4)).GT.1D-5)) THEN
            WRITE(6,*) 'UPEVNT: post rescaling debugging output'
            WRITE(6,*) 'PTOT = ',PTOT(1),PTOT(2),PTOT(3),PTOT(4)
            WRITE(6,*) 'PIN  = ',PIN(1) ,PIN(2) ,PIN(3) ,PIN(4) ,PIN(5)
            WRITE(6,*) 'POUT = ',POUT(1),POUT(2),POUT(3),POUT(4),POUT(5)
            DO IXX=1,NUP
               WRITE(6,*) 
     $           IXX,IDUP(IXX),
     $           PUP(1,IXX),PUP(2,IXX),PUP(3,IXX),PUP(4,IXX),PUP(5,IXX),
     $           PYMASS(ABS(IDUP(IXX)))
            ENDDO
         ENDIF

      ENDIF

      end


      subroutine upveto
c pythia routine to abort event
      end


      subroutine pyabeg
      call init_hist
      end

c      subroutine pyabeg
c      implicit none
c      include '../include/hepevt.h'
c      nevhep=0
c      call abegin
c      end

      subroutine pyaend
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      open(unit=99,file=pwgprefix(1:lprefix)//'POWHEG+PYTHIA-output.top'
     #     ,status='unknown')
      call pwhgsetout
      call pwhgtopout
      close(99)
      end


      subroutine pyanal
      implicit none
      include '../include/LesHouches.h'
      include '../include/hepevt.h'
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
      real * 8 powheginput,bratio
      external powheginput
      integer hdecaymode

      nevhep=nevhep+1
c      hdecaymode=powheginput('hdecaymode')
      hdecaymode=-1
      if (hdecaymode.eq.0) then
         bratio=1d0
      elseif (hdecaymode.eq.12) then
         bratio=brat(223)
      elseif(hdecaymode.eq.11) then
         bratio=brat(225)
      elseif(hdecaymode.eq.10) then
         bratio=brat(226)
      elseif(hdecaymode.eq.7) then
         bratio=brat(218)
      elseif(hdecaymode.eq.8) then
         bratio=brat(219)
      elseif(hdecaymode.eq.9) then
         bratio=brat(220) 
      elseif(hdecaymode.eq.-1) then
         bratio=1d0  
      else
         bratio=brat(209+hdecaymode)
      endif
      if(idwtup.eq.3) xwgtup=xwgtup*xsecup(1)
      xwgtup=xwgtup*bratio
      call analysis(xwgtup)
      call pwhgaccumup 
      end




CDECK  ID>, HWULF4.
*CMZ :-        -05/11/95  19.33.42  by  Mike Seymour
*-- Author :    Adapted by Bryan Webber
C-----------------------------------------------------------------------
      SUBROUTINE HWULF4(PS,PI,PF)
C-----------------------------------------------------------------------
C     TRANSFORMS PI (GIVEN IN LAB) INTO PF (IN REST FRAME OF PS)
C     N.B. P(1,2,3,4) = (PX,PY,PZ,E); PS(5)=M
C-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION PF4,FN,PS(5),PI(4),PF(4)
      IF (PS(4).EQ.PS(5)) THEN
        PF(1)= PI(1)
        PF(2)= PI(2)
        PF(3)= PI(3)
        PF(4)= PI(4)
      ELSE
        PF4  = (PI(4)*PS(4)-PI(3)*PS(3)
     &         -PI(2)*PS(2)-PI(1)*PS(1))/PS(5)
        FN   = (PF4+PI(4)) / (PS(4)+PS(5))
        PF(1)= PI(1) - FN*PS(1)
        PF(2)= PI(2) - FN*PS(2)
        PF(3)= PI(3) - FN*PS(3)
        PF(4)= PF4
      END IF
      END
