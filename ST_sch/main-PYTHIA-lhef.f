      program main_pythia
      implicit none
      include '../include/LesHouches.h'
c     pythia common blocks
      real * 8 parp,pari
      integer mstp,msti
      common/pypars/mstp(200),parp(200),msti(200),pari(200)
      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      integer KCHG
      double precision PMAS,PARF,VCKM
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      integer MDCY,MDME,KFDP
      double precision BRAT
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
c     New standard event common
      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
     &     JMOHEP,JDAHEP
      double precision phep,vhep
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &     JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),
     &     VHEP(4,NMXHEP)
c     mcmaxev
      integer maxev
      common/mcmaxev/maxev
c     local
      integer pycomp
      integer maxpr
      integer i,iev,temp
c     external
      real *8 powheginput
      external powheginput
      external pydata
c     multiple interactions
      logical mult_inter
      parameter (mult_inter=.true.)
c     we need to tell to the analysis file which program is running it
      character * 6 WHCPRG
      integer iun
      common/cWHCPRG/WHCPRG
      WHCPRG='PYTHIA'

      call opencount(maxev)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Useful settings to interface POWHEG with PYTHIA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     to use external PDF
c     ...................
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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

c       mstp(64) =3 !use Lambda_MC for IS shower
       mstp(64) =1 !use Lambda_MSbar (default)

c     number of events printed on the shell
      maxpr=2

c     number of warnings printed on the shell
      mstu(26)=20

c     Make PI0 stable as in herwig default
      mdcy(pycomp(111),1)=0
c     set stable lighter b-flavoured states: needed to analize single-top
c     events in a reasonable simple way (see the analize subroutine), but,
c     strictly speaking, not necessary for the program.
      mdcy(pycomp( 521  ) ,1)=0
      mdcy(pycomp( -521 ) ,1)=0
      mdcy(pycomp( 511  ) ,1)=0
      mdcy(pycomp( -511 ) ,1)=0
      mdcy(pycomp( 531  ) ,1)=0
      mdcy(pycomp( -531 ) ,1)=0
      mdcy(pycomp( 5222 ) ,1)=0
      mdcy(pycomp( 5112 ) ,1)=0
      mdcy(pycomp( 5232 ) ,1)=0
      mdcy(pycomp( -5132) ,1)=0
      mdcy(pycomp( 5132 ) ,1)=0
      mdcy(pycomp( 541  ) ,1)=0
      mdcy(pycomp( -541 ) ,1)=0
      mdcy(pycomp( 553  ) ,1)=0
      mdcy(pycomp( -5112) ,1)=0
      mdcy(pycomp( -5222) ,1)=0
      mdcy(pycomp( -5122) ,1)=0
      mdcy(pycomp( 5332 ) ,1)=0
      mdcy(pycomp( -5232) ,1)=0
      mdcy(pycomp( -5332) ,1)=0
      mdcy(pycomp( 5122 ) ,1)=0

c$$$c     PYTHIA masses
c$$$      pmas(pycomp(2),1)=     powheginput('#mup_MC')
c$$$      if(pmas(pycomp(2),1).le.0d0)   pmas(pycomp(2),1)=0.32d0     !as mcnlo input file default
c$$$      pmas(pycomp(1),1)=     powheginput('#mdown_MC')
c$$$      if(pmas(pycomp(1),1).le.0d0)   pmas(pycomp(1),1)=0.32d0     !as mcnlo input file default
c$$$      pmas(pycomp(4),1)=     powheginput('#mcharm_MC')
c$$$      if(pmas(pycomp(4),1).le.0d0)   pmas(pycomp(4),1)=1.55d0     !as mcnlo input file default
c$$$      pmas(pycomp(3),1)=     powheginput('#mstrange_MC')
c$$$      if(pmas(pycomp(3),1).le.0d0)   pmas(pycomp(3),1)=0.5d0      !as mcnlo input file default
c$$$      pmas(pycomp(5),1)=     powheginput('#mbottom_MC')
c$$$      if(pmas(pycomp(5),1).le.0d0)   pmas(pycomp(5),1)=4.95d0     !as mcnlo input file default

c$$$c     PYTHIA ckm matrix: for consistency it should be the
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

c     top decay:
c     relevant only when POWHEG is run with spin correlations switched off;
c     in that case, force t->e ve b decay.
c     force the top to decay always in a W-b
      do i=41,55
         mdme(i,1)=0
      enddo
      mdme(46,1)=1
c     force the W to decay always in a (e,ve)
      do i=190,209
         mdme(i,1)=0
      enddo
      mdme(206,1)=1
c     call PYLIST(12) to see the PYTHIA decay table
ccccccccccccccccccccccccccccccccccccccccccccccccccc

c     Set up PYTHIA to accept user processes
      call PYINIT('USER','','',0d0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     tolerate 2% of killed events
!      mstu(22)=maxev/50
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ccccccccccccccccccccccccccccccccccccccccccccccccccc
C---USER'S INITIAL CALCULATIONS
      call PYABEG
C---LOOP OVER EVENTS       
      do iev=1,maxev
         call pyevnw
         if(nup.eq.0) goto 111
c     Convert from PYJETS event record to HEPEVT event record
         temp=nevhep
         call pyhepc(1)
         nevhep=temp
cccccccccccccccccccccccccccccccccccccccccccccccccc         
C     Print out the event record

         IF (IEV.le.maxpr) THEN 
c     print the event
c            call pystat(2)      ! print cross sections, widths, branchings,...
c            CALL PYLIST(7)      ! print the HEPEUP common block
            CALL PYLIST(5)       ! print the HEPEVT common block
c            CALL PYLIST(2)      ! print the event
c            call PYLIST(1)      ! as PYLIST(2) but with less information
         ENDIF
ccccccccccccccccccccccccccccccccccccccccccccccccc
C---USER'S EVENT ANALYSIS
         call PYANAL
         IF(nevhep.gt.0.and.MOD(nevhep,20000).EQ.0) THEN
            WRITE(*,*)'# of events processed=',NEVHEP
            CALL PYAEND
         ENDIF 
      enddo
 111  continue
      write(*,*) 'At the end NEVHEP is ',nevhep
C---USER'S TERMINAL CALCULATIONS
      call PYAEND
      END


      subroutine UPINIT
      implicit none
      call lhefreadhdr(97)
      end

      subroutine UPEVNT
      implicit none
      call lhefreadev(97)
      end


      subroutine upveto
c pythia routine to abort event
      end

      subroutine pyabeg
      implicit none
      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
     &     JMOHEP,JDAHEP
      double precision phep,vhep
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &   JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      nevhep=0
      call init_hist
      end

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
c     pythia common blocks
      integer NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,
     &     JMOHEP,JDAHEP
      double precision phep,vhep
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &   JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      integer mint
      double precision vint
      COMMON/PYINT1/MINT(400),VINT(400)
      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)

      integer idummy
      real *8 totbr
      logical firstcall
      save totbr,firstcall
      data firstcall/.true./
      data totbr/1d0/
c     check parameters
      logical verbose
      parameter (verbose=.true.)
      
      if(mint(51).ne.0) then
         if(verbose) then
            write(*,*) 'Killed event'
            write(*,*) 'Scalup= ',scalup
            write(*,*) idup(1),idup(2),nup
c$$$            if(scalup.gt.4.95d0) then
c$$$               write(*,*) 'KILLED AN EVENT WITH scalup > (4.95*1.05)'
c$$$            endif

c$$$            call pylist(7)      !hepeup
c$$$            call pylist(2)      !all the event
c$$$            write(*,*) 'current error:   mstu(23),mstu(24) ',
c$$$     #           mstu(23),mstu(24)
c$$$            write(*,*) 'current warning: mstu(27),mstu(28) ',
c$$$     #           mstu(27),mstu(28)
         endif
         return
      endif
      nevhep=nevhep+1

      if(firstcall) then
         call pickwdecay(-1000,idummy,idummy,idummy,totbr)
         write(*,*) 'Total branching ratio used in analysis: ',totbr
         firstcall=.false.
      endif

      xwgtup=xwgtup*xsecup(1) *totbr
      call analysis(xwgtup)
      call pwhgaccumup 
      end


      subroutine pickwdecay(iw1,mdecw1,iw2,mdecw2,totbr)
c     !: originally taken from POWHEG-hvq
c     Finds which decays to choose with correct probability, according
c     to topdecaymode. It returns always particle ids of W+ decay.
c     iw1, iw2 refer to the pdg ids of W+ decay products 1 and 2;
c     by convention 1 is down type (e,mu,tau,d,s) and 2 is up type.
c     topdecaymode has to be an integer with 5 digits that are either 0 or 1
c     and they represent respectively the maximum number of the following particles
c     (antiparticles) in the top decay final state:
c     e  mu tau up charm
c     Relevant examples:
c     11111    All decays
c     leptonic:
c     10000    t->(b e ve) (with the appropriate signs)
c     11000    t->(b e ve) or t->(b mu vmu) (with the appropriate signs)
c     11100    fully leptonic
c     hadronic:
c     00010    t->(b u d) (with the appropriate signs)
c     00001    t->(b c s) (with the appropriate signs)
c     00011    fully hadronic
      implicit none
      include 'PhysPars.h'
      integer iw1,iw2
      real * 8 mdecw1,mdecw2,totbr
c     local
      integer iwa(2)
      real * 8 prbs(1:5),totprbs(0:5),mass(16),sin2cabibbo,ebr,hbr,r
      integer ini,ii(5),j,k,imode,iwp(5,2)
      data ini/0/
c     pdg id's of the W+ decay products for e,mu,tau,up and charm decays (ignoring CKM)
      data ((iwp(j,k),k=1,2),j=1,5)/-11,12, -13,14, -15,16, -1,2, -3,4/
c     external
      real * 8 random,powheginput
      external random,powheginput
c     save
      save ini,totprbs,iwp,mass,sin2cabibbo
      if(ini.eq.2) return
      if(ini.eq.0.or.iw1.eq.-1000) then
         ini=1
c     on first run look for decay mode in powheginput
         imode=powheginput('topdecaymode')
         if(imode.le.0) then
            write(*,*) 'Invalid value for tdecaymode, in pickwdecay'
            call exit(1)
         endif
c$$$         if(imode.eq.0) then
c$$$            ini=2
c$$$            return
c$$$         endif
         ii(1)=imode/10000
         imode=imode-ii(1)*10000
         ii(2)=imode/1000
         imode=imode-ii(2)*1000
         ii(3)=imode/100
         imode=imode-ii(3)*100
         ii(4)=imode/10
         imode=imode-ii(4)*10
         ii(5)=imode
c     load from input card the branching t->(b l vl) (only one lepton flavour)
         ebr=powheginput('tdec/elbranching')
c     from ebr calculates the hadronic branching t->(b u d)
         hbr=(1-3*ebr)/2
         do j=1,5
            if(ii(j).eq.0) then
               prbs(j)=0
            else
               if(j.le.3) then
                  prbs(j)=ebr
               else
                  prbs(j)=hbr
               endif
            endif
         enddo
c     now in prbs(j) there is the branching ratio assumed by the program for the
c     j-type decay. If prbs(j)=0, the corresponding decay channel will be closed.
         totprbs(0)=0d0
         do j=1,5
            totprbs(j)=prbs(j)+totprbs(j-1)
         enddo

         totbr=totprbs(5)

c     mass of decay products. For internal consistency, here one should use
c     the masses assumed by the shower. Leptonic W decay products masses have to be
c     assigned here. The 3 light quarks are assumed massless.
         mass(11)=powheginput('tdec/emass')
         mass(13)=powheginput('tdec/mumass')
         mass(15)=powheginput('tdec/taumass')
         mass(12)=0
         mass(14)=0
         mass(16)=0
         mass(1)=0
         mass(2)=0
         mass(3)=0
         mass(4)=powheginput('tdec/cmass')
         mass(5)=powheginput('tdec/bmass')
         sin2cabibbo=(CKM_pow(1,2))**2
         return
      endif
c     end initialization

      r=random()*totprbs(5)
      do j=1,5
         if(r.lt.totprbs(j)) goto 1
      enddo
 1    continue
c     now we have j decay mode
      if(j.gt.5) then
         write(*,*) 'Error in pickwdecay, j',r,totprbs
         call exit(1)
      endif

c     W decay products
      iwa(1)=iwp(j,1)
      iwa(2)=iwp(j,2)
c     if any W decay product is down (or strange), it may turn to
c     strange (or down) with a probability sin^2 theta
      do j=1,2
         if(abs(iwa(j)).eq.1) then
            if(random().lt.sin2cabibbo) then
               iwa(j)=sign(3,iwa(j))
            endif
         elseif(abs(iwa(j)).eq.3) then
            if(random().lt.sin2cabibbo) then
               iwa(j)=sign(1,iwa(j))
            endif
         endif
      enddo
      iw1=iwa(1)
      iw2=iwa(2)
      mdecw1=mass(abs(iw1))
      mdecw2=mass(abs(iw2))
      end
