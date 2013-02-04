      subroutine setup_PYTHIA_tune
      implicit none
      real * 8 powheginput
      external powheginput
      integer pythiatune
C --------------------------------------------------------------- C
C - N.B. PYTUNE(ITUNE) must be called before the call to PYINIT - C
C --------------------------------------------------------------- C
C -  100         A :  Rick Field's CDF Tune A     (Oct 2002)
C -  103        DW :  Rick Field's CDF Tune DW    (Apr 2006)
C -  320 Perugia 0 :  Perugia update of S0-Pro    (Feb 2009)      
      
      pythiatune=powheginput('#pythiatune')
      if (pythiatune.lt.0) then
         pythiatune=340
      endif         
      call PYTUNE(pythiatune)
      end

      subroutine setup_PYTHIA_parameters
      implicit none
      include 'hepevt.h'
      include 'LesHouches.h'
      double precision parp,pari
      integer mstp,msti
      common/pypars/mstp(200),parp(200),msti(200),pari(200)
      integer MSTU,MSTJ
      double precision PARU,PARJ
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      integer MDCY,MDME,KFDP
      double precision brat
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      integer MRPY
      double precision RRPY
      COMMON/PYDATR/MRPY(6),RRPY(100)
      integer pycomp
      external pycomp
      integer maxev
      common/mcmaxev/maxev
      real * 8 scalupfact
      common/cscalupfac/scalupfact
      real * 8 powheginput
      external powheginput
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer iseed,ios

c$$$      if(lprefix.eq.7.and.pwgprefix(1:3).eq.'pwg') then
c$$$         read(pwgprefix(4:7),fmt='(i4)',iostat=ios) iseed
c$$$         if(ios.eq.0) then
c$$$c initialize random seed
c$$$            write(*,*) ' *****************************************'
c$$$            write(*,*) ' initializing PYTHIA random seed to ',iseed
c$$$            write(*,*) ' *****************************************'
c$$$            mrpy(1)=iseed+10
c$$$            mrpy(2)=0
c$$$         endif
c$$$      endif
c$$$
      scalupfact=powheginput('#scalupfact')
      if(scalupfact.gt.0) then
         write(*,*)' ********* SCALUP scale multiplied by ',scalupfact
      endif
c      mstj(41) =3 ! Photon radiation off leptons. Not recommended to touch
c                  ! this -- causes interference with the pT ordered shower.
c      mstp(61) =0 ! No IS shower
c      mstp(71) =0 ! No FS shower
c      mstp(81) =0 ! No Multiple interactions (MI increases execution time).
c      mstp(91) =0 ! No Primordial kt
c      mstp(131)=0 ! No Pile Up
c      mstp(111)=0 ! No hadronization
      
c     N.B.
c     ====
c     For the case of jet production the following parameter setting
c     limits the transverse momentum of secondary scatterings, due
c     to multiple parton interactions, to be less than that of the
c     primary interaction (see POWHEG Dijet paper arXiv:1012.3380
c     [hep-ph] sec. 4.1 and also the PYTHIA Manual).
      mstp(86)=1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     maximum number of errors before pythia aborts (def=10)
      mstu(22)=1000
c     number of warnings printed on the shell
      mstu(26)=20
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c Hadronization off
      mstp(111)=0
c primordial kt off
      mstp(91)=0
c No multiple parton interactions
      if(mstp(81).eq.1) then
c Q2 ordered shower
         mstp(81)=0
      elseif(mstp(81).eq.21) then
c p_t^2 ordered shower
         mstp(81)=20
      endif
      end

      subroutine getmaxev(maxev)
      integer maxev
C--- Opens input file and counts number of events, setting MAXEV;
      call opencount(maxev)
      end

      subroutine UPINIT
      implicit none
      include 'hepevt.h'
      include 'LesHouches.h'
      double precision parp,pari
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
      integer maxev
      common/mcmaxev/maxev
      nevhep=0
c read the header first, so lprup is set
      call lhefreadhdr(97)

c     Make PI0 stable as in herwig default
      mdcy(pycomp(111),1)=0
c     Make tau stable
      mdcy(pycomp(15),1)=0 ! tau stable. Done by Tauola too
      end

      subroutine UPEVNT
      implicit none
      include 'LesHouches.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_rad.h'
      real * 8 scalupfact
      common/cscalupfac/scalupfact
      real * 8 ptmin, pcm(0:3,5), beta, vec(3)
      integer k,mu,j
      real * 8 ptkj2
      real * 8 dotp
      external dotp
      logical ini, changescalup
      save ini, changescalup
      real * 8 powheginput
      external powheginput
      data ini/.true./
      call lhefreadev(97)
      
      if (ini) then
         if(powheginput('#changescalup').eq.1d0) then
            changescalup=.true.
            write(*,*) '*************************************'
            write(*,*) 'scalup set to the min pt in the event'
            write(*,*) '*************************************'
         else
            changescalup=.false.
         endif
         ini=.false.
      endif
      
      if (changescalup) then
c     comupute pt's of partons respect the beams
         ptmin=min(pup(1,3)**2+pup(2,3)**2, pup(1,4)**2+pup(2,4)**2)
         if(nup.eq.5) then
            ptmin=min(ptmin,pup(1,5)**2+pup(2,5)**2)
         endif
c     compute pt's of the final state partons with respect to each other
c     go in the CM frame   
         do k=1,nup
            do mu=1,3
               pcm(mu,k)=pup(mu,k)
            enddo
            pcm(0,k)=pup(4,k)
         enddo
         beta=-(pup(3,1)+pup(3,2))/(pup(4,1)+pup(4,2))
         vec(1)=0
         vec(2)=0
         vec(3)=1
         call mboost(nup,vec,beta,pcm,pcm)
c     write(*,*) 'momenta'
c     do k=1,nup
c     write(*,*) (pcm(mu,k),mu=0,3)
c     enddo
         do k=3,nup
            do j=k+1,nup
               ptkj2 = 2*dotp(pcm(0,k),pcm(0,j))*
     1              pcm(0,k)*pcm(0,j)/(pcm(0,k)+pcm(0,j))**2
               ptmin=min(ptmin,ptkj2)
            enddo
         enddo
         ptmin=sqrt(ptmin)
         if(scalup.gt.ptmin) then
c     write(*,*) 'DECREASED SCALUP FROM ',scalup,' TO ',ptmin
            scalup = ptmin
         endif
      endif


c      if(scalup.gt.ptmin*2) then
      if(scalup.eq.-1) then
         write(*,*) ' scalup: ',scalup,' ptmin:',ptmin
         call  lhefreadextra(97)
         
         write(*,*) ' rad_kinreg:',rad_kinreg
         write(*,'(5(1x,i2))') (idup(k),k=1,nup)
         do k=1,nup
            do mu=1,3
               pcm(mu,k)=pup(mu,k)
            enddo
            pcm(0,k)=pup(4,k)
         enddo
         beta=-(pup(3,1)+pup(3,2))/(pup(4,1)+pup(4,2))
         vec(1)=0
         vec(2)=0
         vec(3)=1
         call mboost(nup,vec,beta,pcm,pcm)
         write(*,'(4(1x,d10.4))') ((pcm(mu,k),mu=0,3),k=1,nup)
      endif


      if(scalupfact.gt.0) scalup=scalup*scalupfact
c      call lhefinitemasses      
      end

      subroutine upveto
c pythia routine to abort event
      end

      subroutine pyabeg
      call init_hist
      end

      subroutine pyaend
      implicit none
      include 'pwhg_rnd.h'
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      if(rnd_cwhichseed.eq.'none') then
         open(unit=99,file=pwgprefix(1:lprefix)//
     1     'POWHEG+PYTHIA-output.top',status='unknown')
      else
         open(unit=99,file=pwgprefix(1:lprefix)//'-'//
     1        rnd_cwhichseed //'-'//
     2     'POWHEG+PYTHIA-output.top',status='unknown')
      endif
      call pwhgsetout
      call pwhgtopout
      close(99)
      end


      subroutine pyanal
      implicit none
      include 'hepevt.h'
      include 'LesHouches.h'
      nevhep=nevhep+1
      if(abs(idwtup).eq.3) xwgtup=xwgtup*xsecup(1)
      call analysis(xwgtup)
      call pwhgaccumup 
      end


      function dotp(p1,p2)
      implicit none
      real * 8 dotp,p1(0:3),p2(0:3)
      dotp = (p1(0)*p2(0) - p1(3)*p2(3)) - p1(1)*p2(1) - p1(2)*p2(2)
      end
