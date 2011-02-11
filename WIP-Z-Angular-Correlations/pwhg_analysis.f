c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  'LesHouches.h'
      include '../pwhg_book.h'
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
      real * 8 pi,pi2
      parameter(pi = 3.141592653589793D0, pi2 = 9.869604401089358D0)

      call pwhginihist

      diag=1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'total','LOG',binsize(diag),0d0,3d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'mt(Z)','LOG',binsize(diag),50d0,100d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'m(Z)','LOG',binsize(diag),60d0,120d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'pt(l+)','LOG',binsize(diag),0d0
     $     ,100d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'eta(l+)','LOG',binsize(diag),-3d0,3d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'pt(Z)','LOG',binsize(diag),0d0,100d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'y(Z)','LOG',binsize(diag),-3d0,3d0)


      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'cos(theta) Pt_Z<5','LIN',binsize(diag),-1d0
     $     ,1d0)

      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'cos(theta) 5<Pt_Z<10','LIN',binsize(diag),
     $     -1d0,1d0)

      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'cos(theta) 10<Pt_Z<20','LIN',binsize(diag),
     $     -1d0,1d0)

      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'cos(theta) 20<Pt_Z<40','LIN',binsize(diag),
     $     -1d0,1d0)
      
      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'cos(theta) 40<Pt_Z','LIN',binsize(diag),-1d0
     $     ,1d0)

      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'phi Pt_Z<5','LIN',binsize(diag),-3.15d0,3
     $     .15d0)

      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'phi 5<Pt_Z<10','LIN',binsize(diag),-3.15d0,3
     $     .15d0)

      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'phi 10<Pt_Z<20','LIN',binsize(diag),-3.15d0
     $     ,3.15d0)

      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'phi 20<Pt_Z<40','LIN',binsize(diag),-3.15d0
     $     ,3.15d0)
      
      diag=diag+1
      binsize(diag) = 0.01d0
      call pwhgbookup(diag,'phi 40<Pt_Z','LIN',binsize(diag),-3.15d0,3
     $     .15d0)


      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'A0','LIN',binsize(diag),0d0,100d0)

      diag=diag+1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'A1','LIN',binsize(diag),0d0,100d0)
      
      diag=diag+1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'A2','LIN',binsize(diag),0d0,100d0)

      diag=diag+1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'A3','LIN',binsize(diag),0d0,100d0)

      diag=diag+1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'A4','LIN',binsize(diag),0d0,100d0)

      diag=diag+1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'A5','LIN',binsize(diag),0d0,100d0)

      diag=diag+1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'A6','LIN',binsize(diag),0d0,100d0)

      diag=diag+1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'A7','LIN',binsize(diag),0d0,100d0)

      end

      

      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include 'hepevt.h'
      include 'pwhg_math.h'
      include  'LesHouches.h'
      real *8 ptvb,yvb
      real *8 ptep,ptem,yep,yem
      real * 8 mvb,pvb(0:3),tmp
      logical ini
      data ini/.true./
      save ini
      integer maxnumlep
      parameter (maxnumlep=10)
      integer emvec(maxnumlep),epvec(maxnumlep),iep,iem,ep,em
      real * 8  Zmass,Zwidth,Zmass2low,Zmass2high,mV2ref,mV2
      logical foundlep
      integer nem,nep
      logical findmother
      parameter (findmother=.false.)
      logical findinvmass
      parameter (findinvmass=.true.)
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'POWHEG'/
      logical is_Z
c     binsize
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
      real *8 p_lminus(0:3),p_lplus(0:3)
      real *8 a(0:7),lcos,genphi,delphi,mt_v
      integer neplus,neminus,ihep,mu,i
      
      if (WHCPRG.ne.'POWHEG') then 
c     set values if analysis file is run by HERWIG and PYTHIA
         Zmass = 91.188d0
         Zwidth = 2.486d0
         Zmass2low = (Zmass-10*Zwidth)**2
         Zmass2high = (Zmass+10*Zwidth)**2
      endif
      if (ini) then
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '                ANALYSIS CUTS                     '
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*)   sqrt(Zmass2low),' < M_Z < ',sqrt(Zmass2high)
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         ini = .false.
      endif

      if (WHCPRG.eq.'POWHEG') then 
         neminus=0
         neplus=0
         do i=1,maxnumlep
            emvec(i) = 0
            epvec(i) = 0
         enddo
         do ihep=1,nhep
            if (isthep(ihep).eq.1) then
               if(idhep(ihep).eq.11) then
                  neminus=neminus+1
                  emvec(neminus)=ihep
               elseif(idhep(ihep).eq.-11) then
                  neplus=neplus+1
                  epvec(neplus)=ihep
               endif
            endif         
         enddo
         
         if (neminus.ne.1.or.neplus.ne.1) then
            write(*,*) "Too many leptons found. PROGRAM ABORT"
            call exit(1)
         else 
            iem=emvec(1)
            iep=epvec(1)
         endif
      endif


c     Analysis after MC shower
      if (WHCPRG.eq.'HERWIG') then
c     Loop again over final state particles to find products of Z decay, by
c     looking into the shower branchings.
         nem=0
         nep=0
         do ihep=1,nhep
c     works for POWHEG+HERWIG, POWHEG+PYHIA, HERWIG, PYTHIA and real in
c     MC@NLO
            if (isthep(ihep).eq.1.and.abs(idhep(ihep)).eq.11) then
               is_Z = idhep(jmohep(1,jmohep(1,ihep))).eq.23
               if (.not.is_Z) then
                  is_Z = idhep(jmohep(1,jmohep(1,jmohep(1,ihep)))).eq.23
               endif
               if (is_Z) then
c     find first decay product
                  if(idhep(ihep).eq.11) then
                     iem=ihep
                     nem=nem+1
c     find second decay product
                  elseif(idhep(ihep).eq.-11) then
                     iep=ihep
                     nep=nep+1
                  endif
               endif
            endif
         enddo
         if(nep.ne.1.or.nem.ne.1) then
            write(*,*) 'Problems with leptons from Z decay'
            write(*,*) 'PROGRAM ABORT'
            call exit(1)
         endif
      endif


      if (WHCPRG.eq.'PYTHIA') then
c     Loop again over final state particles to find products of Z decay, by
c     looking into the shower branchings.
         nem=0
         nep=0
         do ihep=1,nhep
c     works both for POWHEG+HERWIG and POWHEG+PYHIA
            if (isthep(ihep).eq.1.and.abs(idhep(ihep)).eq.11) then
               is_Z = idhep(jmohep(1,jmohep(1,ihep))).eq.23
               if (.not.is_Z) then
                  is_Z = idhep(jmohep(1,jmohep(1,jmohep(1,ihep)))).eq.23
               endif
               if (is_Z) then
c     find first decay product
                  if(idhep(ihep).eq.11) then
                     iem=ihep
                     nem=nem+1
c     find second decay product
                  elseif(idhep(ihep).eq.-11) then
                     iep=ihep
                     nep=nep+1
                  endif
               endif
            endif
         enddo
         if(nep.ne.1.or.nem.ne.1) then    
            write(*,*) 'nhep ',nhep
             do ihep=1,nhep
                write(*,*) ihep, isthep(ihep)
                write(*,*) (phep(mu,ihep),mu=1,4)
             enddo
            
            write(*,*) 'Problems with leptons from Z decay', nep, nem
            write(*,*) 'PROGRAM ABORT'
            call exit(1)
         endif
      endif

      p_lminus(0)=phep(4,iem)
      do mu=1,3
         p_lminus(mu)=phep(mu,iem)
      enddo

      p_lplus(0)=phep(4,iep)
      do mu=1,3
         p_lplus(mu)=phep(mu,iep)
      enddo

c     Z momentum
      do mu=0,3
         pvb(mu)=p_lminus(mu) + p_lplus(mu)
      enddo
      mV2 = pvb(0)**2-pvb(1)**2-pvb(2)**2-pvb(3)**2
c      write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>',sqrt(mV2)

      ptvb=sqrt(pvb(1)**2+pvb(2)**2)

      call getinvmass(pvb,mvb)
      call getrapidity(pvb,yvb)
      ptep=sqrt(phep(1,iep)**2+phep(2,iep)**2)
      call getrapidity(p_lplus,yep)
      ptem=sqrt(phep(1,iem)**2+phep(2,iem)**2)
      call getrapidity(p_lminus,yem)
c     azimuthal separation between leptons
      delphi = dabs(atan2(p_lplus(2),p_lplus(1)) - 
     $     atan2(p_lminus(2),p_lminus(1)))
      delphi=min(delphi,2d0*pi-delphi)
c     transverse mass of the charged lepton system
      mt_v=sqrt(2*ptep*ptem*(1d0-dcos(delphi)))

c     total sigma
      diag=1
      call pwhgfill(diag,1.5d0,dsig/binsize(diag))

c     transverse mass of the charged lepton system
      diag=diag+1
      call pwhgfill(diag,mt_v,dsig/binsize(diag))

c     invariant mass of the charged lepton system
      diag=diag+1
      call pwhgfill(diag,mvb,dsig/binsize(diag))

c     pt(l+)
      diag=diag+1
      call pwhgfill(diag,ptep,dsig/binsize(diag))

c     eta(l+)
      diag=diag+1
      call pwhgfill(diag,yep,dsig/binsize(diag))

c     pt(v)
      diag=diag+1
      call pwhgfill(diag,ptvb,dsig/binsize(diag))

c     y(v)
      diag=diag+1
      call pwhgfill(diag,yvb,dsig/binsize(diag))
         




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      call get_ang_coeffs(p_lminus,p_lplus,a,lcos,genphi)

      diag=diag+1
      if (ptvb.le.5d0) call pwhgfill(diag,lcos,dsig/binsize(diag))
      diag=diag+1
      if ((ptvb.gt.5d0).and.(ptvb.le.10d0))  call pwhgfill(diag,lcos
     $     ,dsig/binsize(diag))
      diag=diag+1
      if ((ptvb.gt.10d0).and.(ptvb.le.20d0)) call pwhgfill(diag,lcos
     $     ,dsig/binsize(diag))
      diag=diag+1
      if ((ptvb.gt.20d0).and.(ptvb.le.40d0)) call pwhgfill(diag,lcos
     $     ,dsig/binsize(diag))
      diag=diag+1
      if (ptvb.gt.40d0) call pwhgfill(diag,lcos,dsig/binsize(diag))
c
      diag=diag+1
      if (ptvb.le.5d0) call pwhgfill(diag,genphi,dsig/binsize(diag))
      diag=diag+1
      if ((ptvb.gt.5d0).and.(ptvb.le.10d0))  call pwhgfill(diag,genphi
     $     ,dsig/binsize(diag))
      diag=diag+1
      if ((ptvb.gt.10d0).and.(ptvb.le.20d0)) call pwhgfill(diag,genphi
     $     ,dsig/binsize(diag))
      diag=diag+1
      if ((ptvb.gt.20d0).and.(ptvb.le.40d0)) call pwhgfill(diag,genphi
     $     ,dsig/binsize(diag))
      diag=diag+1
      if (ptvb.gt.40d0) call pwhgfill(diag,genphi,dsig/binsize(diag))
      

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(0)/binsize(diag))

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(1)/binsize(diag))

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(2)/binsize(diag))

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(3)/binsize(diag))

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(4)/binsize(diag))

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(5)/binsize(diag))

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(6)/binsize(diag))

      diag=diag+1
      call pwhgfill(diag,ptvb,dsig*a(7)/binsize(diag))

     
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      end
      

      subroutine getrapidity(p,y)
      implicit none
      real * 8 p(0:3),y
      y=0.5d0*log((p(0)+p(3))/(p(0)-p(3)))
      end

      subroutine getinvmass(p,m)
      implicit none
      real * 8 p(0:3),m
      m=sqrt(abs(p(0)**2-p(1)**2-p(2)**2-p(3)**2))
      end

      subroutine get_pseudorap(p,eta)
      implicit none
      real*8 p(0:3),eta,pt,th
      real *8 tiny
      parameter (tiny=1.d-5)

      pt=sqrt(p(1)**2+p(2)**2)
      if(pt.lt.tiny.and.abs(p(3)).lt.tiny)then
         eta=sign(1.d0,p(3))*1.d8
      elseif(pt.lt.tiny) then   !: added this elseif
         eta=sign(1.d0,p(3))*1.d8
      else
         th=atan2(pt,p(3))
         eta=-log(tan(th/2.d0))
      endif
      end

      subroutine get_ang_coeffs(p1,p2,a,lcos,genphi)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_kn.h'
      real *8 p1(0:3),p2(0:3),res(0:3)
      real *8 a(0:7)
      real *8 theta,lcos,genphi
      real *8 mom0,mom1,mom2,mom3,mom4,mom5,mom6,mom7
c //==========================================================
c     // <m0> = <(1/2)*(1-3cos^2(tjeta))> = (3/20)*(A0 - (2/3))
c     // <m1> = <sin(2*theta)*cos(phi)> = (1/5)*A1
c     // <m2> = <sin^2(theta)*cos(2*phi)> = (1/10)*A2
c     // <m3> = <sin(theta)*cos(phi)> = (1/4)*A3
c     // <m4> = <cos(theta)> - (1/4)*A4
c     // <m5> = <sin^2(theta)*sin(2*phi)> = (1/5)*A5
c     // <m6> = <sin(2*theta)*sin(phi)> = (1/5)*A6
c     // <m7> = <sin(theta)*sin(phi)> = (1/4)*A7
c     //======================================================
      
      call calCSVariables(p1,p2,res,.false.)
      
      theta=dacos(res(0))
c      if(abs(res(0)-kn_cthdec).gt.1d-16) then
c         write(*,*) ' cos theta: ', res(0), kn_cthdec,res(0)/kn_cthdec
c      endif
      lcos=res(0)
      genphi=res(3)

      mom0 = 0.5d0*(1-3*lcos*lcos)
      mom1 = dsin(2d0*theta)*dcos(genphi)
      mom2 = dsin(theta)*dsin(theta)*dcos(2d0*genphi)
      mom3 = dsin(theta)*dcos(genphi)
      mom4 = lcos                 
      mom5 = dsin(theta)*dsin(theta)*dsin(2d0*genphi)
      mom6 = dsin(2d0*theta)*dsin(genphi)
      mom7 = dsin(theta)*dsin(genphi)

      a(0)   = (20d0/3d0)*mom0 + (2d0/3d0)
      a(1)   = 5*mom1
      a(2)   = 10*mom2
      a(3)   = 4*mom3
      a(4)   = 4*mom4
      a(5)   = 5*mom5
      a(6)   = 5*mom6
      a(7)   = 4*mom7

      end




      subroutine calCSVariables(p1,p2,res,swap)
      implicit none
      include 'LesHouches.h'
      include 'nlegborn.h'
      include 'pwhg_kn.h'
      real *8 p1(0:3),p2(0:3),res(0:3)
      logical swap
      real *8 Pbeam(0:3),Ptarget(0:3),Q(0:3)
      real *8 p1plus,p1minus,p2plus,p2minus,costheta
      integer nu
      real *8 Qmag,Qpt
      real *8 D(0:3),dt_qt,sin2theta,Dpt
      real *8 R(3),Rmag,Runit(3),Qt(3),Qtunit(3),Dt(3),tanphi,phi
      real *8 dotp,dotp3
      external dotp,dotp3

      do nu=0,3
         Pbeam(nu)=0
         Ptarget(nu)=0
         Q(nu)=p1(nu)+p2(nu)
      enddo
      Pbeam(0)=ebmup(1)
      Ptarget(0)=ebmup(2)
      Pbeam(3)=ebmup(1)
      Ptarget(3)=-ebmup(2)

      Qmag=sqrt(dotp(Q,Q))
      Qpt=sqrt(Q(1)**2+Q(2)**2)
c*********************************************************************
c*
c* 1) cos(theta) = 2 Q^-1 (Q^2+Qt^2)^-1/2 (p1^+ p2^- - p1^- p2^+)
c*
c*
c*********************************************************************
    
      p1plus=1d0/sqrt(2d0) * (p1(0) + p1(3))
      p1minus = 1d0/sqrt(2d0) * (p1(0) - p1(3))
      p2plus=1d0/sqrt(2d0) * (p2(0) + p2(3))
      p2minus = 1d0/sqrt(2d0) * (p2(0) - p2(3))

      costheta = 2d0 / Qmag / sqrt(Qmag**2 + 
     $     Qpt**2) * (p1plus * p2minus - p1minus * p2plus)

      if (swap) costheta = -costheta

c/********************************************************************
c*
c* 2) sin2(theta) = Q^-2 Dt^2 - Q^-2 (Q^2 + Qt^2)^-1 * (Dt dot Qt)^2
c*
c********************************************************************
      do nu=0,3
         D(nu)=p1(nu)-p2(nu)
      enddo
      Dpt=sqrt(D(1)**2+D(2)**2)
      dt_qt = D(1)*Q(1) + D(2)*Q(2)
      sin2theta=(DPt/QMag)**2 -1d0/QMag**2/(QMag**2 + QPt**2)*dt_qt**2

c      if (abs(sin2theta+(costheta*costheta)-1d0).gt.1d-6) then
c         write (*,*) "HAHA",abs(sin2theta+(costheta*costheta)-1d0),Qpt
c         stop
c      endif

c/********************************************************************
c*
c* 3) tanphi = (Q^2 + Qt^2)^1/2 / Q (Dt dot R unit) /(Dt dot Qt unit)
c*
c*********************************************************************
c// unit vector on R direction

      if(Qpt.gt.0d0) then
         call cross3(pbeam(1),Q(1),R)
         Rmag=sqrt(dotp3(R,R))

         Runit(1)=R(1)/Rmag
         Runit(2)=R(2)/Rmag
         Runit(3)=R(3)/Rmag

         Qt(1)=Q(1)
         Qt(2)=Q(2)
         Qt(3)=0

         Qtunit(1)=Qt(1)/Qpt
         Qtunit(2)=Qt(2)/Qpt
         Qtunit(3)=0
    
      
         Dt(1)=D(1)
         Dt(2)=D(2)
         Dt(3)=0

      
         tanphi=sqrt( Qmag**2 + Qpt**2) / Qmag * dotp3(Dt,Runit)/
     $        dotp3(Dt,Qtunit)

         if (swap) tanphi = -tanphi

         phi=atan2(sqrt(Qmag**2 + Qpt**2 )* dotp3(Dt,Runit),QMag
     $        *dotp3(Dt,Qtunit))


         if (swap) phi = atan2(-sqrt(QMag**2+ QPt**2)*dotp3(Dt,Runit)
     $        ,QMag*dotp3(Dt,Qtunit))

      else
         tanphi=0
         phi=0
      endif

      res(0)=costheta
      res(1)=sin2theta
      res(2)=tanphi
      res(3)=phi
      end

      function dotp3(p1,p2)
      implicit none
      real * 8 dotp3,p1(3),p2(3)
      dotp3 = p1(1)*p2(1) + p1(2)*p2(2) + p1(3)*p2(3)
      end

      subroutine cross3(p1,p2,p3)
      implicit none
      real * 8 p3(3),p1(3),p2(3)
      p3(1) = p1(2)*p2(3)-p1(3)*p2(2)
      p3(2) = p1(3)*p2(1)-p1(1)*p2(3)
      p3(3) = p1(1)*p2(2)-p1(2)*p2(1)
      end
