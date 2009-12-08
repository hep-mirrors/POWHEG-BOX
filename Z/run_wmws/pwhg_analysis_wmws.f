c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include '../pwhg_book.h'
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
      character * 10 cut
      cut = ' WMWS cuts'


      call pwhginihist

c     total cross section sanity check
      diag=1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'total','LOG',binsize(diag),0d0,3d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'total'//cut,'LOG',binsize(diag),0d0,3d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'mt(Z)'//cut,'LOG',binsize(diag),50d0,150d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'m(Z)'//cut,'LOG',binsize(diag),50d0,150d0)

      diag=diag+1
      binsize(diag) = 0.25d0
c     too many bins: keep binsize as required, but change boundaries
      call pwhgbookup(diag,'pt(l+)'//cut,'LOG',binsize(diag),32.5d0
     $     ,57.5d0)

      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'eta(l+)'//cut,'LOG',binsize(diag),-3d0,3d0)

      diag=diag+1
      binsize(diag) = 0.02d0
      call pwhgbookup(diag,'y(Z)'//cut,'LOG',binsize(diag),-1d0,1d0)

      diag=diag+1
      binsize(diag) = 0.02d0
      call pwhgbookup(diag,'ctheta*(l+)'//cut,'LOG',binsize(diag),-1d0
     $     ,1d0)

      diag=diag+1
      binsize(diag) = 0.25d0
      call pwhgbookup(diag,'pt(Z) zoom'//cut,'LOG',binsize(diag),0d0
     $     ,25d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'pt(Z)'//cut,'LOG',binsize(diag),0d0,100d0)

      end

      
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include '../include/hepevt.h'
      include '../include/pwhg_math.h' 
      include  '../include/LesHouches.h'
      real *8 ptcut,etacut,invmcut
      real *8 p_lminus(0:3),p_lplus(0:3),pcm(0:3),p_ll(0:3),vec(3),
     $p_lplus_cm(0:3)
      real *8 pt_lplus,pt_lminus,eta_lplus,eta_lminus,beta,ctheta,
     $delphi,mt_v,mv,ptv,yv
      integer ihep,mu
      logical cuts
      logical ini
      data ini/.true./
      save ini
c     binsize
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
c     we need to tell to this analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer vdecaytemp

      
      ptcut=25d0
      etacut=1d0
      invmcut=50d0


      if (ini) then
         write(*,*) '*****************************'
         write(*,*) '       WMWS NLO analysis'
         write(*,*) '*****************************'
         vdecaytemp=lprup(1)-10000 ! Z decay product, with positive id
         if(vdecaytemp.eq.11.or.vdecaytemp.eq.13) then
            continue
         else
            write(*,*) '**************************************'
            write(*,*) ' WMWS analysis works only for e and mu'
            write(*,*) '                 STOP     '
            write(*,*) '**************************************'
            call exit(1)
         endif

         
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '                ANALYSIS CUTS                     '
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '   M_Z      > ',invmcut
         write(*,*) '   |eta(l)| < ',etacut
         write(*,*) '   pt_l     > ',ptcut
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         ini=.false.
      endif

      diag=0

      do mu=0,3
         pcm(mu)=0d0
      enddo


c     total sigma (without cuts)
      diag=diag+1
      call pwhgfill(diag,1.5d0,dsig/binsize(diag))


c     find Z decay products
      if(WHCPRG.eq.'NLO   ') then
         do ihep=1,nhep
            if(isthep(ihep).eq.1) then
               if(idhep(ihep).eq.vdecaytemp) then
                  p_lminus(0)=phep(4,ihep)
                  do mu=1,3
                     p_lminus(mu)=phep(mu,ihep)
                  enddo
               elseif(idhep(ihep).eq.-vdecaytemp) then
                  p_lplus(0)=phep(4,ihep)
                  do mu=1,3
                     p_lplus(mu)=phep(mu,ihep)
                  enddo
               endif
            endif
         enddo
      elseif((WHCPRG.eq.'HERWIG').or.(WHCPRG.eq.'PYTHIA')) then
         write(*,*) 'Not yet implemented analysis'
         call exit(1)
      else
         write(*,*) 'Not yet implemented analysis'
         call exit(1)
      endif

c     lminus transverse momentum
      pt_lminus=sqrt(p_lminus(1)**2 + p_lminus(2)**2)
      call get_pseudorap(p_lminus,eta_lminus)
c     lplus transverse momentum
      pt_lplus=sqrt(p_lplus(1)**2 + p_lplus(2)**2)
      call get_pseudorap(p_lplus,eta_lplus)
c     invariant mass of the charged lepton system
      do mu=0,3
         p_ll(mu)=p_lplus(mu)+p_lminus(mu)
      enddo
      call getinvmass(p_ll,mv)

      cuts=((pt_lplus.gt.ptcut).and.(dabs(eta_lplus).lt.etacut).and.
     $     (pt_lminus.gt.ptcut).and.(dabs(eta_lminus).lt.etacut).and.
     $     (mv.gt.invmcut))


      if(cuts) then
c     azimuthal separation between leptons
         delphi = dabs(atan2(p_lplus(2),p_lplus(1)) - 
     $        atan2(p_lminus(2),p_lminus(1)))
         delphi=min(delphi,2d0*pi-delphi)
c     transverse mass of the charged lepton system
         mt_v=sqrt(2*pt_lplus*pt_lminus*(1d0-dcos(delphi)))
c     rapidity of the charged lepton system
         call getrapidity(p_ll,yv)
c     ctheta_lplus in c.m. frame. Calculate this observable only
c     at the partonic NLO level
         if(WHCPRG.eq.'NLO   ') then
            do mu=1,3
               do ihep=3,nhep
                  pcm(mu)=pcm(mu) + phep(mu,ihep)
               enddo
            enddo
            do ihep=3,nhep
               pcm(0)=pcm(0) + phep(4,ihep)
            enddo
            do mu=1,3
               vec(mu)=pcm(mu)/sqrt(pcm(1)**2+pcm(2)**2+pcm(3)**2)
            enddo
            beta=-sqrt(pcm(1)**2+pcm(2)**2+pcm(3)**2)/pcm(0)
            call mboost(1,vec,beta,p_lplus,p_lplus_cm)
            ctheta=p_lplus_cm(3)/
     $         sqrt(p_lplus_cm(1)**2+p_lplus_cm(2)**2+p_lplus_cm(3)**2)
         endif
         ptv=sqrt((p_lplus(1)+p_lminus(1))**2
     $        + (p_lplus(2)+p_lminus(2))**2)

c     total sigma (after cuts)
         diag=diag+1
         call pwhgfill(diag,1.5d0,dsig/binsize(diag))

c     transverse mass of the charged lepton system
         diag=diag+1
         call pwhgfill(diag,mt_v,dsig/binsize(diag))

c     invariant mass of the charged lepton system
         diag=diag+1
         call pwhgfill(diag,mv,dsig/binsize(diag))

c     pt(l+)
         diag=diag+1
         call pwhgfill(diag,pt_lplus,dsig/binsize(diag))

c     eta(l+)
         diag=diag+1
         call pwhgfill(diag,eta_lplus,dsig/binsize(diag))

c     y(v)
         diag=diag+1
         call pwhgfill(diag,yv,dsig/binsize(diag))

c     ctheta(l+) in c.m. frame
         diag=diag+1
         if(WHCPRG.eq.'NLO   ') then
            call pwhgfill(diag,ctheta,dsig/binsize(diag))
         endif

c     pt(v), zoom
         diag=diag+1
         call pwhgfill(diag,ptv,dsig/binsize(diag))

c     pt(v)
         diag=diag+1
         call pwhgfill(diag,ptv,dsig/binsize(diag))

         
      endif
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
