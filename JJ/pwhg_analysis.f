c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  '../include/LesHouches.h'
      include '../pwhg_book.h'
      include '../include/pwhg_math.h'
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
      character * 10 cut

      cut = 'cuts'

      call pwhginihist

c     total cross section sanity check
      diag=1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'total Et','LOG',binsize(diag),0d0,500d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'pt inc. jet','LOG',binsize(diag),5d0,100d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'pt inc jet, |eta|<0.5jet','LOG',
     1     binsize(diag),5d0,100d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'pt j3','LOG',
     1     binsize(diag),1d0,100d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'eta 2 hardest','LOG',
     1     binsize(diag),0d0,5d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'eta jet3','LOG',binsize(diag),-5d0,5d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'delta eta 2 hardest','LOG',
     1     binsize(diag),-5d0,5d0)


      diag=diag+1
      binsize(diag) = pi/50
      call pwhgbookup(diag,'D Phi 1-2 ','LOG',binsize(diag),0d0,pi)

      end


      subroutine buildjets(mjets,kt,eta,phi)
c     arrays to reconstruct jets
      implicit none
      include '../include/hepevt.h'
      integer mjets
      real * 8 kt(mjets),eta(mjets),phi(mjets)
      integer maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real *8 ptrack(4,maxtrack)
      real *8 pjet(4,maxjet),pp
      integer jetvec(maxtrack)
      integer ihep,j,j1,ntracks,jpart,jjet,mu,njets
      real * 8 found
      real * 8 random
      integer seed
      data seed/1/
      save seed
c     get valid tracks
c     set up arrays for jet finding
      do jpart=1,maxtrack
         do mu=1,4
            ptrack(mu,jpart)=0d0
         enddo
         jetvec(jpart)=0
      enddo      
      do jjet=1,maxjet
         do mu=1,4
            pjet(mu,jjet)=0d0
         enddo
      enddo
      j1=0
      found=0
      ntracks=0
      njets=0
c     loop over final state particles to find jets 
      do ihep=1,nhep
         if (isthep(ihep).eq.1) then
            if(ntracks.eq.maxtrack) then
               write(*,*)
     #              'analyze: too many particles, increase maxtrack'
               stop
            endif
c     copy momenta to construct jets 
            ntracks=ntracks+1
            do mu=1,4
               ptrack(mu,ntracks)=phep(mu,ihep)
            enddo
         endif
      enddo
      if (ntracks.eq.0) then
         return
      endif
c     siscone algorithm
c*********************************************************************
c      R = 0.7  radius parameter
c      f = 0.5  overlapping fraction
c.....run the clustering        
      call fastjetsiscone(ptrack,ntracks,0.7d0,0.5d0,pjet,njets) 
c*********************************************************************
c     fastkt algorithm
c*********************************************************************
c      R = 0.7  Radius parameter
c.....run the clustering 
c      R = 0.5d0          
c      ptmin_fastkt = 0d0
c      call fastjetktwhich(ptrack,ntracks,ptmin_fastkt,R,
c     #     pjet,njets,jetvec)
c     now we have the jets
      mjets=min(mjets,njets)
      do j=1,mjets
         kt(j)=sqrt(pjet(1,j)**2+pjet(2,j)**2)
         pp = sqrt(kt(j)**2+pjet(3,j)**2)
         eta(j)=0.5d0*log((pp+pjet(3,j))/(pp-pjet(3,j)))
         phi(j)=atan2(pjet(2,j),pjet(1,j))
      enddo
c if we only have two tracks, the hardest jet is ill defined;
c take one of the two randomly (this will screw up the error estimates,
c spoiling correlated events ...)
      if(ntracks.eq.2) then
         if(random(seed).gt.0.5) then
            pp=eta(1)
            eta(1) = eta(2)
            eta(2) = pp
            pp = phi(1)
            phi(1) = phi(2)
            phi(2) = pp
         endif
      endif
      end
      
     
      subroutine analysis(dsig0)
      implicit none
      real * 8 dsig0,dsig
      include '../include/hepevt.h'
      include '../include/pwhg_math.h' 
      include  '../include/LesHouches.h'
      integer ihep,mu
      logical ini
      data ini/.true./
      save ini
c     binsize
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
c     we need to tell to this analysis file which program is running it
      real * 8 ktjets(4),etajets(4),phijets(4)
      integer njets
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer j,i
      real *8 et,dphi
c from pico to micro
      dsig=dsig0/1d6
      if (ini) then
         write(*,*) '*****************************'
         if(WHCPRG.eq.'NLO   ') then
            write(*,*) '            NLO analysis'
         elseif(WHCPRG.eq.'HERWIG') then
            write (*,*) '                HERWIG ANALYSIS            '
         elseif(WHCPRG.eq.'PYTHIA') then
            write (*,*) '                PYTHIA ANALYSIS            '
         endif
         write(*,*) '*****************************'
         
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '                ANALYSIS CUTS                     '
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '   no cuts   '
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         ini=.false.
      endif
      diag=0

      et=0
      do ihep=1,nhep
         if(isthep(ihep).eq.1) then
            et=et+sqrt(phep(1,ihep)**2+phep(2,ihep)**2)
         endif
      enddo
      if(et.lt.20) return
      diag=diag+1
      call pwhgfill(diag,et,dsig)

      njets=3

      call buildjets(njets,ktjets,etajets,phijets) 

      diag=diag+1
c inclusive jet distribution
      do j=1,njets
         call pwhgfill(diag,ktjets(j),dsig)
      enddo
      diag=diag+1
      do j=1,njets
         if(abs(etajets(j)).lt.0.5) then
            call pwhgfill(diag,ktjets(j),dsig)
         endif
      enddo
      diag=diag+1
      if(njets.ge.3) call pwhgfill(diag,ktjets(3),dsig)
      
      diag=diag+1
c eta of two hardest jets
      do j=1,2
         if(njets.ge.j) then
            call pwhgfill(diag,etajets(j),dsig)
         endif
      enddo
      diag=diag+1
      if(njets.ge.3.and.ktjets(3).gt.5)
     1     call pwhgfill(diag,etajets(3),dsig)

      diag=diag+1
      if(njets.ge.2) call pwhgfill(diag,abs(etajets(1)-etajets(2)),dsig)

      diag=diag+1
      if(njets.ge.2) then
         dphi=abs(phijets(1)-phijets(2))
         if(dphi.gt.pi) dphi=dphi-pi*int(dphi/pi)
         call pwhgfill(diag,abs(phijets(1)-phijets(2)),dsig)
      endif

      if(WHCPRG.eq.'NLO   ') then
         continue
      elseif ((WHCPRG.eq.'HERWIG').or.(WHCPRG.eq.'PYTHIA')) then
         continue
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




