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
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
      character * 10 cut

      cut = 'cuts'

      call pwhginihist

c     total cross section sanity check
      diag=1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'total','LOG',binsize(diag),0d0,3d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'total'//cut,'LOG',binsize(diag),0d0,3d0)

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
      integer maxnumlep
      parameter (maxnumlep=10)
      integer lplvec(maxnumlep),lmivec(maxnumlep)
      logical foundlep
      integer i,ilm,ilp,i_lminus,i_lplus,jlminus,jlplus,nlmi,nlpl
      real *8 mV2ref,mV2
      real *8 Zmass,Zwidth,Zmass2low,Zmass2high
      
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

      do mu=0,3
         pcm(mu)=0d0
      enddo

c     total sigma (without cuts)
      diag=diag+1
      call pwhgfill(diag,1.5d0,dsig/binsize(diag))

      if(WHCPRG.eq.'NLO   ') then
         continue
      elseif ((WHCPRG.eq.'HERWIG').or.(WHCPRG.eq.'PYTHIA')) then
         continue
      endif

      cuts=.true.

      if(cuts) then
c     total sigma (after cuts)
         diag=diag+1
         call pwhgfill(diag,1.5d0,dsig/binsize(diag))
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
