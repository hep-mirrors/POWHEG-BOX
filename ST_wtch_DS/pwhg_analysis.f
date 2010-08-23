c     !ER: ttype DOVREBBE essere a posto
c     !ER: Entra solo tramite wcode. Ininfluente per NLO.
c     !ER: attenzione quando cerco il W...ce ne sono due,
c     !ER: e qui si vuole solo quello primary...

c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  '../include/LesHouches.h'
      include '../include/pwhg_math.h' 
      integer diag,icut
      character * 10 cut

      integer numplots
      real * 8 binsize(400)
      common/pwhghistcommon/binsize,numplots

      real *8 jet_ktRadius,jet_ktptmin
      common/cjetsparam/jet_ktRadius,jet_ktptmin

      real *8 cut1_etabj1,cut1_ptbj1,cut1_etabj2,cut1_ptbj2,cut1_etaj
     $     ,cut1_ptj,cut1_mjj_min,cut1_mjj_max,cut1_etal,cut1_ptl
     $     ,cut1_ptv
      common/ccut1/cut1_etabj1,cut1_ptbj1,cut1_etabj2,cut1_ptbj2
     $     ,cut1_etaj,cut1_ptj,cut1_mjj_min,cut1_mjj_max,cut1_etal
     $     ,cut1_ptl,cut1_ptv

      real *8 cut2_etal,cut2_ptl,cut2_mll_min,cut2_mll_max,cut2_azill
     $     ,cut2_ptl1_min,cut2_ptl1_max,cut2_ptv,cut2_etaj,cut2_ptj
      common/ccut2/cut2_etal,cut2_ptl,cut2_mll_min,cut2_mll_max
     $     ,cut2_azill,cut2_ptl1_min,cut2_ptl1_max,cut2_ptv,cut2_etaj
     $     ,cut2_ptj

      real *8 cut3_etab,cut3_ptb
      common/ccut3/cut3_etab,cut3_ptb


c     jet parameters
      jet_ktRadius = 0.7d0          
      jet_ktptmin  = 10d0

      write(*,*) '**************************************************'
      write(*,*) '**************************************************'
      write(*,*) '                JET PARAMETERS                    '
      write(*,*) '**************************************************'
      write(*,*) '**************************************************'
      write(*,*) '   inclusive kt (fastkt implementation): '
      write(*,*) '      jet radius ',  jet_ktRadius
      write(*,*) '      jet ptmin ',   jet_ktptmin
      write(*,*) '**************************************************'
      write(*,*) '**************************************************'

c     signal cuts
      cut1_etabj1  = 2.5
      cut1_ptbj1   = 50.
      cut1_etabj2  = 2.5
      cut1_ptbj2   = 25.
      cut1_etaj    = 2.5
      cut1_ptj     = 25.
      cut1_mjj_min = 55.
      cut1_mjj_max = 85.
      cut1_etal    = 2.5
      cut1_ptl     = 25.
      cut1_ptv     = 25.

c     background cuts
      cut2_etal       = 2.5
      cut2_ptl        = 25.
      cut2_mll_min    = 12.
      cut2_mll_max    = 40.
      cut2_azill      = pi/4.
      cut2_ptl1_min   = 30.
      cut2_ptl1_max   = 55.
      cut2_ptv        = 50.
      cut2_etaj       = 2.5
      cut2_ptj        = 25.

c     b-hadron veto cuts
      cut3_etab       = 2.5
      cut3_ptb        = 50.

      write(*,*) '**************************************************'
      write(*,*) '**************************************************'
      write(*,*) '                ANALYSIS CUTS                     '
      write(*,*) '**************************************************'
      write(*,*) '**************************************************'
      write(*,*) '   SIGNAL CUTS (cut_1): '
      write(*,*) '      eta bj ',          cut1_etabj1 
      write(*,*) '      pt bj ',           cut1_ptbj1  
      write(*,*) '      eta bj2 (veto) ',  cut1_etabj2 
      write(*,*) '      pt bj2 (veto) ',   cut1_ptbj2  
      write(*,*) '      eta j ',           cut1_etaj 
      write(*,*) '      pt j ',            cut1_ptj
      write(*,*) '      invm jj ',         cut1_mjj_min,cut1_mjj_max
      write(*,*) '      eta l ',           cut1_etal 
      write(*,*) '      pt l ',            cut1_ptl  
      write(*,*) '      pt vl ',           cut1_ptv  

      write(*,*) '   BACKGROUND CUTS (cut_2): '
      write(*,*) '      eta l ',           cut2_etal 
      write(*,*) '      pt l ',            cut2_ptl  
      write(*,*) '      invm ll ',         cut2_mll_min,cut2_mll_max
      write(*,*) '      azi ll ',          cut2_azill
      write(*,*) '      pt l1 ',           cut2_ptl1_min,cut2_ptl1_max
      write(*,*) '      pt vl ',           cut2_ptv  
      write(*,*) '      eta j (veto) ',    cut2_etaj 
      write(*,*) '      pt j (veto) ',     cut2_ptj  

      write(*,*) '   BVETO CUTS (cut_3): '
      write(*,*) '      eta b ',           cut3_etab
      write(*,*) '      pt b (veto)',      cut3_ptb

      call pwhginihist

      icut=-1
      diag=0

 111  continue
      icut=icut+1

      if (icut.eq.0) then
         cut = ' nocut'
      elseif (icut.eq.1) then
         numplots=diag
         cut = ' cut_1'
      elseif (icut.eq.2) then
         cut = ' cut_2'
      elseif (icut.eq.3) then
         goto 222
         cut = ' cut_3'
      elseif (icut.eq.4) then
         goto 222
      else
         write(*,*) 'Error in init_hist, icut ',icut
         call exit(1)
      endif

c-----top
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'t pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'t pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'t pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'t y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'t eta'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.75d0
      call pwhgbookup(diag,'t invm'//cut,'LOG',binsize(diag),160d0
     $     ,190d0)

c-----W
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'W pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'W pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'W pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'W y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'W eta'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.75d0
      call pwhgbookup(diag,'W invm'//cut,'LOG',binsize(diag),65d0,95d0)

c-----J1 (light)
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'J1 pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'J1 pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'J1 pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'J1 y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'J1 eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----J2 (light)
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'J2 pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'J2 pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'J2 pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'J2 y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'J2 eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----tW
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'tW pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'tW pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'tW pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'tW y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'tW eta'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'tW dphi'//cut,'LIN',binsize(diag),0d0,pi)

      diag=diag+1
      binsize(diag) = pi/3d0/40
      call pwhgbookup(diag,'tW dphi zoom'//cut,'LIN',binsize(diag),pi-pi
     $     /3.,pi)

c-----bW
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'bW pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'bW pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'bW pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'bW y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'bW eta'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 2d0
      call pwhgbookup(diag,'bW invm'//cut,'LOG',binsize(diag),70d0
     $     ,270d0)

      diag=diag+1
      binsize(diag) = 0.5d0
      call pwhgbookup(diag,'bW invm zoom'//cut,'LOG',binsize(diag),160d0
     $     ,190d0)

c-----hardest B
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'B pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'B pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'B pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'B y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'B eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----hardest Bbar
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'Bb pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'Bb pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'Bb pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'Bb y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'Bb eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----hardest lep
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'l1 pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'l1 pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'l1 pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l1 y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l1 eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----second hardest lep
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'l2 pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'l2 pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'l2 pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l2 y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l2 eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----lep1-lep2
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'l1l2 pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'l1l2 pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'l1l2 pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l1l2 y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l1l2 eta'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 2d0
      call pwhgbookup(diag,'l1l2 invm'//cut,'LOG',binsize(diag),0d0
     $     ,200d0)

      diag=diag+1
      binsize(diag) = pi/50
      call pwhgbookup(diag,'l1l2 dphi'//cut,'LIN',binsize(diag),0d0,pi)

c-----lep (from top)
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'l_t pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'l_t pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'l_t pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l_t y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l_t eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----lep (from primary W)
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'l_w pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'l_w pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'l_w pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l_w y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'l_w eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----hardest Bjet
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'bj pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'bj pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'bj pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'bj y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'bj eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----second hardest Bjet
      diag=diag+1
      binsize(diag) = 4d0
      call pwhgbookup(diag,'bj2 pt'//cut,'LOG',binsize(diag),0d0,200d0)

      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'bj2 pt zoom'//cut,'LOG',binsize(diag),0d0
     $     ,50d0)

      diag=diag+1
      binsize(diag) = 16d0
      call pwhgbookup(diag,'bj2 pt tail'//cut,'LOG',binsize(diag),0d0
     $     ,800d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'bj2 y'//cut,'LOG',binsize(diag),-7d0,7d0)

      diag=diag+1
      binsize(diag) = 0.35d0
      call pwhgbookup(diag,'bj2 eta'//cut,'LOG',binsize(diag),-7d0,7d0)

c-----total cross section
      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'total'//cut,'LIN',binsize(diag),0d0,3d0)

      goto 111

 222  end




      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include '../include/hepevt.h' 
      include '../include/pwhg_math.h' 
      include  '../include/LesHouches.h'
c     other common blocks
      integer numplots
      real * 8 binsize(400)
      common/pwhghistcommon/binsize,numplots

      real *8 jet_ktRadius,jet_ktptmin
      common/cjetsparam/jet_ktRadius,jet_ktptmin

      real *8 cut1_etabj1,cut1_ptbj1,cut1_etabj2,cut1_ptbj2,cut1_etaj
     $     ,cut1_ptj,cut1_mjj_min,cut1_mjj_max,cut1_etal,cut1_ptl
     $     ,cut1_ptv
      common/ccut1/cut1_etabj1,cut1_ptbj1,cut1_etabj2,cut1_ptbj2
     $     ,cut1_etaj,cut1_ptj,cut1_mjj_min,cut1_mjj_max,cut1_etal
     $     ,cut1_ptl,cut1_ptv

      real *8 cut2_etal,cut2_ptl,cut2_mll_min,cut2_mll_max,cut2_azill
     $     ,cut2_ptl1_min,cut2_ptl1_max,cut2_ptv,cut2_etaj,cut2_ptj
      common/ccut2/cut2_etal,cut2_ptl,cut2_mll_min,cut2_mll_max
     $     ,cut2_azill,cut2_ptl1_min,cut2_ptl1_max,cut2_ptv,cut2_etaj
     $     ,cut2_ptj

      real *8 cut3_etab,cut3_ptb
      common/ccut3/cut3_etab,cut3_ptb

      integer i,ihep,mu,ist_top,ist_w,ist,id,diag,j,jjet
      logical cuts1,cuts2,cuts3,ini,condition_b,condition_bbar,condition
     $     ,skipjet
      data ini/.true./
      save ini
      integer tcode,wcode,ttype
      save tcode,wcode,ttype
      real *8 powheginput
      external powheginput


      integer maxnum
      parameter (maxnum=10)
      real *8 p_top(0:3,maxnum),p_w(0:3,maxnum),
     $     p_l(0:3,maxnum),p_v(0:3,maxnum),
     $     p_b(0:3,maxnum),p_bbar(0:3,maxnum),p_bhad(0:3,maxnum),
     $     p_lw(0:3,maxnum),p_lt(0:3,maxnum)
      integer nt,nw,nl,nv,nb,nbbar,nbhad,nbjets,njets,nlw,nlt
      integer jbh,jb1,jb2,jbh1,jbh2,jbbar1,jbbar2,jl1,jl2,jv,ij1,ij2
     $     ,ibj1,ibj2

      real *8 pj1(0:3),pj2(0:3),pbj1(0:3),pbj2(0:3),p_bw(0:3),p_tw(0:3),
     $     pj1j2(0:3),pl1l2(0:3)

      real *8 pt_t,y_t,eta_t,minv_t,pt_w,y_w,eta_w,minv_w
      real *8 pt_tw,y_tw,eta_tw,pt_bw,y_bw,eta_bw,minv_bw
      real *8 pttemp,tmp,pt_b1,y_b1,eta_b1,pt_bbar1,y_bbar1,eta_bbar1
      real *8 pt_b2,y_b2,eta_b2,pt_bbar2,y_bbar2,eta_bbar2
      real *8 pt_bhad1,pt_bhad2,eta_bhad
      real *8 pt_j1,y_j1,eta_j1,pt_j2,y_j2,eta_j2,minv_j1j2
      real *8 pt_bj1,y_bj1,eta_bj1,pt_bj2,y_bj2,eta_bj2
      real *8 pt_l1,y_l1,eta_l1,pt_l2,y_l2,eta_l2,pt_v
      real *8 pt_l1l2,y_l1l2,eta_l1l2,minv_l1l2
      real *8 pt_lt,y_lt,eta_lt,pt_lw,y_lw,eta_lw
      real *8 dphi_l1l2,dphi_tw

      integer maxtrack,maxjet
      parameter (maxtrack=2048, maxjet=2048)
      real *8 pjet(4,maxtrack),ptrack(4,maxtrack)
      integer jpart,ntracks,jetvec(maxtrack),ihep_of_track(maxtrack),
     $     jetvec_of_bjet(maxtrack)


      integer icut

c     we need to tell to this analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/

      real *8 pjx,pjy,pjz,pje !ER:

      if (ini) then
         write(*,*) '*****************************'
         if(WHCPRG.eq.'NLO   ') then
            write(*,*) '            NLO analysis '
         elseif(WHCPRG.eq.'HERWIG') then
            write (*,*) '           HERWIG ANALYSIS'
         elseif(WHCPRG.eq.'PYTHIA') then
            write (*,*) '           PYTHIA ANALYSIS'
         endif
         write(*,*) '*****************************'
         ini=.false.
         tcode=6
c     decide t or tbar process
         ttype=powheginput('ttype')
         if(abs(ttype).ne.1) then
            write(*,*) 'Unrecognised ttype in input file'
            write(*,*) 'admitted values: 1 for t, -1 for tbar'
            call exit(1)
         endif
         wcode=-24  *ttype
      endif

      if(WHCPRG.eq.'NLO   ') then
c     find t, W and extra parton, if present
         nt=0
         nw=0
         nl=0
         nv=0
         nb=0
         nbbar=0
         nbhad=0
         nbjets=0
         nlw=0
         nlt=0

         ij1=0
         ij2=0
         ibj1=0
         ibj2=0

         do ihep=1,nhep
            ist=isthep(ihep)
            id=idhep(ihep)
            if(ist.eq.1) then
               if(abs(id).eq.abs(tcode)) then
c     !: ttype-blind
                  nt=nt+1
                  p_top(0,nt)=phep(4,ihep)
                  do mu=1,3
                     p_top(mu,nt)=phep(mu,ihep)
                  enddo
               elseif(id.eq.wcode) then
c     !: look only for W associated to wcode
                  nw=nw+1
                  p_w(0,nw)=phep(4,ihep)
                  do mu=1,3
                     p_w(mu,nw)=phep(mu,ihep)
                  enddo
               endif
            endif
         enddo
      elseif (WHCPRG.eq.'HERWIG'.or.WHCPRG.eq.'PYTHIA') then
         if(WHCPRG.eq.'HERWIG') then
            ist_top=155
            ist_w  =155 !: for decayed events
         elseif(WHCPRG.eq.'PYTHIA') then
            ist_top=3
            ist_w  =3
         endif

         nt=0
         nw=0
         nl=0
         nv=0
         nb=0
         nbbar=0
         nbhad=0
         nbjets=0
         nlw=0
         nlt=0

         ij1=0
         ij2=0
         ibj1=0
         ibj2=0

         do ihep=1,nhep
            ist=isthep(ihep)
            id=idhep(ihep)
            condition_b=.false.
            condition_bbar=.false.
            condition_b=(ist.eq.1).and.
     $           (((id.gt.-600).and.(id.lt.-500)).or.
     $           ((id.gt.5000).and.(id.lt.6000)))
            condition_bbar=(ist.eq.1).and.
     $           (((id.gt.500).and.(id.lt.600)).or.
     $           ((id.gt.-6000).and.(id.lt.-5000)))
            if(condition_b.and.condition_bbar) then
               write(*,*) 'both b and bbar true at the same time'
               call exit(1)
            endif

c     top
            if(ist.eq.ist_top.and.abs(id).eq.abs(tcode)) then
c     !: ttype-blind
               nt=nt+1
               p_top(0,nt)=phep(4,ihep)
               do mu=1,3
                  p_top(mu,nt)=phep(mu,ihep)
               enddo
c     W
            elseif(ist.eq.ist_w.and.id.eq.wcode) then
c     !: look only for W associated to wcode
               nw=nw+1
               p_w(0,nw)=phep(4,ihep)
               do mu=1,3
                  p_w(mu,nw)=phep(mu,ihep)
               enddo
c     electron
            elseif(ist.eq.1.and.abs(id).eq.11) then
               nl=nl+1
               p_l(0,nl)=phep(4,ihep)
               do mu=1,3
                  p_l(mu,nl)=phep(mu,ihep)
               enddo
c              l_w
               if(id.eq.-sign(11,wcode)) then
                  nlw=nlw+1
                  p_lw(0,nlw)=phep(4,ihep)
                  do mu=1,3
                     p_lw(mu,nlw)=phep(mu,ihep)
                  enddo
c              l_top
               elseif(id.eq.sign(11,wcode)) then
                  nlt=nlt+1
                  p_lt(0,nlt)=phep(4,ihep)
                  do mu=1,3
                     p_lt(mu,nlt)=phep(mu,ihep)
                  enddo
               endif
c     electron neutrino
            elseif(ist.eq.1.and.abs(id).eq.12) then
c     !: no ttype, or decaytype, used
               nv=nv+1
               p_v(0,nv)=phep(4,ihep)
               do mu=1,3
                  p_v(mu,nv)=phep(mu,ihep)
               enddo
c     b hadrons
            elseif(condition_b) then
               nb=nb+1
               p_b(0,nb)=phep(4,ihep)
               do mu=1,3
                  p_b(mu,nb)=phep(mu,ihep)
               enddo
c     bbar hadrons
            elseif(condition_bbar) then
               nbbar=nbbar+1
               p_bbar(0,nbbar)=phep(4,ihep)
               do mu=1,3
                  p_bbar(mu,nbbar)=phep(mu,ihep)
               enddo
            endif
c     all b hadrons (b OR bbar)
            if(condition_b.or.condition_bbar) then
               nbhad=nbhad+1
               p_bhad(0,nbhad)=phep(4,ihep)
               do mu=1,3
                  p_bhad(mu,nbhad)=phep(mu,ihep)
               enddo
            endif
         enddo
         
c     set up arrays for jet finding
         ntracks=0
         njets=0
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

c     loop over final state particles, to find particles that
c     can be clustered
         do ihep=1,nhep
c     exclude leptons, gauge and Higgs bosons, but include gluons
            if ((isthep(ihep).eq.1).and.(
     $           (abs(idhep(ihep)).le.10).or.
     $           (abs(idhep(ihep)).ge.40).or.
     $           (abs(idhep(ihep)).eq.21))) then
c     copy momenta to construct jets 
               ntracks=ntracks+1
               ihep_of_track(ntracks)=ihep
               do mu=1,4
                  ptrack(mu,ntracks)=phep(mu,ihep)
               enddo
               if(ntracks.eq.maxtrack) then
                  write(*,*)
     $                 'hwanal: too many particles, increase maxtrack'
                  call exit(1)
               endif
            endif
         enddo
         if (ntracks.eq.0) then
            write(*,*) 'No tracks found, drop analysis of this event'
            goto 999
         endif

**********************************************************************
*     kt algorithm, fastjet implementation
         njets=0
         call fastjetktwhich(ptrack,ntracks,jet_ktptmin,jet_ktRadius,
     $        pjet,njets,jetvec) 
**********************************************************************
cccccccccccccccccccccccc
c$$$c     cancellare...
c$$$         do i=1,njets
c$$$            pjx=0.
c$$$            pjy=0.
c$$$            pjz=0.
c$$$            pje=0.
c$$$            do icut=1,ntracks
c$$$               if(jetvec(icut).eq.i) then
c$$$                  pjx=pjx+ptrack(1,icut)
c$$$                  pjy=pjy+ptrack(2,icut)
c$$$                  pjz=pjz+ptrack(3,icut)
c$$$                  pje=pje+ptrack(4,icut)
c$$$               elseif(jetvec(icut).eq.0) then
c$$$                  if((ptrack(1,icut)**2+ptrack(2,icut)
c$$$     $                 **2).gt.jet_ktptmin**2) then
c$$$                     print*, 'jetvec=0, kt grande'
c$$$                     print*,ptrack(1,icut),ptrack(2,icut),ptrack(3,icut)
c$$$     $                    ,ptrack(4,icut)
c$$$                  endif
c$$$               endif
c$$$            enddo
c$$$            print*, pjx/pjet(1,i)
c$$$            print*, pjy/pjet(2,i)
c$$$            print*, pjz/pjet(3,i)
c$$$            print*, pje/pjet(4,i)
c$$$         enddo
cccccccccccccccccccccccc
         skipjet=.false.
c     tag b-flavoured jets
         nbjets=0
         do i=1,ntracks
            ihep=ihep_of_track(i)
            condition_b=(
     $  ((abs(idhep(ihep)).gt.500).and.(abs(idhep(ihep)).lt.600)).or.
     $  ((abs(idhep(ihep)).gt.5000).and.(abs(idhep(ihep)).lt.6000)))
            if(condition_b) then
               nbjets=nbjets+1
               jetvec_of_bjet(nbjets)=jetvec(i)
            endif
         enddo

c     Find the 2 hardest light-jets
         if(njets.ge.1) then
c     Sort jets according to decreasing pt
c     Find the hardest jet
            pttemp=-1d0
            ij1=0
            tmp=0d0
            do jjet=1,njets
               condition=.true.
               do j=1,nbjets
c     loop on all b-jets: condition becomes false if current
c     jet (jjet) corresponds to one of the b-jets.
                  condition=condition.and.(jjet.ne.jetvec_of_bjet(j))
               enddo
               if(condition) then
                  tmp=sqrt(pjet(1,jjet)**2+pjet(2,jjet)**2)
                  if(tmp.gt.pttemp) then
                     ij1=jjet
                     pttemp=tmp
                  endif
               endif
            enddo
c     Now ij1 is the hardest light-jet
            pj1(0)=pjet(4,ij1)
            do mu=1,3
               pj1(mu)=pjet(mu,ij1)
            enddo
            
            if(njets.ge.2) then
c     Find the next-to-hardest light-jet
               pttemp=-1d0
               ij2=0
               tmp=0d0
               do jjet=1,njets
                  condition=.true.
                  do j=1,nbjets
c     loop on all b-jets: condition becomes false if current
c     jet (jjet) corresponds to one of the b-jets.
                     condition=condition.and.(jjet.ne.jetvec_of_bjet(j))
                  enddo
                  if(condition) then
                     tmp=sqrt(pjet(1,jjet)**2+pjet(2,jjet)**2)
                     if((tmp.gt.pttemp).and.(jjet.ne.ij1))then
                        ij2=jjet
                        pttemp=tmp
                     endif
                  endif
               enddo
c     Now ij2 is the next-to-hardest light-jet
               pj2(0)=pjet(4,ij2)
               do mu=1,3
                  pj2(mu)=pjet(mu,ij2)
               enddo
               if(ij1.eq.ij2) then
                  skipjet=.true.
                  if(ij1.ne.0) write(*,*) 'Suspicious event: ij1=ij2'
               endif
            endif
         else
            skipjet=.true.
         endif
         
         
c     Find the 2 hardest b-jets
         if(nbjets.ge.1) then
c     Sort jets according to decreasing pt
c     Find the hardest jet
            pttemp=-1d0
            ibj1=0
            tmp=0d0
            do i=1,nbjets
               jjet=jetvec_of_bjet(i)
               if(jjet.ne.0) then
                  tmp=sqrt(pjet(1,jjet)**2+pjet(2,jjet)**2)
                  if(tmp.gt.pttemp) then
                     ibj1=jjet
                     pttemp=tmp
                  endif
               endif
            enddo
            if(ibj1.ne.0) then
c     Now ibj1 is the hardest b-jet
               pbj1(0)=pjet(4,ibj1)
               do mu=1,3
                  pbj1(mu)=pjet(mu,ibj1)
               enddo
            endif
            if(nbjets.ge.2) then
c     Find the next-to-hardest bjet
               pttemp=-1d0
               ibj2=0
               tmp=0d0
               do i=1,nbjets
                  jjet=jetvec_of_bjet(i)
                  if(jjet.ne.0) then
                     tmp=sqrt(pjet(1,jjet)**2+pjet(2,jjet)**2)
                     if((tmp.gt.pttemp).and.(jjet.ne.ibj1))then
                        ibj2=jjet
                        pttemp=tmp
                     endif
                  endif
               enddo
               if(ibj2.ne.0) then
c     Now ibj2 is the next-to-hardest bjet
                  pbj2(0)=pjet(4,ibj2)
                  do mu=1,3
                     pbj2(mu)=pjet(mu,ibj2)
                  enddo
               endif
               if(ibj1.eq.ibj2) then
                  skipjet=.true.
                  if(ibj1.ne.0) write(*,*) 'Suspicious event: ibj1=ibj2'
               endif
            endif
         else
            skipjet=.true.
         endif
         
         
      else
         write(*,*) 'Invalid WHCPRG'
         call exit(1)
      endif




ccccccccccccccccccccccccccccccc
c     Observables
ccccccccccccccccccccccccccccccc 

c     t
      pt_t      = sqrt(p_top(1,1)**2+p_top(2,1)**2)
      call getrapidity(p_top(0,1),y_t)
      call get_pseudorap(p_top(0,1),eta_t)
      call getinvmass(p_top(0,1),minv_t)

c     w
      pt_w      = sqrt(p_w(1,1)**2+p_w(2,1)**2)
      call getrapidity(p_w(0,1),y_w)
      call get_pseudorap(p_w(0,1),eta_w)
      call getinvmass(p_w(0,1),minv_w)

c     tW
      do mu=0,3
         p_tw(mu)=p_top(mu,1)+p_w(mu,1)
      enddo
      pt_tw      = sqrt(p_tw(1)**2+p_tw(2)**2)
      call getrapidity(p_tw,y_tw)
      call get_pseudorap(p_tw,eta_tw)
      dphi_tw  = dabs(atan2(p_top(2,1),p_top(1,1)) - 
     $     atan2(p_w(2,1),p_w(1,1)))
      dphi_tw=min(dphi_tw,2d0*pi-dphi_tw)

c     hardest b
      if(nb.ge.1) then
         pttemp=-1d0
         jb1=0
         do i=1,nb
            pt_b1 = sqrt(p_b(1,i)**2+p_b(2,i)**2)
            if(pt_b1.gt.pttemp) then
               pttemp=pt_b1
               jb1=i
            endif
         enddo
         call getrapidity(p_b(0,jb1),y_b1)
         call get_pseudorap(p_b(0,jb1),eta_b1)
      endif

c     hardest bbar
      if(nbbar.ge.1) then
         pttemp=-1d0
         jbbar1=0
         do i=1,nbbar
            pt_bbar1 = sqrt(p_bbar(1,i)**2+p_bbar(2,i)**2)
            if(pt_bbar1.gt.pttemp) then
               pttemp=pt_bbar1
               jbbar1=i
            endif
         enddo
         call getrapidity(p_bbar(0,jbbar1),y_bbar1)
         call get_pseudorap(p_bbar(0,jbbar1),eta_bbar1)
      endif

c     bW
      if(WHCPRG.eq.'NLO   ') then
c     only if real contribution (and not always the 5th is a bbar-quark)
         if(nhep.eq.5) then
            p_bw(0)=p_w(0,1)+phep(4,5)
            do mu=1,3
               p_bw(mu)=p_w(mu,1)+phep(mu,5)
            enddo
         else
            p_bw(0)=-1
         endif
      elseif(WHCPRG.eq.'HERWIG'.or.WHCPRG.eq.'PYTHIA') then
c     !: if top, look for bbar, if antitop, look for b
         if(tcode.eq.6.and.jbbar1.ne.0) then
            do mu=0,3
               p_bw(mu)=p_w(mu,1)+p_bbar(mu,jbbar1)
            enddo
         elseif(tcode.eq.-6.and.jb1.ne.0) then
            do mu=0,3
               p_bw(mu)=p_w(mu,1)+p_b(mu,jb1)
            enddo
         else
            p_bw(0)=-1
         endif
      endif

      if(p_bw(0).gt.0.) then
         pt_bw      = sqrt(p_bw(1)**2+p_bw(2)**2)
         call getrapidity(p_bw,y_bw)
         call get_pseudorap(p_bw,eta_bw)
         call getinvmass(p_bw,minv_bw)
      endif

      if(WHCPRG.eq.'HERWIG'.or.WHCPRG.eq.'PYTHIA') then
         if(.not.skipjet) then
c     j1
            if(ij1.gt.0) then
               pt_j1     = sqrt(pj1(1)**2+pj1(2)**2)
               call getrapidity(pj1,y_j1)
               call get_pseudorap(pj1,eta_j1)
            endif
c     j2
            if(ij2.gt.0) then
               pt_j2     = sqrt(pj2(1)**2+pj2(2)**2)
               call getrapidity(pj2,y_j2)
               call get_pseudorap(pj2,eta_j2)
               do mu=0,3
                  pj1j2(mu)=pj1(mu)+pj2(mu)
               enddo
               call getinvmass(pj1j2,minv_j1j2)
            endif
c     bj1
            if(ibj1.gt.0) then
               pt_bj1     = sqrt(pbj1(1)**2+pbj1(2)**2)
               call getrapidity(pbj1,y_bj1)
               call get_pseudorap(pbj1,eta_bj1)
            endif
c     bj2
            if(ibj2.gt.0) then
               pt_bj2     = sqrt(pbj2(1)**2+pbj2(2)**2)
               call getrapidity(pbj2,y_bj2)
               call get_pseudorap(pbj2,eta_bj2)
            endif
         endif

c     hardest lepton
         if(nl.ge.1) then
            pttemp=-1d0
            jl1=0
            do i=1,nl
               pt_l1 = sqrt(p_l(1,i)**2+p_l(2,i)**2)
               if(pt_l1.gt.pttemp) then
                  pttemp=pt_l1
                  jl1=i
               endif
            enddo
            call getrapidity(p_l(0,jl1),y_l1)
            call get_pseudorap(p_l(0,jl1),eta_l1)
         endif
         
c     second hardest lepton
         if(nl.ge.2) then
            pttemp=-1d0
            jl2=0
            do i=1,nl
               pt_l2 = sqrt(p_l(1,i)**2+p_l(2,i)**2)
               if((pt_l2.gt.pttemp).and.(i.ne.jl1))then
                  pttemp=pt_l2
                  jl2=i
               endif
            enddo
            call getrapidity(p_l(0,jl2),y_l2)
            call get_pseudorap(p_l(0,jl2),eta_l2)
            if(jl1.eq.jl2) then
               write(*,*) 'Suspicious event: jl1=jl2'
            endif
         endif

         if(nl.ge.2) then
            do mu=0,3
               pl1l2(mu)=p_l(mu,jl1)+p_l(mu,jl2)
            enddo
            pt_l1l2 = sqrt(pl1l2(1)**2+pl1l2(2)**2)
            call getrapidity(pl1l2,y_l1l2)
            call get_pseudorap(pl1l2,eta_l1l2)
            call getinvmass(pl1l2,minv_l1l2)
            dphi_l1l2  = dabs(atan2(p_l(2,jl1),p_l(1,jl1)) - 
     $           atan2(p_l(2,jl2),p_l(1,jl2)))
            dphi_l1l2=min(dphi_l1l2,2d0*pi-dphi_l1l2)
         endif

c     hardest neutrino
         if(nv.ge.1) then
            pttemp=-1d0
            jv=0
            do i=1,nv
               pt_v = sqrt(p_v(1,i)**2+p_v(2,i)**2)
               if(pt_v.gt.pttemp) then
                  pttemp=pt_v
                  jv=i
               endif
            enddo
         endif

c     hardest lepton from top
         if(nlt.ge.1) then
            pttemp=-1d0
            jl1=0
            do i=1,nlt
               pt_lt = sqrt(p_lt(1,i)**2+p_lt(2,i)**2)
               if(pt_lt.gt.pttemp) then
                  pttemp=pt_lt
                  jl1=i
               endif
            enddo
            call getrapidity(p_lt(0,jl1),y_lt)
            call get_pseudorap(p_lt(0,jl1),eta_lt)
         endif

c     hardest lepton from primary W
         if(nlw.ge.1) then
            pttemp=-1d0
            jl1=0
            do i=1,nlw
               pt_lw = sqrt(p_lw(1,i)**2+p_lw(2,i)**2)
               if(pt_lw.gt.pttemp) then
                  pttemp=pt_lw
                  jl1=i
               endif
            enddo
            call getrapidity(p_lw(0,jl1),y_lw)
            call get_pseudorap(p_lw(0,jl1),eta_lw)
         endif

      endif

ccccccccccccccccccccccccc
c     Histograms filling
ccccccccccccccccccccccccc

      icut=-1
      diag=0

      if(WHCPRG.eq.'NLO   ') then
         cuts1=.false.
         cuts2=.false.
         cuts3=.false.
      else
c     signal cuts
         cuts1=
     $        (ibj1.gt.0).and.                     !1 bjet
     $        (pt_bj1.ge.cut1_ptbj1).and.          
     $        (dabs(eta_bj1).le.cut1_etabj1).and.
     $        (ij1.gt.0).and.                      !1 light jets
     $        (pt_j1.ge.cut1_ptj).and.
     $        (dabs(eta_j1).le.cut1_etaj).and.
     $        (ij2.gt.0).and.                      !2 light jets
     $        (pt_j2.ge.cut1_ptj).and.
     $        (dabs(eta_j2).le.cut1_etaj).and.
     $        (minv_j1j2.ge.cut1_mjj_min).and.
     $        (minv_j1j2.le.cut1_mjj_max).and.
     $        (nl.ge.1).and.                       !1 lepton
     $        (pt_l1.ge.cut1_ptl).and.
     $        (dabs(eta_l1).le.cut1_etal).and.
     $        (nv.ge.1).and.                       !1 neutrino
     $        (pt_v.ge.cut1_ptv)
         if(ibj2.gt.0) then                        !veto on extra bjets
            cuts1=cuts1.and.
     $           .not.((pt_bj2.ge.cut1_ptbj2).and.
     $           (dabs(eta_bj2).le.cut1_etabj2))
         endif

c     background cuts
         cuts2=
     $        (nl.ge.2).and.                       !2 leptons
     $        (pt_l1.ge.cut2_ptl).and.
     $        (dabs(eta_l1).le.cut2_etal).and.
     $        (pt_l2.ge.cut2_ptl).and.
     $        (dabs(eta_l2).le.cut2_etal).and.
     $        (minv_l1l2.ge.cut2_mll_min).and.
     $        (minv_l1l2.le.cut2_mll_max).and.
     $        (dphi_l1l2.le.cut2_azill).and.
     $        (pt_l1.ge.cut2_ptl1_min).and.
     $        (pt_l1.le.cut2_ptl1_max).and.
     $        (nv.ge.1).and.                       !1 neutrino
     $        (pt_v.ge.cut2_ptv)
         if(ij1.gt.0) then                         !veto on light jets  
            cuts2=cuts2.and.
     $           .not.((pt_j1.ge.cut2_ptj).and.
     $           (dabs(eta_j1).le.cut2_etaj))
         endif
         if(ibj1.gt.0) then                        !veto on bjets  
            cuts2=cuts2.and.
     $           .not.((pt_bj1.ge.cut2_ptj).and.
     $           (dabs(eta_bj1).le.cut2_etaj))
         endif

c     bveto cuts
c     hardest bhad within veto cut
         if(nbhad.ge.1) then
            pttemp=-1d0
            jbh1=0
            do i=1,nbhad
               call get_pseudorap(p_bhad(0,i),eta_bhad)
               if(dabs(eta_bhad).le.cut3_etab) then
                  pt_bhad1 = sqrt(p_bhad(1,i)**2+p_bhad(2,i)**2)
                  if(pt_bhad1.gt.pttemp) then
                     pttemp=pt_bhad1
                     jbh1=i
                  endif
               endif
            enddo
         endif
         
c     second hardest bhad within veto cut
         if(nbhad.ge.2) then
            pttemp=-1d0
            jbh2=0
            do i=1,nbhad
               call get_pseudorap(p_bhad(0,i),eta_bhad)
               if(dabs(eta_bhad).le.cut3_etab) then
                  pt_bhad2 = sqrt(p_bhad(1,i)**2+p_bhad(2,i)**2)
                  if((pt_bhad2.gt.pttemp).and.(i.ne.jbh1))then
                     pttemp=pt_bhad2
                     jbh2=i
                  endif
               endif
            enddo
            if((jbh1.ne.0).and.(jbh1.eq.jbh2)) then
               write(*,*) 'Suspicious event: jbh1=jbh2'
            endif
         endif
c     cuts3 condition
         cuts3=                    !veto on 2nd hardest central hadron, if present
     $        (nbhad.le.1).or.                         !none or just one B
     $        ((nbhad.ge.2).and.(jbh2.eq.0)).or.       !>=2B but 2nd B not central
     $        ((jbh2.ge.1).and.(pt_bhad2.le.cut3_ptb)) !veto on 2nd central B

      endif


 666  continue
      icut=icut+1

      if (icut.eq.0) then
         condition = .true.
      elseif (icut.eq.1) then
         condition = cuts1
      elseif (icut.eq.2) then
         condition = cuts2
      elseif (icut.eq.3) then
         goto 999
         condition = cuts3
      elseif (icut.eq.4) then
         goto 999
      else
         write(*,*) 'Error in init_hist, icut ',icut
         call exit(1)
      endif

      if(condition) then

c-----top
c     pt_top
      diag=diag+1
      if(nt.gt.0) call pwhgfill(diag,pt_t,dsig/binsize(diag))

c     pt_top
      diag=diag+1
      if(nt.gt.0) call pwhgfill(diag,pt_t,dsig/binsize(diag))

c     pt_top
      diag=diag+1
      if(nt.gt.0) call pwhgfill(diag,pt_t,dsig/binsize(diag))

c     y_top
      diag=diag+1
      if(nt.gt.0) call pwhgfill(diag,y_t,dsig/binsize(diag))

c     eta_top
      diag=diag+1
      if(nt.gt.0) call pwhgfill(diag,eta_t,dsig/binsize(diag))

c     minv_top
      diag=diag+1
      if(nt.gt.0) call pwhgfill(diag,minv_t,dsig/binsize(diag))

c-----w
c     pt_w
      diag=diag+1
      if(nw.gt.0) call pwhgfill(diag,pt_w,dsig/binsize(diag))

c     pt_w
      diag=diag+1
      if(nw.gt.0) call pwhgfill(diag,pt_w,dsig/binsize(diag))

c     pt_w
      diag=diag+1
      if(nw.gt.0) call pwhgfill(diag,pt_w,dsig/binsize(diag))

c     y_w
      diag=diag+1
      if(nw.gt.0) call pwhgfill(diag,y_w,dsig/binsize(diag))

c     eta_w
      diag=diag+1
      if(nw.gt.0) call pwhgfill(diag,eta_w,dsig/binsize(diag))

c     minv_w
      diag=diag+1
      if(nw.gt.0) call pwhgfill(diag,minv_w,dsig/binsize(diag))

c-----J1 (light)
c     pt_j1
      diag=diag+1
      if(ij1.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_j1,dsig/binsize(diag))

c     pt_j1
      diag=diag+1
      if(ij1.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_j1,dsig/binsize(diag))

c     pt_j1
      diag=diag+1
      if(ij1.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_j1,dsig/binsize(diag))

c     y_j1
      diag=diag+1
      if(ij1.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,y_j1,dsig/binsize(diag))

c     eta_j1
      diag=diag+1
      if(ij1.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,eta_j1,dsig/binsize(diag))

c-----J2 (light)
c     pt_j2
      diag=diag+1
      if(ij2.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_j2,dsig/binsize(diag))

c     pt_j2
      diag=diag+1
      if(ij2.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_j2,dsig/binsize(diag))

c     pt_j2
      diag=diag+1
      if(ij2.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_j2,dsig/binsize(diag))

c     y_j2
      diag=diag+1
      if(ij2.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,y_j2,dsig/binsize(diag))

c     eta_j2
      diag=diag+1
      if(ij2.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,eta_j2,dsig/binsize(diag))

c-----tW
c     pt_tw
      diag=diag+1
      call pwhgfill(diag,pt_tw,dsig/binsize(diag))

c     pt_tw
      diag=diag+1
      call pwhgfill(diag,pt_tw,dsig/binsize(diag))

c     pt_tw
      diag=diag+1
      call pwhgfill(diag,pt_tw,dsig/binsize(diag))

c     y_tw
      diag=diag+1
      call pwhgfill(diag,y_tw,dsig/binsize(diag))

c     eta_tw
      diag=diag+1
      call pwhgfill(diag,eta_tw,dsig/binsize(diag))

c     dphi_tw
      diag=diag+1
      call pwhgfill(diag,dphi_tw,dsig/binsize(diag))

c     dphi_tw
      diag=diag+1
      call pwhgfill(diag,dphi_tw,dsig/binsize(diag))

c-----bw
c     pt_bw
      diag=diag+1
      if(nw.gt.0) call pwhgfill(diag,pt_bw,dsig/binsize(diag))

c     pt_bw
      diag=diag+1
      if(p_bw(0).gt.0) call pwhgfill(diag,pt_bw,dsig/binsize(diag))

c     pt_bw
      diag=diag+1
      if(p_bw(0).gt.0) call pwhgfill(diag,pt_bw,dsig/binsize(diag))

c     y_bw
      diag=diag+1
      if(p_bw(0).gt.0) call pwhgfill(diag,y_bw,dsig/binsize(diag))

c     eta_bw
      diag=diag+1
      if(p_bw(0).gt.0) call pwhgfill(diag,eta_bw,dsig/binsize(diag))

c     minv_bw
      diag=diag+1
      if(p_bw(0).gt.0) call pwhgfill(diag,minv_bw,dsig/binsize(diag))

c     minv_bw
      diag=diag+1
      if(p_bw(0).gt.0) call pwhgfill(diag,minv_bw,dsig/binsize(diag))

c-----b
c     pt_b
      diag=diag+1
      if(nb.gt.0) call pwhgfill(diag,pt_b1,dsig/binsize(diag))

c     pt_b
      diag=diag+1
      if(nb.gt.0) call pwhgfill(diag,pt_b1,dsig/binsize(diag))

c     pt_b
      diag=diag+1
      if(nb.gt.0) call pwhgfill(diag,pt_b1,dsig/binsize(diag))

c     y_b
      diag=diag+1
      if(nb.gt.0) call pwhgfill(diag,y_b1,dsig/binsize(diag))

c     eta_b
      diag=diag+1
      if(nb.gt.0) call pwhgfill(diag,eta_b1,dsig/binsize(diag))

c-----bbar
c     pt_bbar
      diag=diag+1
      if(nbbar.gt.0) call pwhgfill(diag,pt_bbar1,dsig/binsize(diag))

c     pt_bbar
      diag=diag+1
      if(nbbar.gt.0) call pwhgfill(diag,pt_bbar1,dsig/binsize(diag))

c     pt_bbar
      diag=diag+1
      if(nbbar.gt.0) call pwhgfill(diag,pt_bbar1,dsig/binsize(diag))

c     y_bbar
      diag=diag+1
      if(nbbar.gt.0) call pwhgfill(diag,y_bbar1,dsig/binsize(diag))
      
c     eta_bbar
      diag=diag+1
      if(nbbar.gt.0) call pwhgfill(diag,eta_bbar1,dsig/binsize(diag))

c-----hardest lepton
c     pt_l1
      diag=diag+1
      if(nl.gt.0) call pwhgfill(diag,pt_l1,dsig/binsize(diag))

c     pt_l1
      diag=diag+1
      if(nl.gt.0) call pwhgfill(diag,pt_l1,dsig/binsize(diag))

c     pt_l1
      diag=diag+1
      if(nl.gt.0) call pwhgfill(diag,pt_l1,dsig/binsize(diag))

c     y_l1
      diag=diag+1
      if(nl.gt.0) call pwhgfill(diag,y_l1,dsig/binsize(diag))
      
c     eta_l1
      diag=diag+1
      if(nl.gt.0) call pwhgfill(diag,eta_l1,dsig/binsize(diag))

c-----second hardest lepton
c     pt_l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,pt_l2,dsig/binsize(diag))

c     pt_l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,pt_l2,dsig/binsize(diag))

c     pt_l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,pt_l2,dsig/binsize(diag))

c     y_l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,y_l2,dsig/binsize(diag))
      
c     eta_l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,eta_l2,dsig/binsize(diag))

c-----lep1-lep2
c     pt_l1l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,pt_l1l2,dsig/binsize(diag))

c     pt_l1l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,pt_l1l2,dsig/binsize(diag))

c     pt_l1l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,pt_l1l2,dsig/binsize(diag))

c     y_l1l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,y_l1l2,dsig/binsize(diag))
      
c     eta_l1l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,eta_l1l2,dsig/binsize(diag))

c     minv_l1l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,minv_l1l2,dsig/binsize(diag))

c     dphi_l1l2
      diag=diag+1
      if(nl.gt.1) call pwhgfill(diag,dphi_l1l2,dsig/binsize(diag))

c-----lep (from top)
c     pt_lt
      diag=diag+1
      if(nlt.gt.0) call pwhgfill(diag,pt_lt,dsig/binsize(diag))

c     pt_lt
      diag=diag+1
      if(nlt.gt.0) call pwhgfill(diag,pt_lt,dsig/binsize(diag))

c     pt_lt
      diag=diag+1
      if(nlt.gt.0) call pwhgfill(diag,pt_lt,dsig/binsize(diag))

c     y_lt
      diag=diag+1
      if(nlt.gt.0) call pwhgfill(diag,y_lt,dsig/binsize(diag))
      
c     eta_lt
      diag=diag+1
      if(nlt.gt.0) call pwhgfill(diag,eta_lt,dsig/binsize(diag))

c-----lep (from primary W)
c     pt_lw
      diag=diag+1
      if(nlw.gt.0) call pwhgfill(diag,pt_lw,dsig/binsize(diag))

c     pt_lw
      diag=diag+1
      if(nlw.gt.0) call pwhgfill(diag,pt_lw,dsig/binsize(diag))

c     pt_lw
      diag=diag+1
      if(nlw.gt.0) call pwhgfill(diag,pt_lw,dsig/binsize(diag))

c     y_lw
      diag=diag+1
      if(nlw.gt.0) call pwhgfill(diag,y_lw,dsig/binsize(diag))
      
c     eta_lw
      diag=diag+1
      if(nlw.gt.0) call pwhgfill(diag,eta_lw,dsig/binsize(diag))

c-----hardest Bjet
c     pt_bj1
      diag=diag+1
      if(ibj1.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_bj1,dsig/binsize(diag))

c     pt_bj1
      diag=diag+1
      if(ibj1.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_bj1,dsig/binsize(diag))

c     pt_bj1
      diag=diag+1
      if(ibj1.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_bj1,dsig/binsize(diag))

c     y_bj1
      diag=diag+1
      if(ibj1.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,y_bj1,dsig/binsize(diag))

c     eta_bj1
      diag=diag+1
      if(ibj1.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,eta_bj1,dsig/binsize(diag))

c-----second hardest Bjet
c     pt_bj2
      diag=diag+1
      if(ibj2.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_bj2,dsig/binsize(diag))

c     pt_bj2
      diag=diag+1
      if(ibj2.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_bj2,dsig/binsize(diag))

c     pt_bj2
      diag=diag+1
      if(ibj2.gt.0.and..not.skipjet) 
     $     call pwhgfill(diag,pt_bj2,dsig/binsize(diag))

c     y_bj2
      diag=diag+1
      if(ibj2.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,y_bj2,dsig/binsize(diag))

c     eta_bj2
      diag=diag+1
      if(ibj2.gt.0.and..not.skipjet)
     $     call pwhgfill(diag,eta_bj2,dsig/binsize(diag))

c-----total
      diag=diag+1
      call pwhgfill(diag,1.5d0,dsig/binsize(diag))

      else
         diag=diag+numplots
      endif


      goto 666

 999  end
      


      subroutine getrapidity(p,y)
      implicit none
      real * 8 p(0:3),y
      real *8 tiny
      parameter (tiny=1.d-8)
c     !: protect for small p(0)-p(3) values
      if(dabs(p(0)-p(3)).lt.tiny) then
         y=sign(1.d0,p(3))*1.d8
      else
         y=0.5d0*log((p(0)+p(3))/(p(0)-p(3)))
      endif
      end

      subroutine getinvmass(p,m)
      implicit none
      real * 8 p(0:3),m
      m=sqrt(dabs(p(0)**2-p(1)**2-p(2)**2-p(3)**2))
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

