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

C ---------------------- C
C - Total E_T spectrum - C
C ---------------------- C

C - Only with MC generation cuts!
      diag=1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'Total E0T1','LOG',binsize(diag),0d0,500d0)

C ---------------- C
C - ET1 > 20 GeV - C
C ---------------- C

      diag=10

C - Pseudorapidity of the 1st jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J11 E0T11 > 20','LOG',
     1                binsize(diag),-5d0,5d0)

C - Pseudorapidity of the 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J21 E0T11 > 20','LOG',
     1                binsize(diag),-5d0,5d0)

C - Delta Eta between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'DH0J21 E0T11 > 20','LOG',
     1                binsize(diag),-5d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 > 20','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 > 20','LOG',
     $                binsize(diag),0d0,6d0)

C ---------------- C
C - ET1 > 40 GeV - C
C ---------------- C

C - Pseudorapidity of the 1st jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J11 E0T11 > 40','LOG',
     1                binsize(diag),-5d0,5d0)

C - Pseudorapidity of the 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J21 E0T11 > 40','LOG',
     1                binsize(diag),-5d0,5d0)

C - Delta Eta between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'DH0J21 E0T11 > 40','LOG',
     1                binsize(diag),-5d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 > 40','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 > 40','LOG',
     $                binsize(diag),0d0,6d0)

C ----------------- C
C - ET1 > 100 GeV - C
C ----------------- C

C - Pseudorapidity of the 1st jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J11 E0T11 > 100','LOG',
     1                binsize(diag),-5d0,5d0)

C - Pseudorapidity of the 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J21 E0T11 > 100','LOG',
     1                binsize(diag),-5d0,5d0)

C - Delta Eta between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'DH0J21 E0T11 > 100','LOG',
     1                binsize(diag),-5d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 > 100','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 > 100','LOG',
     $                binsize(diag),0d0,6d0)


C -------------------------- C
C - Now looking to 3rd jet - C
C -------------------------- C

      diag=30

C - p_T of the 3rd jet
      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'p0T,J31','LOG',
     1                binsize(diag),1d0,100d0)

C - Pseudorapidity of the 3rd jet, p_T,3 > 10
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J31, p0T,J31>10','LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity of the 3rd jet, p_T,3 > 10
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y0J31, p0T,J31>10','LOG',
     $                binsize(diag),-5d0,5d0)

C - Pseudorapidity of the 3rd jet, p_T,3 > 100
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J31, p0T,J31>100','LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity of the 3rd jet, p_T,3 > 100
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y0J31, p0T,J31>100','LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 10
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y0J31-Y0J121, p0T,J31>10','LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 50
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y0J31-Y0J121, p0T,J31>50','LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 150
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y0J31-Y0J121, p0T,J31>150','LOG',
     $                binsize(diag),-5d0,5d0)

C - Delta Phi between jets 1 & 2 and jet 3 p_T,3 > 10
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF0J3,J121, p0T,J31>10','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta Phi between jets 1 & 2 and jet 3 p_T,3 > 50
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF0J3,J121, p0T,J31>50','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta Phi between jets 1 & 2 and jet 3 p_T,3 > 150
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF0J3,J121, p0T,J31>150','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between jets 1 & 2 and jet 3 p_T,3 > 10
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR0J3,J121, p0T,J31>10','LOG',
     $                binsize(diag),0d0,6d0)

C - Delta R between jets 1 & 2 and jet 3 p_T,3 > 50
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR0J3,J121, p0T,J31>50','LOG',
     $                binsize(diag),0d0,6d0)

C - Delta R between jets 1 & 2 and jet 3 p_T,3 > 150
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR0J3,J121, p0T,J31>150','LOG',
     $                binsize(diag),0d0,6d0)

C ------------------------ C
C - Dijet invariant mass - C
C ------------------------ C

C - Dijet invariant mass using binning & cuts from arXiv:1002.4594v1,
C - Figure. 1 (D0).
C - http://hepdata.cedar.ac.uk/view/irn8566488;jsessionid=1w84smqmgwfrz
C - Dijet invariant mass computed from the two jets with the largest pT.
C - Both jets must have pT>40 GeV.
C - |y_max| is defined as max(|y_1|,|y_2|) where y_1 and y_2 are the
C - rapidities of the two largest pT jets from which m_JJ is computed.
C - Data are for sqrt(S)=1.96 TeV. I am not totally sure about the jet
C - algorithm but the D0 run II cone plugin seems most likely (the paper
C - says it is a seeded midpoint cone algorithm). 
C - N.B. Also the jet algorithm overlap parameter is not given nor is
C - the the min_jet_Et value, which causes cones to be discarded at if
C - at any iteration they have pt < Et_min_ratio * min_jet_Et. For these
C - we will use the values mentioned in the D0RunIICone plugin in fastjet
C - i.e. 0.5 for the overlap and 6 GeV for min_jet_Et (the plugin advises
C - that D0 used 8 GeV for early run II analysis and 6 GeV for later ones
C - hence as the analysis is 2010 we opt for 6 GeV). We iterate that these
C - values are not given in the D0 paper or Durham reaction database.
C - N.B. There are also some missing ET cuts used in the analysis to
C - remove cosmics, these MISSING ET CUTS ARE NEGLECTED IN THE FOLLOWING!!!
      diag=50
C -     |y_max|<0.4 (51)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 |y0max1|<0.4','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 0.4<|y_max|<0.8 (52)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 0.4<|y0max1|<0.8','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 0.8<|y_max|<1.2 (53)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 0.8<|y0max1|<1.2','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 1.2<|y_max|<1.6 (54)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 1.2<|y0max1|<1.6','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 1.6<|y_max|<2.0 (55)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 1.6<|y0max1|<2.0','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 2.0<|y_max|<2.4 (56)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 2.0<|y0max1|<2.4','LOG',
     $                binsize(diag),0d0,1.525d0)
C
C - Same again but with pT>20 GeV instead of 40 GeV.
C
      diag=60
C -     |y_max|<0.4 (61)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>20 |y0max1|<0.4','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 0.4<|y_max|<0.8 (62)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>20 0.4<|y0max1|<0.8','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 0.8<|y_max|<1.2 (63)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>20 0.8<|y0max1|<1.2','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 1.2<|y_max|<1.6 (64)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>20 1.2<|y0max1|<1.6','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 1.6<|y_max|<2.0 (65)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>20 1.6<|y0max1|<2.0','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 2.0<|y_max|<2.4 (66)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>20 2.0<|y0max1|<2.4','LOG',
     $                binsize(diag),0d0,1.525d0)



C --------------------------------- C
C - Dijet azimuthal decorrelation - C
C --------------------------------- C

C - Dijet azimuthal decorrelation binning & cuts to coincide with
C - hep-ex/0409040, Figure. 1 (D0).
C - http://hepdata.cedar.ac.uk/View/5992206
C - Delta phi = |phi_J1-phi_J2| where J1 and J2 are the two
C - jets of highest transverse momentum. The dijet definition uses
C - and 'iterativeC seed based cone algorithm including midpoints'
C - cone algorithm with Rcone=0.7 [and f=0.5?] and the E-scheme for
C - recombination of particles into jets. The same paper is cited in
C - regard to the jet algorithm as in the dijet invariant mass analysis
C - so we will assume it is what is in fastjet's DORunIICone plugin.
C - However, since this is an 'earlier' run II analysis we will assume
C - 6 GeV for min_jet_Et (instead of 8 GeV - see note above).
C - N.B. p_T^max = pT of hardest jet (pT,J1)
C - CUTS: 
C -  75<pT,J1<100 GeV, pT,J2>40 GeV, |yJ1| < 0.5, |yJ2| < 0.5
C - 100<pT,J1<130 GeV, pT,J2>40 GeV, |yJ1| < 0.5, |yJ2| < 0.5
C - 130<pT,J1<180 GeV, pT,J2>40 GeV, |yJ1| < 0.5, |yJ2| < 0.5
C -     pT,J1>180 GeV, pT,J2>40 GeV, |yJ1| < 0.5, |yJ2| < 0.5

      diag=70
C -    75<p_T^max<100 (71)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF  75 < p0T12max3 < 100 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)
C -   100<p_T^max<130 (72)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF 100 < p0T12max3 < 130 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)
C -   130<p_T^max<180 (73)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF 130 < p0T12max3 < 180 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)
C -       p_T^max>180 (74)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF       p0T12max3 > 180 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)


C -------------------------------------------------- C
C - Inclusive jet pT spectrum using cone algorithm - C
C -------------------------------------------------- C

C - Inclusive jet pT spectrum using cuts from arXiv:0807.2204v4,
C - Figure. 15 (CDF).
C - http://hepdata.cedar.ac.uk/View/7828950 (also CDF_2008_S7828950.aida 
C - in rivet if REACTION database is still broken).
C - CDF midpoint cone algorithm R=0.7, f_merge=0.75, (Rsep=1.3 - Sec. VII)
C - Data starts with pTJet>62 GeV so feel free to use an appropriate cut
C - when binning.
      diag=80
C -   First just with MC generation cuts [NOT in CDF analysis obviously].
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 only generation cuts','LOG',
     $                binsize(diag),0d0,710d0)
C -     |y_jet|<0.1 (81)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 |y0jet1|<0.1','LOG',
     $                binsize(diag),0d0,710d0)
C - 0.1<|y_jet|<0.7 (82)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 0.1<|y0jet1|<0.7','LOG',
     $                binsize(diag),0d0,710d0)
C - 0.7<|y_jet|<1.1 (83)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 0.7<|y0jet1|<1.1','LOG',
     $                binsize(diag),0d0,710d0)
C - 1.1<|y_jet|<1.6 (84)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 1.1<|y0jet1|<1.6','LOG',
     $                binsize(diag),0d0,710d0)
C - 1.6<|y_jet|<2.1 (85)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 1.6<|y0jet1|<2.1','LOG',
     $                binsize(diag),0d0,710d0)


C ------------------------------------------------------ C
C - Inclusive jet Y spectrum using same cone algorithm - C
C ------------------------------------------------------ C
      diag=90
C - |p_T|> 10 (91)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 |p0T1|> 10','LOG',
     $                binsize(diag),-5d0,5d0)
C - |p_T|> 20 (92)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 |p0T1|> 20','LOG',
     $                binsize(diag),-5d0,5d0)
C - |p_T|> 50 (93)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 |p0T1|> 50','LOG',
     $                binsize(diag),-5d0,5d0)
C - |p_T|>100 (94)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 |p0T1|>100','LOG',
     $                binsize(diag),-5d0,5d0)


C ------------------------- C
C - p_T^rel of jets 1 & 2 - C
C ------------------------- C
      diag=95
C - p_T^rel of the hardest jet (96)
      diag=diag+1
      binsize(diag) = 0.5d0
      call pwhgbookup(diag,'p0T12rel3 J1','LOG',binsize(diag),0d0,50d0)
C - p_T^rel of the 2nd hardest jet (97)
      diag=diag+1
      binsize(diag) = 0.5d0
      call pwhgbookup(diag,'p0T12rel3 J2','LOG',binsize(diag),0d0,50d0)

      end


      subroutine buildjets(dsig,mjets,kt,eta,rap,phi,pj,
     $                     pT_rel_J1,pT_rel_J2,jet_algo)
c     arrays to reconstruct jets
      implicit none
      include   '../include/hepevt.h'
      integer   maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real * 8  ptrack(4,maxtrack)
      real * 8  pjet(4,maxjet),pT_rel(maxjet)
      integer   mjets
      real * 8  kt(mjets),eta(mjets),rap(mjets),phi(mjets),pj(4,mjets)
      real * 8  pp
      real * 8  pT_rel_J1,pT_rel_J2
      integer   ntracks,njets
      integer   j,k,mu,jet_algo
      real * 8  getrapidity,absy_jet
      real * 8  dsig
      integer   diag
      real * 8  random
      integer   seed
      data      seed/1/
      save      seed

C - Initialize arrays and counters for output jets
      do j=1,maxtrack
         do mu=1,4
            ptrack(mu,j)=0d0
         enddo
      enddo      
      ntracks=0
      do j=1,maxjet
         do mu=1,4
            pjet(mu,j)=0d0
         enddo
         pT_rel(j)=0d0
      enddo
      njets=0
      do j=1,mjets
         do mu=1,4
            pj(mu,j)=0d0
         enddo
      enddo

C - Extract final state particles to feed to jet finder
      do j=1,nhep
         if (isthep(j).eq.1) then
            if(ntracks.eq.maxtrack) then
               write(*,*) 'analyze: need to increase maxtrack!'
               write(*,*) 'ntracks: ',ntracks
               stop
            endif
            ntracks=ntracks+1
            do mu=1,4
               ptrack(mu,ntracks)=phep(mu,j)
            enddo
         endif
      enddo
      if (ntracks.eq.0) then
         return
      endif

C --------------------------------------------------------------------- C
C - Inclusive jet pT and Y spectra are to be compared to CDF data:    - C    
C --------------------------------------------------------------------- C
C     R = 0.7   radius parameter
C     f = 0.75  overlapping fraction
      call fastjetcdfmidpoint(ptrack,ntracks,0.7d0,0.75d0,pjet,njets,
     $                        pT_rel) 

C -------------------------------------------------- C
C - Inclusive jet pT spectrum using cone algorithm - C
C -------------------------------------------------- C
      diag=80
C -   First just with MC generation cuts [NOT in CDF analysis obviously].
      do j=1,njets
         call pwhgfill(diag,sqrt(pjet(1,j)**2+pjet(2,j)**2),dsig)
      enddo
C -     |y_jet|<0.1 (81)
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         if(absy_jet.le.0.1d0)
     $        call pwhgfill(diag,sqrt(pjet(1,j)**2+pjet(2,j)**2),dsig)
      enddo
C - 0.1<|y_jet|<0.7 (82)
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         if(absy_jet.gt.0.1d0.and.absy_jet.le.0.7d0)
     $        call pwhgfill(diag,sqrt(pjet(1,j)**2+pjet(2,j)**2),dsig)
      enddo
C - 0.7<|y_jet|<1.1 (83)
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         if(absy_jet.gt.0.7d0.and.absy_jet.le.1.1d0)
     $        call pwhgfill(diag,sqrt(pjet(1,j)**2+pjet(2,j)**2),dsig)
      enddo
C - 1.1<|y_jet|<1.6 (84)
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         if(absy_jet.gt.1.1d0.and.absy_jet.le.1.6d0)
     $        call pwhgfill(diag,sqrt(pjet(1,j)**2+pjet(2,j)**2),dsig)
      enddo
C - 1.6<|y_jet|<2.1 (85)
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         if(absy_jet.gt.1.6d0.and.absy_jet.le.2.1d0)
     $        call pwhgfill(diag,sqrt(pjet(1,j)**2+pjet(2,j)**2),dsig)
      enddo

C ------------------------------------------------------ C
C - Inclusive jet Y spectrum using same cone algorithm - C
C ------------------------------------------------------ C
      diag=90
C - |p_T|> 10 (91)
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt. 10d0)
     $        call pwhgfill(diag,absy_jet,dsig)
      enddo
C - |p_T|> 20 (92)
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt. 20d0)
     $        call pwhgfill(diag,absy_jet,dsig)
      enddo
C - |p_T|> 50 (93)
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt. 50d0)
     $        call pwhgfill(diag,absy_jet,dsig)
      enddo
C - |p_T|>100 (94)
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt.100d0)
     $        call pwhgfill(diag,absy_jet,dsig)
      enddo


C --------------------------------------------------------------------- C
C - Everything else is analysed using the D0 RunII midpoint cone algo - C
C --------------------------------------------------------------------- C
C -   R = 0.7  radius parameter
C -   E_T,min = 6.0 GeV pt cones discarded if pt < Et_min_ratio * min_jet_Et
C               (see note above where dijet mass histograms are booked).
C -   f = 0.5  overlapping fraction

C - Initialize arrays and counters for output jets
      do j=1,maxtrack
         do mu=1,4
            ptrack(mu,j)=0d0
         enddo
      enddo      
      ntracks=0
      do j=1,maxjet
         do mu=1,4
            pjet(mu,j)=0d0
         enddo
         pT_rel(j)=0d0
      enddo
      njets=0
      do j=1,mjets
         do mu=1,4
            pj(mu,j)=0d0
         enddo
      enddo

C - Extract final state particles to feed to jet finder
      do j=1,nhep
         if (isthep(j).eq.1) then
            if(ntracks.eq.maxtrack) then
               write(*,*)
     #              'analyze: too many particles, increase maxtrack'
               stop
            endif
            ntracks=ntracks+1
            do mu=1,4
               ptrack(mu,ntracks)=phep(mu,j)
            enddo
         endif
      enddo
      if (ntracks.eq.0) then
         return
      endif

      call fastjetd0runiicone(ptrack,ntracks,0.7d0,6d0,0.5d0,pjet,njets,
     $                        pT_rel) 

C ------------------------------------- C
C - Store pT_rel's of jet 1 and jet 2 - C
C --------------------------------------C
      if(njets.ge.1) pT_rel_J1=pT_rel(1)
      if(njets.ge.2) pT_rel_J2=pT_rel(2)

C --------------------------------------------------------------------- C
C - Computing arrays of useful kinematics quantities for hardest jets - C
C --------------------------------------------------------------------- C
      mjets=min(mjets,njets)
      do j=1,mjets
         kt(j)=sqrt(pjet(1,j)**2+pjet(2,j)**2)
         pp = sqrt(kt(j)**2+pjet(3,j)**2)
         eta(j)=0.5d0*log((pp+pjet(3,j))/(pp-pjet(3,j)))
         rap(j)=getrapidity(pjet(4,j),pjet(3,j))
         phi(j)=atan2(pjet(2,j),pjet(1,j))
      enddo

C - Copying the momenta of the hardest jets
      do j=1,mjets
         do k=1,4
            pj(k,j)=pjet(k,j)
         enddo
      enddo

C - If we only have two tracks, the hardest jet is ill defined;
C   take one of the two randomly (this will screw up the error
C   estimates, spoiling correlated events ...)
      if(ntracks.eq.2) then
         if(random(seed).gt.0.5) then
            pp=eta(1)
            eta(1) = eta(2)
            eta(2) = pp
            pp=rap(1)
            rap(1) = rap(2)
            rap(2) = pp
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
      integer diag
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
      real * 8 ktjets(4),etajets(4),rapjets(4),phijets(4),pj(4,4)
      real * 8 pT_rel_J1,pT_rel_J2
      real * 8 getrapidity,y12,phi12,dphi312,dr312,mjj,et1
      integer njets
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer j,i
      real *8 et,dphi,dR,absy_max

C - Pico to microbarn conversion:
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


C ---------------------- C
C - Total E_T spectrum - C
C ---------------------- C

C - Only with MC generation cuts!
      et=0
      do ihep=1,nhep
         if(isthep(ihep).eq.1) then
            et=et+sqrt(phep(1,ihep)**2+phep(2,ihep)**2)
         endif
      enddo
      diag=1
      call pwhgfill(diag,et,dsig)

C - OK have the total E_T from all the particles in the event.
C - Everything we want to look at from now on involves only the
C - 1st, 2nd and 3rd hardest jets in the event:
      njets=3
      call buildjets(dsig,njets,ktjets,etajets,rapjets,phijets,pj,
     $               pT_rel_J1,pT_rel_J2,1) 


C ----------------------------------- C
C - Work out E_T of the hardest jet - C
C ----------------------------------- C

      et1 = 0.
      if(njets.ge.1) then
         et1 = pj(4,1)**2*ktjets(1)**2/(ktjets(1)**2+pj(3,1)**2)
         if(et1.ge.0) then
            et1 =  sqrt( et1)
         else
            et1 = -sqrt(-et1)
         endif
      endif

C ---------------- C
C - ET1 > 20 GeV - C
C ---------------- C

      diag=10
      if(njets.ge.2.and.et1.ge.20.) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events
         diag=diag+1
         call pwhgfill(diag,etajets(1),dsig)
         diag=diag+1
         call pwhgfill(diag,etajets(2),dsig)
         
C - Delta Eta between 1st and 2nd jet in >= 2 jet events
         diag=diag+1
         call pwhgfill(diag,etajets(1)-etajets(2),dsig)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events
         dphi=abs(phijets(1)-phijets(2))
         if(dphi.gt.pi) dphi=dphi-pi*int(dphi/pi)
         diag=diag+1
         call pwhgfill(diag,dphi,dsig)

C - Delta R between 1st and 2nd jet in >= 2 jet events
         dR=sqrt((etajets(1)-etajets(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig)

      endif

C ---------------- C
C - ET1 > 40 GeV - C
C ---------------- C

      diag=15
      if(njets.ge.2.and.et1.ge.40.) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events
         diag=diag+1
         call pwhgfill(diag,etajets(1),dsig)
         diag=diag+1
         call pwhgfill(diag,etajets(2),dsig)
         
C - Delta Eta between 1st and 2nd jet in >= 2 jet events
         diag=diag+1
         call pwhgfill(diag,etajets(1)-etajets(2),dsig)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events
         dphi=abs(phijets(1)-phijets(2))
         if(dphi.gt.pi) dphi=dphi-pi*int(dphi/pi)
         diag=diag+1
         call pwhgfill(diag,dphi,dsig)

C - Delta R between 1st and 2nd jet in >= 2 jet events
         dR=sqrt((etajets(1)-etajets(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig)

      endif

C ----------------- C
C - ET1 > 100 GeV - C
C ----------------- C

      diag=20
      if(njets.ge.2.and.et1.ge.100.) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events
         diag=diag+1
         call pwhgfill(diag,etajets(1),dsig)
         diag=diag+1
         call pwhgfill(diag,etajets(2),dsig)
         
C - Delta Eta between 1st and 2nd jet in >= 2 jet events
         diag=diag+1
         call pwhgfill(diag,etajets(1)-etajets(2),dsig)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events
         dphi=abs(phijets(1)-phijets(2))
         if(dphi.gt.pi) dphi=dphi-pi*int(dphi/pi)
         diag=diag+1
         call pwhgfill(diag,dphi,dsig)

C - Delta R between 1st and 2nd jet in >= 2 jet events
         dR=sqrt((etajets(1)-etajets(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig)

      endif

C -------------------------- C
C - Now looking to 3rd jet - C
C -------------------------- C

      diag=30
      if(njets.ge.3) then
C - p_T of the 3rd jet
         diag=diag+1
         call pwhgfill(diag,ktjets(3),dsig)
C - Pseudorapidity of the 3rd jet, p_T,3 > 10
         diag=diag+1
         if(ktjets(3).gt.10.0) call pwhgfill(diag,etajets(3),dsig)
C - Rapidity of the 3rd jet, p_T,3 > 10
         diag=diag+1
         if(ktjets(3).gt.10.0) call pwhgfill(diag,rapjets(3),dsig)
C - Pseudorapidity of the 3rd jet, p_T,3 > 100
         diag=diag+1
         if(ktjets(3).gt.100.0) call pwhgfill(diag,etajets(3),dsig)
C - Rapidity of the 3rd jet, p_T,3 > 100 
         diag=diag+1
         if(ktjets(3).gt.100.0) call pwhgfill(diag,rapjets(3),dsig)

         y12     = getrapidity(pj(4,1)+pj(4,2),pj(3,1)+pj(3,2))
         phi12   = atan2(pj(2,1)+pj(2,2),pj(1,1)+pj(1,2))
         dphi312 = abs(phijets(3)-phi12)
         if(dphi312.gt.pi) dphi312=dphi312-pi*int(dphi312/pi)
         dr312   = sqrt((rapjets(3)-y12)**2+dphi312**2)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 10
         diag=diag+1
         if(ktjets(3).gt. 10.0) call pwhgfill(diag,rapjets(3)-y12,dsig)
C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 50
         diag=diag+1
         if(ktjets(3).gt. 50.0) call pwhgfill(diag,rapjets(3)-y12,dsig)
C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 100
         diag=diag+1
         if(ktjets(3).gt.100.0) call pwhgfill(diag,rapjets(3)-y12,dsig)
C - Delta Phi between jets 1 & 2 and jet 3 p_T,3 > 10
         diag=diag+1
         if(ktjets(3).gt. 10.0) call pwhgfill(diag,dphi312,dsig)
C - Delta Phi between jets 1 & 2 and jet 3 p_T,3 > 50
         diag=diag+1
         if(ktjets(3).gt. 50.0) call pwhgfill(diag,dphi312,dsig)
C - Delta Phi between jets 1 & 2 and jet 3 p_T,3 > 100
         diag=diag+1
         if(ktjets(3).gt.100.0) call pwhgfill(diag,dphi312,dsig)
C - Delta R between jets 1 & 2 and jet 3 p_T,3 > 10
         diag=diag+1
         if(ktjets(3).gt. 10.0) call pwhgfill(diag,dr312,dsig)
C - Delta R between jets 1 & 2 and jet 3 p_T,3 > 50
         diag=diag+1
         if(ktjets(3).gt. 50.0) call pwhgfill(diag,dr312,dsig)
C - Delta R between jets 1 & 2 and jet 3 p_T,3 > 100
         diag=diag+1
         if(ktjets(3).gt.100.0) call pwhgfill(diag,dr312,dsig)

      endif

C ------------------------ C
C - Dijet invariant mass - C
C ------------------------ C

      diag=50
      if(njets.ge.2) then
C - Computing the dijet invariant mass:
         if(njets.ge.2) then 
            mjj = pj(4,1)*pj(4,2) - pj(1,1)*pj(1,2)
     $          - pj(2,1)*pj(2,2) - pj(3,1)*pj(3,2)
            if(mjj.ge.0) then
               mjj =  sqrt(mjj)
            else
               mjj = -sqrt(-mjj)
            endif
            mjj = mjj / 1000d0 ! Binning is in TeV
         endif
         absy_max=abs(max(rapjets(1),rapjets(2)))
C - Both jets must have pT>40 GeV.
         if(ktjets(1).ge.40.0.and.ktjets(2).ge.40.0) then
C -     |y_max|<0.4 (51)
            diag=diag+1
            if(absy_max.le.0.4)
     $           call pwhgfill(diag,mjj,dsig)
C - 0.4<|y_max|<0.8 (52)
            diag=diag+1
            if(absy_max.gt.0.4.and.absy_max.le.0.8)
     $           call pwhgfill(diag,mjj,dsig)
C - 0.8<|y_max|<1.2 (53)
            diag=diag+1
            if(absy_max.gt.0.8.and.absy_max.le.1.2)
     $           call pwhgfill(diag,mjj,dsig)
C - 1.2<|y_max|<1.6 (54)
            diag=diag+1
            if(absy_max.gt.1.2.and.absy_max.le.1.6)
     $           call pwhgfill(diag,mjj,dsig)
C - 1.6<|y_max|<2.0 (55)
            diag=diag+1
            if(absy_max.gt.1.6.and.absy_max.le.2.0)
     $           call pwhgfill(diag,mjj,dsig)
C - 2.0<|y_max|<2.4 (56)
            diag=diag+1
            if(absy_max.gt.2.0.and.absy_max.le.2.4)
     $           call pwhgfill(diag,mjj,dsig)
         endif
      endif

      diag=60
      if(njets.ge.2) then
C - Both jets must have pT>20 GeV.
         absy_max=abs(max(rapjets(1),rapjets(2)))
         if(ktjets(1).ge.20.0.and.ktjets(2).ge.20.0) then
C -     |y_max|<0.4 (61)
            diag=diag+1
            if(absy_max.le.0.4)
     $           call pwhgfill(diag,mjj,dsig)
C - 0.4<|y_max|<0.8 (62)
            diag=diag+1
            if(absy_max.gt.0.4.and.absy_max.le.0.8)
     $           call pwhgfill(diag,mjj,dsig)
C - 0.8<|y_max|<1.2 (63)
            diag=diag+1
            if(absy_max.gt.0.8.and.absy_max.le.1.2)
     $           call pwhgfill(diag,mjj,dsig)
C - 1.2<|y_max|<1.6 (64)
            diag=diag+1
            if(absy_max.gt.1.2.and.absy_max.le.1.6)
     $           call pwhgfill(diag,mjj,dsig)
C - 1.6<|y_max|<2.0 (65)
            diag=diag+1
            if(absy_max.gt.1.6.and.absy_max.le.2.0)
     $           call pwhgfill(diag,mjj,dsig)
C - 2.0<|y_max|<2.4 (66)
            diag=diag+1
            if(absy_max.gt.2.0.and.absy_max.le.2.4)
     $           call pwhgfill(diag,mjj,dsig)
         endif
      endif

C --------------------------------- C
C - Dijet azimuthal decorrelation - C
C --------------------------------- C

      diag=70
      if(njets.ge.2.and.abs(rapjets(1)).le.0.5d0
     $             .and.abs(rapjets(2)).le.0.5d0
     $             .and.ktjets(2).ge.40d0) then
C - Delta Phi between 1st and 2nd jet in >= 2 jet events for
C - |yJ1| < 0.5, |yJ2| < 0.5, pT,J2 > 40 ... 
         dphi=abs(phijets(1)-phijets(2))
         if(dphi.gt.pi) dphi=dphi-pi*int(dphi/pi)
C - ... and 75<pT,J1<100 GeV
         diag=diag+1
         if(ktjets(1).gt. 75d0.and.ktjets(1).le.100d0)
     $        call pwhgfill(diag,dphi,dsig)
C - ... and 100<pT,J1<130 GeV
         diag=diag+1
         if(ktjets(1).gt.100d0.and.ktjets(1).le.130d0)
     $        call pwhgfill(diag,dphi,dsig)
C - ... and 130<pT,J1<180 GeV
         diag=diag+1
         if(ktjets(1).gt.130d0.and.ktjets(1).le.180d0)
     $        call pwhgfill(diag,dphi,dsig)
C - ... and     pT,J1>180 GeV
         diag=diag+1
         if(ktjets(1).gt.180d0)
     $        call pwhgfill(diag,dphi,dsig)
      endif

C - The following analysis was moved into the buildjets
C - routine in order to make sure ALL jets found went into
C - the plots instead of what is called 'njets' in this 
C - subroutine. Perhaps this is wrong.
c$$$C -------------------------------------------------- C
c$$$C - Inclusive jet pT spectrum using cone algorithm - C
c$$$C -------------------------------------------------- C
c$$$
c$$$      diag=80
c$$$C -   First just with MC generation cuts [NOT in CDF analysis obviously].
c$$$      do j=1,njets
c$$$         call pwhgfill(diag,ktjets(j),dsig)
c$$$      enddo
c$$$C -     |y_jet|<0.1 (81)
c$$$      diag=diag+1
c$$$      do j=1,njets
c$$$         if(abs(rapjets(j)).le.0.1d0)
c$$$     $        call pwhgfill(diag,ktjets(j),dsig)
c$$$      enddo
c$$$C - 0.1<|y_jet|<0.7 (82)
c$$$      diag=diag+1
c$$$      do j=1,njets
c$$$         if(abs(rapjets(j)).gt.0.1d0.and.abs(rapjets(j)).le.0.7d0)
c$$$     $        call pwhgfill(diag,ktjets(j),dsig)
c$$$      enddo
c$$$C - 0.7<|y_jet|<1.1 (83)
c$$$      diag=diag+1
c$$$      do j=1,njets
c$$$         if(abs(rapjets(j)).gt.0.7d0.and.abs(rapjets(j)).le.1.1d0)
c$$$     $        call pwhgfill(diag,ktjets(j),dsig)
c$$$      enddo
c$$$C - 1.1<|y_jet|<1.6 (84)
c$$$      diag=diag+1
c$$$      do j=1,njets
c$$$         if(abs(rapjets(j)).gt.1.1d0.and.abs(rapjets(j)).le.1.6d0)
c$$$     $        call pwhgfill(diag,ktjets(j),dsig)
c$$$      enddo
c$$$C - 1.6<|y_jet|<2.1 (85)
c$$$      diag=diag+1
c$$$      do j=1,njets
c$$$         if(abs(rapjets(j)).gt.1.6d0.and.abs(rapjets(j)).le.2.1d0)
c$$$     $        call pwhgfill(diag,ktjets(j),dsig)
c$$$      enddo
C ------------------------------------------------------ C
C - Inclusive jet Y spectrum using same cone algorithm - C
C ------------------------------------------------------ C
      diag=90
C - |p_T|> 10 (91)
      diag=diag+1
      do j=1,njets
         if(ktjets(j).gt. 10d0) call pwhgfill(diag,rapjets(j),dsig)
      enddo
C - |p_T|> 20 (92)
      diag=diag+1
      do j=1,njets
         if(ktjets(j).gt. 20d0) call pwhgfill(diag,rapjets(j),dsig)
      enddo
C - |p_T|> 50 (93)
      diag=diag+1
      do j=1,njets
         if(ktjets(j).gt. 50d0) call pwhgfill(diag,rapjets(j),dsig)
      enddo
C - |p_T|>100 (94)
      diag=diag+1
      do j=1,njets
         if(ktjets(j).gt.100d0) call pwhgfill(diag,rapjets(j),dsig)
      enddo

C ------------------------- C
C - p_T^rel of jets 1 & 2 - C
C ------------------------- C
      diag=95
C - p_T^rel of the hardest jet (96)
      diag=diag+1
      if(njets.ge.1) call pwhgfill(diag,pT_rel_J1,dsig)
C - p_T^rel of the 2nd hardest jet (97)
      diag=diag+1
      if(njets.ge.1) call pwhgfill(diag,pT_rel_J2,dsig)


      if(WHCPRG.eq.'NLO   ') then
         continue
      elseif ((WHCPRG.eq.'HERWIG').or.(WHCPRG.eq.'PYTHIA')) then
         continue
      endif
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

C *********************************************************************** C
      FUNCTION GETRAPIDITY(EN,PL)
C     Returns the rapidity calculated from E and P (EN,PL)
C *********************************************************************** C
      IMPLICIT NONE
      DOUBLE PRECISION GETRAPIDITY,EN,PL,TINY,XPLUS,XMINUS,Y
      PARAMETER (TINY=1.d-5)
C
      XPLUS=EN+PL
      XMINUS=EN-PL
      IF(XPLUS.GT.TINY.AND.XMINUS.GT.TINY) THEN
        IF((XPLUS/XMINUS).GT.TINY) THEN
          y=0.5d0*LOG(XPLUS/XMINUS)
        ELSE
          y=SIGN(1.d0,PL)*1.d8
        ENDIF
      ELSE
        Y=SIGN(1.d0,PL)*1.d8
      ENDIF
      GETRAPIDITY=Y
      RETURN
      END
