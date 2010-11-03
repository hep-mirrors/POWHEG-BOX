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
      real * 8 binsize(700)
      common/pwhghistcommon/binsize
      character * 10 cut

      cut = 'cuts'

      call pwhginihist

C ---------------------- C
C - Total E_T spectrum - C
C ---------------------- C

C - Only with MC generation cuts! (id 1)
      diag=1
      binsize(diag) = 5d0
      call pwhgbookup(diag,'Total E0T1','LOG',binsize(diag),0d0,500d0)

C ---------------------- C
C - ET1 & ET2 > 20 GeV - C
C ---------------------- C

      diag=10

C - Pseudorapidity of the 1st and 2nd jet in >= 2 jet events (id 11)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J1/J21 E0T11 & E0T21 > 20 GeV','LOG',
     1                binsize(diag),-5d0,5d0)

C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 12)
      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'abs(DH01,21) E0T11 & E0T21 > 20 GeV','LOG',
     1                binsize(diag),0d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 13)
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 & E0T21 > 20 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 14)
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 & E0T21 > 20 GeV','LOG',
     $                binsize(diag),0d0,6d0)

C ---------------------- C
C - ET1 & ET2 > 40 GeV - C
C ---------------------- C

      diag=15

C - Pseudorapidity of the 1st jet in >= 2 jet events (id 16)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J1/J21 E0T11 & E0T21 > 40 GeV','LOG',
     1                binsize(diag),-5d0,5d0)

C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 17)
      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'abs(DH01,21) E0T11 & E0T21 > 40 GeV','LOG',
     1                binsize(diag),0d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 18)
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 & E0T21 > 40 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 19)
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 & E0T21 > 40 GeV','LOG',
     $                binsize(diag),0d0,6d0)

C ----------------------- C
C - ET1 & ET2 > 100 GeV - C
C ----------------------- C

      diag=20

C - Pseudorapidity of the 1st jet in >= 2 jet events (id 21)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J1/J21 E0T11 & E0T21 > 100 GeV','LOG',
     1                binsize(diag),-5d0,5d0)

C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 22)
      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'abs(DH01,21) E0T11 & E0T21 > 100 GeV','LOG',
     1                binsize(diag),0d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 23)
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 & E0T21 > 100 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 24)
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 & E0T21 > 100 GeV','LOG',
     $                binsize(diag),0d0,6d0)

C ---------------- C
C - ET1 > 20 GeV - C
C ---------------- C

      diag=25

C - Pseudorapidity of the 1st and 2nd jet in >= 2 jet events (id 26)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J1/J21 E0T11 > 20 GeV','LOG',
     1                binsize(diag),-5d0,5d0)

C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 27)
      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'abs(DH01,21) E0T11 > 20 GeV','LOG',
     1                binsize(diag),0d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 28)
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 > 20 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 29)
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 > 20 GeV','LOG',
     $                binsize(diag),0d0,6d0)

C ---------------- C
C - ET1 > 40 GeV - C
C ---------------- C

      diag=30

C - Pseudorapidity of the 1st jet in >= 2 jet events (id 31)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J1/J21 E0T11 > 40 GeV','LOG',
     1                binsize(diag),-5d0,5d0)

C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 32)
      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'abs(DH01,21) E0T11 > 40 GeV','LOG',
     1                binsize(diag),0d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 33)
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 > 40 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 34)
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 > 40 GeV','LOG',
     $                binsize(diag),0d0,6d0)

C ----------------- C
C - ET1 > 100 GeV - C
C ----------------- C

      diag=35

C - Pseudorapidity of the 1st jet in >= 2 jet events (id 36)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J1/J21 E0T11 > 100 GeV','LOG',
     1                binsize(diag),-5d0,5d0)

C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 37)
      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'abs(DH01,21) E0T11 > 100 GeV','LOG',
     1                binsize(diag),0d0,5d0)

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 38)
      diag=diag+1
      binsize(diag) = pi/50d0
      call pwhgbookup(diag,'DF01,21 E0T11 > 100 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 39)
      diag=diag+1
      binsize(diag) = 0.15d0
      call pwhgbookup(diag,'DR01,21 E0T11 > 100 GeV','LOG',
     $                binsize(diag),0d0,6d0)


C ----------------------------------------------------- C
C - Now looking to 3rd jet for p0T11 & p0T21 > 40 GeV - C
C ----------------------------------------------------- C

      diag=40

C - p_T of the 3rd jet (id 41)
      diag=diag+1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'p0T,J31, p0T11 & p0T21 > 40 GeV','LOG',
     1                binsize(diag),1d0,100d0)

C - Pseudorapidity of the 3rd jet, p_T,3 > 10 (id 42)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J31, p0T,J31>10, p0T11 & p0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity of the 3rd jet, p_T,3 > 10 (id 43)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y0J31, p0T,J31>10, p0T11 & p0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),-5d0,5d0)

C - Pseudorapidity of the 3rd jet, p_T,3 > 100 (id 44)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J31, p0T,J31>100, p0T11 & p0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity of the 3rd jet, p_T,3 > 100 (id 45)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y0J31, p0T,J31>100, p0T11 & p0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 10 (id 46)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,
     $               'Y0J31-Y0J121, p0T,J31>10, p0T11 & p0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 50 (id 47)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,
     $               'Y0J31-Y0J121, p0T,J31>50, p0T11 & p0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),-5d0,5d0)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 100 (id 48)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,
     $              'Y0J31-Y0J121, p0T,J31>100, p0T11 & p0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),-5d0,5d0)


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
      diag=50
C -     |y_max|<0.4 (id 51)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 |y0max1|<0.4','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 0.4<|y_max|<0.8 (id 52)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 0.4<|y0max1|<0.8','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 0.8<|y_max|<1.2 (id 53)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 0.8<|y0max1|<1.2','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 1.2<|y_max|<1.6 (id 54)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 1.2<|y0max1|<1.6','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 1.6<|y_max|<2.0 (id 55)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 1.6<|y0max1|<2.0','LOG',
     $                binsize(diag),0d0,1.525d0)
C - 2.0<|y_max|<2.4 (id 56)
      diag=diag+1
      binsize(diag) = 0.025d0     ! N.B. Binning in  TeV!
      call pwhgbookup(diag,'M0JJ1 pT>40 2.0<|y0max1|<2.4','LOG',
     $                binsize(diag),0d0,1.525d0)

C ---------------------- C
C - Sigma Delta Plots! - C
C ---------------------- C

      diag=56

C - sigma(delta) for et1>delta (id 57)
      diag=diag+1
      binsize(diag) = 2.5d0
      call pwhgbookup(diag,'S(D) E0T11>D','LOG',
     $                binsize(diag),  0d0,250d0)

C - sigma(delta) for et1>delta (zoom out) (id 58)
      diag=diag+1
      binsize(diag) = 5.0d0
      call pwhgbookup(diag,'S(D) E0T11>D','LOG',
     $                binsize(diag),  0d0,500d0)

C - sigma(delta) for et1>20+delta, et2>20 (id 59)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'S(D) E0T11>20+D E0T21>20','LOG',
     $                binsize(diag),  0d0,20d0)

C - sigma(delta) for et1>20, et2>20+delta (id 60)
      diag=diag+1
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'S(D) E0T11>20 E0T21>20+D','LOG',
     $                binsize(diag),-20d0,20d0)

C - sigma(delta) for et1>20+delta, et2>20 (id 61)
      diag=diag+1
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'S(D) E0T11>20+D E0T21>20','LOG',
     $                binsize(diag),  0d0,5d0)

C - sigma(delta) for et1 and et2 > delta (id 62)
      diag=diag+1
      binsize(diag) = 2.5d0
      call pwhgbookup(diag,'S(D) E0T11 & E0T21>D','LOG',
     $                binsize(diag),  0d0,250d0)

C - sigma(delta) for et1 & et2 > delta (zoomed out) (id 63)
      diag=diag+1
      binsize(diag) = 5.0d0
      call pwhgbookup(diag,'S(D) E0T11 & E0T21>D','LOG',
     $                binsize(diag),  0d0,500d0)

C - sigma(delta) for et1>40+delta et2>40 (id 64)
      diag=diag+1
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'S(D) E0T11>40+D E0T21>40','LOG',
     $                binsize(diag),  0d0,40d0)

C - sigma(delta) for et1>40 et2>40+delta (id 65)
      diag=diag+1
      binsize(diag) = 0.8d0
      call pwhgbookup(diag,'S(D) E0T11>40 E0T21>40+D','LOG',
     $                binsize(diag),-40d0,40d0)

C - sigma(delta) for et1>40+delta et2>40 (id 66)
      diag=diag+1
      binsize(diag) = 0.1d0
      call pwhgbookup(diag,'S(D) E0T11>40+D E0T21>40','LOG',
     $                binsize(diag),  0d0,10d0)

C - sigma(delta) for et1>100+delta et2>100 (id 67)
      diag=diag+1
      binsize(diag) = 1.0d0
      call pwhgbookup(diag,'S(D) E0T11>100+D E0T21>100','LOG',
     $                binsize(diag),0d0,100d0)

C - sigma(delta) for et1>100 et2>100+delta (id 68)
      diag=diag+1
      binsize(diag) = 2.0d0
      call pwhgbookup(diag,'S(D) E0T11>100 E0T21>100+D','LOG',
     $                binsize(diag),-100d0,100d0)

C - sigma(delta) for et1>100+delta et2>100 (id 69)
      diag=diag+1
      binsize(diag) = 0.25d0
      call pwhgbookup(diag,'S(D) E0T11>100+D E0T21>100','LOG',
     $                binsize(diag),0d0,25d0)

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
C -    75<p_T^max<100 (id 71)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF  75 < p0T12max3 < 100 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)
C -   100<p_T^max<130 (id 72)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF 100 < p0T12max3 < 130 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)
C -   130<p_T^max<180 (id 73)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF 130 < p0T12max3 < 180 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)
C -       p_T^max>180 (id 74)
      diag=diag+1
      binsize(diag) = 2d0*pi/128d0  ! Fig 1 has many irregular bin sizes.
      call pwhgbookup(diag,'DF       p0T12max3 > 180 GeV','LOG',
     $                binsize(diag),pi/2d0,pi)


C -------------------------------------------------- C
C - Inclusive jet pT spectrum using cone algorithm - C
C -------------------------------------------------- C

C - Inclusive jet pT spectrum using cuts from arXiv:0807.2204v4,
C - Figure. 15 (CDF).
C - http://hepdata.cedar.ac.uk/view/p7628
C - CDF midpoint cone algorithm R=0.7, f_merge=0.75, (Rsep=1.3 - Sec. VII)
C - Data starts with pTJet>62 GeV so feel free to use an appropriate cut
C - when binning.
      diag=80
C -   First just with MC generation cuts (id 80) [NOT in CDF obviously].
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 only generation cuts','LOG',
     $                binsize(diag),0d0,710d0)
C -     |y_jet|<0.1 (id 81)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 |y0jet1|<0.1','LOG',
     $                binsize(diag),0d0,710d0)
C - 0.1<|y_jet|<0.7 (id 82)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 0.1<|y0jet1|<0.7','LOG',
     $                binsize(diag),0d0,710d0)
C - 0.7<|y_jet|<1.1 (id 83)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 0.7<|y0jet1|<1.1','LOG',
     $                binsize(diag),0d0,710d0)
C - 1.1<|y_jet|<1.6 (id 84)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 1.1<|y0jet1|<1.6','LOG',
     $                binsize(diag),0d0,710d0)
C - 1.6<|y_jet|<2.1 (id 85)
      diag=diag+1
      binsize(diag) = 10d0     ! ! Fig 15 has many irregular bin sizes.
      call pwhgbookup(diag,'p0T12JET3 1.6<|y0jet1|<2.1','LOG',
     $                binsize(diag),0d0,710d0)


C ------------------------------------------------------ C
C - Inclusive jet Y spectrum using same cone algorithm - C
C ------------------------------------------------------ C
      diag=90
C - |p_T|> 10 (id 91)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 (1-3 incl) |p0T1|> 10','LOG',
     $                binsize(diag),-5d0,5d0)
C - |p_T|> 20 (id 92)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 (1-3 incl) |p0T1|> 20','LOG',
     $                binsize(diag),-5d0,5d0)
C - |p_T|> 50 (id 93)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 (1-3 incl) |p0T1|> 50','LOG',
     $                binsize(diag),-5d0,5d0)
C - |p_T|>100 (id 94)
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Y2JET3 (1-3 incl) |p0T1|>100','LOG',
     $                binsize(diag),-5d0,5d0)


C --------------------------------------------- C
C - p_T^rel of jets 1 & 2, ET1 & ET2 > 40 GeV - C
C --------------------------------------------- C
      diag=95
C - p_T^rel of the hardest jet (id 96)
      diag=diag+1
      binsize(diag) = 0.5d0
      call pwhgbookup(diag,
     $               'p0T12rel3 J1, E0T11 & E0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),0d0,50d0)
C - p_T^rel inclusive in jets 1 and 2 in >= 2 jet events (id 97)
      diag=diag+1
      binsize(diag) = 0.5d0
      call pwhgbookup(diag,
     $               'p0T12rel3 J1 and J2, E0T11 & E0T21 > 40 GeV',
     $               'LOG',
     $                binsize(diag),0d0,50d0)

C ------------------------- C
C - CDF Coherence plot(s) - C
C ------------------------- C
      diag=100

C - Evidence for color coherence in p anti-p collisions at 
C - s**(1/2) = 1.8-TeV, By CDF Collaboration, PRD50:5562-5579,1994.
C - http://hepdata.cedar.ac.uk/View/2952106

C - Pseudorapidity of the third hardest jet (id 101)
C - Cuts: |eta_1|<0.7, |eta_2|<0.7, |phi_1-phi_2|>2.79
C - E_T1 > 110 GeV, E_T3 > 10GeV. In an ideal world I
C - would also know better what exactly was the R=0.7
C - cone algorithm they used.
      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'H0J31','LOG',binsize(diag),-4d0,4d0)
C - N.B. If plotting the 'alpha' variable used in this study
C - one must impose an additional cut 1.1 < DeltaR_23 < pi.
C - I didn't plot alpha because looking at the results of the 
C - CDF study, alpha doesn't discriminate much between anything.

      end


      subroutine buildjets(dsig,mjets,kt,eta,rap,phi,pj,
     $                                et_et,eta_et,rap_et,phi_et,pj_et,
     $                     pT_rel_J1,pT_rel_J2,jet_algo)
c     arrays to reconstruct jets
      implicit none
      include   '../include/hepevt.h'
      include '../include/pwhg_math.h' 
      integer   maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real * 8 binsize(700)
      common/pwhghistcommon/binsize
      real * 8  ptrack(4,maxtrack)
      real * 8  pjet(4,maxjet),pT_rel(maxjet)
      integer   mjets
      real * 8  kt(mjets),eta(mjets),rap(mjets),phi(mjets),pj(4,mjets)
      real      et_array(maxjet)
      integer   et_idx(maxjet)
      real * 8  et_et(mjets),eta_et(mjets),rap_et(mjets),phi_et(mjets)
      real * 8  pj_et(4,mjets),tmp1
      integer   tmp_int
      real * 8  pp,phi1,phi2,dphi12,eta0,eta1,eta2,eta3,et12,et22,et32
      real * 8  pT_rel_J1,pT_rel_J2
      real * 8  mET(2),tot_ET,pT_j,modp_j,ET_j
      logical   passed_mET
      integer   ntracks,njets
      integer   j,k,mu,jet_algo
      real * 8  getrapidity,absy_jet,the_pt
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

C ------------------------------------------------------ C
C - Inclusive jet pT spectrum using CDF cone algorithm - C
C ------------------------------------------------------ C
C - Compute the total and missing ET here (CDF arXiv:0807.2204v4).
      mET(1)=0d0
      mET(2)=0d0
      tot_ET=0d0
      do j=1,njets
         call get_pseudorap(pjet(1,j),eta0)
C - N.B. We have neglected a small E_T cut here, which goes with
C - |eta|<3.6; each calo tower is required to have ET > 100 MeV
C - (arXiv:0807.2204v4 item 36 in the bibliography)!!!
C - N.B. Also the sum used to obtain mET and tot_ET is experimentally
C - defined to be the sum over calorimeter cells, not a sum over jets!
C - In any case the efficiency of this cut is said to range from
C - 100 % (for low pT jets) to 90 % for high pT jets. So the impact
C - of this should in general be small and, hopefully, it is fairly
C - approximated by our sum over jets instead of calo cells.
         if(abs(eta0).le.3.6d0) then
            pT_j   = sqrt(pjet(1,j)**2+pjet(2,j)**2)
            modp_j = sqrt(pT_j**2     +pjet(3,j)**2)
            ET_j   = pjet(4,j)*pT_j/modp_j
            mET(1) = mET(1) - pjet(1,j)*ET_j/pT_j
            mET(2) = mET(2) - pjet(2,j)*ET_j/pT_j
            tot_ET = tot_ET + ET_j
         endif
      enddo
      passed_mET=.false.
      if(sqrt(mET(1)**2+mET(2)**2).lt.sqrt(tot_ET)*
     $   min(3d0+0.0125*sqrt(pjet(1,1)**2+pjet(2,1)**2),6d0))
     $     passed_mET=.true.

      diag=80
C -   First just with MC generation cuts (id 80) [NOT in CDF obviously].
      do j=1,njets
         call pwhgfill(diag,sqrt(pjet(1,j)**2+pjet(2,j)**2),
     $                 dsig/binsize(diag))
      enddo
C -     |y_jet|<0.1 (id 81)
C - ( and missing ET cut, and nothing gets plotted below 50 GeV )
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         the_pt = sqrt(pjet(1,j)**2+pjet(2,j)**2)
         if(absy_jet.le.0.1d0.and.the_pt.ge.50d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,the_pt,dsig/binsize(diag))
      enddo
C - 0.1<|y_jet|<0.7 (id 82)
C - ( and missing ET cut, and nothing gets plotted below 50 GeV )
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         the_pt = sqrt(pjet(1,j)**2+pjet(2,j)**2)
         if(absy_jet.gt.0.1d0.and.absy_jet.le.0.7d0.and.the_pt.ge.50d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,the_pt,dsig/binsize(diag))
      enddo
C - 0.7<|y_jet|<1.1 (id 83)
C - ( and missing ET cut, and nothing gets plotted below 50 GeV )
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         the_pt = sqrt(pjet(1,j)**2+pjet(2,j)**2)
         if(absy_jet.gt.0.7d0.and.absy_jet.le.1.1d0.and.the_pt.ge.50d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,the_pt,dsig/binsize(diag))
      enddo
C - 1.1<|y_jet|<1.6 (id 84)
C - ( and missing ET cut, and nothing gets plotted below 50 GeV )
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         the_pt = sqrt(pjet(1,j)**2+pjet(2,j)**2)
         if(absy_jet.gt.1.1d0.and.absy_jet.le.1.6d0.and.the_pt.ge.50d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,the_pt,dsig/binsize(diag))
      enddo
C - 1.6<|y_jet|<2.1 (id 85)
C - ( and missing ET cut, and nothing gets plotted below 50 GeV )
      diag=diag+1
      do j=1,njets
         absy_jet = abs(getrapidity(pjet(4,j),pjet(3,j)))
         the_pt = sqrt(pjet(1,j)**2+pjet(2,j)**2)
         if(absy_jet.gt.1.6d0.and.absy_jet.le.2.1d0.and.the_pt.ge.50d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,the_pt,dsig/binsize(diag))
      enddo

C ------------------------------------------------------ C
C - Inclusive jet Y spectrum using same cone algorithm - C
C ------------------------------------------------------ C
      diag=90
C - |p_T|> 10 (id 91)
C - ( and missing ET cut )
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt. 10d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,absy_jet,dsig/binsize(diag))
      enddo
C - |p_T|> 20 (id 92)
C - ( and missing ET cut )
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt. 20d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,absy_jet,dsig/binsize(diag))
      enddo
C - |p_T|> 50 (id 93)
C - ( and missing ET cut )
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt. 50d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,absy_jet,dsig/binsize(diag))
      enddo
C - |p_T|>100 (id 94)
C - ( and missing ET cut )
      diag=diag+1
      do j=1,njets
         absy_jet = getrapidity(pjet(4,j),pjet(3,j))
         if(sqrt(pjet(1,j)**2+pjet(2,j)**2).gt.100d0
     $      .and.passed_mET)
     $        call pwhgfill(diag,absy_jet,dsig/binsize(diag))
      enddo

      diag=100
C - Pseudorapidity of the third hardest jet (id 101)
C - Cuts: |eta_1|<0.7, |eta_2|<0.7, |phi_1-phi_2|>2.79
C - E_T1 > 110 GeV, E_T3 > 10 GeV.
C - N.B. There must be another cut here somewhere, requiring
C - the 3rd jet to actually be IN the calorimeter! 
      diag=diag+1
      if(njets.ge.3) then
         call get_pseudorap(pjet(1,1),eta1)
         call get_pseudorap(pjet(1,2),eta2)
         call get_pseudorap(pjet(1,3),eta3)
         if(abs(eta1).le.0.7.and.abs(eta2).le.0.7) then
            phi1=atan2(pjet(2,1),pjet(1,1))
            phi2=atan2(pjet(2,2),pjet(1,2))
            dphi12=abs(phi1-phi2)
            dphi12=dphi12-2*pi*int(dphi12/(2*pi))
            if(dphi12.gt.pi) dphi12=2*pi-dphi12
	    if(dphi12>2.79) then
               et12 = pjet(4,1)*pjet(4,1)
     $              *(pjet(1,1)*pjet(1,1)
     $               +pjet(2,1)*pjet(2,1))
     $              /(pjet(1,1)*pjet(1,1)
     $               +pjet(2,1)*pjet(2,1)
     $               +pjet(3,1)*pjet(3,1))
               et22 = pjet(4,2)*pjet(4,2)
     $              *(pjet(1,2)*pjet(1,2)
     $               +pjet(2,2)*pjet(2,2))
     $              /(pjet(1,2)*pjet(1,2)
     $               +pjet(2,2)*pjet(2,2)
     $               +pjet(3,2)*pjet(3,2))
               et32 = pjet(4,3)*pjet(4,3)
     $              *(pjet(1,3)*pjet(1,3)
     $               +pjet(2,3)*pjet(2,3))
     $              /(pjet(1,3)*pjet(1,3)
     $               +pjet(2,3)*pjet(2,3)
     $               +pjet(3,3)*pjet(3,3))
               if(et12.GT.12100.and.
     $            et32.GT.100) then
                  call pwhgfill(diag,eta3,dsig/binsize(diag))
               endif
	    endif
	 endif
      endif

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

C ------------------------------------------------------- C
C - Store pT_rel's of highest and second highest pT jet - C
C ------------------------------------------------------- C
      if(njets.ge.1) pT_rel_J1=pT_rel(1)
      if(njets.ge.2) pT_rel_J2=pT_rel(2)

C ------------------------------------------------------------------- C
C - Computing arrays of useful quantities for 3 HIGHEST **PT** jets - C
C ------------------------------------------------------------------- C
      mjets=min(mjets,njets)
      do j=1,mjets
         kt(j)  = sqrt(pjet(1,j)**2+pjet(2,j)**2)
         pp     = sqrt(kt(j)**2+pjet(3,j)**2)
         eta(j) = 0.5d0*log((pp+pjet(3,j))/(pp-pjet(3,j)))
         rap(j) = getrapidity(pjet(4,j),pjet(3,j))
         phi(j) = atan2(pjet(2,j),pjet(1,j))
      enddo

C - Copying the momenta of the hardest pT jets
      do j=1,mjets
         do k=1,4
            pj(k,j) = pjet(k,j)
         enddo
      enddo

C ------------------------------------------------------- C
C - Work out array indices of mjets HIGHEST **ET** jets - C
C ------------------------------------------------------- C
C   Find permutation to put them in descending ET using CERNLIB:
      do j=1,maxjet
         et_idx(j)=0
         et_array(j)=0.0
      enddo
      do j=1,njets
         tmp1 = sqrt(pjet(1,j)**2+pjet(2,j)**2)
         et_array(j) = pjet(4,j)*tmp1/sqrt(tmp1**2+pjet(3,j)**2)
      enddo
      call sortzv(et_array,et_idx,njets,1,1,0)

C -------------------------------------------------------------------------- C
C - OK now calculate all the same quantities for the mjets highest ET jets - C
C -------------------------------------------------------------------------- C
      do j=1,mjets
         tmp_int   = et_idx(j)
         tmp1      = sqrt(pjet(1,tmp_int)**2+pjet(2,tmp_int)**2)
         pp        = sqrt(tmp1**2+pjet(3,tmp_int)**2)
         et_et(j)  = pjet(4,tmp_int) * ( tmp1 / pp )
         eta_et(j) = 0.5d0*log((pp+pjet(3,tmp_int))
     $                        /(pp-pjet(3,tmp_int)))
         rap_et(j) = getrapidity(pjet(4,tmp_int),pjet(3,tmp_int))
         phi_et(j) = atan2(pjet(2,tmp_int),pjet(1,tmp_int))
      enddo
      do j=1,mjets
         tmp_int   = et_idx(j)
         do k=1,4
            pj_et(k,j) = pjet(k,tmp_int)
         enddo
      enddo


      end


      subroutine analysis(dsig0)
      implicit none
      real * 8 dsig0,dsig
      include '../include/hepevt.h'
      include '../include/pwhg_math.h' 
      include  '../include/LesHouches.h'
      integer ihep
      logical ini
      data ini/.true./
      save ini
      integer diag
      real * 8 binsize(700)
      common/pwhghistcommon/binsize
      real * 8 ktjets(4),etajets(4),rapjets(4),phijets(4),pj(4,4)
      real * 8 et_et(4),eta_et(4),rap_et(4),phi_et(4),pj_et(4,4)
      real * 8 pT_rel_J1,pT_rel_J2,tmp1,tmp2
      real * 8 getrapidity,y12,phi12,dphi312,dr312,mjj,et1,et2
      integer njets
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer j,ixx
      real * 8 et,dphi,dR,absy_max
      real * 8 mET(2),eta0,Delta,Delta_bin
      logical  passed_mET

C -------------------------------- C
C - Pico to microbarn conversion - C
      dsig=dsig0/1d6
C -------------------------------- C

      if (ini) then
         write(*,*)     '*****************************'
         if(WHCPRG.eq.'NLO   ') then
            write (*,*) '        NLO analysis         '
         elseif(WHCPRG.eq.'HERWIG') then
            write (*,*) '       HERWIG ANALYSIS       '
         elseif(WHCPRG.eq.'PYTHIA') then
            write (*,*) '       PYTHIA ANALYSIS       '
         endif
         write(*,*)     '*****************************'
         ini=.false.
      endif

C ---------------------- C
C - Total E_T spectrum - C
C ---------------------- C

C - Only with MC generation cuts! (id 1)
      et=0
      do ihep=1,nhep
         if(isthep(ihep).eq.1) then
            et=et+sqrt(phep(1,ihep)**2+phep(2,ihep)**2)
         endif
      enddo
      diag=1
      call pwhgfill(diag,et,dsig/binsize(diag))

C - OK have the total E_T from all the particles in the event.
C - Everything we want to look at from now on involves only the
C - 1st, 2nd and 3rd hardest jets in the event:
      njets=3
      call buildjets(dsig,njets,ktjets,etajets,rapjets,phijets,pj,
     $                          et_et,eta_et,rap_et,phi_et,pj_et,
     $               pT_rel_J1,pT_rel_J2,1) 


C ------------------------------------------- C
C - Work out E_T of the two highest pT jets - C
C ------------------------------------------- C

      et1 = 0.
      if(njets.ge.1) et1 = et_et(1)
      et2 = 0.
      if(njets.ge.2) et2 = et_et(2)

C ---------------------- C
C - Sigma Delta Plots! - C
C ---------------------- C

C - Delta distribution where Delta is defined by ET1>Delta
C - Fill Delta distributions (diag=57,58 binsize=2.5,5.0, bins=100):
      if(njets.ge.1) then  ! (id 57)
         diag=57
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.Delta) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.1) then  ! (id 58)
         diag=58
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.Delta) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif

C - Delta distribution where Delta is defined by ET1 & ET2>Delta
C - Fill Delta distributions (diag=62,63 binsize=2.5,5.0, bins=100):
      if(njets.ge.1) then  ! (id 62)
         diag=62
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.Delta.and.et2.gt.Delta) 
     $           call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.1) then  ! (id 63)
         diag=63
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.Delta.and.et2.gt.Delta)
     $           call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif

C - Delta distribution where Delta is defined by ET1>ETcut+Delta, ET2>ETcut.
C - Fill Delta distributions (diag=59,64,67, binsize=0.2,0.4,1.0, bins=100):
      if(njets.ge.2.and.et2.ge. 20d0) then  ! (id 59)
         diag=59
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.( 20d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.2.and.et2.ge. 40d0) then  ! (id 64)
         diag=64
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.( 40d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.2.and.et2.ge.100d0) then  ! (id 67)
         diag=67
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.(100d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif

C - Delta distribution where Delta is defined by ET1>ETcut, ET2>ETcut+Delta.
C - Fill Delta distributions (diag=60,65,68, binsize=0.4,0.8,2.0, bins=100):
      if(njets.ge.2.and.et1.ge. 20d0) then  ! (id 60)
         diag=60
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)- 20d0
            Delta_bin = Delta + 0.005d0
            if(et2.gt.( 20d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.2.and.et1.ge. 40d0) then  ! (id 65)
         diag=65
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)- 40d0
            Delta_bin = Delta + 0.005d0
            if(et2.gt.( 40d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.2.and.et1.ge.100d0) then  ! (id 68)
         diag=68
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)-100d0
            Delta_bin = Delta + 0.005d0
            if(et2.gt.(100d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif

C - Delta distribution where Delta is defined by ET1>ETcut+Delta, ET2>ETcut.
C - Fill Delta distributions (diag=61,66,69, binsize=0.05,0.1,0.25, bins=100):
      if(njets.ge.2.and.et2.ge. 20d0) then  ! (id 61)
         diag=61
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.( 20d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.2.and.et2.ge. 40d0) then  ! (id 66)
         diag=66
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.( 40d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif
      if(njets.ge.2.and.et2.ge.100d0) then  ! (id 69)
         diag=69
         do ixx=1,100
            Delta     = binsize(diag)*(ixx-1)
            Delta_bin = Delta + 0.005d0
            if(et1.gt.(100d0+Delta)) call pwhgfill(diag,Delta_bin,dsig)
         enddo
      endif

C ---------------------- C
C - ET1 & ET2 > 20 GeV - C
C ---------------------- C

      diag=10
      if(njets.ge.2.and.et1.ge.20d0.and.et2.ge.20d0) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events (id 11)
         diag=diag+1
         call pwhgfill(diag,eta_et(1),dsig/binsize(diag))
         call pwhgfill(diag,eta_et(2),dsig/binsize(diag))
         
C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 12)
         diag=diag+1
         call pwhgfill(diag,abs(eta_et(1)-eta_et(2)),
     $                 dsig/binsize(diag))

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 13)
         dphi=abs(phi_et(1)-phi_et(2))
         dphi=dphi-2*pi*int(dphi/(2*pi))
         if(dphi.gt.pi) dphi=2*pi-dphi
         diag=diag+1
         call pwhgfill(diag,dphi,dsig/binsize(diag))

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 14)
         dR=sqrt((eta_et(1)-eta_et(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig/binsize(diag))

      endif

C ---------------------- C
C - ET1 & ET2 > 40 GeV - C
C ---------------------- C

      diag=15
      if(njets.ge.2.and.et1.ge.40d0.and.et2.ge.40d0) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events (id 16)
         diag=diag+1
         call pwhgfill(diag,eta_et(1),dsig/binsize(diag))
         call pwhgfill(diag,eta_et(2),dsig/binsize(diag))
         
C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 17)
         diag=diag+1
         call pwhgfill(diag,abs(eta_et(1)-eta_et(2)),
     $                 dsig/binsize(diag))

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 18)
         dphi=abs(phi_et(1)-phi_et(2))
         dphi=dphi-2*pi*int(dphi/(2*pi))
         if(dphi.gt.pi) dphi=2*pi-dphi
         diag=diag+1
         call pwhgfill(diag,dphi,dsig/binsize(diag))

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 19)
         dR=sqrt((eta_et(1)-eta_et(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig/binsize(diag))

      endif

C ----------------------- C
C - ET1 & ET2 > 100 GeV - C
C ----------------------- C

      diag=20
      if(njets.ge.2.and.et1.ge.100d0.and.et2.ge.100d0) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events (id 21)
         diag=diag+1
         call pwhgfill(diag,eta_et(1),dsig/binsize(diag))
         call pwhgfill(diag,eta_et(2),dsig/binsize(diag))
         
C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 22)
         diag=diag+1
         call pwhgfill(diag,abs(eta_et(1)-eta_et(2)),
     $                 dsig/binsize(diag))

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 23)
         dphi=abs(phi_et(1)-phi_et(2))
         dphi=dphi-2*pi*int(dphi/(2*pi))
         if(dphi.gt.pi) dphi=2*pi-dphi
         diag=diag+1
         call pwhgfill(diag,dphi,dsig/binsize(diag))

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 24)
         dR=sqrt((eta_et(1)-eta_et(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig/binsize(diag))

      endif

C ---------------- C
C - ET1 > 20 GeV - C
C ---------------- C

      diag=25
      if(njets.ge.2.and.et1.ge.20d0) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events (id 26)
         diag=diag+1
         call pwhgfill(diag,eta_et(1),dsig/binsize(diag))
         call pwhgfill(diag,eta_et(2),dsig/binsize(diag))
         
C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 27)
         diag=diag+1
         call pwhgfill(diag,abs(eta_et(1)-eta_et(2)),
     $                 dsig/binsize(diag))

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 28)
         dphi=abs(phi_et(1)-phi_et(2))
         dphi=dphi-2*pi*int(dphi/(2*pi))
         if(dphi.gt.pi) dphi=2*pi-dphi
         diag=diag+1
         call pwhgfill(diag,dphi,dsig/binsize(diag))

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 29)
         dR=sqrt((eta_et(1)-eta_et(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig/binsize(diag))

      endif

C ---------------- C
C - ET1 > 40 GeV - C
C ---------------- C

      diag=30
      if(njets.ge.2.and.et1.ge.40d0) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events (id 31)
         diag=diag+1
         call pwhgfill(diag,eta_et(1),dsig/binsize(diag))
         call pwhgfill(diag,eta_et(2),dsig/binsize(diag))
         
C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 32)
         diag=diag+1
         call pwhgfill(diag,abs(eta_et(1)-eta_et(2)),
     $                 dsig/binsize(diag))

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 33)
         dphi=abs(phi_et(1)-phi_et(2))
         dphi=dphi-2*pi*int(dphi/(2*pi))
         if(dphi.gt.pi) dphi=2*pi-dphi
         diag=diag+1
         call pwhgfill(diag,dphi,dsig/binsize(diag))

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 34)
         dR=sqrt((eta_et(1)-eta_et(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig/binsize(diag))

      endif

C ----------------- C
C - ET1 > 100 GeV - C
C ----------------- C

      diag=35
      if(njets.ge.2.and.et1.ge.100d0) then

C - Pseudorapidity of the 1st & 2nd jets jet in >= 2 jet events (id 36)
         diag=diag+1
         call pwhgfill(diag,eta_et(1),dsig/binsize(diag))
         call pwhgfill(diag,eta_et(2),dsig/binsize(diag))
         
C - abs(Delta Eta) between 1st and 2nd jet in >= 2 jet events (id 37)
         diag=diag+1
         call pwhgfill(diag,abs(eta_et(1)-eta_et(2)),
     $                 dsig/binsize(diag))

C - Delta Phi between 1st and 2nd jet in >= 2 jet events (id 38)
         dphi=abs(phi_et(1)-phi_et(2))
         dphi=dphi-2*pi*int(dphi/(2*pi))
         if(dphi.gt.pi) dphi=2*pi-dphi
         diag=diag+1
         call pwhgfill(diag,dphi,dsig/binsize(diag))

C - Delta R between 1st and 2nd jet in >= 2 jet events (id 39)
         dR=sqrt((eta_et(1)-eta_et(2))**2+dphi**2)
         diag=diag+1
         call pwhgfill(diag,dR,dsig/binsize(diag))

      endif

C ----------------------------------------------------- C
C - Now looking to 3rd hardest **pT** jet for cuts on - C
C - the two highest **pT** jets p0T11 & p0T21 >40 GeV - C
C ----------------------------------------------------- C

      diag=40
      if(njets.ge.3.and.ktjets(1).ge.40d0.and.ktjets(2).ge.40d0) then
C - p_T of the 3rd jet (id 41)
         diag=diag+1
         call pwhgfill(diag,ktjets(3),dsig/binsize(diag))
C - Pseudorapidity of the 3rd jet, p_T,3 > 10 (id 42)
         diag=diag+1
         if(ktjets(3).gt.10.0) 
     $        call pwhgfill(diag,etajets(3),dsig/binsize(diag))
C - Rapidity of the 3rd jet, p_T,3 > 10 (id 43)
         diag=diag+1
         if(ktjets(3).gt.10.0) 
     $        call pwhgfill(diag,rapjets(3),dsig/binsize(diag))
C - Pseudorapidity of the 3rd jet, p_T,3 > 100 (id 44)
         diag=diag+1
         if(ktjets(3).gt.100.0) 
     $        call pwhgfill(diag,etajets(3),dsig/binsize(diag))
C - Rapidity of the 3rd jet, p_T,3 > 100 (id 45)
         diag=diag+1
         if(ktjets(3).gt.100.0) 
     $        call pwhgfill(diag,rapjets(3),dsig/binsize(diag))

         y12     = getrapidity(pj(4,1)+pj(4,2),pj(3,1)+pj(3,2))
         tmp1    = pj(2,1)+pj(2,2)
         tmp2    = pj(1,1)+pj(1,2)
         phi12   = atan2(tmp1,tmp2)
         dphi312 = abs(phijets(3)-phi12)
         dphi312 = dphi312-2*pi*int(dphi312/(2*pi))
         if(dphi312.gt.pi) dphi312=2*pi-dphi312
         dr312   = sqrt((rapjets(3)-y12)**2+dphi312**2)

C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 10 (id 46)
         diag=diag+1
         if(ktjets(3).gt. 10.0) 
     $        call pwhgfill(diag,rapjets(3)-y12,dsig/binsize(diag))
C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 50 (id 47)
         diag=diag+1
         if(ktjets(3).gt. 50.0) 
     $        call pwhgfill(diag,rapjets(3)-y12,dsig/binsize(diag))
C - Rapidity gap between jets 1 & 2 and jet 3 p_T,3 > 100 (id 48)
         diag=diag+1
         if(ktjets(3).gt.100.0) 
     $        call pwhgfill(diag,rapjets(3)-y12,dsig/binsize(diag))

      endif

C ------------------------ C
C - Dijet invariant mass - C
C ------------------------ C
C - Compute the missing ET cut here (D0 arXiv:1002.4594v1)
      mET(1)=0d0
      mET(2)=0d0
      do j=1,nhep
         call get_pseudorap(phep(1,j),eta0)
         if(isthep(j).eq.1.and.abs(eta0).le.4.2) then
            mET(1)=mET(1)+phep(1,j)
            mET(2)=mET(2)+phep(2,j)
         endif
      enddo
      if(ktjets(1).ge.100d0) then
         if((sqrt(mET(1)**2+mET(2)**2)/ktjets(1)).lt.0.5) then
            passed_mET=.true.
         else
            passed_mET=.false.
         endif
      else
         if((sqrt(mET(1)**2+mET(2)**2)/ktjets(1)).lt.0.7) then
            passed_mET=.true.
         else
            passed_mET=.false.
         endif
      endif
      
      diag=50
      if(njets.ge.2) then
C - Computing the dijet invariant mass:
         if(njets.ge.2) then 
            mjj = (pj(4,1)+pj(4,2)-pj(3,1)-pj(3,2))
     $           *(pj(4,1)+pj(4,2)+pj(3,1)+pj(3,2))
     $           -(pj(1,1)+pj(1,2))**2
     $           -(pj(2,1)+pj(2,2))**2
            if(mjj.ge.0) then
               mjj =  sqrt(mjj)
            else
               mjj = -sqrt(-mjj)
            endif
            mjj = mjj / 1000d0 ! Binning is in TeV
         endif
         absy_max=max(abs(rapjets(1)),abs(rapjets(2)))
C - Both jets must have pT>40 GeV and we are not interested in mjj < 150 GeV
C - and the missing ET cut needs to be passed too.
         if(ktjets(1).ge.40.0.and.ktjets(2).ge.40.0.and.
     $      mjj.ge.0.15.and.passed_mET) then
C -     |y_max|<0.4 (id 51)
            diag=diag+1
            if(absy_max.le.0.4)
     $           call pwhgfill(diag,mjj,dsig/binsize(diag))
C - 0.4<|y_max|<0.8 (id 52)
            diag=diag+1
            if(absy_max.gt.0.4.and.absy_max.le.0.8)
     $           call pwhgfill(diag,mjj,dsig/binsize(diag))
C - 0.8<|y_max|<1.2 (id 53)
            diag=diag+1
            if(absy_max.gt.0.8.and.absy_max.le.1.2)
     $           call pwhgfill(diag,mjj,dsig/binsize(diag))
C - 1.2<|y_max|<1.6 (id 54)
            diag=diag+1
            if(absy_max.gt.1.2.and.absy_max.le.1.6)
     $           call pwhgfill(diag,mjj,dsig/binsize(diag))
C - 1.6<|y_max|<2.0 (id 55)
            diag=diag+1
            if(absy_max.gt.1.6.and.absy_max.le.2.0)
     $           call pwhgfill(diag,mjj,dsig/binsize(diag))
C - 2.0<|y_max|<2.4 (id 56)
            diag=diag+1
            if(absy_max.gt.2.0.and.absy_max.le.2.4)
     $           call pwhgfill(diag,mjj,dsig/binsize(diag))
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
         dphi=dphi-2*pi*int(dphi/(2*pi))
         if(dphi.gt.pi) dphi=2*pi-dphi
C - ... and 75<pT,J1<100 GeV (id 71)
         diag=diag+1
         if(ktjets(1).gt. 75d0.and.ktjets(1).le.100d0)
     $        call pwhgfill(diag,dphi,dsig/binsize(diag))
C - ... and 100<pT,J1<130 GeV (id 72)
         diag=diag+1
         if(ktjets(1).gt.100d0.and.ktjets(1).le.130d0)
     $        call pwhgfill(diag,dphi,dsig/binsize(diag))
C - ... and 130<pT,J1<180 GeV (id 73)
         diag=diag+1
         if(ktjets(1).gt.130d0.and.ktjets(1).le.180d0)
     $        call pwhgfill(diag,dphi,dsig/binsize(diag))
C - ... and     pT,J1>180 GeV (id 74)
         diag=diag+1
         if(ktjets(1).gt.180d0)
     $        call pwhgfill(diag,dphi,dsig/binsize(diag))
      endif


C --------------------------------------------- C
C - p_T^rel of jets 1 & 2, ET1 & ET2 > 40 GeV - C
C --------------------------------------------- C
      diag=95
C - p_T^rel of the hardest jet (id 96)
      diag=diag+1
      if(njets.ge.2.and.et1.ge.40d0.and.et2.ge.40d0) 
     $     call pwhgfill(diag,pT_rel_J1,dsig/binsize(diag))
C - p_T^rel inclusive in jets 1 and 2 in >= 2 jet events (id 97)
      diag=diag+1
      if(njets.ge.2.and.et1.ge.40d0.and.et2.ge.40d0) then
         call pwhgfill(diag,pT_rel_J1,dsig/binsize(diag))
         call pwhgfill(diag,pT_rel_J2,dsig/binsize(diag))
      endif

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
      real*8 p(4),eta,pt,th
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
