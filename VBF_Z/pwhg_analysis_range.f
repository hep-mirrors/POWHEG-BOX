c  The next subroutines opens some histograms and prepares them 
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
      integer max_diag
      parameter (max_diag=200)
      real * 8 binsize(max_diag)
      common/pwhghistcommon/binsize
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
            
      call pwhginihist

c ========================

c diagnostic histograms:
      
c     qsq_1
      diag = 34
      binsize(diag) = 2d0
      call pwhgbookup(diag,'qsq_1- NO CUTS','LIN',
     .     binsize(diag),0d0,100d0)
c
c     qsq_2
      diag = 35
      binsize(diag) = 5d0
      call pwhgbookup(diag,'qsq_2- NO CUTS','LIN',
     .     binsize(diag),0d0,200d0)

c     qsq_1
      diag = 36
      binsize(diag) = 2d0
      call pwhgbookup(diag,'qsq_1- VBF CUTS','LIN',
     .     binsize(diag),0d0,100d0)

c     qsq_2
      diag = 37
      binsize(diag) = 5d0
      call pwhgbookup(diag,'qsq_2- VBF CUTS','LIN',
     .     binsize(diag),0d0,200d0)

c     qsq_1
      diag = 38
      binsize(diag) = 2d0
      call pwhgbookup(diag,'qsq_1- CJV CUTS','LIN',
     .     binsize(diag),0d0,100d0)

c     qsq_2
      diag = 39
      binsize(diag) = 5d0
      call pwhgbookup(diag,'qsq_2- CJV CUTS','LIN',
     .     binsize(diag),0d0,200d0)
      
c==============
c 
c plot number of jets:

      diag = 10
      binsize(diag) = 1d0
      call pwhgbookup(diag,'number of jets - NO CUTS','LIN',
     .     binsize(diag),-0.5d0,15.5d0)
     
      diag = 11
      binsize(diag) = 1d0
      call pwhgbookup(diag,'number of jets - Jet PT CUTS','LIN',
     .     binsize(diag),-0.5d0,15.5d0)
      
      diag = 12
      binsize(diag) = 1d0
      call pwhgbookup(diag,'number of jets - INC. CUTS','LIN',
     .     binsize(diag),-0.5d0,15.5d0)

      diag = 13
      binsize(diag) = 1d0
      call pwhgbookup(diag,'number of jets - VBF CUTS','LIN',
     .     binsize(diag),-0.5d0,15.5d0)

      diag = 14
      binsize(diag) = 1d0
      call pwhgbookup(diag,'number of jets - CJV CUTS','LIN',
     .     binsize(diag),-0.5d0,15.5d0)

c==============

      diag = 170
      binsize(diag) = 1d0
      call pwhgbookup(diag,'sig(tot)','LIN',binsize(diag),0d0,1d0)

      diag = 1
      binsize(diag) = 1d0
      call pwhgbookup(diag,'sig(no cuts, all mll)',
     & 'LIN',binsize(diag),0d0,1d0)

C     -- HISTOGRAMS WITHOUT LEPTON CUTS 
            
c==============

c     total cross section sanity check
      diag = 43
      binsize(diag) = 10d0
      call pwhgbookup(diag,'3 jet incl.- NO CUTS, all mll','LIN',
     .     binsize(diag),5d0,75d0)

c     total cross section sanity check
      diag = 44
      binsize(diag) = 10d0
      call pwhgbookup(diag,'2 jet incl.- NO CUTS, all mll','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 45
      binsize(diag) = 10d0
      call pwhgbookup(diag,'2 jet excl.- NO CUTS, all mll','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 46
      binsize(diag) = 40d0
      call pwhgbookup(diag,'PT jet 1- NO CUTS','LOG',
     .     binsize(diag),0d0,880d0)

      diag = 47
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT jet 2- NO CUTS','LOG',
     .     binsize(diag),0d0,520d0)

      diag = 48
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y jet 1- NO CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 51
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y jet 2- NO CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 54
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y_j1j2- NO CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 55
      binsize(diag) = 200d0
      call pwhgbookup(diag,'HT,TOT- NO CUTS','LOG',
     .     binsize(diag),0d0,3000d0)

      diag = 56
      binsize(diag) = 200d0
      call pwhgbookup(diag,'HT,TOT 2j- NO CUTS','LOG',
     .     binsize(diag),0d0,3000d0)

      diag = 57
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT LEPT- NO CUTS','LOG',
     .     binsize(diag),0d0,440d0)


      diag = 59
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Eta_lept- NO CUTS','LIN',
     .     binsize(diag),-4d0,4d0)


      diag = 60
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Eta_l1l2- NO CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 61
      binsize(diag) = 60d0
      call pwhgbookup(diag,'M(l1l2)- NO CUTS','LOG',
     .     binsize(diag),0d0,960d0)

      diag = 63
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j1lep)- NO CUTS','LIN',
     .     binsize(diag),0d0,6d0)

      diag = 64
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j2lept)- NO CUTS','LIN',
     .     binsize(diag),0d0,6d0)

      diag = 65
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Phi(l1l2)- NO CUTS','LIN',
     .     binsize(diag),0d0,3.2d0)

      diag = 66
c      binsize(diag) = 20d0
c      binsize(diag) = 5d0
      binsize(diag) = 1d0
      call pwhgbookup(diag,'Pt J3- NO CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 67
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y J3- NO CUTS, kt3>pt_jet_min','LOG',
     .     binsize(diag),-5d0,5d0)

      diag = 175
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y J3- NO CUTS, kt3>pt_cut','LOG',
     .     binsize(diag),-5d0,5d0)

c      diag = 68
c      binsize(diag) = 5d0
c      call pwhgbookup(diag,'Y J3 asym- NO CUTS','LOG',
c     .     binsize(diag),0d0,5d0)

      diag = 69
c      binsize(diag) = 10d0
      binsize(diag) = 5d0
      call pwhgbookup(diag,'Ptrel J1- NO CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 70
c      binsize(diag) = 10d0
      binsize(diag) = 5d0
      call pwhgbookup(diag,'Ptrel J2- NO CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 71
      binsize(diag) = 200d0
      call pwhgbookup(diag,'M_j1j2- NO CUTS','LIN',
     .     binsize(diag),0d0,3000d0)

      diag = 72
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Phi(j1j2)- NO CUTS','LIN',
     .     binsize(diag),0d0,3.2d0)
   
      diag = 77
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'y*- NO CUTS, kt3>pt_jet_min','LIN',
     .     binsize(diag),0d0,4d0)
   
      diag = 78
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'|y*|- NO CUTS, kt3>pt_jet_min','LIN',
     .     binsize(diag),0d0,2d0)
   
      diag = 176
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'y*- NO CUTS, kt3>pt_cut','LIN',
     .     binsize(diag),0d0,4d0)
   
      diag = 177
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'|y*|- NO CUTS, kt3>pt_cut','LIN',
     .     binsize(diag),0d0,2d0)
   
      diag = 79
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'(y1+y2)/2- NO CUTS','LIN',
     .     binsize(diag),0d0,4d0)

      diag = 80
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j1j2)- NO CUTS','LIN',
     .     binsize(diag),0d0,6d0)

C -----------------------------------------------------
C     -- SAME HISTOGRAMS WITH VBF CUTS 

      diag = 83
      binsize(diag) = 1d0
      call pwhgbookup(diag,'sig(all VBF cuts)','LIN',
     &                binsize(diag),0d0,1d0)
      
c     total cross section sanity check
      diag = 82
      binsize(diag) = 10d0
      call pwhgbookup(diag,'3 jet incl.- VBF CUTS','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 84
      binsize(diag) = 10d0
      call pwhgbookup(diag,'2 jet incl.- VBF CUTS','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 85
      binsize(diag) = 10d0
      call pwhgbookup(diag,'2 jet excl.- VBF CUTS','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 86
      binsize(diag) = 40d0
      call pwhgbookup(diag,'PT jet 1- VBF CUTS','LOG',
     .     binsize(diag),0d0,880d0)

      diag = 87
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT jet 2- VBF CUTS','LOG',
     .     binsize(diag),0d0,520d0)

      diag = 88
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y jet 1- VBF CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 91
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y jet 2- VBF CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 94
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y j1j2(tag)- VBF CUTS','LIN',
     .     binsize(diag),-6d0,6d0)

      diag = 95
      binsize(diag) = 200d0
      call pwhgbookup(diag,'HT,TOT- VBF CUTS','LOG',
     .     binsize(diag),0d0,3000d0)

      diag = 96
      binsize(diag) = 200d0
      call pwhgbookup(diag,'HT,TOT 2j- VBF CUTS','LOG',
     .     binsize(diag),0d0,3000d0)

      diag = 97
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT LEPT- VBF CUTS','LOG',
     .     binsize(diag),0d0,440d0)

      diag = 99
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Eta_lept- VBF CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 100
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Eta_l1l2- VBF CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 101
      binsize(diag) = 60d0
      call pwhgbookup(diag,'M(l1l2)- VBF CUTS','LOG',
     .     binsize(diag),0d0,960d0)

      diag = 103
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j1lep)- VBF CUTS','LIN',
     .     binsize(diag),0d0,6d0)

      diag = 104
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j2lept)- VBF CUTS','LIN',
     .     binsize(diag),0d0,6d0)

      diag = 105
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Phi(l1l2)- VBF CUTS','LIN',
     .     binsize(diag),0d0,3.2d0)

      diag = 106
c      binsize(diag) = 20d0
c      binsize(diag) = 5d0
      binsize(diag) = 1d0
      call pwhgbookup(diag,'Pt J3- VBF CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 107
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y J3- VBF CUTS ,kt3>pt_jet_min','LOG',
     .     binsize(diag),-5d0,5d0)

      diag = 178
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y J3- VBF CUTS, kt3>pt_cut','LOG',
     .     binsize(diag),-5d0,5d0)

      diag = 109
      binsize(diag) = 10d0
      call pwhgbookup(diag,'Ptrel J1- VBF CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 110
      binsize(diag) = 10d0
      call pwhgbookup(diag,'Ptrel J2- VBF CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 111
      binsize(diag) = 200d0
      call pwhgbookup(diag,'M_j1j2(tag) - VBF CUTS','LIN',
     .     binsize(diag),0d0,3000d0)

      diag = 112
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Phi(j1j2)- VBF CUTS','LIN',
     .     binsize(diag),0d0,3.2d0)

      diag = 117
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'y*- VBF CUTS, kt3>pt_jet_min','LIN',
     .     binsize(diag),0d0,4d0)
   
      diag = 118
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'|y*|- VBF CUTS, kt3>pt_jet_min','LIN',
     .     binsize(diag),0d0,2d0)
   
      diag = 179
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'y*- VBF CUTS, kt3>pt_cut','LIN',
     .     binsize(diag),0d0,4d0)
   
      diag = 180
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'|y*|- VBF CUTS, kt3>pt_cut','LIN',
     .     binsize(diag),0d0,2d0)
   
      diag = 119
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'(y1+y2)/2- VBF CUTS','LIN',
     .     binsize(diag),0d0,4d0)

      diag = 120
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j1j2)- VBF CUTS','LIN',
     .     binsize(diag),0d0,6d0)  
c
c
C -----------------------------------------------------
C     -- SAME HISTOGRAMS WITH VBF CUTS and CJV 

      diag = 123
      binsize(diag) = 1d0
      call pwhgbookup(diag,'sig(CJV cuts)','LIN',
     &                binsize(diag),0d0,1d0)
      
c     total cross section sanity check
      diag = 122
      binsize(diag) = 10d0
      call pwhgbookup(diag,'3 jet incl.- CJV CUT','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 124
      binsize(diag) = 10d0
      call pwhgbookup(diag,'2 jet incl.- CJV CUTS','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 125
      binsize(diag) = 10d0
      call pwhgbookup(diag,'2 jet excl.- CJV CUTS','LIN',
     .     binsize(diag),5d0,75d0)

      diag = 126
      binsize(diag) = 40d0
      call pwhgbookup(diag,'PT jet 1- CJV CUTS','LOG',
     .     binsize(diag),0d0,880d0)

      diag = 127
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT jet 2- CJV CUTS','LOG',
     .     binsize(diag),0d0,520d0)

      diag = 128
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y jet 1- CJV CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 131
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y jet 2- CJV CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 134
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y j1j2(tag)- CJV CUTS','LIN',
     .     binsize(diag),-6d0,6d0)

      diag = 135
      binsize(diag) = 200d0
      call pwhgbookup(diag,'HT,TOT- CJV CUTS','LOG',
     .     binsize(diag),0d0,3000d0)

      diag = 136
      binsize(diag) = 200d0
      call pwhgbookup(diag,'HT,TOT 2j- CJV CUTS','LOG',
     .     binsize(diag),0d0,3000d0)

      diag = 137
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT LEPT- CJV CUTS','LOG',
     .     binsize(diag),0d0,440d0)

      diag = 139
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Eta_lept- CJV CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 140
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Eta_l1l2- CJV CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 141
      binsize(diag) = 60d0
      call pwhgbookup(diag,'M(l1l2)- CJV CUTS','LOG',
     .     binsize(diag),0d0,960d0)


      diag = 143
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j1lep)- CJV CUTS','LIN',
     .     binsize(diag),0d0,6d0)

      diag = 144
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j2lept)- CJV CUTS','LIN',
     .     binsize(diag),0d0,6d0)

      diag = 145
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Phi(l1l2)- CJV CUTS','LIN',
     .     binsize(diag),0d0,3.2d0)

      diag = 146
c      binsize(diag) = 20d0
c      binsize(diag) = 5d0
      binsize(diag) = 1d0
      call pwhgbookup(diag,'Pt J3- CJV CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 147
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y J3- CJV CUTS, kt3>pt_jet_min','LOG',
     .     binsize(diag),-5d0,5d0)

      diag = 181
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y J3- CJV CUTS,kt>pt_cut','LOG',
     .     binsize(diag),-5d0,5d0)

      diag = 149
      binsize(diag) = 10d0
      call pwhgbookup(diag,'Ptrel J1- CJV CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 150
      binsize(diag) = 10d0
      call pwhgbookup(diag,'Ptrel J2- CJV CUTS','LOG',
     .     binsize(diag),0d0,200d0)

      diag = 151
      binsize(diag) = 200d0
      call pwhgbookup(diag,'M_j1j2(tag) - CJV CUTS','LIN',
     .     binsize(diag),0d0,3000d0)

      diag = 152
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'Phi(j1j2)- CJV CUTS','LIN',
     .     binsize(diag),0d0,3.2d0)

      diag = 157
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'y*- CJV CUTS, kt3>pt_jet_min','LIN',
     .     binsize(diag),0d0,4d0)
   
      diag = 158
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'|y*|- CJV CUTS, kt3>pt_jet_min','LIN',
     .     binsize(diag),0d0,2d0)

      diag = 182
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'y*- CJV CUTS, kt3>pt_cut','LIN',
     .     binsize(diag),0d0,4d0)
   
      diag = 183
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'|y*|- CJV CUTS, kt3>pt_cut','LIN',
     .     binsize(diag),0d0,2d0)
   
      diag = 159
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'(y1+y2)/2- CJV CUTS','LIN',
     .     binsize(diag),0d0,4d0)

      diag = 160
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'DelR(j1j2)- CJV CUTS','LIN',
     .     binsize(diag),0d0,6d0)  


      diag = 165
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT veto-jet1- VBF CUTS','LOG',
     .     binsize(diag),0d0,440d0)

      diag = 166
      binsize(diag) = 20d0
      call pwhgbookup(diag,'PT veto-jet2- VBF CUTS','LOG',
     .     binsize(diag),0d0,440d0)

      diag = 167
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y veto-jet1- VBF CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 168
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'Y veto-jet2- VBF CUTS','LIN',
     .     binsize(diag),-4d0,4d0)

      diag = 169
      binsize(diag) = 1d0
      call pwhgbookup(diag,'number of veto jets - VBF CUTS','LIN',
     .     binsize(diag),-0.5d0,9.5d0)

ccccccc
c
      end 
      
      subroutine analysis(dsig0)
      implicit none
      real * 8 dsig0,dsig
      include '../include/hepevt.h'
      include '../include/pwhg_math.h' 
      include 'nlegborn.h'
c test m34 only:
      include '../include/pwhg_kn.h' 
      include  '../include/LesHouches.h'
      include 'PhysPars.h'
      include 'cvecbos.h'
      logical ini
      data ini/.true./
      save ini
      integer diag
      integer max_diag
      parameter (max_diag=200)
      real * 8 binsize(max_diag)
      common/pwhghistcommon/binsize
c     we need to tell to this analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer nleptons
      integer ileptons(200)
      integer   maxjet
      parameter (maxjet=2048)
      real * 8  kt(maxjet),eta(maxjet),rap(maxjet),
     1    phi(maxjet),pj(4,maxjet),ptrel(maxjet)
      real * 8 ptel1,ptel2,etael1,etael2,httot
      real * 8 pj12(4),y12, pel12(4),etael12
      real*8 ptl(100),etal(100)
      real * 8  mll, rj1l, rj2l, phill
      real * 8 Etll,invmass 
      real * 8 mjj,phijj
c      real * 8 rap_max,rap_min
      real * 8 r,fphi
      integer ihep,j,mjets
      real * 8 etafromp,ptfromp 
      logical passcuts_vbf
     
c cut parameters:
      real*8 ptmin,pt_cut
      real*8 etal_max,ptl_min
      real*8 pt_tag_min,pt_jet_min,yj_max
      real*8 yjj_min,Rjj_min,mjj_min
      real*8 Rjl_min,Rll_min
      logical rap_gap,rap_sign

      common /jetcuts/ptmin,yj_max,Rjj_min

      logical with_vbf_cuts
      parameter (with_vbf_cuts = .true.)

      real*8 Rjl_tmp0,Rjl_tmp1
      integer ij,il
      real*8 Rll_tmp
      real*8 Rjj_tmp,Rjj_tmp0

      integer itag1,itag2,itag3
      real*8 pt_1,pt_2,pt_3
      integer ltag1,ltag2
      real*8 ptl_1,ptl_2

      real*8 pl_sum(1:4)
      real*8 m34,p34(0:3)
      real*8 ystar, ystar_abs,rap_av,Rj12

      integer iveto1,iveto2,nveto
      real*8 pt_v1,pt_v2
      real*8 pt_veto
      logical rap_veto
      logical passcuts_cjv
c
c qsq diagnostics:     
      integer njet
      real*8 q12(0:4),q34(0:4)
      integer mu
      include '../include/pwhg_flg.h'

c mll window: 
      real*8 massmin,massmax,Vmass
      logical mll_window
      real*8 window_width
      real * 8 powheginput
      external powheginput
c
c =======================================================

c initialize all cuts:
      etal_max   = 1d10 
      ptl_min    = 0d0   
   
      pt_tag_min = 0d0
      pt_jet_min = 0d0
      pt_cut = 0d0
      yj_max  = 1d10
      
      yjj_min = 0d0
      rap_sign = .false.
      rap_gap  = .false.

      Rjj_min = 0d0
      mjj_min = 0d0

      Rjl_min = 0d0
      Rll_min = 0d0
c
c decide about mll mass window:
      mll_window = .true.
      window_width = 10d0
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
c VBF cuts (can be modified by user):
C
        etal_max   = 2.5d0
        ptl_min    = 20d0

        pt_tag_min = 20d0  !tag jets
        pt_jet_min = 20d0  !non-tag extra jets
        ptmin = 1d0        !parameter for jet algorithm
        pt_cut = 10d0       !parameter for y3 distributions
        yj_max  = 4.5d0

        yjj_min  = 4d0
        rap_sign = .true.
        rap_gap  = .true.

        Rjj_min = 0.4d0
        mjj_min = 600d0

        Rjl_min = 0.4d0
        Rll_min = 0.1d0
C
        passcuts_vbf = .true. 

C CJV cuts:
        pt_veto = 20d0
        rap_veto = .true.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c      
c from pico to femto      
      dsig=dsig0*1000
      diag = 0 
      if (ini) then
         write (*,*)
         write (*,*) '********************************************'
         if(WHCPRG.eq.'NLO   ') then
            write (*,*) 'Fixed order ANALYSIS CALLED '
         
          if( (abs(vdecaymode).eq.11).or. 
     $        (abs(vdecaymode).eq.13) ) then
              continue
           else
             write(*,*) '**************************************'
             write(*,*) ' template analysis works only for Z  '
             write(*,*) ' bosons decaying to electrons or muons'
             write(*,*) '                 STOP                 '
             write(*,*) '**************************************'
             call exit(1)
           endif
      
         elseif(WHCPRG.eq.'HERWIG') then
            ph_Zmass   = powheginput('zmass') 
            write (*,*) '           HERWIG ANALYSIS CALLED     '
         elseif(WHCPRG.eq.'PYTHIA') then
            ph_Zmass   = powheginput('zmass') 
            write (*,*) '           PYTHIA ANALYSIS CALLED     '
         else
            ph_Zmass   = powheginput('zmass') 
            write (*,*) '           ANALYSIS CALLED     '
         endif  
c
         write (*,*) '********************************************'
         write(*,*) '********************************************'
         write(*,*) '                ANALYSIS CUTS                '
         write(*,*) '*********************************************'
         write(*,*) '*********************************************'
            write(*,*) ''
            write(*,*) 'hard-wired mll > 30 GeV'
            write(*,*) 'mZ =  ',ph_Zmass         
            if (mll_window) then
              write(*,*) 'mll =  mZ+-',window_width
              write(*,*) '(mll cut not applied to incl. test plots)'
            endif               
            write(*,*) 'jet cuts:'
            write(*,*) 'jet ptmin = ',ptmin
            write(*,*) 'jet3 pt_cut = ',pt_cut
            write(*,*) 'tag jets pt_tag_min = ',pt_tag_min
            write(*,*) 'non-tag jets pt_jet_min = ',pt_jet_min
            write(*,*) 'yj_max  = ',yj_max
            write(*,*) 'yjj_min = ',yjj_min
            write(*,*) 'Rjj_min = ',Rjj_min
            write(*,*) 'mjj_min = ',mjj_min
            write(*,*) ''
            write(*,*) 'lepton cuts:'
            write(*,*) 'ptl_min    = ',ptl_min 
            write(*,*) 'etal_max   = ',etal_max 
            write(*,*) 'Rll_min    = ',Rll_min
            write(*,*) ''
            write(*,*) 'extra cuts:'
            write(*,*) 'Rjl_min  = ',Rjl_min
            write(*,*) 'rap_gap  = ',rap_gap
            write(*,*) 'rap_sign  = ',rap_sign
            write(*,*) ''
            write(*,*) 'extra CJV settings'
            write(*,*) 'pt_veto = ',pt_veto
            write(*,*) 'rap_veto = ',rap_veto
            write(*,*) ''
         write(*,*) '********************************************'
         write(*,*) '********************************************'        
         ini=.false.
      endif
         
c find electrons and muons
      nleptons=0
      do ihep=1,nhep
         if(isthep(ihep).eq.1) then
            if((abs(idhep(ihep)).eq.11).or.
     %         (abs(idhep(ihep)).eq.13)) then
               nleptons=nleptons+1
               ileptons(nleptons)=ihep
            endif
         endif
      enddo
      if(nleptons.gt.200) then
         write(*,*) ' crazy event, too many leptons'
         return
      endif
      if(nleptons.lt.2) then
         write(*,*) ' crazy event, missing leptons'
         return
      endif
c sort by pt
      call sortbypt(nleptons,ileptons)

c initialize:
	do j = 1,maxjet
	   kt(j) = 0d0
	enddo   

c mll-cut:
      pel12 = phep(:4,ileptons(1))+phep(:4,ileptons(2))
      mll = invmass(pel12)
      if ((mll.lt.30d0)) return

c total xsec:
      diag = 170
      call pwhgfill(diag,0.5d0,dsig)

      mjets=10
      call buildjets(mjets,kt,eta,rap,phi,pj,ptrel)

c number of jets before cuts:
      njet = 0
      do j = 1,maxjet
         if (kt(j).gt.0d0) njet = njet+1
      enddo 
      diag = 10
      call pwhgfill(diag,dble(njet),dsig/binsize(diag))

      if (mjets.lt.2) return

      if (kt(1).lt.pt_jet_min) return
      if (kt(2).lt.pt_jet_min) return

c number of jets with pt cuts for jet1,2:
      diag = 11
      call pwhgfill(diag,dble(njet),dsig/binsize(diag))

c cross section:
      diag = 1
      call pwhgfill(diag,0.5d0,dsig)

c************************************

      etael1=etafromp(phep(1,ileptons(1)))
      etael2=etafromp(phep(1,ileptons(2)))

      ptel1=ptfromp(phep(1,ileptons(1)))
      ptel2=ptfromp(phep(1,ileptons(2)))

      pel12 = phep(:4,ileptons(1))+phep(:4,ileptons(2))

c============================================
c
c diagnostic histograms (qsq_i) for parton level calculation:

c default is Born kinematics:
         do mu = 0,3
          q12(mu) = kn_pborn(mu,1)-kn_pborn(mu,5)
          q34(mu) = kn_pborn(mu,2)-kn_pborn(mu,6)
         enddo
         q12(4) = abs(q12(0)**2-q12(1)**2-q12(2)**2-q12(3)**2)
         q34(4) = abs(q34(0)**2-q34(1)**2-q34(2)**2-q34(3)**2)

         if(flg_btildepart.eq.'c') then 
         do mu = 0,3
          q12(mu) = kn_pborn(mu,1)-kn_pborn(mu,5)
          q34(mu) = kn_pborn(mu,2)-kn_pborn(mu,6)
         enddo
         q12(4) = abs(q12(0)**2-q12(1)**2-q12(2)**2-q12(3)**2)
         q34(4) = abs(q34(0)**2-q34(1)**2-q34(2)**2-q34(3)**2)
         endif

         if(flg_btildepart.eq.'r') then
         do mu = 0,3
            q12(mu) = kn_preal(mu,1)-kn_preal(mu,5)-kn_preal(1,7)
            q34(mu) = kn_preal(mu,2)-kn_preal(mu,6)-kn_preal(1,7)
         enddo
         q12(4) = abs(q12(0)**2-q12(1)**2-q12(2)**2-q12(3)**2)
         q34(4) = abs(q34(0)**2-q34(1)**2-q34(2)**2-q34(3)**2)
         endif 

c qsq_i with "no cuts":
c    qsq_1
      diag=34
      call pwhgfill(diag,dsqrt(q12(4)),dsig/binsize(diag))
c    qsq_2
      diag=35
      call pwhgfill(diag,dsqrt(q34(4)),dsig/binsize(diag))
c
c ======================================
c
C     histograms WITHOUT lepton cuts 
c
c     three jet inclusive.
      diag=43
      do j=1,3
         if(mjets.lt.j) then
            kt(j)=0
         endif   
      enddo
      do j=10,70,10
         if(kt(1).gt.j.and.kt(2).gt.j.and.kt(3).gt.j) then
            call pwhgfill(diag,dble(j),dsig)
         endif
      enddo

c     two jet inclusive and two jet exclusive.
c     It is exclusive if the third jet is below the cut
c     inclusive otherwise
      diag=44
      do j=1,3
         if(mjets.lt.j) kt(j)=0
      enddo
      do j=10,70,10
         if(kt(1).gt.j.and.kt(2).gt.j) then
c inclusive
            call pwhgfill(diag,dble(j),dsig)
            if(kt(3).lt.j) then
c exclusive
               call pwhgfill(diag+1,dble(j),dsig)
            endif
         endif
      enddo
      diag=45

c mll window:
      mll = invmass(pel12)
      if (mll_window) then
         Vmass = ph_Zmass
         massmin = Vmass-window_width
         massmax = Vmass+window_width
         if ((mll.lt.massmin).or.(mll.gt.massmax)) then
             return
         endif  
      endif   

c number of jets with inc. cuts:
      diag = 12
      call pwhgfill(diag,dble(njet),dsig/binsize(diag))


c     PT of jet 1
      diag=46
      call pwhgfill(diag,kt(1),dsig/binsize(diag))
c     PT of jet 2
      diag=47
      call pwhgfill(diag,kt(2),dsig/binsize(diag))
c     Y of jet 1
      diag=48
      call pwhgfill(diag,rap(1),dsig/binsize(diag))
c     Y of jet 2
      diag=51
      call pwhgfill(diag,rap(2),dsig/binsize(diag))
c     Y jet1 - Y jet 2
      diag=54
      pj12 = pj(:4,1)+pj(:4,2)
      y12 = rap(1)-rap(2)
      call pwhgfill(diag,y12,dsig/binsize(diag))

c     H_T tot
      httot=ptel1+ptel2
      do j=1,mjets
         httot=httot+kt(j)
      enddo
      diag=55
      call pwhgfill(diag,httot,dsig/binsize(diag))

c     H_T tot, 2j
      httot=ptel1+ptel2
      httot=httot+kt(1)+kt(2)
      diag=56
      call pwhgfill(diag,httot,dsig/binsize(diag))

C     Pt lept     
      diag=57
      call pwhgfill(diag,ptel1,dsig/binsize(diag)/2d0)
      call pwhgfill(diag,ptel2,dsig/binsize(diag)/2d0)

C     eta lept
      diag=59
      call pwhgfill(diag,etael1,dsig/binsize(diag)/2d0)
      call pwhgfill(diag,etael2,dsig/binsize(diag)/2d0)

C     eta(l1)- eta(l2) 
      diag=60
      etael12 = etael1-etael2
      call pwhgfill(diag,etael12,dsig/binsize(diag))

C     M(l1l2) 
      diag=61
      mll = invmass(pel12)
      call pwhgfill(diag,mll,dsig/binsize(diag))

C     Delta R(j1 lept) 
      diag=63
      if (mjets .ge. 2) then 
        Rj1l = r(pj(1:4,1),phep(1:4,ileptons(1)))
        call pwhgfill(diag,Rj1l,dsig/binsize(diag)/2d0)
        Rj1l = r(pj(1:4,1),phep(1:4,ileptons(2)))
        call pwhgfill(diag,Rj1l,dsig/binsize(diag)/2d0)
      endif

C     Delta R(j2 lept) 
      diag=64
      if (mjets .ge. 2) then 
        Rj2l = r(pj(1:4,2),phep(1:4,ileptons(1)))
        call pwhgfill(diag,Rj2l,dsig/binsize(diag)/2d0)
        Rj2l = r(pj(1:4,2),phep(1:4,ileptons(2)))
        call pwhgfill(diag,Rj2l,dsig/binsize(diag)/2d0)
      endif

c     Phi(l1l2) 
      diag=65 
      phill=fphi(phep(1,ileptons(1)),phep(1,ileptons(2)))
      call pwhgfill(diag,phill,dsig/binsize(diag))

c     pt of third jet
      diag=66
      if(mjets.ge.3) call pwhgfill(diag,kt(3),dsig/binsize(diag))

c     y of third jet with standard pt_jet cut
      diag=67
      if(mjets.ge.3 .and. kt(3) > pt_jet_min ) 
     .     call pwhgfill(diag,rap(3),dsig/binsize(diag))
c
c     y of third jet with reduced pt_jet cut
      diag=175
      if(mjets.ge.3 .and. kt(3) > pt_cut ) 
     .     call pwhgfill(diag,rap(3),dsig/binsize(diag))

c     pt-rel of first jet
      diag=69
      if(mjets.ge.1) call pwhgfill(diag,ptrel(1),dsig/binsize(diag))
c     pt-rel of second jet
      diag=70
      if(mjets.ge.2) call pwhgfill(diag,ptrel(2),dsig/binsize(diag))

C     M_j1j2
      diag=71
      mjj = invmass(pj12)
      if(mjets.ge.2) call pwhgfill(diag,mjj,dsig/binsize(diag))

c     Phi(j1j2) 
      diag=72 
      phijj=fphi(pj(1:4,1),pj(1:4,2))
      if(mjets.ge.2) call pwhgfill(diag,phijj,dsig/binsize(diag))


c standard pt_jet cut:
      if(mjets.ge.3 .and. kt(3) > pt_jet_min ) then
          ystar     = rap(3)-(rap(1)+rap(2))/2d0
          ystar_abs = ystar/abs(rap(1)-rap(2))
c         y* 
          diag=77
          call pwhgfill(diag,ystar,dsig/binsize(diag))
c         |y*| 
          diag=78
          call pwhgfill(diag,ystar_abs,dsig/binsize(diag))
      endif

c reduced pt_jet cut:
      if(mjets.ge.3 .and. kt(3) > pt_cut ) then
          ystar     = rap(3)-(rap(1)+rap(2))/2d0
          ystar_abs = ystar/abs(rap(1)-rap(2))
c         y* 
          diag=176
          call pwhgfill(diag,ystar,dsig/binsize(diag))
c         |y*| 
          diag=177
          call pwhgfill(diag,ystar_abs,dsig/binsize(diag))
      endif

c     (y1+y2)/2
      diag=79
      rap_av = (rap(1)+rap(2))/2d0
      call pwhgfill(diag,rap_av,dsig/binsize(diag))

      diag = 80
      if (mjets .ge. 2) then
        Rj12 = r(pj(1:4,1),pj(1:4,2))
        call pwhgfill(diag,Rj12,dsig/binsize(diag))
      endif

cccccccccccccccccccccccccccccccccc
c
c check, if event passes VBF cuts:
c
c identify 2 hardest jets ("tag" jets) and 3rd hardest jet, 
c and check if they pass pt and rap cuts:
      pt_1 = 0d0
      pt_2 = 0d0
      pt_3 = 0d0
      itag1 = 0
      itag2 = 0
      itag3 = 0

      do j = 1, mjets
           if ( kt(j).gt.pt_1 .and. kt(j).gt.pt_tag_min 
     &          .and. abs(rap(j)).lt.yj_max) then
              itag1 = j   
              pt_1  = kt(j)
           end if
      end do
      do j = 1, mjets
           if ( kt(j).gt.pt_2 .and. kt(j).gt.pt_tag_min 
     &          .and. abs(rap(j)).lt.yj_max
     &          .and. j.ne.itag1 
     &         ) then
              itag2 = j    
              pt_2  = kt(j)
           end if         
      end do
c
      do j = 1, mjets
           if ( kt(j).gt.pt_3 .and. kt(j).gt.ptmin 
     &          .and. abs(rap(j)).lt.yj_max
     &          .and. j.ne.itag1 
     &          .and. j.ne.itag2
     &         ) then
              itag3= j    
              pt_3 = kt(j)
           end if         
      end do
      if (itag1.eq.0.or.itag2.eq.0) passcuts_vbf = .false. 
      if (itag1.eq.0.or.itag2.eq.0) return

c identify 2 hardest leptons: 
      ptl_1 = 0d0
      ptl_2 = 0d0
      ltag1 = 0
      ltag2 = 0
      do j = 1,nleptons
         ptl(j) = ptfromp(phep(1,ileptons(j)))
         etal(j)= etafromp(phep(1,ileptons(j)))
      enddo
      do j = 1,nleptons 
           if ( ptl(j).gt.ptl_1 .and. ptl(j).gt.ptl_min 
     &          .and. abs(etal(j)).lt.etal_max) then
              ltag1 = j   
              ptl_1 = ptl(j)
           end if
      end do
      do j = 1,nleptons
           if ( ptl(j).gt.ptl_2 .and. ptl(j).gt.ptl_min 
     &          .and. abs(etal(j)).lt.etal_max
     &          .and. j.ne.ltag1 
     &         ) then
              ltag2 = j    
              ptl_2  = ptl(j)
           end if         
      end do
      if (ltag1.eq.0.or.ltag2.eq.0) passcuts_vbf = .false.
      if (ltag1.eq.0.or.ltag2.eq.0) return 
c
cccccccccccccccccccccccccccccccccccccc
c
C     now come histograms with VBF cuts 
c
      if(mjets.lt.2) passcuts_vbf = .false.      
      if (itag1.eq.0.or.itag2.eq.0) passcuts_vbf = .false. 
      if (ltag1.eq.0.or.ltag2.eq.0) passcuts_vbf = .false. 

      if (kt(itag1).lt.pt_tag_min) passcuts_vbf = .false.   
      if (kt(itag2).lt.pt_tag_min) passcuts_vbf = .false. 
c      
      if(abs(rap(itag1)).gt.yj_max) passcuts_vbf = .false. 
      if(abs(rap(itag2)).gt.yj_max) passcuts_vbf = .false.
      if(abs(rap(itag1)-rap(itag2)).lt.yjj_min) passcuts_vbf = .false.
      if (rap_sign.and.(rap(itag1)*rap(itag2).ge.0d0)) 
     &     passcuts_vbf = .false.
c      
      Rjj_tmp = r(pj(1:4,itag1),pj(1:4,itag2))
      if (Rjj_tmp.lt.Rjj_min) passcuts_vbf = .false.
      
c      etael1=etafromp(phep(1,ileptons(ltag1)))
      etael1=etal(ltag1)
      if(abs(etael1).gt.etal_max) passcuts_vbf = .false. 

c      etael2=etafromp(phep(1,ileptons(ltag2)))
      etael2=etal(ltag2)
      if(abs(etael2).gt.etal_max)  passcuts_vbf = .false. 

c      ptel1=ptfromp(phep(1,ileptons(ltag1)))
      ptel1=ptl(ltag1)
      if(ptel1.lt.ptl_min)  passcuts_vbf = .false. 

c      ptel2=ptfromp(phep(1,ileptons(ltag2)))
      ptel2=ptl(ltag2)
      if(ptel2.lt.ptl_min)  passcuts_vbf = .false. 

c separation of tagging jets from leading charged leptons:
      Rjl_tmp1 = 1d10
      do ij = 1,mjets
         if(ij.eq.itag1.or.ij.eq.itag2) then
         do il = 1,nleptons
            if(il.eq.ltag1.or.il.eq.ltag2) then
               Rjl_tmp0 = r(pj(1:4,ij),phep(1:4,ileptons(il)))
               Rjl_tmp1 = min(Rjl_tmp0,Rjl_tmp1)
            endif   
         enddo !il
         endif
      enddo !ij
      if (Rjl_tmp1.lt.Rjl_min) passcuts_vbf = .false.

      Rll_tmp = r(phep(1:4,ileptons(ltag1)),
     &            phep(1:4,ileptons(ltag2)))
      if (Rll_tmp.lt.Rll_min) passcuts_vbf = .false.

      pj12 = pj(:4,itag1)+pj(:4,itag2)
      mjj = invmass(pj12)
      if(mjj.lt.mjj_min)  passcuts_vbf = .false. 

      if (rap_gap.and.
     &     (min(rap(itag1),rap(itag2)).ge.min(etael1,etael2)))
     & passcuts_vbf = .false.

      if (rap_gap.and.
     &     (max(rap(itag1),rap(itag2)).le.max(etael1,etael2)))
     & passcuts_vbf = .false.

c*************************

      if (passcuts_vbf) then
c
c********
c
c qsq_i with "vbf cuts":
c    qsq_1
      diag=36
      call pwhgfill(diag,dsqrt(q12(4)),dsig/binsize(diag))
c    qsq_2
      diag=37
      call pwhgfill(diag,dsqrt(q34(4)),dsig/binsize(diag))
c
c********

c number of jets with VBF cuts:
      diag = 13
      call pwhgfill(diag,dble(njet),dsig/binsize(diag))


      diag = 83   
      call pwhgfill(diag,0.5d0,dsig)

c     three jet inclusive.
      diag=82
      do j=1,3
         if(mjets.lt.j) kt(j)=0
      enddo
      do j=10,70,10
         if(kt(1).gt.j.and.kt(2).gt.j.and.kt(3).gt.j) then
            call pwhgfill(diag,dble(j),dsig)
         endif
      enddo

c     two jet inclusive and two jet exclusive.
c     It is exclusive if the third jet is below the cut
c     inclusive otherwise
      diag=84
      do j=1,3
         if(mjets.lt.j) kt(j)=0
      enddo
      do j=10,70,10
         if(kt(itag1).gt.dble(j).and.kt(itag2).gt.dble(j)) then
c inclusive:
            call pwhgfill(diag,dble(j),dsig)
c exclusive:
            if(itag3.eq.0) then 
               call pwhgfill(diag+1,dble(j),dsig)
            elseif (kt(itag3).lt.j) then 
               call pwhgfill(diag+1,dble(j),dsig)
            endif
         endif
      enddo
      diag=85

c     PT of jet 1
      diag=86
      call pwhgfill(diag,kt(itag1),dsig/binsize(diag))
c     PT of jet 2
      diag=87
      call pwhgfill(diag,kt(itag2),dsig/binsize(diag))
c     Y of jet 1
      diag=88
      call pwhgfill(diag,rap(itag1),dsig/binsize(diag))
c     Y of jet 2
      diag=91
      call pwhgfill(diag,rap(itag2),dsig/binsize(diag))
c     Y jet1 - Y jet 2
      diag=94
      pj12 = pj(:4,itag1)+pj(:4,itag2)
      y12 = rap(itag1)-rap(itag2)
      call pwhgfill(diag,y12,dsig/binsize(diag))

c     H_T tot
      httot=ptel1+ptel2
      do j=1,mjets
         if(abs(rap(j)).lt.yj_max.and.kt(j).gt.pt_jet_min) 
     &        httot=httot+kt(j)
      enddo
      diag=95
      call pwhgfill(diag,httot,dsig/binsize(diag))

c     H_T tot, 2j
      httot=ptel1+ptel2
      httot=httot+kt(itag1)+kt(itag2)
      diag=96
      call pwhgfill(diag,httot,dsig/binsize(diag))

C     Pt lept     
      diag=97
      call pwhgfill(diag,ptel1,dsig/binsize(diag)/2d0)
      call pwhgfill(diag,ptel2,dsig/binsize(diag)/2d0)

C     ETA lept
      diag=99
      call pwhgfill(diag,etael1,dsig/binsize(diag)/2d0)
      call pwhgfill(diag,etael2,dsig/binsize(diag)/2d0)

C     ETA(l1)- ETA(l2) 
      diag=100
      etael12 = etael1-etael2
      call pwhgfill(diag,etael12,dsig/binsize(diag))

C     M(l1l2) 
      diag=101
      pel12 = phep(:4,ileptons(ltag1))+phep(:4,ileptons(ltag2))
      mll = invmass(pel12)
      call pwhgfill(diag,mll,dsig/binsize(diag))

C     Delta R(j1 lept) 
      diag=103
      if (mjets .ge. 2) then 
      Rj1l = r(pj(1:4,itag1),phep(1:4,ileptons(ltag1)))
      call pwhgfill(diag,Rj1l,dsig/binsize(diag)/2d0)
      Rj1l = r(pj(1:4,itag1),phep(1:4,ileptons(ltag2)))
      call pwhgfill(diag,Rj1l,dsig/binsize(diag)/2d0)
      endif

C     Delta R(j2 lept) 
      diag=104
      if (mjets .ge. 2) then 
      Rj2l = r(pj(1:4,itag2),phep(1:4,ileptons(ltag1)))
      call pwhgfill(diag,Rj2l,dsig/binsize(diag)/2d0)
      Rj2l = r(pj(1:4,itag2),phep(1:4,ileptons(ltag2)))
      call pwhgfill(diag,Rj2l,dsig/binsize(diag)/2d0)
      endif

c     Phi(l1l2) 
      diag=105 
      phill=fphi(phep(1,ileptons(ltag1)),phep(1,ileptons(ltag2)))
      call pwhgfill(diag,phill,dsig/binsize(diag))

c     pt of third jet
      diag=106
      if(itag3.ne.0) call pwhgfill(diag,kt(itag3),dsig/binsize(diag))

c     y of third jet, kt3>pt_jet_min
      diag=107
      if(itag3.ne.0 .and. kt(itag3) > pt_jet_min ) 
     .     call pwhgfill(diag,rap(itag3),dsig/binsize(diag))
c     y of third jet, kt3>pt_cut
      diag=178
      if(itag3.ne.0 .and. kt(itag3) > pt_cut ) 
     .     call pwhgfill(diag,rap(itag3),dsig/binsize(diag))

c     pt-rel of first jet
      diag=109
      if(mjets.ge.1) call pwhgfill(diag,ptrel(itag1),dsig/binsize(diag))
c     pt-rel of second jet
      diag=110
      if(mjets.ge.2) call pwhgfill(diag,ptrel(itag2),dsig/binsize(diag))

C     M_j1j2
      diag=111
      mjj = invmass(pj12)
      if(mjets.ge.2) call pwhgfill(diag,mjj,dsig/binsize(diag))

c     Phi(j1j2) 
      diag=112 
      phijj=fphi(pj(1:4,itag1),pj(1:4,itag2))
      if(mjets.ge.2) call pwhgfill(diag,phijj,dsig/binsize(diag))

      if(itag3.ne.0 .and. kt(itag3) > pt_jet_min ) then 
          ystar     = rap(itag3)-(rap(itag1)+rap(itag2))/2d0
          ystar_abs = ystar/abs(rap(itag1)-rap(itag2))
c         y* 
          diag=117
          call pwhgfill(diag,ystar,dsig/binsize(diag))
c         |y*| 
          diag=118
          call pwhgfill(diag,ystar_abs,dsig/binsize(diag))
      endif

      if(itag3.ne.0 .and. kt(itag3) > pt_cut ) then 
          ystar     = rap(itag3)-(rap(itag1)+rap(itag2))/2d0
          ystar_abs = ystar/abs(rap(itag1)-rap(itag2))
c         y* 
          diag=179
          call pwhgfill(diag,ystar,dsig/binsize(diag))
c         |y*| 
          diag=180
          call pwhgfill(diag,ystar_abs,dsig/binsize(diag))
      endif

c     (y1+y2)/2
      diag=119
      rap_av = (rap(itag1)+rap(itag2))/2d0
      call pwhgfill(diag,rap_av,dsig/binsize(diag))

      diag = 120
      Rj12 = r(pj(1:4,itag1),pj(1:4,itag2))
      call pwhgfill(diag,Rj12,dsig/binsize(diag))
c
ccccccccccccccccccccccccccccccccccccccccccccc
c
      passcuts_cjv = .true.

c identify 2 hardest veto jets:
      pt_v1 = 0d0
      pt_v2 = 0d0
      iveto1 = 0
      iveto2 = 0

      nveto = 0

      if (rap_veto) then 
c hardest veto jet:
      do j = 1, mjets
           if ( kt(j).gt.pt_v1.and.kt(j).gt.pt_veto .and.(
     &          rap(j).gt.min(rap(itag1),rap(itag2)).and.
     &          rap(j).lt.max(rap(itag1),rap(itag2)))
     &        ) then              
              iveto1 = j
              pt_v1 = kt(j)
           end if
      end do

c 2nd hardest veto jet:
      do j = 1, mjets
           if ( kt(j).gt.pt_v2.and.kt(j).gt.pt_veto .and.(
     &          rap(j).gt.min(rap(itag1),rap(itag2)).and.
     &          rap(j).lt.max(rap(itag1),rap(itag2))).and. 
     &          j.ne.iveto1 ) then
              iveto2 = j
              pt_v2 = kt(j)
           end if
      end do

c count veto jet:
      do j = 1, mjets
           if ( kt(j).gt.pt_veto .and.
     &          rap(j).gt.min(rap(itag1),rap(itag2)).and.
     &          rap(j).lt.max(rap(itag1),rap(itag2))) then
              nveto = nveto+1
           end if
      end do


      else ! no rap-constraint on veto jet 
c hardest veto jet:
      do j = 1, mjets
           if ( kt(j).gt.pt_v1.and.kt(j).gt.pt_veto
     &        ) then              
              iveto1 = j
              pt_v1 = kt(j)
           end if
      end do

c 2nd hardest veto jet:
      do j = 1, mjets
           if ( kt(j).gt.pt_v2.and.kt(j).gt.pt_veto.and. 
     &          j.ne.iveto1 ) then
              iveto2 = j
              pt_v2 = kt(j)
           end if
      end do

c count veto jet:
      do j = 1, mjets
           if ( kt(j).gt.pt_veto) then
              nveto = nveto+1
           end if
      end do

      endif !identification of veto jets

c     pt of hardest veto jet
      diag=165
      if(iveto1.ne.0) call pwhgfill(diag,kt(iveto1),dsig/binsize(diag))
c     pt of 2nd hardest veto jet
      diag=166
      if(iveto2.ne.0) call pwhgfill(diag,kt(iveto2),dsig/binsize(diag))
c     y of 1st veto jet
      diag=167
      if(iveto1.ne.0) call pwhgfill(diag,rap(iveto1),dsig/binsize(diag))
c     y of 2nd veto jet
      diag=168
      if(iveto2.ne.0) call pwhgfill(diag,rap(iveto2),dsig/binsize(diag))
    
c     number of veto jets
      diag=169
      call pwhgfill(diag,dble(nveto),dsig/binsize(diag))

      if (nveto.ne.0) passcuts_cjv = .false.       
c
cccccccccccccccccccccccc
c
c histograms with VBF and extra CJV cuts:
c
      if (passcuts_cjv) then
c
c********
c
c qsq_i with "vbf cuts":
c    qsq_1
      diag=38
      call pwhgfill(diag,dsqrt(q12(4)),dsig/binsize(diag))
c    qsq_2
      diag=39
      call pwhgfill(diag,dsqrt(q34(4)),dsig/binsize(diag))
c
c********

c number of jets with CJV cuts:
      diag = 14
      call pwhgfill(diag,dble(njet),dsig/binsize(diag))

c cross section:
      diag = 123   
      call pwhgfill(diag,0.5d0,dsig)

c     three jet inclusive.
      diag=122
      do j=1,3
         if(mjets.lt.j) kt(j)=0
      enddo
      do j=10,70,10
         if(kt(1).gt.j.and.kt(2).gt.j.and.kt(3).gt.j) then
            call pwhgfill(diag,dble(j),dsig)
         endif
      enddo

c     two jet inclusive and two jet exclusive.
c     It is exclusive if the third jet is below the cut
c     inclusive otherwise
      diag=124
      do j=1,3
         if(mjets.lt.j) kt(j)=0
      enddo
      do j=10,70,10
         if(kt(itag1).gt.dble(j).and.kt(itag2).gt.dble(j)) then
c inclusive:
            call pwhgfill(diag,dble(j),dsig)
c exclusive:
            if(itag3.eq.0) then 
               call pwhgfill(diag+1,dble(j),dsig)
            elseif (kt(itag3).lt.j) then 
               call pwhgfill(diag+1,dble(j),dsig)
            endif
         endif
      enddo
      diag=125

c     PT of jet 1
      diag=126
      call pwhgfill(diag,kt(itag1),dsig/binsize(diag))
c     PT of jet 2
      diag=127
      call pwhgfill(diag,kt(itag2),dsig/binsize(diag))
c     Y of jet 1
      diag=128
      call pwhgfill(diag,rap(itag1),dsig/binsize(diag))
c     Y of jet 2
      diag=131
      call pwhgfill(diag,rap(itag2),dsig/binsize(diag))
c     Y jet1 - Y jet 2
      diag=134
      pj12 = pj(:4,itag1)+pj(:4,itag2)
      y12 = rap(itag1)-rap(itag2)
      call pwhgfill(diag,y12,dsig/binsize(diag))

c     H_T tot
      httot=ptel1+ptel2
      do j=1,mjets
         if(abs(rap(j)).lt.yj_max.and.kt(j).gt.pt_jet_min) 
     &        httot=httot+kt(j)
      enddo
      diag=135
      call pwhgfill(diag,httot,dsig/binsize(diag))

c     H_T tot, 2j
      httot=ptel1+ptel2
      httot=httot+kt(itag1)+kt(itag2)
      diag=136
      call pwhgfill(diag,httot,dsig/binsize(diag))

C     Pt lept     
      diag=137
      call pwhgfill(diag,ptel1,dsig/binsize(diag)/2d0)
      call pwhgfill(diag,ptel2,dsig/binsize(diag)/2d0)

C     ETA lept
      diag=139
      call pwhgfill(diag,etael1,dsig/binsize(diag)/2d0)
      call pwhgfill(diag,etael2,dsig/binsize(diag)/2d0)

C     ETA(l1)- ETA(l2) 
      diag=140
      etael12 = etael1-etael2
      call pwhgfill(diag,etael12,dsig/binsize(diag))

C     M(l1l2) 
      diag=141
      pel12 = phep(:4,ileptons(ltag1))+phep(:4,ileptons(ltag2))
      mll = invmass(pel12)
      call pwhgfill(diag,mll,dsig/binsize(diag))

C     Delta R(j1 lept) 
      diag=143
      if (mjets .ge. 2) then 
      Rj1l = r(pj(1:4,itag1),phep(1:4,ileptons(ltag1)))
      call pwhgfill(diag,Rj1l,dsig/binsize(diag)/2d0)
      Rj1l = r(pj(1:4,itag1),phep(1:4,ileptons(ltag2)))
      call pwhgfill(diag,Rj1l,dsig/binsize(diag)/2d0)
      endif

C     Delta R(j2 lept) 
      diag=144
      if (mjets .ge. 2) then 
      Rj2l = r(pj(1:4,itag2),phep(1:4,ileptons(ltag1)))
      call pwhgfill(diag,Rj2l,dsig/binsize(diag)/2d0)
      Rj2l = r(pj(1:4,itag2),phep(1:4,ileptons(ltag2)))
      call pwhgfill(diag,Rj2l,dsig/binsize(diag)/2d0)
      endif

c     Phi(l1l2) 
      diag=145 
      phill=fphi(phep(1,ileptons(ltag1)),phep(1,ileptons(ltag2)))
      call pwhgfill(diag,phill,dsig/binsize(diag))

c     pt of third jet
      diag=146
      if(itag3.ne.0) call pwhgfill(diag,kt(itag3),dsig/binsize(diag))

c     y of third jet, kt>pt_jet_min
      diag=147
      if(itag3.ne.0 .and. kt(itag3) > pt_jet_min ) 
     .     call pwhgfill(diag,rap(itag3),dsig/binsize(diag))
c     y of third jet, kt>pt_cut
      diag=181
      if(itag3.ne.0 .and. kt(itag3) > pt_cut ) 
     .     call pwhgfill(diag,rap(itag3),dsig/binsize(diag))

c     pt-rel of first jet
      diag=149
      if(mjets.ge.1) call pwhgfill(diag,ptrel(itag1),dsig/binsize(diag))
c     pt-rel of second jet
      diag=150
      if(mjets.ge.2) call pwhgfill(diag,ptrel(itag2),dsig/binsize(diag))

C     M_j1j2
      diag=151
      mjj = invmass(pj12)
      if(mjets.ge.2) call pwhgfill(diag,mjj,dsig/binsize(diag))

c     Phi(j1j2) 
      diag=152 
      phijj=fphi(pj(1:4,itag1),pj(1:4,itag2))
      if(mjets.ge.2) call pwhgfill(diag,phijj,dsig/binsize(diag))

      if(itag3.ne.0 .and. kt(itag3) > pt_jet_min ) then 
          ystar     = rap(itag3)-(rap(itag1)+rap(itag2))/2d0
          ystar_abs = ystar/abs(rap(itag1)-rap(itag2))
c         y* 
          diag=157
          call pwhgfill(diag,ystar,dsig/binsize(diag))
c         |y*| 
          diag=158
          call pwhgfill(diag,ystar_abs,dsig/binsize(diag))
      endif
      if(itag3.ne.0 .and. kt(itag3) > pt_cut ) then 
          ystar     = rap(itag3)-(rap(itag1)+rap(itag2))/2d0
          ystar_abs = ystar/abs(rap(itag1)-rap(itag2))
c         y* 
          diag=182
          call pwhgfill(diag,ystar,dsig/binsize(diag))
c         |y*| 
          diag=183
          call pwhgfill(diag,ystar_abs,dsig/binsize(diag))
      endif

c     (y1+y2)/2
      diag=159
      rap_av = (rap(itag1)+rap(itag2))/2d0
      call pwhgfill(diag,rap_av,dsig/binsize(diag))

      diag = 160
      Rj12 = r(pj(1:4,itag1),pj(1:4,itag2))
      call pwhgfill(diag,Rj12,dsig/binsize(diag))

c
      endif !passcuts_vbf


      endif !passcuts_vbf


      end
      
      function etafromp(p)
      implicit none
      real * 8 p(4),etafromp,pp
      pp=sqrt(p(1)**2+p(2)**2+p(3)**2)
      if (pp-abs(p(3)) .lt. 1d-13) then 
         etafromp = 100d0 
      else
         etafromp=log((pp+p(3))/(pp-p(3)))/2
      endif
      end
      
      function ptfromp(p)
      implicit none
      real * 8 p(4),ptfromp
      ptfromp=sqrt(p(1)**2+p(2)**2)
      end

      function invmass(p)
      implicit none
      real * 8 p(1:4),invmass
      invmass=sqrt(abs(p(4)**2-p(1)**2-p(2)**2-p(3)**2))
      end


c---- calculate the jets separation between p1 and p2
      double precision function r(p1,p2)
      implicit none
      double precision p1(4),p2(4),r2,dely,delphi,e1,e2
      
      e1=dsqrt(p1(1)**2+p1(2)**2+p1(3)**2)
      e2=dsqrt(p2(1)**2+p2(2)**2+p2(3)**2)
      
      dely = (e1+p1(3))*(e2-p2(3))/
     .     ((e2+p2(3))*(e1-p1(3)))
      dely = 0.5d0*dlog(dely)
      
      r2= (p1(1)*p2(1)+p1(2)*p2(2))
     .     /dsqrt((p1(1)**2+p1(2)**2)*(p2(1)**2+p2(2)**2))
      if (r2 .gt. +0.9999999D0) r2=+1d0
      if (r2 .lt. -0.9999999D0) r2=-1d0
      delphi=dacos(r2)

      r=dsqrt(dely**2+delphi**2)
      
      end


C---  calculate azimuthal angle between vectors 
      double precision function fphi(p1,p2)
      implicit none
      double precision p1(4),p2(4)
      double precision pi
      parameter(pi=3.14159265358979d0)
   
      fphi=p1(1)*p2(1)+p1(2)*p2(2)
      fphi=fphi/dsqrt(p1(1)**2+p1(2)**2)
      fphi=fphi/dsqrt(p2(1)**2+p2(2)**2)
      if     (fphi .gt. +0.9999999D0) then
        fphi=0d0
      elseif (fphi .lt. -0.9999999D0) then
        fphi=pi
      else
        fphi=dacos(fphi)
      endif

      end
      
      function idigit(k,l)
      implicit none
      integer idigit,k,l
      idigit=abs(mod(l,10**k)/10**(k-1))
      end

      subroutine buildjets(mjets,kt,eta,rap,phi,pjet,ptrel)
c     arrays to reconstruct jets
      implicit none
      integer mjets
      real * 8  kt(mjets),eta(mjets),rap(mjets),phi(mjets),
     &          pjet(4,mjets),ptrel(mjets)
      include   '../include/hepevt.h'
      integer   maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real * 8  ptrack(4,maxtrack),pj(4,maxjet)
      integer   jetvec(maxtrack),itrackhep(maxtrack)
      integer   ntracks,njets
      integer   j,k,mu
      real * 8 palg,pp,tmp
c
      real*8 ptmin,yjmax,R
      common /jetcuts/ptmin,yjmax,R
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C - Initialize arrays and counters for output jets
      do j=1,maxtrack
         do mu=1,4
            ptrack(mu,j)=0d0
         enddo
         jetvec(j)=0
      enddo      
      ntracks=0
      do j=1,mjets
         do mu=1,4
            pjet(mu,j)=0d0
            pj(mu,j)=0d0
         enddo
      enddo
C - Extract final state particles to feed to jet finder
      do j=1,nhep
         if (isthep(j).eq.1.and.
     1        (abs(idhep(j)).lt.11.or.abs(idhep(j)).gt.16)) then
            if(ntracks.eq.maxtrack) then
               write(*,*) 'analyze: need to increase maxtrack!'
               write(*,*) 'ntracks: ',ntracks
               stop
            endif
            ntracks=ntracks+1
            do mu=1,4
               ptrack(mu,ntracks)=phep(mu,j)
            enddo
            itrackhep(ntracks)=j
         endif
      enddo
      if (ntracks.eq.0) then
         mjets=0
         return
      endif
C --------------------------------------------------------------------- C
C - Inclusive jet pT and Y spectra are to be compared to CDF data:    - C    
C --------------------------------------------------------------------- C
c
cc note: ptmin and R are in common block "jetcuts"
c
c this is for kt-algorithm:
c      call fastjetktwhich(ptrack,ntracks,ptmin,R,
c     #     pjet,njets,jetvec) 
c
c anti-kT (c.f. ZZ code):
      palg=-1
c      r=rr
c      ptmin=1d0 
      call fastjetppgenkt(ptrack,ntracks,r,palg,ptmin,pjet,njets,
     $                        jetvec)

      mjets=min(mjets,njets)
      if(njets.eq.0) return
c check consistency
      do k=1,ntracks
         if(jetvec(k).gt.0) then
            do mu=1,4
               pj(mu,jetvec(k))=pj(mu,jetvec(k))+ptrack(mu,k)
            enddo
         endif
      enddo
      tmp=0
      do j=1,mjets
         do mu=1,4
            tmp=tmp+abs(pj(mu,j)-pjet(mu,j))
         enddo
      enddo
      if(tmp.gt.1d-4) then
         write(*,*) ' bug!'
      endif

c      print*,'mjets =',mjets

c end check consistency
c
C --------------------------------------------------------------------- C
C - Computing arrays of useful kinematics quantities for hardest jets - C
C --------------------------------------------------------------------- C
      do j=1,mjets
         kt(j)=sqrt(pjet(1,j)**2+pjet(2,j)**2)
         pp = sqrt(kt(j)**2+pjet(3,j)**2)
         eta(j)=0.5d0*log((pjet(4,j)+pjet(3,j))/(pjet(4,j)-pjet(3,j)))
         rap(j)=0.5d0*log((pjet(4,j)+pjet(3,j))/(pjet(4,j)-pjet(3,j)))
         phi(j)=atan2(pjet(2,j),pjet(1,j))
      enddo

c      print*,'in jet:',kt(1:mjets)
      call computeptrel(ptrack,ntracks,rap,kt,phi,mjets,jetvec,ptrel)

c      print*,'end jet:',ptrel(1)

      end

      logical function bson(j,jb)
      implicit none
      integer j,jb
      include   '../include/hepevt.h'
      integer jcurr
      logical bhadrstable
      jcurr=j
c     This only happens in parton level analysis
      if(abs(idhep(jcurr)).eq.5) then
         bson=.true.
         jb=jcurr
         return
      endif
 1    continue
      bson=.false.
      if(bhadrstable(idhep(jcurr))) then
         bson=.true.
         jb=jcurr
         return
      endif
      jcurr=jmohep(1,jcurr)
      if(idhep(jcurr).eq.0) then
         bson=.false.
         return
      endif
      goto 1
      end

      logical function bhadr(idhep)
      implicit none
      integer idhep
      integer i1,i2,idigit
      i1=idigit(1,idhep)
      if(i1.eq.1) then
c         is a bottomed meson
         i2=idigit(3,idhep)
      elseif(i1.eq.2) then
c is a bottomed barion
         i2=idigit(4,idhep)
      endif
      if(i2.eq.5) then
         bhadr=.true.
      else
         bhadr=.false.
      endif
      end

      logical function bhadrstable(idhep)
      implicit none
      integer idhep
      integer i1,i2,i3,idigit
      i1=idigit(1,idhep)
      if(i1.eq.1) then
c         is a bottomed meson
         i2=idigit(3,idhep)
         i3=idhep/1000
      elseif(i1.eq.2) then
c is a bottomed barion
         i2=idigit(4,idhep)
         i3=idhep/10000
      else
         bhadrstable=.false.
c         write(133,*) idhep,' false '
         return
      endif
      if(i2.eq.5.and.i3.eq.0) then
         bhadrstable=.true.
c         write(133,*) idhep,' true '
      else
         bhadrstable=.false.
c         write(133,*) idhep,' false '
      endif
      end

      subroutine incbhadrons(jb)
      implicit none
      include   '../include/hepevt.h'      
      integer jb
      integer barray(1000),bnum
      common/cbarray/barray,bnum
      integer k
      if(jb.eq.-1) then
         bnum=0
         return
      endif
      do k=1,bnum
         if(jb.eq.barray(k)) return
         if(jmohep(1,barray(k)).eq.jb) return
         if(jmohep(1,jb).eq.barray(k)) then
            barray(k)=jb
            return
         endif
      enddo
      bnum=bnum+1
      barray(bnum)=jb
      end

      subroutine sortbypt(n,iarr)
      implicit none
      integer n,iarr(n)
      include '../include/hepevt.h'
      integer j,k
      real * 8 tmp,pt(nmxhep)
      logical touched
      do j=1,n
         pt(j)=sqrt(phep(1,iarr(j))**2+phep(2,iarr(j))**2)
      enddo
c bubble sort
      touched=.true.
      do while(touched)
         touched=.false.
         do j=1,n-1
            if(pt(j).lt.pt(j+1)) then
               k=iarr(j)
               iarr(j)=iarr(j+1)
               iarr(j+1)=k
               tmp=pt(j)
               pt(j)=pt(j+1)
               pt(j+1)=tmp
               touched=.true.
            endif
         enddo
      enddo
      end


      subroutine computeptrel(ptracks,ntracks,rapjets,ktjets,phijets,
     1     njets,jetvec,ptrel)
      implicit none
      integer ntracks,njets,jetvec(ntracks)
      real * 8 ptracks(4,ntracks),rapjets(njets),
     1     ktjets(njets),phijets(njets),ptrel(njets)
      integer j,i
      real * 8 yj,kj1,kj2,y,pt(3)
      do j=1,njets
         ptrel(j)=0
      enddo
      do i=1,ntracks
         j=jetvec(i)
         if(j.gt.0.and.j.le.njets) then
c Track i belongs to jet j
            yj=rapjets(j)
            kj1=ktjets(j)*cos(phijets(j))
            kj2=ktjets(j)*sin(phijets(j))
c rapidity of track i
            y=0.5d0*log((ptracks(4,i)+ptracks(3,i))
     1                 /(ptracks(4,i)-ptracks(3,i)))
c rapidity of track i in frame where the jet has zero rapidity
            y=y-yj
c find momentum of track i in frame where the jet has zero rapidity
            pt(1)=ptracks(1,i)
            pt(2)=ptracks(2,i)
            pt(3)=sqrt(pt(1)**2+pt(2)**2)*sinh(y)
c pt rel is sum of the ptrack momentum projection ortogonal to the jet
c momentum in the frame where the jet has zero rapidity
            ptrel(j)=sqrt(((pt(1)*kj2-pt(2)*kj1)**2+
     1                     (         -pt(3)*kj2)**2+
     2                     (pt(3)*kj1          )**2)/
     3                     (kj1**2+kj2**2)) + ptrel(j)
         endif
      enddo
      end
