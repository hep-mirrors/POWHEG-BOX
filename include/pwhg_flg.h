c -*- Fortran -*-

c flg_nlotest: perform calls to outfun for testing NLO output
c flg_withsubtr: counterterms are included in NLO test
c flg_withdamp: Born zero procedure
c flg_withreg: if regular regions exist or not
c flg_smartsig: remember or not equal suqred amplitude
c flg_bornonly: do the Born contribution only
      logical flg_nlotest,flg_withsubtr,flg_withdamp,flg_withreg,
     1     flg_smartsig,flg_bornonly,flg_debug,flg_withnegweights,
     2     flg_jacsing,flg_weightedev,flg_pdfreweight,flg_collremnsamp
      common/pwhg_flg/flg_nlotest,flg_withsubtr,flg_withdamp,
     2     flg_withreg,flg_smartsig,flg_bornonly,flg_debug,
     3     flg_withnegweights,flg_jacsing,flg_weightedev,
     4     flg_pdfreweight,flg_collremnsamp
      save /pwhg_flg/
