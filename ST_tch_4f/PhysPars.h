c -*- Fortran -*-
      double precision ph_alphaem,ph_Zmass,ph_Zwidth,ph_Wmass,ph_Wwidth,ph_cthw,
     $     ph_sthw,ph_sthw2,ph_Zmass2,ph_Zmass2low,ph_Zmass2high,
     $     ph_Wmass2,ph_Wmass2low,ph_Wmass2high ,ph_ZmZw,ph_WmWw,
     $     ph_unit_e, ph_CKM(3,3)
      common/ph_common/ph_alphaem,ph_Zmass,ph_Zwidth,ph_Wmass,ph_Wwidth,
     $     ph_cthw,ph_sthw,ph_sthw2,ph_Zmass2,ph_Zmass2low,
     $     ph_Zmass2high,ph_Wmass2,ph_Wmass2low,ph_Wmass2high ,ph_ZmZw,
     $     ph_WmWw,ph_unit_e,ph_CKM
      
c     T/TBAR
      integer ttype
      common/cttype/ttype
      
      double precision topmass_pow,bmass_pow,topwidth_pow
      common/ctopmass/topmass_pow,bmass_pow,topwidth_pow
