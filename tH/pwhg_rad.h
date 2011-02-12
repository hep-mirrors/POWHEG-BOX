c -*- Fortran -*-
c rad_ubornidx: current index of underlying born
c rad_alr_list: list of alr's that share the current underlying born
c rad_alr_nlist: length of the above list
c rad_realidx: index in rad_alr_list of current alr
c rad_realalr: current alr
c rad_realreg: index of regular contribution in the array flst_regular
      integer rad_ubornidx,rad_alr_list(maxalr),rad_alr_nlist,
     #     rad_realidx,rad_realalr,rad_realreg
c rad_kinreg: index in current kinematic region
c rad_nkinreg: number of kinematic regions
c     kinematic regions are numbered as:
c     1: initial state region
c     2 ... rad_nkinreg: final state regions with increasing
c                        emitter
c     rad_kinreg_on(rad_nkinreg): logical, entry j is true if there is a region
c     with rad_kinreg=j associated with current underlying born.
      integer rad_kinreg,rad_nkinreg
      logical rad_kinreg_on(nlegborn-1)
c rad_ncsiynormsmx: maximum number of csi-y subdivision when computing
c                   the upper bounds
c rad_ncsinorms,rad_nynorms: effective number of csi and y subdivisions
      integer rad_ncsiynormsmx
      parameter (rad_ncsiynormsmx=100)
      integer rad_ncsinorms,rad_nynorms
c 1 for Btilde event, 2 for remnant, 3 for regular
      integer rad_type
c Signed total, absolute value total, positive total and negative total
c obtained in the integration of btilde, sigremnant
      real * 8
     1     rad_totbtl,rad_etotbtl,
     2     rad_totabsbtl,rad_etotabsbtl,
     3     rad_totposbtl,rad_etotposbtl,
     4     rad_totnegbtl,rad_etotnegbtl,
     5     rad_totreg,rad_etotreg,
     6     rad_totabsreg,rad_etotabsreg,
     7     rad_totposreg,rad_etotposreg,
     8     rad_totnegreg,rad_etotnegreg,
     9     rad_totrem,rad_etotrem,
     1     rad_totabsrem,rad_etotabsrem,
     2     rad_totposrem,rad_etotposrem,
     3     rad_totnegrem,rad_etotnegrem,
     4     rad_totbtlgen,rad_etotbtlgen,
     5     rad_tot,rad_etot,
     6     rad_totgen,rad_etotgen
      real * 8 rad_totarr(2,15)
      equivalence
     1 (rad_totbtl,   rad_totarr(1, 1)),
     a              (rad_etotbtl,   rad_totarr(2, 1)),
     2 (rad_totabsbtl,rad_totarr(1, 2)),
     a              (rad_etotabsbtl,rad_totarr(2, 2)),
     3 (rad_totposbtl,rad_totarr(1, 3)),
     a              (rad_etotposbtl,rad_totarr(2, 3)),
     4 (rad_totnegbtl,rad_totarr(1, 4)),
     a              (rad_etotnegbtl,rad_totarr(2, 4)),
     5 (rad_totreg,   rad_totarr(1, 5)),
     a              (rad_etotreg,   rad_totarr(2, 5)),
     6 (rad_totabsreg,rad_totarr(1, 6)),
     a              (rad_etotabsreg,rad_totarr(2, 6)),
     7 (rad_totposreg,rad_totarr(1, 7)),
     a              (rad_etotposreg,rad_totarr(2, 7)),
     8 (rad_totnegreg,rad_totarr(1, 8)),
     a              (rad_etotnegreg,rad_totarr(2, 8)),
     9 (rad_totrem,   rad_totarr(1, 9)),
     a              (rad_etotrem,   rad_totarr(2, 9)),
     1 (rad_totabsrem,rad_totarr(1,10)),
     a              (rad_etotabsrem,rad_totarr(2,10)),
     2 (rad_totposrem,rad_totarr(1,11)),
     a              (rad_etotposrem,rad_totarr(2,11)),
     3 (rad_totnegrem,rad_totarr(1,12)),
     a              (rad_etotnegrem,rad_totarr(2,12)),
     4 (rad_totbtlgen,rad_totarr(1,13)),
     a              (rad_etotbtlgen,rad_totarr(2,13)),
     5 (rad_tot,      rad_totarr(1,14)),
     a              (rad_etot,      rad_totarr(2,14)),
     6 (rad_totgen,   rad_totarr(1,15)),
     a              (rad_etotgen,   rad_totarr(2,15))
c Grid of the upper bounds of the ratio (R*kn_jacreal/B)/upper_bounding function
c for each given kinematic region and underlying born
      real * 8 rad_csiynorms(rad_ncsiynormsmx,
     #     rad_ncsiynormsmx,nlegborn-1,maxprocborn)
c as above, on the whole grid, for each given underlying born
      real * 8 rad_norms(nlegborn-1,maxprocborn)
c value of btilde for each given underlying Born;
c filled after each final call to btilde.
      real * 8 rad_btilde_arr(maxprocborn)
c stores the sign of the above results in case the BOX is used
c with withnegweights=1.
      integer  rad_btilde_sign(maxprocborn)
c filled with contributions to real cross section after
c a call to sigreal_rad, according to the mapping rad_realalr
      real * 8 rad_real_arr(maxalr)
c filled with contributions of the real remnants after a call to
c sigremnants;
c filled with contributions of the regular real graphs (graphs with no singular
c regions), filled after a call to sigremnants;
c rad_totabsreg=Sum rad_reg_arr, rad_totreg=Sum rad_reg_arr*rad_reg_sign
      real * 8 rad_reg_arr(maxprocreal)
      integer rad_reg_sign(maxprocreal)
      real * 8 rad_damp_rem_arr(maxalr)
      integer  rad_damp_rem_sign(maxalr)
c radiation variables in sigremnant call
      real * 8 rad_xradremn(3)
c user provided factor, to increase the upper bounding ratios
      real * 8 rad_normfact
c minimum pt-squared
      real * 8 rad_ptsqmin,rad_charmthr2,rad_bottomthr2
c LambdaLL for upper bounding coupling (see notes: running_coupling)
      real * 8 rad_lamll
c Hardest radiation kt2
      real * 8 rad_pt2max
c Branching ratio (useful to change xsecup properly when a decay is
c added a posteriori)
      real * 8 rad_branching
      integer rad_iupperfsr,rad_iupperisr
      common/pwhg_rad/
     1     rad_damp_rem_arr,
     2     rad_reg_arr,
     3     rad_totarr,
     4     rad_csiynorms,rad_norms,rad_btilde_arr,rad_real_arr,
     5     rad_normfact,rad_ptsqmin,rad_charmthr2,rad_bottomthr2,
     6     rad_lamll,rad_xradremn,rad_pt2max,
     7     rad_branching,
c     integers
     1     rad_ubornidx,rad_alr_list,rad_alr_nlist,
     2     rad_realidx,rad_realalr,rad_realreg,
     3     rad_kinreg,rad_nkinreg,
     4     rad_ncsinorms,rad_nynorms,rad_type,rad_btilde_sign,
     5     rad_reg_sign,rad_damp_rem_sign,
     5     rad_iupperfsr,rad_iupperisr,
c     logical
     6     rad_kinreg_on
