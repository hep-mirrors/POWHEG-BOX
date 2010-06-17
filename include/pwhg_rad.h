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
c obtained in the integration of btilde
      real * 8
     1     rad_totbtl,rad_etotbtl,
     2     rad_totabsbtl,rad_etotabsbtl,
     3     rad_totposbtl,rad_etotposbtl,
     4     rad_totnegbtl,rad_etotnegbtl,
     5     rad_sigrm,rad_esigrm,
     6     rad_sigbtl,rad_esigbtl,
     7     rad_sigtotgen,rad_esigtotgen,
     8     rad_sigtot,rad_esigtot
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
c sigremnants; rad_damp_rem_tot=Sum rad_damp_rem_arr
      real * 8 rad_damp_rem_arr(maxalr),rad_damp_rem_tot
c filled with contributions of the regular real graphs (graphs with no singular
c regions), filled after a call to sigremnants;
c rad_reg_tot=Sum rad_reg_arr
      real * 8 rad_reg_arr(maxprocreal),rad_reg_tot
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
      common/pwhg_rad/
     1     rad_totbtl,rad_etotbtl,
     2     rad_totabsbtl,rad_etotabsbtl,
     3     rad_totposbtl,rad_etotposbtl,
     4     rad_totnegbtl,rad_etotnegbtl,
     5     rad_sigrm,rad_esigrm,
     6     rad_sigbtl,rad_esigbtl,
     7     rad_sigtotgen,rad_esigtotgen,
     8     rad_sigtot,rad_esigtot,
     #     rad_damp_rem_arr,rad_damp_rem_tot,rad_reg_arr,rad_reg_tot,
     #     rad_csiynorms,rad_norms,rad_btilde_arr,rad_real_arr,
     #     rad_normfact,rad_ptsqmin,rad_charmthr2,rad_bottomthr2,
     #     rad_lamll,rad_xradremn,rad_pt2max,
c     integers
     #     rad_ubornidx,rad_alr_list,rad_alr_nlist,
     #     rad_realidx,rad_realalr,rad_realreg,
     #     rad_kinreg,rad_nkinreg,
     #     rad_ncsinorms,rad_nynorms,rad_type,rad_btilde_sign,
c     logical
     #     rad_kinreg_on
      
      
