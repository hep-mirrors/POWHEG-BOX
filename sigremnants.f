c Function to be integrated with mint, used to generate
c non-singular contributions using standard mint-gen method
c These contributions arise from real graphs without singular regions,
c or, when damping of R/B value is adopted, from the remnant of the
c damping
      function sigremnant(xx,ww,ifirst)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_flg.h'
      include 'include/pwhg_math.h'
      real * 8 sigremnant,xx(ndiminteg),ww
      integer ifirst
      real * 8 xrad(3)
      real * 8 xborn((nlegborn-2)*3-4+2-1)
      integer j,alr
      real * 8 ttt
      real * 8 jac_over_csi,jac_over_csi_p,jac_over_csi_m,
     #         jac_over_csi_s,jac_over_csi_coll
      real * 8 xjac,suppfact
      logical valid_emitter
      external valid_emitter
      if(ifirst.eq.2) then
         sigremnant=rad_reg_tot+rad_damp_rem_tot
         if(flg_nlotest) call pwhgaccumup
         return
      endif
      do j=1,ndiminteg-3
         xborn(j)=xx(j)
      enddo
      do j=1,3
         xrad(j)=xx(ndiminteg-3 + j)
      enddo
c regular contributions; any phase space parametrization should be OK
      kn_emitter=0
      call gen_born_phsp(xborn)
c set scales
      call setscalesbtilde
c the following is needed to compute soft and collinear limits
      call allborn
      call born_suppression(suppfact)
      if(flg_withreg.or.(flg_withdamp.and.(
     # valid_emitter(0).or.valid_emitter(1).or.valid_emitter(2)))) then
         call gen_real_phsp_isr(xrad,
     #    jac_over_csi,jac_over_csi_p,jac_over_csi_m,jac_over_csi_s)
         xjac=jac_over_csi*kn_csi*kn_csimax*kn_jacborn*suppfact*ww*hc2
      endif
      if(flg_withreg) then
         call sigreal_reg(xjac,rad_reg_tot,rad_reg_arr)
         if(flg_nlotest) then
            call analysis_driver(rad_reg_tot/suppfact,1)
         endif
      else
         rad_reg_tot=0
      endif
      if(flg_withdamp) then
         rad_damp_rem_tot=0
         do alr=1,flst_nalr
            rad_damp_rem_arr(alr)=0
         enddo
         do kn_emitter=0,nlegborn
            if(valid_emitter(kn_emitter)) then
               if(kn_emitter.le.2) then
c     No need to generate phase space; it is already available
                  call sigreal_damp_rem(xjac,ttt,rad_damp_rem_arr)
                  if(flg_nlotest) then
                     call analysis_driver(ttt/suppfact,1)
                  endif
                  rad_damp_rem_tot=rad_damp_rem_tot+ttt
               else
                  call gen_real_phsp_fsr(xrad,
     #jac_over_csi,jac_over_csi_coll,jac_over_csi_s)
                  xjac=jac_over_csi*kn_csi*kn_csimax
     #                *kn_jacborn*suppfact*ww*hc2
                  call sigreal_damp_rem(xjac,ttt,rad_damp_rem_arr)
                  if(flg_nlotest) then
                     call analysis_driver(ttt/suppfact,1)
                  endif
                  rad_damp_rem_tot=rad_damp_rem_tot+ttt
               endif
            endif
         enddo
      else
         rad_damp_rem_tot=0
      endif
      sigremnant=rad_reg_tot+rad_damp_rem_tot
      end

      subroutine sigreal_reg(xjac,sig,r0)
c contributions from real graphs that do not have a singular region
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_flg.h'
      real * 8 xjac,sig,r0(maxprocreal)
      integer lreg,lregpr,iret
      integer nmomset
      parameter (nmomset=10)
      real * 8 res(nmomset,maxprocreal),preal(0:3,nlegreal,nmomset),
     #   cprop
      integer equivto(maxprocreal)
      real * 8 equivcoef(maxprocreal)
      integer j
      real * 8 pdf1(-6:6),pdf2(-6:6)
      logical ini
      data ini/.true./
      save ini,equivto,equivcoef
      if(ini) then
         do lreg=1,flst_nregular
            equivto(lreg)=-1
         enddo
         if(flg_smartsig) then
            call randomsave
c     generate "nmomset" random real-phase space configurations
            call fillmomenta(nlegreal,nmomset,kn_masses,preal)
            do lreg=1,flst_nregular
               do j=1,nmomset
                  call realgr(
     1                 flst_regular(1,lreg),preal(0,1,j),res(j,lreg))
               enddo
               call compare_vecs_reg(nmomset,lreg,res,lregpr,cprop,iret)
               if(iret.eq.0) then
c     they are equal
                  equivto(lreg)=lregpr
                  equivcoef(lreg)=1
               elseif(iret.eq.1) then
c     they are proportional
                  equivto(lreg)=lregpr
                  equivcoef(lreg)=cprop
               endif
            enddo
            call randomrestore
         endif
         ini=.false.
      endif
c End initialization phase; compute graphs
      call pdfcall(1,kn_x1,pdf1)
      call pdfcall(2,kn_x2,pdf2)
      do lreg=1,flst_nregular
c ----------------
         if(equivto(lreg).lt.0) then
            call realgr(flst_regular(1,lreg),kn_cmpreal,r0(lreg))
         else
            r0(lreg)=r0(equivto(lreg))*equivcoef(lreg)
         endif
      enddo
      sig=0
      do lreg=1,flst_nregular
         r0(lreg)=xjac*r0(lreg)*
     #      pdf1(flst_regular(1,lreg))*pdf2(flst_regular(2,lreg))
         sig=sig+r0(lreg)
      enddo
      end


      subroutine compare_vecs_reg(nmomset,lreg,res,lregpr,cprop,iret)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      real * 8 ep
      parameter (ep=1d-12)
      integer nmomset,lreg,lregpr,iret,j,k
      real * 8 res(nmomset,*),cprop,rat
      do j=1,lreg-1
         rat=res(1,lreg)/res(1,j)
         do k=1,nmomset
            if(abs(1-res(k,lreg)/res(k,j)/rat).gt.ep) goto 10
         enddo
         if(abs(1-rat).lt.ep) then
            iret=0
            cprop=1
         else
            iret=1
            cprop=rat
         endif
         lregpr=j
         return
 10      continue
      enddo
      iret=-1
      end


      subroutine sigreal_damp_rem(xjac,sig,r0)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_flg.h'
      real * 8 xjac,sig
      real * 8 r0(maxalr),rc(maxalr),rs(maxalr)
      integer alr,alrpr,iret,em
      integer nmomset
      parameter (nmomset=10)
      real * 8 res(nmomset,maxalr),preal(0:3,nlegreal,nmomset),cprop
      integer equivto(maxalr)
      real * 8 equivcoef(maxalr)
      integer k,j
      real * 8 r,sumdijinv,dampfac
      real * 8 pdf1(-6:6),pdf2(-6:6)
      real * 8 ptsq,pwhg_pt2
      logical ini
      data ini/.true./
      save ini,equivto,equivcoef
      external pwhg_pt2
      if(ini) then
         do alr=1,flst_nalr
            equivto(alr)=-1
         enddo
         if(flg_smartsig) then
            call randomsave
c     generate "nmomset" random real-phase space configurations
            call fillmomenta(nlegreal,nmomset,kn_masses,preal)
            do alr=1,flst_nalr
               do j=1,nmomset
                  call realgr(
     1                 flst_alr(1,alr),preal(0,1,j),res(j,alr))
               enddo
               call compare_vecs_rad(nmomset,alr,res,alrpr,cprop,iret)
               if(iret.eq.0) then
c     they are equal
                  equivto(alr)=alrpr
                  equivcoef(alr)=1
               elseif(iret.eq.1) then
c     they are proportional
                  equivto(alr)=alrpr
                  equivcoef(alr)=cprop
               endif
            enddo
            call randomrestore
         endif
         ini=.false.
      endif
c End initialization phase; compute graphs
      call pdfcall(1,kn_x1,pdf1)
      call pdfcall(2,kn_x2,pdf2)
      if(flg_withdamp) then
         call collbtl(rc)
         call softbtl(rs)
      endif
      sig=0
      do alr=1,flst_nalr
         em=flst_emitter(alr)
         if(em.eq.kn_emitter) then
c check if we have a g -> Q Qbar splitting below threshold:
c            if(flst_alr(em,alr)+flst_alr(nlegreal,alr).eq.0.and.
c     #        abs(flst_alr(em,alr)).ge.4) then
c               ptsq=pwhg_pt2()
c               if(abs(flst_alr(em,alr)).eq.4.and.ptsq.lt.rad_charmthr2
c     #                                 .or.
c     #       abs(flst_alr(em,alr)).eq.5.and.ptsq.lt.rad_bottomthr2) then
c                  goto 995
c               endif
c            endif
c ----------------
            if(equivto(alr).lt.0) then
               call realgr(flst_alr(1,alr),kn_cmpreal,r0(alr))
               sumdijinv=0
               do k=1,flst_allreg(1,0,alr)
                  sumdijinv=sumdijinv
     #+1/kn_dijterm(flst_allreg(1,k,alr),flst_allreg(2,k,alr))
               enddo
               r0(alr)=r0(alr)/kn_dijterm(em,nlegreal)/sumdijinv
c If the emitter is in the final state, and if the emitted and emitter
c are both gluons, supply a factor E_em/(E_em+E_rad) * 2
               if(em.gt.2.and.flst_alr(em,alr).eq.0.and.
     #              flst_alr(nlegreal,alr).eq.0) then
                  r0(alr)=r0(alr)*2
     #         *kn_cmpreal(0,em)/
     #         (kn_cmpreal(0,em)+kn_cmpreal(0,nlegreal))
               endif
               r0(alr)=r0(alr)*flst_mult(alr)
c supply Born zero damping factor, if required
               if(flg_withdamp) then
                  if(kn_emitter.gt.2) then
                     r=r0(alr)*(1-kn_y)*kn_csi**2
                  else
                     r=r0(alr)*(1-kn_y**2)*kn_csi**2
                  endif
                  call bornzerodamp(alr,r,rc(alr),rs(alr),dampfac)
c                  if(kn_y.gt.0.99.and.kn_emitter.eq.1) then
c                     write(*,*) alr,kn_emitter,kn_y,r,rc(alr),
c     #                  kn_csi,kn_azi
c                  endif
                  r0(alr)=r0(alr) * (1-dampfac)
               endif
            else
               r0(alr)=r0(equivto(alr))*equivcoef(alr)
            endif
         endif
c 995     continue
      enddo
      do alr=1,flst_nalr
         em=flst_emitter(alr)
         if(em.eq.kn_emitter) then
            r0(alr)=xjac*r0(alr)
     #         *pdf1(flst_alr(1,alr))*pdf2(flst_alr(2,alr))
            sig=sig+r0(alr)
         endif
      enddo
      end

      
