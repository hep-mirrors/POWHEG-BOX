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
      real * 8 xborn(ndiminteg-3)
      integer j,alr
      real * 8 ttt
      real * 8 jac_over_csi,jac_over_csi_p,jac_over_csi_m,
     #         jac_over_csi_s,jac_over_csi_coll
      real * 8 xjac,suppfact,totrem,totreg
      logical valid_emitter
      external valid_emitter
      if(ifirst.eq.2) then
         call addupweightsrmn(sigremnant)
         if(flg_nlotest) call pwhgaccumup
         return
      endif
      do j=1,ndiminteg-3
         xborn(j)=xx(j)
      enddo
      do j=1,3
         xrad(j)=xx(ndiminteg-3 + j)
         rad_xradremn(j)=xrad(j)
      enddo
c regular contributions; any phase space parametrization should be OK
      kn_emitter=0
      call gen_born_phsp(xborn)
c set scales
      call setscalesbtilde
c the following is needed to compute soft and collinear limits
      call allborn
      call born_suppression(suppfact)
      totreg=0
      if(flg_withreg) then
c         call gen_real_phsp_isr(xrad,
c     #   jac_over_csi,jac_over_csi_p,jac_over_csi_m,jac_over_csi_s)
c          xjac=jac_over_csi*kn_csi*kn_csimax*kn_jacborn*suppfact*ww*hc2
         call phasespaceregular(xx)
         xjac=kn_jacborn*ww*hc2
         call sigreal_reg(xjac,totreg,rad_reg_arr)
         call transfersign(rad_reg_arr,rad_reg_sign,flst_nregular)
         if(flg_nlotest) then
            call analysis_driver(totreg,1)
         endif
      endif
      totrem=0
      if(flg_withdamp) then
         do alr=1,flst_nalr
            rad_damp_rem_arr(alr)=0
         enddo
         do kn_emitter=0,nlegborn
            if(valid_emitter(kn_emitter)) then
               if(kn_emitter.le.2) then
c     No need to generate phase space; it is already available
                  call gen_real_phsp_isr(xrad,
     #   jac_over_csi,jac_over_csi_p,jac_over_csi_m,jac_over_csi_s)
          xjac=jac_over_csi*kn_csi*kn_csimax*kn_jacborn*suppfact*ww*hc2
                  call sigreal_damp_rem(xjac,ttt,rad_damp_rem_arr)
                  if(flg_nlotest) then
                     call analysis_driver(ttt/suppfact,1)
                  endif
               else
                  call gen_real_phsp_fsr(xrad,
     #jac_over_csi,jac_over_csi_coll,jac_over_csi_s)
                  xjac=jac_over_csi*kn_csi*kn_csimax
     #                *kn_jacborn*suppfact*ww*hc2
                  call sigreal_damp_rem(xjac,ttt,rad_damp_rem_arr)
                  if(flg_nlotest) then
                     call analysis_driver(ttt/suppfact,1)
                  endif
               endif
               totrem=totrem+ttt
            endif
         enddo
         call transfersign(rad_damp_rem_arr,rad_damp_rem_sign,
     1        flst_nalr)
      endif
      sigremnant=totrem+totreg
      end

      subroutine transfersign(arr,intarr,n)
      implicit none
      integer n
      real * 8 arr(n)
      integer intarr(n)
      integer j
      do j=1,n
         if(arr(j).lt.0) then
            arr(j)=-arr(j)
            intarr(j)=-1
         else
            intarr(j)=1
         endif
      enddo
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

      subroutine sigreal_damp_rem(xjac,sig,r1)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      real * 8 xjac,sig
      real * 8 r0(maxalr),r1(maxalr)
      integer alr
      sig=0
      call sigreal_btl0(r0,1)
      do alr=1,flst_nalr
         if(kn_emitter.eq.flst_emitter(alr)) then
            if(kn_emitter.le.2) then
               r0(alr)=r0(alr)/((1-kn_y**2)*kn_csi**2)
            else
               r0(alr)=r0(alr)/((1-kn_y)*kn_csi**2)
            endif
            r0(alr)=r0(alr)*xjac
            r1(alr)=r1(alr)+r0(alr)
            sig=sig+r0(alr)
         endif
      enddo
      end

      


      subroutine addupweightsrmn(sigremnant)
      implicit none
      real * 8 sigremnant
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_rad.h'
      real * 8 dtotreg,dtotabsreg,dtotrem,dtotabsrem,
     1     dtotposreg,dtotnegreg,dtotposrem,dtotnegrem
      real * 8 totreg,etotreg,totabsreg,etotabsreg,
     1         totposreg,etotposreg,totnegreg,etotnegreg
      real * 8 totrem,etotrem,totabsrem,etotabsrem,
     1         totposrem,etotposrem,totnegrem,etotnegrem
      integer n
      common/cadduptotalsrmn/totreg,etotreg,totabsreg,etotabsreg,
     1     totposreg,etotposreg,totnegreg,etotnegreg,
     2     totrem,etotrem,totabsrem,etotabsrem,
     3     totposrem,etotposrem,totnegrem,etotnegrem,n
      integer j
      n=n+1
      dtotreg=0
      dtotabsreg=0
      dtotposreg=0
      dtotnegreg=0
      dtotrem=0
      dtotabsrem=0
      dtotposrem=0
      dtotnegrem=0
      do j=1,flst_nregular
         dtotreg=dtotreg+rad_reg_arr(j)*rad_reg_sign(j)
         dtotabsreg=dtotabsreg+rad_reg_arr(j)
         if(rad_reg_sign(j).eq.1) then
            dtotposreg=dtotposreg+rad_reg_arr(j)
         else
            dtotnegreg=dtotnegreg+rad_reg_arr(j)
         endif
      enddo
      do j=1,flst_nalr
         dtotrem=dtotrem+rad_damp_rem_arr(j)*rad_damp_rem_sign(j)
         dtotabsrem=dtotabsrem+rad_damp_rem_arr(j)
         if(rad_damp_rem_sign(j).eq.1) then
            dtotposrem=dtotposrem+rad_damp_rem_arr(j)
         else
            dtotnegrem=dtotnegrem+rad_damp_rem_arr(j)
         endif
      enddo
      totreg=totreg+dtotreg
      etotreg=etotreg+dtotreg**2
      totabsreg=totabsreg+dtotabsreg
      etotabsreg=etotabsreg+dtotabsreg**2
      totposreg=totposreg+dtotposreg
      etotposreg=etotposreg+dtotposreg**2
      totnegreg=totnegreg+dtotnegreg
      etotnegreg=etotnegreg+dtotnegreg**2
c      
      totrem=totrem+dtotrem
      etotrem=etotrem+dtotrem**2
      totabsrem=totabsrem+dtotabsrem
      etotabsrem=etotabsrem+dtotabsrem**2
      totposrem=totposrem+dtotposrem
      etotposrem=etotposrem+dtotposrem**2
      totnegrem=totnegrem+dtotnegrem
      etotnegrem=etotnegrem+dtotnegrem**2
c
      sigremnant=dtotabsreg+dtotabsrem
      end


      subroutine resettotalsrmn
      implicit none
      real * 8 totreg,etotreg,totabsreg,etotabsreg,
     1     totposreg,etotposreg,totnegreg,etotnegreg
      real * 8 totrem,etotrem,totabsrem,etotabsrem,
     1     totposrem,etotposrem,totnegrem,etotnegrem
      integer n
      common/cadduptotalsrmn/totreg,etotreg,totabsreg,etotabsreg,
     1     totposreg,etotposreg,totnegreg,etotnegreg,
     2     totrem,etotrem,totabsrem,etotabsrem,
     3     totposrem,etotposrem,totnegrem,etotnegrem,n
      n=0
      totreg=0
      etotreg=0
      totabsreg=0
      etotabsreg=0
      totposreg=0
      etotposreg=0
      totnegreg=0
      etotnegreg=0
      totrem=0
      etotrem=0
      totabsrem=0
      etotabsrem=0
      totposrem=0
      etotposrem=0
      totnegrem=0
      etotnegrem=0
      end

      subroutine finaltotalsrmn
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_rad.h'
      real * 8 totreg,etotreg,totabsreg,etotabsreg,
     1         totposreg,etotposreg,totnegreg,etotnegreg
      real * 8 totrem,etotrem,totabsrem,etotabsrem,
     1         totposrem,etotposrem,totnegrem,etotnegrem
      integer n
      common/cadduptotalsrmn/totreg,etotreg,totabsreg,etotabsreg,
     1     totposreg,etotposreg,totnegreg,etotnegreg,
     2     totrem,etotrem,totabsrem,etotabsrem,
     3     totposrem,etotposrem,totnegrem,etotnegrem,n
      real * 8 merr,t,e
      merr(t,e)=sqrt((e/n-(t/n)**2)/n)
      rad_totreg=totreg/n
      rad_etotreg=merr(totreg,etotreg)
      rad_totabsreg=totabsreg/n
      rad_etotabsreg=merr(totabsreg,etotabsreg)
      rad_totposreg=totposreg/n
      rad_etotposreg=merr(totposreg,etotposreg)
      rad_totnegreg=totnegreg/n
      rad_etotnegreg=merr(totnegreg,etotnegreg)
      rad_totrem=totrem/n
      rad_etotrem=merr(totrem,etotrem)
      rad_totabsrem=totabsrem/n
      rad_etotabsrem=merr(totabsrem,etotabsrem)
      rad_totposrem=totposrem/n
      rad_etotposrem=merr(totposrem,etotposrem)
      rad_totnegrem=totnegrem/n
      rad_etotnegrem=merr(totnegrem,etotnegrem)
      end

