      subroutine pwhgevent
      implicit none
      include 'include/pwhg_math.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/LesHouches.h'
      integer iret,iun
      real * 8 suppfact
      real * 8 random,powheginput
      external random,powheginput
      integer mcalls,icalls
      real * 8 pwhg_pt2,pt2max_regular
      external pwhg_pt2,pt2max_regular
      integer i
c     store current random seeds. To be used to restart at problematic events
      call savecurrentrandom
      if(random().gt.rad_sigrm/rad_sigtotgen) then
c     generate underlying Born kinematics
         call gen_btilde(mcalls,icalls)
c     generate underlying Born flavour
         call gen_uborn_idx
c
         if(powheginput('#testsuda').eq.1) then
            call testsuda
         endif
c generate radiation
         call gen_radiation
c add a random azimuthal rotation around beam axis
         call add_azimuth
c --- set up les houches interface
         rad_pt2max=pwhg_pt2()
         call gen_leshouches
c if negative weight, flip the sign of xwgtup
         if(rad_btilde_sign(rad_ubornidx).eq.-1) then
            xwgtup=-xwgtup
         endif
c rad_type=1 for btilde events (used only for debugging purposes)
         rad_type=1
         call born_suppression(suppfact)
         if(suppfact.eq.0) then
            write(*,*) ' 0 suppression factor in event generation'
            write(*,*) ' aborting'
            call exit(-1)
         endif
         xwgtup=xwgtup/suppfact
      else
c generate remnant n+1 body cross section
         call gen_sigremnant
c pick a configuration according to its cross section
c iret=1: rem contribution (leftover from damping factor on R)
c iret=2: reg contribution (real graphs without singular regions)
         call gen_remnant(iret)
c         if (pwhg_pt2().lt.rad_ptsqmin) then
c            write(*,*) '****************************************'
c            write(*,*) 'WARNING in gen_remnant'
c            write(*,*) 'pwhg_pt2 < rad_ptsqmin ',
c     #           pwhg_pt2(),' < ',rad_ptsqmin
c            write(*,*) (flst_alr(i,rad_realalr),i=1,nlegreal)
c            write(*,*) 'To generate this event, use the following seeds'
c            call printcurrentrandom
c            write(*,*) '****************************************'
c         endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         call add_azimuth
         if(iret.eq.1) then
c     set st_muren2 equal to pt2 for scalup value
            rad_pt2max=max(rad_ptsqmin,pwhg_pt2())
            call set_rad_scales(rad_pt2max)
            call gen_leshouches
            call born_suppression(suppfact)
            if(suppfact.eq.0) then
               write(*,*) ' 0 suppression factor in event generation'
               write(*,*) ' aborting'
               call exit(-1)
            endif
            xwgtup=xwgtup/suppfact
c     rad_type=2 for remnants
            rad_type=2
         else
c     set st_muren2 for scalup value for regular contributions
            rad_pt2max=max(rad_ptsqmin,pt2max_regular())
            call set_rad_scales(rad_pt2max)
            call gen_leshouches_reg
c rad_type=3 for regular contributions
            rad_type=3
         endif         
      endif
      end


      subroutine gen_radiation
      implicit none
      include 'include/pwhg_math.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      real * 8 t,csi,y,azi,sig
      real * 8 tmax
      common/ctmax/tmax
      integer kinreg,firstreg,lastreg
      logical ini
      data ini/.true./
      real * 8 pwhg_pt2,powheginput
      external pwhg_pt2,powheginput
      save ini,firstreg,lastreg
      if(ini) then
         firstreg=powheginput("#radregion")
         if(firstreg.le.0) then
            firstreg=1
            lastreg=rad_nkinreg
         else
            lastreg=firstreg
         endif
         ini=.false.
      endif
c Use highest bid procedure (see appendix B of FNO2006)
      tmax=0
      kinreg=0
      do rad_kinreg=firstreg,lastreg
         if(rad_kinreg_on(rad_kinreg)) then
            if(rad_kinreg.eq.1) then
c     initial state radiation
               call gen_rad_isr(t)
            else
c     final state radiation
               call gen_rad_fsr(t)
            endif
            if(t.gt.tmax) then
               tmax=t
               kinreg=rad_kinreg
               csi=kn_csi
               y=kn_y
               azi=kn_azi
            endif
         endif
      enddo
c Set up radiation kinematics
      if(tmax.eq.0) then
c Generate a Born like event
         kn_csi=0
         return
      else         
         rad_kinreg=kinreg
         kn_csi=csi
         kn_y=y
         kn_azi=azi
         t=tmax
         if(rad_kinreg.eq.1) then
            call gen_real_phsp_isr_rad
         else
            call gen_real_phsp_fsr_rad
         endif
         call set_rad_scales(t)
         call sigreal_rad(sig)
         call gen_real_idx
      endif
      end



      function pwhg_pt2()
      implicit none
      real * 8 pwhg_pt2
      include 'include/pwhg_math.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      if(rad_kinreg.eq.1) then
         pwhg_pt2=(kn_sreal/4)*(1-kn_y**2)*kn_csi**2
      else
         pwhg_pt2=(kn_sreal/2)*(1-kn_y)*kn_csi**2
      endif
      end



      function pwhg_upperb_rad()
      implicit none
      real * 8 pwhg_upperb_rad
      include 'include/pwhg_math.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_st.h'
      integer iupperisr,iupperfsr
      common/cupper/iupperisr,iupperfsr
      real * 8 x,y,csi
      csi=kn_csi
      x=1-csi
      y=kn_y
      if(rad_kinreg.eq.1) then
         if(iupperisr.eq.1) then
            pwhg_upperb_rad = st_alpha/((1-x)*(1-y**2))
c Possible alternatives:
c iupper=2   pwhg_upperb_rad = st_alpha/(x*(1-x)*(1-y**2))
c 
c iupper=3:  pwhg_upperb_rad = st_alpha/(x**2*(1-x)*(1-y**2))
         else
            write(*,*) ' iupper=',iupperisr,
     1        'alternative not implemented'
            stop
         endif
      else
c for now use the same
         if(iupperfsr.eq.1) then
            pwhg_upperb_rad = st_alpha/(csi*(1-y))
         elseif(iupperfsr.eq.2) then
            pwhg_upperb_rad = st_alpha/(csi**2*(1-y)*(1-csi/2*(1-y))**2)
     2          *csi
         elseif(iupperfsr.eq.3) then
            pwhg_upperb_rad = st_alpha/(csi*(1-y)*
     2         (1-csi/2*(1-y)))
         else
            write(*,*) ' iupper=',iupperfsr,
     1            'alternative not implemented'
            stop
         endif
      endif
      end



      function pt2solve(pt2,i)
c Returns  xlr - log(Delta^{(tilde{V})}) , see eq. D14, D15 in ZZ paper
c We use it to find its zero in pt2.
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_math.h'
      real * 8 pt2solve,pt2
c i set by dzero: 1 for first call, 2 for subsequent calls, 3 for last call
c before a normal exit; not used here
      integer i
      real * 8 xlr,q2,xlam2c,kt2max,cunorm
      integer nlc
      common/cpt2solve/xlr,q2,kt2max,xlam2c,cunorm,nlc
      integer iupperisr,iupperfsr
      common/cupper/iupperisr,iupperfsr
      real * 8 b0,sborn,xm,p
      sborn=kn_sborn
      b0=(11*CA-4*TF*nlc)/(12*pi)
      if(rad_kinreg.eq.1) then
         if(iupperisr.eq.1) then
c see Notes/upperbounding-isr.pdf
            if(pt2.lt.sborn) then
               if(sborn.lt.kt2max) then
                  pt2solve=cunorm*pi/b0*(
     #      (log(2*sborn/xlam2c)*log(log(sborn/xlam2c)/log(pt2/xlam2c))
     #        - log(sborn/pt2)) +
     #           log(2d0)*log(log(kt2max/xlam2c)/log(sborn/xlam2c)))
     #           + xlr
               else
                  pt2solve=cunorm*pi/b0*(
     #     (log(2*sborn/xlam2c)*log(log(kt2max/xlam2c)/log(pt2/xlam2c))
     #        - log(kt2max/pt2)) )
     #           + xlr
               endif
            else
               pt2solve=cunorm*pi/b0*(log(2d0)
     #           *log(log(kt2max/xlam2c)/log(pt2/xlam2c)))
     #           + xlr
            endif
         else
            write(*,*) ' iupper=',iupperisr,' not implemented'
            stop
c Alternatives: iupper=2
c         pt2solve=cunorm*pi/b0/2
c     #        *(log(q2/xlam2c)*log(log(kt2max/xlam2c)/log(pt2/xlam2c))
c     #        - log(kt2max/pt2)) + xlr
         endif
      else
         if(iupperfsr.eq.1) then
c final state radiation
         pt2solve=cunorm*pi/b0*(
     #     (log(kt2max/xlam2c)*log(log(kt2max/xlam2c)/log(pt2/xlam2c))
     #        - log(kt2max/pt2)) )
     #           + xlr
         elseif(iupperfsr.eq.2) then
            xm=kn_csimax
            p=sqrt(pt2/sborn)
            pt2solve=cunorm*2*pi*2*(
     3   (log(xm-xm**2)+(2*xm-2)*log(xm)-2*log(1-xm)*xm-2)/xm/2.d+0
     1   -(p*log(xm-p**2)+(2*p*log(p)-2*log(1-p)*p-2)*xm-2*p*log(p))
     2   /(p*xm)/2.d+0) + xlr
         elseif(iupperfsr.eq.3) then
            xm=kn_csimax
            p=sqrt(pt2/sborn)
            pt2solve=cunorm*2*pi*2*(
     3   (log(xm-xm**2)+(2*xm-2)*log(xm)-2*log(1-xm)*xm-2)/xm/2.d+0
     1   -(p*log(xm-p**2)+(2*p*log(p)-2*log(1-p)*p-2)*xm-2*p*log(p))
     2   /(p*xm)/2.d+0) + xlr
         else
            write(*,*) ' iupper=',iupperfsr,' not implemented'
            stop
         endif            
      endif
      end

      subroutine gen_rad_isr(t)
c Generates hard radiation kinematics according to
c appendix D in ZZ paper.
c
c  common/cptmin/ptminsq: minimum pt^2 accepted
c 
      implicit none
      include 'include/pwhg_math.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_st.h'
      real * 8 t
      real * 8 x,y,x1b,x2b
      real * 8 xlr,q2,xlam2c,kt2max,unorm
      integer nlc
      common/cpt2solve/xlr,q2,kt2max,xlam2c,unorm,nlc
      integer iupperisr,iupperfsr
      common/cupper/iupperisr,iupperfsr
      real * 8 xmin,rv,xp,xm,chi,tk,uk,ubound,ufct,
     #   sborn,value,err,tmp1,tmp2,tmp,rvalue,born,sig
      common/cdfxmin/xmin
      real * 8 tmax
      common/ctmax/tmax
      real * 8 random,pt2solve,dfxmin,pwhg_alphas0,pwhg_upperb_rad
      external random,pt2solve,dfxmin,pwhg_alphas0,pwhg_upperb_rad
      unorm=rad_norms(rad_kinreg,rad_ubornidx)
      sborn=kn_sborn
      x1b=kn_xb1
      x2b=kn_xb2
c See Notes/kt2max.pdf
      kt2max = sborn*(1-x2b**2)*(1-x1b**2)/(x1b+x2b)**2
      if(kt2max.lt.rad_ptsqmin.or.kt2max.lt.tmax) then
         t=-1
         goto 3
      endif
c upper bound is log(q2/t)
      if(iupperisr.eq.1) then
         q2=2*sborn
      elseif(iupperisr.eq.2) then
         write(*,*) ' iupper=',iupperisr,' not implemented'
         stop
c Alternative iupper=2
c         q2=4*sborn/min(x1b,x2b)**2
      endif
c see section 4 in ZZ paper, last paragraph
      xlam2c=rad_lamll**2
      nlc=5
      xlr=0
 1    continue
      xlr=xlr+log(random())
c CERNLIB voodoo:
      call KERSET('C205. ',0,0,101)
c solve for zero of pt2solve
c dzero(xmin,xmax,x,err,eps,maxcalls,function)
c err: on exit if no error occours: |y-y0|<err 
c      error C205.1 function(xmin)*function(xmax)>0,
c                   x=0 and r=-2(ymax-ymin)
c      error C205.2 Number of calls to F exceeds maxcalls,
c                   x=0 and r=-(xmax-xmin)/2
c eps: required accuracy
      call dzero(rad_ptsqmin,kt2max,t,err,1d-8,1000000,pt2solve)
c error conditions
      if(t.eq.0.and.err.lt.0d0
     # .and.err.gt.rad_ptsqmin-kt2max) then
         write(*,*) 'DZERO fails'
         write(*,*) ' number of calls exceeded'
         stop
      endif
 3    if(t.lt.rad_ptsqmin.or.t.lt.tmax) then
c below cut (either below absolute minimum, or below previously generated
c radiation in highest bid loop): generate a born event
         t=-1
         kn_csi=0
         return
      endif
c vetoes:
      rv=random()
      xp=(sqrt(1+t/sborn)+sqrt(t/sborn))**2
      xm=(sqrt(1+t/sborn)-sqrt(t/sborn))**2
c tmp1: V(t)/tilde{V}(t) in appendix D of ZZ paper;
c (typo: in D.13, log log -> log
      xmin=min(x1b,x2b)/(2*sqrt(1+t/sborn))
      if(iupperisr.eq.1) then
         tmp1=log((sqrt(xp-xmin)+sqrt(xm-xmin))
     #       /(sqrt(xp-xmin)-sqrt(xm-xmin)))
         if(t.lt.sborn) then
            tmp1=tmp1/(log(2*sborn/t)/2)
         else
            tmp1=tmp1/(log(2d0)/2)
         endif
      elseif(iupperisr.eq.2) then
         tmp1=log(2/xmin*(sqrt((xp-xmin)*(xm-xmin))
     # +1-xmin/2*(xp+xm))/(xp-xm)) /(log(q2/t)/2)
      endif
c compare with D.11-D.12
c to set xmuren2:
      call set_rad_scales(t)
      tmp2=st_alpha / pwhg_alphas0(t,rad_lamll,nlc)
      tmp=tmp1*tmp2
      if(tmp.gt.1) then
         write(*,*) ' errore: maggiorante non maggiora',
     #        tmp,tmp1,tmp2,t
         stop
      endif
      if(rv.gt.tmp) then
         goto 1
      endif
c At this stage: pt generated according to D.2
c generate x proportional to 1/(x sqrt((xp-x)*(xm-x)))
c in the range xmin < x < xm (cf. D.5)
c Generate in d sqrt(xm-x) /sqrt(xp-x)  (iupper=1) or d sqrt(xm-x) /(x sqrt(xp-x)) (iupper=2)
c using       d sqrt(xm-x) /sqrt(xp-xm) (iupper=1) or d sqrt(xm-x) /(xmin sqrt(xp-xm)) (iupper=2) as upper bound using hit and miss
 2    chi=sqrt(xm-xmin)*random()
      x=xm-chi**2
      if(iupperisr.eq.1) then
         if(random().gt.sqrt(xp-xm)/sqrt(xp-x)) goto 2
      elseif(iupperisr.eq.2) then
         if(random().gt.(xmin*sqrt(xp-xm))/(x*sqrt(xp-x))) goto 2
      endif
c get y (abs to avoid tiny negative values)
      y=sqrt(abs(1-4*x/(1-x)**2*t/sborn))
      if(random().gt.0.5d0) y=-y
c At this point an x-y pair is generated according to the
c distribution upper().
c
c Veto if out of range (x1>1 or x2>1)
      tk=-1d0/2*(1-x)*(1-y)
      uk=-1d0/2*(1-x)*(1+y)
      if(   x1b*sqrt((1+tk)/(1+uk)/x) .gt. 1
     # .or. x2b*sqrt((1+uk)/(1+tk)/x) .gt. 1) then
         goto 1
      endif
c extra suppression factor of upper bounding function (may depend upon radiation variables)
      call uboundfct(ufct,1-x,y)
      if(random().gt.ufct) goto 1
c Veto from upper bound to real value. Count how many vetoes,
c since these may be expensive.
      call sigborn_rad(born)
      if(born.lt.0) then
         born=0
      endif
      if(born.eq.0) then
c bizarre situation that may arise when the scale gets so low
c that some pdf vanish (typically heavy flavour pdf's)
         t=-1
         goto 3
      endif
      kn_y=y
      kn_csi=1-x
      kn_azi=2*pi*random()
      ubound=born*pwhg_upperb_rad()*unorm*ufct
      call gen_real_phsp_isr_rad
      call sigreal_rad(sig)
      value=sig*kn_jacreal
      if(value.gt.ubound) then
         call increasecnt(
     # 'upper bound failures in generation of radiation')
      endif
      rvalue=random()*ubound
      if(rvalue.gt.value) then
         call increasecnt('vetoed radiation')
         goto 1
      endif
      end


      subroutine gen_rad_fsr(t)
c Generates final state hard radiation kinematics according to
c Notes/upperbounding-fsr.pdf
c 
      implicit none
      include 'include/pwhg_math.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_st.h'
      real * 8 t
      real * 8 csi,y
      real * 8 xlr,q2,xlam2c,kt2max,unorm
      integer nlc
      common/cpt2solve/xlr,q2,kt2max,xlam2c,unorm,nlc
      integer iupperisr,iupperfsr
      common/cupper/iupperisr,iupperfsr
      real * 8 xmin,rv,ubound,ufct,
     #   s,value,err,tmp,rvalue,born,sig
      common/cdfxmin/xmin
      real * 8 tmax
      common/ctmax/tmax
      real * 8 random,pt2solve,pwhg_alphas0,pwhg_upperb_rad
      external random,pt2solve,pwhg_alphas0,pwhg_upperb_rad
      unorm=rad_norms(rad_kinreg,rad_ubornidx)
c kn_sborn=kn_sreal:
      s=kn_sborn
      kn_emitter=flst_lightpart+rad_kinreg-2
      kn_csimax=kn_csimax_arr(kn_emitter)
c See Notes/kt2max.pdf
      kt2max = kn_csimax**2*s
      if(kt2max.lt.rad_ptsqmin.or.kt2max.lt.tmax) then
         t=-1
         goto 3
      endif
c see section 4 in ZZ paper, last paragraph
      xlam2c=rad_lamll**2
      nlc=5
      xlr=0
 1    continue
      xlr=xlr+log(random())
c CERNLIB voodoo:
      call KERSET('C205. ',0,0,101)
c solve for zero of pt2solve
c dzero(xmin,xmax,x,err,eps,maxcalls,function)
c err: on exit if no error occours: |y-y0|<err 
c      error C205.1 function(xmin)*function(xmax)>0,
c                   x=0 and r=-2(ymax-ymin)
c      error C205.2 Number of calls to F exceeds maxcalls,
c                   x=0 and r=-(xmax-xmin)/2
c eps: required accuracy
      call dzero(rad_ptsqmin,kt2max,t,err,1d-8,1000000,pt2solve)
c error conditions
      if(t.eq.0.and.err.lt.0d0
     # .and.err.gt.rad_ptsqmin-kt2max) then
         write(*,*) 'DZERO fails'
         write(*,*) ' number of calls exceeded'
         stop
      endif
 3    if(t.lt.rad_ptsqmin.or.t.lt.tmax) then
c below cut (either below absolute minimum, or below previously generated
c radiation in highest bid loop): generate a born event
         t=-1
         kn_csi=0
         return
      endif
c vetoes:
      rv=random()
      call set_rad_scales(t)
      if(iupperfsr.eq.1) then
         tmp=st_alpha / pwhg_alphas0(t,rad_lamll,nlc)
      elseif(iupperfsr.eq.2) then
         tmp=st_alpha
      elseif(iupperfsr.eq.3) then
         tmp=st_alpha
      endif
      if(tmp.gt.1) then
         write(*,*) ' errore: maggiorante non maggiora',
     #        tmp,t
         stop
      endif
      if(rv.gt.tmp) then
         goto 1
      endif
      if(iupperfsr.eq.1) then         
c At this stage: pt generated according to (1) of upperbounding-fsr.pdf;
c generate csi uniformly in 1/csi
c in the range t/s < csi^2 < csimax^2
         rv=random()
         csi=exp(rv*log(t/s)/2+(1-rv)*log(kn_csimax))
c get y
         y=1-2*t/(s*csi**2)
c At this point a csi-y pair is generated according to the
c distribution upper(). It is automatically within range.
      elseif(iupperfsr.eq.2) then
c     csi distributed uniformly in 1/(csi-t/s)
         rv=random()
         csi=1/(rv/(sqrt(t/s)-t/s)+(1-rv)/(kn_csimax-t/s))+t/s
c extra csi dependent factor
         if(random().gt.csi) goto 1
c get y
         y=1-2*t/(s*csi**2)
c At this point a csi-y pair is generated according to the
c distribution upper(). It is automatically within range.
      elseif(iupperfsr.eq.3) then
c     csi distributed uniformly in 1/(csi-t/s)
         rv=random()
         csi=1/(rv/(sqrt(t/s)-t/s)+(1-rv)/(kn_csimax-t/s))+t/s
c get y
         y=1-2*t/(s*csi**2)
         if(random().gt.(csi-t/s)) goto 1
      else
         write(*,*) ' gen_rad_fsr:  iupper=',iupperfsr,' invalid'
      endif
c
c extra suppression factor of upper bounding function (may depend upon radiation variables)
      call uboundfct(ufct,csi,y)
      if(random().gt.ufct) goto 1
c Veto from upper bound to real value. Count how many vetoes,
c since these may be expensive.
c      write(*,*) ' genrad_fsr: y and csi ',y,csi
      call sigborn_rad(born)
      if(born.lt.0) then
         born=0
      endif
      if(born.eq.0) then
c bizarre situation that may arise when the scale gets so low
c that some pdf vanish (typically heavy flavour pdf's)
         t=-1
         goto 3
      endif
      kn_y=y
      kn_csi=csi
      kn_azi=2*pi*random()
      ubound=born*pwhg_upperb_rad()*unorm*ufct
      call gen_real_phsp_fsr_rad
      call sigreal_rad(sig)
      value=sig*kn_jacreal
      if(value.gt.ubound) then
         call increasecnt(
     # 'upper bound failures in generation of radiation')
      endif
      rvalue=random()*ubound
      if(rvalue.gt.value) then
         call increasecnt('vetoed radiation')
         goto 1
      endif
      end


      subroutine add_azimuth
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_math.h'
      include 'include/pwhg_kn.h'
      integer ileg
      real * 8 azi,sazi,cazi
      real * 8 dir(3)
      data dir/0d0,0d0,1d0/
      real * 8 random
      external random
      azi=2d0*pi*random()
      sazi = sin(azi)
      cazi = cos(azi)
      if (kn_csi.eq.0d0) then
         do ileg=1, nlegborn
            call mrotate(dir,sazi,cazi,kn_pborn(1,ileg))
         enddo
      else
         do ileg=1, nlegreal
            call mrotate(dir,sazi,cazi,kn_preal(1,ileg))
         enddo
      endif
      end
      
      
