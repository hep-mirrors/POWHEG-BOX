!!!!!!!!!ER:
c     Questa e' la versione migliore. Fa uso della variabile genrad
!!!!!!!!!

!!!!!!!!!ER:
c     attenzione: quando il reale e' negativo, in gen_rad_isr sigreal_rad
c     torna output negativo. In quel caso radiazione sempre vetata.
!!!!!!!!!!

!!!!!!!!!ER:
c     Attenzione, per qualche ragione anche i remnant SONO NEGATIVI.
c     Lo sono perche' i remnant sono i processi di tipo qqbar -> Wt qbar',
c     con qbar' quark di tipo b MA DIVERSO da q. Questi processi non hanno
c     underlying Born (non sono singolari), ma hanno uno dei due grafici che
c     e' double-resonant (un tbar interno puo' andare onshell).
c     Quindi necessitano di sottrazione 'DS', che non garantisce la positivita'.
!!!!!!!!!!!!

      subroutine setreal(p,rflav,amp2)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_kn.h'
      include 'PhysPars.h'
      logical genrad
      common/cgenrad/genrad

      real * 8 p(0:3,nlegreal)
      integer rflav(nlegreal)
      real * 8 amp2,amp2_mad,amp2tt_mad

cccccccccccccccccccccccccccccccc    
c     common bl. originally present in lh_readin, needed
c     by my_setpara
c
c     Common to lh_readin and printout
c
      double precision  alpha, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb !CKM matrix elements
      common/values/    alpha,gfermi,alfas,   
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb
ccccccccccccccccccccccccccccccccc


      integer nleg
      parameter (nleg=nlegreal)
      real *8 kr(0:3,nleg)
      integer rflav_ME(nleg)
      real *8 ewcoupl
      integer mu,ileg

      real * 8 vecl(3),betal
      data vecl/0d0,0d0,1d0/
      save vecl

      integer ftemp,mflav(nleg)
      real *8 ktemp,kr_mad(0:3,nleg),krcm_mad(0:3,nleg)

      real *8 x1,x2,s,x1p,x2p,sp
      real *8 krcm_mad_resh(0:3,nleg)
      real *8 wbmass2,BWfactor,PDFfactor,fluxfactor

      real *8 dotp
      external dotp

      logical check1
      parameter (check1=.false.)
      logical check2
      parameter (check2=.false.)

      real *8 tiny
      data tiny/1.d-5/

      include '../include/pwhg_flg.h'
      real *8 ptb,wt_plus_int_ofull
      
      real *8 powheginput
      external powheginput

ccccccccccccccccccccccccccccccccccccccc
c     charge conjugation
c     if ttype=-1, then rflav has been filled with tbar
c     production flavours. Subroutines here work for t flavour.
c     Therefore, invert the sign of local flavours.
      do ileg=1,nleg
         rflav_ME(ileg)= ttype *rflav(ileg)
      enddo
ccccccccccccccccccccccccccccccccccccccc

c     local copy of input variables (p->kr)
      do ileg=1,nleg
         do mu=0,3
            kr(mu,ileg)=p(mu,ileg)
         enddo
      enddo

c     check
      if ((abs(rflav(3)).ne.24).or.(abs(rflav(4)).ne.6)) then
         write(*,*) 'real_ampsq: ERROR in flavor assignement'
         call exit(1)
      endif

c     ew coupling
      ewcoupl=4d0*pi*alphaem_pow/sthw2_pow

ccccccccccccccccccccccccccccccccccccccccccc
c     >>> WT CHANNEL <<<
ccccccccccccccccccccccccccccccccccccccccccc

c     USING MADGRAPH SUBROUTINES
      do ileg=1,5
         mflav(ileg)=rflav_ME(ileg)
         do mu=0,3
            kr_mad(mu,ileg)=kr(mu,ileg)
         enddo
      enddo
c     to avoid bugs in HELAS, restore exact masslessness of incoming partons 
      kr_mad(0,1)=dabs(kr_mad(3,1))
      kr_mad(0,2)=dabs(kr_mad(3,2))
c     reassign here helas couplings and parameters that 
c     can change on an event-by-event basis
      alfas=st_alpha
      mtMS=sqrt(dotp(kr_mad(0,4),kr_mad(0,4)))
      tmass=mtMS
      twidth=topwidth_pow
      wwidth=0d0
      call my_setpara
c     invert 3rd and 4th particles before passing the array to
c     madgraph.
      ftemp=mflav(4)
      mflav(4)=mflav(3)
      mflav(3)=ftemp
      do mu=0,3
         ktemp=kr_mad(mu,4)
         kr_mad(mu,4)=kr_mad(mu,3)
         kr_mad(mu,3)=ktemp
      enddo

      amp2_mad=0d0
      call choose_real_process_full(kr_mad,mflav,amp2_mad)

c     if amp2_mad vanishes, there is no need to subtract a local
c     counterterm
      if(amp2_mad.eq.0) then
         PDFfactor=0d0
         goto 999
      endif
         
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     For convenience, boost real momenta in their CM rest frame
      betal=-(kr_mad(3,1)+kr_mad(3,2))/(kr_mad(0,1)+kr_mad(0,2))
      call mboost(nlegreal,vecl,betal,kr_mad,krcm_mad)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      s=4*krcm_mad(0,1)*krcm_mad(0,2)
      x1=kn_x1
      x2=kn_x2

      !check that these values correspond to the current s for kr
      if(check1) then
         if(dabs(x1*x2*kn_sbeams/s-1).gt.tiny) then
            write(*,*) 'Error 1 in setreal', s,x1*x2*kn_sbeams
            write(*,*) 'This check1 has to be switched off if'//
     $' smartsig is on'
            write(*,*) '  Conflict with the kinematics generated'//
     $' during the process of finding proportional matrix elements'
            call exit(1)
         endif
      endif

      call generate_reshuffled_kinematics(x1,x2,s,krcm_mad,x1p,x2p,sp,
     $krcm_mad_resh)

      call PDFreweight(mflav(1),mflav(2),x1,x2,x1p,x2p,PDFfactor)

      if(PDFfactor.lt.0) then
c     this means that original PDF's were vanishing
         PDFfactor=0d0
         goto 999
      endif

      call choose_real_process_tt(krcm_mad_resh,mflav,amp2tt_mad)

      wbmass2=dotp(krcm_mad(0,4),krcm_mad(0,4)) + 
     $     2*dotp(krcm_mad(0,4),krcm_mad(0,5))

      BWfactor=((tmass**2-tmass**2)**2 + tmass**2 *twidth**2)/
     $         ((wbmass2 -tmass**2)**2 + tmass**2 *twidth**2)      

      fluxfactor=1.
      if(powheginput('#withfluxfactor').eq.1) fluxfactor=s/sp
ccccccccccccccccccccccccccccccccccccc


ccccccccccccccccccccccc

c      PDFfactor=1d0 !ER: to test

c      BWfactor=BWfactor**(1.1) !ER: to test


 999  continue
c     if the flavour string does not contain a w-b pair in the
c     final state, there is no local counterterm to subtract
c     This is taken into account by means of the subroutine
c     choose_real_process_tt, that returns 0.

c     assign output
      amp2=amp2_mad-BWfactor*PDFfactor*fluxfactor*amp2tt_mad

c     check that subtraction works well close to the t pole
      if((check2).and.(174.lt.sqrt(wbmass2).and.sqrt(wbmass2).lt.176))
     $     then
         if(BWfactor*PDFfactor*fluxfactor*amp2tt_mad.ne.0.) then
            write(*,*) amp2_mad/(BWfactor*PDFfactor*fluxfactor
     $           *amp2tt_mad),sqrt(wbmass2)
         endif
      endif
      
c     A negative real contribution generated conflict with realgr. Now
c     it's also possible to run the program with negative weights (see
c     input file).
      if(amp2.le.0d0) then
         if((abs(rflav_ME(5)).ne.abs(rflav_ME(1))).and.
c     This if selects ONLY remnants that become negative. In this case,
c     assign an arbitrary small value. See comment at the beginning of
c     this file.
     $        (rflav_ME(1)+rflav_ME(2).eq.0).and.(rflav_ME(1).ne.0)) then
            amp2=1d-20
            goto 123
         endif

c$$$cccccccccccccccccccccccccccc
c$$$         wt_plus_int_ofull=amp2/amp2_mad
c$$$         ptb=sqrt(kr_mad(1,5)**2+kr_mad(2,5)**2)
c$$$         if(flg_nlotest) then
c$$$         if(wt_plus_int_ofull.lt.-5d-1) then
c$$$            write(65,*) sqrt(wbmass2),ptb
c$$$         elseif(-5d-1.lt.wt_plus_int_ofull.and.wt_plus_int_ofull.lt.-1d
c$$$     $           -1) then
c$$$            write(66,*) sqrt(wbmass2),ptb
c$$$         elseif(-1d-1.lt.wt_plus_int_ofull.and.wt_plus_int_ofull.lt.-1d
c$$$     $           -2) then
c$$$            write(67,*) sqrt(wbmass2),ptb
c$$$         elseif(-1d-2.lt.wt_plus_int_ofull.and.wt_plus_int_ofull.lt.-1d
c$$$     $           -3) then
c$$$            write(68,*) sqrt(wbmass2),ptb
c$$$         elseif(-1d-3.lt.wt_plus_int_ofull.and.wt_plus_int_ofull.lt.-1d
c$$$     $           -4) then
c$$$            write(69,*) sqrt(wbmass2),ptb
c$$$         elseif(-1d-4.lt.wt_plus_int_ofull.and.wt_plus_int_ofull.lt.-1d
c$$$     $           -5) then
c$$$            write(70,*) sqrt(wbmass2),ptb
c$$$         elseif(-1d-5.lt.wt_plus_int_ofull.and.wt_plus_int_ofull.lt.-1d
c$$$     $           -6) then
c$$$            write(71,*) sqrt(wbmass2),ptb
c$$$         elseif(-1d-6.lt.wt_plus_int_ofull.and.wt_plus_int_ofull.le.0d0)
c$$$     $           then
c$$$            write(72,*) sqrt(wbmass2),ptb
c$$$         endif
c$$$         endif
c$$$ccccccccccccccccccccccccccccc
      endif

c     to avoid the exact wb peak in the Sudakov generation stage.
c     There is no need to use genrad also in sigremnants, since
c     remnants are handled by the previous if statement.
      if(genrad) then
c     170-180 is OK
         if(dabs(sqrt(wbmass2)-topmass_pow)/topwidth_pow.lt.3) then
            amp2=1.d-20
         endif
      endif

      if((powheginput('#withthetacut').eq.1).and.(amp2.lt.0.)) amp2=1d
     $     -20

 123  continue
      amp2=amp2/(st_alpha/2./pi)

ccccccccccccccccccccc
      end


      subroutine generate_reshuffled_kinematics(x1,x2,s,kr,x1p,x2p,sp,
     $kr_resh)
c     kr is ordered as madgraph: 3=t, 4=w, 5=b. So has
c     to be the output kr_resh
      implicit none
      include 'nlegborn.h'
      include 'PhysPars.h'
      include '../include/pwhg_kn.h'
      real *8 x1,x2,s
      integer nleg
      parameter (nleg=nlegreal)
      real *8 kr(0:3,nlegreal)
      real *8 x1p,x2p,sp
      real *8 kr_resh(0:3,nlegreal)


      real *8 ct1p,ct2p,ctbw,phibw
      integer ileg,mu
      real *8 mwb,kb(0:3),kw(0:3),kt(0:3)
      real *8 tap,logstap,ycmp
      real *8 kt_mod,ktbar(0:3),beta(3),kw_tbar(0:3),
     $kw_tbar_mod,kt_resh(0:3),kt_resh_mod,
     $ktbar_resh(0:3),ktbar_resh_mod,ew_tbar,eb_tbar,
     $kb_tbar(0:3)

      real *8 dotp
      external dotp

      logical check
      parameter (check=.true.)
      real *8 tiny
      data tiny/1.d-5/


      do mu=0,3
         kt(mu)=kr(mu,3)
         kw(mu)=kr(mu,4)
         kb(mu)=kr(mu,5)
      enddo

      mwb=sqrt(dotp(kw,kw)+2*dotp(kb,kw))

      sp=min(s*(2*topmass_pow/(topmass_pow+mwb))**2,kn_sbeams)
      tap=sp/kn_sbeams
      logstap=0.5*log(tap)
      ycmp=0.5*log(x1/x2)
      if(ycmp.lt.logstap) ycmp=0.99*logstap
      if(ycmp.gt.-logstap) ycmp=-0.99*logstap
      x1p=sqrt(tap)*exp(ycmp)
      x2p=sqrt(tap)*exp(-ycmp)

c     t kinematics in cm rest frame
      kt_mod=sqrt(kt(1)**2+kt(2)**2+kt(3)**2)
      ct1p=kt(3)/kt_mod
      ct2p=cos(atan2(kt(1),kt(2)))
      if(kt_mod.lt.tiny) then
         ct1p=1.
         ct2p=1.
      endif
c      print*, kt(1),kt(2)

c     find wb kinematics in tbar rest frame
      do mu=0,3
         ktbar(mu)=kb(mu)+kw(mu)
      enddo
      do mu=1,3
         beta(mu)=-ktbar(mu)/ktbar(0)
      enddo
      call boost(beta,kw,kw_tbar)
      kw_tbar_mod=sqrt(kw_tbar(1)**2+kw_tbar(2)**2+kw_tbar(3)**2)
      ctbw=kw_tbar(3)/kw_tbar_mod
      phibw=atan2(kw_tbar(2),kw_tbar(1))
      if(kw_tbar_mod.lt.tiny) then
         ctbw=1.
         phibw=0.
      endif

c     generate a ttbar kinematics in partonic cm frame,
c     from sp, ct1p, ct2p. At the end ct2p is not used, since in Born_phsp 
c     the top momentum has always positive component along x axis,
c     and 0 along y. Therefore, I do the same here.
      do ileg=1,nlegreal
         do mu=0,3
            kr_resh(mu,ileg)=0d0
         enddo
      enddo
      kr_resh(0,1)=sqrt(sp)/2
      kr_resh(3,1)=sqrt(sp)/2
      kr_resh(0,2)=sqrt(sp)/2
      kr_resh(3,2)=-sqrt(sp)/2
      !top quark
      kt_resh_mod=sqrt(dabs(sp-4*topmass_pow**2)/4)
      kt_resh(0)=sqrt(sp)/2
      kt_resh(3)=kt_resh_mod*ct1p
      kt_resh(2)=0.
      kt_resh(1)=kt_resh_mod*sqrt(dabs(1-ct1p**2))
      !topbar quark
      ktbar_resh(0)=kt_resh(0)
      do mu=1,3
         ktbar_resh(mu)=-kt_resh(mu)
      enddo

c     from the generated tbar kinematics, 
c     add on top the wb decay, using ktbar_resh, ctbw, phibw
      ew_tbar=(topmass_pow**2+dotp(kw,kw))/2/topmass_pow
      eb_tbar=(topmass_pow**2-dotp(kw,kw))/2/topmass_pow
      ! check that ew+eb=mt, in this frame
      if(check) then
         if(dabs((ew_tbar+eb_tbar)/topmass_pow -1).gt.tiny) then
            write(*,*) 'Error 1 in gen_res_kin'
            call exit(1)
         endif
      endif
      kw_tbar(0)=ew_tbar
      kw_tbar(3)=eb_tbar *ctbw
      kw_tbar(1)=eb_tbar *sqrt(dabs(1-ctbw**2)) *cos(phibw)
      kw_tbar(2)=eb_tbar *sqrt(dabs(1-ctbw**2)) *sin(phibw)
      kb_tbar(0)=eb_tbar
      do mu=1,3
         kb_tbar(mu)=-kw_tbar(mu)
      enddo
      do mu=1,3
         beta(mu)=ktbar_resh(mu)/ktbar_resh(0)
      enddo

c     top, reshuffled
      do mu=0,3
         kr_resh(mu,3)=kt_resh(mu)
      enddo
c     w, reshuffled
      call boost(beta,kw_tbar,kr_resh(0,4))
c     b, reshuffled
      call boost(beta,kb_tbar,kr_resh(0,5))

      call checkmomzero(nlegreal,kr_resh)      

      end

      subroutine PDFreweight(f1,f2,x1,x2,x1p,x2p,PDFfactor)
      implicit none
      integer f1,f2
      real *8 x1,x2,x1p,x2p
      real *8 PDFfactor

      real *8 pdf1(-6:6),pdf2(-6:6),pdf1p(-6:6),pdf2p(-6:6)
      
c     old values (non reshuffled kinematics)
      call pdfcall(1,x1,pdf1)
      call pdfcall(2,x2,pdf2)

c     new values (reshuffled kinematics)
      call pdfcall(1,x1p,pdf1p)
      call pdfcall(2,x2p,pdf2p)


c     if the PDF's factor associated to the original (non reshuffled)
c     kinematics vanishes (or it is very small), then in this point no 
c     local subtraction is needed because original PDF already vanishes!)
c     Assign the special value -1 to PDF factor. See the effect of this in
c     setreal subroutine.
      if((pdf1(f1).lt.1d-6).or.(pdf2(f2).lt.1d-6)) then
         PDFfactor=-1
      else
         PDFfactor=pdf1p(f1)*pdf2p(f2)/pdf1(f1)/pdf2(f2)
      endif

c$$$      print*, '-----------------'
c$$$      print*, x1,x2,x1p,x2p
c$$$      print*,''
c$$$      print*, pdf1p(f1),pdf2p(f2),pdf1(f1),pdf2(f2)

      end
