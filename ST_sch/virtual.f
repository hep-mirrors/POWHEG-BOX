c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point
      subroutine setvirtual(p,vflav,virtual)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      include 'PhysPars.h'
      integer nleg
      parameter (nleg=nlegborn)
      real * 8 p(0:3,nleg)
      integer vflav(nleg)
      real * 8 virtual

ccccccccccccccccccccc
      integer mu,ileg
      real *8 kblab(0:3,nleg)
      real *8 ewcoupl,s,t,u
      real *8 fvirt_udx,fvirt_dxu
      real *8 dotp
      external dotp

      integer three_ch(-6:6)
      data three_ch /-2,1,-2,1,-2,1,0,-1,2,-1,2,-1,2/

      real *8 ckm_b_s
ccccccccccccccccccccc

ccccccccccccccc
      include 'stpcblks.h'
      include '../include/pwhg_flst.h'
      integer i_fb
      logical mcnlo_ME
      parameter (mcnlo_ME=.false.)
      real *8 matout(8,2)
      real *8 amp2mcnlo_udx,amp2mcnlo_dxu
c     To pass the already subtracted virtuals to sigsoftvirt
      real *8 fksfinite(1000)
      common/cfksfinite/fksfinite
ccccccccccccccc

      integer vflav_loc(nleg)

c     check
      if (abs(vflav(3)).ne.6) then
         write(*,*) 'setvirtual: ERROR in flavor assignement'
         call exit(1)
      endif

ccccccccccccccccccccccccccccccccccccccc
c     charge conjugation
c     if ttype=-1, then bflav has been filled with tbar-production flavours.
c     Subroutines here work for t-production flavour assignment.
c     Therefore, invert the sign of local flavours.
      do ileg=1,nleg
         vflav_loc(ileg)= ttype *vflav(ileg)
      enddo
ccccccccccccccccccccccccccccccccccccccc

c     local copy of variables
      do ileg=1,nleg
         do mu=0,3
            kblab(mu,ileg)=p(mu,ileg)
         enddo
      enddo

c     ew coupling
      ewcoupl=4d0*pi*alphaem_pow/sthw2_pow

c     Mandelstam variables
      s=2d0*dotp(kblab(0,1),kblab(0,2))
      t=dotp(kblab(0,3),kblab(0,3)) - 2d0*dotp(kblab(0,1),kblab(0,3))
      u=dotp(kblab(0,3),kblab(0,3))-s-t


      if(mcnlo_ME) then
c     fill 'mcnlo' common block
c     stpcblks.h already filled in init_parameters (see set_mcnlo_parameters routine)
c     However xmuf2h1, xmuf2h2 and xmur2 need to be assigned and to be equal
c     among themselves (otherwise f2sv doesn't work)
         xmuf2h1 =st_mufact2
         xmuf2h2 =st_mufact2
         xmur2   =st_muren2

c     calculate FKS-SUBTRACTED virtual amplitudes
c$$$         call f2sv(s,t,2,matout)
         amp2mcnlo_udx=matout(1,1) *ewcoupl**2
         amp2mcnlo_dxu=matout(3,1) *ewcoupl**2

c     Fill virtual table (no flux, no as/2pi)

c     ttype*flst_born needed because flst_born can be
c     the tbar-production flavour assignment, and the three_ch
c     mechanism works for t-production flavour assignment.
c     Here the use of bflav_loc is not possible, because I need
c     to run over all i_fb values, not only on the current one.

         do i_fb=1,flst_nborn
c     udx
            if ((three_ch(ttype*flst_born(1,i_fb)).eq.2).and.
     #(three_ch(ttype*flst_born(2,i_fb)).eq.1)) then
               fksfinite(i_fb)=amp2mcnlo_udx
     #*CKM(abs(flst_born(1,i_fb)),abs(flst_born(2,i_fb)))**2 
     #* CKM(abs(flst_born(3,i_fb)),abs(flst_born(4,i_fb)))**2 
c     dxu
            elseif ((three_ch(ttype*flst_born(1,i_fb)).eq.1).and.
     #(three_ch(ttype*flst_born(2,i_fb)).eq.2)) then
               fksfinite(i_fb)=amp2mcnlo_dxu
     #*CKM(abs(flst_born(1,i_fb)),abs(flst_born(2,i_fb)))**2 
     #* CKM(abs(flst_born(3,i_fb)),abs(flst_born(4,i_fb)))**2
            else
               write(*,*) 'Error in fill_virtual, (s), mcnlo check'
               call exit(1)
            endif
         enddo

      endif


      
ccccccccccccccccccccccccccccccccccccccccccc
c     >>> S CHANNEL <<<
ccccccccccccccccccccccccccccccccccccccccccc

c     Evaluate finite part of virtual contribution
      call virt_finite(s,t,u,dotp(kblab(0,3),kblab(0,3)),
     #fvirt_udx,fvirt_dxu,st_muren2)

!      write(*,*) 'virtual: ',s,t,fvirt_udx,fvirt_dxu

c     udx
      if ((three_ch(vflav_loc(1)).eq.2).and.
     #(three_ch(vflav_loc(2)).eq.1)) then
         virtual=fvirt_udx
         ckm_b_s=
     #CKM(abs(vflav_loc(1)),abs(vflav_loc(2)))**2 
     #* CKM(abs(vflav_loc(3)),abs(vflav_loc(4)))**2 

c     dxu
      elseif ((three_ch(vflav_loc(1)).eq.1).and.
     #(three_ch(vflav_loc(2)).eq.2)) then
         virtual=fvirt_dxu
         ckm_b_s=
     #CKM(abs(vflav_loc(1)),abs(vflav_loc(2)))**2 
     #* CKM(abs(vflav_loc(3)),abs(vflav_loc(4)))**2

      else
         write(*,*) 'Error in setvirtual, (s)'
         call exit(1)
      endif

ccccccccccccccccccccccc
c     assign output
      virtual=virtual *ewcoupl**2 *ckm_b_s
cccccccccccccccccccccc
      end



c     Output the finite part of virtual contribution.
c     Finite parts for s-channel single-top.
c     Virtual contributions are taken from Laenen-Weinzierl-Sullivan...
c     paper, after a manipulation analogous to the
c     one described in the t-channel note section.
c     An analytic continuation is also needed in this case.
c     Prefactor of FKS is different, so an expansion of
c     EulerGamma functions was needed.
c     Moreover, nontrivial logaritms arise because of
c     the presence of (mu^2/s) in front of the result.
      subroutine virt_finite(s,t,u,m2,fvirt_udx,fvirt_dxu,mur2)
      implicit none
      real *8 s,t,u,m2,fvirt_udx,fvirt_dxu,mur2
      include '../include/pwhg_math.h'
      include 'PhysPars.h'

      real *8 lambda,q2,prop
      real *8 amp2_udx,amp2_dxu

      real *8 vcf
      parameter (vcf=4.d0/3.d0)

      real *8 ddilog,A0,A1

      real *8 A0h_s,A0l_s,A1h_s,A1l_s,A2h_s,A2l_s

      real *8 cv0,cv,prop_mcfm,prop_ztop
      double complex c1

      logical check_virt
      parameter (check_virt=.true.)

      if(dabs(m2/topmass_pow**2 -1.).gt.1d-6) then
         write(*,*) 'virt_finite: problem with top offshelness'
         call exit(1)
      endif

      amp2_udx=u*(u-topmass_pow**2)/(s-wmass_pow**2)**2/4
      amp2_dxu=t*(t-topmass_pow**2)/(s-wmass_pow**2)**2/4

      if(s+t+u-m2.gt.1d-7) then
         print*, 'virt_finite: problem with invariants'
         call exit(1)
      endif


cccccccccccccccccccccccc
c     s-channel
cccccccccccccccccccccccc
      lambda=s/(s-m2)


ccccccccccccccccccccccccccccc
c     This result has been obtained by crossing
c     the result in the paper by Laenen&al.
c     Several checks have been performed.
c      A2l=-2.
c      A1l=-3.
c      A0l=pi**2-8.-pi**2/3.
c     After having factorized the same prefactor
c     of Altarelli-Ellis-Martinelli, that corresponds
c     to 1/Ga[1-ep] up to order O(ep^3), we have
      A2l_s=-2.
      A1l_s=-3.
      A0l_s=(pi**2 -8. -pi**2/3.) 
     $     + pi**2/3.           !from 'EulerGamma prefactor'
c     A2h=-1.
c     A1h=-5./2. -2.*log(m2/(s-m2)) -log(s/m2)
c     A0h=....
c     After having factorized the same prefactor
c     of Altarelli-Ellis-Martinelli, that corresponds
c     to 1/Ga[1-ep] up to order O(ep^3), we have
      A2h_s=-1.
      A1h_s=-5./2. -2.*log(m2/(s-m2)) -log(s/m2)
      A0h_s=(
     $     -0.5*(log(s/m2))**2 -5./2.*log(s/m2) 
     $     - 2*log(m2/(s-m2))*log(s/m2) -6. 
     $     -(s-m2)/s*log(m2/(s-m2)) -(log(m2/(s-m2)))**2
     $     +pi**2 -2*log(m2/(s-m2))
     $     +2*ddilog(lambda) -pi**2/3.)
     $     +pi**2/6.            !from 'EulerGamma prefactor'

      prop= (A0l_s+A0h_s)
     $     +(A1l_s+A1h_s) *log(mur2/s)
     $     +(A2l_s+A2h_s) *0.5*(log(mur2/s))**2
ccccccccccccccccccccccccccccc

      if(check_virt) then
c     A0 from ZTOP, s-channel
         A0= 
     $        + (-0.5*(log(s/m2))**2 -5./2.*log(s/m2) 
     $        - 2*log(m2/(s-m2))*log(s/m2) -3.*log(m2/(s-m2))
     $        + m2/s*log(m2/(s-m2)) -(log(m2/(s-m2)))**2
     $        +2*ddilog(lambda))
     $        - 14. +pi**2 +pi**2/3.
c     A1, by crossing the Laenen&al result
         A1= -11./2. -2.*log(m2/(s-m2)) -log(s/m2)
c     from previous A0 and A1
         prop_ztop=
     $        A0 
     $        + pi**2/2.        !from different EulerGamma
     $        + A1*log(mur2/s)
     $        - 3./2.*(log(mur2/s))**2

c     from Campbell-Ellis, MCFM
c     Notice that the use of lnrat is crucial in order
c     to get all pi's from analytical continuation
         call coefs(s,m2,cv0,cv,c1)
         prop_mcfm=cv0+cv

         if(dabs(prop/prop_mcfm-1).gt.1d-6) then
            write(*,*) '>>>>> POW/MCFM, light ',
     $           (A0l_s+A1l_s*log(mur2/s)+A2l_s*0.5*(log(mur2/s))**2)/cv0

            write(*,*) '>>>>> POW/MCFM, heavy ',
     $     (A0h_s+A1h_s*log(mur2/s)+A2h_s*0.5*(log(mur2/s))**2)/cv

            write(*,*) 'POW/MCFM ',prop/prop_mcfm
            call exit(1)
         endif
         if(dabs(prop/prop_ztop-1).gt.1d-6) then
            write(*,*) 'POW/ZTOP ',prop/prop_ztop
            call exit(1)
         endif
      endif

      prop=prop*vcf
      fvirt_udx=prop*amp2_udx   !1
      fvirt_dxu=prop*amp2_dxu   !3

c     add part not proportional to born
      fvirt_udx=fvirt_udx  +vcf *m2*t*u/s *log((s-m2)/m2) /4.
     $     * (s- wmass_pow**2)**(-2)
      fvirt_dxu=fvirt_dxu  +vcf *m2*u*t/s *log((s-m2)/m2) /4.
     $     * (s- wmass_pow**2)**(-2)
      
      end



      subroutine coefs(s12,mtsq,cv0,cv,c1)
      implicit none
!:      include 'constants.f'
      include '../include/pwhg_math.h'
!:      include 'epinv.f'

!:      include 'epinv2.f'

!:      include 'scale.f'
      include '../include/pwhg_st.h'

!:      include 'scheme.f'

      
      double precision cv,cv0,Li2la
      double precision s12,mtsq,taucs,ddilog,eta,la,oml
      double complex lnrat,logoml,logla,xl12,logsca,Kfun,c1

cccccccccccccccccccccccccccccccccccccc
c     !:
      double precision epinv
      double precision epinv2
      real *8 musq
      double complex cone
      real *8 pisqo6
      character*4 scheme
      external lnrat


      cone=(1d0,0d0)
      pisqo6=pi**2/6.
      scheme='tH-V'    
      epinv=0d0
      epinv2=0d0
      musq=st_muren2
cccccccccccccccccccccccccccccccccccccc

      if (scheme .eq.'dred') then
C------        eta=0 4d-hel
         eta=0d0
      elseif (scheme .eq. 'tH-V') then
C------       eta=1 t'Hooft Veltman
         eta=1d0
      endif

C**********************************************************************
C   Massless case
C   Taken from
C   %\cite{Altarelli:1979ub}
C   \bibitem{Altarelli:1979ub}
C   G.~Altarelli, R.~K.~Ellis and G.~Martinelli,
C   %``Large Perturbative Corrections To The Drell-Yan Process In QCD,''
C   Nucl.\ Phys.\ B {\bf 157}, 461 (1979).
C   %%CITATION = NUPHA,B157,461;%%
C   Using Eqn(58) with normalization changed to 
C   as/2/pi*cf*(4*pi)^ep/Gamma(1-ep) 
C   Taking account that Gamma(1-ep)^2/Gamma(1-2*ep)=1-ep^2*pi^2/6
C**********************************************************************
      xl12=lnrat(-s12,musq) 
      cv0=-2d0*epinv*(epinv2-dble(xl12))-dble(xl12**2)
     .           -3d0*(epinv-dble(xl12))-7d0-eta



C---- this routine has been constructed following closely 
C---- the notation of
C---- %\cite{Gottschalk:1980rv}
C---- \bibitem{Gottschalk:1980rv}
C---- T.~Gottschalk,
C---- %``Chromodynamic Corrections To Neutrino Production Of Heavy Quarks,''
C---- Phys.\ Rev.\ D {\bf 23}, 56 (1981).
C---- %%CITATION = PHRVA,D23,56;%%
C----- Adapted from Eqs.(A8,A9)

      taucs=s12-mtsq
      la=-s12/(mtsq-s12)
      oml=1d0-la
C-----oml=mtsq/(mtsq-s12)
      logoml=-lnrat(-taucs,mtsq)
      logsca=lnrat(-taucs,musq)
      Kfun=dcmplx(oml/la)*logoml

c--- Minus sign relative to Gottschalk since incoming b has momentum
c--- vector reversed for the t-channel process
c--- s-channel process follows by crossing
      c1=-dcmplx(2d0)*Kfun

      if (la .lt. 1d0) then
      Li2la=ddilog(la)
      else
      logla=lnrat(-s12,-taucs)
      Li2la=pisqo6-ddilog(oml)-dble(logla*logoml)
      endif
      cv=-epinv*epinv2
     . -epinv*(2.5d0+dble(logoml-logsca))
     . -0.5d0*(11d0+eta)-pisqo6+2d0*Li2la-dble(Kfun)
     .  -0.5d0*dble(logoml*(cone-logoml))
     .  +2.5d0*dble(logsca)+dble(logsca*logoml)-0.5d0*dble(logsca**2)

      return
      end


      double complex function Lnrat(x,y)
************************************************************************
*     Author: R.K. Ellis                                               *
*     August, 1998.                                                    *
c     Lnrat(x,y)=log(x-i*ep)-log(y-i*ep)                               *
c     this function is hard-wired for sign of epsilon we must adjust   *
c     sign of x and y to get the right sign for epsilon                *
************************************************************************
      implicit none
!:      include 'constants.f'
      include '../include/pwhg_math.h'
      double precision x,y,htheta
C--- define Heaviside theta function (=1 for x>0) and (0 for x < 0)
      htheta(x)=0.5+0.5*sign(1d0,x)


c$$$      real *8 half,one
c$$$      one=1d0
c$$$      half=0.5d0

      double complex impi
      impi=(0d0,1d0)*pi


      Lnrat=dcmplx(dlog(abs(x/y)))-impi*dcmplx((htheta(-x)-htheta(-y)))
      return
      end
