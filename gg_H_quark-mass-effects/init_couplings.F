      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      include 'pwhg_st.h'
      include 'pwhg_math.h'    
      include 'nlegborn.h'      
      include 'pwhg_kn.h'      
      real * 8 powheginput
      external powheginput
      real * 8 ewamplitude,amplew
      common /ew/ewamplitude
      external amplew

      real * 8 foo

c     number of light flavors
      st_nlight = 5

c     Model selection
      model = int(powheginput('model'))

      massren = int(powheginput('massren'))
      if (massren.eq.0) then
         write(*,*) 'On-shell mass renormalization scheme selected.'
      else if (massren.eq.1) then
         if (model.eq.2) then
            write(*,*) 'DRBar mass renormalization scheme selected'
         else
            write(*,*) 'MSBar mass renormalization scheme selected'
         endif
      else if ((massren.eq.2).and.(model.eq.2)) then
         write(*,*) 'Mixed mass renormalization scheme selected'
      else if ((massren.eq.2).and.(model.eq.0)) then
         write(*,*) 'DRBar mass renormalization scheme selected'
      else
         write(*,*) 'Unknow mass renormalization scheme selected.
     $Aborting.'
         stop
      endif
      
c     Couplings calculation
      if (model.eq.0) then
         write(*,*) 'SM selected'
         asca = 0
         call init_ind()
         call init_fermions()
      else if (model.eq.2) then
         write(*,*) 'MSSM not supported in this version.
     $ Contact the authors.'
         stop
         write(*,*) 'MSSM selected'
         afer = 2
         flg_noscalars = int(powheginput('#noscalars'))
         if (flg_noscalars.le.0) then
            asca = 4
            flg_lhscalars = int(powheginput('#lhscalars'))
            if (flg_lhscalars.gt.0) then
               write(*,*) 'Light Higgs limit for scalars enabled'
               flg_lhscalars = 1
            else
               flg_lhscalars = 0
            endif
         else
            asca = 0
         endif
         mmaa = 256d0
         ih = powheginput('higgstype')
         call genmssmvar()
         if (massren.eq.2) then
            write(*,*) 'Not implemented... yet'
            stop
         else if ((massren.gt.2).or.(massren.lt.0)) then
            write(*,*) 'Wrong value for massren'
            stop
         end if
      else if (model.eq.1) then
         write(*,*) 'MW not supported in this version.
     $ Contact the authors.'
         write(*,*) 'Manhoar-Wise selected'
         stop
         asca = 3
         call init_ind()
         call init_mw()
      else
         write(*,*) 'Unknow model selected. Aborting.'
         stop
      endif
      call init_higgs()

c     EW corrections
c$$$      flg_ew = int(powheginput('ew'))
c$$$      if (flg_ew.le.0) then
c$$$         flg_ew = 0
c$$$         write(*,*) '2-loops EW corrections disabled'
c$$$      else
c$$$         flg_ew = 1
c$$$         write(*,*) '2-loops EW corrections enabled'
c$$$
c$$$         flg_passarino = int(powheginput('#passarino'))
c$$$         if (flg_passarino.gt.0) then
c$$$            write(*,*) 'Passarino EW corrections enabled'
c$$$         end if
c$$$
c$$$         flg_fast_ew = int(powheginput('#fastew'))
c$$$         if (flg_fast_ew.gt.0) then
c$$$            write (*,*) 'Enable fast ew corrections evaluations
c$$$     $ by mass sampling'
c$$$            flg_fast_ew = 1
c$$$            ewbins = 61
c$$$            call init_cached_ew_corr
c$$$         endif
c$$$      endif
      flg_ew = int(powheginput('ew'))
      if (flg_ew.le.0) then
         flg_ew = 0
         write(*,*) '2-loops EW corrections disabled'
         flg_fast_ew = 0
         flg_passarino = 0
      else
         write(*,*) '2-loops EW corrections enabled'
c     For release only passarino is supported, with mass sampling
         flg_ew = 1
         flg_fast_ew = 1
         flg_passarino = 1
         call init_cached_ew_corr
      endif



      flg_nnlo = int(powheginput('#nnlo'))
      if (flg_nnlo.le.0) then
         flg_nnlo = 0
         write(*,*) 'NNLO rescaling disabled'
         nnlorescfactor = 1d0
      else
         open(24,file='nnlo.dat')
         read (24,*) nnlorescfactor
         write(*,*) 'NNLO rescaling factor set to', nnlorescfactor
      endif

      write(*,*) 'Active fermions'
      write(*,*) afer
      write(*,*) 'fermion masses'
      write(*,*) mfer
      write(*,*) 'fermion couplings'
      write(*,*) lambdafer
      if ((model.eq.1).or.(model.eq.2)) then
         write(*,*) 'Active scalars'
         write(*,*) asca
         write(*,*) 'Scalar masses'
         write(*,*) msca
         write(*,*) 'Scalar couplings'
         write(*,*) lambdasca
         write(*,*) 'mumass2'
         write(*,*) mumass2
      endif
      write(*,*) '1/alphaem = ',1d0/ph_alphaem
      write(*,*) 'sthw2 = ',ph_sthw2
      write(*,*) 'GF = ',ph_GF
      end

c     Indipendent variables for SM and MW
      subroutine init_ind()
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      real * 8 powheginput
      external powheginput

      ph_Hmass = powheginput('hmass')
      ph_Hmass2 = ph_Hmass**2

      flg_hdecay = int(powheginput('#hdecaywidth'))
      if ((flg_hdecay.gt.0).and.(model.eq.0)) then
         call hdecayparser(ph_Hmass,ph_Hwidth)
      else
         ph_Hwidth = powheginput('hwidth')
      endif
      ph_Zmass  = 91.1876d0
      ph_Zwidth =  2.4952d0
      ph_Wmass  = 80.398d0
      ph_Wwidth =  2.141d0

      ph_alphaem = 1d0/137.035999679d0
      ph_alphaemmz = 1d0/128d0
      ph_sthw2 = abs(1d0-(ph_Wmass/ph_Zmass)**2)
      ph_GF= powheginput('gfermi')

      ph_sthw = sqrt(ph_sthw2)
      ph_cthw2 = 1-ph_sthw2
      ph_cthw = sqrt(ph_cthw2)

      end subroutine


c     If fast_ew is enabled, create the array with the campionazied deltaew value.
      subroutine init_cached_ew_corr
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      integer i
      real * 8 m12,m0,y12,y0,value
      complex * 16 ampl, reduced,x12,x0,aux,bornqcd
      common /bornampl/ampl,bornqcd
      real * 8 deltaew
      external reduced,deltaew

      write(*,*) 'Starting cached EW corrections array initialization'
      do mh=0,5000
         value = 0d0
         if (flg_passarino.eq.1) then
            call exa_ewgrid(mh,value)
c            write(*,*) 'mh',mh, value
            cached_ew_corr(int(mh)) = value/100d0
         else
c     write(*,*) 'mh', mh
            ampl = dcmplx(0d0)
            do i=1,afer
               m12=mfer(i)
               y12=m12**2/mh**2
               x12 = reduced(1d0/y12)
               aux = lambdafer(i)*trfer(i)*
     &              (-4d0)*y12*(2d0-(1d0-4d0*y12)*0.5d0*log(x12)**2)
               ampl = ampl+aux
            end do
            if (flg_lhscalars.eq.0) then
               do i=1,asca
                  m0 = msca(i)
                  y0 = m0**2/mh**2
                  x0 = reduced(1d0/y0)
                  ampl = ampl + lambdasca(i)*trsca(i)*(mmaa/m0)**2*
     &                 4d0*y0*(1d0+2d0*y0*0.5d0*log(x0)**2)
               end do
            else
               do i=1,asca
                  m0 = msca(i)
                  y0 = m0**2/mh**2
                  ampl = ampl + lambdasca(i)*trsca(i)*(mmaa/m0)**2*
     &                 (-1d0/3d0-8d0/(45d0*4d0*y0))
               end do
            endif
            cached_ew_corr(int(mh)) = deltaew(ampl)
         end if
         write(33,*) 'deltaew',mh, cached_ew_corr(int(mh))
      end do
      close(33)
      write(*,*) 'Finished cached EW corrections array initialization'
       end subroutine init_cached_ew_corr

c     Initialize Higgs mass
      subroutine init_higgs()
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      include 'nlegborn.h'
      include 'pwhg_kn.h'
      include 'pwhg_math.h'
      real * 8 masswindow
      real * 8 powheginput
      external powheginput
      logical verbose
      parameter(verbose=.true.)

      flg_onshell = int(powheginput('zerowidth'))
      if (flg_onshell.eq.1) then
c     Higgs is produced on-shell
         ph_Hmass2low = ph_Hmass2
         ph_Hmass2high = ph_Hmass2low
         ph_HmHw = 0d0
      else
c     set mass windows around H-mass peak in unit of ph_Hwidth
c     It is used in the generation of the Born phase space
c     masswindow is an optonal  parameter passed by the user
c     the default vale is 10 
         masswindow = powheginput("#masswindow")
         if(masswindow.lt.0d0) masswindow=10d0
c     ph_Hmass2low=(ph_Hmass-masswindow*ph_Hwidth)^2
         ph_Hmass2low=max(0d0,ph_Hmass-masswindow*ph_Hwidth)
         ph_Hmass2low= ph_Hmass2low**2
c     ph_Hmass2high=(ph_Hmass+masswindow*ph_Hwidth)^2
         ph_Hmass2high=min(kn_sbeams,(ph_Hmass+masswindow*ph_Hwidth)**2)
         ph_HmHw = ph_Hmass * ph_Hwidth
         ph_unit_e = sqrt(4*pi*ph_alphaem)
      endif
      if(verbose) then
         write(*,*) '*************************************'
         write(*,*) 'H mass = ',ph_Hmass
         if (flg_onshell.ne.1) then
            write(*,*) 'H width = ',ph_Hwidth
         else
            write(*,*) 'H is produced on-shell'
         endif
         if (flg_onshell.ne.1) then
            write(*,*) '*************************************'
            write(*,*) sqrt(ph_Hmass2low),' < M_H <',sqrt(ph_Hmass2high)
         endif
         write(*,*) '*************************************'
      endif

      end subroutine

c     Fermions initialization subroutines used by SM and MW
      subroutine init_fermions()
      implicit none
      include 'PhysPars.h'
      include 'pwhg_st.h'
      include 'Flags.h'
      real * 8 powheginput,getRSmass,muren2
      real * 8 pwhg_alphas,alpha_s
      external pwhg_alphas
      external powheginput

c     TODO: for the final release this should be changed
      muren2 = ph_Hmass2
      alpha_s =  pwhg_alphas(ph_Hmass2,st_lambda5MSB,st_nlight)
      write(*,*) 'lambda', st_lambda5MSB, st_nlight

      afer = 0
      ph_topmass = powheginput('#topmass')
      if (ph_topmass.ne.-1000000d0) then
         write(*,*) "Top Quark enabled"
         afer = afer + 1
         trfer(afer) = 1d0/2d0
         mfer(afer) = getRSmass(ph_topmass,massren,ph_Hmass,alpha_s)
         lambdafer(afer) = 1d0
         ferlogmratio(afer) = log(mfer(afer)**2/muren2)
         if (massren.eq.2) then
            ferlogmratio(afer) = ferlogmratio(afer) - 1d0/3d0
         endif
      endif
      ph_bottommass = powheginput('#bottommass')
      if (ph_bottommass.ne.-1000000d0) then
         write(*,*) "Bottom Quark enabled"
         afer = afer+1
         trfer(afer) = 1d0/2d0
         mfer(afer) = getRSmass(ph_bottommass,massren,ph_Hmass,alpha_s)
         lambdafer(afer) = 1d0
         ferlogmratio(afer) = log(mfer(afer)**2/muren2)
         if (massren.eq.2) then
            ferlogmratio(afer) = ferlogmratio(afer) - 1d0/3d0
         endif
      endif
      ph_charmmass = powheginput('#charmmass')
      if (ph_charmmass.ne.-1000000d0) then
         write(*,*) "Charm Quark enabled"
         afer = afer+1
         trfer(afer) = 1d0/2d0
         mfer(afer) = getRSmass(ph_charmmass,massren,ph_Hmass,alpha_s)
         lambdafer(afer) = 1d0
         ferlogmratio(afer) = log(mfer(afer)**2/muren2)
         if (massren.eq.2) then
            ferlogmratio(afer) = ferlogmratio(afer) - 1d0/3d0
         endif
      endif

      end subroutine


      real * 8 function getRSmass(osmass,RS,q,alpha_s)
      implicit none
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      real * 8 osmass,q,alpha_s
      integer RS
      
c     OS
      if (RS.eq.0) then
         getRSmass = osmass
c     MSBAR
      else if (RS.eq.1) then
         getRSmass = osmass*(1-alpha_s/pi*(log(q**2/osmass**2)
     $                  +4d0/3d0))
c     DRBAR
      else if (RS.eq.2) then
         getRSmass = osmass*(1-alpha_s/pi*(log(q**2/osmass**2)
     $                  +5d0/3d0))
      else
         write(*,*) 'Error: unknown renormalization scheme! Exiting'
         stop
      endif

c      write(*,*) 'st_alpha', alpha_s
     
      end function

c     MW initialization subroutine
      subroutine init_mw()
      implicit none
      include 'PhysPars.h'
      real * 8 ms,v,lam1,lam2
      real * 8 powheginput
      external powheginput

      call init_fermions()
c     Getting Manohar-Wise paramter from powheg.input
      ms = powheginput('ms')
      v = powheginput('v')
      lam1 = powheginput('lam1')
      lam2 = powheginput('lam2')
      mmaa = v
c     scalar   (charged)
      trsca(1) = 3d0
      msca(1) = sqrt(ms**2 + lam1*v**2/4d0)
      lambdasca(1) = lam1/2d0
c     scalar   (neutral even)
      trsca(2) = 3d0
      msca(2) = sqrt(ms**2 + (lam1+2d0*lam2)*v**2/4d0)
      lambdasca(2) = (lam1+2d0*lam2)/4d0
c     scalar   (neutral odd)
      trsca(3) = 3d0
      msca(3) = sqrt(ms**2 + lam1*v**2/4d0)
      lambdasca(3) = lam1/4d0
      mumass2 = 750d0**2

      end subroutine

c     MSSM DRBAR
      subroutine genmssmvar()
      implicit none
      include 'Flags.h'
#include "PhysPars.h"
#include "pwhg_st.h"
      real * 8 qtop,i3top,lambdat,lambdab
      real * 8 qbot,i3bot,hmixfact1,hmixfact2
      real * 8 Ab,At,c2b,c2t,cosa,s2b,s2t,sina,sin2b
      real * 8 Xb,Xt,fact1,fact2

c     We fill the common block with the physical parameters according
c     to the chosen renormalization scheme.
      if (massren.eq.0) then
         call OSsusyparameter()
      else if (massren.eq.1) then
         call DRBARsusyparameter()
      else if (massren.eq.2) then
         write(*,*) 'Not implemented, stop'
      end if

c     Couplings (lambdas) calculation
c     Physcal parameters needed to calculate lambdafer e lambdasca
      qtop = 2d0/3d0
      i3top = 1d0/2d0
      qbot = -1d0/3d0
      i3bot = -1d0/2d0

      s2t = 2d0*ph_ctt*ph_stt
      c2t = ph_ctt**2 - ph_stt**2

      s2b = 2d0*ph_ctb*ph_stb
      c2b = ph_ctb**2 - ph_stb**2

      Xt = (ph_t1_2-ph_t2_2)*s2t/2d0/ph_topmass
      At = Xt - ph_mumssm/ph_tanb           ! note our sign convention for mu

      Xb = (ph_b1_2-ph_b2_2)*s2b/2d0/ph_mbsb
      Ab = Xb - ph_mumssm*ph_tanb           ! note our sign convention for mu

      sina = sin(ph_alpha)
      cosa = cos(ph_alpha)

c     sin(2beta)
      sin2b = 2d0 * ph_sb * ph_cb

      lambdat = 1d0/ph_sb
      lambdab = 1d0/ph_cb

c     Light Higgs
      if (ih.eq.1) then
         hmixfact1 = -sina
         hmixfact2 = cosa
c     Heavy Higgs
      else
         hmixfact1 = cosa
         hmixfact2 = sina
      endif

c     fermion 1  (top)
      trfer(1) = 1d0/2d0
      mfer(1) = ph_topmass
      lambdafer(1) = lambdat * hmixfact2
      if (massren.eq.1) then
         ferlogmratio(1) = log(mfer(1)**2/q**2)-1d0/3d0
      end if

c     fermion 2  (bottom)
      trfer(2) = 1d0/2d0
      if (flg_mssm_q_mh.eq.1) then
         mfer(2) = ph_bottommass_mh
      else
         mfer(2) = ph_bottommass
      end if
      lambdafer(2) = lambdab * hmixfact1
      if (massren.eq.1) then
         if (flg_mssm_q_mh.eq.1) then
            ferlogmratio(2) = log(mfer(2)**2/ph_Hmass**2)-1d0/3d0
         else
            ferlogmratio(2) = log(mfer(2)**2/q**2)-1d0/3d0
         end if
      end if

c     scalar 1  (stop 1)
      trsca(1) = 1d0/2d0
      msca(1) = ph_t1

      fact1 =   1d0/2d0 * s2t * ph_mumssm * mfer(1)
     $        + 1d0/4d0 * i3top * ph_Zmass**2 * sin2b * (1+c2t)
     $        - 1d0/2d0 * c2t * ph_Zmass**2 * qtop * sin2b * ph_sthw2

      fact2 =   mfer(1)**2
     $        + 1d0/2d0 * At * mfer(1) * s2t
     $        - 1d0/2d0 * i3top * ph_Zmass**2 * ph_sb**2 * (1+c2t)
     $        + c2t * ph_Zmass**2 * qtop * ph_sb**2 * ph_sthw2

      lambdasca(1) = (lambdat/mmaa**2) * (
     $hmixfact1 * fact1 +
     $hmixfact2 * fact2
     $)

c     scalar 2  (stop 2)
      trsca(2) = 1d0/2d0
      msca(2) = ph_t2

      fact1 = - 1d0/2d0 * s2t * ph_mumssm * mfer(1)
     $        + 1d0/4d0 * i3top * ph_Zmass**2 * sin2b * (1-c2t)
     $        + 1d0/2d0 * c2t * ph_Zmass**2 * qtop * sin2b * ph_sthw2

      fact2 =  mfer(1)**2
     $       - 1d0/2d0 * At * mfer(1) * s2t
     $       - 1d0/2d0 * i3top * ph_Zmass**2 * ph_sb**2 * (1-c2t)
     $       - c2t * ph_Zmass**2 * qtop * ph_sb**2 * ph_sthw2

      lambdasca(2) = (lambdat/mmaa**2) * (
     $hmixfact1 * fact1 +
     $hmixfact2 * fact2
     $)

c     scalar 1  (sbottom 1)
      trsca(3) = 1d0/2d0
      msca(3) = ph_b1

      fact1 =   ph_mbsb**2
     $        + 1d0/2d0 * i3bot * ph_Zmass**2 * ph_cb**2
     $        + 1d0/2d0 * i3bot * ph_Zmass**2 * ph_cb**2 * c2b
     $        + 1d0/2d0 * Ab * ph_mbsb * s2b
     $        - c2b * ph_cb**2 * ph_Zmass**2 * qbot * ph_sthw2

      fact2 =   1d0/2d0 * s2b * ph_mumssm * ph_mbsb
     $        - 1d0/4d0 * i3bot * ph_Zmass**2 * sin2b * (1+c2b)
     $        + 1d0/2d0 * c2b * ph_Zmass**2 * qbot * sin2b * ph_sthw2

      lambdasca(3) = (lambdab/mmaa**2) * (
     $hmixfact1 * fact1 +
     $hmixfact2 * fact2
     $)

c     scalar 2  (sbottom 2)
      trsca(4) = 1d0/2d0
      msca(4) = ph_b2

      fact1 =   ph_mbsb**2 
     $        - 1d0/2d0 * Ab * ph_mbsb * s2b
     $        + 1d0/2d0 * ph_cb**2 * i3bot * ph_Zmass**2 * (1-c2b)
     $        + c2b * ph_cb**2 * ph_Zmass**2 * qbot * ph_sthw2

      fact2 = - 1d0/2d0 * s2b * ph_mumssm * ph_mbsb
     $        - 1d0/4d0 * i3bot * ph_Zmass**2 * sin2b * (1-c2b)
     $        - 1d0/2d0 * c2b * ph_Zmass**2 * qbot * sin2b * ph_sthw2

      lambdasca(4) = (lambdab/mmaa**2) * (
     $hmixfact1 * fact1 +
     $hmixfact2 * fact2
     $)

      end

      subroutine DRBARsusyparameter()
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      real * 8 v,mh_1,mh_2,alpha
      real * 8 Ab,At,g,gp,msbr,msql,mstr
      real * 8 q_g,q_yb,q_yt,qsoft,q_ad,q_au
      real * 8 yb,yt
      real * 8 checkvalue
      real * 8 mstop2(2),msbot2(2)
      real * 8 powheginput
      real * 8 q_yb_mh,v_mh,yb_mh
      external powheginput
      character*50 valname
      integer error
c     SLHALib definition
#include "SLHA.h"
      complex * 16 slhadata(nslhadata)
c     First implementation of quarks mass renormalised at mh scale.
c     These are the only two model parameter we actually retrive from the powheg.input file
      ph_GF= powheginput('gfermi')
      ph_Hwidth = powheginput('hwidth')

c     WARNING: Commented since this is the SM version only
c     The others are read from the SLHA file
c      call SLHAclear(slhadata)
c      call SLHARead(error, slhadata, "mssm-param.slha", 0)
      if(error.ne.0) stop "Read error from mssm-param.slha"
      write(*,*) "** SLHA file input parameters **"
c     CP-Even neutral Higgs double masses
      valname = "Light CP-even Higgs mass"
      mh_1 = checkvalue(Mass_Mh0,valname)
      valname = "Heavy CP-even Higgs mass"
      mh_2 = checkvalue(Mass_MHH,valname)
c     Higgsino mass parameters
      valname = "Higgsino mass parameter"
      ph_mumssm = checkvalue(HMix_MUE,valname)
c     Tan beta
      valname = "Tan(beta)"
      ph_tanb = checkvalue(HMix_TB,valname)
c     Vev
      valname = "Higgs vev"
      v = checkvalue(HMix_VEV,valname)
c     MSOFT Block
      valname = "Soft Block renormalization scale"
      qsoft = checkvalue(MSoft_Q,valname)
      valname = "Gluino mass"
      ph_mg  = checkvalue(MSoft_M3,valname)
      valname = "mqL3"
      msql = checkvalue(MSoft_MSQ(3),valname)
      valname = "mtR"
      mstr = checkvalue(MSoft_MSU(3),valname)
      valname = "mbR"
      msbr = checkvalue(MSoft_MSD(3),valname)
c     AU Block
      valname = "Au block renormalization scale"
      q_au = checkvalue(Au_Q,valname)
      valname = "At"
      At   = checkvalue(Au_At,valname)
c     AD Block
      valname = "Ad block renormalization scale"
      q_ad = checkvalue(Ad_Q,valname)
      valname = "Ab"
      Ab   = checkvalue(Ad_Ab,valname)
c     YU Block
      valname = "Yu block renormalization scale"
      q_yt = checkvalue(Yu_Q,valname)
      valname = "Yt"
      yt = checkvalue(Yu_Yt,valname)
c     YD Block
      valname = "Yd block renormazation scale"
      q_yb = checkvalue(Yd_Q,valname)
      valname = "Yb"
      yb = checkvalue(Yd_Yb,valname)
c     Mixing angle in the CP-Even Higgs sector
      valname = "CP-even Higgs sector mixing angle"
      alpha = checkvalue(Alpha_Alpha,valname)
c     GAUGE Block
      valname = "Gauge block renormalization scale"
      q_g = checkvalue(Gauge_Q,valname)
      valname = "g'mass"
      gp  = checkvalue(Gauge_g1,valname)
      valname = "g mass"
      g  = checkvalue(Gauge_g2,valname)

      if ((qsoft.eq.q_au).and.(q_au.eq.q_ad).and.(q_ad.eq.q_yt)
     $     .and.(q_yt.eq.q_yb).and.(q_yb.eq.q_g)) then
         q = qsoft
      else
         write(*,*) 'Error: inconsistency in the
     $renormalization scales. Aborting.'
         stop
      endif
      ph_mumssm = -ph_mumssm                  ! we use the opposite convention

c     DR-BAR
c     compute the running masses for quarks, squarks and gauge bosons

      write(*,*) 'Computing the running masses for quarks, squarks
     $ and gauge bosons'

      ph_cb = 1d0/sqrt(1+ph_tanb**2)
      ph_sb = ph_tanb*ph_cb

      ph_topmass = yt*v/sqrt(2d0)*ph_sb
      ph_bottommass = yb*v/sqrt(2d0)*ph_cb

      ph_Wmass = g*v/2d0
      ph_Zmass = sqrt(g**2+gp**2)*v/2d0
c     TODO: Correct?
      ph_Zwidth = 2.4952d0
      ph_Wwidth = 2.141d0


      ph_sthw2 = 1-(ph_Wmass/ph_Zmass)**2
      ph_alphaem = g**2*gp**2/(g**2+gp**2)/16d0/atan(1d0) ! note it's at Q=MS!!!

      write(*,*) 'Running top mass ', ph_topmass
      write(*,*) 'Running bottom mass ', ph_bottommass
      write(*,*) 'Running Z mass ', ph_Zmass
      write(*,*) 'Running W mass ', ph_Wmass
      write(*,*) 'Running sin(thetaw)', ph_sthw2

c     compute the squark masses

      call diagonalize(ph_topmass,ph_Wmass,ph_Zmass,msql,mstr,At,
     $     ph_mumssm,ph_tanb,1,0,mstop2,ph_stt,ph_ctt)

      ph_t1 = sqrt(mstop2(1))
      ph_t1_2 = mstop2(1)
      ph_t2 = sqrt(mstop2(2))
      ph_t2_2 = mstop2(2)

      write(*,*) 'stop masses and sin(tht)',ph_t1,ph_t2,ph_stt

      call diagonalize(ph_bottommass,ph_Wmass,ph_Zmass,msql,msbr,Ab,
     $     ph_mumssm,ph_tanb,2,0,msbot2,ph_stb,ph_ctb)

      ph_b1 = sqrt(msbot2(1))
      ph_b1_2 = msbot2(1)
      ph_b2 = sqrt(msbot2(2))
      ph_b2_2 = msbot2(2)

      write(*,*) 'sbottom masses and sin(thb)',ph_b1,ph_b2,ph_stb

      mumass2 = q**2
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw2 = 1-ph_sthw2
      ph_cthw = sqrt(ph_cthw2)

      ph_alpha = alpha

      if (massren.eq.1) then
         ph_mbsb = ph_bottommass
         ph_mbpole = ph_bottommass
      else if (massren.eq.2) then
         write(*,*) 'Error: Mixed renormalization scheme
     $ still not implemented'
         stop
      else if (massren.eq.0) then
         write(*,*) 'Error: OS renormaliazation schceme
     $ still not implemented'
         stop
      endif

      if (ih.eq.1) then
         write(*,*) 'Light neutral CP-even Higgs selected'
         ph_Hmass = mh_1
         ph_Hmass2 = mh_1**2
      else
         write(*,*) 'Heavy neutral CP-even Higgs selected'
         ph_Hmass = mh_2
         ph_Hmass2 = mh_2**2
      endif

      flg_mssm_q_mh = int(powheginput('#mssmmbatmh'))
      if (flg_mssm_q_mh.eq.1) then
c     WARNING: commented in this SM only version.
c         call SLHAclear(slhadata)
c         call SLHARead(error, slhadata, "mssm-param-mh.slha", 0)
         if(error.ne.0) stop "Read error from mssm-param-mh.slha"
         write(*,*) "** SLHA file input parameters at scale MH **"
c     Tan beta@mh
         valname = "Tan(beta)@mh"
         ph_tanb_mh = checkvalue(HMix_TB,valname)
c     Vev
         valname = "Higgs vev@mh"
         v_mh = checkvalue(HMix_VEV,valname)
c     YD Block
         valname = "Yd block renormazation scale == mh?"
         q_yb_mh = checkvalue(Yd_Q,valname)
         valname = "Yb@mh"
         yb_mh = checkvalue(Yd_Yb,valname)

         ph_cb_mh = 1d0/sqrt(1+ph_tanb_mh**2)

         ph_bottommass_mh = yb_mh*v_mh/sqrt(2d0)*ph_cb_mh
         write(*,*) 'mb(mh) is', ph_bottommass_mh
      else
         ph_bottommass_mh = 0d0
      end if

      end

      real * 8 function checkvalue(val,valname)
      complex * 16 val
      character*50 valname
c     SLHALib definition
#include "SLHA.h"
      if (val.eq.invalid) then
         write(*,*) 'Invalid/Absent value in SLHA file for ', valname
         stop
      endif
      checkvalue = REAL(val)
      write(*,*) valname,checkvalue
      end

c     MSSM OS
c     This subroutine read the SUSY input parameters required for OS calculation
      subroutine OSsusyparameter()
      implicit none
      include 'PhysPars.h'
      include 'pwhg_math.h'
      real * 8 M2,M3,mstl,msbr,mstr,msf,At,Ab,Af,asq
      real * 8 mstop2(2),msbot2(2)
      real * 8 mh_int(2),v
c     TODO: REMOVE
      real * 8 mbpole,mtrun,mbrun,msbl
      real * 8 powheginput
      external powheginput

c     TODO: unify definition of OS weak boson masses somewhere...
      ph_Zmass  = 91.1876d0
      ph_Zwidth =  2.4952d0
      ph_Wmass  = 80.398d0
      ph_Wwidth =  2.141d0

      ph_asmz_nnlo = 0.118d0

      ph_GF= powheginput('gfermi')
      ph_tanb_mz = powheginput('tanb')
      ph_tanb = ph_tanb_mz
      ph_ma_mz = powheginput('mA')
      ph_MSUSY = powheginput('MSUSY')
      ph_topmass = powheginput('topmass')
      ph_bottommass = powheginput('bottommass')
      ph_mbmb = powheginput('mbmb')

      ph_cb = 1d0/sqrt(1+ph_tanb**2)
      ph_sb = ph_tanb*ph_cb

      M2 = ph_MSUSY
      M3 = ph_MSUSY
      mstl = ph_MSUSY
      msbr = 1.5d0*ph_MSUSY
      mstr = ph_MSUSY
      msf = ph_MSUSY
      At = ph_MSUSY
      Ab = ph_MSUSY
      Af = ph_MSUSY
      ph_mumssm = ph_MSUSY

      ph_mg = M3
c     WARNING: commented in this SM only version. The file with this subroutine is not present in this package.
c     From Feynhiggs we get the Higgs masses and the alpha mixing angle
c      call runFH(ph_ma_mz,M2,M3,mstl,mstr,msbr,msf,At,Ab,Af,ph_mumssm,
c     $ ph_tanb_mz,mh_int,ph_alpha)

      write(*,*) 'MH from FeynHiggs', mh_int
      write(*,*) 'alpha from FeynHiggs', ph_alpha

      if (ih.eq.1) then
         write(*,*) 'Light neutral CP-even Higgs selected'
         ph_Hmass = mh_int(1)
      else
         write(*,*) 'Heavy neutral CP-even Higgs selected'
         ph_Hmass = mh_int(2)
      end if
      ph_Hmass2 = ph_Hmass**2

c     Compute various physical quantities
      v = 1d0/sqrt(sqrt(2d0)*ph_GF)
      mbpole = 4.49d0           ! fix later -> TODO
      ph_sthw2 = 1-(ph_Wmass/ph_Zmass)**2
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw2 = 1-ph_sthw2
      ph_cthw = sqrt(ph_cthw2)

      ph_alphaem = ph_Wmass**2*ph_sthw2/v**2/pi
      ph_alphaemmz = ph_alphaem

      asq = ph_asmz_nnlo/(1 + (11 - 10/3D0)/(4*pi)*ph_asmz_nnlo*2
     $      *log(ph_topmass/ph_Zmass)) ! LO for coherence with FeynHiggs.

      q = ph_topmass
      mtrun = ph_topmass

c     WARNING: commented (and implementation removed in this SM version only)
c      call bottomshifts(ph_topmass,mtrun,mstl,mstr,msbr,ph_ma_mz,
c     $     Ab,At,ph_mg,ph_mumssm,
c     $     ph_tanb,q,v,ph_asmz_nnlo,asq,ph_Wmass,ph_Zmass,ph_mbmb,
c     $     mbrun,3,ph_mbsb,msbl)

      write(*,*) 'mtrun=',mtrun,', mbrun=',mbrun,
     $     ', mbOS=',ph_mbsb,', msbl=',msbl 
     $     ,', s2wrun =',ph_sthw2,', 1/aemrun =',1d0/ph_alphaem

c      q = 1d10*qq               ! check scale dependence

c     compute the squark masses
      call diagonalize(ph_topmass,ph_Wmass,ph_Zmass,mstl,mstr,At,
     $               ph_mumssm,ph_tanb,1,0,mstop2,ph_stt,ph_ctt)

      ph_t1 = sqrt(mstop2(1))
      ph_t1_2 = mstop2(1)
      ph_t2 = sqrt(mstop2(2))
      ph_t2_2 = mstop2(2)

      write(*,*) 'stop masses and sin(tht)',ph_t1,ph_t2,ph_stt

      call diagonalize(ph_mbsb,ph_Wmass,ph_Zmass,msbl,msbr,Ab,ph_mumssm,
     $ph_tanb,2,0,msbot2,ph_stb,ph_ctb)

      ph_b1 = sqrt(msbot2(1))
      ph_b1_2 = msbot2(1)
      ph_b2 = sqrt(msbot2(2))
      ph_b2_2 = msbot2(2)

      write(*,*) 'sbottom masses and sin(thb)',ph_b1,ph_b2,ph_stb


c     compute mb(qb) for the mixed scheme
c$$$
c$$$      fac = 1d0
c$$$      
c$$$      if(OS.eq.2) then
c$$$         stop
c$$$         q = mh(i_h)            ! the scale of mbrun
c$$$         
c$$$c         call topshift(mtpole,mstl,mstr,At,mg,mu,tanb,q,asq,mw,mz,mtrun)
c$$$         mtrun = mtpole
c$$$
c$$$         asq = asz
c$$$c$$$         asq = asf(q,mt,as,mz)         
c$$$c$$$         asq = asz/(1 + (11 - 10/3D0)/(4*pi)*asz*2*log(q/mz)) ! LO
c$$$      
c$$$         call bottomshifts(mtpole,mtrun,mstl,mstr,msbr,ma,Ab,At,mg,mu,
c$$$     $        tanb,q,v,asz,asq,mw,mz,mbmb,mbrun,3,dummy,msbl)
c$$$         
c$$$c         write(*,*) 'mtrun=',mtrun,', mbrun=',mbrun,
c$$$c     $        ', mbOS=',mbsb,', msbl=',msbl
c$$$
c$$$         fac = mbrun/mbpole
c$$$
c$$$      endif



c      stop

      end subroutine


      subroutine diagonalize(mq,mw,mz,msql,msqr,Aq,mu,tb,iq,lim,
     $     msq2,sth,cth)

      implicit none

      integer iq,lim
      double precision mq,mw,mz,msql,msqr,Aq,mu,tb,msq2(2),sth,cth
      double precision mq2,mz2,mw2,c2b,xq,yq,zq,tth,dx,dy

      mq2 = mq**2
      mz2 = mz**2
      mw2 = mw**2
      c2b = (1-tb**2)/(1+tb**2)

      if(lim.eq.0) then
         if(iq.eq.1) then
            zq = mq*(Aq+mu/tb)
            dx = 1d0/4d0*mz2*c2b
            dy = 1d0/12d0*(8*mw2-5*mz2)*c2b
         elseif(iq.eq.2) then
            zq = mq*(Aq+mu*tb)
            dx = -1d0/4d0*mz2*c2b
            dy = -1d0/12d0*(4*mw2-mz2)*c2b 
         else
            write(*,*) 'ERROR: iq out of range'
         endif
      else
c$$$         if(iq.eq.1) then
c$$$            zq = mq*Aq
c$$$            dx = -1d0/4d0*mz2
c$$$            dy = -1d0/12d0*(8*mw2-5*mz2)
c$$$         elseif(iq.eq.2) then
c$$$            zq = mq*mu*sqrt(1+tb**2)
c$$$            dx = 1d0/4d0*mz2
c$$$            dy = 1d0/12d0*(4*mw2-mz2)
c$$$         else
c$$$            write(*,*) 'ERROR: iq out of range'
c$$$         endif
         if(iq.eq.1) then
            zq = mq*Aq
            dx = 0d0!-1d0/4d0*mz2
            dy = 0d0!-1d0/12d0*(8*mw2-5*mz2)
         elseif(iq.eq.2) then
            mq2 = 0d0
            zq = mq*mu*sqrt(1+tb**2)
            dx = 0d0!1d0/4d0*mz2
            dy = 0d0!1d0/12d0*(4*mw2-mz2)
         else
            write(*,*) 'ERROR: iq out of range'
         endif
      endif

      xq = mq2 + 1d0/2d0*(msql**2+msqr**2) + dx
      yq = 1d0/2d0*(msql**2-msqr**2) + dy

      msq2(1) = xq + sqrt(yq**2+zq**2)
      msq2(2) = xq - sqrt(yq**2+zq**2)

      if(zq.eq.0.and.yq.ge.0)then
         cth=1d0
         sth=0d0
      elseif(zq.eq.0.and.yq.lt.0)then
         cth=0d0
         sth=1d0
      else
         tth = 1/zq*(sqrt(yq**2+zq**2)-yq)
         cth = 1/sqrt(1+tth**2)
         sth = cth*tth
      endif

      return
      end
