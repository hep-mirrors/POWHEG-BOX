      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include 'zcouple.f'  !TM now set the z-coupling parameters here
      include 'ewcharge.f'
      include 'qcdcouple.f'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      real * 8 masswindow_low,masswindow_high
      logical verbose
      parameter(verbose=.true.)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C      ph_Zmass  = 91.1876d0     
C      ph_Zwidth =  2.4952d0
C      ph_Wmass  = 80.398d0     
      ph_Wwidth =  2.141d0

C      ph_alphaem = (137.035999679)**-1

      !TM madgraph couplings
      ph_Zmass = 91.1880d0
      ph_Zwidth = 2.44140351d0
      ph_Wmass = 80.419d0
      ph_alphaem = 0.0948355370740407433d0/4d0/pi

      write(*,*)'alphaem',ph_alphaem

      ph_sthw2 = abs(1d0-(ph_Wmass/ph_Zmass)**2)


c     number of light flavors
      st_nlight = 5


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   DEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw = sqrt(1-ph_sthw2)
      ph_Zmass2 = ph_Zmass**2

c     set mass windows around Z-mass peak in unit of ph_Zwidth
c     It is used in the generation of the Born phase space
      masswindow_low = 10
      masswindow_high = 10
      ph_Zmass2low=max(0d0,ph_Zmass-masswindow_low*ph_Zwidth)
      ph_Zmass2low=ph_Zmass2low**2
      ph_Zmass2high=(ph_Zmass+masswindow_high*ph_Zwidth)**2
      ph_ZmZw = ph_Zmass * ph_Zwidth
      ph_unit_e = sqrt(4*pi*ph_alphaem)


      !TM added QCD couplings

      write(*,*)'alpha',st_alpha
      gsq = st_alpha*4d0*pi
      as  = st_alpha
      ason2pi = st_alpha/2d0/pi
      ason4pi = st_alpha/4d0/pi


      
      !TM added z couplings
      Q(-5)=+0.333333333333333d0
      Q(-4)=-0.666666666666667d0
      Q(-3)=+0.333333333333333d0
      Q(-2)=-0.666666666666667d0
      Q(-1)=+0.333333333333333d0
      Q(0)=+0d0
      Q(+1)=-0.333333333333333d0
      Q(+2)=+0.666666666666667d0
      Q(+3)=-0.333333333333333d0
      Q(+4)=+0.666666666666667d0
      Q(+5)=-0.333333333333333d0
      tau=(/1d0,-1d0,1d0,-1d0,1d0,0d0,-1d0,1d0,-1d0,1d0,-1d0/)
      esq = ph_unit_e**2
      zmass = ph_Zmass
      zwidth = ph_Zwidth
      call couplz(ph_sthw2)


      ! TM for the different processes the
      ! ew couplings need to be set, as in
      ! chooser.f. For now, ee,mumu
      ! ---really should depend on idvecdecay
      ! and ideally would be in init_process,
      ! but the above constatns need to be set
      q1=-1d0
      l1=le
      r1=re
      q2=-1d0
      l2=le
      r2=re


      if(verbose) then
      write(*,*) '*************************************'
      write(*,*) 'Z mass = ',ph_Zmass
      write(*,*) 'Z width = ',ph_Zwidth
      write(*,*) 'W mass = ',ph_Wmass
      write(*,*) 'W width = ',ph_Wwidth
      write(*,*) '1/alphaem = ',1d0/ph_alphaem
      write(*,*) 'sthw2 = ',ph_sthw2
      write(*,*) 'e**2  = ',ph_unit_e**2
      write(*,*) '*************************************'
      endif

      end



