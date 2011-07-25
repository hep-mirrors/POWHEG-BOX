      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include 'zcouple.f'  !TM now set the z-coupling parameters here
      include 'ewcharge.f'
      include 'qcdcouple.f'
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      include 'pwhg_physpar.h'
      logical verbose
      parameter(verbose=.true.)
      physpar_ml(1)=0.511d-3
      physpar_ml(2)=0.1057d0
      physpar_ml(3)=1.777d0
      physpar_mq(1)=0.33d0     ! up
      physpar_mq(2)=0.33d0     ! down
      physpar_mq(3)=0.50d0     ! strange
      physpar_mq(4)=1.50d0     ! charm
      physpar_mq(5)=4.80d0     ! bottom

      call smcouplings

c     number of light flavors
      st_nlight = 5

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



