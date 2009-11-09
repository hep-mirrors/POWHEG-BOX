      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      real * 8 masswindow
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_Zmass  = 91.1876d0     
      ph_Zwidth =  2.4952d0

      ph_alphaem = 1d0/137.03599
      ph_sthw2 = 0.2226458534

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
      masswindow = 10
      ph_Zmass2low=(ph_Zmass-masswindow*ph_Zwidth)**2
      ph_Zmass2high=(ph_Zmass+masswindow*ph_Zwidth)**2
      ph_ZmZw = ph_Zmass * ph_Zwidth
      ph_unit_e = sqrt(4*pi*ph_alphaem)

      end



