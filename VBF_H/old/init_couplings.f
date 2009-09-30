      subroutine init_couplings
      implicit none
      include '../include/PhysPars.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      real * 8 pwhg_alphas
      external pwhg_alphas
      integer i,j

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_Zmass  = 91.188d0     
      ph_Zwidth =  2.486d0
      ph_Wmass  = 80.419d0     
      ph_Wwidth =  2.064d0

      ph_alphaem = 1d0/128.930d0
      ph_sthw2 = 0.2312d0

      ph_CKM(1,1)=0.9748 	
      ph_CKM(1,2)=0.2225  	 
      ph_CKM(1,3)=0.0036  	
      ph_CKM(2,1)=0.2225  	
      ph_CKM(2,2)=0.9740 	
      ph_CKM(2,3)=0.041	
      ph_CKM(3,1)=0.009    
      ph_CKM(3,2)=0.0405   
      ph_CKM(3,3)=0.9992

      st_nlight = 5
c  MRST set uses  alpha_s(M_Z) =   0.119118459
c      lambda5 = 0.24075d0       ! this gives as = 0.119118459

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   DEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw = sqrt(1-ph_sthw2)
      ph_Zmass2 = ph_Zmass**2
c CAVEAT 10 dovrebbe diventare un parametro da passare
      ph_Zmass2low=(ph_Zmass-10*ph_Zwidth)**2
      ph_Zmass2high=(ph_Zmass+10*ph_Zwidth)**2
      ph_ZmZw = ph_Zmass * ph_Zwidth
      ph_Wmass2 = ph_Wmass**2
      ph_Wmass2low=(ph_Wmass-10*ph_Wwidth)**2
      ph_Wmass2high=(ph_Wmass+10*ph_Wwidth)**2
      ph_WmWw = ph_Wmass * ph_Wwidth

      ph_unit_e = sqrt(4*pi*ph_alphaem)

      write(*,*) '*************************************'
      write(*,*) 'Z mass = ',ph_Zmass
      write(*,*) 'Z width = ',ph_Zwidth
      write(*,*) 'W mass = ',ph_Wmass
      write(*,*) 'W width = ',ph_Wwidth
      write(*,*) '1/alphaem = ',1d0/ph_alphaem
      write(*,*) 'sthw2 = ',ph_sthw2
      write(*,*) 'CKM matrix ' 
      do i=1,3
         write(*,*) (ph_CKM(i,j),j=1,3)
      enddo
      write(*,*) 'Lambda5MSB = ',st_Lambda5MSB
      write(*,*) 'alpha_s(M_Z) = ',
     #     pwhg_alphas(ph_Zmass**2,st_Lambda5MSB,-1)
      write(*,*) '*************************************'
      end

