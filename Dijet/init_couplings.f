      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      logical verbose
      parameter(verbose=.true.)
      integer aemrun
      real *8 powheginput
      external powheginput
      real *8 alfaem,pwhg_alphas
      external alfaem,pwhg_alphas
      integer i,j
      real *8 alphaem_inv
      common/calphaem_inv/alphaem_inv

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     setting physical parameters
      write(*,*) 'POWHEG: loading-setting physical parameters'

c     Set also EW parameters. Otherwise FPE occurs in set_madgraph_parameters
      ph_topmass = 171.3d0
      ph_topwidth =1.31d0
      ph_Zmass  = 91.1876d0     
      ph_Zwidth =  2.4952d0
      ph_Wmass  = 80.398d0     
      ph_Wwidth =  2.141d0

      ph_alphaem = 1d0/137.035999679
      ph_sthw2 = abs(1d0-(ph_Wmass/ph_Zmass)**2)

c     CAVEAT: 
      ph_CKM(1,1)=0.975 
      ph_CKM(1,2)=0.222 
      ph_CKM(1,3)=1d-5
      ph_CKM(2,1)=0.222 
      ph_CKM(2,2)=0.975 
      ph_CKM(2,3)=1d-5
      ph_CKM(3,1)=1d-5
      ph_CKM(3,2)=1d-5
      ph_CKM(3,3)=1.0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   DEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw = sqrt(1-ph_sthw2)
      
      ph_topmass2 = ph_topmass**2

      ph_unit_e = sqrt(4*pi*ph_alphaem)


      if(verbose) then
         write(*,*) '--------------------------------------'
         write(*,*) 'POWHEG: RELEVANT PARAMETERS'
         write(*,*) 'lambda_QCD     ',st_lambda5MSB
         write(*,'(1X,A,f7.3,A,f15.7)') 'alpha_s(',91.2d0,')'
     $,pwhg_alphas(91.2d0**2,st_lambda5MSB,st_nlight)
         write(*,'(1X,A,f7.3,A,f15.7)') 'alpha_s(',ph_topmass,')'
     $,pwhg_alphas(ph_topmass**2,st_lambda5MSB,st_nlight)
         write(*,*) '--------------------------------------'
      endif

      end


