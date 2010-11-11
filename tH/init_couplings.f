      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      real * 8 masswindow
      real * 8 powheginput
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	
	ph_mCH = powheginput("masshiggsch")
	ph_mT = 172.d0
        ph_Twidth = powheginput("#topwidth")
        if(ph_Twidth.lt.0) ph_Twidth=1.32d0
	
	ph_tanb = 30.d0
	ph_GF = 1.16637d-5
c POWHEG provides alpha/(2 pi)
c	ph_alphas = 2*pi
		
	
      ph_Zmass  = 91.188d0     
      ph_Zwidth =  2.486d0
      ph_Wmass  = 79.964d0
      ph_Wwidth =  2.064d0

c      ph_mb=4.25d0
	ph_mb=4.45d0


c check why changes if ph_Hmass is removed!	
	ph_Hmass  = 1000d0

      ph_Hwidth = 0.0033d0


       
      ph_alphaem = 1d0/128.930d0
c      ph_sthw2 = 0.2312d0
      ph_sthw2 = 0.23102d0

c     number of light flavors
      st_nlight = 5

      ph_CKM(1,1)=0.9748 	
      ph_CKM(1,2)=0.2225  	 
      ph_CKM(1,3)=0.0036  	
      ph_CKM(2,1)=0.2225  	
      ph_CKM(2,2)=0.9740 	
      ph_CKM(2,3)=0.041	
      ph_CKM(3,1)=0.009    
      ph_CKM(3,2)=0.0405   
      ph_CKM(3,3)=0.9992

c     initialize CKM with flavor indexes
      call inizialize_ph_CKM_matrix

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   DEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      ph_A = ph_mt/ph_tanb
c      ph_B = ph_mb*ph_tanb

c      ph_A = 163.578674451712/ph_tanb
c      ph_B = 2.44944690952071*ph_tanb


      ph_A = 1 ! will be computed later by the born subroutine
      ph_B = 1 ! will be computed later by the born subroutine
      
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw = sqrt(1-ph_sthw2)
      ph_Zmass2 = ph_Zmass**2
      ph_Wmass2 = ph_Wmass**2
      ph_Hmass2 = ph_Hmass**2
      ph_ZmZw = ph_Zmass * ph_Zwidth
      ph_WmWw = ph_Wmass * ph_Wwidth
      ph_HmHw = ph_Hmass * ph_Hwidth


c     set mass windows around Z-mass peak in unit of ph_Zwidth
c     It is used in the generation of the Born phase space
      masswindow = 30
c      ph_Zmass2low=(ph_Zmass-masswindow*ph_Zwidth)**2
c      ph_Zmass2high=(ph_Zmass+masswindow*ph_Zwidth)**2

      ph_Hmass2low=(ph_Hmass-masswindow*ph_Hwidth)**2
      ph_Hmass2high=(ph_Hmass+masswindow*ph_Hwidth)**2
c      ph_Hmass2low=0d0
c      ph_Hmass2high=kn_sbeams/4
    
      ph_unit_e = sqrt(4*pi*ph_alphaem)
      end


      subroutine inizialize_ph_CKM_matrix
      implicit none     
      include 'PhysPars.h'  
      integer i,j
      do i=1,6
         do j=1,6
            ph_CKM_matrix(i,j) = 0
         enddo
      enddo
      ph_CKM_matrix(1,2) = ph_CKM(1,1)
      ph_CKM_matrix(1,4) = ph_CKM(2,1)
      ph_CKM_matrix(1,6) = ph_CKM(3,1)
      ph_CKM_matrix(2,3) = ph_CKM(1,2)
      ph_CKM_matrix(2,5) = ph_CKM(1,3)
      ph_CKM_matrix(3,4) = ph_CKM(2,2)
      ph_CKM_matrix(3,6) = ph_CKM(3,2)
      ph_CKM_matrix(4,5) = ph_CKM(2,3)
      ph_CKM_matrix(5,6) = ph_CKM(3,3)
      do i=1,6
         do j=i+1,6
            ph_CKM_matrix(j,i) = ph_CKM_matrix(i,j)
         enddo
      enddo
      end


