      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      real * 8 masswindow
      logical verbose
      parameter(verbose=.true.)
      integer i,j
      real *8 powheginput
      external powheginput
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_Hmass = powheginput('hmass')
      ph_Hwidth = powheginput('hwidth')
      ph_Zmass  = 91.1876d0     
      ph_Zwidth =  2.4952d0
      ph_Wmass  = 80.398d0     
      ph_Wwidth =  2.141d0

      ph_alphaem = 1d0/137.035999679
      ph_sthw2 = abs(1d0-(ph_Wmass/ph_Zmass)**2)
      ph_GF= powheginput('gfermi') 
      ph_topmass = powheginput('topmass')
      
c$$$c     CAVEAT: 
c$$$      ph_CKM(1,1)=0.975 
c$$$      ph_CKM(1,2)=0.222 
c$$$      ph_CKM(1,3)=1d-5
c$$$      ph_CKM(2,1)=0.222 
c$$$      ph_CKM(2,2)=0.975 
c$$$      ph_CKM(2,3)=1d-5
c$$$      ph_CKM(3,1)=1d-5
c$$$      ph_CKM(3,2)=1d-5
c$$$      ph_CKM(3,3)=1.0

c     number of light flavors
      st_nlight = 5


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   DEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw = sqrt(1-ph_sthw2)
      
      ph_Hmass2 = ph_Hmass**2

c     set mass windows around H-mass peak in unit of ph_Hwidth
c     It is used in the generation of the Born phase space
C     masswindow is an optonal  parameter passed by the user
C     the default vale is 10 
      masswindow = powheginput("#masswindow")
      if(masswindow.lt.0d0) masswindow=10d0
c      ph_Hmass2low=(ph_Hmass-masswindow*ph_Hwidth)^2
      ph_Hmass2low=max(0d0,ph_Hmass-masswindow*ph_Hwidth)
      ph_Hmass2low= ph_Hmass2low**2
      ph_Hmass2high=(ph_Hmass+masswindow*ph_Hwidth)**2
      ph_HmHw = ph_Hmass * ph_Hwidth


      ph_unit_e = sqrt(4*pi*ph_alphaem)

      if(verbose) then
      write(*,*) '*************************************'
      write(*,*) 'H mass = ',ph_Hmass
      write(*,*) 'H width = ',ph_Hwidth
       write(*,*) '1/alphaem = ',1d0/ph_alphaem
      write(*,*) 'sthw2 = ',ph_sthw2
      write(*,*) 'GF = ',ph_GF
      write(*,*) 'top mass = ',ph_topmass
c      write(*,*) 'CKM matrix' 
c      do i=1,3
c         write(*,*) (ph_CKM(i,j),j=1,3)
c      enddo
      write(*,*) '*************************************'
      write(*,*)
      write(*,*) '*************************************'
      write(*,*) sqrt(ph_Hmass2low),' < M_H <',sqrt(ph_Hmass2high)
      write(*,*) '*************************************'
      endif
      end




