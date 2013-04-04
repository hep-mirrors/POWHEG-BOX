module     p0_dbard_hepemg_abbrevd196h2
   use p0_dbard_hepemg_config, only: ki
   use p0_dbard_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(11), public :: abb196
   complex(ki), public :: R2d196
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p0_dbard_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_color, only: TR
      use p0_dbard_hepemg_globalsl1, only: epspow
      implicit none
      abb196(1)=1.0_ki/(-es61-es12+es345)
      abb196(2)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb196(3)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb196(4)=NC**(-1)
      abb196(5)=NC-abb196(4)
      abb196(6)=gHZZ*spak1k4*c1*abb196(1)**2*i_*TR*gDl*ger*abb196(3)*abb196(2)
      abb196(7)=abb196(5)*abb196(6)*spbk6k5*spbe6k2
      abb196(8)=-spae6k6*abb196(7)
      abb196(9)=-es61+es345-es12
      abb196(10)=-2.0_ki*abb196(8)*abb196(9)
      abb196(5)=-spbk5k2*abb196(6)*abb196(5)
      abb196(6)=8.0_ki*abb196(9)*abb196(5)
      abb196(9)=-4.0_ki*abb196(8)
      abb196(8)=8.0_ki*abb196(8)
      abb196(11)=spbk6e6*abb196(5)
      abb196(7)=abb196(7)+abb196(11)
      abb196(7)=4.0_ki*spak2k6*abb196(7)
      abb196(5)=2.0_ki*spae6k6*spbe6k2*abb196(5)
      R2d196=0.0_ki
      rat2 = rat2 + R2d196
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='196' value='", &
          & R2d196, "'/>"
      end if
   end subroutine
end module p0_dbard_hepemg_abbrevd196h2
