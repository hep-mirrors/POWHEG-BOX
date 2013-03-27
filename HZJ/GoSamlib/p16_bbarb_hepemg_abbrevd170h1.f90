module     p16_bbarb_hepemg_abbrevd170h1
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(11), public :: abb170
   complex(ki), public :: R2d170
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p16_bbarb_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_model
      use p16_bbarb_hepemg_color, only: TR
      use p16_bbarb_hepemg_globalsl1, only: epspow
      implicit none
      abb170(1)=1.0_ki/(-es61-es12+es345)
      abb170(2)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb170(3)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb170(4)=NC**(-1)
      abb170(5)=NC-abb170(4)
      abb170(6)=gHZZ*spbk4k1*c1*abb170(1)**2*i_*TR*gBr*gel*abb170(3)*abb170(2)
      abb170(7)=abb170(5)*abb170(6)*spak5k6*spak2e6
      abb170(8)=-spbk6e6*abb170(7)
      abb170(9)=-es61+es345-es12
      abb170(10)=-2.0_ki*abb170(8)*abb170(9)
      abb170(5)=-spak2k5*abb170(6)*abb170(5)
      abb170(6)=8.0_ki*abb170(9)*abb170(5)
      abb170(9)=-4.0_ki*abb170(8)
      abb170(8)=8.0_ki*abb170(8)
      abb170(11)=spae6k6*abb170(5)
      abb170(7)=abb170(7)+abb170(11)
      abb170(7)=4.0_ki*spbk6k2*abb170(7)
      abb170(5)=2.0_ki*spbk6e6*spak2e6*abb170(5)
      R2d170=0.0_ki
      rat2 = rat2 + R2d170
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='170' value='", &
          & R2d170, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd170h1
