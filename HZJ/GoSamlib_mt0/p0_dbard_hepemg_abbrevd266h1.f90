module     p0_dbard_hepemg_abbrevd266h1
   use p0_dbard_hepemg_config, only: ki
   use p0_dbard_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(19), public :: abb266
   complex(ki), public :: R2d266
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
      abb266(1)=1.0_ki/(-es61-es12+es345)
      abb266(2)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb266(3)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb266(4)=NC**(-1)
      abb266(5)=gDr*gHZZ*spbk4k1*c1*i_*TR*gel*abb266(3)*abb266(2)*abb266(1)
      abb266(6)=abb266(4)*abb266(5)*spak5k6
      abb266(7)=spbk6e6*spak2e6
      abb266(8)=abb266(7)*abb266(6)
      abb266(9)=abb266(5)*NC
      abb266(10)=abb266(9)*spak5k6
      abb266(11)=abb266(7)*abb266(10)
      abb266(8)=abb266(8)-abb266(11)
      abb266(12)=-es61+es345-es12
      abb266(12)=2.0_ki*abb266(12)
      abb266(13)=abb266(11)*abb266(12)
      abb266(9)=abb266(9)*spak2k5
      abb266(12)=-abb266(9)*abb266(12)
      abb266(14)=-4.0_ki*abb266(8)
      abb266(11)=4.0_ki*abb266(11)
      abb266(5)=abb266(5)*spak2k5
      abb266(15)=abb266(4)*abb266(5)
      abb266(16)=abb266(9)+abb266(15)
      abb266(16)=8.0_ki*abb266(16)
      abb266(17)=2.0_ki*abb266(8)
      abb266(18)=-abb266(9)-2.0_ki*abb266(15)
      abb266(18)=spae6k6*abb266(18)
      abb266(19)=-spak2e6*abb266(10)
      abb266(18)=abb266(19)+abb266(18)
      abb266(18)=spbk6k2*abb266(18)
      abb266(19)=2.0_ki*abb266(4)
      abb266(5)=-abb266(19)*abb266(7)*abb266(5)
      abb266(7)=-abb266(9)+abb266(15)
      abb266(7)=2.0_ki*spbe6k2*spae6k6*abb266(7)
      abb266(6)=abb266(10)+abb266(6)
      abb266(6)=4.0_ki*abb266(6)
      R2d266=-abb266(8)
      rat2 = rat2 + R2d266
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='266' value='", &
          & R2d266, "'/>"
      end if
   end subroutine
end module p0_dbard_hepemg_abbrevd266h1
