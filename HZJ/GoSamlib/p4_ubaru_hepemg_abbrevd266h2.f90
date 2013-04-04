module     p4_ubaru_hepemg_abbrevd266h2
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(19), public :: abb266
   complex(ki), public :: R2d266
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p4_ubaru_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_color, only: TR
      use p4_ubaru_hepemg_globalsl1, only: epspow
      implicit none
      abb266(1)=1.0_ki/(-es61-es12+es345)
      abb266(2)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb266(3)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb266(4)=NC**(-1)
      abb266(5)=gUl*gHZZ*spak1k4*c1*i_*TR*ger*abb266(3)*abb266(2)*abb266(1)
      abb266(6)=abb266(4)*abb266(5)*spbk6k5
      abb266(7)=spae6k6*spbe6k2
      abb266(8)=abb266(7)*abb266(6)
      abb266(9)=abb266(5)*NC
      abb266(10)=abb266(9)*spbk6k5
      abb266(11)=abb266(7)*abb266(10)
      abb266(8)=abb266(8)-abb266(11)
      abb266(12)=-es61+es345-es12
      abb266(12)=2.0_ki*abb266(12)
      abb266(13)=abb266(11)*abb266(12)
      abb266(9)=abb266(9)*spbk5k2
      abb266(12)=-abb266(9)*abb266(12)
      abb266(14)=-4.0_ki*abb266(8)
      abb266(11)=4.0_ki*abb266(11)
      abb266(5)=abb266(5)*spbk5k2
      abb266(15)=abb266(4)*abb266(5)
      abb266(16)=abb266(9)+abb266(15)
      abb266(16)=8.0_ki*abb266(16)
      abb266(17)=2.0_ki*abb266(8)
      abb266(18)=-abb266(9)-2.0_ki*abb266(15)
      abb266(18)=spbk6e6*abb266(18)
      abb266(19)=-spbe6k2*abb266(10)
      abb266(18)=abb266(19)+abb266(18)
      abb266(18)=spak2k6*abb266(18)
      abb266(9)=-abb266(9)+abb266(15)
      abb266(9)=2.0_ki*spak2e6*spbk6e6*abb266(9)
      abb266(6)=abb266(10)+abb266(6)
      abb266(6)=4.0_ki*abb266(6)
      abb266(10)=2.0_ki*abb266(4)
      abb266(5)=-abb266(10)*abb266(7)*abb266(5)
      R2d266=-abb266(8)
      rat2 = rat2 + R2d266
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='266' value='", &
          & R2d266, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd266h2
