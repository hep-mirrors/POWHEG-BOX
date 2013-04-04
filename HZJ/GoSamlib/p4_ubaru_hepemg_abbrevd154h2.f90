module     p4_ubaru_hepemg_abbrevd154h2
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(21), public :: abb154
   complex(ki), public :: R2d154
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
      abb154(1)=1.0_ki/(-es61-es12+es345)
      abb154(2)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb154(3)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb154(4)=NC**(-1)
      abb154(5)=spae6k6*spak1k4
      abb154(6)=abb154(5)*spbk6k5
      abb154(7)=NC-abb154(4)
      abb154(7)=abb154(7)*gUl*gHZZ*c1*i_*TR*ger*abb154(3)*abb154(2)*abb154(1)
      abb154(8)=spbe6k2*abb154(7)
      abb154(9)=abb154(6)*abb154(8)
      abb154(10)=spak1k2*spbk5k2
      abb154(5)=-spbk6k1*abb154(10)*abb154(5)
      abb154(6)=es61*abb154(6)
      abb154(5)=abb154(6)+abb154(5)
      abb154(5)=2.0_ki*abb154(8)*abb154(5)
      abb154(6)=-es345+es12+es61
      abb154(11)=spbk5k2*spak1k4
      abb154(6)=abb154(11)*abb154(6)
      abb154(12)=spbk5k1*spak1k4
      abb154(13)=spbk6k2*spak1k6
      abb154(14)=-abb154(12)*abb154(13)
      abb154(6)=abb154(14)+abb154(6)
      abb154(6)=-abb154(7)*abb154(6)
      abb154(14)=spak1k6*spbk6k5
      abb154(10)=abb154(14)+abb154(10)
      abb154(14)=-spak4k6*abb154(10)*abb154(7)
      abb154(15)=-spbk6k2*abb154(14)
      abb154(6)=abb154(15)+abb154(6)
      abb154(6)=4.0_ki*abb154(6)
      abb154(10)=abb154(10)*abb154(8)
      abb154(15)=abb154(10)*spak4e6
      abb154(16)=abb154(8)*spak1e6
      abb154(17)=abb154(16)*abb154(12)
      abb154(15)=abb154(15)+abb154(17)
      abb154(15)=4.0_ki*abb154(15)
      abb154(17)=4.0_ki*abb154(9)
      abb154(18)=-2.0_ki*abb154(9)
      abb154(19)=abb154(7)*spbk6e6
      abb154(20)=-spak1k6*abb154(19)
      abb154(21)=abb154(8)*spak1k2
      abb154(20)=abb154(20)-abb154(21)
      abb154(12)=abb154(20)*abb154(12)
      abb154(14)=spbk6e6*abb154(14)
      abb154(10)=spak2k4*abb154(10)
      abb154(11)=abb154(11)*abb154(19)
      abb154(19)=-abb154(8)*spbk6k5*spak1k4
      abb154(11)=abb154(19)+abb154(11)
      abb154(11)=spak2k6*abb154(11)
      abb154(10)=abb154(11)+abb154(10)+abb154(12)+abb154(14)
      abb154(10)=2.0_ki*abb154(10)
      abb154(11)=2.0_ki*spae6k6
      abb154(12)=abb154(11)*spbk6k2
      abb154(14)=abb154(12)*abb154(21)
      abb154(7)=-8.0_ki*abb154(7)*abb154(13)
      abb154(13)=8.0_ki*abb154(16)
      abb154(16)=-4.0_ki*abb154(20)
      abb154(11)=abb154(8)*abb154(11)
      abb154(8)=spak2k4*abb154(12)*abb154(8)
      R2d154=abb154(9)
      rat2 = rat2 + R2d154
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='154' value='", &
          & R2d154, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd154h2
