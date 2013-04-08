module     p4_ubaru_hepemg_abbrevd206h2
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(29), public :: abb206
   complex(ki), public :: R2d206
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
      abb206(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb206(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb206(3)=spbe6k1*spak1e6
      abb206(4)=gHZZ*c1*NC*i_*TR*gUl*ger*abb206(2)*abb206(1)
      abb206(5)=abb206(4)*spak1k4
      abb206(6)=abb206(5)*spbk5k2
      abb206(7)=abb206(6)*abb206(3)
      abb206(8)=-es61+es345-es12
      abb206(9)=abb206(7)*abb206(8)
      abb206(10)=abb206(5)*spbk6k5
      abb206(11)=spbe6k2*abb206(10)*spae6k6
      abb206(12)=2.0_ki*abb206(11)
      abb206(13)=es61*abb206(12)
      abb206(9)=abb206(13)+abb206(9)
      abb206(13)=-es12+es61
      abb206(13)=abb206(6)*abb206(13)
      abb206(14)=spak4k6*spbk6k2
      abb206(15)=abb206(4)*spak1k6
      abb206(16)=spbk6k5*abb206(15)
      abb206(17)=-abb206(16)*abb206(14)
      abb206(18)=spak1k6*spbk6k5
      abb206(19)=-spbk2k1*abb206(5)*abb206(18)
      abb206(13)=abb206(19)+abb206(17)+abb206(13)
      abb206(13)=4.0_ki*abb206(13)
      abb206(17)=2.0_ki*abb206(4)
      abb206(18)=spbe6k2*spak4e6*abb206(18)*abb206(17)
      abb206(19)=abb206(7)+abb206(18)
      abb206(19)=2.0_ki*abb206(19)
      abb206(20)=8.0_ki*abb206(6)
      abb206(7)=abb206(18)-abb206(7)
      abb206(18)=abb206(4)*spak1e6
      abb206(21)=abb206(18)*spbe6k5
      abb206(22)=abb206(14)*abb206(21)
      abb206(22)=abb206(22)-abb206(7)
      abb206(22)=2.0_ki*abb206(22)
      abb206(23)=4.0_ki*abb206(6)
      abb206(24)=2.0_ki*abb206(21)
      abb206(25)=-abb206(14)*abb206(24)
      abb206(7)=-abb206(11)+abb206(25)+abb206(7)
      abb206(6)=2.0_ki*abb206(6)
      abb206(11)=spbk6e6*spak4k6
      abb206(25)=-spak2k4*spbe6k2
      abb206(11)=abb206(25)+abb206(11)
      abb206(11)=abb206(16)*abb206(11)
      abb206(25)=abb206(5)*spbe6k5
      abb206(26)=es61*abb206(25)
      abb206(11)=abb206(26)+abb206(11)
      abb206(11)=2.0_ki*abb206(11)
      abb206(26)=-2.0_ki*abb206(25)
      abb206(3)=abb206(10)*abb206(3)
      abb206(27)=spak2k4*abb206(21)*spbk6k2
      abb206(3)=2.0_ki*abb206(3)+abb206(27)
      abb206(10)=4.0_ki*abb206(10)
      abb206(27)=abb206(18)*spbe6k2
      abb206(8)=abb206(27)*abb206(8)
      abb206(15)=-8.0_ki*spbk6k2*abb206(15)
      abb206(28)=2.0_ki*abb206(27)
      abb206(17)=spbk6e6*spak1k6*abb206(17)
      abb206(18)=2.0_ki*abb206(18)
      abb206(18)=spbk6e6*abb206(18)
      abb206(29)=-spak1k2*abb206(4)*spbk5k2
      abb206(16)=-abb206(16)+abb206(29)
      abb206(16)=4.0_ki*abb206(16)
      abb206(4)=4.0_ki*abb206(4)
      abb206(14)=abb206(14)*abb206(4)
      abb206(5)=-4.0_ki*spbk5k1*abb206(5)
      R2d206=0.0_ki
      rat2 = rat2 + R2d206
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='206' value='", &
          & R2d206, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd206h2