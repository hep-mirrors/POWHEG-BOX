module     p16_bbarb_hepemg_abbrevd533h2
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(32), public :: abb533
   complex(ki), public :: R2d533
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
      abb533(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb533(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb533(3)=NC**(-1)
      abb533(4)=spbk6e6*spak4k6
      abb533(5)=spak1k4*spbe6k1
      abb533(4)=abb533(4)-abb533(5)
      abb533(4)=abb533(4)*spbk5k2
      abb533(6)=gBl*ger*abb533(3)*abb533(1)*abb533(2)*gHZZ*c1*TR*i_
      abb533(7)=2.0_ki*abb533(6)
      abb533(8)=abb533(7)*spak1e6
      abb533(9)=abb533(8)*es12*abb533(4)
      abb533(10)=spak1k4*es12
      abb533(11)=-spak1k2*spak4k6*spbk6k2
      abb533(10)=abb533(11)+abb533(10)
      abb533(11)=4.0_ki*abb533(6)
      abb533(12)=abb533(11)*spbk5k2
      abb533(10)=abb533(10)*abb533(12)
      abb533(13)=spbe6k2*spak1k2
      abb533(14)=spbk5k2*spak4e6
      abb533(15)=abb533(13)*abb533(14)
      abb533(16)=abb533(11)*abb533(15)
      abb533(4)=spak1e6*abb533(4)
      abb533(4)=-abb533(15)+abb533(4)
      abb533(4)=abb533(4)*abb533(11)
      abb533(6)=8.0_ki*spak1k4*abb533(6)*spbk5k2
      abb533(17)=-spbe6k5*spbk2k1
      abb533(18)=spbe6k2*spbk5k1
      abb533(17)=abb533(17)+abb533(18)
      abb533(17)=abb533(17)*spak1e6
      abb533(18)=-spbe6k2*spae6k6*spbk6k5
      abb533(17)=abb533(17)+abb533(18)
      abb533(17)=spak1k4*abb533(17)
      abb533(15)=abb533(15)+abb533(17)
      abb533(15)=abb533(15)*abb533(7)
      abb533(17)=abb533(11)*spak1k4
      abb533(18)=-spbk5k2*abb533(17)
      abb533(19)=abb533(17)*spbe6k5
      abb533(20)=spbe6k5*abb533(7)*spak1k4
      abb533(21)=abb533(14)*abb533(11)
      abb533(14)=-abb533(7)*abb533(14)
      abb533(22)=spbk6e6*spbk5k1
      abb533(23)=abb533(22)*abb533(8)*spak1k4
      abb533(24)=-spbk6k5*abb533(17)
      abb533(25)=abb533(13)*spae6k6
      abb533(26)=spbk6k2*abb533(7)*abb533(25)
      abb533(27)=abb533(11)*spak1e6
      abb533(28)=spbe6k2*abb533(27)
      abb533(29)=spak1k6*abb533(7)*spbk6e6
      abb533(30)=-spae6k6*spbk6k2
      abb533(31)=spak1e6*spbk2k1
      abb533(30)=abb533(30)+abb533(31)
      abb533(30)=abb533(30)*abb533(7)
      abb533(31)=abb533(8)*spbk6e6
      abb533(22)=-spak1k6*abb533(22)
      abb533(32)=-spbe6k5*es12
      abb533(13)=spbk5k1*abb533(13)
      abb533(13)=abb533(13)+abb533(22)+abb533(32)
      abb533(13)=spak1e6*abb533(13)
      abb533(22)=-spbk6k5*abb533(25)
      abb533(13)=abb533(22)+abb533(13)
      abb533(7)=abb533(13)*abb533(7)
      abb533(13)=spbk6k5*spak1k6
      abb533(22)=-spbk5k2*spak1k2
      abb533(13)=abb533(13)+abb533(22)
      abb533(13)=abb533(13)*abb533(11)
      abb533(22)=abb533(27)*spbe6k5
      abb533(25)=-spbe6k5*abb533(8)
      abb533(12)=spak4k6*abb533(12)
      abb533(5)=abb533(5)*abb533(8)*spbk5k1
      abb533(8)=-spbk5k1*abb533(17)
      abb533(17)=-spbe6k1*abb533(27)
      R2d533=0.0_ki
      rat2 = rat2 + R2d533
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='533' value='", &
          & R2d533, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd533h2
