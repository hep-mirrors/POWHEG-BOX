module     p16_bbarb_hepemg_abbrevd233h2
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(32), public :: abb233
   complex(ki), public :: R2d233
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
      abb233(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb233(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb233(3)=NC**(-1)
      abb233(4)=spbk6e6*spak4k6
      abb233(5)=spak1k4*spbe6k1
      abb233(4)=abb233(4)-abb233(5)
      abb233(4)=abb233(4)*spbk5k2
      abb233(6)=gBl*ger*abb233(3)*abb233(1)*abb233(2)*gHZZ*c1*TR*i_
      abb233(7)=2.0_ki*abb233(6)
      abb233(8)=abb233(7)*spak1e6
      abb233(9)=abb233(8)*es12*abb233(4)
      abb233(10)=spak1k4*es12
      abb233(11)=-spak1k2*spak4k6*spbk6k2
      abb233(10)=abb233(11)+abb233(10)
      abb233(11)=4.0_ki*abb233(6)
      abb233(12)=abb233(11)*spbk5k2
      abb233(10)=abb233(10)*abb233(12)
      abb233(13)=spbe6k2*spak1k2
      abb233(14)=spbk5k2*spak4e6
      abb233(15)=abb233(13)*abb233(14)
      abb233(16)=abb233(11)*abb233(15)
      abb233(4)=spak1e6*abb233(4)
      abb233(4)=-abb233(15)+abb233(4)
      abb233(4)=abb233(4)*abb233(11)
      abb233(6)=8.0_ki*spak1k4*abb233(6)*spbk5k2
      abb233(17)=-spbe6k5*spbk2k1
      abb233(18)=spbe6k2*spbk5k1
      abb233(17)=abb233(17)+abb233(18)
      abb233(17)=abb233(17)*spak1e6
      abb233(18)=-spbe6k2*spae6k6*spbk6k5
      abb233(17)=abb233(17)+abb233(18)
      abb233(17)=spak1k4*abb233(17)
      abb233(15)=abb233(15)+abb233(17)
      abb233(15)=abb233(15)*abb233(7)
      abb233(17)=abb233(11)*spak1k4
      abb233(18)=-spbk5k2*abb233(17)
      abb233(19)=abb233(17)*spbe6k5
      abb233(20)=spbe6k5*abb233(7)*spak1k4
      abb233(21)=abb233(14)*abb233(11)
      abb233(14)=-abb233(7)*abb233(14)
      abb233(22)=spbk6e6*spbk5k1
      abb233(23)=abb233(22)*abb233(8)*spak1k4
      abb233(24)=-spbk6k5*abb233(17)
      abb233(25)=abb233(13)*spae6k6
      abb233(26)=spbk6k2*abb233(7)*abb233(25)
      abb233(27)=abb233(11)*spak1e6
      abb233(28)=spbe6k2*abb233(27)
      abb233(29)=spak1k6*abb233(7)*spbk6e6
      abb233(30)=-spae6k6*spbk6k2
      abb233(31)=spak1e6*spbk2k1
      abb233(30)=abb233(30)+abb233(31)
      abb233(30)=abb233(30)*abb233(7)
      abb233(31)=abb233(8)*spbk6e6
      abb233(22)=-spak1k6*abb233(22)
      abb233(32)=-spbe6k5*es12
      abb233(13)=spbk5k1*abb233(13)
      abb233(13)=abb233(13)+abb233(22)+abb233(32)
      abb233(13)=spak1e6*abb233(13)
      abb233(22)=-spbk6k5*abb233(25)
      abb233(13)=abb233(22)+abb233(13)
      abb233(7)=abb233(13)*abb233(7)
      abb233(13)=spbk6k5*spak1k6
      abb233(22)=-spbk5k2*spak1k2
      abb233(13)=abb233(13)+abb233(22)
      abb233(13)=abb233(13)*abb233(11)
      abb233(22)=abb233(27)*spbe6k5
      abb233(25)=-spbe6k5*abb233(8)
      abb233(12)=spak4k6*abb233(12)
      abb233(5)=abb233(5)*abb233(8)*spbk5k1
      abb233(8)=-spbk5k1*abb233(17)
      abb233(17)=-spbe6k1*abb233(27)
      R2d233=0.0_ki
      rat2 = rat2 + R2d233
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='233' value='", &
          & R2d233, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd233h2
