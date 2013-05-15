module     p8_cbarc_hepemg_abbrevd268h0
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(22), public :: abb268
   complex(ki), public :: R2d268
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p8_cbarc_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_color, only: TR
      use p8_cbarc_hepemg_globalsl1, only: epspow
      implicit none
      abb268(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb268(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb268(3)=NC**(-1)
      abb268(4)=es61**(-1)
      abb268(5)=spak1e6*abb268(3)
      abb268(6)=NC*spak1e6
      abb268(7)=abb268(6)-abb268(5)
      abb268(8)=spbk6e6*spak5k6
      abb268(9)=spak1k5*spbe6k1
      abb268(10)=abb268(8)-abb268(9)
      abb268(7)=abb268(10)*abb268(7)
      abb268(10)=gCl*gel*abb268(1)*abb268(2)*gHZZ*c1*TR*i_
      abb268(11)=abb268(10)*abb268(4)
      abb268(12)=spbk4k2*abb268(11)
      abb268(13)=-abb268(7)*abb268(12)
      abb268(14)=-2.0_ki*abb268(8)+abb268(9)
      abb268(14)=abb268(14)*abb268(6)*abb268(10)*spbk4k2
      abb268(15)=NC*spak1k5
      abb268(16)=abb268(3)*spak1k5
      abb268(15)=abb268(15)+abb268(16)
      abb268(10)=4.0_ki*abb268(10)
      abb268(10)=-spbk4k2*abb268(15)*abb268(10)
      abb268(17)=abb268(9)*spak1e6
      abb268(18)=spbk6e6*spak1k6
      abb268(19)=abb268(18)*spak5e6
      abb268(17)=abb268(17)+abb268(19)
      abb268(17)=abb268(17)*NC
      abb268(19)=abb268(19)*abb268(3)
      abb268(17)=abb268(17)-abb268(19)
      abb268(9)=-abb268(9)*abb268(5)
      abb268(9)=abb268(9)-abb268(17)
      abb268(19)=4.0_ki*abb268(12)
      abb268(9)=abb268(9)*abb268(19)
      abb268(15)=abb268(15)*abb268(12)
      abb268(20)=16.0_ki*abb268(15)
      abb268(5)=abb268(8)*abb268(5)
      abb268(5)=abb268(5)+abb268(17)
      abb268(5)=abb268(5)*abb268(19)
      abb268(8)=-8.0_ki*abb268(15)
      abb268(12)=2.0_ki*abb268(12)
      abb268(7)=abb268(7)*abb268(12)
      abb268(12)=-spae6k6*spbk6k1*abb268(16)*abb268(12)
      abb268(6)=abb268(11)*abb268(6)
      abb268(15)=spak1k6*spbk6k2
      abb268(16)=spbe6k1*abb268(15)
      abb268(17)=3.0_ki*abb268(18)
      abb268(18)=spbk2k1*abb268(17)
      abb268(16)=abb268(16)+abb268(18)
      abb268(16)=abb268(16)*abb268(6)
      abb268(11)=8.0_ki*abb268(11)
      abb268(18)=NC+abb268(3)
      abb268(11)=abb268(11)*abb268(18)
      abb268(15)=-abb268(15)*abb268(11)
      abb268(21)=spak1k6*spbk6k4
      abb268(22)=-spbe6k1*abb268(21)
      abb268(17)=-spbk4k1*abb268(17)
      abb268(17)=abb268(22)+abb268(17)
      abb268(6)=abb268(17)*abb268(6)
      abb268(11)=abb268(21)*abb268(11)
      abb268(17)=abb268(19)*spak5k6*abb268(18)
      R2d268=abb268(13)
      rat2 = rat2 + R2d268
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='268' value='", &
          & R2d268, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd268h0
