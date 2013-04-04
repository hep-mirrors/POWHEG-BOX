module     p12_sbars_hepemg_abbrevd268h3
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(21), public :: abb268
   complex(ki), public :: R2d268
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p12_sbars_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_color, only: TR
      use p12_sbars_hepemg_globalsl1, only: epspow
      implicit none
      abb268(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb268(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb268(3)=NC**(-1)
      abb268(4)=es61**(-1)
      abb268(5)=spbe6k1*abb268(3)
      abb268(6)=NC*spbe6k1
      abb268(7)=abb268(6)-abb268(5)
      abb268(8)=spae6k6*spbk6k5
      abb268(9)=spbk5k1*spak1e6
      abb268(10)=abb268(8)-abb268(9)
      abb268(7)=abb268(10)*abb268(7)
      abb268(10)=gSr*ger*abb268(1)*abb268(2)*gHZZ*c1*TR*i_
      abb268(11)=abb268(10)*abb268(4)
      abb268(12)=spak2k4*abb268(11)
      abb268(13)=-abb268(7)*abb268(12)
      abb268(14)=-2.0_ki*abb268(8)+abb268(9)
      abb268(14)=abb268(14)*abb268(6)*abb268(10)*spak2k4
      abb268(15)=NC*spbk5k1
      abb268(16)=abb268(3)*spbk5k1
      abb268(15)=abb268(15)+abb268(16)
      abb268(10)=4.0_ki*abb268(10)
      abb268(10)=-spak2k4*abb268(15)*abb268(10)
      abb268(17)=abb268(9)*spbe6k1
      abb268(18)=spae6k6*spbk6k1
      abb268(19)=abb268(18)*spbe6k5
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
      abb268(12)=-spbk6e6*spak1k6*abb268(16)*abb268(12)
      abb268(15)=NC+abb268(3)
      abb268(16)=abb268(19)*spbk6k5*abb268(15)
      abb268(6)=abb268(11)*abb268(6)
      abb268(17)=spbk6k1*spak2k6
      abb268(19)=spak1e6*abb268(17)
      abb268(18)=3.0_ki*abb268(18)
      abb268(21)=spak1k2*abb268(18)
      abb268(19)=abb268(19)+abb268(21)
      abb268(19)=abb268(19)*abb268(6)
      abb268(11)=8.0_ki*abb268(11)
      abb268(11)=abb268(11)*abb268(15)
      abb268(15)=-abb268(17)*abb268(11)
      abb268(17)=spbk6k1*spak4k6
      abb268(21)=-spak1e6*abb268(17)
      abb268(18)=-spak1k4*abb268(18)
      abb268(18)=abb268(21)+abb268(18)
      abb268(6)=abb268(18)*abb268(6)
      abb268(11)=abb268(17)*abb268(11)
      R2d268=abb268(13)
      rat2 = rat2 + R2d268
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='268' value='", &
          & R2d268, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd268h3
