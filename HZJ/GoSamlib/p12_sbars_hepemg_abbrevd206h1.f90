module     p12_sbars_hepemg_abbrevd206h1
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(30), public :: abb206
   complex(ki), public :: R2d206
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
      abb206(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb206(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb206(3)=spak1e6*spbe6k1
      abb206(4)=gHZZ*c1*NC*i_*TR*gSr*gel*abb206(2)*abb206(1)
      abb206(5)=abb206(4)*spbk4k1
      abb206(6)=abb206(5)*spak2k5
      abb206(7)=abb206(6)*abb206(3)
      abb206(8)=-es61+es345-es12
      abb206(9)=abb206(7)*abb206(8)
      abb206(10)=abb206(5)*spak5k6
      abb206(11)=spak2e6*abb206(10)*spbk6e6
      abb206(12)=2.0_ki*abb206(11)
      abb206(13)=es61*abb206(12)
      abb206(9)=abb206(13)+abb206(9)
      abb206(13)=-es12+es61
      abb206(13)=abb206(6)*abb206(13)
      abb206(14)=spbk6k4*spak2k6
      abb206(15)=abb206(4)*spbk6k1
      abb206(16)=spak5k6*abb206(15)
      abb206(17)=-abb206(16)*abb206(14)
      abb206(18)=spbk6k1*spak5k6
      abb206(19)=-spak1k2*abb206(5)*abb206(18)
      abb206(13)=abb206(19)+abb206(17)+abb206(13)
      abb206(13)=4.0_ki*abb206(13)
      abb206(17)=2.0_ki*abb206(4)
      abb206(18)=spak2e6*spbe6k4*abb206(18)*abb206(17)
      abb206(19)=abb206(7)+abb206(18)
      abb206(19)=2.0_ki*abb206(19)
      abb206(20)=8.0_ki*abb206(6)
      abb206(7)=abb206(18)-abb206(7)
      abb206(18)=abb206(4)*spbe6k1
      abb206(21)=abb206(18)*spak5e6
      abb206(22)=abb206(14)*abb206(21)
      abb206(22)=abb206(22)-abb206(7)
      abb206(22)=2.0_ki*abb206(22)
      abb206(23)=4.0_ki*abb206(6)
      abb206(24)=2.0_ki*abb206(21)
      abb206(25)=-abb206(14)*abb206(24)
      abb206(7)=-abb206(11)+abb206(25)+abb206(7)
      abb206(6)=2.0_ki*abb206(6)
      abb206(11)=spae6k6*spbk6k4
      abb206(25)=-spbk4k2*spak2e6
      abb206(11)=abb206(25)+abb206(11)
      abb206(11)=abb206(16)*abb206(11)
      abb206(25)=abb206(5)*spak5e6
      abb206(26)=es61*abb206(25)
      abb206(11)=abb206(26)+abb206(11)
      abb206(11)=2.0_ki*abb206(11)
      abb206(26)=-2.0_ki*abb206(25)
      abb206(27)=abb206(18)*spak2e6
      abb206(8)=abb206(27)*abb206(8)
      abb206(15)=-8.0_ki*spak2k6*abb206(15)
      abb206(28)=2.0_ki*abb206(27)
      abb206(17)=spae6k6*spbk6k1*abb206(17)
      abb206(29)=4.0_ki*abb206(4)
      abb206(14)=abb206(14)*abb206(29)
      abb206(3)=abb206(10)*abb206(3)
      abb206(30)=spbk4k2*abb206(21)*spak2k6
      abb206(3)=2.0_ki*abb206(3)+abb206(30)
      abb206(10)=4.0_ki*abb206(10)
      abb206(18)=2.0_ki*abb206(18)
      abb206(18)=spae6k6*abb206(18)
      abb206(4)=-spbk2k1*abb206(4)*spak2k5
      abb206(4)=-abb206(16)+abb206(4)
      abb206(4)=4.0_ki*abb206(4)
      abb206(5)=-4.0_ki*spak1k5*abb206(5)
      R2d206=0.0_ki
      rat2 = rat2 + R2d206
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='206' value='", &
          & R2d206, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd206h1
