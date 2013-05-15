module     p16_bbarb_hepemg_abbrevd235h0
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(29), public :: abb235
   complex(ki), public :: R2d235
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
      abb235(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb235(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb235(3)=spbe6k1*spak1e6
      abb235(4)=gHZZ*c1*NC*i_*TR*gBl*gel*abb235(2)*abb235(1)
      abb235(5)=abb235(4)*spak1k5
      abb235(6)=abb235(5)*spbk4k2
      abb235(7)=abb235(6)*abb235(3)
      abb235(8)=-es61+es345-es12
      abb235(9)=abb235(7)*abb235(8)
      abb235(10)=abb235(5)*spbk6k4
      abb235(11)=spbe6k2*abb235(10)*spae6k6
      abb235(12)=2.0_ki*abb235(11)
      abb235(13)=es61*abb235(12)
      abb235(9)=abb235(13)+abb235(9)
      abb235(13)=-es12+es61
      abb235(13)=abb235(6)*abb235(13)
      abb235(14)=spak5k6*spbk6k2
      abb235(15)=abb235(4)*spak1k6
      abb235(16)=spbk6k4*abb235(15)
      abb235(17)=-abb235(16)*abb235(14)
      abb235(18)=spak1k6*spbk6k4
      abb235(19)=-spbk2k1*abb235(5)*abb235(18)
      abb235(13)=abb235(19)+abb235(17)+abb235(13)
      abb235(13)=4.0_ki*abb235(13)
      abb235(17)=2.0_ki*abb235(4)
      abb235(18)=spbe6k2*spak5e6*abb235(18)*abb235(17)
      abb235(19)=abb235(7)+abb235(18)
      abb235(19)=2.0_ki*abb235(19)
      abb235(20)=8.0_ki*abb235(6)
      abb235(7)=abb235(18)-abb235(7)
      abb235(18)=abb235(4)*spak1e6
      abb235(21)=abb235(18)*spbe6k4
      abb235(22)=abb235(14)*abb235(21)
      abb235(22)=abb235(22)-abb235(7)
      abb235(22)=2.0_ki*abb235(22)
      abb235(23)=4.0_ki*abb235(6)
      abb235(24)=2.0_ki*abb235(21)
      abb235(25)=-abb235(14)*abb235(24)
      abb235(7)=-abb235(11)+abb235(25)+abb235(7)
      abb235(6)=2.0_ki*abb235(6)
      abb235(11)=spbk6e6*spak5k6
      abb235(25)=-spak2k5*spbe6k2
      abb235(11)=abb235(25)+abb235(11)
      abb235(11)=abb235(16)*abb235(11)
      abb235(25)=abb235(5)*spbe6k4
      abb235(26)=es61*abb235(25)
      abb235(11)=abb235(26)+abb235(11)
      abb235(11)=2.0_ki*abb235(11)
      abb235(26)=-2.0_ki*abb235(25)
      abb235(3)=abb235(10)*abb235(3)
      abb235(27)=spak2k5*abb235(21)*spbk6k2
      abb235(3)=2.0_ki*abb235(3)+abb235(27)
      abb235(10)=4.0_ki*abb235(10)
      abb235(27)=abb235(18)*spbe6k2
      abb235(8)=abb235(27)*abb235(8)
      abb235(15)=-8.0_ki*spbk6k2*abb235(15)
      abb235(28)=2.0_ki*abb235(27)
      abb235(17)=spbk6e6*spak1k6*abb235(17)
      abb235(18)=2.0_ki*abb235(18)
      abb235(18)=spbk6e6*abb235(18)
      abb235(29)=-spak1k2*abb235(4)*spbk4k2
      abb235(16)=-abb235(16)+abb235(29)
      abb235(16)=4.0_ki*abb235(16)
      abb235(4)=4.0_ki*abb235(4)
      abb235(14)=abb235(14)*abb235(4)
      abb235(5)=-4.0_ki*spbk4k1*abb235(5)
      R2d235=0.0_ki
      rat2 = rat2 + R2d235
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='235' value='", &
          & R2d235, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd235h0
