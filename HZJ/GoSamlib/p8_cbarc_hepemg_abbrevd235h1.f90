module     p8_cbarc_hepemg_abbrevd235h1
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(21), public :: abb235
   complex(ki), public :: R2d235
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
      abb235(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb235(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb235(3)=NC**(-1)
      abb235(4)=es61**(-1)
      abb235(5)=spbe6k1*abb235(3)
      abb235(6)=NC*spbe6k1
      abb235(7)=abb235(6)-abb235(5)
      abb235(8)=spae6k6*spbk6k4
      abb235(9)=spbk4k1*spak1e6
      abb235(10)=abb235(8)-abb235(9)
      abb235(7)=abb235(10)*abb235(7)
      abb235(10)=gCr*gel*abb235(1)*abb235(2)*gHZZ*c1*TR*i_
      abb235(11)=abb235(10)*abb235(4)
      abb235(12)=spak2k5*abb235(11)
      abb235(13)=-abb235(7)*abb235(12)
      abb235(14)=-2.0_ki*abb235(8)+abb235(9)
      abb235(14)=abb235(14)*abb235(6)*abb235(10)*spak2k5
      abb235(15)=NC*spbk4k1
      abb235(16)=abb235(3)*spbk4k1
      abb235(15)=abb235(15)+abb235(16)
      abb235(10)=4.0_ki*abb235(10)
      abb235(10)=-spak2k5*abb235(15)*abb235(10)
      abb235(17)=abb235(9)*spbe6k1
      abb235(18)=spae6k6*spbk6k1
      abb235(19)=abb235(18)*spbe6k4
      abb235(17)=abb235(17)+abb235(19)
      abb235(17)=abb235(17)*NC
      abb235(19)=abb235(19)*abb235(3)
      abb235(17)=abb235(17)-abb235(19)
      abb235(9)=-abb235(9)*abb235(5)
      abb235(9)=abb235(9)-abb235(17)
      abb235(19)=4.0_ki*abb235(12)
      abb235(9)=abb235(9)*abb235(19)
      abb235(15)=abb235(15)*abb235(12)
      abb235(20)=16.0_ki*abb235(15)
      abb235(5)=abb235(8)*abb235(5)
      abb235(5)=abb235(5)+abb235(17)
      abb235(5)=abb235(5)*abb235(19)
      abb235(8)=-8.0_ki*abb235(15)
      abb235(12)=2.0_ki*abb235(12)
      abb235(7)=abb235(7)*abb235(12)
      abb235(12)=-spbk6e6*spak1k6*abb235(16)*abb235(12)
      abb235(15)=NC+abb235(3)
      abb235(16)=abb235(19)*spbk6k4*abb235(15)
      abb235(6)=abb235(11)*abb235(6)
      abb235(17)=spbk6k1*spak2k6
      abb235(19)=spak1e6*abb235(17)
      abb235(18)=3.0_ki*abb235(18)
      abb235(21)=spak1k2*abb235(18)
      abb235(19)=abb235(19)+abb235(21)
      abb235(19)=abb235(19)*abb235(6)
      abb235(11)=8.0_ki*abb235(11)
      abb235(11)=abb235(11)*abb235(15)
      abb235(15)=-abb235(17)*abb235(11)
      abb235(17)=spbk6k1*spak5k6
      abb235(21)=-spak1e6*abb235(17)
      abb235(18)=-spak1k5*abb235(18)
      abb235(18)=abb235(21)+abb235(18)
      abb235(6)=abb235(18)*abb235(6)
      abb235(11)=abb235(17)*abb235(11)
      R2d235=abb235(13)
      rat2 = rat2 + R2d235
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='235' value='", &
          & R2d235, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd235h1
