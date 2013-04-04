module     p8_cbarc_hepemg_abbrevd150h1
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(20), public :: abb150
   complex(ki), public :: R2d150
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
      abb150(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb150(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb150(3)=NC**(-1)
      abb150(4)=es61**(-1)
      abb150(5)=abb150(3)-NC
      abb150(6)=-spae6k6*spbk6k4*abb150(5)
      abb150(7)=-spak1e6*abb150(5)
      abb150(8)=-spbk4k1*abb150(7)
      abb150(9)=abb150(6)+abb150(8)
      abb150(10)=abb150(4)*spak2k5
      abb150(11)=gHZZ*gCr*gel*abb150(1)*abb150(2)*spbe6k1*c1*TR*i_
      abb150(12)=abb150(10)*abb150(11)
      abb150(13)=-abb150(9)*abb150(12)
      abb150(14)=spbk4k1*es12
      abb150(15)=spak2k6*spbk6k4*spbk2k1
      abb150(14)=abb150(14)-abb150(15)
      abb150(14)=abb150(14)*abb150(7)
      abb150(15)=es12-es345
      abb150(15)=abb150(15)*spbk6k4
      abb150(16)=spak1k2*spbk4k1*spbk6k2
      abb150(15)=abb150(15)-abb150(16)
      abb150(5)=abb150(5)*spae6k6
      abb150(15)=-abb150(15)*abb150(5)
      abb150(14)=abb150(15)+abb150(14)
      abb150(10)=abb150(14)*abb150(10)
      abb150(14)=spak2k5*abb150(6)
      abb150(10)=abb150(14)+abb150(10)
      abb150(10)=2.0_ki*abb150(10)*abb150(11)
      abb150(12)=4.0_ki*abb150(12)
      abb150(6)=-abb150(6)*abb150(12)
      abb150(8)=abb150(8)*abb150(12)
      abb150(12)=2.0_ki*abb150(4)
      abb150(11)=abb150(12)*abb150(11)
      abb150(12)=abb150(11)*spak2k5
      abb150(14)=abb150(9)*abb150(12)
      abb150(15)=-abb150(12)*spbk6k4*abb150(7)
      abb150(16)=-abb150(11)*spak5k6*abb150(9)
      abb150(17)=-abb150(11)*abb150(5)
      abb150(18)=-spak5k6*abb150(7)
      abb150(19)=-spak1k5*abb150(5)
      abb150(18)=abb150(19)+abb150(18)
      abb150(18)=spbk6k1*abb150(18)
      abb150(19)=spbk2k1*abb150(7)
      abb150(20)=spbk6k2*abb150(5)
      abb150(19)=abb150(20)+abb150(19)
      abb150(19)=spak2k5*abb150(19)
      abb150(18)=abb150(19)+abb150(18)
      abb150(18)=abb150(18)*abb150(11)
      abb150(9)=-abb150(11)*spak1k5*abb150(9)
      abb150(7)=abb150(7)*abb150(11)
      abb150(5)=abb150(12)*spbk4k1*abb150(5)
      R2d150=abb150(13)
      rat2 = rat2 + R2d150
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='150' value='", &
          & R2d150, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd150h1
