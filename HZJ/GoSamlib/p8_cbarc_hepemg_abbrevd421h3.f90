module     p8_cbarc_hepemg_abbrevd421h3
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(11), public :: abb421
   complex(ki), public :: R2d421
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
      abb421(1)=1.0_ki/(-es61-es12+es345)
      abb421(2)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb421(3)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb421(4)=NC**(-1)
      abb421(5)=NC-abb421(4)
      abb421(6)=gHZZ*spbk5k1*c1*abb421(1)**2*i_*TR*gCr*ger*abb421(3)*abb421(2)
      abb421(7)=abb421(5)*abb421(6)*spak4k6*spak2e6
      abb421(8)=-spbk6e6*abb421(7)
      abb421(9)=-es61+es345-es12
      abb421(10)=-2.0_ki*abb421(8)*abb421(9)
      abb421(5)=-spak2k4*abb421(6)*abb421(5)
      abb421(6)=8.0_ki*abb421(9)*abb421(5)
      abb421(9)=-4.0_ki*abb421(8)
      abb421(8)=8.0_ki*abb421(8)
      abb421(11)=spae6k6*abb421(5)
      abb421(7)=abb421(7)+abb421(11)
      abb421(7)=4.0_ki*spbk6k2*abb421(7)
      abb421(5)=2.0_ki*spbk6e6*spak2e6*abb421(5)
      R2d421=0.0_ki
      rat2 = rat2 + R2d421
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='421' value='", &
          & R2d421, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd421h3
