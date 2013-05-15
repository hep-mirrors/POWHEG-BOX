module     p8_cbarc_hepemg_abbrevd193h0
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(16), public :: abb193
   complex(ki), public :: R2d193
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
      abb193(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb193(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb193(3)=NC**(-1)
      abb193(4)=es61**(-1)
      abb193(5)=gHZZ*spak1e6*c1*i_*TR*gCl*gel*abb193(2)*abb193(1)
      abb193(6)=abb193(5)*spbk6e6
      abb193(7)=abb193(6)*abb193(3)
      abb193(8)=abb193(5)*NC
      abb193(9)=-spbk6e6*abb193(8)
      abb193(7)=abb193(7)+abb193(9)
      abb193(7)=spak5k6*abb193(7)
      abb193(9)=abb193(8)*spbe6k1
      abb193(10)=abb193(5)*spbe6k1
      abb193(11)=abb193(10)*abb193(3)
      abb193(9)=-abb193(11)+abb193(9)
      abb193(9)=spak1k5*abb193(9)
      abb193(7)=abb193(7)+abb193(9)
      abb193(7)=2.0_ki*abb193(7)*spbk4k2*abb193(4)
      abb193(9)=abb193(3)-NC
      abb193(11)=abb193(4)**2
      abb193(5)=abb193(9)*abb193(5)*abb193(11)*spbk4k2
      abb193(9)=-spbk6e6*abb193(5)
      abb193(12)=-spak5k6*abb193(9)
      abb193(5)=-spbe6k1*abb193(5)
      abb193(13)=2.0_ki*spak1k5
      abb193(14)=abb193(5)*abb193(13)
      abb193(12)=abb193(12)+abb193(14)
      abb193(12)=4.0_ki*abb193(12)
      abb193(14)=2.0_ki*spak5k6
      abb193(15)=abb193(9)*abb193(14)
      abb193(16)=-spak1k5*abb193(5)
      abb193(15)=abb193(15)+abb193(16)
      abb193(15)=4.0_ki*abb193(15)
      abb193(9)=abb193(9)*abb193(13)
      abb193(8)=abb193(8)*abb193(11)
      abb193(13)=abb193(8)*spbe6k1
      abb193(11)=abb193(11)*abb193(3)
      abb193(10)=abb193(11)*abb193(10)
      abb193(10)=abb193(13)-abb193(10)
      abb193(13)=-spbk6k2*abb193(10)
      abb193(8)=abb193(8)*spbk6e6
      abb193(6)=abb193(11)*abb193(6)
      abb193(6)=abb193(8)-abb193(6)
      abb193(8)=spbk2k1*abb193(6)
      abb193(8)=abb193(13)+abb193(8)
      abb193(11)=4.0_ki*spak1k6
      abb193(8)=abb193(8)*abb193(11)
      abb193(10)=spbk6k4*abb193(10)
      abb193(6)=-spbk4k1*abb193(6)
      abb193(6)=abb193(10)+abb193(6)
      abb193(6)=abb193(6)*abb193(11)
      abb193(5)=abb193(5)*abb193(14)
      R2d193=0.0_ki
      rat2 = rat2 + R2d193
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='193' value='", &
          & R2d193, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd193h0
