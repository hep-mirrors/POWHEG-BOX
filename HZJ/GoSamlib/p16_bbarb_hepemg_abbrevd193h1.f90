module     p16_bbarb_hepemg_abbrevd193h1
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(16), public :: abb193
   complex(ki), public :: R2d193
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
      abb193(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb193(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb193(3)=NC**(-1)
      abb193(4)=es61**(-1)
      abb193(5)=gHZZ*spbe6k1*c1*i_*TR*gBr*gel*abb193(2)*abb193(1)
      abb193(6)=abb193(5)*spae6k6
      abb193(7)=abb193(6)*abb193(3)
      abb193(8)=abb193(5)*NC
      abb193(9)=-spae6k6*abb193(8)
      abb193(7)=abb193(7)+abb193(9)
      abb193(7)=spbk6k4*abb193(7)
      abb193(9)=abb193(8)*spak1e6
      abb193(10)=abb193(5)*spak1e6
      abb193(11)=abb193(10)*abb193(3)
      abb193(9)=-abb193(11)+abb193(9)
      abb193(9)=spbk4k1*abb193(9)
      abb193(7)=abb193(7)+abb193(9)
      abb193(7)=2.0_ki*abb193(7)*spak2k5*abb193(4)
      abb193(9)=abb193(3)-NC
      abb193(11)=abb193(4)**2
      abb193(5)=abb193(9)*abb193(5)*abb193(11)*spak2k5
      abb193(9)=-spae6k6*abb193(5)
      abb193(12)=-spbk6k4*abb193(9)
      abb193(5)=-spak1e6*abb193(5)
      abb193(13)=2.0_ki*spbk4k1
      abb193(14)=abb193(5)*abb193(13)
      abb193(12)=abb193(12)+abb193(14)
      abb193(12)=4.0_ki*abb193(12)
      abb193(14)=2.0_ki*spbk6k4
      abb193(15)=abb193(9)*abb193(14)
      abb193(16)=-spbk4k1*abb193(5)
      abb193(15)=abb193(15)+abb193(16)
      abb193(15)=4.0_ki*abb193(15)
      abb193(5)=abb193(5)*abb193(14)
      abb193(8)=abb193(8)*abb193(11)
      abb193(14)=abb193(8)*spak1e6
      abb193(11)=abb193(11)*abb193(3)
      abb193(10)=abb193(11)*abb193(10)
      abb193(10)=abb193(14)-abb193(10)
      abb193(14)=-spak2k6*abb193(10)
      abb193(8)=abb193(8)*spae6k6
      abb193(6)=abb193(11)*abb193(6)
      abb193(6)=abb193(8)-abb193(6)
      abb193(8)=spak1k2*abb193(6)
      abb193(8)=abb193(14)+abb193(8)
      abb193(11)=4.0_ki*spbk6k1
      abb193(8)=abb193(8)*abb193(11)
      abb193(10)=spak5k6*abb193(10)
      abb193(6)=-spak1k5*abb193(6)
      abb193(6)=abb193(10)+abb193(6)
      abb193(6)=abb193(6)*abb193(11)
      abb193(9)=abb193(9)*abb193(13)
      R2d193=0.0_ki
      rat2 = rat2 + R2d193
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='193' value='", &
          & R2d193, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd193h1
