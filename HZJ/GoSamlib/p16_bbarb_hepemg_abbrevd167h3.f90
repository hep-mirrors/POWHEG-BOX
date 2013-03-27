module     p16_bbarb_hepemg_abbrevd167h3
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(16), public :: abb167
   complex(ki), public :: R2d167
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
      abb167(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb167(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb167(3)=NC**(-1)
      abb167(4)=es61**(-1)
      abb167(5)=gHZZ*spbe6k1*c1*i_*TR*gBr*ger*abb167(2)*abb167(1)
      abb167(6)=abb167(5)*spae6k6
      abb167(7)=abb167(6)*abb167(3)
      abb167(8)=abb167(5)*NC
      abb167(9)=-spae6k6*abb167(8)
      abb167(7)=abb167(7)+abb167(9)
      abb167(7)=spbk6k5*abb167(7)
      abb167(9)=abb167(8)*spak1e6
      abb167(10)=abb167(5)*spak1e6
      abb167(11)=abb167(10)*abb167(3)
      abb167(9)=-abb167(11)+abb167(9)
      abb167(9)=spbk5k1*abb167(9)
      abb167(7)=abb167(7)+abb167(9)
      abb167(7)=2.0_ki*abb167(7)*spak2k4*abb167(4)
      abb167(9)=abb167(3)-NC
      abb167(11)=abb167(4)**2
      abb167(5)=abb167(9)*abb167(5)*abb167(11)*spak2k4
      abb167(9)=-spae6k6*abb167(5)
      abb167(12)=-spbk6k5*abb167(9)
      abb167(5)=-spak1e6*abb167(5)
      abb167(13)=2.0_ki*spbk5k1
      abb167(14)=abb167(5)*abb167(13)
      abb167(12)=abb167(12)+abb167(14)
      abb167(12)=4.0_ki*abb167(12)
      abb167(14)=2.0_ki*spbk6k5
      abb167(15)=abb167(9)*abb167(14)
      abb167(16)=-spbk5k1*abb167(5)
      abb167(15)=abb167(15)+abb167(16)
      abb167(15)=4.0_ki*abb167(15)
      abb167(5)=abb167(5)*abb167(14)
      abb167(8)=abb167(8)*abb167(11)
      abb167(14)=abb167(8)*spak1e6
      abb167(11)=abb167(11)*abb167(3)
      abb167(10)=abb167(11)*abb167(10)
      abb167(10)=abb167(14)-abb167(10)
      abb167(14)=-spak2k6*abb167(10)
      abb167(8)=abb167(8)*spae6k6
      abb167(6)=abb167(11)*abb167(6)
      abb167(6)=abb167(8)-abb167(6)
      abb167(8)=spak1k2*abb167(6)
      abb167(8)=abb167(14)+abb167(8)
      abb167(11)=4.0_ki*spbk6k1
      abb167(8)=abb167(8)*abb167(11)
      abb167(10)=spak4k6*abb167(10)
      abb167(6)=-spak1k4*abb167(6)
      abb167(6)=abb167(10)+abb167(6)
      abb167(6)=abb167(6)*abb167(11)
      abb167(9)=abb167(9)*abb167(13)
      R2d167=0.0_ki
      rat2 = rat2 + R2d167
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='167' value='", &
          & R2d167, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd167h3
