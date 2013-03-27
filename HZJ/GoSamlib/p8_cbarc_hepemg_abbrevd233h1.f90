module     p8_cbarc_hepemg_abbrevd233h1
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(19), public :: abb233
   complex(ki), public :: R2d233
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
      abb233(1)=1.0_ki/(-es61-es12+es345)
      abb233(2)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb233(3)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb233(4)=NC**(-1)
      abb233(5)=gCr*gHZZ*spbk4k1*c1*i_*TR*gel*abb233(3)*abb233(2)*abb233(1)
      abb233(6)=abb233(4)*abb233(5)*spak5k6
      abb233(7)=spbk6e6*spak2e6
      abb233(8)=abb233(7)*abb233(6)
      abb233(9)=abb233(5)*NC
      abb233(10)=abb233(9)*spak5k6
      abb233(11)=abb233(7)*abb233(10)
      abb233(8)=abb233(8)-abb233(11)
      abb233(12)=-es61+es345-es12
      abb233(12)=2.0_ki*abb233(12)
      abb233(13)=abb233(11)*abb233(12)
      abb233(9)=abb233(9)*spak2k5
      abb233(12)=-abb233(9)*abb233(12)
      abb233(14)=-4.0_ki*abb233(8)
      abb233(11)=4.0_ki*abb233(11)
      abb233(5)=abb233(5)*spak2k5
      abb233(15)=abb233(4)*abb233(5)
      abb233(16)=abb233(9)+abb233(15)
      abb233(16)=8.0_ki*abb233(16)
      abb233(17)=2.0_ki*abb233(8)
      abb233(18)=-abb233(9)-2.0_ki*abb233(15)
      abb233(18)=spae6k6*abb233(18)
      abb233(19)=-spak2e6*abb233(10)
      abb233(18)=abb233(19)+abb233(18)
      abb233(18)=spbk6k2*abb233(18)
      abb233(19)=2.0_ki*abb233(4)
      abb233(5)=-abb233(19)*abb233(7)*abb233(5)
      abb233(7)=-abb233(9)+abb233(15)
      abb233(7)=2.0_ki*spbe6k2*spae6k6*abb233(7)
      abb233(6)=abb233(10)+abb233(6)
      abb233(6)=4.0_ki*abb233(6)
      R2d233=-abb233(8)
      rat2 = rat2 + R2d233
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='233' value='", &
          & R2d233, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd233h1
