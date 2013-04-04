module     p16_bbarb_hepemg_abbrevd244h0
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(37), public :: abb244
   complex(ki), public :: R2d244
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
      abb244(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb244(2)=1.0_ki/(mH**2+es45+es61-es23-es345)
      abb244(3)=1.0_ki/(mH**2+es45-es123+es12-es345)
      abb244(4)=sqrt(mT**2)
      abb244(5)=1.0_ki/(es45-es123-es61+es23)
      abb244(6)=spak2l3**(-1)
      abb244(7)=spbl3k2**(-1)
      abb244(8)=spak1k5*abb244(5)
      abb244(9)=i_*TR*c1*gHT*gBl*gel*abb244(3)*abb244(1)
      abb244(10)=abb244(4)*abb244(9)
      abb244(11)=abb244(10)*abb244(8)
      abb244(12)=abb244(11)*spbk4k1
      abb244(10)=abb244(10)*abb244(2)
      abb244(13)=spbk4k2*abb244(10)
      abb244(14)=abb244(13)*spak2k5
      abb244(12)=abb244(14)-abb244(12)
      abb244(14)=-spbe6k2*abb244(12)
      abb244(13)=abb244(13)*spak4k5
      abb244(15)=abb244(13)*spbe6k4
      abb244(16)=abb244(14)+abb244(15)
      abb244(17)=-spak1e6*abb244(16)
      abb244(11)=abb244(11)*spbk5k4
      abb244(18)=abb244(11)*spak5e6
      abb244(19)=abb244(18)*spbe6k2
      abb244(17)=abb244(17)-abb244(19)
      abb244(19)=4.0_ki*abb244(17)
      abb244(20)=spak1e6*abb244(9)*spbk4k2*abb244(2)
      abb244(21)=spak2k5*spbe6k2
      abb244(22)=abb244(21)*abb244(20)
      abb244(8)=abb244(9)*abb244(8)
      abb244(9)=-spbk4k1*abb244(8)*spak1e6*spbe6k2
      abb244(9)=abb244(22)+abb244(9)
      abb244(22)=2.0_ki*spbe6k2
      abb244(8)=-spak5e6*spbk5k4*abb244(8)*abb244(22)
      abb244(20)=abb244(20)*spbe6k4*spak4k5
      abb244(8)=2.0_ki*abb244(9)+abb244(8)-2.0_ki*abb244(20)
      abb244(8)=abb244(8)*abb244(4)**3
      abb244(9)=abb244(6)*abb244(7)*mH**2
      abb244(20)=abb244(21)*abb244(9)
      abb244(21)=spal3k5*spbe6l3
      abb244(20)=abb244(21)+abb244(20)
      abb244(21)=spbk6k2*spae6k6
      abb244(23)=abb244(21)*abb244(11)
      abb244(20)=abb244(23)*abb244(20)
      abb244(10)=spbk4k2**2*abb244(9)*spak4k5*abb244(10)
      abb244(24)=abb244(10)*spak2e6
      abb244(25)=spal3e6*spbl3k2
      abb244(26)=abb244(25)*abb244(12)
      abb244(27)=abb244(13)*spbk4l3
      abb244(28)=abb244(27)*spal3e6
      abb244(24)=abb244(28)+abb244(24)+abb244(26)
      abb244(26)=spak1k6*spbk6e6
      abb244(28)=abb244(26)*abb244(24)
      abb244(29)=spbe6l3*spak1l3
      abb244(30)=abb244(29)*abb244(12)
      abb244(31)=abb244(21)*abb244(30)
      abb244(32)=abb244(13)*spbk6k4
      abb244(33)=abb244(32)*spae6k6
      abb244(34)=-abb244(29)*abb244(33)
      abb244(14)=-abb244(14)*abb244(21)
      abb244(35)=-spbe6k2*abb244(33)
      abb244(14)=abb244(14)+abb244(35)
      abb244(9)=abb244(9)*spak1k2
      abb244(14)=abb244(14)*abb244(9)
      abb244(35)=abb244(11)*spak5k6
      abb244(36)=abb244(35)*spbk6e6
      abb244(37)=-abb244(25)*abb244(36)
      abb244(8)=abb244(37)+abb244(14)+abb244(34)+abb244(31)+abb244(28)+abb244(2&
      &0)+abb244(8)
      abb244(8)=4.0_ki*abb244(8)
      abb244(14)=-spbk6k2*abb244(12)
      abb244(14)=abb244(14)+abb244(32)
      abb244(14)=spak1k6*abb244(14)
      abb244(20)=spbk6k2*abb244(35)
      abb244(14)=abb244(20)+abb244(14)
      abb244(14)=8.0_ki*abb244(14)
      abb244(20)=16.0_ki*abb244(17)
      abb244(17)=-8.0_ki*abb244(17)
      abb244(28)=spak1k6*abb244(16)
      abb244(31)=spbe6k2*abb244(35)
      abb244(28)=abb244(31)+abb244(28)
      abb244(28)=2.0_ki*abb244(28)
      abb244(31)=-spak1e6*abb244(12)
      abb244(18)=abb244(31)+abb244(18)
      abb244(31)=spbk6k2*abb244(18)
      abb244(32)=spak1e6*abb244(32)
      abb244(31)=abb244(32)+abb244(31)
      abb244(31)=2.0_ki*abb244(31)
      abb244(25)=abb244(11)*abb244(25)
      abb244(23)=abb244(23)+abb244(25)
      abb244(23)=4.0_ki*abb244(23)
      abb244(25)=spbe6k2*abb244(9)
      abb244(25)=abb244(25)+abb244(26)+abb244(29)
      abb244(25)=4.0_ki*abb244(13)*abb244(25)
      abb244(29)=-spak1l3*abb244(16)
      abb244(32)=abb244(11)*spal3k5
      abb244(34)=spbe6k2*abb244(32)
      abb244(29)=abb244(34)+abb244(29)
      abb244(29)=4.0_ki*abb244(29)
      abb244(34)=-spbl3k2*abb244(18)
      abb244(27)=spak1e6*abb244(27)
      abb244(27)=abb244(27)+abb244(34)
      abb244(27)=4.0_ki*abb244(27)
      abb244(26)=-abb244(12)*abb244(26)
      abb244(9)=-abb244(15)*abb244(9)
      abb244(15)=-spbe6l3*abb244(32)
      abb244(9)=abb244(15)+abb244(36)+abb244(9)+abb244(26)-abb244(30)
      abb244(9)=4.0_ki*abb244(9)
      abb244(10)=4.0_ki*spak1e6*abb244(10)
      abb244(15)=-abb244(12)*abb244(21)
      abb244(15)=abb244(33)+abb244(15)-abb244(24)
      abb244(15)=4.0_ki*abb244(15)
      abb244(21)=2.0_ki*spak1e6
      abb244(21)=-spbk6e6*abb244(13)*abb244(21)
      abb244(18)=-2.0_ki*spbk6e6*abb244(18)
      abb244(22)=-abb244(22)*abb244(11)*spae6k6
      abb244(11)=-32.0_ki*abb244(11)
      abb244(16)=-2.0_ki*spae6k6*abb244(16)
      abb244(13)=-32.0_ki*abb244(13)
      abb244(12)=32.0_ki*abb244(12)
      R2d244=abb244(19)
      rat2 = rat2 + R2d244
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='244' value='", &
          & R2d244, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd244h0
