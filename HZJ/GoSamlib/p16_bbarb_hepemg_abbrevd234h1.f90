module     p16_bbarb_hepemg_abbrevd234h1
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(29), public :: abb234
   complex(ki), public :: R2d234
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
      abb234(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb234(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb234(3)=NC**(-1)
      abb234(4)=spak2e6*spbk6e6
      abb234(5)=gBr*gel*abb234(3)*abb234(1)*abb234(2)*gHZZ*c1*TR*i_
      abb234(6)=2.0_ki*abb234(5)
      abb234(7)=abb234(4)*abb234(6)
      abb234(8)=spbk4k1*spak5k6
      abb234(9)=-es12*abb234(8)*abb234(7)
      abb234(10)=spak2k5*spbk2k1
      abb234(11)=abb234(10)*spbk6k4
      abb234(12)=spak2k6*abb234(11)
      abb234(13)=spbk4k1*spak2k5
      abb234(14)=-es12*abb234(13)
      abb234(12)=abb234(12)+abb234(14)
      abb234(14)=4.0_ki*abb234(5)
      abb234(12)=abb234(12)*abb234(14)
      abb234(15)=spak2e6*spbe6k4
      abb234(16)=abb234(15)*abb234(14)
      abb234(17)=-abb234(10)*abb234(16)
      abb234(18)=abb234(10)*abb234(15)
      abb234(19)=spbe6k1*spak1e6*abb234(13)
      abb234(18)=abb234(18)+abb234(19)
      abb234(18)=abb234(18)*abb234(14)
      abb234(5)=abb234(5)*abb234(13)
      abb234(5)=8.0_ki*abb234(5)
      abb234(13)=abb234(14)*spbk4k1
      abb234(19)=abb234(13)*spak5k6
      abb234(4)=abb234(19)*abb234(4)
      abb234(20)=-spbk2k1*spbe6k4
      abb234(21)=-spbe6k1*spbk4k2
      abb234(20)=abb234(20)+abb234(21)
      abb234(20)=spak2e6*abb234(20)
      abb234(21)=spae6k6*spbk6k4*spbe6k1
      abb234(20)=abb234(21)+abb234(20)
      abb234(20)=spak2k5*abb234(20)
      abb234(21)=spbk4k1*spak5e6
      abb234(22)=spbe6k1*spak1k2*abb234(21)
      abb234(20)=abb234(22)+abb234(20)
      abb234(20)=abb234(20)*abb234(6)
      abb234(22)=spak5k6*spbk6k1
      abb234(22)=abb234(22)+abb234(10)
      abb234(23)=-spak1e6*abb234(22)
      abb234(24)=-spak5e6*es12
      abb234(23)=abb234(24)+abb234(23)
      abb234(23)=spbk4k1*abb234(23)
      abb234(11)=-spae6k6*abb234(11)
      abb234(10)=spak2e6*spbk4k2*abb234(10)
      abb234(10)=abb234(23)+abb234(11)+abb234(10)
      abb234(10)=abb234(10)*abb234(6)
      abb234(11)=abb234(13)*spak5e6
      abb234(21)=-abb234(6)*abb234(21)
      abb234(23)=-spak2k6*spbk2k1*abb234(7)
      abb234(24)=abb234(14)*spbe6k1
      abb234(25)=spak2e6*abb234(24)
      abb234(26)=abb234(6)*spbe6k1
      abb234(27)=spak2e6*abb234(26)
      abb234(28)=-spae6k6*spbk6k1
      abb234(29)=2.0_ki*spak2e6
      abb234(29)=-spbk2k1*abb234(29)
      abb234(28)=abb234(28)+abb234(29)
      abb234(28)=abb234(28)*abb234(6)
      abb234(29)=abb234(14)*spbk6k4*spak2k6
      abb234(6)=-abb234(6)*abb234(15)
      abb234(8)=spak1e6*abb234(26)*abb234(8)
      abb234(15)=spae6k6*abb234(26)
      abb234(22)=-abb234(22)*abb234(14)
      abb234(24)=spak5e6*abb234(24)
      abb234(26)=spak5e6*abb234(26)
      abb234(13)=-spak1k5*abb234(13)
      R2d234=0.0_ki
      rat2 = rat2 + R2d234
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='234' value='", &
          & R2d234, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd234h1
