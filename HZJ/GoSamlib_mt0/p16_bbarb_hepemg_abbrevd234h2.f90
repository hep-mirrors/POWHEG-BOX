module     p16_bbarb_hepemg_abbrevd234h2
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(30), public :: abb234
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
      abb234(4)=spbe6k2*spae6k6
      abb234(5)=gBl*ger*abb234(3)*abb234(1)*abb234(2)*gHZZ*c1*TR*i_
      abb234(6)=2.0_ki*abb234(5)
      abb234(7)=abb234(4)*abb234(6)
      abb234(8)=spak1k4*spbk6k5
      abb234(9)=-es12*abb234(8)*abb234(7)
      abb234(10)=spbk5k2*spak1k2
      abb234(11)=abb234(10)*spak4k6
      abb234(12)=spbk6k2*abb234(11)
      abb234(13)=spak1k4*spbk5k2
      abb234(14)=-es12*abb234(13)
      abb234(12)=abb234(12)+abb234(14)
      abb234(14)=4.0_ki*abb234(5)
      abb234(12)=abb234(12)*abb234(14)
      abb234(15)=spbe6k2*spak4e6
      abb234(16)=abb234(15)*abb234(14)
      abb234(17)=-abb234(10)*abb234(16)
      abb234(18)=abb234(10)*abb234(15)
      abb234(19)=spak1e6*spbe6k1*abb234(13)
      abb234(18)=abb234(18)+abb234(19)
      abb234(18)=abb234(18)*abb234(14)
      abb234(5)=abb234(5)*abb234(13)
      abb234(5)=8.0_ki*abb234(5)
      abb234(13)=abb234(14)*spak1k4
      abb234(19)=abb234(13)*spbk6k5
      abb234(4)=abb234(19)*abb234(4)
      abb234(20)=-spak1k2*spak4e6
      abb234(21)=-spak1e6*spak2k4
      abb234(20)=abb234(20)+abb234(21)
      abb234(20)=spbe6k2*abb234(20)
      abb234(21)=spbk6e6*spak4k6*spak1e6
      abb234(20)=abb234(21)+abb234(20)
      abb234(20)=spbk5k2*abb234(20)
      abb234(21)=spak1k4*spbe6k5
      abb234(22)=spak1e6*spbk2k1*abb234(21)
      abb234(20)=abb234(22)+abb234(20)
      abb234(20)=abb234(20)*abb234(6)
      abb234(22)=spbk6k5*spak1k6
      abb234(22)=abb234(22)+abb234(10)
      abb234(23)=-spbe6k1*abb234(22)
      abb234(24)=-spbe6k5*es12
      abb234(23)=abb234(24)+abb234(23)
      abb234(23)=spak1k4*abb234(23)
      abb234(11)=-spbk6e6*abb234(11)
      abb234(10)=spbe6k2*spak2k4*abb234(10)
      abb234(10)=abb234(23)+abb234(11)+abb234(10)
      abb234(10)=abb234(10)*abb234(6)
      abb234(11)=abb234(13)*spbe6k5
      abb234(21)=-abb234(6)*abb234(21)
      abb234(23)=abb234(6)*spak1e6
      abb234(8)=spbe6k1*abb234(23)*abb234(8)
      abb234(24)=-spbk6k2*spak1k2*abb234(7)
      abb234(25)=abb234(14)*spak1e6
      abb234(26)=spbe6k2*abb234(25)
      abb234(27)=spbe6k2*abb234(23)
      abb234(28)=-spbk6e6*spak1k6
      abb234(29)=2.0_ki*spbe6k2
      abb234(29)=-spak1k2*abb234(29)
      abb234(28)=abb234(28)+abb234(29)
      abb234(28)=abb234(28)*abb234(6)
      abb234(29)=spbk6e6*abb234(23)
      abb234(22)=-abb234(22)*abb234(14)
      abb234(25)=spbe6k5*abb234(25)
      abb234(23)=spbe6k5*abb234(23)
      abb234(30)=abb234(14)*spak4k6*spbk6k2
      abb234(6)=-abb234(6)*abb234(15)
      abb234(13)=-spbk5k1*abb234(13)
      R2d234=0.0_ki
      rat2 = rat2 + R2d234
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='234' value='", &
          & R2d234, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd234h2