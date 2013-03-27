module     p8_cbarc_hepemg_abbrevd205h2
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(30), public :: abb205
   complex(ki), public :: R2d205
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
      abb205(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb205(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb205(3)=NC**(-1)
      abb205(4)=spbe6k2*spae6k6
      abb205(5)=gCl*ger*abb205(3)*abb205(1)*abb205(2)*gHZZ*c1*TR*i_
      abb205(6)=2.0_ki*abb205(5)
      abb205(7)=abb205(4)*abb205(6)
      abb205(8)=spak1k4*spbk6k5
      abb205(9)=-es12*abb205(8)*abb205(7)
      abb205(10)=spbk5k2*spak1k2
      abb205(11)=abb205(10)*spak4k6
      abb205(12)=spbk6k2*abb205(11)
      abb205(13)=spak1k4*spbk5k2
      abb205(14)=-es12*abb205(13)
      abb205(12)=abb205(12)+abb205(14)
      abb205(14)=4.0_ki*abb205(5)
      abb205(12)=abb205(12)*abb205(14)
      abb205(15)=spbe6k2*spak4e6
      abb205(16)=abb205(15)*abb205(14)
      abb205(17)=-abb205(10)*abb205(16)
      abb205(18)=abb205(10)*abb205(15)
      abb205(19)=spak1e6*spbe6k1*abb205(13)
      abb205(18)=abb205(18)+abb205(19)
      abb205(18)=abb205(18)*abb205(14)
      abb205(5)=abb205(5)*abb205(13)
      abb205(5)=8.0_ki*abb205(5)
      abb205(13)=abb205(14)*spak1k4
      abb205(19)=abb205(13)*spbk6k5
      abb205(4)=abb205(19)*abb205(4)
      abb205(20)=-spak1k2*spak4e6
      abb205(21)=-spak1e6*spak2k4
      abb205(20)=abb205(20)+abb205(21)
      abb205(20)=spbe6k2*abb205(20)
      abb205(21)=spbk6e6*spak4k6*spak1e6
      abb205(20)=abb205(21)+abb205(20)
      abb205(20)=spbk5k2*abb205(20)
      abb205(21)=spak1k4*spbe6k5
      abb205(22)=spak1e6*spbk2k1*abb205(21)
      abb205(20)=abb205(22)+abb205(20)
      abb205(20)=abb205(20)*abb205(6)
      abb205(22)=spbk6k5*spak1k6
      abb205(22)=abb205(22)+abb205(10)
      abb205(23)=-spbe6k1*abb205(22)
      abb205(24)=-spbe6k5*es12
      abb205(23)=abb205(24)+abb205(23)
      abb205(23)=spak1k4*abb205(23)
      abb205(11)=-spbk6e6*abb205(11)
      abb205(10)=spbe6k2*spak2k4*abb205(10)
      abb205(10)=abb205(23)+abb205(11)+abb205(10)
      abb205(10)=abb205(10)*abb205(6)
      abb205(11)=abb205(13)*spbe6k5
      abb205(21)=-abb205(6)*abb205(21)
      abb205(23)=abb205(6)*spak1e6
      abb205(8)=spbe6k1*abb205(23)*abb205(8)
      abb205(24)=-spbk6k2*spak1k2*abb205(7)
      abb205(25)=abb205(14)*spak1e6
      abb205(26)=spbe6k2*abb205(25)
      abb205(27)=spbe6k2*abb205(23)
      abb205(28)=-spbk6e6*spak1k6
      abb205(29)=2.0_ki*spbe6k2
      abb205(29)=-spak1k2*abb205(29)
      abb205(28)=abb205(28)+abb205(29)
      abb205(28)=abb205(28)*abb205(6)
      abb205(29)=spbk6e6*abb205(23)
      abb205(22)=-abb205(22)*abb205(14)
      abb205(25)=spbe6k5*abb205(25)
      abb205(23)=spbe6k5*abb205(23)
      abb205(30)=abb205(14)*spak4k6*spbk6k2
      abb205(6)=-abb205(6)*abb205(15)
      abb205(13)=-spbk5k1*abb205(13)
      R2d205=0.0_ki
      rat2 = rat2 + R2d205
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='205' value='", &
          & R2d205, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd205h2
