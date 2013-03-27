module     p12_sbars_hepemg_abbrevd205h3
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(29), public :: abb205
   complex(ki), public :: R2d205
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p12_sbars_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_color, only: TR
      use p12_sbars_hepemg_globalsl1, only: epspow
      implicit none
      abb205(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb205(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb205(3)=NC**(-1)
      abb205(4)=spak2e6*spbk6e6
      abb205(5)=gSr*ger*abb205(3)*abb205(1)*abb205(2)*gHZZ*c1*TR*i_
      abb205(6)=2.0_ki*abb205(5)
      abb205(7)=abb205(4)*abb205(6)
      abb205(8)=spbk5k1*spak4k6
      abb205(9)=-es12*abb205(8)*abb205(7)
      abb205(10)=spak2k4*spbk2k1
      abb205(11)=abb205(10)*spbk6k5
      abb205(12)=spak2k6*abb205(11)
      abb205(13)=spbk5k1*spak2k4
      abb205(14)=-es12*abb205(13)
      abb205(12)=abb205(12)+abb205(14)
      abb205(14)=4.0_ki*abb205(5)
      abb205(12)=abb205(12)*abb205(14)
      abb205(15)=spak2e6*spbe6k5
      abb205(16)=abb205(15)*abb205(14)
      abb205(17)=-abb205(10)*abb205(16)
      abb205(18)=abb205(10)*abb205(15)
      abb205(19)=spbe6k1*spak1e6*abb205(13)
      abb205(18)=abb205(18)+abb205(19)
      abb205(18)=abb205(18)*abb205(14)
      abb205(5)=abb205(5)*abb205(13)
      abb205(5)=8.0_ki*abb205(5)
      abb205(13)=abb205(14)*spbk5k1
      abb205(19)=abb205(13)*spak4k6
      abb205(4)=abb205(19)*abb205(4)
      abb205(20)=-spbk2k1*spbe6k5
      abb205(21)=-spbe6k1*spbk5k2
      abb205(20)=abb205(20)+abb205(21)
      abb205(20)=spak2e6*abb205(20)
      abb205(21)=spae6k6*spbk6k5*spbe6k1
      abb205(20)=abb205(21)+abb205(20)
      abb205(20)=spak2k4*abb205(20)
      abb205(21)=spbk5k1*spak4e6
      abb205(22)=spbe6k1*spak1k2*abb205(21)
      abb205(20)=abb205(22)+abb205(20)
      abb205(20)=abb205(20)*abb205(6)
      abb205(22)=spak4k6*spbk6k1
      abb205(22)=abb205(22)+abb205(10)
      abb205(23)=-spak1e6*abb205(22)
      abb205(24)=-spak4e6*es12
      abb205(23)=abb205(24)+abb205(23)
      abb205(23)=spbk5k1*abb205(23)
      abb205(11)=-spae6k6*abb205(11)
      abb205(10)=spak2e6*spbk5k2*abb205(10)
      abb205(10)=abb205(23)+abb205(11)+abb205(10)
      abb205(10)=abb205(10)*abb205(6)
      abb205(11)=abb205(13)*spak4e6
      abb205(21)=-abb205(6)*abb205(21)
      abb205(23)=-spak2k6*spbk2k1*abb205(7)
      abb205(24)=abb205(14)*spbe6k1
      abb205(25)=spak2e6*abb205(24)
      abb205(26)=abb205(6)*spbe6k1
      abb205(27)=spak2e6*abb205(26)
      abb205(28)=-spae6k6*spbk6k1
      abb205(29)=2.0_ki*spak2e6
      abb205(29)=-spbk2k1*abb205(29)
      abb205(28)=abb205(28)+abb205(29)
      abb205(28)=abb205(28)*abb205(6)
      abb205(29)=abb205(14)*spbk6k5*spak2k6
      abb205(6)=-abb205(6)*abb205(15)
      abb205(8)=spak1e6*abb205(26)*abb205(8)
      abb205(15)=spae6k6*abb205(26)
      abb205(22)=-abb205(22)*abb205(14)
      abb205(24)=spak4e6*abb205(24)
      abb205(26)=spak4e6*abb205(26)
      abb205(13)=-spak1k4*abb205(13)
      R2d205=0.0_ki
      rat2 = rat2 + R2d205
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='205' value='", &
          & R2d205, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd205h3
