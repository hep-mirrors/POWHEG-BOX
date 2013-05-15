module     p12_sbars_hepemg_abbrevd233h1
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(32), public :: abb233
   complex(ki), public :: R2d233
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
      abb233(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb233(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb233(3)=NC**(-1)
      abb233(4)=spae6k6*spbk6k4
      abb233(5)=spbk4k1*spak1e6
      abb233(4)=abb233(4)-abb233(5)
      abb233(4)=abb233(4)*spak2k5
      abb233(6)=gSr*gel*abb233(3)*abb233(1)*abb233(2)*gHZZ*c1*TR*i_
      abb233(7)=2.0_ki*abb233(6)
      abb233(8)=abb233(7)*spbe6k1
      abb233(9)=abb233(8)*es12*abb233(4)
      abb233(10)=spbk4k1*es12
      abb233(11)=-spbk2k1*spbk6k4*spak2k6
      abb233(10)=abb233(11)+abb233(10)
      abb233(11)=4.0_ki*abb233(6)
      abb233(12)=abb233(11)*spak2k5
      abb233(10)=abb233(10)*abb233(12)
      abb233(13)=spak2e6*spbk2k1
      abb233(14)=spak2k5*spbe6k4
      abb233(15)=abb233(13)*abb233(14)
      abb233(16)=abb233(11)*abb233(15)
      abb233(4)=spbe6k1*abb233(4)
      abb233(4)=-abb233(15)+abb233(4)
      abb233(4)=abb233(4)*abb233(11)
      abb233(6)=8.0_ki*spbk4k1*abb233(6)*spak2k5
      abb233(17)=-spak5e6*spak1k2
      abb233(18)=spak2e6*spak1k5
      abb233(17)=abb233(17)+abb233(18)
      abb233(17)=abb233(17)*spbe6k1
      abb233(18)=-spak2e6*spbk6e6*spak5k6
      abb233(17)=abb233(17)+abb233(18)
      abb233(17)=spbk4k1*abb233(17)
      abb233(15)=abb233(15)+abb233(17)
      abb233(15)=abb233(15)*abb233(7)
      abb233(17)=abb233(11)*spbk4k1
      abb233(18)=-spak2k5*abb233(17)
      abb233(19)=abb233(17)*spak5e6
      abb233(20)=spak5e6*abb233(7)*spbk4k1
      abb233(21)=abb233(14)*abb233(11)
      abb233(14)=-abb233(7)*abb233(14)
      abb233(12)=spbk6k4*abb233(12)
      abb233(22)=abb233(13)*spbk6e6
      abb233(23)=spak2k6*abb233(7)*abb233(22)
      abb233(24)=abb233(11)*spbe6k1
      abb233(25)=spak2e6*abb233(24)
      abb233(26)=spbk6k1*abb233(7)*spae6k6
      abb233(27)=-spbk6e6*spak2k6
      abb233(28)=spbe6k1*spak1k2
      abb233(27)=abb233(27)+abb233(28)
      abb233(27)=abb233(27)*abb233(7)
      abb233(28)=spae6k6*spak1k5
      abb233(29)=abb233(28)*abb233(8)*spbk4k1
      abb233(30)=-spak5k6*abb233(17)
      abb233(31)=abb233(8)*spae6k6
      abb233(28)=-spbk6k1*abb233(28)
      abb233(32)=-spak5e6*es12
      abb233(13)=spak1k5*abb233(13)
      abb233(13)=abb233(13)+abb233(28)+abb233(32)
      abb233(13)=spbe6k1*abb233(13)
      abb233(22)=-spak5k6*abb233(22)
      abb233(13)=abb233(22)+abb233(13)
      abb233(7)=abb233(13)*abb233(7)
      abb233(13)=spak5k6*spbk6k1
      abb233(22)=-spak2k5*spbk2k1
      abb233(13)=abb233(13)+abb233(22)
      abb233(13)=abb233(13)*abb233(11)
      abb233(22)=abb233(24)*spak5e6
      abb233(28)=-spak5e6*abb233(8)
      abb233(5)=abb233(5)*abb233(8)*spak1k5
      abb233(8)=-spak1k5*abb233(17)
      abb233(17)=-spak1e6*abb233(24)
      R2d233=0.0_ki
      rat2 = rat2 + R2d233
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='233' value='", &
          & R2d233, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd233h1
