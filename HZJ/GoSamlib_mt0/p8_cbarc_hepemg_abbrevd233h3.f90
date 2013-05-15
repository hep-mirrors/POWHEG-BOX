module     p8_cbarc_hepemg_abbrevd233h3
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(32), public :: abb233
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
      abb233(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb233(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb233(3)=NC**(-1)
      abb233(4)=spae6k6*spbk6k5
      abb233(5)=spbk5k1*spak1e6
      abb233(4)=abb233(4)-abb233(5)
      abb233(4)=abb233(4)*spak2k4
      abb233(6)=gCr*ger*abb233(3)*abb233(1)*abb233(2)*gHZZ*c1*TR*i_
      abb233(7)=2.0_ki*abb233(6)
      abb233(8)=abb233(7)*spbe6k1
      abb233(9)=abb233(8)*es12*abb233(4)
      abb233(10)=spbk5k1*es12
      abb233(11)=-spbk2k1*spbk6k5*spak2k6
      abb233(10)=abb233(11)+abb233(10)
      abb233(11)=4.0_ki*abb233(6)
      abb233(12)=abb233(11)*spak2k4
      abb233(10)=abb233(10)*abb233(12)
      abb233(13)=spak2e6*spbk2k1
      abb233(14)=spak2k4*spbe6k5
      abb233(15)=abb233(13)*abb233(14)
      abb233(16)=abb233(11)*abb233(15)
      abb233(4)=spbe6k1*abb233(4)
      abb233(4)=-abb233(15)+abb233(4)
      abb233(4)=abb233(4)*abb233(11)
      abb233(6)=8.0_ki*spbk5k1*abb233(6)*spak2k4
      abb233(17)=-spak4e6*spak1k2
      abb233(18)=spak2e6*spak1k4
      abb233(17)=abb233(17)+abb233(18)
      abb233(17)=abb233(17)*spbe6k1
      abb233(18)=-spak2e6*spbk6e6*spak4k6
      abb233(17)=abb233(17)+abb233(18)
      abb233(17)=spbk5k1*abb233(17)
      abb233(15)=abb233(15)+abb233(17)
      abb233(15)=abb233(15)*abb233(7)
      abb233(17)=abb233(11)*spbk5k1
      abb233(18)=-spak2k4*abb233(17)
      abb233(19)=abb233(17)*spak4e6
      abb233(20)=spak4e6*abb233(7)*spbk5k1
      abb233(21)=abb233(14)*abb233(11)
      abb233(14)=-abb233(7)*abb233(14)
      abb233(12)=spbk6k5*abb233(12)
      abb233(22)=abb233(13)*spbk6e6
      abb233(23)=spak2k6*abb233(7)*abb233(22)
      abb233(24)=abb233(11)*spbe6k1
      abb233(25)=spak2e6*abb233(24)
      abb233(26)=spbk6k1*abb233(7)*spae6k6
      abb233(27)=-spbk6e6*spak2k6
      abb233(28)=spbe6k1*spak1k2
      abb233(27)=abb233(27)+abb233(28)
      abb233(27)=abb233(27)*abb233(7)
      abb233(28)=spae6k6*spak1k4
      abb233(29)=abb233(28)*abb233(8)*spbk5k1
      abb233(30)=-spak4k6*abb233(17)
      abb233(31)=abb233(8)*spae6k6
      abb233(28)=-spbk6k1*abb233(28)
      abb233(32)=-spak4e6*es12
      abb233(13)=spak1k4*abb233(13)
      abb233(13)=abb233(13)+abb233(28)+abb233(32)
      abb233(13)=spbe6k1*abb233(13)
      abb233(22)=-spak4k6*abb233(22)
      abb233(13)=abb233(22)+abb233(13)
      abb233(7)=abb233(13)*abb233(7)
      abb233(13)=spak4k6*spbk6k1
      abb233(22)=-spak2k4*spbk2k1
      abb233(13)=abb233(13)+abb233(22)
      abb233(13)=abb233(13)*abb233(11)
      abb233(22)=abb233(24)*spak4e6
      abb233(28)=-spak4e6*abb233(8)
      abb233(5)=abb233(5)*abb233(8)*spak1k4
      abb233(8)=-spak1k4*abb233(17)
      abb233(17)=-spak1e6*abb233(24)
      R2d233=0.0_ki
      rat2 = rat2 + R2d233
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='233' value='", &
          & R2d233, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd233h3
