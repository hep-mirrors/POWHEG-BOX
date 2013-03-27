module     p8_cbarc_hepemg_abbrevd204h1
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(32), public :: abb204
   complex(ki), public :: R2d204
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
      abb204(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb204(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb204(3)=NC**(-1)
      abb204(4)=spae6k6*spbk6k4
      abb204(5)=spbk4k1*spak1e6
      abb204(4)=abb204(4)-abb204(5)
      abb204(4)=abb204(4)*spak2k5
      abb204(6)=gCr*gel*abb204(3)*abb204(1)*abb204(2)*gHZZ*c1*TR*i_
      abb204(7)=2.0_ki*abb204(6)
      abb204(8)=abb204(7)*spbe6k1
      abb204(9)=abb204(8)*es12*abb204(4)
      abb204(10)=spbk4k1*es12
      abb204(11)=-spbk2k1*spbk6k4*spak2k6
      abb204(10)=abb204(11)+abb204(10)
      abb204(11)=4.0_ki*abb204(6)
      abb204(12)=abb204(11)*spak2k5
      abb204(10)=abb204(10)*abb204(12)
      abb204(13)=spak2e6*spbk2k1
      abb204(14)=spak2k5*spbe6k4
      abb204(15)=abb204(13)*abb204(14)
      abb204(16)=abb204(11)*abb204(15)
      abb204(4)=spbe6k1*abb204(4)
      abb204(4)=-abb204(15)+abb204(4)
      abb204(4)=abb204(4)*abb204(11)
      abb204(6)=8.0_ki*spbk4k1*abb204(6)*spak2k5
      abb204(17)=-spak5e6*spak1k2
      abb204(18)=spak2e6*spak1k5
      abb204(17)=abb204(17)+abb204(18)
      abb204(17)=abb204(17)*spbe6k1
      abb204(18)=-spak2e6*spbk6e6*spak5k6
      abb204(17)=abb204(17)+abb204(18)
      abb204(17)=spbk4k1*abb204(17)
      abb204(15)=abb204(15)+abb204(17)
      abb204(15)=abb204(15)*abb204(7)
      abb204(17)=abb204(11)*spbk4k1
      abb204(18)=-spak2k5*abb204(17)
      abb204(19)=abb204(17)*spak5e6
      abb204(20)=spak5e6*abb204(7)*spbk4k1
      abb204(21)=abb204(14)*abb204(11)
      abb204(14)=-abb204(7)*abb204(14)
      abb204(12)=spbk6k4*abb204(12)
      abb204(22)=abb204(13)*spbk6e6
      abb204(23)=spak2k6*abb204(7)*abb204(22)
      abb204(24)=abb204(11)*spbe6k1
      abb204(25)=spak2e6*abb204(24)
      abb204(26)=spbk6k1*abb204(7)*spae6k6
      abb204(27)=-spbk6e6*spak2k6
      abb204(28)=spbe6k1*spak1k2
      abb204(27)=abb204(27)+abb204(28)
      abb204(27)=abb204(27)*abb204(7)
      abb204(28)=spae6k6*spak1k5
      abb204(29)=abb204(28)*abb204(8)*spbk4k1
      abb204(30)=-spak5k6*abb204(17)
      abb204(31)=abb204(8)*spae6k6
      abb204(28)=-spbk6k1*abb204(28)
      abb204(32)=-spak5e6*es12
      abb204(13)=spak1k5*abb204(13)
      abb204(13)=abb204(13)+abb204(28)+abb204(32)
      abb204(13)=spbe6k1*abb204(13)
      abb204(22)=-spak5k6*abb204(22)
      abb204(13)=abb204(22)+abb204(13)
      abb204(7)=abb204(13)*abb204(7)
      abb204(13)=spak5k6*spbk6k1
      abb204(22)=-spak2k5*spbk2k1
      abb204(13)=abb204(13)+abb204(22)
      abb204(13)=abb204(13)*abb204(11)
      abb204(22)=abb204(24)*spak5e6
      abb204(28)=-spak5e6*abb204(8)
      abb204(5)=abb204(5)*abb204(8)*spak1k5
      abb204(8)=-spak1k5*abb204(17)
      abb204(17)=-spak1e6*abb204(24)
      R2d204=0.0_ki
      rat2 = rat2 + R2d204
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='204' value='", &
          & R2d204, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd204h1
