module     p8_cbarc_hepemg_abbrevd145h0
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(61), public :: abb145
   complex(ki), public :: R2d145
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
      abb145(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb145(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb145(3)=es12**(-1)
      abb145(4)=dotproduct(k3,e6)
      abb145(5)=dotproduct(k3,spvak1e6)
      abb145(6)=dotproduct(k3,spvae6k2)
      abb145(7)=dotproduct(k3,spvae6k4)
      abb145(8)=dotproduct(k3,spvak5e6)
      abb145(9)=dotproduct(k3,spvak1k2)
      abb145(10)=dotproduct(k3,spvak1k4)
      abb145(11)=dotproduct(k3,spvak5k2)
      abb145(12)=1.0_ki/2.0_ki*spak5e6
      abb145(13)=-spbk5k4*abb145(12)
      abb145(13)=abb145(13)+1.0_ki/4.0_ki*abb145(7)
      abb145(14)=spbe6k2*spak1k5
      abb145(13)=abb145(14)*abb145(13)
      abb145(15)=1.0_ki/2.0_ki*spbk4k2
      abb145(16)=spbe6k4*spak1k5
      abb145(17)=-spak4e6*abb145(16)*abb145(15)
      abb145(18)=abb145(10)*spbe6k2*spak5e6
      abb145(19)=spak5e6*spbk4k2
      abb145(20)=1.0_ki/2.0_ki*abb145(19)
      abb145(21)=spak1k4*spbe6k4*abb145(20)
      abb145(13)=abb145(21)+abb145(17)-1.0_ki/4.0_ki*abb145(18)+abb145(13)
      abb145(17)=gHZZ*Nfrat*c1*i_*TR*gel*abb145(3)*abb145(2)*abb145(1)
      abb145(18)=abb145(17)*gBr
      abb145(17)=abb145(17)*gBl
      abb145(21)=abb145(18)+1.0_ki/3.0_ki*abb145(17)
      abb145(13)=abb145(21)*abb145(13)
      abb145(22)=spak4k5*abb145(15)
      abb145(22)=abb145(22)-1.0_ki/4.0_ki*abb145(11)
      abb145(23)=spak1e6*spbe6k4
      abb145(22)=abb145(23)*abb145(22)
      abb145(24)=spbk5k2*abb145(16)*abb145(12)
      abb145(25)=spak1e6*spbk4k2
      abb145(26)=abb145(8)*abb145(25)
      abb145(20)=-spbe6k5*spak1k5*abb145(20)
      abb145(20)=abb145(24)+abb145(20)+1.0_ki/4.0_ki*abb145(26)+abb145(22)
      abb145(22)=abb145(17)+1.0_ki/3.0_ki*abb145(18)
      abb145(20)=abb145(22)*abb145(20)
      abb145(24)=spak5k6*spbk6e6
      abb145(26)=abb145(22)*abb145(25)*abb145(24)
      abb145(27)=spbk6k4*spae6k6
      abb145(28)=abb145(14)*abb145(27)
      abb145(29)=abb145(21)*abb145(28)
      abb145(26)=abb145(26)+abb145(29)
      abb145(29)=abb145(18)-abb145(17)
      abb145(30)=abb145(29)*abb145(19)
      abb145(31)=spbk6e6*abb145(30)
      abb145(21)=-spbk6k4*spbe6k2*abb145(21)*abb145(12)
      abb145(32)=abb145(18)+abb145(17)
      abb145(33)=spak5e6*abb145(32)*spbe6k4
      abb145(34)=spbk6k2*abb145(33)
      abb145(21)=2.0_ki/3.0_ki*abb145(34)-1.0_ki/3.0_ki*abb145(31)+abb145(21)
      abb145(21)=spak1k6*abb145(21)
      abb145(31)=abb145(5)*abb145(30)
      abb145(34)=abb145(29)*abb145(16)
      abb145(35)=abb145(6)*abb145(34)
      abb145(31)=abb145(31)-abb145(35)
      abb145(35)=spae6k6*abb145(34)
      abb145(36)=1.0_ki/2.0_ki*spbe6k4
      abb145(22)=-abb145(22)*abb145(36)*spak5k6*spak1e6
      abb145(22)=1.0_ki/3.0_ki*abb145(35)+abb145(22)
      abb145(22)=spbk6k2*abb145(22)
      abb145(35)=abb145(32)*spbk4k2
      abb145(36)=abb145(35)*spak1k5
      abb145(37)=abb145(4)*abb145(36)
      abb145(38)=abb145(9)*abb145(33)
      abb145(13)=1.0_ki/3.0_ki*abb145(38)-2.0_ki/3.0_ki*abb145(37)+abb145(21)+1&
      &.0_ki/2.0_ki*abb145(26)+abb145(22)+abb145(20)+abb145(13)-1.0_ki/6.0_ki*a&
      &bb145(31)
      abb145(20)=spak1k2*spbk4k2
      abb145(21)=abb145(17)*abb145(20)*spbk6k2*spak5k6
      abb145(22)=spbk2k1*spak1k5
      abb145(26)=-abb145(18)*abb145(22)*spak1k6*spbk6k4
      abb145(21)=abb145(21)+abb145(26)
      abb145(21)=4.0_ki*abb145(21)
      abb145(26)=spbk2k1*spak1e6
      abb145(31)=abb145(16)*abb145(18)*abb145(26)
      abb145(37)=spak1k2*spbe6k2
      abb145(38)=-abb145(17)*abb145(19)*abb145(37)
      abb145(31)=abb145(31)+abb145(38)
      abb145(31)=4.0_ki*abb145(31)
      abb145(28)=abb145(32)*abb145(28)
      abb145(38)=abb145(17)+3.0_ki*abb145(18)
      abb145(39)=abb145(38)*abb145(24)
      abb145(40)=-abb145(25)*abb145(39)
      abb145(28)=abb145(40)-2.0_ki*abb145(28)
      abb145(40)=8.0_ki*abb145(36)
      abb145(41)=abb145(24)*spak1e6
      abb145(42)=abb145(35)*abb145(41)
      abb145(17)=abb145(18)+3.0_ki*abb145(17)
      abb145(18)=abb145(17)*abb145(27)
      abb145(43)=-abb145(14)*abb145(18)
      abb145(42)=-2.0_ki*abb145(42)+abb145(43)
      abb145(36)=-4.0_ki*abb145(36)
      abb145(25)=abb145(25)*abb145(29)
      abb145(43)=abb145(17)*abb145(15)
      abb145(44)=spak1e6*abb145(43)
      abb145(45)=abb145(14)*abb145(29)
      abb145(46)=1.0_ki/2.0_ki*abb145(38)
      abb145(14)=abb145(14)*abb145(46)
      abb145(24)=-spak1k2*abb145(29)*abb145(15)*abb145(24)
      abb145(47)=1.0_ki/2.0_ki*abb145(29)
      abb145(48)=-abb145(27)*abb145(47)*abb145(22)
      abb145(49)=spbk6e6*spak1e6
      abb145(50)=abb145(49)*abb145(22)*abb145(46)
      abb145(51)=abb145(29)*spak1k5
      abb145(52)=-spbk6k4*abb145(51)
      abb145(43)=-abb145(43)*abb145(37)*spae6k6
      abb145(53)=abb145(37)*abb145(29)
      abb145(54)=spbk6k2*spae6k6*abb145(53)
      abb145(55)=abb145(29)*spak1k6
      abb145(56)=spbk2k1*abb145(49)*abb145(55)
      abb145(54)=abb145(54)+abb145(56)
      abb145(54)=1.0_ki/2.0_ki*abb145(54)
      abb145(56)=spak1k6*spbk6e6*abb145(47)
      abb145(53)=-abb145(53)+abb145(56)
      abb145(26)=-abb145(29)*abb145(26)
      abb145(56)=abb145(47)*spae6k6
      abb145(57)=-spbk6k2*abb145(56)
      abb145(26)=abb145(26)+abb145(57)
      abb145(49)=abb145(49)*abb145(47)
      abb145(57)=1.0_ki/2.0_ki*abb145(18)
      abb145(37)=abb145(37)*abb145(57)
      abb145(20)=abb145(17)*abb145(20)
      abb145(55)=-spbk6k4*abb145(55)
      abb145(20)=2.0_ki*abb145(20)+abb145(55)
      abb145(55)=abb145(29)*abb145(23)
      abb145(17)=1.0_ki/2.0_ki*abb145(17)
      abb145(23)=-abb145(23)*abb145(17)
      abb145(58)=abb145(29)*spak5k6
      abb145(59)=spbk4k2*abb145(58)
      abb145(56)=-spbe6k2*abb145(56)
      abb145(46)=-spbk2k1*abb145(41)*abb145(46)
      abb145(22)=abb145(22)*abb145(38)
      abb145(58)=spbk6k2*abb145(58)
      abb145(22)=-2.0_ki*abb145(22)+abb145(58)
      abb145(58)=abb145(29)*spbe6k2
      abb145(60)=-spak5e6*abb145(58)
      abb145(38)=abb145(38)*abb145(12)
      abb145(61)=-spbe6k2*abb145(38)
      abb145(27)=spak2k5*spbe6k2*abb145(27)
      abb145(41)=spbk4k1*abb145(41)
      abb145(27)=abb145(27)+abb145(41)
      abb145(27)=abb145(32)*abb145(27)
      abb145(15)=spak2e6*abb145(15)*abb145(39)
      abb145(41)=abb145(57)*spbe6k1*spak1k5
      abb145(15)=abb145(41)+abb145(15)+abb145(27)
      abb145(27)=-spbk6k4*spak5k6
      abb145(41)=spbk4k1*spak1k5
      abb145(27)=abb145(41)+abb145(27)
      abb145(27)=abb145(32)*abb145(27)
      abb145(32)=spak2k5*abb145(35)
      abb145(27)=abb145(32)+abb145(27)
      abb145(27)=4.0_ki*abb145(27)
      abb145(32)=4.0_ki*abb145(33)
      abb145(33)=2.0_ki*abb145(33)
      abb145(35)=spbk4k1*spak1e6
      abb145(41)=spak2e6*spbk4k2
      abb145(35)=abb145(41)+abb145(35)
      abb145(35)=abb145(29)*abb145(35)
      abb145(18)=abb145(18)+abb145(35)
      abb145(18)=1.0_ki/2.0_ki*abb145(18)
      abb145(35)=-spak2k5*abb145(58)
      abb145(41)=-spbe6k1*abb145(51)
      abb145(35)=abb145(41)+abb145(39)+abb145(35)
      abb145(35)=1.0_ki/2.0_ki*abb145(35)
      abb145(39)=-spak2k5*spbe6k4*abb145(47)
      abb145(19)=abb145(19)*abb145(47)
      abb145(16)=-abb145(16)*abb145(47)
      abb145(12)=spbk4k1*abb145(29)*abb145(12)
      abb145(29)=spbk6e6*abb145(38)
      abb145(17)=spae6k6*spbe6k4*abb145(17)
      R2d145=abb145(13)
      rat2 = rat2 + R2d145
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='145' value='", &
          & R2d145, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd145h0