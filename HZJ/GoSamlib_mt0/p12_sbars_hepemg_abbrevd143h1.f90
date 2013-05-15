module     p12_sbars_hepemg_abbrevd143h1
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(65), public :: abb143
   complex(ki), public :: R2d143
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
      abb143(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb143(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb143(3)=es12**(-1)
      abb143(4)=dotproduct(k3,e6)
      abb143(5)=dotproduct(k3,spvae6k1)
      abb143(6)=dotproduct(k3,spvak2e6)
      abb143(7)=dotproduct(k3,spvae6k4)
      abb143(8)=dotproduct(k3,spvak5e6)
      abb143(9)=dotproduct(k3,spvak2k1)
      abb143(10)=dotproduct(k3,spvak2k4)
      abb143(11)=dotproduct(k3,spvak5k1)
      abb143(12)=sqrt(mT**2)
      abb143(13)=1.0_ki/2.0_ki*spak5e6
      abb143(14)=-spbk5k4*abb143(13)
      abb143(14)=abb143(14)+1.0_ki/4.0_ki*abb143(7)
      abb143(15)=spbe6k1*spak2k5
      abb143(14)=abb143(15)*abb143(14)
      abb143(16)=1.0_ki/2.0_ki*spbk4k1
      abb143(17)=spbe6k4*spak2k5
      abb143(16)=-spak4e6*abb143(17)*abb143(16)
      abb143(18)=1.0_ki/2.0_ki*spbe6k4
      abb143(19)=spak5e6*spbk4k1
      abb143(20)=spak2k4*abb143(19)*abb143(18)
      abb143(21)=spbe6k1*spak5e6
      abb143(22)=abb143(10)*abb143(21)
      abb143(14)=abb143(20)+abb143(16)-1.0_ki/4.0_ki*abb143(22)+abb143(14)
      abb143(16)=gHZZ*c1*i_*TR*gel*abb143(3)*abb143(2)*abb143(1)
      abb143(20)=abb143(16)*gTr
      abb143(16)=abb143(16)*gTl
      abb143(22)=abb143(20)+1.0_ki/3.0_ki*abb143(16)
      abb143(14)=abb143(22)*abb143(14)
      abb143(23)=spak4k5*abb143(18)
      abb143(23)=abb143(23)+1.0_ki/4.0_ki*abb143(8)
      abb143(24)=spak2e6*spbk4k1
      abb143(23)=abb143(24)*abb143(23)
      abb143(25)=1.0_ki/2.0_ki*spak2k5
      abb143(25)=-spbe6k5*abb143(19)*abb143(25)
      abb143(26)=spbk5k1*abb143(17)*abb143(13)
      abb143(27)=spak2e6*spbe6k4
      abb143(28)=abb143(11)*abb143(27)
      abb143(23)=abb143(26)+abb143(25)-1.0_ki/4.0_ki*abb143(28)+abb143(23)
      abb143(25)=abb143(16)+1.0_ki/3.0_ki*abb143(20)
      abb143(23)=abb143(25)*abb143(23)
      abb143(26)=spak5k6*spbk6e6
      abb143(28)=abb143(26)*abb143(24)
      abb143(29)=abb143(25)*abb143(28)
      abb143(30)=spbk6k4*spae6k6
      abb143(31)=abb143(30)*abb143(15)
      abb143(32)=abb143(22)*abb143(31)
      abb143(29)=abb143(29)+abb143(32)
      abb143(32)=abb143(20)-abb143(16)
      abb143(33)=abb143(32)*abb143(17)
      abb143(34)=spae6k6*abb143(33)
      abb143(25)=-spak5k6*spak2e6*abb143(25)*abb143(18)
      abb143(35)=abb143(20)+abb143(16)
      abb143(36)=spak5e6*abb143(35)*spbe6k4
      abb143(37)=spak2k6*abb143(36)
      abb143(25)=2.0_ki/3.0_ki*abb143(37)+1.0_ki/3.0_ki*abb143(34)+abb143(25)
      abb143(25)=spbk6k1*abb143(25)
      abb143(34)=abb143(5)*abb143(33)
      abb143(37)=abb143(32)*abb143(19)
      abb143(38)=abb143(6)*abb143(37)
      abb143(34)=abb143(34)-abb143(38)
      abb143(38)=spbk6e6*abb143(37)
      abb143(22)=-spbk6k4*spbe6k1*abb143(22)*abb143(13)
      abb143(22)=-1.0_ki/3.0_ki*abb143(38)+abb143(22)
      abb143(22)=spak2k6*abb143(22)
      abb143(38)=abb143(35)*spbk4k1
      abb143(39)=abb143(38)*spak2k5
      abb143(40)=abb143(4)*abb143(39)
      abb143(41)=abb143(9)*abb143(36)
      abb143(14)=1.0_ki/3.0_ki*abb143(41)-2.0_ki/3.0_ki*abb143(40)+abb143(25)+1&
      &.0_ki/2.0_ki*abb143(29)+abb143(22)+abb143(23)+abb143(14)+1.0_ki/6.0_ki*a&
      &bb143(34)
      abb143(22)=abb143(28)-abb143(31)
      abb143(22)=abb143(32)*abb143(22)
      abb143(23)=spbk2k1*spak2e6
      abb143(25)=abb143(23)*abb143(33)
      abb143(29)=spak1k2*spbe6k1
      abb143(34)=abb143(29)*abb143(37)
      abb143(22)=abb143(34)+abb143(22)+abb143(25)
      abb143(25)=abb143(12)**2
      abb143(34)=2.0_ki*abb143(25)
      abb143(22)=abb143(22)*abb143(34)
      abb143(25)=abb143(25)*abb143(39)
      abb143(40)=spbk2k1*spak2k5
      abb143(41)=abb143(20)*abb143(40)*spak2k6*spbk6k4
      abb143(42)=spak1k2*spbk4k1
      abb143(43)=-abb143(16)*abb143(42)*spbk6k1*spak5k6
      abb143(25)=abb143(43)+abb143(25)+abb143(41)
      abb143(25)=4.0_ki*abb143(25)
      abb143(17)=-abb143(17)*abb143(20)*abb143(23)
      abb143(19)=abb143(16)*abb143(19)*abb143(29)
      abb143(17)=abb143(17)+abb143(19)
      abb143(17)=4.0_ki*abb143(17)
      abb143(19)=2.0_ki*abb143(35)
      abb143(28)=-abb143(28)*abb143(19)
      abb143(41)=abb143(20)+3.0_ki*abb143(16)
      abb143(43)=abb143(41)*abb143(30)
      abb143(44)=-abb143(15)*abb143(43)
      abb143(28)=abb143(28)+abb143(44)
      abb143(44)=8.0_ki*abb143(39)
      abb143(19)=-abb143(31)*abb143(19)
      abb143(31)=abb143(16)+3.0_ki*abb143(20)
      abb143(45)=abb143(31)*abb143(26)
      abb143(46)=-abb143(24)*abb143(45)
      abb143(19)=abb143(46)+abb143(19)
      abb143(39)=-4.0_ki*abb143(39)
      abb143(16)=abb143(34)*abb143(16)
      abb143(46)=-abb143(24)*abb143(16)
      abb143(47)=abb143(32)*abb143(24)
      abb143(48)=1.0_ki/2.0_ki*abb143(41)
      abb143(24)=abb143(24)*abb143(48)
      abb143(20)=abb143(34)*abb143(20)
      abb143(49)=-abb143(15)*abb143(20)
      abb143(50)=abb143(32)*abb143(15)
      abb143(51)=1.0_ki/2.0_ki*abb143(31)
      abb143(15)=abb143(15)*abb143(51)
      abb143(52)=1.0_ki/2.0_ki*abb143(32)
      abb143(53)=abb143(30)*abb143(52)*abb143(40)
      abb143(54)=abb143(26)*abb143(52)*abb143(42)
      abb143(55)=abb143(23)*spbk6e6
      abb143(51)=-spak2k5*abb143(51)*abb143(55)
      abb143(56)=abb143(32)*spbk6k4
      abb143(57)=-spak2k5*abb143(56)
      abb143(58)=abb143(29)*spae6k6
      abb143(48)=spbk4k1*abb143(48)*abb143(58)
      abb143(55)=-spak2k6*abb143(55)
      abb143(58)=-spbk6k1*abb143(58)
      abb143(55)=abb143(55)+abb143(58)
      abb143(55)=abb143(55)*abb143(52)
      abb143(58)=abb143(32)*abb143(23)
      abb143(59)=abb143(52)*spae6k6
      abb143(60)=-spbk6k1*abb143(59)
      abb143(58)=abb143(58)+abb143(60)
      abb143(60)=abb143(32)*abb143(29)
      abb143(52)=abb143(52)*spbk6e6
      abb143(61)=spak2k6*abb143(52)
      abb143(60)=abb143(60)+abb143(61)
      abb143(52)=spak2e6*abb143(52)
      abb143(16)=abb143(27)*abb143(16)
      abb143(61)=1.0_ki/2.0_ki*abb143(43)
      abb143(29)=-abb143(29)*abb143(61)
      abb143(16)=abb143(16)+abb143(29)
      abb143(29)=abb143(41)*abb143(42)
      abb143(42)=-spak2k6*abb143(56)
      abb143(29)=-2.0_ki*abb143(29)+abb143(42)
      abb143(27)=abb143(32)*abb143(27)
      abb143(41)=abb143(41)*abb143(18)
      abb143(42)=-spak2e6*abb143(41)
      abb143(56)=abb143(32)*spak5k6
      abb143(62)=spbk4k1*abb143(56)
      abb143(59)=-spbe6k1*abb143(59)
      abb143(20)=abb143(21)*abb143(20)
      abb143(63)=1.0_ki/2.0_ki*abb143(45)
      abb143(23)=abb143(23)*abb143(63)
      abb143(20)=abb143(20)+abb143(23)
      abb143(23)=abb143(31)*abb143(40)
      abb143(40)=spbk6k1*abb143(56)
      abb143(23)=2.0_ki*abb143(23)+abb143(40)
      abb143(21)=-abb143(32)*abb143(21)
      abb143(31)=abb143(31)*abb143(13)
      abb143(40)=-spbe6k1*abb143(31)
      abb143(56)=spbk4k2*spak2e6
      abb143(26)=abb143(26)*abb143(56)
      abb143(64)=spak1k5*spbe6k1
      abb143(30)=abb143(30)*abb143(64)
      abb143(26)=abb143(30)+abb143(26)
      abb143(26)=abb143(35)*abb143(26)
      abb143(30)=-abb143(36)*abb143(34)
      abb143(34)=spbe6k2*spak2k5
      abb143(61)=abb143(34)*abb143(61)
      abb143(65)=spak1e6*spbk4k1
      abb143(63)=abb143(65)*abb143(63)
      abb143(26)=abb143(63)+abb143(61)+abb143(30)+abb143(26)
      abb143(30)=-spbk6k4*spak5k6
      abb143(61)=spbk4k2*spak2k5
      abb143(30)=abb143(30)+abb143(61)
      abb143(30)=abb143(35)*abb143(30)
      abb143(35)=spak1k5*abb143(38)
      abb143(30)=abb143(35)+abb143(30)
      abb143(30)=4.0_ki*abb143(30)
      abb143(35)=4.0_ki*abb143(36)
      abb143(36)=2.0_ki*abb143(36)
      abb143(38)=abb143(65)+abb143(56)
      abb143(38)=abb143(32)*abb143(38)
      abb143(38)=abb143(43)+abb143(38)
      abb143(38)=1.0_ki/2.0_ki*abb143(38)
      abb143(34)=-abb143(34)-abb143(64)
      abb143(34)=abb143(32)*abb143(34)
      abb143(34)=abb143(45)+abb143(34)
      abb143(34)=1.0_ki/2.0_ki*abb143(34)
      abb143(43)=-1.0_ki/2.0_ki*abb143(33)
      abb143(13)=spbk4k2*abb143(32)*abb143(13)
      abb143(18)=-spak1k5*abb143(32)*abb143(18)
      abb143(32)=1.0_ki/2.0_ki*abb143(37)
      abb143(31)=spbk6e6*abb143(31)
      abb143(41)=spae6k6*abb143(41)
      R2d143=abb143(14)
      rat2 = rat2 + R2d143
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='143' value='", &
          & R2d143, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd143h1
