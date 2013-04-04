module     p12_sbars_hepemg_abbrevd43h1
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(93), public :: abb43
   complex(ki), public :: R2d43
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
      abb43(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb43(2)=sqrt(mT**2)
      abb43(3)=es12**(-1)
      abb43(4)=spbl3k2**(-1)
      abb43(5)=spak2l3**(-1)
      abb43(6)=spal3e6*spbl3k1
      abb43(7)=spbe6k4*spak2k5
      abb43(8)=abb43(6)*abb43(7)
      abb43(9)=spbe6l3*spak2l3
      abb43(10)=spak5e6*spbk4k1
      abb43(11)=abb43(9)*abb43(10)
      abb43(12)=-abb43(8)+abb43(11)
      abb43(13)=gTl-gTr
      abb43(14)=i_*TR*c1*gHT*gel*abb43(1)
      abb43(15)=abb43(3)*abb43(14)
      abb43(16)=-abb43(13)*abb43(15)*abb43(2)**3
      abb43(12)=abb43(16)*abb43(12)
      abb43(17)=mH**2
      abb43(18)=abb43(17)*abb43(4)
      abb43(19)=abb43(18)*abb43(5)
      abb43(13)=abb43(19)*abb43(13)*abb43(2)*abb43(14)
      abb43(14)=spak2e6*abb43(13)
      abb43(20)=abb43(16)*spak2e6
      abb43(21)=-abb43(20)+abb43(14)
      abb43(22)=spak5k6*spbk6e6
      abb43(23)=abb43(22)*spbk4k1
      abb43(21)=abb43(21)*abb43(23)
      abb43(15)=abb43(15)*abb43(2)
      abb43(24)=gTl*abb43(15)
      abb43(15)=gTr*abb43(15)
      abb43(25)=abb43(24)-abb43(15)
      abb43(26)=abb43(25)*spak2k5
      abb43(27)=abb43(18)*spbk2k1
      abb43(28)=-abb43(26)*abb43(27)
      abb43(29)=abb43(25)*spak2l3
      abb43(30)=abb43(29)*spbk2k1
      abb43(31)=spak2k5*abb43(30)
      abb43(28)=abb43(28)+abb43(31)
      abb43(28)=spbe6l3*abb43(28)
      abb43(31)=abb43(16)*spbe6k1
      abb43(32)=abb43(31)*spak2k5
      abb43(28)=abb43(32)+abb43(28)
      abb43(33)=spae6k6*spbk6k4
      abb43(28)=abb43(28)*abb43(33)
      abb43(34)=abb43(29)*abb43(6)
      abb43(35)=spak2e6*abb43(27)
      abb43(36)=abb43(25)*abb43(35)
      abb43(36)=abb43(34)+abb43(36)
      abb43(36)=abb43(22)*abb43(36)
      abb43(37)=abb43(31)*spak5e6
      abb43(38)=-spak2l3*abb43(37)
      abb43(36)=abb43(38)+abb43(36)
      abb43(36)=spbk4l3*abb43(36)
      abb43(38)=spbe6k4*abb43(20)
      abb43(39)=abb43(29)*spbe6l3
      abb43(40)=-abb43(33)*abb43(39)
      abb43(38)=abb43(38)+abb43(40)
      abb43(40)=spal3k5*spbl3k1
      abb43(38)=abb43(38)*abb43(40)
      abb43(31)=-abb43(10)*abb43(31)
      abb43(41)=abb43(6)*abb43(25)
      abb43(42)=abb43(23)*abb43(41)
      abb43(31)=abb43(31)+abb43(42)
      abb43(31)=spak1k2*abb43(31)
      abb43(42)=abb43(7)*spbk2k1
      abb43(43)=-abb43(20)*abb43(42)
      abb43(12)=abb43(31)+abb43(38)+abb43(36)+abb43(28)+abb43(43)+abb43(21)+abb&
      &43(12)
      abb43(12)=2.0_ki*abb43(12)
      abb43(21)=-spak2k5*abb43(13)
      abb43(28)=abb43(24)*spak5k6
      abb43(31)=abb43(28)*spak2l3
      abb43(36)=spbk6l3*abb43(31)
      abb43(21)=2.0_ki*abb43(36)+abb43(21)
      abb43(21)=spbk4k1*abb43(21)
      abb43(36)=spak1k2*spbk4k1
      abb43(38)=-abb43(25)*abb43(40)*abb43(36)
      abb43(43)=spbk2k1*spak2k5
      abb43(44)=abb43(43)-abb43(40)
      abb43(44)=spak2k6*abb43(44)
      abb43(45)=2.0_ki*spak2k5
      abb43(46)=spal3k6*spbl3k1*abb43(45)
      abb43(44)=abb43(46)+2.0_ki*abb43(44)
      abb43(44)=abb43(44)*abb43(15)*spbk6k4
      abb43(46)=abb43(29)*spbk4l3
      abb43(47)=-abb43(46)*abb43(43)
      abb43(28)=-abb43(36)*abb43(28)
      abb43(31)=-spbk4l3*abb43(31)
      abb43(28)=abb43(31)+abb43(28)
      abb43(28)=spbk6k1*abb43(28)
      abb43(21)=2.0_ki*abb43(28)+abb43(38)+abb43(47)+abb43(21)+abb43(44)
      abb43(21)=4.0_ki*abb43(21)
      abb43(28)=3.0_ki*abb43(24)
      abb43(31)=abb43(28)+abb43(15)
      abb43(11)=abb43(31)*abb43(11)
      abb43(38)=3.0_ki*abb43(15)
      abb43(44)=abb43(38)+abb43(24)
      abb43(8)=abb43(44)*abb43(8)
      abb43(8)=abb43(11)+abb43(8)
      abb43(11)=abb43(10)*spak1k2
      abb43(47)=spbk4l3*spak5e6
      abb43(48)=spak2l3*abb43(47)
      abb43(48)=abb43(11)+abb43(48)
      abb43(49)=7.0_ki*abb43(24)
      abb43(50)=abb43(49)+abb43(15)
      abb43(48)=abb43(50)*spbe6k1*abb43(48)
      abb43(51)=7.0_ki*abb43(15)
      abb43(52)=abb43(51)+abb43(24)
      abb43(53)=abb43(52)*spak2e6
      abb43(54)=abb43(25)*spak2e6
      abb43(55)=abb43(54)*abb43(19)
      abb43(56)=abb43(53)+abb43(55)
      abb43(56)=abb43(56)*spbk2k1
      abb43(57)=-abb43(7)*abb43(56)
      abb43(58)=abb43(53)*abb43(40)
      abb43(59)=spbe6k4*abb43(58)
      abb43(8)=abb43(59)+abb43(57)+abb43(48)-2.0_ki*abb43(8)
      abb43(48)=abb43(54)*spbk4k1
      abb43(57)=abb43(48)*abb43(22)
      abb43(59)=abb43(25)*spbe6k1
      abb43(60)=abb43(59)*spak2k5
      abb43(61)=abb43(60)*abb43(33)
      abb43(57)=abb43(57)-abb43(61)
      abb43(61)=abb43(25)*abb43(7)
      abb43(62)=abb43(61)*abb43(6)
      abb43(63)=abb43(39)*abb43(10)
      abb43(64)=abb43(62)-abb43(63)
      abb43(42)=abb43(54)*abb43(42)
      abb43(65)=abb43(47)*abb43(29)
      abb43(66)=spbe6k1*abb43(65)
      abb43(67)=abb43(54)*spbe6k4
      abb43(68)=-abb43(67)*abb43(40)
      abb43(69)=abb43(59)*abb43(11)
      abb43(42)=abb43(69)+abb43(68)+abb43(66)+abb43(42)+abb43(64)-3.0_ki*abb43(&
      &57)
      abb43(66)=abb43(53)+3.0_ki*abb43(55)
      abb43(68)=-abb43(66)*abb43(23)
      abb43(69)=abb43(25)*abb43(10)
      abb43(70)=-abb43(18)*abb43(69)
      abb43(71)=abb43(10)*abb43(29)
      abb43(70)=abb43(70)+abb43(71)
      abb43(70)=spbe6l3*abb43(70)
      abb43(19)=abb43(19)*abb43(25)
      abb43(71)=abb43(19)*abb43(7)
      abb43(71)=abb43(71)-abb43(61)
      abb43(72)=2.0_ki*abb43(6)
      abb43(72)=abb43(71)*abb43(72)
      abb43(73)=abb43(45)*abb43(24)
      abb43(74)=abb43(19)*spak2k5
      abb43(73)=abb43(73)-abb43(74)
      abb43(75)=abb43(33)*spbe6k1
      abb43(76)=abb43(73)*abb43(75)
      abb43(77)=abb43(59)*abb43(47)*abb43(18)
      abb43(78)=abb43(55)*spbe6k4
      abb43(79)=abb43(40)*abb43(78)
      abb43(11)=abb43(11)*abb43(19)
      abb43(80)=spbe6k1*abb43(11)
      abb43(68)=abb43(80)-2.0_ki*abb43(79)+abb43(77)-4.0_ki*abb43(76)+abb43(72)&
      &+abb43(68)+abb43(70)
      abb43(51)=abb43(51)+9.0_ki*abb43(24)
      abb43(45)=abb43(45)*spbk4k1
      abb43(70)=abb43(51)*abb43(45)
      abb43(72)=abb43(55)*spbk2k1
      abb43(76)=abb43(72)*abb43(7)
      abb43(23)=abb43(23)*abb43(15)
      abb43(77)=spak2e6*abb43(23)
      abb43(75)=abb43(75)*abb43(50)
      abb43(79)=-spak2k5*abb43(75)
      abb43(62)=abb43(79)-abb43(62)+2.0_ki*abb43(63)-abb43(76)-8.0_ki*abb43(77)
      abb43(49)=abb43(49)+9.0_ki*abb43(15)
      abb43(45)=abb43(49)*abb43(45)
      abb43(57)=-abb43(76)-abb43(64)-abb43(57)
      abb43(63)=1.0_ki/2.0_ki*abb43(25)
      abb43(64)=abb43(63)*abb43(36)
      abb43(64)=abb43(64)+1.0_ki/2.0_ki*abb43(46)
      abb43(64)=spbe6k1*abb43(64)
      abb43(76)=-spbk4k1*abb43(39)
      abb43(64)=abb43(76)+abb43(64)
      abb43(64)=spak5k6*abb43(64)
      abb43(76)=abb43(54)+abb43(55)
      abb43(76)=abb43(76)*abb43(43)
      abb43(77)=abb43(6)*abb43(26)
      abb43(79)=1.0_ki/2.0_ki*abb43(54)
      abb43(80)=-abb43(40)*abb43(79)
      abb43(76)=abb43(80)+1.0_ki/2.0_ki*abb43(76)+abb43(77)
      abb43(76)=spbk6k4*abb43(76)
      abb43(77)=1.0_ki/2.0_ki*spbk4k1*abb43(20)
      abb43(80)=spbk4k1*abb43(55)
      abb43(80)=-abb43(48)+abb43(80)
      abb43(32)=-1.0_ki/2.0_ki*abb43(32)
      abb43(26)=abb43(74)-abb43(26)
      abb43(81)=-spbe6k1*abb43(26)
      abb43(82)=-spak2l3*spbk2k1
      abb43(82)=abb43(27)+abb43(82)
      abb43(82)=abb43(61)*abb43(82)
      abb43(83)=abb43(29)*abb43(40)*spbe6k4
      abb43(82)=abb43(83)+abb43(82)
      abb43(82)=1.0_ki/2.0_ki*abb43(82)
      abb43(83)=abb43(69)*spak1k2
      abb43(65)=-abb43(65)-abb43(83)
      abb43(84)=1.0_ki/2.0_ki*spbl3k1
      abb43(65)=abb43(65)*abb43(84)
      abb43(13)=-abb43(16)-1.0_ki/2.0_ki*abb43(13)
      abb43(13)=abb43(10)*abb43(13)
      abb43(47)=abb43(47)*abb43(63)
      abb43(85)=-abb43(27)*abb43(47)
      abb43(13)=abb43(85)+abb43(13)
      abb43(85)=abb43(10)*abb43(19)
      abb43(85)=-abb43(69)+abb43(85)
      abb43(7)=abb43(16)*abb43(7)
      abb43(16)=-abb43(53)+abb43(55)
      abb43(16)=abb43(16)*abb43(43)
      abb43(16)=abb43(58)+abb43(16)
      abb43(43)=1.0_ki/2.0_ki*spbk6e6
      abb43(16)=abb43(43)*abb43(16)
      abb43(58)=4.0_ki*abb43(15)
      abb43(58)=-spbk6e6*spak2k5*abb43(6)*abb43(58)
      abb43(16)=abb43(58)+abb43(16)
      abb43(58)=spbk4l3*spak2l3
      abb43(58)=abb43(36)+abb43(58)
      abb43(86)=1.0_ki/2.0_ki*abb43(50)
      abb43(87)=spae6k6*spbe6k1
      abb43(58)=abb43(86)*abb43(87)*abb43(58)
      abb43(88)=spae6k6*spbk4k1*abb43(24)*abb43(9)
      abb43(58)=-4.0_ki*abb43(88)+abb43(58)
      abb43(88)=-abb43(72)-abb43(41)
      abb43(88)=spak2k6*spbk6e6*abb43(88)
      abb43(89)=spbk6k1*spae6k6*abb43(39)
      abb43(88)=abb43(88)+abb43(89)
      abb43(89)=spbe6k1*abb43(55)
      abb43(55)=abb43(55)-abb43(54)
      abb43(90)=-spbk2k1*abb43(55)
      abb43(41)=abb43(90)-abb43(41)
      abb43(90)=spak1k2*abb43(59)
      abb43(90)=abb43(39)+abb43(90)
      abb43(91)=abb43(54)*abb43(43)
      abb43(92)=abb43(25)*spbe6k4
      abb43(35)=-abb43(92)*abb43(35)
      abb43(34)=-spbe6k4*abb43(34)
      abb43(93)=-spak2l3*abb43(75)
      abb43(34)=abb43(93)+abb43(35)+abb43(34)
      abb43(34)=1.0_ki/2.0_ki*abb43(34)
      abb43(14)=-abb43(20)-abb43(14)
      abb43(14)=spbe6k4*abb43(14)
      abb43(20)=abb43(92)*abb43(6)
      abb43(20)=abb43(20)+abb43(75)
      abb43(35)=spak1k2*abb43(20)
      abb43(14)=abb43(14)-abb43(35)
      abb43(9)=abb43(31)*abb43(33)*abb43(9)
      abb43(9)=abb43(9)+1.0_ki/2.0_ki*abb43(14)
      abb43(14)=abb43(38)+13.0_ki*abb43(24)
      abb43(14)=abb43(14)*abb43(36)
      abb43(31)=-spak2k6*spbk6k4*abb43(25)
      abb43(14)=abb43(31)-abb43(46)+abb43(14)
      abb43(31)=-abb43(22)*abb43(53)
      abb43(35)=spak5e6*abb43(39)
      abb43(31)=abb43(31)+abb43(35)
      abb43(31)=abb43(31)*abb43(84)
      abb43(35)=abb43(22)*spbk4l3
      abb43(36)=-abb43(54)*abb43(35)
      abb43(38)=spbe6l3*abb43(83)
      abb43(36)=abb43(36)+abb43(38)
      abb43(36)=1.0_ki/2.0_ki*abb43(36)
      abb43(38)=-spbe6l3*abb43(79)
      abb43(39)=-abb43(63)*abb43(87)
      abb43(46)=abb43(22)*abb43(56)
      abb43(53)=abb43(25)*spak5e6
      abb43(27)=abb43(53)*abb43(27)
      abb43(30)=-spak5e6*abb43(30)
      abb43(27)=abb43(27)+abb43(30)
      abb43(27)=spbe6l3*abb43(27)
      abb43(27)=abb43(27)+abb43(37)+abb43(46)
      abb43(6)=abb43(44)*abb43(6)*abb43(22)
      abb43(6)=1.0_ki/2.0_ki*abb43(27)+abb43(6)
      abb43(27)=spbk6k1*spak5k6
      abb43(27)=abb43(27)+abb43(40)
      abb43(27)=abb43(25)*abb43(27)
      abb43(28)=-13.0_ki*abb43(15)-abb43(28)
      abb43(28)=spak2k5*abb43(28)
      abb43(28)=abb43(28)+abb43(74)
      abb43(28)=spbk2k1*abb43(28)
      abb43(27)=abb43(28)+abb43(27)
      abb43(28)=abb43(59)*spak5e6
      abb43(19)=abb43(19)*spak5e6
      abb43(30)=-spbe6k1*abb43(19)
      abb43(37)=-spal3e6*spbk2k1*abb43(71)
      abb43(40)=abb43(72)*spbe6k4
      abb43(44)=abb43(59)*abb43(33)
      abb43(44)=abb43(40)+abb43(44)
      abb43(44)=spal3k5*abb43(44)
      abb43(37)=abb43(37)+abb43(44)
      abb43(37)=1.0_ki/2.0_ki*abb43(37)
      abb43(44)=spal3e6*spbe6k1*abb43(63)
      abb43(46)=abb43(66)*abb43(22)
      abb43(54)=abb43(18)*abb43(53)
      abb43(29)=-spak5e6*abb43(29)
      abb43(29)=abb43(54)+abb43(29)
      abb43(29)=spbe6l3*abb43(29)
      abb43(29)=abb43(46)+abb43(29)
      abb43(46)=1.0_ki/2.0_ki*spbk4k2
      abb43(29)=abb43(29)*abb43(46)
      abb43(54)=2.0_ki*abb43(33)
      abb43(56)=abb43(73)*abb43(54)
      abb43(18)=-abb43(18)*abb43(47)
      abb43(11)=-1.0_ki/2.0_ki*abb43(11)+abb43(56)+abb43(18)
      abb43(11)=spbe6k2*abb43(11)
      abb43(17)=abb43(17)*abb43(5)
      abb43(18)=spbl3k2-abb43(17)
      abb43(18)=abb43(61)*abb43(18)
      abb43(56)=2.0_ki*abb43(25)
      abb43(35)=abb43(35)*abb43(56)
      abb43(18)=abb43(35)+abb43(18)
      abb43(18)=spal3e6*abb43(18)
      abb43(17)=abb43(17)*abb43(67)
      abb43(35)=abb43(25)*spbe6l3
      abb43(54)=-abb43(35)*abb43(54)
      abb43(17)=abb43(17)+abb43(54)
      abb43(17)=spal3k5*abb43(17)
      abb43(20)=abb43(40)+abb43(20)
      abb43(20)=spak1k5*abb43(20)
      abb43(23)=spak1e6*abb43(23)
      abb43(40)=-spak1l3*abb43(69)*spbe6l3
      abb43(11)=abb43(40)+4.0_ki*abb43(23)+1.0_ki/2.0_ki*abb43(20)+abb43(11)+ab&
      &b43(29)+abb43(17)+abb43(18)
      abb43(15)=abb43(24)+abb43(15)
      abb43(15)=8.0_ki*abb43(15)
      abb43(17)=spbk6k4*spak5k6*abb43(15)
      abb43(18)=-spbk4k2*spak2k5*abb43(51)
      abb43(20)=-spak1k5*spbk4k1*abb43(49)
      abb43(17)=abb43(20)+abb43(17)+abb43(18)
      abb43(15)=-spak5e6*spbe6k4*abb43(15)
      abb43(18)=abb43(63)*spbe6k4
      abb43(20)=-spak5k6*abb43(18)
      abb43(23)=abb43(63)*spak5e6
      abb43(24)=spbk6k4*abb43(23)
      abb43(29)=-spbk4l3*spal3e6
      abb43(40)=spak1e6*spbk4k1
      abb43(29)=abb43(40)+abb43(29)
      abb43(25)=abb43(25)*abb43(29)
      abb43(29)=-spbk4k2*abb43(55)
      abb43(33)=-abb43(50)*abb43(33)
      abb43(25)=abb43(29)+abb43(33)+abb43(25)
      abb43(25)=1.0_ki/2.0_ki*abb43(25)
      abb43(26)=spbe6k2*abb43(26)
      abb43(22)=-abb43(52)*abb43(22)
      abb43(29)=spal3k5*abb43(35)
      abb43(33)=-spak1k5*abb43(59)
      abb43(22)=abb43(33)+abb43(26)+abb43(22)+abb43(29)
      abb43(22)=1.0_ki/2.0_ki*abb43(22)
      abb43(26)=spal3k5*abb43(18)
      abb43(29)=1.0_ki/2.0_ki*abb43(71)
      abb43(33)=abb43(53)-abb43(19)
      abb43(33)=abb43(33)*abb43(46)
      abb43(35)=-spak1k5*abb43(18)
      abb43(10)=abb43(10)*abb43(63)
      abb43(40)=-spak5e6*abb43(52)*abb43(43)
      abb43(43)=-spae6k6*spbe6k4*abb43(86)
      abb43(18)=-spal3e6*abb43(18)
      abb43(46)=-1.0_ki/2.0_ki*abb43(78)
      abb43(23)=spbe6l3*abb43(23)
      abb43(19)=1.0_ki/2.0_ki*spbe6k2*abb43(19)
      R2d43=0.0_ki
      rat2 = rat2 + R2d43
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='43' value='", &
          & R2d43, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd43h1
