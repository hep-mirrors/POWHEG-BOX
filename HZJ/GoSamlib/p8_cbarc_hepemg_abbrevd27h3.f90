module     p8_cbarc_hepemg_abbrevd27h3
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(78), public :: abb27
   complex(ki), public :: R2d27
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
      abb27(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb27(2)=sqrt(mT**2)
      abb27(3)=es12**(-1)
      abb27(4)=spak2l3**(-1)
      abb27(5)=spbl3k2**(-1)
      abb27(6)=abb27(3)*abb27(2)**3
      abb27(7)=i_*TR*c1*gHT*ger*abb27(1)
      abb27(8)=abb27(7)*gTl
      abb27(9)=abb27(6)*abb27(8)
      abb27(10)=abb27(7)*gTr
      abb27(11)=abb27(6)*abb27(10)
      abb27(12)=abb27(9)-abb27(11)
      abb27(13)=spbe6k5*spak2e6
      abb27(14)=-abb27(12)*abb27(13)
      abb27(15)=abb27(2)*abb27(7)*abb27(3)
      abb27(16)=abb27(15)*gTr
      abb27(15)=abb27(15)*gTl
      abb27(17)=abb27(16)-abb27(15)
      abb27(18)=abb27(17)*spbe6k1
      abb27(19)=abb27(18)*spak1k2
      abb27(20)=spbk6k5*spae6k6
      abb27(21)=-abb27(20)*abb27(19)
      abb27(22)=spbk5k1*spbe6k1
      abb27(23)=abb27(22)*spak1k2
      abb27(24)=abb27(23)*spak1e6
      abb27(25)=abb27(17)*abb27(24)
      abb27(26)=abb27(17)*spak2e6
      abb27(27)=abb27(26)*spbk5k2
      abb27(28)=spak1k2*spbe6k1
      abb27(29)=abb27(27)*abb27(28)
      abb27(14)=abb27(29)+abb27(25)+abb27(14)+abb27(21)
      abb27(21)=spbl3k1*spal3k4
      abb27(14)=abb27(14)*abb27(21)
      abb27(25)=spbk5k1*spak2e6
      abb27(29)=spak4k6*spbk6e6
      abb27(30)=abb27(25)*abb27(29)
      abb27(31)=spbk2k1*spak2k4
      abb27(32)=abb27(31)*abb27(13)
      abb27(33)=mH**2*abb27(5)*abb27(4)
      abb27(34)=1.0_ki-abb27(33)
      abb27(34)=abb27(32)*abb27(34)
      abb27(35)=abb27(23)*spak4e6
      abb27(34)=abb27(30)+abb27(35)+abb27(34)
      abb27(34)=abb27(12)*abb27(34)
      abb27(36)=spak4e6*spbe6k1
      abb27(37)=abb27(12)*abb27(36)
      abb27(38)=spbk2k1*spak2e6
      abb27(39)=-abb27(17)*abb27(38)*abb27(29)
      abb27(37)=abb27(37)+abb27(39)
      abb27(39)=spak2l3*spbk5l3
      abb27(37)=abb27(37)*abb27(39)
      abb27(8)=abb27(8)-abb27(10)
      abb27(8)=abb27(33)*abb27(2)*abb27(8)
      abb27(10)=abb27(8)-abb27(12)
      abb27(40)=spak2k4*spbe6k1
      abb27(41)=abb27(20)*abb27(40)
      abb27(42)=spak1e6*abb27(40)*spbk5k1
      abb27(41)=abb27(41)-abb27(42)
      abb27(42)=abb27(40)*spak2e6
      abb27(43)=-spbk5k2*abb27(42)
      abb27(43)=abb27(43)+abb27(41)
      abb27(10)=abb27(10)*abb27(43)
      abb27(43)=-spak2e6*abb27(12)*abb27(22)
      abb27(44)=abb27(17)*spak2l3
      abb27(45)=abb27(44)*spbk5l3
      abb27(46)=spbe6k1*abb27(38)*abb27(45)
      abb27(43)=abb27(43)+abb27(46)
      abb27(43)=spak1k4*abb27(43)
      abb27(10)=abb27(43)+abb27(37)+abb27(14)+abb27(34)+abb27(10)
      abb27(10)=2.0_ki*abb27(10)
      abb27(6)=abb27(2)-abb27(6)
      abb27(14)=-gTr-gTl
      abb27(6)=abb27(14)*abb27(7)*abb27(6)
      abb27(6)=2.0_ki*abb27(6)-abb27(8)
      abb27(7)=spbk5k1*spak2k4
      abb27(6)=abb27(7)*abb27(6)
      abb27(8)=abb27(17)*spbk5k1
      abb27(14)=spak1k2*abb27(8)*abb27(21)
      abb27(21)=abb27(31)*abb27(45)
      abb27(6)=abb27(21)+abb27(14)+abb27(6)
      abb27(14)=abb27(16)+abb27(15)
      abb27(21)=2.0_ki*abb27(14)
      abb27(33)=abb27(33)*abb27(17)
      abb27(34)=abb27(21)-abb27(33)
      abb27(37)=abb27(31)*abb27(34)
      abb27(43)=abb27(17)*spbl3k1
      abb27(46)=abb27(43)*spal3k4
      abb27(37)=abb27(37)-abb27(46)
      abb27(37)=spak2k6*spbk6k5*abb27(37)
      abb27(47)=spak1k2*spbk5k1
      abb27(48)=-abb27(21)*abb27(47)
      abb27(48)=abb27(48)+abb27(45)
      abb27(48)=spbk6k1*spak4k6*abb27(48)
      abb27(6)=abb27(48)+2.0_ki*abb27(6)+abb27(37)
      abb27(6)=2.0_ki*abb27(6)
      abb27(37)=abb27(33)*abb27(32)
      abb27(48)=abb27(45)*abb27(36)
      abb27(49)=abb27(13)*spal3k4
      abb27(43)=abb27(49)*abb27(43)
      abb27(43)=-abb27(37)+abb27(48)-abb27(43)
      abb27(48)=abb27(35)-abb27(32)
      abb27(48)=abb27(21)*abb27(48)
      abb27(48)=abb27(48)-abb27(43)
      abb27(48)=2.0_ki*abb27(48)
      abb27(50)=abb27(8)*spak1e6
      abb27(51)=abb27(20)*abb27(17)
      abb27(27)=abb27(27)+abb27(50)-abb27(51)
      abb27(50)=abb27(27)*abb27(40)
      abb27(51)=abb27(32)+abb27(35)
      abb27(51)=abb27(17)*abb27(51)
      abb27(52)=abb27(22)*spak1k4
      abb27(53)=abb27(29)*spbk5k1
      abb27(54)=abb27(53)-abb27(52)
      abb27(54)=abb27(26)*abb27(54)
      abb27(50)=abb27(50)+abb27(54)+abb27(51)
      abb27(51)=-2.0_ki*abb27(50)
      abb27(34)=-abb27(34)*abb27(41)
      abb27(41)=2.0_ki*abb27(17)
      abb27(54)=spbl3k1*abb27(49)*abb27(41)
      abb27(55)=abb27(41)*abb27(36)
      abb27(39)=abb27(39)*abb27(55)
      abb27(39)=abb27(54)-abb27(39)
      abb27(54)=abb27(32)*abb27(21)
      abb27(56)=abb27(21)+abb27(33)
      abb27(35)=-abb27(56)*abb27(35)
      abb27(57)=3.0_ki*abb27(15)
      abb27(58)=abb27(57)+abb27(16)
      abb27(59)=abb27(25)*abb27(58)
      abb27(60)=-abb27(33)*abb27(25)
      abb27(60)=-abb27(59)+abb27(60)
      abb27(60)=abb27(60)*abb27(29)
      abb27(42)=abb27(21)*abb27(42)
      abb27(61)=abb27(33)*spak2e6
      abb27(62)=-abb27(40)*abb27(61)
      abb27(42)=abb27(42)+abb27(62)
      abb27(42)=spbk5k2*abb27(42)
      abb27(62)=abb27(58)*spak2e6
      abb27(63)=abb27(62)+abb27(61)
      abb27(63)=abb27(63)*abb27(52)
      abb27(34)=abb27(63)+abb27(42)+abb27(60)+abb27(35)+abb27(54)-3.0_ki*abb27(&
      &37)-abb27(39)+abb27(34)
      abb27(34)=2.0_ki*abb27(34)
      abb27(35)=abb27(8)*spak2k4
      abb27(35)=4.0_ki*abb27(35)
      abb27(32)=abb27(14)*abb27(32)
      abb27(42)=abb27(14)*spak4e6
      abb27(54)=-abb27(23)*abb27(42)
      abb27(32)=abb27(54)+abb27(32)-abb27(37)
      abb27(37)=spak2e6*abb27(52)
      abb27(30)=abb27(37)-abb27(30)
      abb27(30)=abb27(21)*abb27(30)
      abb27(37)=spbk5k2*spak2e6
      abb27(52)=spak1e6*spbk5k1
      abb27(52)=abb27(37)+abb27(52)-abb27(20)
      abb27(54)=3.0_ki*abb27(16)
      abb27(60)=abb27(54)+abb27(15)
      abb27(63)=abb27(60)*abb27(40)
      abb27(52)=abb27(63)*abb27(52)
      abb27(30)=2.0_ki*abb27(32)+abb27(52)+abb27(30)-abb27(39)
      abb27(30)=2.0_ki*abb27(30)
      abb27(32)=abb27(43)-abb27(50)
      abb27(32)=2.0_ki*abb27(32)
      abb27(39)=-8.0_ki*abb27(14)*abb27(7)
      abb27(43)=-3.0_ki*abb27(11)-abb27(9)
      abb27(40)=abb27(43)*abb27(40)
      abb27(43)=2.0_ki*abb27(63)
      abb27(50)=-abb27(11)-3.0_ki*abb27(9)
      abb27(25)=abb27(50)*abb27(25)
      abb27(50)=2.0_ki*abb27(59)
      abb27(52)=-spak4e6*spbk5k1*abb27(12)
      abb27(64)=abb27(8)*spak4e6
      abb27(65)=2.0_ki*abb27(64)
      abb27(66)=spbe6k5*spak2k4
      abb27(12)=abb27(12)*abb27(66)
      abb27(67)=abb27(41)*abb27(66)
      abb27(66)=abb27(17)*abb27(66)
      abb27(68)=abb27(60)*spbk6e6
      abb27(69)=-spak2k4*abb27(38)*abb27(68)
      abb27(70)=spbk6k5*spak2k4*abb27(41)
      abb27(71)=abb27(58)*spae6k6
      abb27(72)=abb27(23)*abb27(71)
      abb27(73)=-spak1l3*spbe6k1
      abb27(74)=spal3k6*spbk6e6
      abb27(73)=abb27(74)+abb27(73)
      abb27(73)=abb27(26)*abb27(73)
      abb27(74)=spak1e6*spbe6k1*abb27(44)
      abb27(75)=spal3e6*abb27(19)
      abb27(73)=abb27(75)+abb27(74)+abb27(73)
      abb27(73)=spbl3k1*abb27(73)
      abb27(74)=spbe6l3*abb27(38)
      abb27(75)=-spbk6l3*spae6k6*spbe6k1
      abb27(74)=abb27(75)+abb27(74)
      abb27(44)=abb27(44)*abb27(74)
      abb27(74)=abb27(17)+abb27(33)
      abb27(74)=spak2k6*spbk6e6*abb27(38)*abb27(74)
      abb27(19)=abb27(19)*spbk6k1*spae6k6
      abb27(75)=spbl3k2*spak2l3*spbe6k1*abb27(26)
      abb27(19)=abb27(75)+abb27(19)+abb27(74)+abb27(73)+abb27(44)
      abb27(44)=abb27(17)*spbk6k1
      abb27(73)=spae6k6*abb27(44)
      abb27(74)=abb27(17)*spak2k6
      abb27(75)=-spbk6e6*abb27(74)
      abb27(26)=-spbk6e6*abb27(26)
      abb27(23)=abb27(23)*abb27(62)
      abb27(76)=abb27(13)*abb27(9)
      abb27(28)=abb27(28)*abb27(15)
      abb27(77)=-abb27(20)*abb27(28)
      abb27(28)=abb27(28)*abb27(37)
      abb27(28)=abb27(28)+abb27(76)+abb27(77)
      abb27(37)=abb27(16)+7.0_ki*abb27(15)
      abb27(24)=abb27(37)*abb27(24)
      abb27(24)=abb27(24)+4.0_ki*abb27(28)
      abb27(28)=abb27(37)*abb27(47)
      abb27(37)=spbk6k5*abb27(74)
      abb27(28)=abb27(28)+abb27(37)
      abb27(28)=2.0_ki*abb27(28)
      abb27(37)=-abb27(13)*abb27(41)
      abb27(13)=abb27(13)*abb27(15)
      abb27(41)=8.0_ki*abb27(13)
      abb27(13)=-4.0_ki*abb27(13)
      abb27(8)=-2.0_ki*spak4k6*abb27(8)
      abb27(18)=spae6k6*abb27(18)
      abb27(47)=spak1k4*spbe6k1
      abb27(47)=abb27(47)-abb27(29)
      abb27(74)=-abb27(38)*abb27(16)*abb27(47)
      abb27(76)=abb27(11)*abb27(36)
      abb27(74)=abb27(76)+abb27(74)
      abb27(74)=4.0_ki*abb27(74)
      abb27(76)=-7.0_ki*abb27(16)-abb27(15)
      abb27(76)=abb27(76)*abb27(31)
      abb27(44)=-spak4k6*abb27(44)
      abb27(44)=abb27(76)+abb27(44)
      abb27(44)=2.0_ki*abb27(44)
      abb27(76)=abb27(36)*abb27(16)
      abb27(77)=8.0_ki*abb27(76)
      abb27(76)=-4.0_ki*abb27(76)
      abb27(56)=abb27(20)*spak2k4*abb27(56)
      abb27(14)=4.0_ki*abb27(14)
      abb27(78)=-abb27(14)-abb27(33)
      abb27(7)=spak1e6*abb27(7)*abb27(78)
      abb27(45)=-spak4e6*abb27(45)
      abb27(7)=abb27(45)+abb27(56)+abb27(7)
      abb27(7)=spbe6k2*abb27(7)
      abb27(27)=-spbe6l3*spal3k4*abb27(27)
      abb27(45)=spbe6k2*spak2k4
      abb27(45)=abb27(45)+abb27(47)
      abb27(45)=spal3e6*abb27(45)
      abb27(47)=-spak1l3*abb27(36)
      abb27(45)=abb27(47)+abb27(45)
      abb27(45)=abb27(45)*abb27(17)*spbk5l3
      abb27(31)=abb27(31)*abb27(33)
      abb27(31)=abb27(46)+abb27(31)
      abb27(31)=spbe6k5*abb27(31)
      abb27(21)=abb27(21)*abb27(53)
      abb27(21)=abb27(21)+abb27(31)
      abb27(21)=spak1e6*abb27(21)
      abb27(9)=-abb27(11)-abb27(9)
      abb27(9)=spak4e6*spbe6k5*abb27(9)
      abb27(11)=abb27(62)-abb27(61)
      abb27(11)=abb27(11)*abb27(29)
      abb27(31)=-spak1k2*abb27(36)*abb27(33)
      abb27(11)=abb27(31)+abb27(11)
      abb27(11)=spbk5k2*abb27(11)
      abb27(31)=-spak2e6*abb27(14)
      abb27(31)=abb27(31)+abb27(61)
      abb27(31)=spbk5k2*abb27(31)
      abb27(33)=abb27(60)*abb27(20)
      abb27(31)=abb27(31)+abb27(33)
      abb27(31)=spbe6k1*abb27(31)
      abb27(16)=abb27(57)+5.0_ki*abb27(16)
      abb27(22)=-spak1e6*abb27(16)*abb27(22)
      abb27(22)=abb27(22)+abb27(31)
      abb27(22)=spak1k4*abb27(22)
      abb27(17)=spbl3k2*abb27(17)*abb27(49)
      abb27(7)=abb27(17)+abb27(27)+abb27(7)+abb27(22)+abb27(11)+4.0_ki*abb27(9)&
      &+abb27(21)+abb27(45)
      abb27(9)=spak4k6*spbk6k5*abb27(14)
      abb27(11)=-abb27(54)-5.0_ki*abb27(15)
      abb27(11)=spbk5k2*spak2k4*abb27(11)
      abb27(14)=-spak1k4*spbk5k1*abb27(16)
      abb27(9)=abb27(14)+abb27(9)+abb27(11)
      abb27(9)=2.0_ki*abb27(9)
      abb27(11)=abb27(42)*spbe6k5
      abb27(14)=8.0_ki*abb27(11)
      abb27(11)=4.0_ki*abb27(11)
      abb27(15)=-abb27(60)*abb27(29)
      abb27(16)=-abb27(58)*abb27(20)
      abb27(17)=-spak4e6*abb27(68)
      abb27(20)=-spbe6k5*abb27(71)
      abb27(21)=-abb27(38)*abb27(63)
      R2d27=0.0_ki
      rat2 = rat2 + R2d27
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='27' value='", &
          & R2d27, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd27h3
