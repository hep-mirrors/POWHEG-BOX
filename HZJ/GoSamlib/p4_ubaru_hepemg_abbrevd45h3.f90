module     p4_ubaru_hepemg_abbrevd45h3
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(78), public :: abb45
   complex(ki), public :: R2d45
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p4_ubaru_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_color, only: TR
      use p4_ubaru_hepemg_globalsl1, only: epspow
      implicit none
      abb45(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb45(2)=sqrt(mT**2)
      abb45(3)=es12**(-1)
      abb45(4)=spak2l3**(-1)
      abb45(5)=spbl3k2**(-1)
      abb45(6)=abb45(3)*abb45(2)**3
      abb45(7)=i_*TR*c1*gHT*ger*abb45(1)
      abb45(8)=abb45(7)*gTl
      abb45(9)=abb45(6)*abb45(8)
      abb45(10)=abb45(7)*gTr
      abb45(11)=abb45(6)*abb45(10)
      abb45(12)=abb45(9)-abb45(11)
      abb45(13)=spbe6k5*spak2e6
      abb45(14)=-abb45(12)*abb45(13)
      abb45(15)=abb45(2)*abb45(7)*abb45(3)
      abb45(16)=abb45(15)*gTr
      abb45(15)=abb45(15)*gTl
      abb45(17)=abb45(16)-abb45(15)
      abb45(18)=abb45(17)*spbe6k1
      abb45(19)=abb45(18)*spak1k2
      abb45(20)=spbk6k5*spae6k6
      abb45(21)=-abb45(20)*abb45(19)
      abb45(22)=spbk5k1*spbe6k1
      abb45(23)=abb45(22)*spak1k2
      abb45(24)=abb45(23)*spak1e6
      abb45(25)=abb45(17)*abb45(24)
      abb45(26)=abb45(17)*spak2e6
      abb45(27)=abb45(26)*spbk5k2
      abb45(28)=spak1k2*spbe6k1
      abb45(29)=abb45(27)*abb45(28)
      abb45(14)=abb45(29)+abb45(25)+abb45(14)+abb45(21)
      abb45(21)=spbl3k1*spal3k4
      abb45(14)=abb45(14)*abb45(21)
      abb45(25)=spbk5k1*spak2e6
      abb45(29)=spak4k6*spbk6e6
      abb45(30)=abb45(25)*abb45(29)
      abb45(31)=spbk2k1*spak2k4
      abb45(32)=abb45(31)*abb45(13)
      abb45(33)=mH**2*abb45(5)*abb45(4)
      abb45(34)=1.0_ki-abb45(33)
      abb45(34)=abb45(32)*abb45(34)
      abb45(35)=abb45(23)*spak4e6
      abb45(34)=abb45(30)+abb45(35)+abb45(34)
      abb45(34)=abb45(12)*abb45(34)
      abb45(36)=spak4e6*spbe6k1
      abb45(37)=abb45(12)*abb45(36)
      abb45(38)=spbk2k1*spak2e6
      abb45(39)=-abb45(17)*abb45(38)*abb45(29)
      abb45(37)=abb45(37)+abb45(39)
      abb45(39)=spak2l3*spbk5l3
      abb45(37)=abb45(37)*abb45(39)
      abb45(8)=abb45(8)-abb45(10)
      abb45(8)=abb45(33)*abb45(2)*abb45(8)
      abb45(10)=abb45(8)-abb45(12)
      abb45(40)=spak2k4*spbe6k1
      abb45(41)=abb45(20)*abb45(40)
      abb45(42)=spak1e6*abb45(40)*spbk5k1
      abb45(41)=abb45(41)-abb45(42)
      abb45(42)=abb45(40)*spak2e6
      abb45(43)=-spbk5k2*abb45(42)
      abb45(43)=abb45(43)+abb45(41)
      abb45(10)=abb45(10)*abb45(43)
      abb45(43)=-spak2e6*abb45(12)*abb45(22)
      abb45(44)=abb45(17)*spak2l3
      abb45(45)=abb45(44)*spbk5l3
      abb45(46)=spbe6k1*abb45(38)*abb45(45)
      abb45(43)=abb45(43)+abb45(46)
      abb45(43)=spak1k4*abb45(43)
      abb45(10)=abb45(43)+abb45(37)+abb45(14)+abb45(34)+abb45(10)
      abb45(10)=2.0_ki*abb45(10)
      abb45(6)=abb45(2)-abb45(6)
      abb45(14)=-gTr-gTl
      abb45(6)=abb45(14)*abb45(7)*abb45(6)
      abb45(6)=2.0_ki*abb45(6)-abb45(8)
      abb45(7)=spbk5k1*spak2k4
      abb45(6)=abb45(7)*abb45(6)
      abb45(8)=abb45(17)*spbk5k1
      abb45(14)=spak1k2*abb45(8)*abb45(21)
      abb45(21)=abb45(31)*abb45(45)
      abb45(6)=abb45(21)+abb45(14)+abb45(6)
      abb45(14)=abb45(16)+abb45(15)
      abb45(21)=2.0_ki*abb45(14)
      abb45(33)=abb45(33)*abb45(17)
      abb45(34)=abb45(21)-abb45(33)
      abb45(37)=abb45(31)*abb45(34)
      abb45(43)=abb45(17)*spbl3k1
      abb45(46)=abb45(43)*spal3k4
      abb45(37)=abb45(37)-abb45(46)
      abb45(37)=spak2k6*spbk6k5*abb45(37)
      abb45(47)=spak1k2*spbk5k1
      abb45(48)=-abb45(21)*abb45(47)
      abb45(48)=abb45(48)+abb45(45)
      abb45(48)=spbk6k1*spak4k6*abb45(48)
      abb45(6)=abb45(48)+2.0_ki*abb45(6)+abb45(37)
      abb45(6)=2.0_ki*abb45(6)
      abb45(37)=abb45(33)*abb45(32)
      abb45(48)=abb45(45)*abb45(36)
      abb45(49)=abb45(13)*spal3k4
      abb45(43)=abb45(49)*abb45(43)
      abb45(43)=-abb45(37)+abb45(48)-abb45(43)
      abb45(48)=abb45(35)-abb45(32)
      abb45(48)=abb45(21)*abb45(48)
      abb45(48)=abb45(48)-abb45(43)
      abb45(48)=2.0_ki*abb45(48)
      abb45(50)=abb45(8)*spak1e6
      abb45(51)=abb45(20)*abb45(17)
      abb45(27)=abb45(27)+abb45(50)-abb45(51)
      abb45(50)=abb45(27)*abb45(40)
      abb45(51)=abb45(32)+abb45(35)
      abb45(51)=abb45(17)*abb45(51)
      abb45(52)=abb45(22)*spak1k4
      abb45(53)=abb45(29)*spbk5k1
      abb45(54)=abb45(53)-abb45(52)
      abb45(54)=abb45(26)*abb45(54)
      abb45(50)=abb45(50)+abb45(54)+abb45(51)
      abb45(51)=-2.0_ki*abb45(50)
      abb45(34)=-abb45(34)*abb45(41)
      abb45(41)=2.0_ki*abb45(17)
      abb45(54)=spbl3k1*abb45(49)*abb45(41)
      abb45(55)=abb45(41)*abb45(36)
      abb45(39)=abb45(39)*abb45(55)
      abb45(39)=abb45(54)-abb45(39)
      abb45(54)=abb45(32)*abb45(21)
      abb45(56)=abb45(21)+abb45(33)
      abb45(35)=-abb45(56)*abb45(35)
      abb45(57)=3.0_ki*abb45(15)
      abb45(58)=abb45(57)+abb45(16)
      abb45(59)=abb45(25)*abb45(58)
      abb45(60)=-abb45(33)*abb45(25)
      abb45(60)=-abb45(59)+abb45(60)
      abb45(60)=abb45(60)*abb45(29)
      abb45(42)=abb45(21)*abb45(42)
      abb45(61)=abb45(33)*spak2e6
      abb45(62)=-abb45(40)*abb45(61)
      abb45(42)=abb45(42)+abb45(62)
      abb45(42)=spbk5k2*abb45(42)
      abb45(62)=abb45(58)*spak2e6
      abb45(63)=abb45(62)+abb45(61)
      abb45(63)=abb45(63)*abb45(52)
      abb45(34)=abb45(63)+abb45(42)+abb45(60)+abb45(35)+abb45(54)-3.0_ki*abb45(&
      &37)-abb45(39)+abb45(34)
      abb45(34)=2.0_ki*abb45(34)
      abb45(35)=abb45(8)*spak2k4
      abb45(35)=4.0_ki*abb45(35)
      abb45(32)=abb45(14)*abb45(32)
      abb45(42)=abb45(14)*spak4e6
      abb45(54)=-abb45(23)*abb45(42)
      abb45(32)=abb45(54)+abb45(32)-abb45(37)
      abb45(37)=spak2e6*abb45(52)
      abb45(30)=abb45(37)-abb45(30)
      abb45(30)=abb45(21)*abb45(30)
      abb45(37)=spbk5k2*spak2e6
      abb45(52)=spak1e6*spbk5k1
      abb45(52)=abb45(37)+abb45(52)-abb45(20)
      abb45(54)=3.0_ki*abb45(16)
      abb45(60)=abb45(54)+abb45(15)
      abb45(63)=abb45(60)*abb45(40)
      abb45(52)=abb45(63)*abb45(52)
      abb45(30)=2.0_ki*abb45(32)+abb45(52)+abb45(30)-abb45(39)
      abb45(30)=2.0_ki*abb45(30)
      abb45(32)=abb45(43)-abb45(50)
      abb45(32)=2.0_ki*abb45(32)
      abb45(39)=-8.0_ki*abb45(14)*abb45(7)
      abb45(43)=-3.0_ki*abb45(11)-abb45(9)
      abb45(40)=abb45(43)*abb45(40)
      abb45(43)=2.0_ki*abb45(63)
      abb45(50)=-abb45(11)-3.0_ki*abb45(9)
      abb45(25)=abb45(50)*abb45(25)
      abb45(50)=2.0_ki*abb45(59)
      abb45(52)=-spak4e6*spbk5k1*abb45(12)
      abb45(64)=abb45(8)*spak4e6
      abb45(65)=2.0_ki*abb45(64)
      abb45(66)=spbe6k5*spak2k4
      abb45(12)=abb45(12)*abb45(66)
      abb45(67)=abb45(41)*abb45(66)
      abb45(66)=abb45(17)*abb45(66)
      abb45(68)=abb45(60)*spbk6e6
      abb45(69)=-spak2k4*abb45(38)*abb45(68)
      abb45(70)=spbk6k5*spak2k4*abb45(41)
      abb45(71)=abb45(58)*spae6k6
      abb45(72)=abb45(23)*abb45(71)
      abb45(73)=-spak1l3*spbe6k1
      abb45(74)=spal3k6*spbk6e6
      abb45(73)=abb45(74)+abb45(73)
      abb45(73)=abb45(26)*abb45(73)
      abb45(74)=spak1e6*spbe6k1*abb45(44)
      abb45(75)=spal3e6*abb45(19)
      abb45(73)=abb45(75)+abb45(74)+abb45(73)
      abb45(73)=spbl3k1*abb45(73)
      abb45(74)=spbe6l3*abb45(38)
      abb45(75)=-spbk6l3*spae6k6*spbe6k1
      abb45(74)=abb45(75)+abb45(74)
      abb45(44)=abb45(44)*abb45(74)
      abb45(74)=abb45(17)+abb45(33)
      abb45(74)=spak2k6*spbk6e6*abb45(38)*abb45(74)
      abb45(19)=abb45(19)*spbk6k1*spae6k6
      abb45(75)=spbl3k2*spak2l3*spbe6k1*abb45(26)
      abb45(19)=abb45(75)+abb45(19)+abb45(74)+abb45(73)+abb45(44)
      abb45(44)=abb45(17)*spbk6k1
      abb45(73)=spae6k6*abb45(44)
      abb45(74)=abb45(17)*spak2k6
      abb45(75)=-spbk6e6*abb45(74)
      abb45(26)=-spbk6e6*abb45(26)
      abb45(23)=abb45(23)*abb45(62)
      abb45(76)=abb45(13)*abb45(9)
      abb45(28)=abb45(28)*abb45(15)
      abb45(77)=-abb45(20)*abb45(28)
      abb45(28)=abb45(28)*abb45(37)
      abb45(28)=abb45(28)+abb45(76)+abb45(77)
      abb45(37)=abb45(16)+7.0_ki*abb45(15)
      abb45(24)=abb45(37)*abb45(24)
      abb45(24)=abb45(24)+4.0_ki*abb45(28)
      abb45(28)=abb45(37)*abb45(47)
      abb45(37)=spbk6k5*abb45(74)
      abb45(28)=abb45(28)+abb45(37)
      abb45(28)=2.0_ki*abb45(28)
      abb45(37)=-abb45(13)*abb45(41)
      abb45(13)=abb45(13)*abb45(15)
      abb45(41)=8.0_ki*abb45(13)
      abb45(13)=-4.0_ki*abb45(13)
      abb45(8)=-2.0_ki*spak4k6*abb45(8)
      abb45(18)=spae6k6*abb45(18)
      abb45(47)=spak1k4*spbe6k1
      abb45(47)=abb45(47)-abb45(29)
      abb45(74)=-abb45(38)*abb45(16)*abb45(47)
      abb45(76)=abb45(11)*abb45(36)
      abb45(74)=abb45(76)+abb45(74)
      abb45(74)=4.0_ki*abb45(74)
      abb45(76)=-7.0_ki*abb45(16)-abb45(15)
      abb45(76)=abb45(76)*abb45(31)
      abb45(44)=-spak4k6*abb45(44)
      abb45(44)=abb45(76)+abb45(44)
      abb45(44)=2.0_ki*abb45(44)
      abb45(76)=abb45(36)*abb45(16)
      abb45(77)=8.0_ki*abb45(76)
      abb45(76)=-4.0_ki*abb45(76)
      abb45(56)=abb45(20)*spak2k4*abb45(56)
      abb45(14)=4.0_ki*abb45(14)
      abb45(78)=-abb45(14)-abb45(33)
      abb45(7)=spak1e6*abb45(7)*abb45(78)
      abb45(45)=-spak4e6*abb45(45)
      abb45(7)=abb45(45)+abb45(56)+abb45(7)
      abb45(7)=spbe6k2*abb45(7)
      abb45(27)=-spbe6l3*spal3k4*abb45(27)
      abb45(45)=spbe6k2*spak2k4
      abb45(45)=abb45(45)+abb45(47)
      abb45(45)=spal3e6*abb45(45)
      abb45(47)=-spak1l3*abb45(36)
      abb45(45)=abb45(47)+abb45(45)
      abb45(45)=abb45(45)*abb45(17)*spbk5l3
      abb45(31)=abb45(31)*abb45(33)
      abb45(31)=abb45(46)+abb45(31)
      abb45(31)=spbe6k5*abb45(31)
      abb45(21)=abb45(21)*abb45(53)
      abb45(21)=abb45(21)+abb45(31)
      abb45(21)=spak1e6*abb45(21)
      abb45(9)=-abb45(11)-abb45(9)
      abb45(9)=spak4e6*spbe6k5*abb45(9)
      abb45(11)=abb45(62)-abb45(61)
      abb45(11)=abb45(11)*abb45(29)
      abb45(31)=-spak1k2*abb45(36)*abb45(33)
      abb45(11)=abb45(31)+abb45(11)
      abb45(11)=spbk5k2*abb45(11)
      abb45(31)=-spak2e6*abb45(14)
      abb45(31)=abb45(31)+abb45(61)
      abb45(31)=spbk5k2*abb45(31)
      abb45(33)=abb45(60)*abb45(20)
      abb45(31)=abb45(31)+abb45(33)
      abb45(31)=spbe6k1*abb45(31)
      abb45(16)=abb45(57)+5.0_ki*abb45(16)
      abb45(22)=-spak1e6*abb45(16)*abb45(22)
      abb45(22)=abb45(22)+abb45(31)
      abb45(22)=spak1k4*abb45(22)
      abb45(17)=spbl3k2*abb45(17)*abb45(49)
      abb45(7)=abb45(17)+abb45(27)+abb45(7)+abb45(22)+abb45(11)+4.0_ki*abb45(9)&
      &+abb45(21)+abb45(45)
      abb45(9)=spak4k6*spbk6k5*abb45(14)
      abb45(11)=-abb45(54)-5.0_ki*abb45(15)
      abb45(11)=spbk5k2*spak2k4*abb45(11)
      abb45(14)=-spak1k4*spbk5k1*abb45(16)
      abb45(9)=abb45(14)+abb45(9)+abb45(11)
      abb45(9)=2.0_ki*abb45(9)
      abb45(11)=abb45(42)*spbe6k5
      abb45(14)=8.0_ki*abb45(11)
      abb45(11)=4.0_ki*abb45(11)
      abb45(15)=-abb45(60)*abb45(29)
      abb45(16)=-abb45(58)*abb45(20)
      abb45(17)=-spak4e6*abb45(68)
      abb45(20)=-spbe6k5*abb45(71)
      abb45(21)=-abb45(38)*abb45(63)
      R2d45=0.0_ki
      rat2 = rat2 + R2d45
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='45' value='", &
          & R2d45, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd45h3
