module     p12_sbars_hepemg_abbrevd55h2
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(69), public :: abb55
   complex(ki), public :: R2d55
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
      abb55(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb55(2)=sqrt(mT**2)
      abb55(3)=es12**(-1)
      abb55(4)=spak2l3**(-1)
      abb55(5)=spbl3k2**(-1)
      abb55(6)=spak1e6*spbe6k5
      abb55(7)=spak4k6*spbk6k2
      abb55(8)=abb55(6)*abb55(7)
      abb55(9)=spbe6k5*spak1k4
      abb55(10)=abb55(9)*spbk2k1
      abb55(11)=abb55(10)*spak1e6
      abb55(8)=abb55(8)-abb55(11)
      abb55(11)=spbe6k2*spak4e6
      abb55(12)=spbk6k5*spak1k6
      abb55(13)=abb55(11)*abb55(12)
      abb55(14)=spak4e6*spbk5k2
      abb55(15)=abb55(14)*spak1k2
      abb55(16)=abb55(15)*spbe6k2
      abb55(13)=abb55(13)+abb55(16)
      abb55(16)=spbk6e6*spak1k6
      abb55(17)=abb55(14)*abb55(16)
      abb55(18)=spae6k6*spbk6k2
      abb55(19)=abb55(9)*abb55(18)
      abb55(20)=-abb55(19)+abb55(17)-abb55(13)+abb55(8)
      abb55(21)=abb55(3)*abb55(2)**3
      abb55(22)=i_*TR*c1*gHT*ger*abb55(1)
      abb55(23)=abb55(22)*gTl
      abb55(24)=abb55(21)*abb55(23)
      abb55(25)=abb55(22)*gTr
      abb55(21)=abb55(21)*abb55(25)
      abb55(26)=abb55(24)-abb55(21)
      abb55(20)=abb55(26)*abb55(20)
      abb55(27)=spbk2k1*spak1k4
      abb55(28)=abb55(27)-abb55(7)
      abb55(22)=abb55(22)*abb55(3)*abb55(2)
      abb55(29)=abb55(22)*gTr
      abb55(22)=abb55(22)*gTl
      abb55(30)=abb55(29)-abb55(22)
      abb55(31)=abb55(30)*abb55(16)
      abb55(32)=-abb55(31)*abb55(28)
      abb55(33)=spbe6k2*spak1k4
      abb55(34)=-abb55(26)*abb55(33)
      abb55(32)=abb55(34)+abb55(32)
      abb55(34)=spal3e6*spbk5l3
      abb55(32)=abb55(32)*abb55(34)
      abb55(35)=spak1e6*spbk5k2
      abb55(36)=abb55(35)*abb55(26)
      abb55(37)=abb55(30)*abb55(18)
      abb55(38)=abb55(12)*abb55(37)
      abb55(36)=abb55(38)-abb55(36)
      abb55(38)=abb55(4)*abb55(5)*mH**2
      abb55(39)=abb55(38)*spbe6k2
      abb55(40)=-abb55(39)*abb55(36)
      abb55(41)=abb55(30)*spbk5k2
      abb55(42)=abb55(41)*abb55(38)
      abb55(43)=abb55(18)*spak1k2
      abb55(44)=-abb55(43)*abb55(42)*spbe6k2
      abb55(40)=abb55(44)+abb55(40)
      abb55(40)=spak2k4*abb55(40)
      abb55(43)=-abb55(41)*abb55(43)
      abb55(36)=abb55(43)-abb55(36)
      abb55(43)=spbe6l3*spal3k4
      abb55(36)=abb55(36)*abb55(43)
      abb55(44)=abb55(42)*spak2e6
      abb55(45)=-abb55(16)*abb55(28)*abb55(44)
      abb55(20)=abb55(45)+abb55(32)+abb55(36)+abb55(40)+abb55(20)
      abb55(20)=2.0_ki*abb55(20)
      abb55(32)=2.0_ki*abb55(29)
      abb55(36)=spbk5k2*abb55(32)
      abb55(36)=abb55(36)-abb55(42)
      abb55(40)=2.0_ki*spak4k6
      abb55(36)=spak1k2*abb55(40)*abb55(36)
      abb55(45)=abb55(42)*spak2k4
      abb55(46)=-spak1k6*abb55(45)
      abb55(36)=abb55(46)+abb55(36)
      abb55(36)=spbk6k2*abb55(36)
      abb55(46)=abb55(12)*abb55(30)
      abb55(47)=spak1k2*abb55(41)
      abb55(47)=abb55(46)+abb55(47)
      abb55(47)=spbl3k2*abb55(47)
      abb55(48)=-spbk6l3*spak1k6*abb55(41)
      abb55(47)=abb55(48)+2.0_ki*abb55(47)
      abb55(47)=spal3k4*abb55(47)
      abb55(48)=abb55(29)+abb55(22)
      abb55(49)=4.0_ki*abb55(48)
      abb55(50)=abb55(7)*abb55(49)
      abb55(51)=4.0_ki*abb55(22)
      abb55(51)=-abb55(27)*abb55(51)
      abb55(50)=abb55(51)+abb55(50)
      abb55(50)=abb55(12)*abb55(50)
      abb55(51)=-es345+es61
      abb55(42)=abb55(42)*abb55(51)
      abb55(51)=spal3k6*spbk5l3*spbk6k2*abb55(30)
      abb55(42)=abb55(51)+abb55(42)
      abb55(42)=spak1k4*abb55(42)
      abb55(51)=abb55(24)+abb55(21)
      abb55(52)=spak1k4*spbk5k2
      abb55(53)=4.0_ki*abb55(52)
      abb55(54)=abb55(51)*abb55(53)
      abb55(23)=abb55(25)-abb55(23)
      abb55(23)=abb55(38)*abb55(52)*abb55(2)*abb55(23)
      abb55(7)=abb55(7)*abb55(30)
      abb55(25)=abb55(30)*abb55(27)
      abb55(25)=-abb55(7)+abb55(25)
      abb55(25)=spak1l3*spbk5l3*abb55(25)
      abb55(23)=2.0_ki*abb55(25)+abb55(54)+3.0_ki*abb55(23)+abb55(42)+abb55(50)&
      &+abb55(47)+abb55(36)
      abb55(23)=2.0_ki*abb55(23)
      abb55(25)=abb55(19)+abb55(17)
      abb55(25)=abb55(48)*abb55(25)
      abb55(36)=-2.0_ki*abb55(8)
      abb55(36)=abb55(22)*abb55(36)
      abb55(13)=-abb55(32)*abb55(13)
      abb55(13)=abb55(13)+abb55(25)+abb55(36)
      abb55(25)=spbe6k2*abb55(45)
      abb55(32)=abb55(43)*abb55(41)
      abb55(25)=abb55(25)+abb55(32)
      abb55(25)=spak1e6*abb55(25)
      abb55(32)=abb55(30)*spbe6k2
      abb55(36)=abb55(34)*spak1k4
      abb55(42)=-abb55(32)*abb55(36)
      abb55(13)=abb55(42)+2.0_ki*abb55(13)+abb55(25)
      abb55(13)=2.0_ki*abb55(13)
      abb55(42)=abb55(52)*abb55(48)
      abb55(45)=-16.0_ki*abb55(42)
      abb55(47)=abb55(37)*abb55(9)
      abb55(50)=abb55(31)*abb55(14)
      abb55(47)=abb55(47)-abb55(50)
      abb55(50)=-2.0_ki*abb55(47)
      abb55(48)=2.0_ki*abb55(48)
      abb55(19)=-abb55(19)*abb55(48)
      abb55(52)=-abb55(47)*abb55(38)
      abb55(54)=3.0_ki*abb55(29)
      abb55(55)=abb55(54)+abb55(22)
      abb55(56)=abb55(55)*abb55(16)
      abb55(57)=-abb55(14)*abb55(56)
      abb55(19)=abb55(52)+abb55(19)+abb55(57)
      abb55(19)=2.0_ki*abb55(19)
      abb55(52)=3.0_ki*abb55(22)
      abb55(57)=abb55(52)+5.0_ki*abb55(29)
      abb55(58)=abb55(57)*abb55(53)
      abb55(17)=-abb55(17)*abb55(48)
      abb55(52)=abb55(52)+abb55(29)
      abb55(59)=abb55(52)*abb55(18)
      abb55(60)=-abb55(9)*abb55(59)
      abb55(17)=abb55(60)+abb55(17)
      abb55(17)=2.0_ki*abb55(17)
      abb55(54)=abb55(54)+5.0_ki*abb55(22)
      abb55(53)=abb55(54)*abb55(53)
      abb55(8)=abb55(30)*abb55(8)
      abb55(60)=-spak4e6*abb55(12)
      abb55(36)=-abb55(36)-abb55(15)+abb55(60)
      abb55(36)=abb55(32)*abb55(36)
      abb55(8)=abb55(36)+abb55(8)+abb55(47)+abb55(25)
      abb55(8)=2.0_ki*abb55(8)
      abb55(25)=-8.0_ki*abb55(42)
      abb55(28)=abb55(56)*abb55(28)
      abb55(36)=-3.0_ki*abb55(21)-abb55(24)
      abb55(36)=abb55(36)*abb55(33)
      abb55(28)=abb55(36)+abb55(28)
      abb55(33)=abb55(33)*abb55(55)
      abb55(36)=2.0_ki*abb55(33)
      abb55(42)=spak1k2*spbk5k2
      abb55(12)=-abb55(42)-abb55(12)
      abb55(12)=abb55(59)*abb55(12)
      abb55(47)=-abb55(21)-3.0_ki*abb55(24)
      abb55(47)=abb55(47)*abb55(35)
      abb55(12)=abb55(47)+abb55(12)
      abb55(35)=abb55(35)*abb55(52)
      abb55(47)=2.0_ki*abb55(35)
      abb55(60)=abb55(26)*abb55(9)
      abb55(61)=abb55(30)*abb55(9)
      abb55(26)=-abb55(26)*abb55(14)
      abb55(62)=-abb55(30)*abb55(14)
      abb55(56)=abb55(56)*spak4e6
      abb55(63)=-spbk6k2*abb55(56)
      abb55(64)=2.0_ki*spbk6k5
      abb55(64)=-spak4e6*abb55(31)*abb55(64)
      abb55(65)=abb55(30)*spbk6e6
      abb55(15)=-abb55(15)*abb55(65)
      abb55(15)=abb55(64)+abb55(15)
      abb55(64)=abb55(30)*spbk6k5
      abb55(66)=2.0_ki*spak1k4*abb55(64)
      abb55(59)=abb55(59)*spbe6k5
      abb55(67)=-spak1k6*abb55(59)
      abb55(39)=spbe6k2+abb55(39)
      abb55(39)=spak1k2*abb55(39)
      abb55(68)=spak1l3*spbe6l3
      abb55(39)=abb55(68)+abb55(39)
      abb55(39)=abb55(37)*abb55(39)
      abb55(68)=spbk2k1*spak1e6
      abb55(69)=-spbl3k2*spal3e6
      abb55(68)=abb55(69)+abb55(68)
      abb55(68)=abb55(31)*abb55(68)
      abb55(39)=abb55(68)+abb55(39)
      abb55(65)=-spak1e6*abb55(65)
      abb55(68)=4.0_ki*abb55(6)
      abb55(24)=abb55(24)*abb55(68)
      abb55(59)=spak1k2*abb55(59)
      abb55(24)=abb55(24)+abb55(59)
      abb55(59)=-abb55(29)-7.0_ki*abb55(22)
      abb55(42)=abb55(59)*abb55(42)
      abb55(42)=abb55(46)+abb55(42)
      abb55(42)=2.0_ki*abb55(42)
      abb55(6)=-2.0_ki*abb55(52)*abb55(6)
      abb55(46)=-abb55(22)*abb55(68)
      abb55(59)=spbe6k5*abb55(37)*abb55(40)
      abb55(10)=-abb55(10)*abb55(30)*spae6k6
      abb55(10)=abb55(59)+abb55(10)
      abb55(40)=-abb55(41)*abb55(40)
      abb55(32)=abb55(32)*spae6k6
      abb55(41)=4.0_ki*abb55(11)
      abb55(21)=abb55(21)*abb55(41)
      abb55(56)=-spbk2k1*abb55(56)
      abb55(21)=abb55(21)+abb55(56)
      abb55(22)=7.0_ki*abb55(29)+abb55(22)
      abb55(22)=abb55(22)*abb55(27)
      abb55(7)=-abb55(7)+abb55(22)
      abb55(7)=2.0_ki*abb55(7)
      abb55(11)=-2.0_ki*abb55(55)*abb55(11)
      abb55(22)=-abb55(29)*abb55(41)
      abb55(27)=spbe6k5*abb55(51)
      abb55(16)=spbk5k1*abb55(16)*abb55(48)
      abb55(16)=abb55(16)-4.0_ki*abb55(27)
      abb55(16)=spak4e6*abb55(16)
      abb55(27)=abb55(30)*abb55(34)
      abb55(27)=abb55(44)+abb55(27)
      abb55(27)=spak4k6*abb55(27)
      abb55(14)=spak2k6*abb55(55)*abb55(14)
      abb55(14)=abb55(14)+abb55(27)
      abb55(14)=spbk6e6*abb55(14)
      abb55(18)=spbe6k5*abb55(18)*abb55(48)
      abb55(27)=-spbk6k5*abb55(38)*abb55(32)
      abb55(18)=abb55(18)+abb55(27)
      abb55(18)=spak2k4*abb55(18)
      abb55(27)=-spae6k6*abb55(43)*abb55(64)
      abb55(29)=abb55(52)*spae6k6
      abb55(9)=spbk6k1*abb55(9)*abb55(29)
      abb55(9)=abb55(9)+abb55(27)+abb55(18)+abb55(14)+abb55(16)
      abb55(14)=spbk6k5*spak4k6*abb55(49)
      abb55(16)=-spak2k4*spbk5k2*abb55(57)
      abb55(18)=-spbk5k1*spak1k4*abb55(54)
      abb55(14)=abb55(18)+abb55(14)+abb55(16)
      abb55(14)=2.0_ki*abb55(14)
      abb55(16)=spak4e6*spbe6k5*abb55(49)
      abb55(18)=abb55(55)*spbk6e6
      abb55(27)=-spak4k6*abb55(18)
      abb55(30)=-spbk6k5*abb55(29)
      abb55(18)=-spak4e6*abb55(18)
      abb55(29)=-spbe6k5*abb55(29)
      R2d55=0.0_ki
      rat2 = rat2 + R2d55
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='55' value='", &
          & R2d55, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd55h2
