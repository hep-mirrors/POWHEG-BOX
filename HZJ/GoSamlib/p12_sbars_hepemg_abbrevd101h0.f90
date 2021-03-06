module     p12_sbars_hepemg_abbrevd101h0
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(69), public :: abb101
   complex(ki), public :: R2d101
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
      abb101(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb101(2)=sqrt(mT**2)
      abb101(3)=es12**(-1)
      abb101(4)=spak2l3**(-1)
      abb101(5)=spbl3k2**(-1)
      abb101(6)=spak1e6*spbe6k4
      abb101(7)=spak5k6*spbk6k2
      abb101(8)=abb101(6)*abb101(7)
      abb101(9)=spbe6k4*spak1k5
      abb101(10)=abb101(9)*spbk2k1
      abb101(11)=abb101(10)*spak1e6
      abb101(8)=abb101(8)-abb101(11)
      abb101(11)=spbe6k2*spak5e6
      abb101(12)=spbk6k4*spak1k6
      abb101(13)=abb101(11)*abb101(12)
      abb101(14)=spak5e6*spbk4k2
      abb101(15)=abb101(14)*spak1k2
      abb101(16)=abb101(15)*spbe6k2
      abb101(13)=abb101(13)+abb101(16)
      abb101(16)=spae6k6*spbk6k2
      abb101(17)=abb101(9)*abb101(16)
      abb101(18)=spbk6e6*spak1k6
      abb101(19)=abb101(14)*abb101(18)
      abb101(20)=abb101(19)-abb101(17)-abb101(13)+abb101(8)
      abb101(21)=abb101(3)*abb101(2)**3
      abb101(22)=i_*TR*c1*gHT*gel*abb101(1)
      abb101(23)=abb101(22)*gTl
      abb101(24)=abb101(21)*abb101(23)
      abb101(25)=abb101(22)*gTr
      abb101(21)=abb101(21)*abb101(25)
      abb101(26)=abb101(24)-abb101(21)
      abb101(20)=abb101(26)*abb101(20)
      abb101(27)=spbk2k1*spak1k5
      abb101(28)=abb101(27)-abb101(7)
      abb101(22)=abb101(22)*abb101(3)*abb101(2)
      abb101(29)=abb101(22)*gTr
      abb101(22)=abb101(22)*gTl
      abb101(30)=abb101(29)-abb101(22)
      abb101(31)=abb101(30)*abb101(18)
      abb101(32)=-abb101(31)*abb101(28)
      abb101(33)=spbe6k2*spak1k5
      abb101(34)=-abb101(26)*abb101(33)
      abb101(32)=abb101(34)+abb101(32)
      abb101(34)=spal3e6*spbk4l3
      abb101(32)=abb101(32)*abb101(34)
      abb101(35)=spak1e6*spbk4k2
      abb101(36)=abb101(35)*abb101(26)
      abb101(37)=abb101(30)*abb101(16)
      abb101(38)=abb101(12)*abb101(37)
      abb101(36)=abb101(38)-abb101(36)
      abb101(38)=abb101(4)*abb101(5)*mH**2
      abb101(39)=abb101(38)*spbe6k2
      abb101(40)=-abb101(39)*abb101(36)
      abb101(41)=abb101(30)*spbk4k2
      abb101(42)=abb101(41)*abb101(38)
      abb101(43)=abb101(16)*spak1k2
      abb101(44)=-abb101(43)*abb101(42)*spbe6k2
      abb101(40)=abb101(44)+abb101(40)
      abb101(40)=spak2k5*abb101(40)
      abb101(43)=-abb101(41)*abb101(43)
      abb101(36)=abb101(43)-abb101(36)
      abb101(43)=spbe6l3*spal3k5
      abb101(36)=abb101(36)*abb101(43)
      abb101(44)=abb101(42)*spak2e6
      abb101(45)=-abb101(18)*abb101(28)*abb101(44)
      abb101(20)=abb101(45)+abb101(32)+abb101(36)+abb101(40)+abb101(20)
      abb101(20)=2.0_ki*abb101(20)
      abb101(32)=2.0_ki*abb101(29)
      abb101(36)=spbk4k2*abb101(32)
      abb101(36)=abb101(36)-abb101(42)
      abb101(40)=2.0_ki*spak5k6
      abb101(36)=spak1k2*abb101(40)*abb101(36)
      abb101(45)=abb101(42)*spak2k5
      abb101(46)=-spak1k6*abb101(45)
      abb101(36)=abb101(46)+abb101(36)
      abb101(36)=spbk6k2*abb101(36)
      abb101(46)=abb101(12)*abb101(30)
      abb101(47)=spak1k2*abb101(41)
      abb101(47)=abb101(46)+abb101(47)
      abb101(47)=spbl3k2*abb101(47)
      abb101(48)=-spbk6l3*spak1k6*abb101(41)
      abb101(47)=abb101(48)+2.0_ki*abb101(47)
      abb101(47)=spal3k5*abb101(47)
      abb101(48)=abb101(29)+abb101(22)
      abb101(49)=4.0_ki*abb101(48)
      abb101(50)=abb101(7)*abb101(49)
      abb101(51)=4.0_ki*abb101(22)
      abb101(51)=-abb101(27)*abb101(51)
      abb101(50)=abb101(51)+abb101(50)
      abb101(50)=abb101(12)*abb101(50)
      abb101(51)=-es345+es61
      abb101(42)=abb101(42)*abb101(51)
      abb101(51)=spal3k6*spbk4l3*spbk6k2*abb101(30)
      abb101(42)=abb101(51)+abb101(42)
      abb101(42)=spak1k5*abb101(42)
      abb101(51)=abb101(24)+abb101(21)
      abb101(52)=spak1k5*spbk4k2
      abb101(53)=4.0_ki*abb101(52)
      abb101(54)=abb101(51)*abb101(53)
      abb101(23)=abb101(25)-abb101(23)
      abb101(23)=abb101(38)*abb101(52)*abb101(2)*abb101(23)
      abb101(7)=abb101(7)*abb101(30)
      abb101(25)=abb101(30)*abb101(27)
      abb101(25)=-abb101(7)+abb101(25)
      abb101(25)=spak1l3*spbk4l3*abb101(25)
      abb101(23)=2.0_ki*abb101(25)+abb101(54)+3.0_ki*abb101(23)+abb101(42)+abb1&
      &01(50)+abb101(47)+abb101(36)
      abb101(23)=2.0_ki*abb101(23)
      abb101(25)=abb101(19)+abb101(17)
      abb101(25)=abb101(48)*abb101(25)
      abb101(36)=-2.0_ki*abb101(8)
      abb101(36)=abb101(22)*abb101(36)
      abb101(13)=-abb101(32)*abb101(13)
      abb101(13)=abb101(13)+abb101(25)+abb101(36)
      abb101(25)=spbe6k2*abb101(45)
      abb101(32)=abb101(43)*abb101(41)
      abb101(25)=abb101(25)+abb101(32)
      abb101(25)=spak1e6*abb101(25)
      abb101(32)=abb101(30)*spbe6k2
      abb101(36)=abb101(34)*spak1k5
      abb101(42)=-abb101(32)*abb101(36)
      abb101(13)=abb101(42)+2.0_ki*abb101(13)+abb101(25)
      abb101(13)=2.0_ki*abb101(13)
      abb101(42)=abb101(52)*abb101(48)
      abb101(45)=-16.0_ki*abb101(42)
      abb101(47)=abb101(37)*abb101(9)
      abb101(50)=abb101(31)*abb101(14)
      abb101(47)=abb101(47)-abb101(50)
      abb101(50)=-2.0_ki*abb101(47)
      abb101(48)=2.0_ki*abb101(48)
      abb101(17)=-abb101(17)*abb101(48)
      abb101(52)=-abb101(47)*abb101(38)
      abb101(54)=3.0_ki*abb101(29)
      abb101(55)=abb101(54)+abb101(22)
      abb101(56)=abb101(55)*abb101(18)
      abb101(57)=-abb101(14)*abb101(56)
      abb101(17)=abb101(52)+abb101(57)+abb101(17)
      abb101(17)=2.0_ki*abb101(17)
      abb101(52)=3.0_ki*abb101(22)
      abb101(57)=abb101(52)+5.0_ki*abb101(29)
      abb101(58)=abb101(57)*abb101(53)
      abb101(19)=-abb101(19)*abb101(48)
      abb101(52)=abb101(52)+abb101(29)
      abb101(59)=abb101(52)*abb101(16)
      abb101(60)=-abb101(9)*abb101(59)
      abb101(19)=abb101(19)+abb101(60)
      abb101(19)=2.0_ki*abb101(19)
      abb101(54)=abb101(54)+5.0_ki*abb101(22)
      abb101(53)=abb101(54)*abb101(53)
      abb101(8)=abb101(30)*abb101(8)
      abb101(60)=-spak5e6*abb101(12)
      abb101(36)=-abb101(36)-abb101(15)+abb101(60)
      abb101(36)=abb101(32)*abb101(36)
      abb101(8)=abb101(36)+abb101(8)+abb101(47)+abb101(25)
      abb101(8)=2.0_ki*abb101(8)
      abb101(25)=-8.0_ki*abb101(42)
      abb101(36)=spak1k2*spbk4k2
      abb101(12)=-abb101(36)-abb101(12)
      abb101(12)=abb101(59)*abb101(12)
      abb101(42)=-abb101(21)-3.0_ki*abb101(24)
      abb101(42)=abb101(42)*abb101(35)
      abb101(12)=abb101(42)+abb101(12)
      abb101(35)=abb101(35)*abb101(52)
      abb101(42)=2.0_ki*abb101(35)
      abb101(28)=abb101(56)*abb101(28)
      abb101(47)=-3.0_ki*abb101(21)-abb101(24)
      abb101(47)=abb101(47)*abb101(33)
      abb101(28)=abb101(47)+abb101(28)
      abb101(33)=abb101(33)*abb101(55)
      abb101(47)=2.0_ki*abb101(33)
      abb101(60)=abb101(26)*abb101(9)
      abb101(61)=abb101(30)*abb101(9)
      abb101(26)=-abb101(26)*abb101(14)
      abb101(62)=-abb101(30)*abb101(14)
      abb101(56)=abb101(56)*spak5e6
      abb101(63)=-spbk6k2*abb101(56)
      abb101(64)=2.0_ki*spbk6k4
      abb101(64)=-spak5e6*abb101(31)*abb101(64)
      abb101(65)=abb101(30)*spbk6e6
      abb101(15)=-abb101(15)*abb101(65)
      abb101(15)=abb101(64)+abb101(15)
      abb101(64)=abb101(30)*spbk6k4
      abb101(66)=2.0_ki*spak1k5*abb101(64)
      abb101(59)=abb101(59)*spbe6k4
      abb101(67)=-spak1k6*abb101(59)
      abb101(39)=spbe6k2+abb101(39)
      abb101(39)=spak1k2*abb101(39)
      abb101(68)=spak1l3*spbe6l3
      abb101(39)=abb101(68)+abb101(39)
      abb101(39)=abb101(37)*abb101(39)
      abb101(68)=spbk2k1*spak1e6
      abb101(69)=-spbl3k2*spal3e6
      abb101(68)=abb101(69)+abb101(68)
      abb101(68)=abb101(31)*abb101(68)
      abb101(39)=abb101(68)+abb101(39)
      abb101(65)=-spak1e6*abb101(65)
      abb101(68)=4.0_ki*abb101(6)
      abb101(24)=abb101(24)*abb101(68)
      abb101(59)=spak1k2*abb101(59)
      abb101(24)=abb101(24)+abb101(59)
      abb101(59)=-abb101(29)-7.0_ki*abb101(22)
      abb101(36)=abb101(59)*abb101(36)
      abb101(36)=abb101(46)+abb101(36)
      abb101(36)=2.0_ki*abb101(36)
      abb101(6)=-2.0_ki*abb101(52)*abb101(6)
      abb101(46)=-abb101(22)*abb101(68)
      abb101(59)=spbe6k4*abb101(37)*abb101(40)
      abb101(10)=-abb101(10)*abb101(30)*spae6k6
      abb101(10)=abb101(59)+abb101(10)
      abb101(40)=-abb101(41)*abb101(40)
      abb101(32)=abb101(32)*spae6k6
      abb101(41)=4.0_ki*abb101(11)
      abb101(21)=abb101(21)*abb101(41)
      abb101(56)=-spbk2k1*abb101(56)
      abb101(21)=abb101(21)+abb101(56)
      abb101(22)=7.0_ki*abb101(29)+abb101(22)
      abb101(22)=abb101(22)*abb101(27)
      abb101(7)=-abb101(7)+abb101(22)
      abb101(7)=2.0_ki*abb101(7)
      abb101(11)=-2.0_ki*abb101(55)*abb101(11)
      abb101(22)=-abb101(29)*abb101(41)
      abb101(27)=spbe6k4*abb101(51)
      abb101(18)=spbk4k1*abb101(18)*abb101(48)
      abb101(18)=abb101(18)-4.0_ki*abb101(27)
      abb101(18)=spak5e6*abb101(18)
      abb101(27)=abb101(30)*abb101(34)
      abb101(27)=abb101(44)+abb101(27)
      abb101(27)=spak5k6*abb101(27)
      abb101(14)=spak2k6*abb101(55)*abb101(14)
      abb101(14)=abb101(14)+abb101(27)
      abb101(14)=spbk6e6*abb101(14)
      abb101(16)=spbe6k4*abb101(16)*abb101(48)
      abb101(27)=-spbk6k4*abb101(38)*abb101(32)
      abb101(16)=abb101(16)+abb101(27)
      abb101(16)=spak2k5*abb101(16)
      abb101(27)=-spae6k6*abb101(43)*abb101(64)
      abb101(29)=abb101(52)*spae6k6
      abb101(9)=spbk6k1*abb101(9)*abb101(29)
      abb101(9)=abb101(9)+abb101(27)+abb101(16)+abb101(14)+abb101(18)
      abb101(14)=spbk6k4*spak5k6*abb101(49)
      abb101(16)=-spak2k5*spbk4k2*abb101(57)
      abb101(18)=-spbk4k1*spak1k5*abb101(54)
      abb101(14)=abb101(18)+abb101(14)+abb101(16)
      abb101(14)=2.0_ki*abb101(14)
      abb101(16)=spbe6k4*spak5e6*abb101(49)
      abb101(18)=-spbk6k4*abb101(29)
      abb101(27)=abb101(55)*spbk6e6
      abb101(30)=-spak5k6*abb101(27)
      abb101(27)=-spak5e6*abb101(27)
      abb101(29)=-spbe6k4*abb101(29)
      R2d101=0.0_ki
      rat2 = rat2 + R2d101
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='101' value='", &
          & R2d101, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd101h0
