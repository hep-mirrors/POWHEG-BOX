module     p12_sbars_hepemg_d27h0l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity0d27h0l1d.f90
   ! generator: buildfortran_d.py
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   integer, private :: iv4
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d27
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd27h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(90) :: acd27
      complex(ki) :: brack
      acd27(1)=dotproduct(k1,qshift)
      acd27(2)=dotproduct(e6,qshift)
      acd27(3)=abb27(35)
      acd27(4)=dotproduct(qshift,spvak1k2)
      acd27(5)=abb27(14)
      acd27(6)=dotproduct(qshift,spvak1k4)
      acd27(7)=abb27(41)
      acd27(8)=dotproduct(qshift,spvak5k2)
      acd27(9)=abb27(78)
      acd27(10)=dotproduct(qshift,spvak1e6)
      acd27(11)=abb27(67)
      acd27(12)=dotproduct(qshift,spvae6k2)
      acd27(13)=abb27(65)
      acd27(14)=dotproduct(qshift,spvae6k4)
      acd27(15)=abb27(50)
      acd27(16)=dotproduct(qshift,spvak5e6)
      acd27(17)=abb27(43)
      acd27(18)=abb27(30)
      acd27(19)=dotproduct(k2,qshift)
      acd27(20)=abb27(34)
      acd27(21)=dotproduct(l3,qshift)
      acd27(22)=abb27(51)
      acd27(23)=dotproduct(k6,qshift)
      acd27(24)=abb27(37)
      acd27(25)=abb27(55)
      acd27(26)=abb27(48)
      acd27(27)=dotproduct(qshift,qshift)
      acd27(28)=abb27(39)
      acd27(29)=abb27(9)
      acd27(30)=abb27(28)
      acd27(31)=abb27(44)
      acd27(32)=dotproduct(qshift,spvak1k6)
      acd27(33)=abb27(79)
      acd27(34)=dotproduct(qshift,spvak6k2)
      acd27(35)=abb27(8)
      acd27(36)=abb27(6)
      acd27(37)=abb27(11)
      acd27(38)=abb27(13)
      acd27(39)=abb27(77)
      acd27(40)=abb27(66)
      acd27(41)=abb27(64)
      acd27(42)=abb27(59)
      acd27(43)=abb27(63)
      acd27(44)=abb27(32)
      acd27(45)=abb27(16)
      acd27(46)=abb27(15)
      acd27(47)=dotproduct(qshift,spvak5k6)
      acd27(48)=abb27(20)
      acd27(49)=dotproduct(qshift,spvak6k4)
      acd27(50)=abb27(17)
      acd27(51)=abb27(7)
      acd27(52)=abb27(24)
      acd27(53)=abb27(76)
      acd27(54)=dotproduct(qshift,spvak5k4)
      acd27(55)=abb27(74)
      acd27(56)=abb27(12)
      acd27(57)=abb27(72)
      acd27(58)=abb27(52)
      acd27(59)=abb27(25)
      acd27(60)=abb27(40)
      acd27(61)=abb27(26)
      acd27(62)=abb27(18)
      acd27(63)=abb27(71)
      acd27(64)=abb27(69)
      acd27(65)=abb27(19)
      acd27(66)=dotproduct(qshift,spvak2k4)
      acd27(67)=abb27(23)
      acd27(68)=dotproduct(qshift,spvak5k1)
      acd27(69)=abb27(38)
      acd27(70)=abb27(10)
      acd27(71)=-acd27(28)*acd27(2)
      acd27(72)=-acd27(37)*acd27(4)
      acd27(73)=-acd27(38)*acd27(6)
      acd27(74)=-acd27(39)*acd27(8)
      acd27(75)=-acd27(40)*acd27(10)
      acd27(76)=-acd27(41)*acd27(12)
      acd27(77)=-acd27(42)*acd27(14)
      acd27(78)=-acd27(43)*acd27(16)
      acd27(71)=acd27(44)+acd27(78)+acd27(77)+acd27(76)+acd27(75)+acd27(74)+acd&
      &27(73)+acd27(72)+acd27(71)
      acd27(71)=acd27(27)*acd27(71)
      acd27(72)=-acd27(1)+acd27(19)
      acd27(72)=acd27(3)*acd27(72)
      acd27(73)=acd27(29)*acd27(4)
      acd27(74)=acd27(30)*acd27(6)
      acd27(75)=acd27(31)*acd27(8)
      acd27(76)=acd27(33)*acd27(32)
      acd27(77)=acd27(35)*acd27(34)
      acd27(72)=acd27(72)-acd27(36)+acd27(77)+acd27(76)+acd27(75)+acd27(74)+acd&
      &27(73)
      acd27(72)=acd27(2)*acd27(72)
      acd27(73)=-acd27(7)*acd27(6)
      acd27(74)=-acd27(9)*acd27(8)
      acd27(75)=acd27(15)*acd27(14)
      acd27(76)=acd27(17)*acd27(16)
      acd27(73)=acd27(76)+acd27(75)+acd27(74)+acd27(73)
      acd27(74)=acd27(19)+acd27(1)
      acd27(73)=acd27(74)*acd27(73)
      acd27(75)=acd27(45)*acd27(14)
      acd27(76)=acd27(46)*acd27(16)
      acd27(77)=acd27(48)*acd27(47)
      acd27(78)=acd27(50)*acd27(49)
      acd27(75)=-acd27(51)+acd27(78)+acd27(77)+acd27(76)+acd27(75)
      acd27(75)=acd27(4)*acd27(75)
      acd27(76)=acd27(55)*acd27(10)
      acd27(77)=acd27(57)*acd27(12)
      acd27(78)=acd27(61)*acd27(32)
      acd27(79)=acd27(62)*acd27(34)
      acd27(76)=-acd27(65)+acd27(79)+acd27(78)+acd27(77)+acd27(76)
      acd27(76)=acd27(54)*acd27(76)
      acd27(77)=acd27(5)*acd27(4)
      acd27(78)=-acd27(11)*acd27(10)
      acd27(79)=acd27(13)*acd27(12)
      acd27(77)=acd27(79)+acd27(77)+acd27(78)
      acd27(74)=acd27(74)-acd27(23)
      acd27(74)=acd27(74)*acd27(77)
      acd27(77)=acd27(24)*acd27(6)
      acd27(78)=-acd27(25)*acd27(8)
      acd27(77)=-acd27(26)+acd27(78)+acd27(77)
      acd27(77)=acd27(23)*acd27(77)
      acd27(78)=-acd27(18)*acd27(1)
      acd27(79)=-acd27(20)*acd27(19)
      acd27(80)=-acd27(22)*acd27(21)
      acd27(81)=-acd27(52)*acd27(6)
      acd27(82)=-acd27(53)*acd27(8)
      acd27(83)=-acd27(56)*acd27(10)
      acd27(84)=-acd27(58)*acd27(12)
      acd27(85)=-acd27(59)*acd27(14)
      acd27(86)=-acd27(60)*acd27(16)
      acd27(87)=-acd27(63)*acd27(47)
      acd27(88)=-acd27(64)*acd27(49)
      acd27(89)=-acd27(67)*acd27(66)
      acd27(90)=-acd27(69)*acd27(68)
      brack=acd27(70)+acd27(71)+acd27(72)+acd27(73)+acd27(74)+acd27(75)+acd27(7&
      &6)+acd27(77)+acd27(78)+acd27(79)+acd27(80)+acd27(81)+acd27(82)+acd27(83)&
      &+acd27(84)+acd27(85)+acd27(86)+acd27(87)+acd27(88)+acd27(89)+acd27(90)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd27h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(111) :: acd27
      complex(ki) :: brack
      acd27(1)=k1(iv1)
      acd27(2)=dotproduct(e6,qshift)
      acd27(3)=abb27(35)
      acd27(4)=dotproduct(qshift,spvak1k2)
      acd27(5)=abb27(14)
      acd27(6)=dotproduct(qshift,spvak1k4)
      acd27(7)=abb27(41)
      acd27(8)=dotproduct(qshift,spvak5k2)
      acd27(9)=abb27(78)
      acd27(10)=dotproduct(qshift,spvak1e6)
      acd27(11)=abb27(67)
      acd27(12)=dotproduct(qshift,spvae6k2)
      acd27(13)=abb27(65)
      acd27(14)=dotproduct(qshift,spvae6k4)
      acd27(15)=abb27(50)
      acd27(16)=dotproduct(qshift,spvak5e6)
      acd27(17)=abb27(43)
      acd27(18)=abb27(30)
      acd27(19)=k2(iv1)
      acd27(20)=abb27(34)
      acd27(21)=l3(iv1)
      acd27(22)=abb27(51)
      acd27(23)=k6(iv1)
      acd27(24)=abb27(37)
      acd27(25)=abb27(55)
      acd27(26)=abb27(48)
      acd27(27)=e6(iv1)
      acd27(28)=dotproduct(k1,qshift)
      acd27(29)=dotproduct(k2,qshift)
      acd27(30)=dotproduct(qshift,qshift)
      acd27(31)=abb27(39)
      acd27(32)=abb27(9)
      acd27(33)=abb27(28)
      acd27(34)=abb27(44)
      acd27(35)=dotproduct(qshift,spvak1k6)
      acd27(36)=abb27(79)
      acd27(37)=dotproduct(qshift,spvak6k2)
      acd27(38)=abb27(8)
      acd27(39)=abb27(6)
      acd27(40)=qshift(iv1)
      acd27(41)=abb27(11)
      acd27(42)=abb27(13)
      acd27(43)=abb27(77)
      acd27(44)=abb27(66)
      acd27(45)=abb27(64)
      acd27(46)=abb27(59)
      acd27(47)=abb27(63)
      acd27(48)=abb27(32)
      acd27(49)=spvak1k2(iv1)
      acd27(50)=dotproduct(k6,qshift)
      acd27(51)=abb27(16)
      acd27(52)=abb27(15)
      acd27(53)=dotproduct(qshift,spvak5k6)
      acd27(54)=abb27(20)
      acd27(55)=dotproduct(qshift,spvak6k4)
      acd27(56)=abb27(17)
      acd27(57)=abb27(7)
      acd27(58)=spvak1k4(iv1)
      acd27(59)=abb27(24)
      acd27(60)=spvak5k2(iv1)
      acd27(61)=abb27(76)
      acd27(62)=spvak1e6(iv1)
      acd27(63)=dotproduct(qshift,spvak5k4)
      acd27(64)=abb27(74)
      acd27(65)=abb27(12)
      acd27(66)=spvae6k2(iv1)
      acd27(67)=abb27(72)
      acd27(68)=abb27(52)
      acd27(69)=spvae6k4(iv1)
      acd27(70)=abb27(25)
      acd27(71)=spvak5e6(iv1)
      acd27(72)=abb27(40)
      acd27(73)=spvak1k6(iv1)
      acd27(74)=abb27(26)
      acd27(75)=spvak6k2(iv1)
      acd27(76)=abb27(18)
      acd27(77)=spvak5k6(iv1)
      acd27(78)=abb27(71)
      acd27(79)=spvak6k4(iv1)
      acd27(80)=abb27(69)
      acd27(81)=spvak5k4(iv1)
      acd27(82)=abb27(19)
      acd27(83)=spvak2k4(iv1)
      acd27(84)=abb27(23)
      acd27(85)=spvak5k1(iv1)
      acd27(86)=abb27(38)
      acd27(87)=acd27(16)*acd27(47)
      acd27(88)=acd27(14)*acd27(46)
      acd27(89)=acd27(12)*acd27(45)
      acd27(90)=acd27(10)*acd27(44)
      acd27(91)=acd27(8)*acd27(43)
      acd27(92)=acd27(6)*acd27(42)
      acd27(93)=acd27(2)*acd27(31)
      acd27(87)=acd27(93)+acd27(92)+acd27(91)+acd27(90)+acd27(89)+acd27(88)-acd&
      &27(48)+acd27(87)
      acd27(88)=2.0_ki*acd27(40)
      acd27(87)=acd27(87)*acd27(88)
      acd27(89)=-acd27(19)+acd27(23)
      acd27(89)=acd27(5)*acd27(89)
      acd27(90)=-acd27(56)*acd27(79)
      acd27(91)=-acd27(54)*acd27(77)
      acd27(92)=-acd27(71)*acd27(52)
      acd27(93)=-acd27(69)*acd27(51)
      acd27(88)=acd27(41)*acd27(88)
      acd27(94)=-acd27(27)*acd27(32)
      acd27(88)=acd27(94)+acd27(88)+acd27(93)+acd27(92)+acd27(90)+acd27(91)+acd&
      &27(89)
      acd27(88)=acd27(4)*acd27(88)
      acd27(89)=acd27(28)+acd27(29)
      acd27(90)=acd27(50)-acd27(89)
      acd27(90)=acd27(5)*acd27(90)
      acd27(91)=-acd27(56)*acd27(55)
      acd27(92)=-acd27(54)*acd27(53)
      acd27(93)=-acd27(16)*acd27(52)
      acd27(94)=-acd27(14)*acd27(51)
      acd27(95)=acd27(30)*acd27(41)
      acd27(96)=-acd27(2)*acd27(32)
      acd27(90)=acd27(96)+acd27(95)+acd27(94)+acd27(93)+acd27(92)+acd27(57)+acd&
      &27(91)+acd27(90)
      acd27(90)=acd27(49)*acd27(90)
      acd27(91)=acd27(71)*acd27(17)
      acd27(92)=acd27(69)*acd27(15)
      acd27(93)=acd27(60)*acd27(9)
      acd27(94)=acd27(58)*acd27(7)
      acd27(95)=acd27(13)*acd27(66)
      acd27(96)=acd27(11)*acd27(62)
      acd27(91)=acd27(91)+acd27(92)-acd27(93)-acd27(94)+acd27(95)-acd27(96)
      acd27(89)=-acd27(91)*acd27(89)
      acd27(91)=acd27(71)*acd27(47)
      acd27(92)=acd27(69)*acd27(46)
      acd27(93)=acd27(66)*acd27(45)
      acd27(94)=acd27(62)*acd27(44)
      acd27(95)=acd27(60)*acd27(43)
      acd27(96)=acd27(58)*acd27(42)
      acd27(91)=acd27(96)+acd27(95)+acd27(94)+acd27(93)+acd27(91)+acd27(92)
      acd27(91)=acd27(30)*acd27(91)
      acd27(92)=acd27(28)-acd27(29)
      acd27(92)=acd27(3)*acd27(92)
      acd27(93)=-acd27(37)*acd27(38)
      acd27(94)=-acd27(35)*acd27(36)
      acd27(95)=-acd27(8)*acd27(34)
      acd27(96)=-acd27(6)*acd27(33)
      acd27(97)=acd27(30)*acd27(31)
      acd27(92)=acd27(97)+acd27(96)+acd27(95)+acd27(94)+acd27(39)+acd27(93)+acd&
      &27(92)
      acd27(92)=acd27(27)*acd27(92)
      acd27(93)=acd27(16)*acd27(17)
      acd27(94)=acd27(14)*acd27(15)
      acd27(95)=acd27(8)*acd27(9)
      acd27(96)=acd27(6)*acd27(7)
      acd27(97)=acd27(13)*acd27(12)
      acd27(98)=acd27(11)*acd27(10)
      acd27(93)=acd27(93)+acd27(94)-acd27(95)-acd27(96)+acd27(97)-acd27(98)
      acd27(94)=acd27(2)*acd27(3)
      acd27(95)=-acd27(94)+acd27(20)-acd27(93)
      acd27(95)=acd27(19)*acd27(95)
      acd27(96)=-acd27(4)*acd27(5)
      acd27(93)=acd27(96)+acd27(94)+acd27(18)-acd27(93)
      acd27(93)=acd27(1)*acd27(93)
      acd27(94)=-acd27(37)*acd27(76)
      acd27(96)=-acd27(35)*acd27(74)
      acd27(97)=-acd27(12)*acd27(67)
      acd27(98)=-acd27(10)*acd27(64)
      acd27(94)=acd27(98)+acd27(97)+acd27(96)+acd27(82)+acd27(94)
      acd27(94)=acd27(81)*acd27(94)
      acd27(96)=-acd27(38)*acd27(75)
      acd27(97)=-acd27(36)*acd27(73)
      acd27(98)=-acd27(60)*acd27(34)
      acd27(99)=-acd27(58)*acd27(33)
      acd27(96)=acd27(99)+acd27(98)+acd27(96)+acd27(97)
      acd27(96)=acd27(2)*acd27(96)
      acd27(97)=-acd27(75)*acd27(76)
      acd27(98)=-acd27(73)*acd27(74)
      acd27(97)=acd27(97)+acd27(98)
      acd27(97)=acd27(63)*acd27(97)
      acd27(98)=acd27(8)*acd27(25)
      acd27(99)=-acd27(6)*acd27(24)
      acd27(98)=acd27(99)+acd27(26)+acd27(98)
      acd27(98)=acd27(23)*acd27(98)
      acd27(99)=acd27(66)*acd27(50)
      acd27(100)=acd27(23)*acd27(12)
      acd27(99)=acd27(99)+acd27(100)
      acd27(99)=acd27(13)*acd27(99)
      acd27(100)=-acd27(62)*acd27(50)
      acd27(101)=-acd27(23)*acd27(10)
      acd27(100)=acd27(100)+acd27(101)
      acd27(100)=acd27(11)*acd27(100)
      acd27(101)=acd27(85)*acd27(86)
      acd27(102)=acd27(83)*acd27(84)
      acd27(103)=acd27(21)*acd27(22)
      acd27(104)=acd27(79)*acd27(80)
      acd27(105)=acd27(77)*acd27(78)
      acd27(106)=acd27(71)*acd27(72)
      acd27(107)=acd27(69)*acd27(70)
      acd27(108)=-acd27(63)*acd27(67)
      acd27(108)=acd27(68)+acd27(108)
      acd27(108)=acd27(66)*acd27(108)
      acd27(109)=-acd27(63)*acd27(64)
      acd27(109)=acd27(65)+acd27(109)
      acd27(109)=acd27(62)*acd27(109)
      acd27(110)=acd27(50)*acd27(25)
      acd27(110)=acd27(61)+acd27(110)
      acd27(110)=acd27(60)*acd27(110)
      acd27(111)=-acd27(50)*acd27(24)
      acd27(111)=acd27(59)+acd27(111)
      acd27(111)=acd27(58)*acd27(111)
      brack=acd27(87)+acd27(88)+acd27(89)+acd27(90)+acd27(91)+acd27(92)+acd27(9&
      &3)+acd27(94)+acd27(95)+acd27(96)+acd27(97)+acd27(98)+acd27(99)+acd27(100&
      &)+acd27(101)+acd27(102)+acd27(103)+acd27(104)+acd27(105)+acd27(106)+acd2&
      &7(107)+acd27(108)+acd27(109)+acd27(110)+acd27(111)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd27h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(93) :: acd27
      complex(ki) :: brack
      acd27(1)=d(iv1,iv2)
      acd27(2)=dotproduct(e6,qshift)
      acd27(3)=abb27(39)
      acd27(4)=dotproduct(qshift,spvak1k2)
      acd27(5)=abb27(11)
      acd27(6)=dotproduct(qshift,spvak1k4)
      acd27(7)=abb27(13)
      acd27(8)=dotproduct(qshift,spvak5k2)
      acd27(9)=abb27(77)
      acd27(10)=dotproduct(qshift,spvak1e6)
      acd27(11)=abb27(66)
      acd27(12)=dotproduct(qshift,spvae6k2)
      acd27(13)=abb27(64)
      acd27(14)=dotproduct(qshift,spvae6k4)
      acd27(15)=abb27(59)
      acd27(16)=dotproduct(qshift,spvak5e6)
      acd27(17)=abb27(63)
      acd27(18)=abb27(32)
      acd27(19)=k1(iv1)
      acd27(20)=e6(iv2)
      acd27(21)=abb27(35)
      acd27(22)=spvak1k2(iv2)
      acd27(23)=abb27(14)
      acd27(24)=spvak1k4(iv2)
      acd27(25)=abb27(41)
      acd27(26)=spvak5k2(iv2)
      acd27(27)=abb27(78)
      acd27(28)=spvak1e6(iv2)
      acd27(29)=abb27(67)
      acd27(30)=spvae6k2(iv2)
      acd27(31)=abb27(65)
      acd27(32)=spvae6k4(iv2)
      acd27(33)=abb27(50)
      acd27(34)=spvak5e6(iv2)
      acd27(35)=abb27(43)
      acd27(36)=k1(iv2)
      acd27(37)=e6(iv1)
      acd27(38)=spvak1k2(iv1)
      acd27(39)=spvak1k4(iv1)
      acd27(40)=spvak5k2(iv1)
      acd27(41)=spvak1e6(iv1)
      acd27(42)=spvae6k2(iv1)
      acd27(43)=spvae6k4(iv1)
      acd27(44)=spvak5e6(iv1)
      acd27(45)=k2(iv1)
      acd27(46)=k2(iv2)
      acd27(47)=k6(iv1)
      acd27(48)=abb27(37)
      acd27(49)=abb27(55)
      acd27(50)=k6(iv2)
      acd27(51)=qshift(iv2)
      acd27(52)=abb27(9)
      acd27(53)=abb27(28)
      acd27(54)=abb27(44)
      acd27(55)=spvak1k6(iv2)
      acd27(56)=abb27(79)
      acd27(57)=spvak6k2(iv2)
      acd27(58)=abb27(8)
      acd27(59)=qshift(iv1)
      acd27(60)=spvak1k6(iv1)
      acd27(61)=spvak6k2(iv1)
      acd27(62)=abb27(16)
      acd27(63)=abb27(15)
      acd27(64)=spvak5k6(iv2)
      acd27(65)=abb27(20)
      acd27(66)=spvak6k4(iv2)
      acd27(67)=abb27(17)
      acd27(68)=spvak5k6(iv1)
      acd27(69)=spvak6k4(iv1)
      acd27(70)=spvak5k4(iv2)
      acd27(71)=abb27(74)
      acd27(72)=spvak5k4(iv1)
      acd27(73)=abb27(72)
      acd27(74)=abb27(26)
      acd27(75)=abb27(18)
      acd27(76)=-acd27(17)*acd27(16)
      acd27(77)=-acd27(15)*acd27(14)
      acd27(78)=-acd27(13)*acd27(12)
      acd27(79)=-acd27(11)*acd27(10)
      acd27(80)=-acd27(9)*acd27(8)
      acd27(81)=-acd27(7)*acd27(6)
      acd27(82)=-acd27(5)*acd27(4)
      acd27(83)=-acd27(3)*acd27(2)
      acd27(76)=acd27(83)+acd27(82)+acd27(81)+acd27(80)+acd27(79)+acd27(78)+acd&
      &27(77)+acd27(18)+acd27(76)
      acd27(76)=acd27(1)*acd27(76)
      acd27(77)=acd27(36)+acd27(46)-acd27(50)
      acd27(77)=acd27(23)*acd27(77)
      acd27(78)=acd27(67)*acd27(66)
      acd27(79)=acd27(65)*acd27(64)
      acd27(80)=acd27(34)*acd27(63)
      acd27(81)=acd27(32)*acd27(62)
      acd27(82)=2.0_ki*acd27(51)
      acd27(83)=-acd27(5)*acd27(82)
      acd27(84)=acd27(20)*acd27(52)
      acd27(77)=acd27(84)+acd27(83)+acd27(81)+acd27(80)+acd27(78)+acd27(79)+acd&
      &27(77)
      acd27(77)=acd27(38)*acd27(77)
      acd27(78)=acd27(19)+acd27(45)-acd27(47)
      acd27(78)=acd27(23)*acd27(78)
      acd27(79)=acd27(67)*acd27(69)
      acd27(80)=acd27(65)*acd27(68)
      acd27(81)=acd27(44)*acd27(63)
      acd27(83)=acd27(43)*acd27(62)
      acd27(84)=2.0_ki*acd27(59)
      acd27(85)=-acd27(5)*acd27(84)
      acd27(86)=acd27(37)*acd27(52)
      acd27(78)=acd27(86)+acd27(85)+acd27(83)+acd27(81)+acd27(79)+acd27(80)+acd&
      &27(78)
      acd27(78)=acd27(22)*acd27(78)
      acd27(79)=-acd27(34)*acd27(17)
      acd27(80)=-acd27(32)*acd27(15)
      acd27(81)=-acd27(30)*acd27(13)
      acd27(83)=-acd27(28)*acd27(11)
      acd27(85)=-acd27(26)*acd27(9)
      acd27(86)=-acd27(24)*acd27(7)
      acd27(79)=acd27(86)+acd27(85)+acd27(83)+acd27(81)+acd27(79)+acd27(80)
      acd27(79)=acd27(79)*acd27(84)
      acd27(80)=-acd27(44)*acd27(17)
      acd27(81)=-acd27(43)*acd27(15)
      acd27(83)=-acd27(42)*acd27(13)
      acd27(85)=-acd27(41)*acd27(11)
      acd27(86)=-acd27(40)*acd27(9)
      acd27(87)=-acd27(39)*acd27(7)
      acd27(80)=acd27(87)+acd27(86)+acd27(85)+acd27(83)+acd27(80)+acd27(81)
      acd27(80)=acd27(80)*acd27(82)
      acd27(81)=acd27(35)*acd27(44)
      acd27(83)=acd27(33)*acd27(43)
      acd27(85)=acd27(40)*acd27(27)
      acd27(86)=acd27(39)*acd27(25)
      acd27(87)=acd27(31)*acd27(42)
      acd27(88)=acd27(29)*acd27(41)
      acd27(81)=-acd27(85)-acd27(86)+acd27(87)-acd27(88)+acd27(81)+acd27(83)
      acd27(83)=acd27(46)*acd27(81)
      acd27(85)=acd27(34)*acd27(35)
      acd27(86)=acd27(32)*acd27(33)
      acd27(87)=acd27(26)*acd27(27)
      acd27(88)=acd27(24)*acd27(25)
      acd27(89)=acd27(31)*acd27(30)
      acd27(90)=acd27(29)*acd27(28)
      acd27(85)=-acd27(87)-acd27(88)+acd27(89)-acd27(90)+acd27(85)+acd27(86)
      acd27(86)=acd27(45)*acd27(85)
      acd27(87)=acd27(57)*acd27(58)
      acd27(88)=acd27(55)*acd27(56)
      acd27(89)=acd27(26)*acd27(54)
      acd27(90)=acd27(24)*acd27(53)
      acd27(82)=-acd27(3)*acd27(82)
      acd27(91)=acd27(46)*acd27(21)
      acd27(82)=acd27(91)+acd27(82)+acd27(90)+acd27(89)+acd27(87)+acd27(88)
      acd27(82)=acd27(37)*acd27(82)
      acd27(87)=-acd27(37)*acd27(21)
      acd27(81)=acd27(87)+acd27(81)
      acd27(81)=acd27(36)*acd27(81)
      acd27(87)=acd27(58)*acd27(61)
      acd27(88)=acd27(56)*acd27(60)
      acd27(89)=acd27(40)*acd27(54)
      acd27(90)=acd27(39)*acd27(53)
      acd27(84)=-acd27(3)*acd27(84)
      acd27(91)=acd27(45)*acd27(21)
      acd27(84)=acd27(91)+acd27(84)+acd27(90)+acd27(89)+acd27(87)+acd27(88)
      acd27(84)=acd27(20)*acd27(84)
      acd27(87)=-acd27(20)*acd27(21)
      acd27(85)=acd27(87)+acd27(85)
      acd27(85)=acd27(19)*acd27(85)
      acd27(87)=acd27(57)*acd27(75)
      acd27(88)=acd27(55)*acd27(74)
      acd27(89)=acd27(30)*acd27(73)
      acd27(90)=acd27(28)*acd27(71)
      acd27(87)=acd27(90)+acd27(89)+acd27(87)+acd27(88)
      acd27(87)=acd27(72)*acd27(87)
      acd27(88)=acd27(61)*acd27(75)
      acd27(89)=acd27(60)*acd27(74)
      acd27(90)=acd27(42)*acd27(73)
      acd27(91)=acd27(41)*acd27(71)
      acd27(88)=acd27(91)+acd27(90)+acd27(88)+acd27(89)
      acd27(88)=acd27(70)*acd27(88)
      acd27(89)=-acd27(40)*acd27(49)
      acd27(90)=acd27(39)*acd27(48)
      acd27(89)=acd27(90)+acd27(89)
      acd27(89)=acd27(50)*acd27(89)
      acd27(90)=-acd27(26)*acd27(49)
      acd27(91)=acd27(24)*acd27(48)
      acd27(90)=acd27(91)+acd27(90)
      acd27(90)=acd27(47)*acd27(90)
      acd27(91)=-acd27(42)*acd27(50)
      acd27(92)=-acd27(30)*acd27(47)
      acd27(91)=acd27(91)+acd27(92)
      acd27(91)=acd27(31)*acd27(91)
      acd27(92)=acd27(41)*acd27(50)
      acd27(93)=acd27(28)*acd27(47)
      acd27(92)=acd27(92)+acd27(93)
      acd27(92)=acd27(29)*acd27(92)
      brack=2.0_ki*acd27(76)+acd27(77)+acd27(78)+acd27(79)+acd27(80)+acd27(81)+&
      &acd27(82)+acd27(83)+acd27(84)+acd27(85)+acd27(86)+acd27(87)+acd27(88)+ac&
      &d27(89)+acd27(90)+acd27(91)+acd27(92)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd27h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(45) :: acd27
      complex(ki) :: brack
      acd27(1)=d(iv1,iv2)
      acd27(2)=e6(iv3)
      acd27(3)=abb27(39)
      acd27(4)=spvak1k2(iv3)
      acd27(5)=abb27(11)
      acd27(6)=spvak1k4(iv3)
      acd27(7)=abb27(13)
      acd27(8)=spvak5k2(iv3)
      acd27(9)=abb27(77)
      acd27(10)=spvak1e6(iv3)
      acd27(11)=abb27(66)
      acd27(12)=spvae6k2(iv3)
      acd27(13)=abb27(64)
      acd27(14)=spvae6k4(iv3)
      acd27(15)=abb27(59)
      acd27(16)=spvak5e6(iv3)
      acd27(17)=abb27(63)
      acd27(18)=d(iv1,iv3)
      acd27(19)=e6(iv2)
      acd27(20)=spvak1k2(iv2)
      acd27(21)=spvak1k4(iv2)
      acd27(22)=spvak5k2(iv2)
      acd27(23)=spvak1e6(iv2)
      acd27(24)=spvae6k2(iv2)
      acd27(25)=spvae6k4(iv2)
      acd27(26)=spvak5e6(iv2)
      acd27(27)=d(iv2,iv3)
      acd27(28)=e6(iv1)
      acd27(29)=spvak1k2(iv1)
      acd27(30)=spvak1k4(iv1)
      acd27(31)=spvak5k2(iv1)
      acd27(32)=spvak1e6(iv1)
      acd27(33)=spvae6k2(iv1)
      acd27(34)=spvae6k4(iv1)
      acd27(35)=spvak5e6(iv1)
      acd27(36)=acd27(2)*acd27(3)
      acd27(37)=acd27(4)*acd27(5)
      acd27(38)=acd27(6)*acd27(7)
      acd27(39)=acd27(8)*acd27(9)
      acd27(40)=acd27(10)*acd27(11)
      acd27(41)=acd27(12)*acd27(13)
      acd27(42)=acd27(14)*acd27(15)
      acd27(43)=acd27(16)*acd27(17)
      acd27(36)=acd27(43)+acd27(42)+acd27(41)+acd27(40)+acd27(39)+acd27(38)+acd&
      &27(36)+acd27(37)
      acd27(36)=acd27(1)*acd27(36)
      acd27(37)=acd27(19)*acd27(3)
      acd27(38)=acd27(20)*acd27(5)
      acd27(39)=acd27(21)*acd27(7)
      acd27(40)=acd27(22)*acd27(9)
      acd27(41)=acd27(23)*acd27(11)
      acd27(42)=acd27(24)*acd27(13)
      acd27(43)=acd27(25)*acd27(15)
      acd27(44)=acd27(26)*acd27(17)
      acd27(37)=acd27(44)+acd27(43)+acd27(42)+acd27(41)+acd27(40)+acd27(39)+acd&
      &27(38)+acd27(37)
      acd27(37)=acd27(18)*acd27(37)
      acd27(38)=acd27(28)*acd27(3)
      acd27(39)=acd27(29)*acd27(5)
      acd27(40)=acd27(30)*acd27(7)
      acd27(41)=acd27(31)*acd27(9)
      acd27(42)=acd27(32)*acd27(11)
      acd27(43)=acd27(33)*acd27(13)
      acd27(44)=acd27(34)*acd27(15)
      acd27(45)=acd27(35)*acd27(17)
      acd27(38)=acd27(45)+acd27(44)+acd27(43)+acd27(42)+acd27(41)+acd27(40)+acd&
      &27(39)+acd27(38)
      acd27(38)=acd27(27)*acd27(38)
      acd27(36)=acd27(38)+acd27(37)+acd27(36)
      brack=2.0_ki*acd27(36)
   end function brack_4
!---#] function brack_4:
!---#[ function brack_5:
   pure function brack_5(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd27h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd27
      complex(ki) :: brack
      brack=0.0_ki
   end function brack_5
!---#] function brack_5:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3,i4) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd27h0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
      integer, intent(in), optional :: i4
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/ (0.0_ki,0.0_ki),(0.0_ki,0.&
      &0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = k3
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
          iv1=i1
          deg=1
      else
          iv1=1
      end if
      if(present(i2)) then
          iv2=i2
          deg=2
      else
          iv2=1
      end if
      if(present(i3)) then
          iv3=i3
          deg=3
      else
          iv3=1
      end if
      if(present(i4)) then
          iv4=i4
          deg=4
      else
          iv4=1
      end if
      t1 = 0
      if(deg.eq.0) then
         numerator = cond(epspow.eq.t1,brack_1,Q,mu2)
         return
      end if
      if(deg.eq.1) then
         numerator = cond(epspow.eq.t1,brack_2,Q,mu2)
         return
      end if
      if(deg.eq.2) then
         numerator = cond(epspow.eq.t1,brack_3,Q,mu2)
         return
      end if
      if(deg.eq.3) then
         numerator = cond(epspow.eq.t1,brack_4,Q,mu2)
         return
      end if
      if(deg.eq.4) then
         numerator = cond(epspow.eq.t1,brack_5,Q,mu2)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d27:
   subroutine     reconstruct_d27(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group5
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group5), intent(out) :: coeffs
      ! rank 4 case :
      !---[# reconstruct coeffs%coeffs_27:
      coeffs%coeffs_27%c0 = derivative(czip)
      coeffs%coeffs_27%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_27%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_27%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_27%c1(1,4) = derivative(czip,1,1,1,1)/ 24.0_ki
      coeffs%coeffs_27%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_27%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_27%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_27%c1(2,4) = derivative(czip,2,2,2,2)/ 24.0_ki
      coeffs%coeffs_27%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_27%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_27%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_27%c1(3,4) = derivative(czip,3,3,3,3)/ 24.0_ki
      coeffs%coeffs_27%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_27%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_27%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_27%c1(4,4) = derivative(czip,4,4,4,4)/ 24.0_ki
      coeffs%coeffs_27%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_27%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_27%c2(1,3) = -derivative(czip,1,2,2,2)/ 6.0_ki
      coeffs%coeffs_27%c2(1,4) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_27%c2(1,5) = derivative(czip,1,1,2,2)/ 4.0_ki
      coeffs%coeffs_27%c2(1,6) = -derivative(czip,1,1,1,2)/ 6.0_ki
      coeffs%coeffs_27%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_27%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_27%c2(2,3) = -derivative(czip,1,3,3,3)/ 6.0_ki
      coeffs%coeffs_27%c2(2,4) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_27%c2(2,5) = derivative(czip,1,1,3,3)/ 4.0_ki
      coeffs%coeffs_27%c2(2,6) = -derivative(czip,1,1,1,3)/ 6.0_ki
      coeffs%coeffs_27%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_27%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_27%c2(3,3) = -derivative(czip,1,4,4,4)/ 6.0_ki
      coeffs%coeffs_27%c2(3,4) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_27%c2(3,5) = derivative(czip,1,1,4,4)/ 4.0_ki
      coeffs%coeffs_27%c2(3,6) = -derivative(czip,1,1,1,4)/ 6.0_ki
      coeffs%coeffs_27%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_27%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_27%c2(4,3) = derivative(czip,2,3,3,3)/ 6.0_ki
      coeffs%coeffs_27%c2(4,4) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_27%c2(4,5) = derivative(czip,2,2,3,3)/ 4.0_ki
      coeffs%coeffs_27%c2(4,6) = derivative(czip,2,2,2,3)/ 6.0_ki
      coeffs%coeffs_27%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_27%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_27%c2(5,3) = derivative(czip,2,4,4,4)/ 6.0_ki
      coeffs%coeffs_27%c2(5,4) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_27%c2(5,5) = derivative(czip,2,2,4,4)/ 4.0_ki
      coeffs%coeffs_27%c2(5,6) = derivative(czip,2,2,2,4)/ 6.0_ki
      coeffs%coeffs_27%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_27%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_27%c2(6,3) = derivative(czip,3,4,4,4)/ 6.0_ki
      coeffs%coeffs_27%c2(6,4) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_27%c2(6,5) = derivative(czip,3,3,4,4)/ 4.0_ki
      coeffs%coeffs_27%c2(6,6) = derivative(czip,3,3,3,4)/ 6.0_ki
      coeffs%coeffs_27%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_27%c3(1,2) = -derivative(czip,1,2,3,3)/ 2.0_ki
      coeffs%coeffs_27%c3(1,3) = -derivative(czip,1,2,2,3)/ 2.0_ki
      coeffs%coeffs_27%c3(1,4) = derivative(czip,1,1,2,3)/ 2.0_ki
      coeffs%coeffs_27%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_27%c3(2,2) = -derivative(czip,1,2,4,4)/ 2.0_ki
      coeffs%coeffs_27%c3(2,3) = -derivative(czip,1,2,2,4)/ 2.0_ki
      coeffs%coeffs_27%c3(2,4) = derivative(czip,1,1,2,4)/ 2.0_ki
      coeffs%coeffs_27%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_27%c3(3,2) = -derivative(czip,1,3,4,4)/ 2.0_ki
      coeffs%coeffs_27%c3(3,3) = -derivative(czip,1,3,3,4)/ 2.0_ki
      coeffs%coeffs_27%c3(3,4) = derivative(czip,1,1,3,4)/ 2.0_ki
      coeffs%coeffs_27%c3(4,1) = -derivative(czip,2,3,4)
      coeffs%coeffs_27%c3(4,2) = derivative(czip,2,3,4,4)/ 2.0_ki
      coeffs%coeffs_27%c3(4,3) = derivative(czip,2,3,3,4)/ 2.0_ki
      coeffs%coeffs_27%c3(4,4) = derivative(czip,2,2,3,4)/ 2.0_ki
      coeffs%coeffs_27%c4(1,1) = -derivative(czip,1,2,3,4)
      !---#] reconstruct coeffs%coeffs_27:
   end subroutine reconstruct_d27
!---#] subroutine reconstruct_d27:
end module     p12_sbars_hepemg_d27h0l1d
