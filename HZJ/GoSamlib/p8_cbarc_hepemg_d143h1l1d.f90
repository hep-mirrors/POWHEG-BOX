module     p8_cbarc_hepemg_d143h1l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity1d143h1l1d.f90
   ! generator: buildfortran_d.py
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d143
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd143h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(82) :: acd143
      complex(ki) :: brack
      acd143(1)=dotproduct(k1,qshift)
      acd143(2)=dotproduct(e6,qshift)
      acd143(3)=abb143(44)
      acd143(4)=dotproduct(qshift,spvae6k1)
      acd143(5)=abb143(33)
      acd143(6)=dotproduct(qshift,spvak2e6)
      acd143(7)=abb143(37)
      acd143(8)=dotproduct(qshift,spvae6k4)
      acd143(9)=abb143(50)
      acd143(10)=dotproduct(qshift,spvak5e6)
      acd143(11)=abb143(47)
      acd143(12)=abb143(19)
      acd143(13)=dotproduct(k2,qshift)
      acd143(14)=abb143(28)
      acd143(15)=dotproduct(k6,qshift)
      acd143(16)=dotproduct(qshift,spvak2k1)
      acd143(17)=abb143(35)
      acd143(18)=dotproduct(qshift,spvak2k4)
      acd143(19)=abb143(21)
      acd143(20)=dotproduct(qshift,spvak5k1)
      acd143(21)=abb143(27)
      acd143(22)=abb143(17)
      acd143(23)=dotproduct(qshift,qshift)
      acd143(24)=abb143(39)
      acd143(25)=abb143(30)
      acd143(26)=abb143(23)
      acd143(27)=abb143(29)
      acd143(28)=dotproduct(qshift,spvak2k6)
      acd143(29)=abb143(62)
      acd143(30)=dotproduct(qshift,spvak6k1)
      acd143(31)=abb143(57)
      acd143(32)=abb143(25)
      acd143(33)=abb143(15)
      acd143(34)=abb143(24)
      acd143(35)=abb143(36)
      acd143(36)=abb143(40)
      acd143(37)=abb143(42)
      acd143(38)=abb143(18)
      acd143(39)=dotproduct(qshift,spvak5k4)
      acd143(40)=abb143(60)
      acd143(41)=abb143(54)
      acd143(42)=abb143(13)
      acd143(43)=abb143(58)
      acd143(44)=abb143(53)
      acd143(45)=abb143(34)
      acd143(46)=abb143(49)
      acd143(47)=abb143(38)
      acd143(48)=abb143(46)
      acd143(49)=dotproduct(qshift,spvak5k6)
      acd143(50)=abb143(41)
      acd143(51)=dotproduct(qshift,spvak6k4)
      acd143(52)=abb143(31)
      acd143(53)=dotproduct(qshift,spvak1e6)
      acd143(54)=abb143(32)
      acd143(55)=dotproduct(qshift,spvae6k2)
      acd143(56)=abb143(43)
      acd143(57)=abb143(26)
      acd143(58)=abb143(20)
      acd143(59)=abb143(16)
      acd143(60)=abb143(59)
      acd143(61)=abb143(52)
      acd143(62)=abb143(48)
      acd143(63)=abb143(51)
      acd143(64)=abb143(55)
      acd143(65)=abb143(22)
      acd143(66)=acd143(17)*acd143(15)
      acd143(67)=acd143(25)*acd143(2)
      acd143(68)=-acd143(35)*acd143(23)
      acd143(69)=acd143(38)*acd143(4)
      acd143(70)=acd143(42)*acd143(6)
      acd143(71)=acd143(45)*acd143(8)
      acd143(72)=acd143(47)*acd143(10)
      acd143(73)=acd143(50)*acd143(49)
      acd143(74)=acd143(52)*acd143(51)
      acd143(75)=acd143(54)*acd143(53)
      acd143(76)=acd143(56)*acd143(55)
      acd143(66)=-acd143(57)+acd143(76)+acd143(75)+acd143(74)+acd143(73)+acd143&
      &(72)+acd143(71)+acd143(70)+acd143(69)+acd143(68)+acd143(67)+acd143(66)
      acd143(66)=acd143(16)*acd143(66)
      acd143(67)=-acd143(24)*acd143(23)
      acd143(68)=acd143(26)*acd143(18)
      acd143(69)=acd143(27)*acd143(20)
      acd143(70)=acd143(29)*acd143(28)
      acd143(71)=acd143(31)*acd143(30)
      acd143(67)=-acd143(32)+acd143(71)+acd143(70)+acd143(69)+acd143(68)+acd143&
      &(67)
      acd143(67)=acd143(2)*acd143(67)
      acd143(68)=-acd143(33)*acd143(8)
      acd143(69)=-acd143(34)*acd143(10)
      acd143(70)=-acd143(36)*acd143(18)
      acd143(71)=-acd143(37)*acd143(20)
      acd143(68)=acd143(71)+acd143(70)+acd143(69)+acd143(68)
      acd143(68)=acd143(23)*acd143(68)
      acd143(69)=acd143(40)*acd143(4)
      acd143(70)=acd143(43)*acd143(6)
      acd143(71)=acd143(60)*acd143(28)
      acd143(72)=acd143(61)*acd143(30)
      acd143(69)=-acd143(64)+acd143(72)+acd143(71)+acd143(70)+acd143(69)
      acd143(69)=acd143(39)*acd143(69)
      acd143(70)=-acd143(3)*acd143(2)
      acd143(71)=acd143(9)*acd143(8)
      acd143(72)=-acd143(11)*acd143(10)
      acd143(70)=acd143(70)+acd143(72)+acd143(71)
      acd143(71)=acd143(13)+acd143(1)
      acd143(70)=acd143(71)*acd143(70)
      acd143(72)=acd143(5)*acd143(4)
      acd143(73)=-acd143(7)*acd143(6)
      acd143(72)=acd143(72)+acd143(73)
      acd143(71)=acd143(71)+acd143(15)-acd143(23)
      acd143(71)=acd143(71)*acd143(72)
      acd143(72)=acd143(19)*acd143(18)
      acd143(73)=acd143(21)*acd143(20)
      acd143(72)=-acd143(22)+acd143(73)+acd143(72)
      acd143(72)=acd143(15)*acd143(72)
      acd143(73)=-acd143(12)*acd143(1)
      acd143(74)=-acd143(14)*acd143(13)
      acd143(75)=-acd143(41)*acd143(4)
      acd143(76)=-acd143(44)*acd143(6)
      acd143(77)=-acd143(46)*acd143(8)
      acd143(78)=-acd143(48)*acd143(10)
      acd143(79)=-acd143(58)*acd143(18)
      acd143(80)=-acd143(59)*acd143(20)
      acd143(81)=-acd143(62)*acd143(49)
      acd143(82)=-acd143(63)*acd143(51)
      brack=acd143(65)+acd143(66)+acd143(67)+acd143(68)+acd143(69)+acd143(70)+a&
      &cd143(71)+acd143(72)+acd143(73)+acd143(74)+acd143(75)+acd143(76)+acd143(&
      &77)+acd143(78)+acd143(79)+acd143(80)+acd143(81)+acd143(82)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd143h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(106) :: acd143
      complex(ki) :: brack
      acd143(1)=k1(iv1)
      acd143(2)=dotproduct(e6,qshift)
      acd143(3)=abb143(44)
      acd143(4)=dotproduct(qshift,spvae6k1)
      acd143(5)=abb143(33)
      acd143(6)=dotproduct(qshift,spvak2e6)
      acd143(7)=abb143(37)
      acd143(8)=dotproduct(qshift,spvae6k4)
      acd143(9)=abb143(50)
      acd143(10)=dotproduct(qshift,spvak5e6)
      acd143(11)=abb143(47)
      acd143(12)=abb143(19)
      acd143(13)=k2(iv1)
      acd143(14)=abb143(28)
      acd143(15)=k6(iv1)
      acd143(16)=dotproduct(qshift,spvak2k1)
      acd143(17)=abb143(35)
      acd143(18)=dotproduct(qshift,spvak2k4)
      acd143(19)=abb143(21)
      acd143(20)=dotproduct(qshift,spvak5k1)
      acd143(21)=abb143(27)
      acd143(22)=abb143(17)
      acd143(23)=e6(iv1)
      acd143(24)=dotproduct(k1,qshift)
      acd143(25)=dotproduct(k2,qshift)
      acd143(26)=dotproduct(qshift,qshift)
      acd143(27)=abb143(39)
      acd143(28)=abb143(30)
      acd143(29)=abb143(23)
      acd143(30)=abb143(29)
      acd143(31)=dotproduct(qshift,spvak2k6)
      acd143(32)=abb143(62)
      acd143(33)=dotproduct(qshift,spvak6k1)
      acd143(34)=abb143(57)
      acd143(35)=abb143(25)
      acd143(36)=qshift(iv1)
      acd143(37)=abb143(15)
      acd143(38)=abb143(24)
      acd143(39)=abb143(36)
      acd143(40)=abb143(40)
      acd143(41)=abb143(42)
      acd143(42)=spvae6k1(iv1)
      acd143(43)=dotproduct(k6,qshift)
      acd143(44)=abb143(18)
      acd143(45)=dotproduct(qshift,spvak5k4)
      acd143(46)=abb143(60)
      acd143(47)=abb143(54)
      acd143(48)=spvak2e6(iv1)
      acd143(49)=abb143(13)
      acd143(50)=abb143(58)
      acd143(51)=abb143(53)
      acd143(52)=spvae6k4(iv1)
      acd143(53)=abb143(34)
      acd143(54)=abb143(49)
      acd143(55)=spvak5e6(iv1)
      acd143(56)=abb143(38)
      acd143(57)=abb143(46)
      acd143(58)=spvak2k1(iv1)
      acd143(59)=dotproduct(qshift,spvak5k6)
      acd143(60)=abb143(41)
      acd143(61)=dotproduct(qshift,spvak6k4)
      acd143(62)=abb143(31)
      acd143(63)=dotproduct(qshift,spvak1e6)
      acd143(64)=abb143(32)
      acd143(65)=dotproduct(qshift,spvae6k2)
      acd143(66)=abb143(43)
      acd143(67)=abb143(26)
      acd143(68)=spvak2k4(iv1)
      acd143(69)=abb143(20)
      acd143(70)=spvak5k1(iv1)
      acd143(71)=abb143(16)
      acd143(72)=spvak2k6(iv1)
      acd143(73)=abb143(59)
      acd143(74)=spvak6k1(iv1)
      acd143(75)=abb143(52)
      acd143(76)=spvak5k6(iv1)
      acd143(77)=abb143(48)
      acd143(78)=spvak6k4(iv1)
      acd143(79)=abb143(51)
      acd143(80)=spvak1e6(iv1)
      acd143(81)=spvae6k2(iv1)
      acd143(82)=spvak5k4(iv1)
      acd143(83)=abb143(55)
      acd143(84)=acd143(66)*acd143(81)
      acd143(85)=acd143(64)*acd143(80)
      acd143(86)=acd143(62)*acd143(78)
      acd143(87)=acd143(60)*acd143(76)
      acd143(88)=acd143(55)*acd143(56)
      acd143(89)=acd143(52)*acd143(53)
      acd143(90)=acd143(15)*acd143(17)
      acd143(91)=acd143(48)*acd143(49)
      acd143(92)=acd143(42)*acd143(44)
      acd143(93)=2.0_ki*acd143(36)
      acd143(94)=-acd143(39)*acd143(93)
      acd143(95)=acd143(23)*acd143(28)
      acd143(84)=acd143(95)+acd143(94)+acd143(92)+acd143(91)+acd143(90)+acd143(&
      &89)+acd143(88)+acd143(87)+acd143(86)+acd143(84)+acd143(85)
      acd143(84)=acd143(16)*acd143(84)
      acd143(85)=acd143(66)*acd143(65)
      acd143(86)=acd143(64)*acd143(63)
      acd143(87)=acd143(62)*acd143(61)
      acd143(88)=acd143(60)*acd143(59)
      acd143(89)=acd143(10)*acd143(56)
      acd143(90)=acd143(8)*acd143(53)
      acd143(91)=acd143(43)*acd143(17)
      acd143(92)=acd143(6)*acd143(49)
      acd143(94)=acd143(4)*acd143(44)
      acd143(95)=-acd143(26)*acd143(39)
      acd143(96)=acd143(2)*acd143(28)
      acd143(85)=acd143(96)+acd143(95)+acd143(94)+acd143(92)+acd143(91)+acd143(&
      &90)+acd143(89)+acd143(88)+acd143(87)+acd143(86)-acd143(67)+acd143(85)
      acd143(85)=acd143(58)*acd143(85)
      acd143(86)=acd143(1)+acd143(13)
      acd143(87)=-acd143(3)*acd143(86)
      acd143(88)=acd143(34)*acd143(74)
      acd143(89)=acd143(32)*acd143(72)
      acd143(90)=acd143(70)*acd143(30)
      acd143(91)=acd143(68)*acd143(29)
      acd143(92)=-acd143(27)*acd143(93)
      acd143(87)=acd143(92)+acd143(91)+acd143(90)+acd143(88)+acd143(89)+acd143(&
      &87)
      acd143(87)=acd143(2)*acd143(87)
      acd143(88)=acd143(24)+acd143(25)
      acd143(89)=-acd143(3)*acd143(88)
      acd143(90)=acd143(33)*acd143(34)
      acd143(91)=acd143(31)*acd143(32)
      acd143(92)=acd143(20)*acd143(30)
      acd143(94)=acd143(18)*acd143(29)
      acd143(95)=-acd143(26)*acd143(27)
      acd143(89)=acd143(95)+acd143(94)+acd143(92)+acd143(91)-acd143(35)+acd143(&
      &90)+acd143(89)
      acd143(89)=acd143(23)*acd143(89)
      acd143(90)=acd143(33)*acd143(75)
      acd143(91)=acd143(31)*acd143(73)
      acd143(92)=acd143(6)*acd143(50)
      acd143(94)=acd143(4)*acd143(46)
      acd143(90)=acd143(94)+acd143(92)+acd143(91)-acd143(83)+acd143(90)
      acd143(90)=acd143(82)*acd143(90)
      acd143(91)=-acd143(20)*acd143(41)
      acd143(92)=-acd143(18)*acd143(40)
      acd143(94)=-acd143(10)*acd143(38)
      acd143(95)=-acd143(8)*acd143(37)
      acd143(91)=acd143(95)+acd143(94)+acd143(91)+acd143(92)
      acd143(91)=acd143(91)*acd143(93)
      acd143(92)=-acd143(70)*acd143(41)
      acd143(94)=-acd143(68)*acd143(40)
      acd143(95)=-acd143(55)*acd143(38)
      acd143(96)=-acd143(52)*acd143(37)
      acd143(92)=acd143(96)+acd143(95)+acd143(92)+acd143(94)
      acd143(92)=acd143(26)*acd143(92)
      acd143(94)=acd143(55)*acd143(11)
      acd143(95)=acd143(52)*acd143(9)
      acd143(94)=acd143(94)-acd143(95)
      acd143(94)=-acd143(94)*acd143(88)
      acd143(95)=acd143(74)*acd143(75)
      acd143(96)=acd143(72)*acd143(73)
      acd143(95)=acd143(95)+acd143(96)
      acd143(95)=acd143(45)*acd143(95)
      acd143(96)=acd143(70)*acd143(21)
      acd143(97)=acd143(68)*acd143(19)
      acd143(96)=acd143(96)+acd143(97)
      acd143(96)=acd143(43)*acd143(96)
      acd143(97)=acd143(20)*acd143(21)
      acd143(98)=acd143(18)*acd143(19)
      acd143(97)=acd143(98)-acd143(22)+acd143(97)
      acd143(97)=acd143(15)*acd143(97)
      acd143(98)=acd143(10)*acd143(11)
      acd143(99)=acd143(8)*acd143(9)
      acd143(98)=acd143(98)-acd143(99)
      acd143(99)=-acd143(14)-acd143(98)
      acd143(99)=acd143(13)*acd143(99)
      acd143(98)=-acd143(12)-acd143(98)
      acd143(98)=acd143(1)*acd143(98)
      acd143(86)=-acd143(93)+acd143(15)+acd143(86)
      acd143(93)=-acd143(6)*acd143(86)
      acd143(88)=-acd143(26)+acd143(88)+acd143(43)
      acd143(100)=-acd143(48)*acd143(88)
      acd143(93)=acd143(93)+acd143(100)
      acd143(93)=acd143(7)*acd143(93)
      acd143(86)=acd143(4)*acd143(86)
      acd143(88)=acd143(42)*acd143(88)
      acd143(86)=acd143(86)+acd143(88)
      acd143(86)=acd143(5)*acd143(86)
      acd143(88)=-acd143(78)*acd143(79)
      acd143(100)=-acd143(76)*acd143(77)
      acd143(101)=-acd143(70)*acd143(71)
      acd143(102)=-acd143(68)*acd143(69)
      acd143(103)=-acd143(55)*acd143(57)
      acd143(104)=-acd143(52)*acd143(54)
      acd143(105)=acd143(45)*acd143(50)
      acd143(105)=-acd143(51)+acd143(105)
      acd143(105)=acd143(48)*acd143(105)
      acd143(106)=acd143(45)*acd143(46)
      acd143(106)=-acd143(47)+acd143(106)
      acd143(106)=acd143(42)*acd143(106)
      brack=acd143(84)+acd143(85)+acd143(86)+acd143(87)+acd143(88)+acd143(89)+a&
      &cd143(90)+acd143(91)+acd143(92)+acd143(93)+acd143(94)+acd143(95)+acd143(&
      &96)+acd143(97)+acd143(98)+acd143(99)+acd143(100)+acd143(101)+acd143(102)&
      &+acd143(103)+acd143(104)+acd143(105)+acd143(106)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd143h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(95) :: acd143
      complex(ki) :: brack
      acd143(1)=d(iv1,iv2)
      acd143(2)=dotproduct(e6,qshift)
      acd143(3)=abb143(39)
      acd143(4)=dotproduct(qshift,spvak2k1)
      acd143(5)=abb143(36)
      acd143(6)=dotproduct(qshift,spvak2k4)
      acd143(7)=abb143(40)
      acd143(8)=dotproduct(qshift,spvak5k1)
      acd143(9)=abb143(42)
      acd143(10)=dotproduct(qshift,spvae6k1)
      acd143(11)=abb143(33)
      acd143(12)=dotproduct(qshift,spvak2e6)
      acd143(13)=abb143(37)
      acd143(14)=dotproduct(qshift,spvae6k4)
      acd143(15)=abb143(15)
      acd143(16)=dotproduct(qshift,spvak5e6)
      acd143(17)=abb143(24)
      acd143(18)=k1(iv1)
      acd143(19)=e6(iv2)
      acd143(20)=abb143(44)
      acd143(21)=spvae6k1(iv2)
      acd143(22)=spvak2e6(iv2)
      acd143(23)=spvae6k4(iv2)
      acd143(24)=abb143(50)
      acd143(25)=spvak5e6(iv2)
      acd143(26)=abb143(47)
      acd143(27)=k1(iv2)
      acd143(28)=e6(iv1)
      acd143(29)=spvae6k1(iv1)
      acd143(30)=spvak2e6(iv1)
      acd143(31)=spvae6k4(iv1)
      acd143(32)=spvak5e6(iv1)
      acd143(33)=k2(iv1)
      acd143(34)=k2(iv2)
      acd143(35)=k6(iv1)
      acd143(36)=spvak2k1(iv2)
      acd143(37)=abb143(35)
      acd143(38)=spvak2k4(iv2)
      acd143(39)=abb143(21)
      acd143(40)=spvak5k1(iv2)
      acd143(41)=abb143(27)
      acd143(42)=k6(iv2)
      acd143(43)=spvak2k1(iv1)
      acd143(44)=spvak2k4(iv1)
      acd143(45)=spvak5k1(iv1)
      acd143(46)=qshift(iv2)
      acd143(47)=abb143(30)
      acd143(48)=abb143(23)
      acd143(49)=abb143(29)
      acd143(50)=spvak2k6(iv2)
      acd143(51)=abb143(62)
      acd143(52)=spvak6k1(iv2)
      acd143(53)=abb143(57)
      acd143(54)=qshift(iv1)
      acd143(55)=spvak2k6(iv1)
      acd143(56)=spvak6k1(iv1)
      acd143(57)=abb143(18)
      acd143(58)=abb143(13)
      acd143(59)=abb143(34)
      acd143(60)=abb143(38)
      acd143(61)=spvak5k6(iv2)
      acd143(62)=abb143(41)
      acd143(63)=spvak6k4(iv2)
      acd143(64)=abb143(31)
      acd143(65)=spvak1e6(iv2)
      acd143(66)=abb143(32)
      acd143(67)=spvae6k2(iv2)
      acd143(68)=abb143(43)
      acd143(69)=spvak5k6(iv1)
      acd143(70)=spvak6k4(iv1)
      acd143(71)=spvak1e6(iv1)
      acd143(72)=spvae6k2(iv1)
      acd143(73)=spvak5k4(iv2)
      acd143(74)=abb143(60)
      acd143(75)=spvak5k4(iv1)
      acd143(76)=abb143(58)
      acd143(77)=abb143(59)
      acd143(78)=abb143(52)
      acd143(79)=acd143(68)*acd143(67)
      acd143(80)=acd143(66)*acd143(65)
      acd143(81)=acd143(64)*acd143(63)
      acd143(82)=acd143(62)*acd143(61)
      acd143(83)=acd143(25)*acd143(60)
      acd143(84)=acd143(23)*acd143(59)
      acd143(85)=acd143(42)*acd143(37)
      acd143(86)=acd143(22)*acd143(58)
      acd143(87)=acd143(21)*acd143(57)
      acd143(88)=2.0_ki*acd143(46)
      acd143(89)=-acd143(5)*acd143(88)
      acd143(90)=acd143(19)*acd143(47)
      acd143(79)=acd143(90)+acd143(89)+acd143(87)+acd143(86)+acd143(85)+acd143(&
      &84)+acd143(83)+acd143(82)+acd143(81)+acd143(79)+acd143(80)
      acd143(79)=acd143(43)*acd143(79)
      acd143(80)=acd143(68)*acd143(72)
      acd143(81)=acd143(66)*acd143(71)
      acd143(82)=acd143(64)*acd143(70)
      acd143(83)=acd143(62)*acd143(69)
      acd143(84)=acd143(32)*acd143(60)
      acd143(85)=acd143(31)*acd143(59)
      acd143(86)=acd143(35)*acd143(37)
      acd143(87)=acd143(30)*acd143(58)
      acd143(89)=acd143(29)*acd143(57)
      acd143(90)=2.0_ki*acd143(54)
      acd143(91)=-acd143(5)*acd143(90)
      acd143(92)=acd143(28)*acd143(47)
      acd143(80)=acd143(92)+acd143(91)+acd143(89)+acd143(87)+acd143(86)+acd143(&
      &85)+acd143(84)+acd143(83)+acd143(82)+acd143(80)+acd143(81)
      acd143(80)=acd143(36)*acd143(80)
      acd143(81)=acd143(27)+acd143(34)
      acd143(82)=-acd143(20)*acd143(81)
      acd143(83)=acd143(52)*acd143(53)
      acd143(84)=acd143(50)*acd143(51)
      acd143(85)=acd143(40)*acd143(49)
      acd143(86)=acd143(38)*acd143(48)
      acd143(87)=-acd143(3)*acd143(88)
      acd143(82)=acd143(87)+acd143(86)+acd143(85)+acd143(83)+acd143(84)+acd143(&
      &82)
      acd143(82)=acd143(28)*acd143(82)
      acd143(83)=acd143(18)+acd143(33)
      acd143(84)=-acd143(20)*acd143(83)
      acd143(85)=acd143(53)*acd143(56)
      acd143(86)=acd143(51)*acd143(55)
      acd143(87)=acd143(45)*acd143(49)
      acd143(89)=acd143(44)*acd143(48)
      acd143(91)=-acd143(3)*acd143(90)
      acd143(84)=acd143(91)+acd143(89)+acd143(87)+acd143(85)+acd143(86)+acd143(&
      &84)
      acd143(84)=acd143(19)*acd143(84)
      acd143(85)=-acd143(17)*acd143(16)
      acd143(86)=-acd143(15)*acd143(14)
      acd143(87)=-acd143(9)*acd143(8)
      acd143(89)=-acd143(7)*acd143(6)
      acd143(91)=-acd143(5)*acd143(4)
      acd143(92)=-acd143(3)*acd143(2)
      acd143(85)=acd143(92)+acd143(91)+acd143(89)+acd143(87)+acd143(85)+acd143(&
      &86)
      acd143(86)=2.0_ki*acd143(1)
      acd143(85)=acd143(85)*acd143(86)
      acd143(87)=acd143(52)*acd143(78)
      acd143(89)=acd143(50)*acd143(77)
      acd143(91)=acd143(22)*acd143(76)
      acd143(92)=acd143(21)*acd143(74)
      acd143(87)=acd143(92)+acd143(91)+acd143(87)+acd143(89)
      acd143(87)=acd143(75)*acd143(87)
      acd143(89)=acd143(56)*acd143(78)
      acd143(91)=acd143(55)*acd143(77)
      acd143(92)=acd143(30)*acd143(76)
      acd143(93)=acd143(29)*acd143(74)
      acd143(89)=acd143(93)+acd143(92)+acd143(89)+acd143(91)
      acd143(89)=acd143(73)*acd143(89)
      acd143(91)=-acd143(9)*acd143(40)
      acd143(92)=-acd143(7)*acd143(38)
      acd143(93)=-acd143(25)*acd143(17)
      acd143(94)=-acd143(23)*acd143(15)
      acd143(91)=acd143(94)+acd143(93)+acd143(91)+acd143(92)
      acd143(91)=acd143(91)*acd143(90)
      acd143(92)=-acd143(9)*acd143(45)
      acd143(93)=-acd143(7)*acd143(44)
      acd143(94)=-acd143(32)*acd143(17)
      acd143(95)=-acd143(31)*acd143(15)
      acd143(92)=acd143(95)+acd143(94)+acd143(92)+acd143(93)
      acd143(92)=acd143(92)*acd143(88)
      acd143(88)=-acd143(88)+acd143(81)+acd143(42)
      acd143(93)=-acd143(30)*acd143(88)
      acd143(90)=-acd143(90)+acd143(83)+acd143(35)
      acd143(94)=-acd143(22)*acd143(90)
      acd143(95)=acd143(12)*acd143(86)
      acd143(93)=acd143(95)+acd143(94)+acd143(93)
      acd143(93)=acd143(13)*acd143(93)
      acd143(88)=acd143(29)*acd143(88)
      acd143(90)=acd143(21)*acd143(90)
      acd143(86)=-acd143(10)*acd143(86)
      acd143(86)=acd143(86)+acd143(90)+acd143(88)
      acd143(86)=acd143(11)*acd143(86)
      acd143(88)=acd143(26)*acd143(32)
      acd143(90)=acd143(24)*acd143(31)
      acd143(88)=acd143(88)-acd143(90)
      acd143(81)=-acd143(88)*acd143(81)
      acd143(88)=acd143(25)*acd143(26)
      acd143(90)=acd143(23)*acd143(24)
      acd143(88)=acd143(88)-acd143(90)
      acd143(83)=-acd143(88)*acd143(83)
      acd143(88)=acd143(45)*acd143(41)
      acd143(90)=acd143(44)*acd143(39)
      acd143(88)=acd143(88)+acd143(90)
      acd143(88)=acd143(42)*acd143(88)
      acd143(90)=acd143(40)*acd143(41)
      acd143(94)=acd143(38)*acd143(39)
      acd143(90)=acd143(90)+acd143(94)
      acd143(90)=acd143(35)*acd143(90)
      brack=acd143(79)+acd143(80)+acd143(81)+acd143(82)+acd143(83)+acd143(84)+a&
      &cd143(85)+acd143(86)+acd143(87)+acd143(88)+acd143(89)+acd143(90)+acd143(&
      &91)+acd143(92)+acd143(93)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd143h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(45) :: acd143
      complex(ki) :: brack
      acd143(1)=d(iv1,iv2)
      acd143(2)=e6(iv3)
      acd143(3)=abb143(39)
      acd143(4)=spvak2k1(iv3)
      acd143(5)=abb143(36)
      acd143(6)=spvak2k4(iv3)
      acd143(7)=abb143(40)
      acd143(8)=spvak5k1(iv3)
      acd143(9)=abb143(42)
      acd143(10)=spvae6k1(iv3)
      acd143(11)=abb143(33)
      acd143(12)=spvak2e6(iv3)
      acd143(13)=abb143(37)
      acd143(14)=spvae6k4(iv3)
      acd143(15)=abb143(15)
      acd143(16)=spvak5e6(iv3)
      acd143(17)=abb143(24)
      acd143(18)=d(iv1,iv3)
      acd143(19)=e6(iv2)
      acd143(20)=spvak2k1(iv2)
      acd143(21)=spvak2k4(iv2)
      acd143(22)=spvak5k1(iv2)
      acd143(23)=spvae6k1(iv2)
      acd143(24)=spvak2e6(iv2)
      acd143(25)=spvae6k4(iv2)
      acd143(26)=spvak5e6(iv2)
      acd143(27)=d(iv2,iv3)
      acd143(28)=e6(iv1)
      acd143(29)=spvak2k1(iv1)
      acd143(30)=spvak2k4(iv1)
      acd143(31)=spvak5k1(iv1)
      acd143(32)=spvae6k1(iv1)
      acd143(33)=spvak2e6(iv1)
      acd143(34)=spvae6k4(iv1)
      acd143(35)=spvak5e6(iv1)
      acd143(36)=-acd143(17)*acd143(35)
      acd143(37)=-acd143(15)*acd143(34)
      acd143(38)=acd143(13)*acd143(33)
      acd143(39)=-acd143(11)*acd143(32)
      acd143(40)=-acd143(9)*acd143(31)
      acd143(41)=-acd143(7)*acd143(30)
      acd143(42)=-acd143(5)*acd143(29)
      acd143(43)=-acd143(3)*acd143(28)
      acd143(36)=acd143(43)+acd143(42)+acd143(41)+acd143(40)+acd143(39)+acd143(&
      &38)+acd143(36)+acd143(37)
      acd143(36)=acd143(27)*acd143(36)
      acd143(37)=-acd143(17)*acd143(26)
      acd143(38)=-acd143(15)*acd143(25)
      acd143(39)=acd143(13)*acd143(24)
      acd143(40)=-acd143(11)*acd143(23)
      acd143(41)=-acd143(9)*acd143(22)
      acd143(42)=-acd143(7)*acd143(21)
      acd143(43)=-acd143(5)*acd143(20)
      acd143(44)=-acd143(3)*acd143(19)
      acd143(37)=acd143(44)+acd143(43)+acd143(42)+acd143(41)+acd143(40)+acd143(&
      &39)+acd143(37)+acd143(38)
      acd143(37)=acd143(18)*acd143(37)
      acd143(38)=-acd143(17)*acd143(16)
      acd143(39)=-acd143(15)*acd143(14)
      acd143(40)=acd143(13)*acd143(12)
      acd143(41)=-acd143(11)*acd143(10)
      acd143(42)=-acd143(9)*acd143(8)
      acd143(43)=-acd143(7)*acd143(6)
      acd143(44)=-acd143(5)*acd143(4)
      acd143(45)=-acd143(3)*acd143(2)
      acd143(38)=acd143(45)+acd143(44)+acd143(43)+acd143(42)+acd143(41)+acd143(&
      &40)+acd143(38)+acd143(39)
      acd143(38)=acd143(1)*acd143(38)
      acd143(36)=acd143(38)+acd143(36)+acd143(37)
      brack=2.0_ki*acd143(36)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd143h1
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/ (0.0_ki,0.0_ki),(0.0_ki,0.&
      &0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = k6+k5+k4
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
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d143:
   subroutine     reconstruct_d143(coeffs)
      use p8_cbarc_hepemg_groups, only: tensrec_info_group5
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group5), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_143:
      coeffs%coeffs_143%c0 = derivative(czip)
      coeffs%coeffs_143%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_143%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_143%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_143%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_143%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_143%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_143%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_143%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_143%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_143%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_143%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_143%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_143%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_143%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_143%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_143%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_143%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_143%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_143%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_143%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_143%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_143%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_143%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_143%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_143%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_143%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_143%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_143%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_143%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_143%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_143%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_143%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_143%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_143%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_143:
   end subroutine reconstruct_d143
!---#] subroutine reconstruct_d143:
end module     p8_cbarc_hepemg_d143h1l1d
