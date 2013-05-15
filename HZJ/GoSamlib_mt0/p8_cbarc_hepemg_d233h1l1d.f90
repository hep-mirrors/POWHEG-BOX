module     p8_cbarc_hepemg_d233h1l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity1d233h1l1d.f90
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
   public :: derivative , reconstruct_d233
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd233h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(46) :: acd233
      complex(ki) :: brack
      acd233(1)=dotproduct(k1,qshift)
      acd233(2)=dotproduct(e6,qshift)
      acd233(3)=abb233(6)
      acd233(4)=dotproduct(qshift,spvak2k4)
      acd233(5)=abb233(22)
      acd233(6)=dotproduct(qshift,spvak5k4)
      acd233(7)=abb233(25)
      acd233(8)=dotproduct(qshift,spvae6k1)
      acd233(9)=abb233(21)
      acd233(10)=dotproduct(qshift,spvak2e6)
      acd233(11)=abb233(19)
      acd233(12)=abb233(4)
      acd233(13)=dotproduct(k6,qshift)
      acd233(14)=abb233(16)
      acd233(15)=dotproduct(qshift,qshift)
      acd233(16)=abb233(18)
      acd233(17)=abb233(13)
      acd233(18)=dotproduct(qshift,spvak2k1)
      acd233(19)=abb233(11)
      acd233(20)=abb233(8)
      acd233(21)=dotproduct(qshift,spvak2k6)
      acd233(22)=abb233(30)
      acd233(23)=dotproduct(qshift,spvak6k1)
      acd233(24)=abb233(12)
      acd233(25)=abb233(10)
      acd233(26)=abb233(28)
      acd233(27)=abb233(14)
      acd233(28)=abb233(20)
      acd233(29)=abb233(15)
      acd233(30)=abb233(7)
      acd233(31)=abb233(27)
      acd233(32)=abb233(26)
      acd233(33)=abb233(17)
      acd233(34)=abb233(31)
      acd233(35)=abb233(23)
      acd233(36)=abb233(5)
      acd233(37)=abb233(29)
      acd233(38)=abb233(9)
      acd233(39)=-acd233(6)*acd233(19)
      acd233(39)=acd233(39)+acd233(20)
      acd233(39)=acd233(18)*acd233(39)
      acd233(40)=acd233(23)*acd233(24)
      acd233(41)=acd233(21)*acd233(22)
      acd233(42)=-acd233(15)*acd233(16)
      acd233(43)=acd233(4)*acd233(17)
      acd233(44)=acd233(1)*acd233(3)
      acd233(39)=acd233(44)+acd233(43)+acd233(42)+acd233(41)-acd233(25)+acd233(&
      &40)+acd233(39)
      acd233(39)=acd233(2)*acd233(39)
      acd233(40)=-acd233(21)*acd233(34)
      acd233(41)=acd233(18)*acd233(33)
      acd233(42)=acd233(10)*acd233(32)
      acd233(43)=acd233(8)*acd233(31)
      acd233(44)=acd233(1)*acd233(7)
      acd233(40)=acd233(44)+acd233(43)+acd233(42)+acd233(41)-acd233(35)+acd233(&
      &40)
      acd233(40)=acd233(6)*acd233(40)
      acd233(41)=acd233(10)*acd233(11)
      acd233(42)=acd233(8)*acd233(9)
      acd233(41)=acd233(41)-acd233(42)
      acd233(42)=-acd233(14)+acd233(41)
      acd233(42)=acd233(13)*acd233(42)
      acd233(43)=-acd233(10)*acd233(28)
      acd233(44)=-acd233(8)*acd233(27)
      acd233(43)=acd233(44)+acd233(29)+acd233(43)
      acd233(43)=acd233(15)*acd233(43)
      acd233(44)=-acd233(13)*acd233(5)
      acd233(45)=-acd233(15)*acd233(26)
      acd233(44)=acd233(45)-acd233(30)+acd233(44)
      acd233(44)=acd233(4)*acd233(44)
      acd233(45)=acd233(4)*acd233(5)
      acd233(41)=acd233(45)-acd233(12)-acd233(41)
      acd233(41)=acd233(1)*acd233(41)
      acd233(45)=-acd233(21)*acd233(37)
      acd233(46)=-acd233(18)*acd233(36)
      brack=acd233(38)+acd233(39)+acd233(40)+acd233(41)+acd233(42)+acd233(43)+a&
      &cd233(44)+acd233(45)+acd233(46)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd233h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(61) :: acd233
      complex(ki) :: brack
      acd233(1)=k1(iv1)
      acd233(2)=dotproduct(e6,qshift)
      acd233(3)=abb233(6)
      acd233(4)=dotproduct(qshift,spvak2k4)
      acd233(5)=abb233(22)
      acd233(6)=dotproduct(qshift,spvak5k4)
      acd233(7)=abb233(25)
      acd233(8)=dotproduct(qshift,spvae6k1)
      acd233(9)=abb233(21)
      acd233(10)=dotproduct(qshift,spvak2e6)
      acd233(11)=abb233(19)
      acd233(12)=abb233(4)
      acd233(13)=k6(iv1)
      acd233(14)=abb233(16)
      acd233(15)=e6(iv1)
      acd233(16)=dotproduct(k1,qshift)
      acd233(17)=dotproduct(qshift,qshift)
      acd233(18)=abb233(18)
      acd233(19)=abb233(13)
      acd233(20)=dotproduct(qshift,spvak2k1)
      acd233(21)=abb233(11)
      acd233(22)=abb233(8)
      acd233(23)=dotproduct(qshift,spvak2k6)
      acd233(24)=abb233(30)
      acd233(25)=dotproduct(qshift,spvak6k1)
      acd233(26)=abb233(12)
      acd233(27)=abb233(10)
      acd233(28)=qshift(iv1)
      acd233(29)=abb233(28)
      acd233(30)=abb233(14)
      acd233(31)=abb233(20)
      acd233(32)=abb233(15)
      acd233(33)=spvak2k4(iv1)
      acd233(34)=dotproduct(k6,qshift)
      acd233(35)=abb233(7)
      acd233(36)=spvak5k4(iv1)
      acd233(37)=abb233(27)
      acd233(38)=abb233(26)
      acd233(39)=abb233(17)
      acd233(40)=abb233(31)
      acd233(41)=abb233(23)
      acd233(42)=spvae6k1(iv1)
      acd233(43)=spvak2e6(iv1)
      acd233(44)=spvak2k1(iv1)
      acd233(45)=abb233(5)
      acd233(46)=spvak2k6(iv1)
      acd233(47)=abb233(29)
      acd233(48)=spvak6k1(iv1)
      acd233(49)=acd233(6)*acd233(21)
      acd233(49)=acd233(49)-acd233(22)
      acd233(50)=acd233(44)*acd233(49)
      acd233(51)=-acd233(26)*acd233(48)
      acd233(52)=-acd233(46)*acd233(24)
      acd233(53)=-acd233(33)*acd233(19)
      acd233(54)=2.0_ki*acd233(28)
      acd233(55)=acd233(18)*acd233(54)
      acd233(56)=-acd233(1)*acd233(3)
      acd233(57)=acd233(36)*acd233(20)*acd233(21)
      acd233(50)=acd233(57)+acd233(56)+acd233(55)+acd233(53)+acd233(51)+acd233(&
      &52)+acd233(50)
      acd233(50)=acd233(2)*acd233(50)
      acd233(49)=acd233(20)*acd233(49)
      acd233(51)=-acd233(26)*acd233(25)
      acd233(52)=-acd233(23)*acd233(24)
      acd233(53)=acd233(17)*acd233(18)
      acd233(55)=-acd233(4)*acd233(19)
      acd233(56)=-acd233(16)*acd233(3)
      acd233(49)=acd233(56)+acd233(55)+acd233(53)+acd233(52)+acd233(27)+acd233(&
      &51)+acd233(49)
      acd233(49)=acd233(15)*acd233(49)
      acd233(51)=acd233(23)*acd233(40)
      acd233(52)=-acd233(20)*acd233(39)
      acd233(53)=-acd233(10)*acd233(38)
      acd233(55)=-acd233(8)*acd233(37)
      acd233(56)=-acd233(16)*acd233(7)
      acd233(51)=acd233(56)+acd233(55)+acd233(53)+acd233(52)+acd233(41)+acd233(&
      &51)
      acd233(51)=acd233(36)*acd233(51)
      acd233(52)=acd233(46)*acd233(40)
      acd233(53)=-acd233(44)*acd233(39)
      acd233(55)=-acd233(43)*acd233(38)
      acd233(56)=-acd233(42)*acd233(37)
      acd233(57)=-acd233(1)*acd233(7)
      acd233(52)=acd233(57)+acd233(56)+acd233(55)+acd233(52)+acd233(53)
      acd233(52)=acd233(6)*acd233(52)
      acd233(53)=acd233(10)*acd233(11)
      acd233(55)=acd233(8)*acd233(9)
      acd233(56)=acd233(4)*acd233(5)
      acd233(53)=-acd233(56)+acd233(53)-acd233(55)
      acd233(55)=acd233(14)-acd233(53)
      acd233(55)=acd233(13)*acd233(55)
      acd233(56)=acd233(10)*acd233(31)
      acd233(57)=acd233(8)*acd233(30)
      acd233(58)=acd233(4)*acd233(29)
      acd233(56)=acd233(58)+acd233(57)-acd233(32)+acd233(56)
      acd233(54)=acd233(56)*acd233(54)
      acd233(53)=acd233(12)+acd233(53)
      acd233(53)=acd233(1)*acd233(53)
      acd233(56)=acd233(11)*acd233(43)
      acd233(57)=acd233(9)*acd233(42)
      acd233(56)=acd233(56)-acd233(57)
      acd233(57)=-acd233(34)*acd233(56)
      acd233(58)=acd233(43)*acd233(31)
      acd233(59)=acd233(42)*acd233(30)
      acd233(58)=acd233(58)+acd233(59)
      acd233(58)=acd233(17)*acd233(58)
      acd233(59)=acd233(17)*acd233(29)
      acd233(60)=acd233(5)*acd233(34)
      acd233(59)=acd233(60)+acd233(35)+acd233(59)
      acd233(59)=acd233(33)*acd233(59)
      acd233(60)=-acd233(33)*acd233(5)
      acd233(56)=acd233(60)+acd233(56)
      acd233(56)=acd233(16)*acd233(56)
      acd233(60)=acd233(46)*acd233(47)
      acd233(61)=acd233(44)*acd233(45)
      brack=acd233(49)+acd233(50)+acd233(51)+acd233(52)+acd233(53)+acd233(54)+a&
      &cd233(55)+acd233(56)+acd233(57)+acd233(58)+acd233(59)+acd233(60)+acd233(&
      &61)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd233h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(60) :: acd233
      complex(ki) :: brack
      acd233(1)=d(iv1,iv2)
      acd233(2)=dotproduct(e6,qshift)
      acd233(3)=abb233(18)
      acd233(4)=dotproduct(qshift,spvak2k4)
      acd233(5)=abb233(28)
      acd233(6)=dotproduct(qshift,spvae6k1)
      acd233(7)=abb233(14)
      acd233(8)=dotproduct(qshift,spvak2e6)
      acd233(9)=abb233(20)
      acd233(10)=abb233(15)
      acd233(11)=k1(iv1)
      acd233(12)=e6(iv2)
      acd233(13)=abb233(6)
      acd233(14)=spvak2k4(iv2)
      acd233(15)=abb233(22)
      acd233(16)=spvae6k1(iv2)
      acd233(17)=abb233(21)
      acd233(18)=spvak2e6(iv2)
      acd233(19)=abb233(19)
      acd233(20)=spvak5k4(iv2)
      acd233(21)=abb233(25)
      acd233(22)=k1(iv2)
      acd233(23)=e6(iv1)
      acd233(24)=spvak2k4(iv1)
      acd233(25)=spvae6k1(iv1)
      acd233(26)=spvak2e6(iv1)
      acd233(27)=spvak5k4(iv1)
      acd233(28)=k6(iv1)
      acd233(29)=k6(iv2)
      acd233(30)=qshift(iv2)
      acd233(31)=abb233(13)
      acd233(32)=dotproduct(qshift,spvak2k1)
      acd233(33)=abb233(11)
      acd233(34)=spvak2k1(iv2)
      acd233(35)=dotproduct(qshift,spvak5k4)
      acd233(36)=abb233(8)
      acd233(37)=spvak2k6(iv2)
      acd233(38)=abb233(30)
      acd233(39)=spvak6k1(iv2)
      acd233(40)=abb233(12)
      acd233(41)=qshift(iv1)
      acd233(42)=spvak2k1(iv1)
      acd233(43)=spvak2k6(iv1)
      acd233(44)=spvak6k1(iv1)
      acd233(45)=abb233(27)
      acd233(46)=abb233(26)
      acd233(47)=abb233(17)
      acd233(48)=abb233(31)
      acd233(49)=acd233(40)*acd233(39)
      acd233(50)=acd233(37)*acd233(38)
      acd233(51)=acd233(34)*acd233(36)
      acd233(52)=2.0_ki*acd233(3)
      acd233(52)=-acd233(30)*acd233(52)
      acd233(53)=acd233(14)*acd233(31)
      acd233(54)=acd233(22)*acd233(13)
      acd233(55)=acd233(33)*acd233(34)
      acd233(56)=-acd233(35)*acd233(55)
      acd233(57)=acd233(33)*acd233(32)
      acd233(58)=-acd233(20)*acd233(57)
      acd233(49)=acd233(58)+acd233(56)+acd233(54)+acd233(53)+acd233(52)+acd233(&
      &51)+acd233(49)+acd233(50)
      acd233(49)=acd233(23)*acd233(49)
      acd233(50)=acd233(40)*acd233(44)
      acd233(51)=acd233(38)*acd233(43)
      acd233(52)=acd233(42)*acd233(36)
      acd233(53)=2.0_ki*acd233(41)
      acd233(54)=-acd233(3)*acd233(53)
      acd233(56)=acd233(24)*acd233(31)
      acd233(58)=acd233(11)*acd233(13)
      acd233(59)=acd233(33)*acd233(42)
      acd233(60)=-acd233(35)*acd233(59)
      acd233(57)=-acd233(27)*acd233(57)
      acd233(50)=acd233(57)+acd233(60)+acd233(58)+acd233(56)+acd233(54)+acd233(&
      &52)+acd233(50)+acd233(51)
      acd233(50)=acd233(12)*acd233(50)
      acd233(51)=-acd233(37)*acd233(48)
      acd233(52)=acd233(34)*acd233(47)
      acd233(54)=acd233(18)*acd233(46)
      acd233(56)=acd233(16)*acd233(45)
      acd233(57)=acd233(22)*acd233(21)
      acd233(55)=-acd233(2)*acd233(55)
      acd233(51)=acd233(55)+acd233(57)+acd233(56)+acd233(54)+acd233(51)+acd233(&
      &52)
      acd233(51)=acd233(27)*acd233(51)
      acd233(52)=-acd233(43)*acd233(48)
      acd233(54)=acd233(42)*acd233(47)
      acd233(55)=acd233(26)*acd233(46)
      acd233(56)=acd233(25)*acd233(45)
      acd233(57)=acd233(11)*acd233(21)
      acd233(58)=-acd233(2)*acd233(59)
      acd233(52)=acd233(58)+acd233(57)+acd233(56)+acd233(55)+acd233(52)+acd233(&
      &54)
      acd233(52)=acd233(20)*acd233(52)
      acd233(54)=-acd233(9)*acd233(8)
      acd233(55)=-acd233(7)*acd233(6)
      acd233(56)=-acd233(5)*acd233(4)
      acd233(57)=-acd233(2)*acd233(3)
      acd233(54)=acd233(57)+acd233(56)+acd233(55)+acd233(10)+acd233(54)
      acd233(54)=acd233(1)*acd233(54)
      acd233(55)=-acd233(26)*acd233(9)
      acd233(56)=-acd233(25)*acd233(7)
      acd233(57)=-acd233(24)*acd233(5)
      acd233(55)=acd233(57)+acd233(55)+acd233(56)
      acd233(55)=acd233(30)*acd233(55)
      acd233(54)=acd233(55)+acd233(54)
      acd233(55)=acd233(19)*acd233(26)
      acd233(56)=acd233(17)*acd233(25)
      acd233(57)=acd233(15)*acd233(24)
      acd233(55)=-acd233(57)+acd233(55)-acd233(56)
      acd233(56)=-acd233(22)+acd233(29)
      acd233(55)=acd233(55)*acd233(56)
      acd233(56)=-acd233(18)*acd233(19)
      acd233(57)=acd233(16)*acd233(17)
      acd233(58)=acd233(14)*acd233(15)
      acd233(56)=acd233(58)+acd233(56)+acd233(57)
      acd233(56)=acd233(11)*acd233(56)
      acd233(57)=-acd233(9)*acd233(53)
      acd233(58)=acd233(19)*acd233(28)
      acd233(57)=acd233(57)+acd233(58)
      acd233(57)=acd233(18)*acd233(57)
      acd233(58)=-acd233(7)*acd233(53)
      acd233(59)=-acd233(17)*acd233(28)
      acd233(58)=acd233(58)+acd233(59)
      acd233(58)=acd233(16)*acd233(58)
      acd233(53)=-acd233(5)*acd233(53)
      acd233(59)=-acd233(15)*acd233(28)
      acd233(53)=acd233(53)+acd233(59)
      acd233(53)=acd233(14)*acd233(53)
      brack=acd233(49)+acd233(50)+acd233(51)+acd233(52)+acd233(53)+2.0_ki*acd23&
      &3(54)+acd233(55)+acd233(56)+acd233(57)+acd233(58)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd233h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(32) :: acd233
      complex(ki) :: brack
      acd233(1)=d(iv1,iv2)
      acd233(2)=e6(iv3)
      acd233(3)=abb233(18)
      acd233(4)=spvak2k4(iv3)
      acd233(5)=abb233(28)
      acd233(6)=spvae6k1(iv3)
      acd233(7)=abb233(14)
      acd233(8)=spvak2e6(iv3)
      acd233(9)=abb233(20)
      acd233(10)=d(iv1,iv3)
      acd233(11)=e6(iv2)
      acd233(12)=spvak2k4(iv2)
      acd233(13)=spvae6k1(iv2)
      acd233(14)=spvak2e6(iv2)
      acd233(15)=d(iv2,iv3)
      acd233(16)=e6(iv1)
      acd233(17)=spvak2k4(iv1)
      acd233(18)=spvae6k1(iv1)
      acd233(19)=spvak2e6(iv1)
      acd233(20)=spvak2k1(iv2)
      acd233(21)=spvak5k4(iv3)
      acd233(22)=abb233(11)
      acd233(23)=spvak2k1(iv3)
      acd233(24)=spvak5k4(iv2)
      acd233(25)=spvak2k1(iv1)
      acd233(26)=spvak5k4(iv1)
      acd233(27)=acd233(9)*acd233(19)
      acd233(28)=acd233(7)*acd233(18)
      acd233(29)=acd233(5)*acd233(17)
      acd233(30)=acd233(3)*acd233(16)
      acd233(27)=acd233(30)+acd233(29)+acd233(27)+acd233(28)
      acd233(27)=acd233(15)*acd233(27)
      acd233(28)=acd233(9)*acd233(14)
      acd233(29)=acd233(7)*acd233(13)
      acd233(30)=acd233(5)*acd233(12)
      acd233(31)=acd233(3)*acd233(11)
      acd233(28)=acd233(31)+acd233(30)+acd233(28)+acd233(29)
      acd233(28)=acd233(10)*acd233(28)
      acd233(29)=acd233(9)*acd233(8)
      acd233(30)=acd233(7)*acd233(6)
      acd233(31)=acd233(5)*acd233(4)
      acd233(32)=acd233(2)*acd233(3)
      acd233(29)=acd233(32)+acd233(31)+acd233(29)+acd233(30)
      acd233(29)=acd233(1)*acd233(29)
      acd233(27)=acd233(29)+acd233(27)+acd233(28)
      acd233(28)=acd233(23)*acd233(24)
      acd233(29)=acd233(20)*acd233(21)
      acd233(28)=acd233(28)+acd233(29)
      acd233(28)=acd233(16)*acd233(28)
      acd233(29)=acd233(23)*acd233(26)
      acd233(30)=acd233(21)*acd233(25)
      acd233(29)=acd233(29)+acd233(30)
      acd233(29)=acd233(11)*acd233(29)
      acd233(30)=acd233(24)*acd233(25)
      acd233(31)=acd233(20)*acd233(26)
      acd233(30)=acd233(30)+acd233(31)
      acd233(30)=acd233(2)*acd233(30)
      acd233(28)=acd233(30)+acd233(28)+acd233(29)
      acd233(28)=acd233(22)*acd233(28)
      brack=2.0_ki*acd233(27)+acd233(28)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd233h1
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
      qshift = k2
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
!---#[ subroutine reconstruct_d233:
   subroutine     reconstruct_d233(coeffs)
      use p8_cbarc_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_233:
      coeffs%coeffs_233%c0 = derivative(czip)
      coeffs%coeffs_233%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_233%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_233%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_233%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_233%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_233%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_233%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_233%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_233%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_233%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_233%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_233%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_233%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_233%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_233%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_233%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_233%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_233%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_233%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_233%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_233%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_233%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_233%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_233%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_233%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_233%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_233%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_233%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_233%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_233%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_233%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_233%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_233%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_233%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_233:
   end subroutine reconstruct_d233
!---#] subroutine reconstruct_d233:
end module     p8_cbarc_hepemg_d233h1l1d
