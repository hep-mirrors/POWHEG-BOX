module     p8_cbarc_hepemg_d234h0l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity0d234h0l1d.f90
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
   public :: derivative , reconstruct_d234
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd234h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(46) :: acd234
      complex(ki) :: brack
      acd234(1)=dotproduct(k1,qshift)
      acd234(2)=dotproduct(e6,qshift)
      acd234(3)=abb234(5)
      acd234(4)=abb234(4)
      acd234(5)=dotproduct(k2,qshift)
      acd234(6)=dotproduct(qshift,spvak1k4)
      acd234(7)=abb234(16)
      acd234(8)=dotproduct(qshift,spvak5k2)
      acd234(9)=abb234(25)
      acd234(10)=dotproduct(qshift,spvak5k4)
      acd234(11)=abb234(26)
      acd234(12)=dotproduct(qshift,spvae6k2)
      acd234(13)=abb234(11)
      acd234(14)=abb234(18)
      acd234(15)=dotproduct(k6,qshift)
      acd234(16)=abb234(17)
      acd234(17)=abb234(30)
      acd234(18)=abb234(22)
      acd234(19)=dotproduct(qshift,spvak1k2)
      acd234(20)=abb234(14)
      acd234(21)=abb234(13)
      acd234(22)=dotproduct(qshift,spvak6k2)
      acd234(23)=abb234(19)
      acd234(24)=abb234(12)
      acd234(25)=dotproduct(qshift,qshift)
      acd234(26)=abb234(6)
      acd234(27)=abb234(23)
      acd234(28)=abb234(27)
      acd234(29)=abb234(21)
      acd234(30)=abb234(20)
      acd234(31)=abb234(28)
      acd234(32)=abb234(29)
      acd234(33)=dotproduct(qshift,spvak1k6)
      acd234(34)=abb234(7)
      acd234(35)=abb234(24)
      acd234(36)=abb234(10)
      acd234(37)=abb234(8)
      acd234(38)=abb234(9)
      acd234(39)=-acd234(33)*acd234(34)
      acd234(40)=acd234(22)*acd234(32)
      acd234(41)=-acd234(25)*acd234(28)
      acd234(42)=acd234(12)*acd234(31)
      acd234(43)=acd234(5)*acd234(11)
      acd234(39)=acd234(43)+acd234(42)+acd234(41)+acd234(40)-acd234(35)+acd234(&
      &39)
      acd234(39)=acd234(10)*acd234(39)
      acd234(40)=acd234(10)*acd234(20)
      acd234(40)=acd234(40)+acd234(21)
      acd234(40)=acd234(19)*acd234(40)
      acd234(41)=acd234(5)+acd234(1)
      acd234(41)=acd234(3)*acd234(41)
      acd234(42)=acd234(22)*acd234(23)
      acd234(43)=acd234(8)*acd234(18)
      acd234(44)=acd234(6)*acd234(17)
      acd234(40)=acd234(44)+acd234(43)+acd234(42)-acd234(24)+acd234(41)+acd234(&
      &40)
      acd234(40)=acd234(2)*acd234(40)
      acd234(41)=acd234(8)*acd234(9)
      acd234(42)=acd234(6)*acd234(7)
      acd234(41)=acd234(41)-acd234(42)
      acd234(42)=-acd234(16)+acd234(41)
      acd234(42)=acd234(15)*acd234(42)
      acd234(43)=-acd234(8)*acd234(27)
      acd234(44)=-acd234(6)*acd234(26)
      acd234(43)=acd234(44)+acd234(30)+acd234(43)
      acd234(43)=acd234(25)*acd234(43)
      acd234(44)=-acd234(15)*acd234(13)
      acd234(45)=-acd234(25)*acd234(29)
      acd234(44)=acd234(45)-acd234(36)+acd234(44)
      acd234(44)=acd234(12)*acd234(44)
      acd234(45)=acd234(12)*acd234(13)
      acd234(41)=acd234(45)-acd234(14)-acd234(41)
      acd234(41)=acd234(5)*acd234(41)
      acd234(45)=-acd234(1)*acd234(4)
      acd234(46)=-acd234(22)*acd234(37)
      brack=acd234(38)+acd234(39)+acd234(40)+acd234(41)+acd234(42)+acd234(43)+a&
      &cd234(44)+acd234(45)+acd234(46)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd234h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(62) :: acd234
      complex(ki) :: brack
      acd234(1)=k1(iv1)
      acd234(2)=dotproduct(e6,qshift)
      acd234(3)=abb234(5)
      acd234(4)=abb234(4)
      acd234(5)=k2(iv1)
      acd234(6)=dotproduct(qshift,spvak1k4)
      acd234(7)=abb234(16)
      acd234(8)=dotproduct(qshift,spvak5k2)
      acd234(9)=abb234(25)
      acd234(10)=dotproduct(qshift,spvak5k4)
      acd234(11)=abb234(26)
      acd234(12)=dotproduct(qshift,spvae6k2)
      acd234(13)=abb234(11)
      acd234(14)=abb234(18)
      acd234(15)=k6(iv1)
      acd234(16)=abb234(17)
      acd234(17)=e6(iv1)
      acd234(18)=dotproduct(k1,qshift)
      acd234(19)=dotproduct(k2,qshift)
      acd234(20)=abb234(30)
      acd234(21)=abb234(22)
      acd234(22)=dotproduct(qshift,spvak1k2)
      acd234(23)=abb234(14)
      acd234(24)=abb234(13)
      acd234(25)=dotproduct(qshift,spvak6k2)
      acd234(26)=abb234(19)
      acd234(27)=abb234(12)
      acd234(28)=qshift(iv1)
      acd234(29)=abb234(6)
      acd234(30)=abb234(23)
      acd234(31)=abb234(27)
      acd234(32)=abb234(21)
      acd234(33)=abb234(20)
      acd234(34)=spvak1k4(iv1)
      acd234(35)=dotproduct(k6,qshift)
      acd234(36)=dotproduct(qshift,qshift)
      acd234(37)=spvak5k2(iv1)
      acd234(38)=spvak5k4(iv1)
      acd234(39)=abb234(28)
      acd234(40)=abb234(29)
      acd234(41)=dotproduct(qshift,spvak1k6)
      acd234(42)=abb234(7)
      acd234(43)=abb234(24)
      acd234(44)=spvae6k2(iv1)
      acd234(45)=abb234(10)
      acd234(46)=spvak1k2(iv1)
      acd234(47)=spvak6k2(iv1)
      acd234(48)=abb234(8)
      acd234(49)=spvak1k6(iv1)
      acd234(50)=acd234(10)*acd234(23)
      acd234(50)=acd234(50)+acd234(24)
      acd234(51)=acd234(46)*acd234(50)
      acd234(52)=acd234(5)+acd234(1)
      acd234(52)=acd234(3)*acd234(52)
      acd234(53)=acd234(47)*acd234(26)
      acd234(54)=acd234(37)*acd234(21)
      acd234(55)=acd234(34)*acd234(20)
      acd234(56)=acd234(38)*acd234(22)*acd234(23)
      acd234(51)=acd234(56)+acd234(55)+acd234(54)+acd234(53)+acd234(52)+acd234(&
      &51)
      acd234(51)=acd234(2)*acd234(51)
      acd234(52)=-acd234(42)*acd234(41)
      acd234(53)=acd234(25)*acd234(40)
      acd234(54)=-acd234(36)*acd234(31)
      acd234(55)=acd234(12)*acd234(39)
      acd234(56)=acd234(19)*acd234(11)
      acd234(52)=acd234(56)+acd234(55)+acd234(54)+acd234(53)-acd234(43)+acd234(&
      &52)
      acd234(52)=acd234(38)*acd234(52)
      acd234(53)=-acd234(42)*acd234(49)
      acd234(54)=acd234(47)*acd234(40)
      acd234(55)=acd234(44)*acd234(39)
      acd234(56)=2.0_ki*acd234(28)
      acd234(57)=-acd234(31)*acd234(56)
      acd234(58)=acd234(5)*acd234(11)
      acd234(53)=acd234(58)+acd234(57)+acd234(55)+acd234(53)+acd234(54)
      acd234(53)=acd234(10)*acd234(53)
      acd234(50)=acd234(22)*acd234(50)
      acd234(54)=acd234(19)+acd234(18)
      acd234(54)=acd234(3)*acd234(54)
      acd234(55)=acd234(25)*acd234(26)
      acd234(57)=acd234(8)*acd234(21)
      acd234(58)=acd234(6)*acd234(20)
      acd234(50)=acd234(58)+acd234(57)-acd234(27)+acd234(55)+acd234(54)+acd234(&
      &50)
      acd234(50)=acd234(17)*acd234(50)
      acd234(54)=acd234(12)*acd234(13)
      acd234(55)=acd234(8)*acd234(9)
      acd234(57)=acd234(6)*acd234(7)
      acd234(54)=acd234(57)+acd234(54)-acd234(55)
      acd234(55)=-acd234(16)-acd234(54)
      acd234(55)=acd234(15)*acd234(55)
      acd234(57)=-acd234(12)*acd234(32)
      acd234(58)=-acd234(8)*acd234(30)
      acd234(59)=-acd234(6)*acd234(29)
      acd234(57)=acd234(59)+acd234(58)+acd234(33)+acd234(57)
      acd234(56)=acd234(57)*acd234(56)
      acd234(54)=-acd234(14)+acd234(54)
      acd234(54)=acd234(5)*acd234(54)
      acd234(57)=acd234(9)*acd234(37)
      acd234(58)=acd234(7)*acd234(34)
      acd234(57)=acd234(57)-acd234(58)
      acd234(58)=acd234(35)*acd234(57)
      acd234(59)=-acd234(37)*acd234(30)
      acd234(60)=-acd234(34)*acd234(29)
      acd234(59)=acd234(60)+acd234(59)
      acd234(59)=acd234(36)*acd234(59)
      acd234(60)=-acd234(36)*acd234(32)
      acd234(61)=-acd234(13)*acd234(35)
      acd234(60)=acd234(61)-acd234(45)+acd234(60)
      acd234(60)=acd234(44)*acd234(60)
      acd234(61)=acd234(44)*acd234(13)
      acd234(57)=acd234(61)-acd234(57)
      acd234(57)=acd234(19)*acd234(57)
      acd234(61)=-acd234(1)*acd234(4)
      acd234(62)=-acd234(47)*acd234(48)
      brack=acd234(50)+acd234(51)+acd234(52)+acd234(53)+acd234(54)+acd234(55)+a&
      &cd234(56)+acd234(57)+acd234(58)+acd234(59)+acd234(60)+acd234(61)+acd234(&
      &62)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd234h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(60) :: acd234
      complex(ki) :: brack
      acd234(1)=d(iv1,iv2)
      acd234(2)=dotproduct(qshift,spvak1k4)
      acd234(3)=abb234(6)
      acd234(4)=dotproduct(qshift,spvak5k2)
      acd234(5)=abb234(23)
      acd234(6)=dotproduct(qshift,spvak5k4)
      acd234(7)=abb234(27)
      acd234(8)=dotproduct(qshift,spvae6k2)
      acd234(9)=abb234(21)
      acd234(10)=abb234(20)
      acd234(11)=k1(iv1)
      acd234(12)=e6(iv2)
      acd234(13)=abb234(5)
      acd234(14)=k1(iv2)
      acd234(15)=e6(iv1)
      acd234(16)=k2(iv1)
      acd234(17)=spvak1k4(iv2)
      acd234(18)=abb234(16)
      acd234(19)=spvak5k2(iv2)
      acd234(20)=abb234(25)
      acd234(21)=spvak5k4(iv2)
      acd234(22)=abb234(26)
      acd234(23)=spvae6k2(iv2)
      acd234(24)=abb234(11)
      acd234(25)=k2(iv2)
      acd234(26)=spvak1k4(iv1)
      acd234(27)=spvak5k2(iv1)
      acd234(28)=spvak5k4(iv1)
      acd234(29)=spvae6k2(iv1)
      acd234(30)=k6(iv1)
      acd234(31)=k6(iv2)
      acd234(32)=abb234(30)
      acd234(33)=abb234(22)
      acd234(34)=dotproduct(qshift,spvak1k2)
      acd234(35)=abb234(14)
      acd234(36)=spvak1k2(iv2)
      acd234(37)=abb234(13)
      acd234(38)=spvak6k2(iv2)
      acd234(39)=abb234(19)
      acd234(40)=spvak1k2(iv1)
      acd234(41)=spvak6k2(iv1)
      acd234(42)=qshift(iv1)
      acd234(43)=qshift(iv2)
      acd234(44)=abb234(28)
      acd234(45)=dotproduct(e6,qshift)
      acd234(46)=abb234(29)
      acd234(47)=spvak1k6(iv2)
      acd234(48)=abb234(7)
      acd234(49)=spvak1k6(iv1)
      acd234(50)=-acd234(48)*acd234(47)
      acd234(51)=acd234(38)*acd234(46)
      acd234(52)=2.0_ki*acd234(7)
      acd234(52)=-acd234(43)*acd234(52)
      acd234(53)=acd234(23)*acd234(44)
      acd234(54)=acd234(25)*acd234(22)
      acd234(55)=acd234(35)*acd234(45)
      acd234(56)=acd234(36)*acd234(55)
      acd234(50)=acd234(56)+acd234(54)+acd234(53)+acd234(52)+acd234(50)+acd234(&
      &51)
      acd234(50)=acd234(28)*acd234(50)
      acd234(51)=-acd234(48)*acd234(49)
      acd234(52)=acd234(41)*acd234(46)
      acd234(53)=2.0_ki*acd234(42)
      acd234(54)=-acd234(7)*acd234(53)
      acd234(56)=acd234(29)*acd234(44)
      acd234(57)=acd234(16)*acd234(22)
      acd234(55)=acd234(40)*acd234(55)
      acd234(51)=acd234(55)+acd234(57)+acd234(56)+acd234(54)+acd234(51)+acd234(&
      &52)
      acd234(51)=acd234(21)*acd234(51)
      acd234(52)=acd234(35)*acd234(6)
      acd234(52)=acd234(52)+acd234(37)
      acd234(54)=acd234(36)*acd234(52)
      acd234(55)=acd234(25)+acd234(14)
      acd234(55)=acd234(13)*acd234(55)
      acd234(56)=acd234(38)*acd234(39)
      acd234(57)=acd234(19)*acd234(33)
      acd234(58)=acd234(17)*acd234(32)
      acd234(59)=acd234(35)*acd234(34)
      acd234(60)=acd234(21)*acd234(59)
      acd234(54)=acd234(60)+acd234(58)+acd234(57)+acd234(56)+acd234(55)+acd234(&
      &54)
      acd234(54)=acd234(15)*acd234(54)
      acd234(52)=acd234(40)*acd234(52)
      acd234(55)=acd234(16)+acd234(11)
      acd234(55)=acd234(13)*acd234(55)
      acd234(56)=acd234(39)*acd234(41)
      acd234(57)=acd234(27)*acd234(33)
      acd234(58)=acd234(26)*acd234(32)
      acd234(59)=acd234(28)*acd234(59)
      acd234(52)=acd234(59)+acd234(58)+acd234(57)+acd234(56)+acd234(55)+acd234(&
      &52)
      acd234(52)=acd234(12)*acd234(52)
      acd234(55)=-acd234(9)*acd234(8)
      acd234(56)=-acd234(6)*acd234(7)
      acd234(57)=-acd234(5)*acd234(4)
      acd234(58)=-acd234(3)*acd234(2)
      acd234(55)=acd234(58)+acd234(57)+acd234(56)+acd234(10)+acd234(55)
      acd234(55)=acd234(1)*acd234(55)
      acd234(56)=-acd234(29)*acd234(9)
      acd234(57)=-acd234(27)*acd234(5)
      acd234(58)=-acd234(26)*acd234(3)
      acd234(56)=acd234(58)+acd234(56)+acd234(57)
      acd234(56)=acd234(43)*acd234(56)
      acd234(55)=acd234(56)+acd234(55)
      acd234(56)=acd234(24)*acd234(29)
      acd234(57)=acd234(20)*acd234(27)
      acd234(58)=acd234(18)*acd234(26)
      acd234(56)=acd234(58)+acd234(56)-acd234(57)
      acd234(57)=acd234(25)-acd234(31)
      acd234(56)=acd234(56)*acd234(57)
      acd234(57)=acd234(23)*acd234(24)
      acd234(58)=-acd234(19)*acd234(20)
      acd234(59)=acd234(17)*acd234(18)
      acd234(57)=acd234(59)+acd234(57)+acd234(58)
      acd234(57)=acd234(16)*acd234(57)
      acd234(58)=-acd234(9)*acd234(53)
      acd234(59)=-acd234(24)*acd234(30)
      acd234(58)=acd234(58)+acd234(59)
      acd234(58)=acd234(23)*acd234(58)
      acd234(59)=-acd234(5)*acd234(53)
      acd234(60)=acd234(20)*acd234(30)
      acd234(59)=acd234(59)+acd234(60)
      acd234(59)=acd234(19)*acd234(59)
      acd234(53)=-acd234(3)*acd234(53)
      acd234(60)=-acd234(18)*acd234(30)
      acd234(53)=acd234(53)+acd234(60)
      acd234(53)=acd234(17)*acd234(53)
      brack=acd234(50)+acd234(51)+acd234(52)+acd234(53)+acd234(54)+2.0_ki*acd23&
      &4(55)+acd234(56)+acd234(57)+acd234(58)+acd234(59)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd234h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(32) :: acd234
      complex(ki) :: brack
      acd234(1)=d(iv1,iv2)
      acd234(2)=spvak1k4(iv3)
      acd234(3)=abb234(6)
      acd234(4)=spvak5k2(iv3)
      acd234(5)=abb234(23)
      acd234(6)=spvak5k4(iv3)
      acd234(7)=abb234(27)
      acd234(8)=spvae6k2(iv3)
      acd234(9)=abb234(21)
      acd234(10)=d(iv1,iv3)
      acd234(11)=spvak1k4(iv2)
      acd234(12)=spvak5k2(iv2)
      acd234(13)=spvak5k4(iv2)
      acd234(14)=spvae6k2(iv2)
      acd234(15)=d(iv2,iv3)
      acd234(16)=spvak1k4(iv1)
      acd234(17)=spvak5k2(iv1)
      acd234(18)=spvak5k4(iv1)
      acd234(19)=spvae6k2(iv1)
      acd234(20)=e6(iv1)
      acd234(21)=spvak1k2(iv3)
      acd234(22)=abb234(14)
      acd234(23)=spvak1k2(iv2)
      acd234(24)=e6(iv2)
      acd234(25)=spvak1k2(iv1)
      acd234(26)=e6(iv3)
      acd234(27)=-acd234(9)*acd234(19)
      acd234(28)=-acd234(7)*acd234(18)
      acd234(29)=-acd234(5)*acd234(17)
      acd234(30)=-acd234(3)*acd234(16)
      acd234(27)=acd234(30)+acd234(29)+acd234(27)+acd234(28)
      acd234(27)=acd234(15)*acd234(27)
      acd234(28)=-acd234(9)*acd234(14)
      acd234(29)=-acd234(7)*acd234(13)
      acd234(30)=-acd234(5)*acd234(12)
      acd234(31)=-acd234(3)*acd234(11)
      acd234(28)=acd234(31)+acd234(30)+acd234(28)+acd234(29)
      acd234(28)=acd234(10)*acd234(28)
      acd234(29)=-acd234(9)*acd234(8)
      acd234(30)=-acd234(6)*acd234(7)
      acd234(31)=-acd234(5)*acd234(4)
      acd234(32)=-acd234(3)*acd234(2)
      acd234(29)=acd234(32)+acd234(31)+acd234(29)+acd234(30)
      acd234(29)=acd234(1)*acd234(29)
      acd234(27)=acd234(29)+acd234(27)+acd234(28)
      acd234(28)=acd234(23)*acd234(26)
      acd234(29)=acd234(21)*acd234(24)
      acd234(28)=acd234(28)+acd234(29)
      acd234(28)=acd234(18)*acd234(28)
      acd234(29)=acd234(25)*acd234(26)
      acd234(30)=acd234(20)*acd234(21)
      acd234(29)=acd234(29)+acd234(30)
      acd234(29)=acd234(13)*acd234(29)
      acd234(30)=acd234(24)*acd234(25)
      acd234(31)=acd234(20)*acd234(23)
      acd234(30)=acd234(30)+acd234(31)
      acd234(30)=acd234(6)*acd234(30)
      acd234(28)=acd234(30)+acd234(28)+acd234(29)
      acd234(28)=acd234(22)*acd234(28)
      brack=2.0_ki*acd234(27)+acd234(28)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd234h0
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
      qshift = -k2
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
!---#[ subroutine reconstruct_d234:
   subroutine     reconstruct_d234(coeffs)
      use p8_cbarc_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_234:
      coeffs%coeffs_234%c0 = derivative(czip)
      coeffs%coeffs_234%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_234%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_234%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_234%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_234%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_234%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_234%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_234%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_234%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_234%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_234%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_234%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_234%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_234%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_234%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_234%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_234%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_234%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_234%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_234%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_234%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_234%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_234%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_234%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_234%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_234%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_234%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_234%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_234%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_234%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_234%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_234%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_234%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_234%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_234:
   end subroutine reconstruct_d234
!---#] subroutine reconstruct_d234:
end module     p8_cbarc_hepemg_d234h0l1d
