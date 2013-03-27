module     p8_cbarc_hepemg_d205h2l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p8_cb &
   ! &arc_hepemg/helicity2d205h2l1d.f90
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
   public :: derivative , reconstruct_d205
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd205h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(48) :: acd205
      complex(ki) :: brack
      acd205(1)=dotproduct(k1,qshift)
      acd205(2)=dotproduct(e6,qshift)
      acd205(3)=abb205(5)
      acd205(4)=abb205(4)
      acd205(5)=dotproduct(k2,qshift)
      acd205(6)=dotproduct(qshift,spvak1k5)
      acd205(7)=abb205(16)
      acd205(8)=dotproduct(qshift,spvak4k2)
      acd205(9)=abb205(25)
      acd205(10)=dotproduct(qshift,spvak4k5)
      acd205(11)=abb205(26)
      acd205(12)=dotproduct(qshift,spvae6k2)
      acd205(13)=abb205(11)
      acd205(14)=abb205(18)
      acd205(15)=dotproduct(k6,qshift)
      acd205(16)=abb205(17)
      acd205(17)=abb205(30)
      acd205(18)=abb205(22)
      acd205(19)=dotproduct(qshift,spvak1k2)
      acd205(20)=abb205(14)
      acd205(21)=abb205(13)
      acd205(22)=dotproduct(qshift,spvak6k2)
      acd205(23)=abb205(19)
      acd205(24)=abb205(12)
      acd205(25)=dotproduct(qshift,qshift)
      acd205(26)=abb205(6)
      acd205(27)=abb205(23)
      acd205(28)=abb205(27)
      acd205(29)=abb205(21)
      acd205(30)=abb205(20)
      acd205(31)=abb205(28)
      acd205(32)=abb205(29)
      acd205(33)=dotproduct(qshift,spvak1k6)
      acd205(34)=abb205(7)
      acd205(35)=abb205(24)
      acd205(36)=abb205(10)
      acd205(37)=abb205(8)
      acd205(38)=abb205(9)
      acd205(39)=acd205(11)*acd205(5)
      acd205(40)=acd205(19)*acd205(2)
      acd205(41)=acd205(20)*acd205(40)
      acd205(42)=-acd205(28)*acd205(25)
      acd205(43)=acd205(31)*acd205(12)
      acd205(44)=acd205(32)*acd205(22)
      acd205(45)=-acd205(34)*acd205(33)
      acd205(39)=-acd205(35)+acd205(45)+acd205(44)+acd205(43)+acd205(42)+acd205&
      &(41)+acd205(39)
      acd205(39)=acd205(10)*acd205(39)
      acd205(41)=acd205(5)+acd205(1)
      acd205(41)=acd205(3)*acd205(41)
      acd205(42)=acd205(17)*acd205(6)
      acd205(43)=acd205(18)*acd205(8)
      acd205(44)=acd205(23)*acd205(22)
      acd205(41)=acd205(41)-acd205(24)+acd205(44)+acd205(43)+acd205(42)
      acd205(41)=acd205(2)*acd205(41)
      acd205(42)=-acd205(7)*acd205(6)
      acd205(43)=acd205(9)*acd205(8)
      acd205(44)=-acd205(13)*acd205(12)
      acd205(42)=acd205(44)+acd205(43)+acd205(42)
      acd205(43)=acd205(15)-acd205(5)
      acd205(42)=acd205(43)*acd205(42)
      acd205(43)=-acd205(26)*acd205(6)
      acd205(44)=-acd205(27)*acd205(8)
      acd205(45)=-acd205(29)*acd205(12)
      acd205(43)=acd205(30)+acd205(45)+acd205(44)+acd205(43)
      acd205(43)=acd205(25)*acd205(43)
      acd205(44)=-acd205(4)*acd205(1)
      acd205(45)=-acd205(14)*acd205(5)
      acd205(46)=-acd205(16)*acd205(15)
      acd205(40)=acd205(21)*acd205(40)
      acd205(47)=-acd205(36)*acd205(12)
      acd205(48)=-acd205(37)*acd205(22)
      brack=acd205(38)+acd205(39)+acd205(40)+acd205(41)+acd205(42)+acd205(43)+a&
      &cd205(44)+acd205(45)+acd205(46)+acd205(47)+acd205(48)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd205h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(62) :: acd205
      complex(ki) :: brack
      acd205(1)=k1(iv1)
      acd205(2)=dotproduct(e6,qshift)
      acd205(3)=abb205(5)
      acd205(4)=abb205(4)
      acd205(5)=k2(iv1)
      acd205(6)=dotproduct(qshift,spvak1k5)
      acd205(7)=abb205(16)
      acd205(8)=dotproduct(qshift,spvak4k2)
      acd205(9)=abb205(25)
      acd205(10)=dotproduct(qshift,spvak4k5)
      acd205(11)=abb205(26)
      acd205(12)=dotproduct(qshift,spvae6k2)
      acd205(13)=abb205(11)
      acd205(14)=abb205(18)
      acd205(15)=k6(iv1)
      acd205(16)=abb205(17)
      acd205(17)=e6(iv1)
      acd205(18)=dotproduct(k1,qshift)
      acd205(19)=dotproduct(k2,qshift)
      acd205(20)=abb205(30)
      acd205(21)=abb205(22)
      acd205(22)=dotproduct(qshift,spvak1k2)
      acd205(23)=abb205(14)
      acd205(24)=abb205(13)
      acd205(25)=dotproduct(qshift,spvak6k2)
      acd205(26)=abb205(19)
      acd205(27)=abb205(12)
      acd205(28)=qshift(iv1)
      acd205(29)=abb205(6)
      acd205(30)=abb205(23)
      acd205(31)=abb205(27)
      acd205(32)=abb205(21)
      acd205(33)=abb205(20)
      acd205(34)=spvak1k5(iv1)
      acd205(35)=dotproduct(k6,qshift)
      acd205(36)=dotproduct(qshift,qshift)
      acd205(37)=spvak4k2(iv1)
      acd205(38)=spvak4k5(iv1)
      acd205(39)=abb205(28)
      acd205(40)=abb205(29)
      acd205(41)=dotproduct(qshift,spvak1k6)
      acd205(42)=abb205(7)
      acd205(43)=abb205(24)
      acd205(44)=spvae6k2(iv1)
      acd205(45)=abb205(10)
      acd205(46)=spvak1k2(iv1)
      acd205(47)=spvak6k2(iv1)
      acd205(48)=abb205(8)
      acd205(49)=spvak1k6(iv1)
      acd205(50)=acd205(10)*acd205(23)
      acd205(50)=acd205(50)+acd205(24)
      acd205(51)=acd205(46)*acd205(50)
      acd205(52)=acd205(5)+acd205(1)
      acd205(52)=acd205(3)*acd205(52)
      acd205(53)=acd205(47)*acd205(26)
      acd205(54)=acd205(37)*acd205(21)
      acd205(55)=acd205(34)*acd205(20)
      acd205(56)=acd205(38)*acd205(22)*acd205(23)
      acd205(51)=acd205(56)+acd205(55)+acd205(54)+acd205(53)+acd205(52)+acd205(&
      &51)
      acd205(51)=acd205(2)*acd205(51)
      acd205(52)=-acd205(42)*acd205(41)
      acd205(53)=acd205(25)*acd205(40)
      acd205(54)=-acd205(36)*acd205(31)
      acd205(55)=acd205(12)*acd205(39)
      acd205(56)=acd205(19)*acd205(11)
      acd205(52)=acd205(56)+acd205(55)+acd205(54)+acd205(53)-acd205(43)+acd205(&
      &52)
      acd205(52)=acd205(38)*acd205(52)
      acd205(53)=-acd205(42)*acd205(49)
      acd205(54)=acd205(47)*acd205(40)
      acd205(55)=acd205(44)*acd205(39)
      acd205(56)=2.0_ki*acd205(28)
      acd205(57)=-acd205(31)*acd205(56)
      acd205(58)=acd205(5)*acd205(11)
      acd205(53)=acd205(58)+acd205(57)+acd205(55)+acd205(53)+acd205(54)
      acd205(53)=acd205(10)*acd205(53)
      acd205(50)=acd205(22)*acd205(50)
      acd205(54)=acd205(19)+acd205(18)
      acd205(54)=acd205(3)*acd205(54)
      acd205(55)=acd205(25)*acd205(26)
      acd205(57)=acd205(8)*acd205(21)
      acd205(58)=acd205(6)*acd205(20)
      acd205(50)=acd205(58)+acd205(57)-acd205(27)+acd205(55)+acd205(54)+acd205(&
      &50)
      acd205(50)=acd205(17)*acd205(50)
      acd205(54)=acd205(12)*acd205(13)
      acd205(55)=acd205(8)*acd205(9)
      acd205(57)=acd205(6)*acd205(7)
      acd205(54)=acd205(57)+acd205(54)-acd205(55)
      acd205(55)=-acd205(16)-acd205(54)
      acd205(55)=acd205(15)*acd205(55)
      acd205(57)=-acd205(12)*acd205(32)
      acd205(58)=-acd205(8)*acd205(30)
      acd205(59)=-acd205(6)*acd205(29)
      acd205(57)=acd205(59)+acd205(58)+acd205(33)+acd205(57)
      acd205(56)=acd205(57)*acd205(56)
      acd205(54)=-acd205(14)+acd205(54)
      acd205(54)=acd205(5)*acd205(54)
      acd205(57)=acd205(9)*acd205(37)
      acd205(58)=acd205(7)*acd205(34)
      acd205(57)=acd205(57)-acd205(58)
      acd205(58)=acd205(35)*acd205(57)
      acd205(59)=-acd205(37)*acd205(30)
      acd205(60)=-acd205(34)*acd205(29)
      acd205(59)=acd205(60)+acd205(59)
      acd205(59)=acd205(36)*acd205(59)
      acd205(60)=-acd205(36)*acd205(32)
      acd205(61)=-acd205(13)*acd205(35)
      acd205(60)=acd205(61)-acd205(45)+acd205(60)
      acd205(60)=acd205(44)*acd205(60)
      acd205(61)=acd205(44)*acd205(13)
      acd205(57)=acd205(61)-acd205(57)
      acd205(57)=acd205(19)*acd205(57)
      acd205(61)=-acd205(1)*acd205(4)
      acd205(62)=-acd205(47)*acd205(48)
      brack=acd205(50)+acd205(51)+acd205(52)+acd205(53)+acd205(54)+acd205(55)+a&
      &cd205(56)+acd205(57)+acd205(58)+acd205(59)+acd205(60)+acd205(61)+acd205(&
      &62)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd205h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(60) :: acd205
      complex(ki) :: brack
      acd205(1)=d(iv1,iv2)
      acd205(2)=dotproduct(qshift,spvak1k5)
      acd205(3)=abb205(6)
      acd205(4)=dotproduct(qshift,spvak4k2)
      acd205(5)=abb205(23)
      acd205(6)=dotproduct(qshift,spvak4k5)
      acd205(7)=abb205(27)
      acd205(8)=dotproduct(qshift,spvae6k2)
      acd205(9)=abb205(21)
      acd205(10)=abb205(20)
      acd205(11)=k1(iv1)
      acd205(12)=e6(iv2)
      acd205(13)=abb205(5)
      acd205(14)=k1(iv2)
      acd205(15)=e6(iv1)
      acd205(16)=k2(iv1)
      acd205(17)=spvak1k5(iv2)
      acd205(18)=abb205(16)
      acd205(19)=spvak4k2(iv2)
      acd205(20)=abb205(25)
      acd205(21)=spvak4k5(iv2)
      acd205(22)=abb205(26)
      acd205(23)=spvae6k2(iv2)
      acd205(24)=abb205(11)
      acd205(25)=k2(iv2)
      acd205(26)=spvak1k5(iv1)
      acd205(27)=spvak4k2(iv1)
      acd205(28)=spvak4k5(iv1)
      acd205(29)=spvae6k2(iv1)
      acd205(30)=k6(iv1)
      acd205(31)=k6(iv2)
      acd205(32)=abb205(30)
      acd205(33)=abb205(22)
      acd205(34)=dotproduct(qshift,spvak1k2)
      acd205(35)=abb205(14)
      acd205(36)=spvak1k2(iv2)
      acd205(37)=abb205(13)
      acd205(38)=spvak6k2(iv2)
      acd205(39)=abb205(19)
      acd205(40)=spvak1k2(iv1)
      acd205(41)=spvak6k2(iv1)
      acd205(42)=qshift(iv1)
      acd205(43)=qshift(iv2)
      acd205(44)=abb205(28)
      acd205(45)=dotproduct(e6,qshift)
      acd205(46)=abb205(29)
      acd205(47)=spvak1k6(iv2)
      acd205(48)=abb205(7)
      acd205(49)=spvak1k6(iv1)
      acd205(50)=-acd205(48)*acd205(47)
      acd205(51)=acd205(38)*acd205(46)
      acd205(52)=2.0_ki*acd205(7)
      acd205(52)=-acd205(43)*acd205(52)
      acd205(53)=acd205(23)*acd205(44)
      acd205(54)=acd205(25)*acd205(22)
      acd205(55)=acd205(35)*acd205(45)
      acd205(56)=acd205(36)*acd205(55)
      acd205(50)=acd205(56)+acd205(54)+acd205(53)+acd205(52)+acd205(50)+acd205(&
      &51)
      acd205(50)=acd205(28)*acd205(50)
      acd205(51)=-acd205(48)*acd205(49)
      acd205(52)=acd205(41)*acd205(46)
      acd205(53)=2.0_ki*acd205(42)
      acd205(54)=-acd205(7)*acd205(53)
      acd205(56)=acd205(29)*acd205(44)
      acd205(57)=acd205(16)*acd205(22)
      acd205(55)=acd205(40)*acd205(55)
      acd205(51)=acd205(55)+acd205(57)+acd205(56)+acd205(54)+acd205(51)+acd205(&
      &52)
      acd205(51)=acd205(21)*acd205(51)
      acd205(52)=acd205(35)*acd205(6)
      acd205(52)=acd205(52)+acd205(37)
      acd205(54)=acd205(36)*acd205(52)
      acd205(55)=acd205(25)+acd205(14)
      acd205(55)=acd205(13)*acd205(55)
      acd205(56)=acd205(38)*acd205(39)
      acd205(57)=acd205(19)*acd205(33)
      acd205(58)=acd205(17)*acd205(32)
      acd205(59)=acd205(35)*acd205(34)
      acd205(60)=acd205(21)*acd205(59)
      acd205(54)=acd205(60)+acd205(58)+acd205(57)+acd205(56)+acd205(55)+acd205(&
      &54)
      acd205(54)=acd205(15)*acd205(54)
      acd205(52)=acd205(40)*acd205(52)
      acd205(55)=acd205(16)+acd205(11)
      acd205(55)=acd205(13)*acd205(55)
      acd205(56)=acd205(39)*acd205(41)
      acd205(57)=acd205(27)*acd205(33)
      acd205(58)=acd205(26)*acd205(32)
      acd205(59)=acd205(28)*acd205(59)
      acd205(52)=acd205(59)+acd205(58)+acd205(57)+acd205(56)+acd205(55)+acd205(&
      &52)
      acd205(52)=acd205(12)*acd205(52)
      acd205(55)=-acd205(9)*acd205(8)
      acd205(56)=-acd205(6)*acd205(7)
      acd205(57)=-acd205(5)*acd205(4)
      acd205(58)=-acd205(3)*acd205(2)
      acd205(55)=acd205(58)+acd205(57)+acd205(56)+acd205(10)+acd205(55)
      acd205(55)=acd205(1)*acd205(55)
      acd205(56)=-acd205(29)*acd205(9)
      acd205(57)=-acd205(27)*acd205(5)
      acd205(58)=-acd205(26)*acd205(3)
      acd205(56)=acd205(58)+acd205(56)+acd205(57)
      acd205(56)=acd205(43)*acd205(56)
      acd205(55)=acd205(56)+acd205(55)
      acd205(56)=acd205(24)*acd205(29)
      acd205(57)=acd205(20)*acd205(27)
      acd205(58)=acd205(18)*acd205(26)
      acd205(56)=acd205(58)+acd205(56)-acd205(57)
      acd205(57)=acd205(25)-acd205(31)
      acd205(56)=acd205(56)*acd205(57)
      acd205(57)=acd205(23)*acd205(24)
      acd205(58)=-acd205(19)*acd205(20)
      acd205(59)=acd205(17)*acd205(18)
      acd205(57)=acd205(59)+acd205(57)+acd205(58)
      acd205(57)=acd205(16)*acd205(57)
      acd205(58)=-acd205(9)*acd205(53)
      acd205(59)=-acd205(24)*acd205(30)
      acd205(58)=acd205(58)+acd205(59)
      acd205(58)=acd205(23)*acd205(58)
      acd205(59)=-acd205(5)*acd205(53)
      acd205(60)=acd205(20)*acd205(30)
      acd205(59)=acd205(59)+acd205(60)
      acd205(59)=acd205(19)*acd205(59)
      acd205(53)=-acd205(3)*acd205(53)
      acd205(60)=-acd205(18)*acd205(30)
      acd205(53)=acd205(53)+acd205(60)
      acd205(53)=acd205(17)*acd205(53)
      brack=acd205(50)+acd205(51)+acd205(52)+acd205(53)+acd205(54)+2.0_ki*acd20&
      &5(55)+acd205(56)+acd205(57)+acd205(58)+acd205(59)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd205h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(32) :: acd205
      complex(ki) :: brack
      acd205(1)=d(iv1,iv2)
      acd205(2)=spvak1k5(iv3)
      acd205(3)=abb205(6)
      acd205(4)=spvak4k2(iv3)
      acd205(5)=abb205(23)
      acd205(6)=spvak4k5(iv3)
      acd205(7)=abb205(27)
      acd205(8)=spvae6k2(iv3)
      acd205(9)=abb205(21)
      acd205(10)=d(iv1,iv3)
      acd205(11)=spvak1k5(iv2)
      acd205(12)=spvak4k2(iv2)
      acd205(13)=spvak4k5(iv2)
      acd205(14)=spvae6k2(iv2)
      acd205(15)=d(iv2,iv3)
      acd205(16)=spvak1k5(iv1)
      acd205(17)=spvak4k2(iv1)
      acd205(18)=spvak4k5(iv1)
      acd205(19)=spvae6k2(iv1)
      acd205(20)=e6(iv1)
      acd205(21)=spvak1k2(iv3)
      acd205(22)=abb205(14)
      acd205(23)=spvak1k2(iv2)
      acd205(24)=e6(iv2)
      acd205(25)=spvak1k2(iv1)
      acd205(26)=e6(iv3)
      acd205(27)=-acd205(9)*acd205(19)
      acd205(28)=-acd205(7)*acd205(18)
      acd205(29)=-acd205(5)*acd205(17)
      acd205(30)=-acd205(3)*acd205(16)
      acd205(27)=acd205(30)+acd205(29)+acd205(27)+acd205(28)
      acd205(27)=acd205(15)*acd205(27)
      acd205(28)=-acd205(9)*acd205(14)
      acd205(29)=-acd205(7)*acd205(13)
      acd205(30)=-acd205(5)*acd205(12)
      acd205(31)=-acd205(3)*acd205(11)
      acd205(28)=acd205(31)+acd205(30)+acd205(28)+acd205(29)
      acd205(28)=acd205(10)*acd205(28)
      acd205(29)=-acd205(9)*acd205(8)
      acd205(30)=-acd205(6)*acd205(7)
      acd205(31)=-acd205(5)*acd205(4)
      acd205(32)=-acd205(3)*acd205(2)
      acd205(29)=acd205(32)+acd205(31)+acd205(29)+acd205(30)
      acd205(29)=acd205(1)*acd205(29)
      acd205(27)=acd205(29)+acd205(27)+acd205(28)
      acd205(28)=acd205(23)*acd205(26)
      acd205(29)=acd205(21)*acd205(24)
      acd205(28)=acd205(28)+acd205(29)
      acd205(28)=acd205(18)*acd205(28)
      acd205(29)=acd205(25)*acd205(26)
      acd205(30)=acd205(20)*acd205(21)
      acd205(29)=acd205(29)+acd205(30)
      acd205(29)=acd205(13)*acd205(29)
      acd205(30)=acd205(24)*acd205(25)
      acd205(31)=acd205(20)*acd205(23)
      acd205(30)=acd205(30)+acd205(31)
      acd205(30)=acd205(6)*acd205(30)
      acd205(28)=acd205(30)+acd205(28)+acd205(29)
      acd205(28)=acd205(22)*acd205(28)
      brack=2.0_ki*acd205(27)+acd205(28)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd205h2
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
!---#[ subroutine reconstruct_d205:
   subroutine     reconstruct_d205(coeffs)
      use p8_cbarc_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_205:
      coeffs%coeffs_205%c0 = derivative(czip)
      coeffs%coeffs_205%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_205%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_205%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_205%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_205%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_205%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_205%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_205%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_205%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_205%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_205%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_205%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_205%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_205%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_205%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_205%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_205%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_205%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_205%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_205%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_205%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_205%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_205%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_205%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_205%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_205%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_205%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_205%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_205%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_205%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_205%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_205%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_205%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_205%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_205:
   end subroutine reconstruct_d205
!---#] subroutine reconstruct_d205:
end module     p8_cbarc_hepemg_d205h2l1d
