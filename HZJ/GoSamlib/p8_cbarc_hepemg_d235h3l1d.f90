module     p8_cbarc_hepemg_d235h3l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity3d235h3l1d.f90
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
   public :: derivative , reconstruct_d235
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd235h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(46) :: acd235
      complex(ki) :: brack
      acd235(1)=dotproduct(k1,qshift)
      acd235(2)=dotproduct(e6,qshift)
      acd235(3)=abb235(23)
      acd235(4)=dotproduct(qshift,spvak2e6)
      acd235(5)=abb235(26)
      acd235(6)=abb235(12)
      acd235(7)=dotproduct(k2,qshift)
      acd235(8)=dotproduct(qshift,spvak2k5)
      acd235(9)=abb235(24)
      acd235(10)=dotproduct(qshift,spvak4k5)
      acd235(11)=abb235(28)
      acd235(12)=abb235(22)
      acd235(13)=dotproduct(k6,qshift)
      acd235(14)=abb235(20)
      acd235(15)=abb235(19)
      acd235(16)=dotproduct(qshift,qshift)
      acd235(17)=abb235(6)
      acd235(18)=abb235(4)
      acd235(19)=dotproduct(qshift,spvak2k1)
      acd235(20)=abb235(29)
      acd235(21)=abb235(15)
      acd235(22)=abb235(5)
      acd235(23)=dotproduct(qshift,spvak2k6)
      acd235(24)=abb235(10)
      acd235(25)=dotproduct(qshift,spvak4k1)
      acd235(26)=abb235(14)
      acd235(27)=abb235(13)
      acd235(28)=abb235(25)
      acd235(29)=abb235(21)
      acd235(30)=abb235(27)
      acd235(31)=abb235(7)
      acd235(32)=abb235(17)
      acd235(33)=abb235(11)
      acd235(34)=abb235(18)
      acd235(35)=abb235(8)
      acd235(36)=abb235(3)
      acd235(37)=abb235(9)
      acd235(38)=acd235(25)*acd235(26)
      acd235(39)=acd235(19)*acd235(22)
      acd235(40)=acd235(23)*acd235(24)
      acd235(41)=acd235(1)*acd235(3)
      acd235(42)=acd235(13)*acd235(14)
      acd235(43)=acd235(8)*acd235(18)
      acd235(44)=-acd235(16)*acd235(17)
      acd235(45)=acd235(19)*acd235(20)
      acd235(45)=acd235(21)+acd235(45)
      acd235(45)=acd235(10)*acd235(45)
      acd235(38)=acd235(45)+acd235(44)+acd235(43)+acd235(42)+acd235(41)+acd235(&
      &40)+acd235(39)-acd235(27)+acd235(38)
      acd235(38)=acd235(2)*acd235(38)
      acd235(39)=acd235(13)+acd235(7)
      acd235(39)=acd235(11)*acd235(39)
      acd235(40)=acd235(23)*acd235(34)
      acd235(41)=acd235(4)*acd235(32)
      acd235(42)=acd235(16)*acd235(30)
      acd235(39)=acd235(42)+acd235(41)-acd235(35)+acd235(40)+acd235(39)
      acd235(39)=acd235(10)*acd235(39)
      acd235(40)=-acd235(8)*acd235(29)
      acd235(41)=acd235(4)*acd235(28)
      acd235(40)=acd235(41)+acd235(31)+acd235(40)
      acd235(40)=acd235(16)*acd235(40)
      acd235(41)=-acd235(23)*acd235(36)
      acd235(42)=-acd235(7)*acd235(12)
      acd235(43)=acd235(1)*acd235(6)
      acd235(44)=-acd235(13)*acd235(15)
      acd235(45)=-acd235(7)+acd235(13)
      acd235(45)=acd235(8)*acd235(9)*acd235(45)
      acd235(46)=acd235(1)*acd235(5)
      acd235(46)=-acd235(33)+acd235(46)
      acd235(46)=acd235(4)*acd235(46)
      brack=acd235(37)+acd235(38)+acd235(39)+acd235(40)+acd235(41)+acd235(42)+a&
      &cd235(43)+acd235(44)+acd235(45)+acd235(46)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd235h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(59) :: acd235
      complex(ki) :: brack
      acd235(1)=k1(iv1)
      acd235(2)=dotproduct(e6,qshift)
      acd235(3)=abb235(23)
      acd235(4)=dotproduct(qshift,spvak2e6)
      acd235(5)=abb235(26)
      acd235(6)=abb235(12)
      acd235(7)=k2(iv1)
      acd235(8)=dotproduct(qshift,spvak2k5)
      acd235(9)=abb235(24)
      acd235(10)=dotproduct(qshift,spvak4k5)
      acd235(11)=abb235(28)
      acd235(12)=abb235(22)
      acd235(13)=k6(iv1)
      acd235(14)=abb235(20)
      acd235(15)=abb235(19)
      acd235(16)=e6(iv1)
      acd235(17)=dotproduct(k1,qshift)
      acd235(18)=dotproduct(k6,qshift)
      acd235(19)=dotproduct(qshift,qshift)
      acd235(20)=abb235(6)
      acd235(21)=abb235(4)
      acd235(22)=dotproduct(qshift,spvak2k1)
      acd235(23)=abb235(29)
      acd235(24)=abb235(15)
      acd235(25)=abb235(5)
      acd235(26)=dotproduct(qshift,spvak2k6)
      acd235(27)=abb235(10)
      acd235(28)=dotproduct(qshift,spvak4k1)
      acd235(29)=abb235(14)
      acd235(30)=abb235(13)
      acd235(31)=qshift(iv1)
      acd235(32)=abb235(25)
      acd235(33)=abb235(21)
      acd235(34)=abb235(27)
      acd235(35)=abb235(7)
      acd235(36)=spvak2e6(iv1)
      acd235(37)=abb235(17)
      acd235(38)=abb235(11)
      acd235(39)=spvak2k5(iv1)
      acd235(40)=dotproduct(k2,qshift)
      acd235(41)=spvak4k5(iv1)
      acd235(42)=abb235(18)
      acd235(43)=abb235(8)
      acd235(44)=spvak2k1(iv1)
      acd235(45)=spvak2k6(iv1)
      acd235(46)=abb235(3)
      acd235(47)=spvak4k1(iv1)
      acd235(48)=-acd235(29)*acd235(28)
      acd235(49)=-acd235(26)*acd235(27)
      acd235(50)=-acd235(3)*acd235(17)
      acd235(51)=-acd235(22)*acd235(25)
      acd235(52)=-acd235(18)*acd235(14)
      acd235(53)=acd235(19)*acd235(20)
      acd235(54)=-acd235(8)*acd235(21)
      acd235(55)=acd235(22)*acd235(23)
      acd235(55)=acd235(55)+acd235(24)
      acd235(56)=-acd235(10)*acd235(55)
      acd235(48)=acd235(56)+acd235(54)+acd235(53)+acd235(52)+acd235(51)+acd235(&
      &50)+acd235(49)+acd235(30)+acd235(48)
      acd235(48)=acd235(16)*acd235(48)
      acd235(49)=-acd235(10)*acd235(23)
      acd235(49)=acd235(49)-acd235(25)
      acd235(49)=acd235(44)*acd235(49)
      acd235(50)=-acd235(29)*acd235(47)
      acd235(51)=-acd235(45)*acd235(27)
      acd235(52)=-acd235(1)*acd235(3)
      acd235(53)=-acd235(39)*acd235(21)
      acd235(54)=-acd235(13)*acd235(14)
      acd235(56)=2.0_ki*acd235(31)
      acd235(57)=acd235(20)*acd235(56)
      acd235(55)=-acd235(41)*acd235(55)
      acd235(49)=acd235(55)+acd235(57)+acd235(54)+acd235(53)+acd235(52)+acd235(&
      &51)+acd235(50)+acd235(49)
      acd235(49)=acd235(2)*acd235(49)
      acd235(50)=-acd235(26)*acd235(42)
      acd235(51)=-acd235(4)*acd235(37)
      acd235(52)=-acd235(19)*acd235(34)
      acd235(53)=-acd235(40)-acd235(18)
      acd235(53)=acd235(11)*acd235(53)
      acd235(50)=acd235(53)+acd235(52)+acd235(51)+acd235(43)+acd235(50)
      acd235(50)=acd235(41)*acd235(50)
      acd235(51)=-acd235(45)*acd235(42)
      acd235(52)=-acd235(36)*acd235(37)
      acd235(53)=-acd235(7)-acd235(13)
      acd235(53)=acd235(11)*acd235(53)
      acd235(54)=-acd235(34)*acd235(56)
      acd235(51)=acd235(54)+acd235(53)+acd235(51)+acd235(52)
      acd235(51)=acd235(10)*acd235(51)
      acd235(52)=acd235(40)-acd235(18)
      acd235(52)=acd235(39)*acd235(52)
      acd235(53)=acd235(7)-acd235(13)
      acd235(53)=acd235(8)*acd235(53)
      acd235(52)=acd235(53)+acd235(52)
      acd235(52)=acd235(9)*acd235(52)
      acd235(53)=acd235(39)*acd235(33)
      acd235(54)=-acd235(36)*acd235(32)
      acd235(53)=acd235(53)+acd235(54)
      acd235(53)=acd235(19)*acd235(53)
      acd235(54)=-acd235(4)*acd235(32)
      acd235(55)=acd235(8)*acd235(33)
      acd235(54)=acd235(55)-acd235(35)+acd235(54)
      acd235(54)=acd235(54)*acd235(56)
      acd235(55)=acd235(45)*acd235(46)
      acd235(56)=acd235(7)*acd235(12)
      acd235(57)=-acd235(4)*acd235(5)
      acd235(57)=-acd235(6)+acd235(57)
      acd235(57)=acd235(1)*acd235(57)
      acd235(58)=-acd235(5)*acd235(17)
      acd235(58)=acd235(38)+acd235(58)
      acd235(58)=acd235(36)*acd235(58)
      acd235(59)=acd235(13)*acd235(15)
      brack=acd235(48)+acd235(49)+acd235(50)+acd235(51)+acd235(52)+acd235(53)+a&
      &cd235(54)+acd235(55)+acd235(56)+acd235(57)+acd235(58)+acd235(59)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd235h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(57) :: acd235
      complex(ki) :: brack
      acd235(1)=d(iv1,iv2)
      acd235(2)=dotproduct(e6,qshift)
      acd235(3)=abb235(6)
      acd235(4)=dotproduct(qshift,spvak2k5)
      acd235(5)=abb235(21)
      acd235(6)=dotproduct(qshift,spvak4k5)
      acd235(7)=abb235(27)
      acd235(8)=dotproduct(qshift,spvak2e6)
      acd235(9)=abb235(25)
      acd235(10)=abb235(7)
      acd235(11)=k1(iv1)
      acd235(12)=e6(iv2)
      acd235(13)=abb235(23)
      acd235(14)=spvak2e6(iv2)
      acd235(15)=abb235(26)
      acd235(16)=k1(iv2)
      acd235(17)=e6(iv1)
      acd235(18)=spvak2e6(iv1)
      acd235(19)=k2(iv1)
      acd235(20)=spvak2k5(iv2)
      acd235(21)=abb235(24)
      acd235(22)=spvak4k5(iv2)
      acd235(23)=abb235(28)
      acd235(24)=k2(iv2)
      acd235(25)=spvak2k5(iv1)
      acd235(26)=spvak4k5(iv1)
      acd235(27)=k6(iv1)
      acd235(28)=abb235(20)
      acd235(29)=k6(iv2)
      acd235(30)=qshift(iv2)
      acd235(31)=abb235(4)
      acd235(32)=dotproduct(qshift,spvak2k1)
      acd235(33)=abb235(29)
      acd235(34)=abb235(15)
      acd235(35)=spvak2k1(iv2)
      acd235(36)=abb235(5)
      acd235(37)=spvak2k6(iv2)
      acd235(38)=abb235(10)
      acd235(39)=spvak4k1(iv2)
      acd235(40)=abb235(14)
      acd235(41)=qshift(iv1)
      acd235(42)=spvak2k1(iv1)
      acd235(43)=spvak2k6(iv1)
      acd235(44)=spvak4k1(iv1)
      acd235(45)=abb235(17)
      acd235(46)=abb235(18)
      acd235(47)=acd235(33)*acd235(6)
      acd235(47)=acd235(47)+acd235(36)
      acd235(48)=acd235(35)*acd235(47)
      acd235(49)=acd235(40)*acd235(39)
      acd235(50)=acd235(37)*acd235(38)
      acd235(51)=acd235(13)*acd235(16)
      acd235(52)=acd235(29)*acd235(28)
      acd235(53)=2.0_ki*acd235(30)
      acd235(54)=-acd235(3)*acd235(53)
      acd235(55)=acd235(20)*acd235(31)
      acd235(56)=acd235(33)*acd235(32)
      acd235(56)=acd235(56)+acd235(34)
      acd235(57)=acd235(22)*acd235(56)
      acd235(48)=acd235(57)+acd235(55)+acd235(54)+acd235(52)+acd235(51)+acd235(&
      &49)+acd235(50)+acd235(48)
      acd235(48)=acd235(17)*acd235(48)
      acd235(47)=acd235(42)*acd235(47)
      acd235(49)=acd235(40)*acd235(44)
      acd235(50)=acd235(38)*acd235(43)
      acd235(51)=acd235(11)*acd235(13)
      acd235(52)=acd235(27)*acd235(28)
      acd235(54)=2.0_ki*acd235(41)
      acd235(55)=-acd235(3)*acd235(54)
      acd235(57)=acd235(25)*acd235(31)
      acd235(56)=acd235(26)*acd235(56)
      acd235(47)=acd235(56)+acd235(57)+acd235(55)+acd235(52)+acd235(51)+acd235(&
      &49)+acd235(50)+acd235(47)
      acd235(47)=acd235(12)*acd235(47)
      acd235(49)=acd235(37)*acd235(46)
      acd235(50)=acd235(14)*acd235(45)
      acd235(51)=acd235(7)*acd235(53)
      acd235(52)=acd235(24)+acd235(29)
      acd235(52)=acd235(23)*acd235(52)
      acd235(55)=acd235(33)*acd235(2)
      acd235(56)=acd235(35)*acd235(55)
      acd235(49)=acd235(56)+acd235(52)+acd235(51)+acd235(49)+acd235(50)
      acd235(49)=acd235(26)*acd235(49)
      acd235(50)=acd235(43)*acd235(46)
      acd235(51)=acd235(18)*acd235(45)
      acd235(52)=acd235(7)*acd235(54)
      acd235(56)=acd235(19)+acd235(27)
      acd235(56)=acd235(23)*acd235(56)
      acd235(55)=acd235(42)*acd235(55)
      acd235(50)=acd235(55)+acd235(56)+acd235(52)+acd235(50)+acd235(51)
      acd235(50)=acd235(22)*acd235(50)
      acd235(51)=acd235(9)*acd235(8)
      acd235(52)=acd235(6)*acd235(7)
      acd235(55)=-acd235(5)*acd235(4)
      acd235(56)=-acd235(2)*acd235(3)
      acd235(51)=acd235(56)+acd235(55)+acd235(52)+acd235(10)+acd235(51)
      acd235(51)=acd235(1)*acd235(51)
      acd235(52)=acd235(11)*acd235(15)
      acd235(55)=acd235(9)*acd235(54)
      acd235(52)=acd235(55)+acd235(52)
      acd235(52)=acd235(14)*acd235(52)
      acd235(55)=acd235(9)*acd235(18)
      acd235(56)=-acd235(25)*acd235(5)
      acd235(55)=acd235(56)+acd235(55)
      acd235(53)=acd235(53)*acd235(55)
      acd235(54)=-acd235(5)*acd235(54)
      acd235(55)=-acd235(19)+acd235(27)
      acd235(55)=acd235(21)*acd235(55)
      acd235(54)=acd235(54)+acd235(55)
      acd235(54)=acd235(20)*acd235(54)
      acd235(55)=acd235(18)*acd235(15)*acd235(16)
      acd235(56)=-acd235(24)+acd235(29)
      acd235(56)=acd235(21)*acd235(25)*acd235(56)
      brack=acd235(47)+acd235(48)+acd235(49)+acd235(50)+2.0_ki*acd235(51)+acd23&
      &5(52)+acd235(53)+acd235(54)+acd235(55)+acd235(56)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd235h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(29) :: acd235
      complex(ki) :: brack
      acd235(1)=d(iv1,iv2)
      acd235(2)=e6(iv3)
      acd235(3)=abb235(6)
      acd235(4)=spvak2k5(iv3)
      acd235(5)=abb235(21)
      acd235(6)=spvak4k5(iv3)
      acd235(7)=abb235(27)
      acd235(8)=spvak2e6(iv3)
      acd235(9)=abb235(25)
      acd235(10)=d(iv1,iv3)
      acd235(11)=e6(iv2)
      acd235(12)=spvak2k5(iv2)
      acd235(13)=spvak4k5(iv2)
      acd235(14)=spvak2e6(iv2)
      acd235(15)=d(iv2,iv3)
      acd235(16)=e6(iv1)
      acd235(17)=spvak2k5(iv1)
      acd235(18)=spvak4k5(iv1)
      acd235(19)=spvak2e6(iv1)
      acd235(20)=spvak2k1(iv3)
      acd235(21)=abb235(29)
      acd235(22)=spvak2k1(iv2)
      acd235(23)=spvak2k1(iv1)
      acd235(24)=-acd235(9)*acd235(19)
      acd235(25)=-acd235(7)*acd235(18)
      acd235(26)=acd235(5)*acd235(17)
      acd235(27)=acd235(3)*acd235(16)
      acd235(24)=acd235(27)+acd235(26)+acd235(24)+acd235(25)
      acd235(24)=acd235(15)*acd235(24)
      acd235(25)=-acd235(9)*acd235(14)
      acd235(26)=-acd235(7)*acd235(13)
      acd235(27)=acd235(5)*acd235(12)
      acd235(28)=acd235(3)*acd235(11)
      acd235(25)=acd235(28)+acd235(27)+acd235(25)+acd235(26)
      acd235(25)=acd235(10)*acd235(25)
      acd235(26)=-acd235(9)*acd235(8)
      acd235(27)=-acd235(6)*acd235(7)
      acd235(28)=acd235(5)*acd235(4)
      acd235(29)=acd235(2)*acd235(3)
      acd235(26)=acd235(29)+acd235(28)+acd235(26)+acd235(27)
      acd235(26)=acd235(1)*acd235(26)
      acd235(24)=acd235(26)+acd235(24)+acd235(25)
      acd235(25)=-acd235(13)*acd235(16)
      acd235(26)=-acd235(11)*acd235(18)
      acd235(25)=acd235(25)+acd235(26)
      acd235(25)=acd235(20)*acd235(25)
      acd235(26)=-acd235(16)*acd235(22)
      acd235(27)=-acd235(11)*acd235(23)
      acd235(26)=acd235(26)+acd235(27)
      acd235(26)=acd235(6)*acd235(26)
      acd235(27)=-acd235(18)*acd235(22)
      acd235(28)=-acd235(13)*acd235(23)
      acd235(27)=acd235(27)+acd235(28)
      acd235(27)=acd235(2)*acd235(27)
      acd235(25)=acd235(27)+acd235(26)+acd235(25)
      acd235(25)=acd235(21)*acd235(25)
      brack=2.0_ki*acd235(24)+acd235(25)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd235h3
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
      qshift = k6
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
!---#[ subroutine reconstruct_d235:
   subroutine     reconstruct_d235(coeffs)
      use p8_cbarc_hepemg_groups, only: tensrec_info_group0
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group0), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_235:
      coeffs%coeffs_235%c0 = derivative(czip)
      coeffs%coeffs_235%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_235%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_235%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_235%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_235%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_235%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_235%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_235%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_235%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_235%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_235%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_235%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_235%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_235%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_235%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_235%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_235%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_235%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_235%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_235%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_235%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_235%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_235%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_235%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_235%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_235%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_235%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_235%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_235%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_235%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_235%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_235%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_235%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_235%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_235:
   end subroutine reconstruct_d235
!---#] subroutine reconstruct_d235:
end module     p8_cbarc_hepemg_d235h3l1d
