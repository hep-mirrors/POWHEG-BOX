module     p12_sbars_hepemg_d206h0l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p12_s &
   ! &bars_hepemg/helicity0d206h0l1d.f90
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
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d206
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd206h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(46) :: acd206
      complex(ki) :: brack
      acd206(1)=dotproduct(k1,qshift)
      acd206(2)=dotproduct(e6,qshift)
      acd206(3)=abb206(23)
      acd206(4)=dotproduct(qshift,spvae6k2)
      acd206(5)=abb206(26)
      acd206(6)=abb206(12)
      acd206(7)=dotproduct(k2,qshift)
      acd206(8)=dotproduct(qshift,spvak5k2)
      acd206(9)=abb206(24)
      acd206(10)=dotproduct(qshift,spvak5k4)
      acd206(11)=abb206(28)
      acd206(12)=abb206(22)
      acd206(13)=dotproduct(k6,qshift)
      acd206(14)=abb206(20)
      acd206(15)=abb206(19)
      acd206(16)=dotproduct(qshift,qshift)
      acd206(17)=abb206(6)
      acd206(18)=abb206(16)
      acd206(19)=dotproduct(qshift,spvak1k2)
      acd206(20)=abb206(4)
      acd206(21)=abb206(15)
      acd206(22)=abb206(5)
      acd206(23)=dotproduct(qshift,spvak1k4)
      acd206(24)=abb206(14)
      acd206(25)=dotproduct(qshift,spvak6k2)
      acd206(26)=abb206(10)
      acd206(27)=abb206(13)
      acd206(28)=abb206(25)
      acd206(29)=abb206(21)
      acd206(30)=abb206(27)
      acd206(31)=abb206(7)
      acd206(32)=abb206(17)
      acd206(33)=abb206(11)
      acd206(34)=abb206(18)
      acd206(35)=abb206(8)
      acd206(36)=abb206(3)
      acd206(37)=abb206(9)
      acd206(38)=acd206(23)*acd206(24)
      acd206(39)=acd206(19)*acd206(22)
      acd206(40)=acd206(25)*acd206(26)
      acd206(41)=acd206(1)*acd206(3)
      acd206(42)=acd206(13)*acd206(14)
      acd206(43)=acd206(8)*acd206(18)
      acd206(44)=-acd206(16)*acd206(17)
      acd206(45)=acd206(19)*acd206(20)
      acd206(45)=acd206(21)+acd206(45)
      acd206(45)=acd206(10)*acd206(45)
      acd206(38)=acd206(45)+acd206(44)+acd206(43)+acd206(42)+acd206(41)+acd206(&
      &40)+acd206(39)-acd206(27)+acd206(38)
      acd206(38)=acd206(2)*acd206(38)
      acd206(39)=acd206(13)+acd206(7)
      acd206(39)=acd206(11)*acd206(39)
      acd206(40)=acd206(25)*acd206(34)
      acd206(41)=acd206(4)*acd206(32)
      acd206(42)=acd206(16)*acd206(30)
      acd206(39)=acd206(42)+acd206(41)-acd206(35)+acd206(40)+acd206(39)
      acd206(39)=acd206(10)*acd206(39)
      acd206(40)=-acd206(8)*acd206(29)
      acd206(41)=acd206(4)*acd206(28)
      acd206(40)=acd206(41)+acd206(31)+acd206(40)
      acd206(40)=acd206(16)*acd206(40)
      acd206(41)=-acd206(25)*acd206(36)
      acd206(42)=-acd206(7)*acd206(12)
      acd206(43)=acd206(1)*acd206(6)
      acd206(44)=-acd206(13)*acd206(15)
      acd206(45)=-acd206(7)+acd206(13)
      acd206(45)=acd206(8)*acd206(9)*acd206(45)
      acd206(46)=acd206(1)*acd206(5)
      acd206(46)=-acd206(33)+acd206(46)
      acd206(46)=acd206(4)*acd206(46)
      brack=acd206(37)+acd206(38)+acd206(39)+acd206(40)+acd206(41)+acd206(42)+a&
      &cd206(43)+acd206(44)+acd206(45)+acd206(46)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd206h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(59) :: acd206
      complex(ki) :: brack
      acd206(1)=k1(iv1)
      acd206(2)=dotproduct(e6,qshift)
      acd206(3)=abb206(23)
      acd206(4)=dotproduct(qshift,spvae6k2)
      acd206(5)=abb206(26)
      acd206(6)=abb206(12)
      acd206(7)=k2(iv1)
      acd206(8)=dotproduct(qshift,spvak5k2)
      acd206(9)=abb206(24)
      acd206(10)=dotproduct(qshift,spvak5k4)
      acd206(11)=abb206(28)
      acd206(12)=abb206(22)
      acd206(13)=k6(iv1)
      acd206(14)=abb206(20)
      acd206(15)=abb206(19)
      acd206(16)=e6(iv1)
      acd206(17)=dotproduct(k1,qshift)
      acd206(18)=dotproduct(k6,qshift)
      acd206(19)=dotproduct(qshift,qshift)
      acd206(20)=abb206(6)
      acd206(21)=abb206(16)
      acd206(22)=dotproduct(qshift,spvak1k2)
      acd206(23)=abb206(4)
      acd206(24)=abb206(15)
      acd206(25)=abb206(5)
      acd206(26)=dotproduct(qshift,spvak1k4)
      acd206(27)=abb206(14)
      acd206(28)=dotproduct(qshift,spvak6k2)
      acd206(29)=abb206(10)
      acd206(30)=abb206(13)
      acd206(31)=qshift(iv1)
      acd206(32)=abb206(25)
      acd206(33)=abb206(21)
      acd206(34)=abb206(27)
      acd206(35)=abb206(7)
      acd206(36)=spvae6k2(iv1)
      acd206(37)=abb206(17)
      acd206(38)=abb206(11)
      acd206(39)=spvak5k2(iv1)
      acd206(40)=dotproduct(k2,qshift)
      acd206(41)=spvak5k4(iv1)
      acd206(42)=abb206(18)
      acd206(43)=abb206(8)
      acd206(44)=spvak1k2(iv1)
      acd206(45)=spvak1k4(iv1)
      acd206(46)=spvak6k2(iv1)
      acd206(47)=abb206(3)
      acd206(48)=-acd206(28)*acd206(29)
      acd206(49)=-acd206(27)*acd206(26)
      acd206(50)=-acd206(3)*acd206(17)
      acd206(51)=-acd206(22)*acd206(25)
      acd206(52)=-acd206(18)*acd206(14)
      acd206(53)=acd206(19)*acd206(20)
      acd206(54)=-acd206(8)*acd206(21)
      acd206(55)=acd206(22)*acd206(23)
      acd206(55)=acd206(55)+acd206(24)
      acd206(56)=-acd206(10)*acd206(55)
      acd206(48)=acd206(56)+acd206(54)+acd206(53)+acd206(52)+acd206(51)+acd206(&
      &50)+acd206(49)+acd206(30)+acd206(48)
      acd206(48)=acd206(16)*acd206(48)
      acd206(49)=-acd206(10)*acd206(23)
      acd206(49)=acd206(49)-acd206(25)
      acd206(49)=acd206(44)*acd206(49)
      acd206(50)=-acd206(27)*acd206(45)
      acd206(51)=-acd206(46)*acd206(29)
      acd206(52)=-acd206(1)*acd206(3)
      acd206(53)=-acd206(39)*acd206(21)
      acd206(54)=-acd206(13)*acd206(14)
      acd206(56)=2.0_ki*acd206(31)
      acd206(57)=acd206(20)*acd206(56)
      acd206(55)=-acd206(41)*acd206(55)
      acd206(49)=acd206(55)+acd206(57)+acd206(54)+acd206(53)+acd206(52)+acd206(&
      &51)+acd206(50)+acd206(49)
      acd206(49)=acd206(2)*acd206(49)
      acd206(50)=-acd206(28)*acd206(42)
      acd206(51)=-acd206(4)*acd206(37)
      acd206(52)=-acd206(19)*acd206(34)
      acd206(53)=-acd206(40)-acd206(18)
      acd206(53)=acd206(11)*acd206(53)
      acd206(50)=acd206(53)+acd206(52)+acd206(51)+acd206(43)+acd206(50)
      acd206(50)=acd206(41)*acd206(50)
      acd206(51)=-acd206(46)*acd206(42)
      acd206(52)=-acd206(36)*acd206(37)
      acd206(53)=-acd206(7)-acd206(13)
      acd206(53)=acd206(11)*acd206(53)
      acd206(54)=-acd206(34)*acd206(56)
      acd206(51)=acd206(54)+acd206(53)+acd206(51)+acd206(52)
      acd206(51)=acd206(10)*acd206(51)
      acd206(52)=acd206(40)-acd206(18)
      acd206(52)=acd206(39)*acd206(52)
      acd206(53)=acd206(7)-acd206(13)
      acd206(53)=acd206(8)*acd206(53)
      acd206(52)=acd206(53)+acd206(52)
      acd206(52)=acd206(9)*acd206(52)
      acd206(53)=acd206(39)*acd206(33)
      acd206(54)=-acd206(36)*acd206(32)
      acd206(53)=acd206(53)+acd206(54)
      acd206(53)=acd206(19)*acd206(53)
      acd206(54)=-acd206(4)*acd206(32)
      acd206(55)=acd206(8)*acd206(33)
      acd206(54)=acd206(55)-acd206(35)+acd206(54)
      acd206(54)=acd206(54)*acd206(56)
      acd206(55)=acd206(46)*acd206(47)
      acd206(56)=acd206(7)*acd206(12)
      acd206(57)=-acd206(4)*acd206(5)
      acd206(57)=-acd206(6)+acd206(57)
      acd206(57)=acd206(1)*acd206(57)
      acd206(58)=-acd206(5)*acd206(17)
      acd206(58)=acd206(38)+acd206(58)
      acd206(58)=acd206(36)*acd206(58)
      acd206(59)=acd206(13)*acd206(15)
      brack=acd206(48)+acd206(49)+acd206(50)+acd206(51)+acd206(52)+acd206(53)+a&
      &cd206(54)+acd206(55)+acd206(56)+acd206(57)+acd206(58)+acd206(59)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd206h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(57) :: acd206
      complex(ki) :: brack
      acd206(1)=d(iv1,iv2)
      acd206(2)=dotproduct(e6,qshift)
      acd206(3)=abb206(6)
      acd206(4)=dotproduct(qshift,spvak5k2)
      acd206(5)=abb206(21)
      acd206(6)=dotproduct(qshift,spvak5k4)
      acd206(7)=abb206(27)
      acd206(8)=dotproduct(qshift,spvae6k2)
      acd206(9)=abb206(25)
      acd206(10)=abb206(7)
      acd206(11)=k1(iv1)
      acd206(12)=e6(iv2)
      acd206(13)=abb206(23)
      acd206(14)=spvae6k2(iv2)
      acd206(15)=abb206(26)
      acd206(16)=k1(iv2)
      acd206(17)=e6(iv1)
      acd206(18)=spvae6k2(iv1)
      acd206(19)=k2(iv1)
      acd206(20)=spvak5k2(iv2)
      acd206(21)=abb206(24)
      acd206(22)=spvak5k4(iv2)
      acd206(23)=abb206(28)
      acd206(24)=k2(iv2)
      acd206(25)=spvak5k2(iv1)
      acd206(26)=spvak5k4(iv1)
      acd206(27)=k6(iv1)
      acd206(28)=abb206(20)
      acd206(29)=k6(iv2)
      acd206(30)=qshift(iv2)
      acd206(31)=abb206(16)
      acd206(32)=dotproduct(qshift,spvak1k2)
      acd206(33)=abb206(4)
      acd206(34)=abb206(15)
      acd206(35)=spvak1k2(iv2)
      acd206(36)=abb206(5)
      acd206(37)=spvak1k4(iv2)
      acd206(38)=abb206(14)
      acd206(39)=spvak6k2(iv2)
      acd206(40)=abb206(10)
      acd206(41)=qshift(iv1)
      acd206(42)=spvak1k2(iv1)
      acd206(43)=spvak1k4(iv1)
      acd206(44)=spvak6k2(iv1)
      acd206(45)=abb206(17)
      acd206(46)=abb206(18)
      acd206(47)=acd206(33)*acd206(6)
      acd206(47)=acd206(47)+acd206(36)
      acd206(48)=acd206(35)*acd206(47)
      acd206(49)=acd206(39)*acd206(40)
      acd206(50)=acd206(38)*acd206(37)
      acd206(51)=acd206(13)*acd206(16)
      acd206(52)=acd206(29)*acd206(28)
      acd206(53)=2.0_ki*acd206(30)
      acd206(54)=-acd206(3)*acd206(53)
      acd206(55)=acd206(20)*acd206(31)
      acd206(56)=acd206(33)*acd206(32)
      acd206(56)=acd206(56)+acd206(34)
      acd206(57)=acd206(22)*acd206(56)
      acd206(48)=acd206(57)+acd206(55)+acd206(54)+acd206(52)+acd206(51)+acd206(&
      &49)+acd206(50)+acd206(48)
      acd206(48)=acd206(17)*acd206(48)
      acd206(47)=acd206(42)*acd206(47)
      acd206(49)=acd206(40)*acd206(44)
      acd206(50)=acd206(38)*acd206(43)
      acd206(51)=acd206(11)*acd206(13)
      acd206(52)=acd206(27)*acd206(28)
      acd206(54)=2.0_ki*acd206(41)
      acd206(55)=-acd206(3)*acd206(54)
      acd206(57)=acd206(25)*acd206(31)
      acd206(56)=acd206(26)*acd206(56)
      acd206(47)=acd206(56)+acd206(57)+acd206(55)+acd206(52)+acd206(51)+acd206(&
      &49)+acd206(50)+acd206(47)
      acd206(47)=acd206(12)*acd206(47)
      acd206(49)=acd206(39)*acd206(46)
      acd206(50)=acd206(14)*acd206(45)
      acd206(51)=acd206(7)*acd206(53)
      acd206(52)=acd206(24)+acd206(29)
      acd206(52)=acd206(23)*acd206(52)
      acd206(55)=acd206(33)*acd206(2)
      acd206(56)=acd206(35)*acd206(55)
      acd206(49)=acd206(56)+acd206(52)+acd206(51)+acd206(49)+acd206(50)
      acd206(49)=acd206(26)*acd206(49)
      acd206(50)=acd206(44)*acd206(46)
      acd206(51)=acd206(18)*acd206(45)
      acd206(52)=acd206(7)*acd206(54)
      acd206(56)=acd206(19)+acd206(27)
      acd206(56)=acd206(23)*acd206(56)
      acd206(55)=acd206(42)*acd206(55)
      acd206(50)=acd206(55)+acd206(56)+acd206(52)+acd206(50)+acd206(51)
      acd206(50)=acd206(22)*acd206(50)
      acd206(51)=acd206(9)*acd206(8)
      acd206(52)=acd206(6)*acd206(7)
      acd206(55)=-acd206(5)*acd206(4)
      acd206(56)=-acd206(2)*acd206(3)
      acd206(51)=acd206(56)+acd206(55)+acd206(52)+acd206(10)+acd206(51)
      acd206(51)=acd206(1)*acd206(51)
      acd206(52)=acd206(11)*acd206(15)
      acd206(55)=acd206(9)*acd206(54)
      acd206(52)=acd206(55)+acd206(52)
      acd206(52)=acd206(14)*acd206(52)
      acd206(55)=acd206(9)*acd206(18)
      acd206(56)=-acd206(25)*acd206(5)
      acd206(55)=acd206(56)+acd206(55)
      acd206(53)=acd206(53)*acd206(55)
      acd206(54)=-acd206(5)*acd206(54)
      acd206(55)=-acd206(19)+acd206(27)
      acd206(55)=acd206(21)*acd206(55)
      acd206(54)=acd206(54)+acd206(55)
      acd206(54)=acd206(20)*acd206(54)
      acd206(55)=acd206(18)*acd206(15)*acd206(16)
      acd206(56)=-acd206(24)+acd206(29)
      acd206(56)=acd206(21)*acd206(25)*acd206(56)
      brack=acd206(47)+acd206(48)+acd206(49)+acd206(50)+2.0_ki*acd206(51)+acd20&
      &6(52)+acd206(53)+acd206(54)+acd206(55)+acd206(56)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd206h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(29) :: acd206
      complex(ki) :: brack
      acd206(1)=d(iv1,iv2)
      acd206(2)=e6(iv3)
      acd206(3)=abb206(6)
      acd206(4)=spvak5k2(iv3)
      acd206(5)=abb206(21)
      acd206(6)=spvak5k4(iv3)
      acd206(7)=abb206(27)
      acd206(8)=spvae6k2(iv3)
      acd206(9)=abb206(25)
      acd206(10)=d(iv1,iv3)
      acd206(11)=e6(iv2)
      acd206(12)=spvak5k2(iv2)
      acd206(13)=spvak5k4(iv2)
      acd206(14)=spvae6k2(iv2)
      acd206(15)=d(iv2,iv3)
      acd206(16)=e6(iv1)
      acd206(17)=spvak5k2(iv1)
      acd206(18)=spvak5k4(iv1)
      acd206(19)=spvae6k2(iv1)
      acd206(20)=spvak1k2(iv3)
      acd206(21)=abb206(4)
      acd206(22)=spvak1k2(iv2)
      acd206(23)=spvak1k2(iv1)
      acd206(24)=-acd206(9)*acd206(19)
      acd206(25)=-acd206(7)*acd206(18)
      acd206(26)=acd206(5)*acd206(17)
      acd206(27)=acd206(3)*acd206(16)
      acd206(24)=acd206(27)+acd206(26)+acd206(24)+acd206(25)
      acd206(24)=acd206(15)*acd206(24)
      acd206(25)=-acd206(9)*acd206(14)
      acd206(26)=-acd206(7)*acd206(13)
      acd206(27)=acd206(5)*acd206(12)
      acd206(28)=acd206(3)*acd206(11)
      acd206(25)=acd206(28)+acd206(27)+acd206(25)+acd206(26)
      acd206(25)=acd206(10)*acd206(25)
      acd206(26)=-acd206(9)*acd206(8)
      acd206(27)=-acd206(6)*acd206(7)
      acd206(28)=acd206(5)*acd206(4)
      acd206(29)=acd206(2)*acd206(3)
      acd206(26)=acd206(29)+acd206(28)+acd206(26)+acd206(27)
      acd206(26)=acd206(1)*acd206(26)
      acd206(24)=acd206(26)+acd206(24)+acd206(25)
      acd206(25)=-acd206(13)*acd206(16)
      acd206(26)=-acd206(11)*acd206(18)
      acd206(25)=acd206(25)+acd206(26)
      acd206(25)=acd206(20)*acd206(25)
      acd206(26)=-acd206(16)*acd206(22)
      acd206(27)=-acd206(11)*acd206(23)
      acd206(26)=acd206(26)+acd206(27)
      acd206(26)=acd206(6)*acd206(26)
      acd206(27)=-acd206(18)*acd206(22)
      acd206(28)=-acd206(13)*acd206(23)
      acd206(27)=acd206(27)+acd206(28)
      acd206(27)=acd206(2)*acd206(27)
      acd206(25)=acd206(27)+acd206(26)+acd206(25)
      acd206(25)=acd206(21)*acd206(25)
      brack=2.0_ki*acd206(24)+acd206(25)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd206h0
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
!---#[ subroutine reconstruct_d206:
   subroutine     reconstruct_d206(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group0
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group0), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_206:
      coeffs%coeffs_206%c0 = derivative(czip)
      coeffs%coeffs_206%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_206%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_206%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_206%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_206%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_206%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_206%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_206%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_206%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_206%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_206%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_206%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_206%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_206%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_206%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_206%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_206%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_206%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_206%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_206%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_206%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_206%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_206%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_206%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_206%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_206%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_206%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_206%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_206%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_206%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_206%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_206%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_206%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_206%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_206:
   end subroutine reconstruct_d206
!---#] subroutine reconstruct_d206:
end module     p12_sbars_hepemg_d206h0l1d
