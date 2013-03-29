module     p12_sbars_hepemg_d204h0l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p12_s &
   ! &bars_hepemg/helicity0d204h0l1d.f90
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
   public :: derivative , reconstruct_d204
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd204h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(48) :: acd204
      complex(ki) :: brack
      acd204(1)=dotproduct(k1,qshift)
      acd204(2)=dotproduct(e6,qshift)
      acd204(3)=abb204(6)
      acd204(4)=dotproduct(qshift,spvak5k2)
      acd204(5)=abb204(22)
      acd204(6)=dotproduct(qshift,spvak5k4)
      acd204(7)=abb204(28)
      acd204(8)=dotproduct(qshift,spvak1e6)
      acd204(9)=abb204(21)
      acd204(10)=dotproduct(qshift,spvae6k2)
      acd204(11)=abb204(19)
      acd204(12)=abb204(4)
      acd204(13)=dotproduct(k6,qshift)
      acd204(14)=abb204(16)
      acd204(15)=dotproduct(qshift,qshift)
      acd204(16)=abb204(18)
      acd204(17)=abb204(13)
      acd204(18)=dotproduct(qshift,spvak1k2)
      acd204(19)=abb204(11)
      acd204(20)=abb204(8)
      acd204(21)=dotproduct(qshift,spvak1k6)
      acd204(22)=abb204(12)
      acd204(23)=dotproduct(qshift,spvak6k2)
      acd204(24)=abb204(24)
      acd204(25)=abb204(10)
      acd204(26)=abb204(25)
      acd204(27)=abb204(14)
      acd204(28)=abb204(20)
      acd204(29)=abb204(15)
      acd204(30)=abb204(7)
      acd204(31)=abb204(30)
      acd204(32)=abb204(29)
      acd204(33)=abb204(17)
      acd204(34)=abb204(31)
      acd204(35)=abb204(26)
      acd204(36)=abb204(5)
      acd204(37)=abb204(23)
      acd204(38)=abb204(9)
      acd204(39)=acd204(3)*acd204(1)
      acd204(40)=-acd204(16)*acd204(15)
      acd204(41)=acd204(17)*acd204(4)
      acd204(42)=acd204(18)*acd204(6)
      acd204(43)=-acd204(19)*acd204(42)
      acd204(44)=acd204(22)*acd204(21)
      acd204(45)=acd204(24)*acd204(23)
      acd204(39)=-acd204(25)+acd204(45)+acd204(44)+acd204(43)+acd204(41)+acd204&
      &(40)+acd204(39)
      acd204(39)=acd204(2)*acd204(39)
      acd204(40)=acd204(7)*acd204(1)
      acd204(41)=acd204(31)*acd204(8)
      acd204(43)=acd204(32)*acd204(10)
      acd204(44)=-acd204(34)*acd204(23)
      acd204(40)=-acd204(35)+acd204(44)+acd204(43)+acd204(41)+acd204(40)
      acd204(40)=acd204(6)*acd204(40)
      acd204(41)=-acd204(5)*acd204(4)
      acd204(43)=-acd204(9)*acd204(8)
      acd204(44)=acd204(11)*acd204(10)
      acd204(41)=acd204(44)+acd204(41)+acd204(43)
      acd204(43)=acd204(13)-acd204(1)
      acd204(41)=acd204(43)*acd204(41)
      acd204(43)=-acd204(26)*acd204(4)
      acd204(44)=-acd204(27)*acd204(8)
      acd204(45)=-acd204(28)*acd204(10)
      acd204(43)=acd204(29)+acd204(45)+acd204(44)+acd204(43)
      acd204(43)=acd204(15)*acd204(43)
      acd204(44)=acd204(20)*acd204(2)
      acd204(44)=-acd204(36)+acd204(44)
      acd204(44)=acd204(18)*acd204(44)
      acd204(45)=-acd204(12)*acd204(1)
      acd204(46)=-acd204(14)*acd204(13)
      acd204(47)=-acd204(30)*acd204(4)
      acd204(42)=acd204(33)*acd204(42)
      acd204(48)=-acd204(37)*acd204(23)
      brack=acd204(38)+acd204(39)+acd204(40)+acd204(41)+acd204(42)+acd204(43)+a&
      &cd204(44)+acd204(45)+acd204(46)+acd204(47)+acd204(48)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd204h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(61) :: acd204
      complex(ki) :: brack
      acd204(1)=k1(iv1)
      acd204(2)=dotproduct(e6,qshift)
      acd204(3)=abb204(6)
      acd204(4)=dotproduct(qshift,spvak5k2)
      acd204(5)=abb204(22)
      acd204(6)=dotproduct(qshift,spvak5k4)
      acd204(7)=abb204(28)
      acd204(8)=dotproduct(qshift,spvak1e6)
      acd204(9)=abb204(21)
      acd204(10)=dotproduct(qshift,spvae6k2)
      acd204(11)=abb204(19)
      acd204(12)=abb204(4)
      acd204(13)=k6(iv1)
      acd204(14)=abb204(16)
      acd204(15)=e6(iv1)
      acd204(16)=dotproduct(k1,qshift)
      acd204(17)=dotproduct(qshift,qshift)
      acd204(18)=abb204(18)
      acd204(19)=abb204(13)
      acd204(20)=dotproduct(qshift,spvak1k2)
      acd204(21)=abb204(11)
      acd204(22)=abb204(8)
      acd204(23)=dotproduct(qshift,spvak1k6)
      acd204(24)=abb204(12)
      acd204(25)=dotproduct(qshift,spvak6k2)
      acd204(26)=abb204(24)
      acd204(27)=abb204(10)
      acd204(28)=qshift(iv1)
      acd204(29)=abb204(25)
      acd204(30)=abb204(14)
      acd204(31)=abb204(20)
      acd204(32)=abb204(15)
      acd204(33)=spvak5k2(iv1)
      acd204(34)=dotproduct(k6,qshift)
      acd204(35)=abb204(7)
      acd204(36)=spvak5k4(iv1)
      acd204(37)=abb204(30)
      acd204(38)=abb204(29)
      acd204(39)=abb204(17)
      acd204(40)=abb204(31)
      acd204(41)=abb204(26)
      acd204(42)=spvak1e6(iv1)
      acd204(43)=spvae6k2(iv1)
      acd204(44)=spvak1k2(iv1)
      acd204(45)=abb204(5)
      acd204(46)=spvak1k6(iv1)
      acd204(47)=spvak6k2(iv1)
      acd204(48)=abb204(23)
      acd204(49)=acd204(6)*acd204(21)
      acd204(49)=acd204(49)-acd204(22)
      acd204(50)=acd204(44)*acd204(49)
      acd204(51)=-acd204(24)*acd204(46)
      acd204(52)=-acd204(47)*acd204(26)
      acd204(53)=-acd204(33)*acd204(19)
      acd204(54)=2.0_ki*acd204(28)
      acd204(55)=acd204(18)*acd204(54)
      acd204(56)=-acd204(1)*acd204(3)
      acd204(57)=acd204(36)*acd204(20)*acd204(21)
      acd204(50)=acd204(57)+acd204(56)+acd204(55)+acd204(53)+acd204(51)+acd204(&
      &52)+acd204(50)
      acd204(50)=acd204(2)*acd204(50)
      acd204(49)=acd204(20)*acd204(49)
      acd204(51)=-acd204(25)*acd204(26)
      acd204(52)=-acd204(24)*acd204(23)
      acd204(53)=acd204(17)*acd204(18)
      acd204(55)=-acd204(4)*acd204(19)
      acd204(56)=-acd204(16)*acd204(3)
      acd204(49)=acd204(56)+acd204(55)+acd204(53)+acd204(52)+acd204(27)+acd204(&
      &51)+acd204(49)
      acd204(49)=acd204(15)*acd204(49)
      acd204(51)=acd204(25)*acd204(40)
      acd204(52)=-acd204(20)*acd204(39)
      acd204(53)=-acd204(10)*acd204(38)
      acd204(55)=-acd204(8)*acd204(37)
      acd204(56)=-acd204(16)*acd204(7)
      acd204(51)=acd204(56)+acd204(55)+acd204(53)+acd204(52)+acd204(41)+acd204(&
      &51)
      acd204(51)=acd204(36)*acd204(51)
      acd204(52)=acd204(47)*acd204(40)
      acd204(53)=-acd204(44)*acd204(39)
      acd204(55)=-acd204(43)*acd204(38)
      acd204(56)=-acd204(42)*acd204(37)
      acd204(57)=-acd204(1)*acd204(7)
      acd204(52)=acd204(57)+acd204(56)+acd204(55)+acd204(52)+acd204(53)
      acd204(52)=acd204(6)*acd204(52)
      acd204(53)=acd204(10)*acd204(11)
      acd204(55)=acd204(8)*acd204(9)
      acd204(56)=acd204(4)*acd204(5)
      acd204(53)=-acd204(56)+acd204(53)-acd204(55)
      acd204(55)=acd204(14)-acd204(53)
      acd204(55)=acd204(13)*acd204(55)
      acd204(56)=acd204(10)*acd204(31)
      acd204(57)=acd204(8)*acd204(30)
      acd204(58)=acd204(4)*acd204(29)
      acd204(56)=acd204(58)+acd204(57)-acd204(32)+acd204(56)
      acd204(54)=acd204(56)*acd204(54)
      acd204(53)=acd204(12)+acd204(53)
      acd204(53)=acd204(1)*acd204(53)
      acd204(56)=acd204(11)*acd204(43)
      acd204(57)=acd204(9)*acd204(42)
      acd204(56)=acd204(56)-acd204(57)
      acd204(57)=-acd204(34)*acd204(56)
      acd204(58)=acd204(43)*acd204(31)
      acd204(59)=acd204(42)*acd204(30)
      acd204(58)=acd204(58)+acd204(59)
      acd204(58)=acd204(17)*acd204(58)
      acd204(59)=acd204(17)*acd204(29)
      acd204(60)=acd204(5)*acd204(34)
      acd204(59)=acd204(60)+acd204(35)+acd204(59)
      acd204(59)=acd204(33)*acd204(59)
      acd204(60)=-acd204(33)*acd204(5)
      acd204(56)=acd204(60)+acd204(56)
      acd204(56)=acd204(16)*acd204(56)
      acd204(60)=acd204(47)*acd204(48)
      acd204(61)=acd204(44)*acd204(45)
      brack=acd204(49)+acd204(50)+acd204(51)+acd204(52)+acd204(53)+acd204(54)+a&
      &cd204(55)+acd204(56)+acd204(57)+acd204(58)+acd204(59)+acd204(60)+acd204(&
      &61)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd204h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(60) :: acd204
      complex(ki) :: brack
      acd204(1)=d(iv1,iv2)
      acd204(2)=dotproduct(e6,qshift)
      acd204(3)=abb204(18)
      acd204(4)=dotproduct(qshift,spvak5k2)
      acd204(5)=abb204(25)
      acd204(6)=dotproduct(qshift,spvak1e6)
      acd204(7)=abb204(14)
      acd204(8)=dotproduct(qshift,spvae6k2)
      acd204(9)=abb204(20)
      acd204(10)=abb204(15)
      acd204(11)=k1(iv1)
      acd204(12)=e6(iv2)
      acd204(13)=abb204(6)
      acd204(14)=spvak5k2(iv2)
      acd204(15)=abb204(22)
      acd204(16)=spvak1e6(iv2)
      acd204(17)=abb204(21)
      acd204(18)=spvae6k2(iv2)
      acd204(19)=abb204(19)
      acd204(20)=spvak5k4(iv2)
      acd204(21)=abb204(28)
      acd204(22)=k1(iv2)
      acd204(23)=e6(iv1)
      acd204(24)=spvak5k2(iv1)
      acd204(25)=spvak1e6(iv1)
      acd204(26)=spvae6k2(iv1)
      acd204(27)=spvak5k4(iv1)
      acd204(28)=k6(iv1)
      acd204(29)=k6(iv2)
      acd204(30)=qshift(iv2)
      acd204(31)=abb204(13)
      acd204(32)=dotproduct(qshift,spvak1k2)
      acd204(33)=abb204(11)
      acd204(34)=spvak1k2(iv2)
      acd204(35)=dotproduct(qshift,spvak5k4)
      acd204(36)=abb204(8)
      acd204(37)=spvak1k6(iv2)
      acd204(38)=abb204(12)
      acd204(39)=spvak6k2(iv2)
      acd204(40)=abb204(24)
      acd204(41)=qshift(iv1)
      acd204(42)=spvak1k2(iv1)
      acd204(43)=spvak1k6(iv1)
      acd204(44)=spvak6k2(iv1)
      acd204(45)=abb204(30)
      acd204(46)=abb204(29)
      acd204(47)=abb204(17)
      acd204(48)=abb204(31)
      acd204(49)=acd204(39)*acd204(40)
      acd204(50)=acd204(38)*acd204(37)
      acd204(51)=acd204(34)*acd204(36)
      acd204(52)=2.0_ki*acd204(3)
      acd204(52)=-acd204(30)*acd204(52)
      acd204(53)=acd204(14)*acd204(31)
      acd204(54)=acd204(22)*acd204(13)
      acd204(55)=acd204(33)*acd204(34)
      acd204(56)=-acd204(35)*acd204(55)
      acd204(57)=acd204(33)*acd204(32)
      acd204(58)=-acd204(20)*acd204(57)
      acd204(49)=acd204(58)+acd204(56)+acd204(54)+acd204(53)+acd204(52)+acd204(&
      &51)+acd204(49)+acd204(50)
      acd204(49)=acd204(23)*acd204(49)
      acd204(50)=acd204(40)*acd204(44)
      acd204(51)=acd204(38)*acd204(43)
      acd204(52)=acd204(42)*acd204(36)
      acd204(53)=2.0_ki*acd204(41)
      acd204(54)=-acd204(3)*acd204(53)
      acd204(56)=acd204(24)*acd204(31)
      acd204(58)=acd204(11)*acd204(13)
      acd204(59)=acd204(33)*acd204(42)
      acd204(60)=-acd204(35)*acd204(59)
      acd204(57)=-acd204(27)*acd204(57)
      acd204(50)=acd204(57)+acd204(60)+acd204(58)+acd204(56)+acd204(54)+acd204(&
      &52)+acd204(50)+acd204(51)
      acd204(50)=acd204(12)*acd204(50)
      acd204(51)=-acd204(39)*acd204(48)
      acd204(52)=acd204(34)*acd204(47)
      acd204(54)=acd204(18)*acd204(46)
      acd204(56)=acd204(16)*acd204(45)
      acd204(57)=acd204(22)*acd204(21)
      acd204(55)=-acd204(2)*acd204(55)
      acd204(51)=acd204(55)+acd204(57)+acd204(56)+acd204(54)+acd204(51)+acd204(&
      &52)
      acd204(51)=acd204(27)*acd204(51)
      acd204(52)=-acd204(44)*acd204(48)
      acd204(54)=acd204(42)*acd204(47)
      acd204(55)=acd204(26)*acd204(46)
      acd204(56)=acd204(25)*acd204(45)
      acd204(57)=acd204(11)*acd204(21)
      acd204(58)=-acd204(2)*acd204(59)
      acd204(52)=acd204(58)+acd204(57)+acd204(56)+acd204(55)+acd204(52)+acd204(&
      &54)
      acd204(52)=acd204(20)*acd204(52)
      acd204(54)=-acd204(9)*acd204(8)
      acd204(55)=-acd204(7)*acd204(6)
      acd204(56)=-acd204(5)*acd204(4)
      acd204(57)=-acd204(2)*acd204(3)
      acd204(54)=acd204(57)+acd204(56)+acd204(55)+acd204(10)+acd204(54)
      acd204(54)=acd204(1)*acd204(54)
      acd204(55)=-acd204(26)*acd204(9)
      acd204(56)=-acd204(25)*acd204(7)
      acd204(57)=-acd204(24)*acd204(5)
      acd204(55)=acd204(57)+acd204(55)+acd204(56)
      acd204(55)=acd204(30)*acd204(55)
      acd204(54)=acd204(55)+acd204(54)
      acd204(55)=acd204(19)*acd204(26)
      acd204(56)=acd204(17)*acd204(25)
      acd204(57)=acd204(15)*acd204(24)
      acd204(55)=-acd204(57)+acd204(55)-acd204(56)
      acd204(56)=-acd204(22)+acd204(29)
      acd204(55)=acd204(55)*acd204(56)
      acd204(56)=-acd204(18)*acd204(19)
      acd204(57)=acd204(16)*acd204(17)
      acd204(58)=acd204(14)*acd204(15)
      acd204(56)=acd204(58)+acd204(56)+acd204(57)
      acd204(56)=acd204(11)*acd204(56)
      acd204(57)=-acd204(9)*acd204(53)
      acd204(58)=acd204(19)*acd204(28)
      acd204(57)=acd204(57)+acd204(58)
      acd204(57)=acd204(18)*acd204(57)
      acd204(58)=-acd204(7)*acd204(53)
      acd204(59)=-acd204(17)*acd204(28)
      acd204(58)=acd204(58)+acd204(59)
      acd204(58)=acd204(16)*acd204(58)
      acd204(53)=-acd204(5)*acd204(53)
      acd204(59)=-acd204(15)*acd204(28)
      acd204(53)=acd204(53)+acd204(59)
      acd204(53)=acd204(14)*acd204(53)
      brack=acd204(49)+acd204(50)+acd204(51)+acd204(52)+acd204(53)+2.0_ki*acd20&
      &4(54)+acd204(55)+acd204(56)+acd204(57)+acd204(58)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd204h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(32) :: acd204
      complex(ki) :: brack
      acd204(1)=d(iv1,iv2)
      acd204(2)=e6(iv3)
      acd204(3)=abb204(18)
      acd204(4)=spvak5k2(iv3)
      acd204(5)=abb204(25)
      acd204(6)=spvak1e6(iv3)
      acd204(7)=abb204(14)
      acd204(8)=spvae6k2(iv3)
      acd204(9)=abb204(20)
      acd204(10)=d(iv1,iv3)
      acd204(11)=e6(iv2)
      acd204(12)=spvak5k2(iv2)
      acd204(13)=spvak1e6(iv2)
      acd204(14)=spvae6k2(iv2)
      acd204(15)=d(iv2,iv3)
      acd204(16)=e6(iv1)
      acd204(17)=spvak5k2(iv1)
      acd204(18)=spvak1e6(iv1)
      acd204(19)=spvae6k2(iv1)
      acd204(20)=spvak1k2(iv2)
      acd204(21)=spvak5k4(iv3)
      acd204(22)=abb204(11)
      acd204(23)=spvak1k2(iv3)
      acd204(24)=spvak5k4(iv2)
      acd204(25)=spvak1k2(iv1)
      acd204(26)=spvak5k4(iv1)
      acd204(27)=acd204(9)*acd204(19)
      acd204(28)=acd204(7)*acd204(18)
      acd204(29)=acd204(5)*acd204(17)
      acd204(30)=acd204(3)*acd204(16)
      acd204(27)=acd204(30)+acd204(29)+acd204(27)+acd204(28)
      acd204(27)=acd204(15)*acd204(27)
      acd204(28)=acd204(9)*acd204(14)
      acd204(29)=acd204(7)*acd204(13)
      acd204(30)=acd204(5)*acd204(12)
      acd204(31)=acd204(3)*acd204(11)
      acd204(28)=acd204(31)+acd204(30)+acd204(28)+acd204(29)
      acd204(28)=acd204(10)*acd204(28)
      acd204(29)=acd204(9)*acd204(8)
      acd204(30)=acd204(7)*acd204(6)
      acd204(31)=acd204(5)*acd204(4)
      acd204(32)=acd204(2)*acd204(3)
      acd204(29)=acd204(32)+acd204(31)+acd204(29)+acd204(30)
      acd204(29)=acd204(1)*acd204(29)
      acd204(27)=acd204(29)+acd204(27)+acd204(28)
      acd204(28)=acd204(23)*acd204(24)
      acd204(29)=acd204(20)*acd204(21)
      acd204(28)=acd204(28)+acd204(29)
      acd204(28)=acd204(16)*acd204(28)
      acd204(29)=acd204(23)*acd204(26)
      acd204(30)=acd204(21)*acd204(25)
      acd204(29)=acd204(29)+acd204(30)
      acd204(29)=acd204(11)*acd204(29)
      acd204(30)=acd204(24)*acd204(25)
      acd204(31)=acd204(20)*acd204(26)
      acd204(30)=acd204(30)+acd204(31)
      acd204(30)=acd204(2)*acd204(30)
      acd204(28)=acd204(30)+acd204(28)+acd204(29)
      acd204(28)=acd204(22)*acd204(28)
      brack=2.0_ki*acd204(27)+acd204(28)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd204h0
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
!---#[ subroutine reconstruct_d204:
   subroutine     reconstruct_d204(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_204:
      coeffs%coeffs_204%c0 = derivative(czip)
      coeffs%coeffs_204%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_204%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_204%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_204%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_204%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_204%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_204%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_204%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_204%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_204%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_204%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_204%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_204%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_204%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_204%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_204%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_204%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_204%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_204%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_204%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_204%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_204%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_204%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_204%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_204%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_204%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_204%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_204%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_204%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_204%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_204%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_204%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_204%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_204%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_204:
   end subroutine reconstruct_d204
!---#] subroutine reconstruct_d204:
end module     p12_sbars_hepemg_d204h0l1d
