module     p4_ubaru_hepemg_d43h3l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p4_ubaru_hepemg/helicity3d43h3l1d.f90
   ! generator: buildfortran_d.py
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   integer, private :: iv4
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d43
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd43h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd43
      complex(ki) :: brack
      acd43(1)=abb43(12)
      brack=acd43(1)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd43h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(69) :: acd43
      complex(ki) :: brack
      acd43(1)=k1(iv1)
      acd43(2)=abb43(62)
      acd43(3)=k2(iv1)
      acd43(4)=abb43(68)
      acd43(5)=l3(iv1)
      acd43(6)=abb43(42)
      acd43(7)=k6(iv1)
      acd43(8)=abb43(8)
      acd43(9)=e6(iv1)
      acd43(10)=abb43(21)
      acd43(11)=spvak2k1(iv1)
      acd43(12)=abb43(11)
      acd43(13)=spvak2l3(iv1)
      acd43(14)=abb43(37)
      acd43(15)=spvak2k5(iv1)
      acd43(16)=abb43(6)
      acd43(17)=spval3k1(iv1)
      acd43(18)=abb43(36)
      acd43(19)=spval3k5(iv1)
      acd43(20)=abb43(31)
      acd43(21)=spvak4k1(iv1)
      acd43(22)=abb43(9)
      acd43(23)=spvak4l3(iv1)
      acd43(24)=abb43(34)
      acd43(25)=spvak4k5(iv1)
      acd43(26)=abb43(88)
      acd43(27)=spvak4k6(iv1)
      acd43(28)=abb43(58)
      acd43(29)=spvak6k5(iv1)
      acd43(30)=abb43(16)
      acd43(31)=spvae6k1(iv1)
      acd43(32)=abb43(7)
      acd43(33)=spvak2e6(iv1)
      acd43(34)=abb43(13)
      acd43(35)=spval3e6(iv1)
      acd43(36)=abb43(65)
      acd43(37)=spvae6l3(iv1)
      acd43(38)=abb43(82)
      acd43(39)=spvak4e6(iv1)
      acd43(40)=abb43(80)
      acd43(41)=spvae6k5(iv1)
      acd43(42)=abb43(32)
      acd43(43)=spvak6e6(iv1)
      acd43(44)=abb43(76)
      acd43(45)=spvae6k6(iv1)
      acd43(46)=abb43(64)
      acd43(47)=-acd43(2)*acd43(1)
      acd43(48)=-acd43(4)*acd43(3)
      acd43(49)=-acd43(6)*acd43(5)
      acd43(50)=-acd43(8)*acd43(7)
      acd43(51)=-acd43(10)*acd43(9)
      acd43(52)=-acd43(12)*acd43(11)
      acd43(53)=-acd43(14)*acd43(13)
      acd43(54)=-acd43(16)*acd43(15)
      acd43(55)=-acd43(18)*acd43(17)
      acd43(56)=-acd43(20)*acd43(19)
      acd43(57)=-acd43(22)*acd43(21)
      acd43(58)=-acd43(24)*acd43(23)
      acd43(59)=-acd43(26)*acd43(25)
      acd43(60)=-acd43(28)*acd43(27)
      acd43(61)=-acd43(30)*acd43(29)
      acd43(62)=-acd43(32)*acd43(31)
      acd43(63)=-acd43(34)*acd43(33)
      acd43(64)=-acd43(36)*acd43(35)
      acd43(65)=-acd43(38)*acd43(37)
      acd43(66)=-acd43(40)*acd43(39)
      acd43(67)=-acd43(42)*acd43(41)
      acd43(68)=-acd43(44)*acd43(43)
      acd43(69)=-acd43(46)*acd43(45)
      brack=acd43(47)+acd43(48)+acd43(49)+acd43(50)+acd43(51)+acd43(52)+acd43(5&
      &3)+acd43(54)+acd43(55)+acd43(56)+acd43(57)+acd43(58)+acd43(59)+acd43(60)&
      &+acd43(61)+acd43(62)+acd43(63)+acd43(64)+acd43(65)+acd43(66)+acd43(67)+a&
      &cd43(68)+acd43(69)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd43h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(118) :: acd43
      complex(ki) :: brack
      acd43(1)=d(iv1,iv2)
      acd43(2)=abb43(57)
      acd43(3)=k1(iv1)
      acd43(4)=e6(iv2)
      acd43(5)=abb43(45)
      acd43(6)=spvae6k1(iv2)
      acd43(7)=abb43(61)
      acd43(8)=spvak2e6(iv2)
      acd43(9)=abb43(69)
      acd43(10)=spvak4e6(iv2)
      acd43(11)=abb43(48)
      acd43(12)=spvae6k5(iv2)
      acd43(13)=abb43(60)
      acd43(14)=k1(iv2)
      acd43(15)=e6(iv1)
      acd43(16)=spvae6k1(iv1)
      acd43(17)=spvak2e6(iv1)
      acd43(18)=spvak4e6(iv1)
      acd43(19)=spvae6k5(iv1)
      acd43(20)=k2(iv1)
      acd43(21)=abb43(70)
      acd43(22)=abb43(71)
      acd43(23)=abb43(85)
      acd43(24)=abb43(81)
      acd43(25)=abb43(77)
      acd43(26)=spvak2k5(iv2)
      acd43(27)=abb43(30)
      acd43(28)=spvak4k1(iv2)
      acd43(29)=abb43(78)
      acd43(30)=spvak4k5(iv2)
      acd43(31)=abb43(89)
      acd43(32)=k2(iv2)
      acd43(33)=spvak2k5(iv1)
      acd43(34)=spvak4k1(iv1)
      acd43(35)=spvak4k5(iv1)
      acd43(36)=l3(iv1)
      acd43(37)=abb43(28)
      acd43(38)=abb43(67)
      acd43(39)=l3(iv2)
      acd43(40)=k6(iv1)
      acd43(41)=spvak2k1(iv2)
      acd43(42)=abb43(15)
      acd43(43)=k6(iv2)
      acd43(44)=spvak2k1(iv1)
      acd43(45)=abb43(27)
      acd43(46)=abb43(14)
      acd43(47)=abb43(17)
      acd43(48)=abb43(90)
      acd43(49)=abb43(35)
      acd43(50)=abb43(41)
      acd43(51)=abb43(33)
      acd43(52)=abb43(25)
      acd43(53)=abb43(22)
      acd43(54)=abb43(19)
      acd43(55)=spvak2l3(iv2)
      acd43(56)=abb43(44)
      acd43(57)=spvak2k6(iv2)
      acd43(58)=abb43(39)
      acd43(59)=spval3k1(iv2)
      acd43(60)=abb43(38)
      acd43(61)=spvak6k1(iv2)
      acd43(62)=abb43(91)
      acd43(63)=spvak2l3(iv1)
      acd43(64)=spvak2k6(iv1)
      acd43(65)=spval3k1(iv1)
      acd43(66)=spvak6k1(iv1)
      acd43(67)=spval3k5(iv2)
      acd43(68)=abb43(23)
      acd43(69)=spvak4k2(iv2)
      acd43(70)=abb43(46)
      acd43(71)=spvak4l3(iv2)
      acd43(72)=abb43(18)
      acd43(73)=spvak4k6(iv2)
      acd43(74)=abb43(43)
      acd43(75)=spvak6k5(iv2)
      acd43(76)=abb43(40)
      acd43(77)=spvak1e6(iv2)
      acd43(78)=abb43(10)
      acd43(79)=spvae6k2(iv2)
      acd43(80)=abb43(29)
      acd43(81)=spval3e6(iv2)
      acd43(82)=abb43(47)
      acd43(83)=spvae6l3(iv2)
      acd43(84)=abb43(26)
      acd43(85)=spvak6e6(iv2)
      acd43(86)=abb43(24)
      acd43(87)=spvae6k6(iv2)
      acd43(88)=abb43(20)
      acd43(89)=spval3k5(iv1)
      acd43(90)=spvak4k2(iv1)
      acd43(91)=spvak4l3(iv1)
      acd43(92)=spvak4k6(iv1)
      acd43(93)=spvak6k5(iv1)
      acd43(94)=spvak1e6(iv1)
      acd43(95)=spvae6k2(iv1)
      acd43(96)=spval3e6(iv1)
      acd43(97)=spvae6l3(iv1)
      acd43(98)=spvak6e6(iv1)
      acd43(99)=spvae6k6(iv1)
      acd43(100)=acd43(88)*acd43(87)
      acd43(101)=acd43(86)*acd43(85)
      acd43(102)=acd43(84)*acd43(83)
      acd43(103)=-acd43(82)*acd43(81)
      acd43(104)=acd43(80)*acd43(79)
      acd43(105)=acd43(78)*acd43(77)
      acd43(106)=acd43(76)*acd43(75)
      acd43(107)=acd43(74)*acd43(73)
      acd43(108)=acd43(72)*acd43(71)
      acd43(109)=acd43(70)*acd43(69)
      acd43(110)=acd43(68)*acd43(67)
      acd43(111)=acd43(43)*acd43(42)
      acd43(112)=acd43(12)*acd43(53)
      acd43(113)=acd43(10)*acd43(52)
      acd43(114)=acd43(26)*acd43(54)
      acd43(115)=acd43(8)*acd43(51)
      acd43(116)=acd43(6)*acd43(49)
      acd43(117)=acd43(4)*acd43(47)
      acd43(100)=acd43(117)+acd43(116)+acd43(115)+acd43(114)+acd43(113)+acd43(1&
      &12)+acd43(111)+acd43(110)+acd43(109)+acd43(108)+acd43(107)+acd43(106)+ac&
      &d43(105)+acd43(104)+acd43(103)+acd43(102)+acd43(100)+acd43(101)
      acd43(100)=acd43(44)*acd43(100)
      acd43(101)=acd43(88)*acd43(99)
      acd43(102)=acd43(86)*acd43(98)
      acd43(103)=acd43(84)*acd43(97)
      acd43(104)=-acd43(82)*acd43(96)
      acd43(105)=acd43(80)*acd43(95)
      acd43(106)=acd43(78)*acd43(94)
      acd43(107)=acd43(76)*acd43(93)
      acd43(108)=acd43(74)*acd43(92)
      acd43(109)=acd43(72)*acd43(91)
      acd43(110)=acd43(70)*acd43(90)
      acd43(111)=acd43(68)*acd43(89)
      acd43(112)=acd43(40)*acd43(42)
      acd43(113)=acd43(19)*acd43(53)
      acd43(114)=acd43(18)*acd43(52)
      acd43(115)=acd43(33)*acd43(54)
      acd43(116)=acd43(17)*acd43(51)
      acd43(117)=acd43(16)*acd43(49)
      acd43(118)=acd43(15)*acd43(47)
      acd43(101)=acd43(118)+acd43(117)+acd43(116)+acd43(115)+acd43(114)+acd43(1&
      &13)+acd43(112)+acd43(111)+acd43(110)+acd43(109)+acd43(108)+acd43(107)+ac&
      &d43(106)+acd43(105)+acd43(104)+acd43(103)+acd43(101)+acd43(102)
      acd43(101)=acd43(41)*acd43(101)
      acd43(102)=acd43(34)*acd43(29)
      acd43(103)=acd43(19)*acd43(25)
      acd43(104)=acd43(18)*acd43(24)
      acd43(105)=acd43(33)*acd43(27)
      acd43(106)=acd43(17)*acd43(23)
      acd43(107)=-acd43(16)*acd43(22)
      acd43(108)=acd43(15)*acd43(21)
      acd43(109)=acd43(35)*acd43(31)
      acd43(102)=acd43(109)+acd43(108)+acd43(107)+acd43(106)+acd43(105)+acd43(1&
      &04)+acd43(102)+acd43(103)
      acd43(102)=acd43(32)*acd43(102)
      acd43(103)=acd43(28)*acd43(29)
      acd43(104)=acd43(12)*acd43(25)
      acd43(105)=acd43(10)*acd43(24)
      acd43(106)=acd43(26)*acd43(27)
      acd43(107)=acd43(8)*acd43(23)
      acd43(108)=-acd43(6)*acd43(22)
      acd43(109)=acd43(4)*acd43(21)
      acd43(110)=acd43(30)*acd43(31)
      acd43(103)=acd43(110)+acd43(109)+acd43(108)+acd43(107)+acd43(106)+acd43(1&
      &05)+acd43(103)+acd43(104)
      acd43(103)=acd43(20)*acd43(103)
      acd43(104)=acd43(62)*acd43(61)
      acd43(105)=acd43(60)*acd43(59)
      acd43(106)=acd43(58)*acd43(57)
      acd43(107)=acd43(56)*acd43(55)
      acd43(108)=acd43(8)*acd43(50)
      acd43(109)=acd43(6)*acd43(48)
      acd43(104)=acd43(109)+acd43(108)+acd43(107)+acd43(106)+acd43(104)+acd43(1&
      &05)
      acd43(104)=acd43(35)*acd43(104)
      acd43(105)=acd43(62)*acd43(66)
      acd43(106)=acd43(60)*acd43(65)
      acd43(107)=acd43(58)*acd43(64)
      acd43(108)=acd43(56)*acd43(63)
      acd43(109)=acd43(17)*acd43(50)
      acd43(110)=acd43(16)*acd43(48)
      acd43(105)=acd43(110)+acd43(109)+acd43(108)+acd43(107)+acd43(105)+acd43(1&
      &06)
      acd43(105)=acd43(30)*acd43(105)
      acd43(106)=acd43(13)*acd43(19)
      acd43(107)=acd43(11)*acd43(18)
      acd43(108)=acd43(17)*acd43(9)
      acd43(109)=acd43(16)*acd43(7)
      acd43(106)=-acd43(106)+acd43(107)+acd43(108)-acd43(109)
      acd43(107)=acd43(15)*acd43(5)
      acd43(107)=acd43(107)-acd43(106)
      acd43(107)=acd43(14)*acd43(107)
      acd43(108)=acd43(12)*acd43(13)
      acd43(109)=acd43(10)*acd43(11)
      acd43(110)=acd43(8)*acd43(9)
      acd43(111)=acd43(6)*acd43(7)
      acd43(108)=-acd43(108)+acd43(109)+acd43(110)-acd43(111)
      acd43(109)=acd43(4)*acd43(5)
      acd43(109)=acd43(109)-acd43(108)
      acd43(109)=acd43(3)*acd43(109)
      acd43(110)=acd43(34)*acd43(38)
      acd43(111)=acd43(33)*acd43(37)
      acd43(110)=acd43(110)-acd43(111)
      acd43(106)=acd43(110)+acd43(106)
      acd43(106)=acd43(39)*acd43(106)
      acd43(111)=acd43(28)*acd43(38)
      acd43(112)=acd43(26)*acd43(37)
      acd43(111)=acd43(111)-acd43(112)
      acd43(108)=acd43(111)+acd43(108)
      acd43(108)=acd43(36)*acd43(108)
      acd43(110)=acd43(43)*acd43(110)
      acd43(111)=acd43(40)*acd43(111)
      acd43(112)=acd43(28)*acd43(46)
      acd43(113)=acd43(26)*acd43(45)
      acd43(112)=acd43(112)+acd43(113)
      acd43(112)=acd43(15)*acd43(112)
      acd43(113)=acd43(34)*acd43(46)
      acd43(114)=acd43(33)*acd43(45)
      acd43(113)=acd43(113)+acd43(114)
      acd43(113)=acd43(4)*acd43(113)
      acd43(114)=acd43(1)*acd43(2)
      brack=acd43(100)+acd43(101)+acd43(102)+acd43(103)+acd43(104)+acd43(105)+a&
      &cd43(106)+acd43(107)+acd43(108)+acd43(109)+acd43(110)+acd43(111)+acd43(1&
      &12)+acd43(113)+2.0_ki*acd43(114)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd43h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd43
      complex(ki) :: brack
      brack=0.0_ki
   end function brack_4
!---#] function brack_4:
!---#[ function brack_5:
   pure function brack_5(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd43h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd43
      complex(ki) :: brack
      brack=0.0_ki
   end function brack_5
!---#] function brack_5:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3,i4) result(numerator)
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd43h3
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
      qshift = 0
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
!---#[ subroutine reconstruct_d43:
   subroutine     reconstruct_d43(coeffs)
      use p4_ubaru_hepemg_groups, only: tensrec_info_group4
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group4), intent(out) :: coeffs
      ! rank 4 case :
      !---[# reconstruct coeffs%coeffs_43:
      coeffs%coeffs_43%c0 = derivative(czip)
      coeffs%coeffs_43%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_43%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_43%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_43%c1(1,4) = derivative(czip,1,1,1,1)/ 24.0_ki
      coeffs%coeffs_43%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_43%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_43%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_43%c1(2,4) = derivative(czip,2,2,2,2)/ 24.0_ki
      coeffs%coeffs_43%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_43%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_43%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_43%c1(3,4) = derivative(czip,3,3,3,3)/ 24.0_ki
      coeffs%coeffs_43%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_43%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_43%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_43%c1(4,4) = derivative(czip,4,4,4,4)/ 24.0_ki
      coeffs%coeffs_43%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_43%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_43%c2(1,3) = -derivative(czip,1,2,2,2)/ 6.0_ki
      coeffs%coeffs_43%c2(1,4) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_43%c2(1,5) = derivative(czip,1,1,2,2)/ 4.0_ki
      coeffs%coeffs_43%c2(1,6) = -derivative(czip,1,1,1,2)/ 6.0_ki
      coeffs%coeffs_43%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_43%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_43%c2(2,3) = -derivative(czip,1,3,3,3)/ 6.0_ki
      coeffs%coeffs_43%c2(2,4) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_43%c2(2,5) = derivative(czip,1,1,3,3)/ 4.0_ki
      coeffs%coeffs_43%c2(2,6) = -derivative(czip,1,1,1,3)/ 6.0_ki
      coeffs%coeffs_43%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_43%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_43%c2(3,3) = -derivative(czip,1,4,4,4)/ 6.0_ki
      coeffs%coeffs_43%c2(3,4) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_43%c2(3,5) = derivative(czip,1,1,4,4)/ 4.0_ki
      coeffs%coeffs_43%c2(3,6) = -derivative(czip,1,1,1,4)/ 6.0_ki
      coeffs%coeffs_43%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_43%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_43%c2(4,3) = derivative(czip,2,3,3,3)/ 6.0_ki
      coeffs%coeffs_43%c2(4,4) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_43%c2(4,5) = derivative(czip,2,2,3,3)/ 4.0_ki
      coeffs%coeffs_43%c2(4,6) = derivative(czip,2,2,2,3)/ 6.0_ki
      coeffs%coeffs_43%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_43%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_43%c2(5,3) = derivative(czip,2,4,4,4)/ 6.0_ki
      coeffs%coeffs_43%c2(5,4) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_43%c2(5,5) = derivative(czip,2,2,4,4)/ 4.0_ki
      coeffs%coeffs_43%c2(5,6) = derivative(czip,2,2,2,4)/ 6.0_ki
      coeffs%coeffs_43%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_43%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_43%c2(6,3) = derivative(czip,3,4,4,4)/ 6.0_ki
      coeffs%coeffs_43%c2(6,4) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_43%c2(6,5) = derivative(czip,3,3,4,4)/ 4.0_ki
      coeffs%coeffs_43%c2(6,6) = derivative(czip,3,3,3,4)/ 6.0_ki
      coeffs%coeffs_43%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_43%c3(1,2) = -derivative(czip,1,2,3,3)/ 2.0_ki
      coeffs%coeffs_43%c3(1,3) = -derivative(czip,1,2,2,3)/ 2.0_ki
      coeffs%coeffs_43%c3(1,4) = derivative(czip,1,1,2,3)/ 2.0_ki
      coeffs%coeffs_43%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_43%c3(2,2) = -derivative(czip,1,2,4,4)/ 2.0_ki
      coeffs%coeffs_43%c3(2,3) = -derivative(czip,1,2,2,4)/ 2.0_ki
      coeffs%coeffs_43%c3(2,4) = derivative(czip,1,1,2,4)/ 2.0_ki
      coeffs%coeffs_43%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_43%c3(3,2) = -derivative(czip,1,3,4,4)/ 2.0_ki
      coeffs%coeffs_43%c3(3,3) = -derivative(czip,1,3,3,4)/ 2.0_ki
      coeffs%coeffs_43%c3(3,4) = derivative(czip,1,1,3,4)/ 2.0_ki
      coeffs%coeffs_43%c3(4,1) = -derivative(czip,2,3,4)
      coeffs%coeffs_43%c3(4,2) = derivative(czip,2,3,4,4)/ 2.0_ki
      coeffs%coeffs_43%c3(4,3) = derivative(czip,2,3,3,4)/ 2.0_ki
      coeffs%coeffs_43%c3(4,4) = derivative(czip,2,2,3,4)/ 2.0_ki
      coeffs%coeffs_43%c4(1,1) = -derivative(czip,1,2,3,4)
      !---#] reconstruct coeffs%coeffs_43:
   end subroutine reconstruct_d43
!---#] subroutine reconstruct_d43:
end module     p4_ubaru_hepemg_d43h3l1d
