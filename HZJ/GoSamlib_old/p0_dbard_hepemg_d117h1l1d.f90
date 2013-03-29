module     p0_dbard_hepemg_d117h1l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p0_db &
   ! &ard_hepemg/helicity1d117h1l1d.f90
   ! generator: buildfortran_d.py
   use p0_dbard_hepemg_config, only: ki
   use p0_dbard_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d117
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd117h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd117
      complex(ki) :: brack
      brack=0.0_ki
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd117h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(36) :: acd117
      complex(ki) :: brack
      acd117(1)=k1(iv1)
      acd117(2)=abb117(36)
      acd117(3)=k2(iv1)
      acd117(4)=abb117(33)
      acd117(5)=k6(iv1)
      acd117(6)=abb117(17)
      acd117(7)=e6(iv1)
      acd117(8)=abb117(21)
      acd117(9)=spvak2k1(iv1)
      acd117(10)=abb117(32)
      acd117(11)=spvak2k4(iv1)
      acd117(12)=abb117(46)
      acd117(13)=spvak5k1(iv1)
      acd117(14)=abb117(12)
      acd117(15)=spvak5k4(iv1)
      acd117(16)=abb117(19)
      acd117(17)=spvak5k6(iv1)
      acd117(18)=abb117(63)
      acd117(19)=spvak6k4(iv1)
      acd117(20)=abb117(61)
      acd117(21)=spvae6k1(iv1)
      acd117(22)=abb117(40)
      acd117(23)=spvak2e6(iv1)
      acd117(24)=abb117(54)
      acd117(25)=-acd117(23)*acd117(24)
      acd117(26)=-acd117(21)*acd117(22)
      acd117(27)=-acd117(19)*acd117(20)
      acd117(28)=-acd117(17)*acd117(18)
      acd117(29)=-acd117(15)*acd117(16)
      acd117(30)=-acd117(13)*acd117(14)
      acd117(31)=-acd117(11)*acd117(12)
      acd117(32)=-acd117(9)*acd117(10)
      acd117(33)=-acd117(7)*acd117(8)
      acd117(34)=-acd117(5)*acd117(6)
      acd117(35)=-acd117(3)*acd117(4)
      acd117(36)=-acd117(1)*acd117(2)
      brack=acd117(25)+acd117(26)+acd117(27)+acd117(28)+acd117(29)+acd117(30)+a&
      &cd117(31)+acd117(32)+acd117(33)+acd117(34)+acd117(35)+acd117(36)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd117h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(75) :: acd117
      complex(ki) :: brack
      acd117(1)=k1(iv1)
      acd117(2)=e6(iv2)
      acd117(3)=abb117(38)
      acd117(4)=spvae6k1(iv2)
      acd117(5)=abb117(57)
      acd117(6)=spvak2e6(iv2)
      acd117(7)=abb117(55)
      acd117(8)=spvae6k4(iv2)
      acd117(9)=abb117(49)
      acd117(10)=spvak5e6(iv2)
      acd117(11)=abb117(44)
      acd117(12)=k1(iv2)
      acd117(13)=e6(iv1)
      acd117(14)=spvae6k1(iv1)
      acd117(15)=spvak2e6(iv1)
      acd117(16)=spvae6k4(iv1)
      acd117(17)=spvak5e6(iv1)
      acd117(18)=k2(iv1)
      acd117(19)=k2(iv2)
      acd117(20)=k6(iv1)
      acd117(21)=spvak2k1(iv2)
      acd117(22)=abb117(30)
      acd117(23)=spvak2k4(iv2)
      acd117(24)=abb117(26)
      acd117(25)=spvak5k1(iv2)
      acd117(26)=abb117(22)
      acd117(27)=k6(iv2)
      acd117(28)=spvak2k1(iv1)
      acd117(29)=spvak2k4(iv1)
      acd117(30)=spvak5k1(iv1)
      acd117(31)=abb117(35)
      acd117(32)=abb117(24)
      acd117(33)=abb117(13)
      acd117(34)=spvak2k6(iv2)
      acd117(35)=abb117(43)
      acd117(36)=spvak6k1(iv2)
      acd117(37)=abb117(37)
      acd117(38)=spvak2k6(iv1)
      acd117(39)=spvak6k1(iv1)
      acd117(40)=abb117(41)
      acd117(41)=spvak5k4(iv2)
      acd117(42)=abb117(27)
      acd117(43)=spvak5k4(iv1)
      acd117(44)=abb117(18)
      acd117(45)=abb117(25)
      acd117(46)=abb117(20)
      acd117(47)=abb117(16)
      acd117(48)=spvak5k6(iv2)
      acd117(49)=abb117(34)
      acd117(50)=spvak6k4(iv2)
      acd117(51)=abb117(39)
      acd117(52)=spvak1e6(iv2)
      acd117(53)=abb117(42)
      acd117(54)=spvae6k2(iv2)
      acd117(55)=abb117(23)
      acd117(56)=spvak5k6(iv1)
      acd117(57)=spvak6k4(iv1)
      acd117(58)=spvak1e6(iv1)
      acd117(59)=spvae6k2(iv1)
      acd117(60)=abb117(50)
      acd117(61)=abb117(29)
      acd117(62)=acd117(55)*acd117(54)
      acd117(63)=acd117(53)*acd117(52)
      acd117(64)=acd117(51)*acd117(50)
      acd117(65)=acd117(49)*acd117(48)
      acd117(66)=acd117(10)*acd117(47)
      acd117(67)=acd117(8)*acd117(46)
      acd117(68)=acd117(27)*acd117(22)
      acd117(69)=acd117(6)*acd117(44)
      acd117(70)=acd117(4)*acd117(40)
      acd117(71)=acd117(2)*acd117(31)
      acd117(62)=acd117(71)+acd117(70)+acd117(69)+acd117(68)+acd117(67)+acd117(&
      &66)+acd117(65)+acd117(64)+acd117(62)+acd117(63)
      acd117(62)=acd117(28)*acd117(62)
      acd117(63)=acd117(55)*acd117(59)
      acd117(64)=acd117(53)*acd117(58)
      acd117(65)=acd117(51)*acd117(57)
      acd117(66)=acd117(49)*acd117(56)
      acd117(67)=acd117(17)*acd117(47)
      acd117(68)=acd117(16)*acd117(46)
      acd117(69)=acd117(20)*acd117(22)
      acd117(70)=acd117(15)*acd117(44)
      acd117(71)=acd117(14)*acd117(40)
      acd117(72)=acd117(13)*acd117(31)
      acd117(63)=acd117(72)+acd117(71)+acd117(70)+acd117(69)+acd117(68)+acd117(&
      &67)+acd117(66)+acd117(65)+acd117(63)+acd117(64)
      acd117(63)=acd117(21)*acd117(63)
      acd117(64)=acd117(12)+acd117(19)
      acd117(65)=-acd117(3)*acd117(64)
      acd117(66)=acd117(36)*acd117(37)
      acd117(67)=acd117(34)*acd117(35)
      acd117(68)=acd117(25)*acd117(33)
      acd117(69)=acd117(23)*acd117(32)
      acd117(65)=acd117(69)+acd117(68)+acd117(66)+acd117(67)+acd117(65)
      acd117(65)=acd117(13)*acd117(65)
      acd117(66)=acd117(1)+acd117(18)
      acd117(67)=-acd117(3)*acd117(66)
      acd117(68)=acd117(37)*acd117(39)
      acd117(69)=acd117(35)*acd117(38)
      acd117(70)=acd117(30)*acd117(33)
      acd117(71)=acd117(29)*acd117(32)
      acd117(67)=acd117(71)+acd117(70)+acd117(68)+acd117(69)+acd117(67)
      acd117(67)=acd117(2)*acd117(67)
      acd117(68)=acd117(36)*acd117(61)
      acd117(69)=acd117(34)*acd117(60)
      acd117(70)=acd117(6)*acd117(45)
      acd117(71)=acd117(4)*acd117(42)
      acd117(68)=acd117(71)+acd117(70)+acd117(68)+acd117(69)
      acd117(68)=acd117(43)*acd117(68)
      acd117(69)=acd117(39)*acd117(61)
      acd117(70)=acd117(38)*acd117(60)
      acd117(71)=acd117(15)*acd117(45)
      acd117(72)=acd117(14)*acd117(42)
      acd117(69)=acd117(72)+acd117(71)+acd117(69)+acd117(70)
      acd117(69)=acd117(41)*acd117(69)
      acd117(70)=acd117(11)*acd117(17)
      acd117(71)=acd117(9)*acd117(16)
      acd117(70)=acd117(70)-acd117(71)
      acd117(70)=-acd117(70)*acd117(64)
      acd117(71)=acd117(11)*acd117(10)
      acd117(72)=acd117(9)*acd117(8)
      acd117(71)=acd117(71)-acd117(72)
      acd117(71)=-acd117(71)*acd117(66)
      acd117(72)=acd117(26)*acd117(30)
      acd117(73)=acd117(24)*acd117(29)
      acd117(72)=acd117(72)+acd117(73)
      acd117(72)=acd117(27)*acd117(72)
      acd117(73)=acd117(25)*acd117(26)
      acd117(74)=acd117(23)*acd117(24)
      acd117(73)=acd117(73)+acd117(74)
      acd117(73)=acd117(20)*acd117(73)
      acd117(64)=acd117(27)+acd117(64)
      acd117(74)=-acd117(15)*acd117(64)
      acd117(66)=acd117(20)+acd117(66)
      acd117(75)=-acd117(6)*acd117(66)
      acd117(74)=acd117(75)+acd117(74)
      acd117(74)=acd117(7)*acd117(74)
      acd117(64)=acd117(14)*acd117(64)
      acd117(66)=acd117(4)*acd117(66)
      acd117(64)=acd117(66)+acd117(64)
      acd117(64)=acd117(5)*acd117(64)
      brack=acd117(62)+acd117(63)+acd117(64)+acd117(65)+acd117(67)+acd117(68)+a&
      &cd117(69)+acd117(70)+acd117(71)+acd117(72)+acd117(73)+acd117(74)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd117h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(45) :: acd117
      complex(ki) :: brack
      acd117(1)=d(iv1,iv2)
      acd117(2)=e6(iv3)
      acd117(3)=abb117(28)
      acd117(4)=spvak2k1(iv3)
      acd117(5)=abb117(14)
      acd117(6)=spvak2k4(iv3)
      acd117(7)=abb117(31)
      acd117(8)=spvak5k1(iv3)
      acd117(9)=abb117(47)
      acd117(10)=spvae6k1(iv3)
      acd117(11)=abb117(57)
      acd117(12)=spvak2e6(iv3)
      acd117(13)=abb117(55)
      acd117(14)=spvae6k4(iv3)
      acd117(15)=abb117(52)
      acd117(16)=spvak5e6(iv3)
      acd117(17)=abb117(48)
      acd117(18)=d(iv1,iv3)
      acd117(19)=e6(iv2)
      acd117(20)=spvak2k1(iv2)
      acd117(21)=spvak2k4(iv2)
      acd117(22)=spvak5k1(iv2)
      acd117(23)=spvae6k1(iv2)
      acd117(24)=spvak2e6(iv2)
      acd117(25)=spvae6k4(iv2)
      acd117(26)=spvak5e6(iv2)
      acd117(27)=d(iv2,iv3)
      acd117(28)=e6(iv1)
      acd117(29)=spvak2k1(iv1)
      acd117(30)=spvak2k4(iv1)
      acd117(31)=spvak5k1(iv1)
      acd117(32)=spvae6k1(iv1)
      acd117(33)=spvak2e6(iv1)
      acd117(34)=spvae6k4(iv1)
      acd117(35)=spvak5e6(iv1)
      acd117(36)=-acd117(2)*acd117(3)
      acd117(37)=-acd117(4)*acd117(5)
      acd117(38)=-acd117(6)*acd117(7)
      acd117(39)=-acd117(8)*acd117(9)
      acd117(40)=-acd117(10)*acd117(11)
      acd117(41)=acd117(12)*acd117(13)
      acd117(42)=-acd117(14)*acd117(15)
      acd117(43)=-acd117(16)*acd117(17)
      acd117(36)=acd117(43)+acd117(42)+acd117(41)+acd117(40)+acd117(39)+acd117(&
      &38)+acd117(36)+acd117(37)
      acd117(36)=acd117(1)*acd117(36)
      acd117(37)=-acd117(19)*acd117(3)
      acd117(38)=-acd117(20)*acd117(5)
      acd117(39)=-acd117(21)*acd117(7)
      acd117(40)=-acd117(22)*acd117(9)
      acd117(41)=-acd117(23)*acd117(11)
      acd117(42)=acd117(24)*acd117(13)
      acd117(43)=-acd117(25)*acd117(15)
      acd117(44)=-acd117(26)*acd117(17)
      acd117(37)=acd117(44)+acd117(43)+acd117(42)+acd117(41)+acd117(40)+acd117(&
      &39)+acd117(38)+acd117(37)
      acd117(37)=acd117(18)*acd117(37)
      acd117(38)=-acd117(28)*acd117(3)
      acd117(39)=-acd117(29)*acd117(5)
      acd117(40)=-acd117(30)*acd117(7)
      acd117(41)=-acd117(31)*acd117(9)
      acd117(42)=-acd117(32)*acd117(11)
      acd117(43)=acd117(33)*acd117(13)
      acd117(44)=-acd117(34)*acd117(15)
      acd117(45)=-acd117(35)*acd117(17)
      acd117(38)=acd117(45)+acd117(44)+acd117(43)+acd117(42)+acd117(41)+acd117(&
      &40)+acd117(39)+acd117(38)
      acd117(38)=acd117(27)*acd117(38)
      acd117(36)=acd117(38)+acd117(37)+acd117(36)
      brack=2.0_ki*acd117(36)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd117h1
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
!---#[ subroutine reconstruct_d117:
   subroutine     reconstruct_d117(coeffs)
      use p0_dbard_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_117:
      coeffs%coeffs_117%c0 = derivative(czip)
      coeffs%coeffs_117%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_117%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_117%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_117%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_117%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_117%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_117%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_117%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_117%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_117%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_117%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_117%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_117%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_117%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_117%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_117%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_117%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_117%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_117%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_117%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_117%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_117%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_117%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_117%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_117%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_117%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_117%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_117%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_117%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_117%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_117%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_117%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_117%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_117%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_117:
   end subroutine reconstruct_d117
!---#] subroutine reconstruct_d117:
end module     p0_dbard_hepemg_d117h1l1d
