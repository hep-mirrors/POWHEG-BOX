module     p12_sbars_hepemg_d145h1l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity1d145h1l1d.f90
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
   public :: derivative , reconstruct_d145
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd145h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd145
      complex(ki) :: brack
      brack=0.0_ki
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd145h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(36) :: acd145
      complex(ki) :: brack
      acd145(1)=k1(iv1)
      acd145(2)=abb145(28)
      acd145(3)=k2(iv1)
      acd145(4)=abb145(39)
      acd145(5)=k6(iv1)
      acd145(6)=abb145(31)
      acd145(7)=e6(iv1)
      acd145(8)=abb145(21)
      acd145(9)=spvak2k1(iv1)
      acd145(10)=abb145(15)
      acd145(11)=spvak2k4(iv1)
      acd145(12)=abb145(46)
      acd145(13)=spvak5k1(iv1)
      acd145(14)=abb145(37)
      acd145(15)=spvak5k4(iv1)
      acd145(16)=abb145(54)
      acd145(17)=spvak5k6(iv1)
      acd145(18)=abb145(43)
      acd145(19)=spvak6k4(iv1)
      acd145(20)=abb145(50)
      acd145(21)=spvae6k1(iv1)
      acd145(22)=abb145(24)
      acd145(23)=spvak2e6(iv1)
      acd145(24)=abb145(48)
      acd145(25)=-acd145(23)*acd145(24)
      acd145(26)=-acd145(21)*acd145(22)
      acd145(27)=-acd145(19)*acd145(20)
      acd145(28)=-acd145(17)*acd145(18)
      acd145(29)=-acd145(15)*acd145(16)
      acd145(30)=-acd145(13)*acd145(14)
      acd145(31)=-acd145(11)*acd145(12)
      acd145(32)=-acd145(9)*acd145(10)
      acd145(33)=-acd145(7)*acd145(8)
      acd145(34)=-acd145(5)*acd145(6)
      acd145(35)=-acd145(3)*acd145(4)
      acd145(36)=-acd145(1)*acd145(2)
      brack=acd145(25)+acd145(26)+acd145(27)+acd145(28)+acd145(29)+acd145(30)+a&
      &cd145(31)+acd145(32)+acd145(33)+acd145(34)+acd145(35)+acd145(36)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd145h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(75) :: acd145
      complex(ki) :: brack
      acd145(1)=k1(iv1)
      acd145(2)=e6(iv2)
      acd145(3)=abb145(42)
      acd145(4)=spvae6k1(iv2)
      acd145(5)=abb145(30)
      acd145(6)=spvak2e6(iv2)
      acd145(7)=abb145(34)
      acd145(8)=spvae6k4(iv2)
      acd145(9)=abb145(45)
      acd145(10)=spvak5e6(iv2)
      acd145(11)=abb145(25)
      acd145(12)=k1(iv2)
      acd145(13)=e6(iv1)
      acd145(14)=spvae6k1(iv1)
      acd145(15)=spvak2e6(iv1)
      acd145(16)=spvae6k4(iv1)
      acd145(17)=spvak5e6(iv1)
      acd145(18)=k2(iv1)
      acd145(19)=k2(iv2)
      acd145(20)=k6(iv1)
      acd145(21)=spvak2k1(iv2)
      acd145(22)=abb145(32)
      acd145(23)=spvak2k4(iv2)
      acd145(24)=abb145(60)
      acd145(25)=spvak5k1(iv2)
      acd145(26)=abb145(53)
      acd145(27)=k6(iv2)
      acd145(28)=spvak2k1(iv1)
      acd145(29)=spvak2k4(iv1)
      acd145(30)=spvak5k1(iv1)
      acd145(31)=abb145(27)
      acd145(32)=abb145(20)
      acd145(33)=abb145(22)
      acd145(34)=spvak2k6(iv2)
      acd145(35)=abb145(59)
      acd145(36)=spvak6k1(iv2)
      acd145(37)=abb145(52)
      acd145(38)=spvak2k6(iv1)
      acd145(39)=spvak6k1(iv1)
      acd145(40)=abb145(29)
      acd145(41)=spvak5k4(iv2)
      acd145(42)=abb145(55)
      acd145(43)=spvak5k4(iv1)
      acd145(44)=abb145(12)
      acd145(45)=abb145(26)
      acd145(46)=abb145(18)
      acd145(47)=abb145(35)
      acd145(48)=spvak5k6(iv2)
      acd145(49)=abb145(38)
      acd145(50)=spvak6k4(iv2)
      acd145(51)=abb145(17)
      acd145(52)=spvak1e6(iv2)
      acd145(53)=abb145(19)
      acd145(54)=spvae6k2(iv2)
      acd145(55)=abb145(16)
      acd145(56)=spvak5k6(iv1)
      acd145(57)=spvak6k4(iv1)
      acd145(58)=spvak1e6(iv1)
      acd145(59)=spvae6k2(iv1)
      acd145(60)=abb145(56)
      acd145(61)=abb145(49)
      acd145(62)=acd145(55)*acd145(54)
      acd145(63)=acd145(53)*acd145(52)
      acd145(64)=acd145(51)*acd145(50)
      acd145(65)=acd145(49)*acd145(48)
      acd145(66)=acd145(10)*acd145(47)
      acd145(67)=acd145(8)*acd145(46)
      acd145(68)=acd145(27)*acd145(22)
      acd145(69)=acd145(6)*acd145(44)
      acd145(70)=acd145(4)*acd145(40)
      acd145(71)=acd145(2)*acd145(31)
      acd145(62)=acd145(71)+acd145(70)+acd145(69)+acd145(68)+acd145(67)+acd145(&
      &66)+acd145(65)+acd145(64)+acd145(62)+acd145(63)
      acd145(62)=acd145(28)*acd145(62)
      acd145(63)=acd145(55)*acd145(59)
      acd145(64)=acd145(53)*acd145(58)
      acd145(65)=acd145(51)*acd145(57)
      acd145(66)=acd145(49)*acd145(56)
      acd145(67)=acd145(17)*acd145(47)
      acd145(68)=acd145(16)*acd145(46)
      acd145(69)=acd145(20)*acd145(22)
      acd145(70)=acd145(15)*acd145(44)
      acd145(71)=acd145(14)*acd145(40)
      acd145(72)=acd145(13)*acd145(31)
      acd145(63)=acd145(72)+acd145(71)+acd145(70)+acd145(69)+acd145(68)+acd145(&
      &67)+acd145(66)+acd145(65)+acd145(63)+acd145(64)
      acd145(63)=acd145(21)*acd145(63)
      acd145(64)=acd145(12)+acd145(19)
      acd145(65)=-acd145(3)*acd145(64)
      acd145(66)=acd145(36)*acd145(37)
      acd145(67)=acd145(34)*acd145(35)
      acd145(68)=acd145(25)*acd145(33)
      acd145(69)=acd145(23)*acd145(32)
      acd145(65)=acd145(69)+acd145(68)+acd145(66)+acd145(67)+acd145(65)
      acd145(65)=acd145(13)*acd145(65)
      acd145(66)=acd145(1)+acd145(18)
      acd145(67)=-acd145(3)*acd145(66)
      acd145(68)=acd145(37)*acd145(39)
      acd145(69)=acd145(35)*acd145(38)
      acd145(70)=acd145(30)*acd145(33)
      acd145(71)=acd145(29)*acd145(32)
      acd145(67)=acd145(71)+acd145(70)+acd145(68)+acd145(69)+acd145(67)
      acd145(67)=acd145(2)*acd145(67)
      acd145(68)=acd145(36)*acd145(61)
      acd145(69)=acd145(34)*acd145(60)
      acd145(70)=acd145(6)*acd145(45)
      acd145(71)=acd145(4)*acd145(42)
      acd145(68)=acd145(71)+acd145(70)+acd145(68)+acd145(69)
      acd145(68)=acd145(43)*acd145(68)
      acd145(69)=acd145(39)*acd145(61)
      acd145(70)=acd145(38)*acd145(60)
      acd145(71)=acd145(15)*acd145(45)
      acd145(72)=acd145(14)*acd145(42)
      acd145(69)=acd145(72)+acd145(71)+acd145(69)+acd145(70)
      acd145(69)=acd145(41)*acd145(69)
      acd145(70)=acd145(11)*acd145(17)
      acd145(71)=acd145(9)*acd145(16)
      acd145(70)=acd145(70)-acd145(71)
      acd145(70)=-acd145(70)*acd145(64)
      acd145(71)=acd145(11)*acd145(10)
      acd145(72)=acd145(9)*acd145(8)
      acd145(71)=acd145(71)-acd145(72)
      acd145(71)=-acd145(71)*acd145(66)
      acd145(72)=acd145(26)*acd145(30)
      acd145(73)=acd145(24)*acd145(29)
      acd145(72)=acd145(72)+acd145(73)
      acd145(72)=acd145(27)*acd145(72)
      acd145(73)=acd145(25)*acd145(26)
      acd145(74)=acd145(23)*acd145(24)
      acd145(73)=acd145(73)+acd145(74)
      acd145(73)=acd145(20)*acd145(73)
      acd145(64)=acd145(27)+acd145(64)
      acd145(74)=-acd145(15)*acd145(64)
      acd145(66)=acd145(20)+acd145(66)
      acd145(75)=-acd145(6)*acd145(66)
      acd145(74)=acd145(75)+acd145(74)
      acd145(74)=acd145(7)*acd145(74)
      acd145(64)=acd145(14)*acd145(64)
      acd145(66)=acd145(4)*acd145(66)
      acd145(64)=acd145(66)+acd145(64)
      acd145(64)=acd145(5)*acd145(64)
      brack=acd145(62)+acd145(63)+acd145(64)+acd145(65)+acd145(67)+acd145(68)+a&
      &cd145(69)+acd145(70)+acd145(71)+acd145(72)+acd145(73)+acd145(74)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd145h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(45) :: acd145
      complex(ki) :: brack
      acd145(1)=d(iv1,iv2)
      acd145(2)=e6(iv3)
      acd145(3)=abb145(36)
      acd145(4)=spvak2k1(iv3)
      acd145(5)=abb145(33)
      acd145(6)=spvak2k4(iv3)
      acd145(7)=abb145(61)
      acd145(8)=spvak5k1(iv3)
      acd145(9)=abb145(23)
      acd145(10)=spvae6k1(iv3)
      acd145(11)=abb145(30)
      acd145(12)=spvak2e6(iv3)
      acd145(13)=abb145(34)
      acd145(14)=spvae6k4(iv3)
      acd145(15)=abb145(14)
      acd145(16)=spvak5e6(iv3)
      acd145(17)=abb145(44)
      acd145(18)=d(iv1,iv3)
      acd145(19)=e6(iv2)
      acd145(20)=spvak2k1(iv2)
      acd145(21)=spvak2k4(iv2)
      acd145(22)=spvak5k1(iv2)
      acd145(23)=spvae6k1(iv2)
      acd145(24)=spvak2e6(iv2)
      acd145(25)=spvae6k4(iv2)
      acd145(26)=spvak5e6(iv2)
      acd145(27)=d(iv2,iv3)
      acd145(28)=e6(iv1)
      acd145(29)=spvak2k1(iv1)
      acd145(30)=spvak2k4(iv1)
      acd145(31)=spvak5k1(iv1)
      acd145(32)=spvae6k1(iv1)
      acd145(33)=spvak2e6(iv1)
      acd145(34)=spvae6k4(iv1)
      acd145(35)=spvak5e6(iv1)
      acd145(36)=-acd145(2)*acd145(3)
      acd145(37)=-acd145(4)*acd145(5)
      acd145(38)=-acd145(6)*acd145(7)
      acd145(39)=-acd145(8)*acd145(9)
      acd145(40)=-acd145(10)*acd145(11)
      acd145(41)=acd145(12)*acd145(13)
      acd145(42)=-acd145(14)*acd145(15)
      acd145(43)=-acd145(16)*acd145(17)
      acd145(36)=acd145(43)+acd145(42)+acd145(41)+acd145(40)+acd145(39)+acd145(&
      &38)+acd145(36)+acd145(37)
      acd145(36)=acd145(1)*acd145(36)
      acd145(37)=-acd145(19)*acd145(3)
      acd145(38)=-acd145(20)*acd145(5)
      acd145(39)=-acd145(21)*acd145(7)
      acd145(40)=-acd145(22)*acd145(9)
      acd145(41)=-acd145(23)*acd145(11)
      acd145(42)=acd145(24)*acd145(13)
      acd145(43)=-acd145(25)*acd145(15)
      acd145(44)=-acd145(26)*acd145(17)
      acd145(37)=acd145(44)+acd145(43)+acd145(42)+acd145(41)+acd145(40)+acd145(&
      &39)+acd145(38)+acd145(37)
      acd145(37)=acd145(18)*acd145(37)
      acd145(38)=-acd145(28)*acd145(3)
      acd145(39)=-acd145(29)*acd145(5)
      acd145(40)=-acd145(30)*acd145(7)
      acd145(41)=-acd145(31)*acd145(9)
      acd145(42)=-acd145(32)*acd145(11)
      acd145(43)=acd145(33)*acd145(13)
      acd145(44)=-acd145(34)*acd145(15)
      acd145(45)=-acd145(35)*acd145(17)
      acd145(38)=acd145(45)+acd145(44)+acd145(43)+acd145(42)+acd145(41)+acd145(&
      &40)+acd145(39)+acd145(38)
      acd145(38)=acd145(27)*acd145(38)
      acd145(36)=acd145(38)+acd145(37)+acd145(36)
      brack=2.0_ki*acd145(36)
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd145h1
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
!---#[ subroutine reconstruct_d145:
   subroutine     reconstruct_d145(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_145:
      coeffs%coeffs_145%c0 = derivative(czip)
      coeffs%coeffs_145%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_145%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_145%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_145%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_145%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_145%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_145%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_145%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_145%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_145%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_145%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_145%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_145%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_145%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_145%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_145%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_145%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_145%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_145%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_145%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_145%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_145%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_145%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_145%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_145%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_145%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_145%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_145%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_145%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_145%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_145%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_145%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_145%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_145%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_145:
   end subroutine reconstruct_d145
!---#] subroutine reconstruct_d145:
end module     p12_sbars_hepemg_d145h1l1d
