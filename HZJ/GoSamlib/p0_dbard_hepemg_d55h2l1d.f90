module     p0_dbard_hepemg_d55h2l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p0_dbard_hepemg/helicity2d55h2l1d.f90
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
   integer, private :: iv4
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d55
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd55h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd55
      complex(ki) :: brack
      acd55(1)=abb55(20)
      brack=acd55(1)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd55h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(51) :: acd55
      complex(ki) :: brack
      acd55(1)=k1(iv1)
      acd55(2)=abb55(17)
      acd55(3)=k2(iv1)
      acd55(4)=abb55(19)
      acd55(5)=l3(iv1)
      acd55(6)=abb55(50)
      acd55(7)=k6(iv1)
      acd55(8)=abb55(13)
      acd55(9)=e6(iv1)
      acd55(10)=abb55(23)
      acd55(11)=spvak1k2(iv1)
      acd55(12)=abb55(9)
      acd55(13)=spvak1k5(iv1)
      acd55(14)=abb55(21)
      acd55(15)=spvak1k6(iv1)
      acd55(16)=abb55(10)
      acd55(17)=spvak4k2(iv1)
      acd55(18)=abb55(24)
      acd55(19)=spvak4k5(iv1)
      acd55(20)=abb55(39)
      acd55(21)=spvak4k6(iv1)
      acd55(22)=abb55(67)
      acd55(23)=spvak6k2(iv1)
      acd55(24)=abb55(15)
      acd55(25)=spvak6k5(iv1)
      acd55(26)=abb55(63)
      acd55(27)=spvak1e6(iv1)
      acd55(28)=abb55(26)
      acd55(29)=spvae6k2(iv1)
      acd55(30)=abb55(60)
      acd55(31)=spvak4e6(iv1)
      acd55(32)=abb55(12)
      acd55(33)=spvae6k5(iv1)
      acd55(34)=abb55(28)
      acd55(35)=acd55(33)*acd55(34)
      acd55(36)=acd55(31)*acd55(32)
      acd55(37)=acd55(29)*acd55(30)
      acd55(38)=acd55(27)*acd55(28)
      acd55(39)=acd55(25)*acd55(26)
      acd55(40)=acd55(23)*acd55(24)
      acd55(41)=acd55(21)*acd55(22)
      acd55(42)=acd55(19)*acd55(20)
      acd55(43)=acd55(17)*acd55(18)
      acd55(44)=acd55(15)*acd55(16)
      acd55(45)=acd55(13)*acd55(14)
      acd55(46)=acd55(11)*acd55(12)
      acd55(47)=acd55(9)*acd55(10)
      acd55(48)=acd55(7)*acd55(8)
      acd55(49)=acd55(5)*acd55(6)
      acd55(50)=acd55(3)*acd55(4)
      acd55(51)=acd55(1)*acd55(2)
      brack=acd55(35)+acd55(36)+acd55(37)+acd55(38)+acd55(39)+acd55(40)+acd55(4&
      &1)+acd55(42)+acd55(43)+acd55(44)+acd55(45)+acd55(46)+acd55(47)+acd55(48)&
      &+acd55(49)+acd55(50)+acd55(51)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd55h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(65) :: acd55
      complex(ki) :: brack
      acd55(1)=d(iv1,iv2)
      acd55(2)=abb55(8)
      acd55(3)=k1(iv1)
      acd55(4)=e6(iv2)
      acd55(5)=abb55(53)
      acd55(6)=k1(iv2)
      acd55(7)=e6(iv1)
      acd55(8)=k2(iv1)
      acd55(9)=abb55(58)
      acd55(10)=k2(iv2)
      acd55(11)=k6(iv1)
      acd55(12)=abb55(45)
      acd55(13)=spvak1k5(iv2)
      acd55(14)=abb55(11)
      acd55(15)=spvak4k2(iv2)
      acd55(16)=abb55(6)
      acd55(17)=spvak4e6(iv2)
      acd55(18)=abb55(47)
      acd55(19)=spvae6k5(iv2)
      acd55(20)=abb55(36)
      acd55(21)=k6(iv2)
      acd55(22)=spvak1k5(iv1)
      acd55(23)=spvak4k2(iv1)
      acd55(24)=spvak4e6(iv1)
      acd55(25)=spvae6k5(iv1)
      acd55(26)=abb55(7)
      acd55(27)=abb55(42)
      acd55(28)=spvak1k2(iv2)
      acd55(29)=abb55(14)
      acd55(30)=spvak1k6(iv2)
      acd55(31)=abb55(40)
      acd55(32)=spvak6k2(iv2)
      acd55(33)=abb55(66)
      acd55(34)=spvak1k2(iv1)
      acd55(35)=spvak1k6(iv1)
      acd55(36)=spvak6k2(iv1)
      acd55(37)=abb55(30)
      acd55(38)=abb55(27)
      acd55(39)=spvak4k6(iv2)
      acd55(40)=abb55(29)
      acd55(41)=spvak6k5(iv2)
      acd55(42)=abb55(18)
      acd55(43)=spvak4k6(iv1)
      acd55(44)=spvak6k5(iv1)
      acd55(45)=spvak4k5(iv2)
      acd55(46)=abb55(32)
      acd55(47)=spvak4k5(iv1)
      acd55(48)=abb55(65)
      acd55(49)=spvak1e6(iv2)
      acd55(50)=abb55(37)
      acd55(51)=spvae6k2(iv2)
      acd55(52)=abb55(31)
      acd55(53)=spvak1e6(iv1)
      acd55(54)=spvae6k2(iv1)
      acd55(55)=acd55(32)*acd55(33)
      acd55(56)=acd55(30)*acd55(31)
      acd55(57)=acd55(15)*acd55(27)
      acd55(58)=acd55(13)*acd55(26)
      acd55(59)=acd55(9)*acd55(10)
      acd55(60)=acd55(5)*acd55(6)
      acd55(61)=acd55(28)*acd55(29)
      acd55(62)=acd55(21)*acd55(12)
      acd55(55)=acd55(62)+acd55(61)+acd55(60)+acd55(59)+acd55(58)+acd55(57)+acd&
      &55(55)+acd55(56)
      acd55(55)=acd55(7)*acd55(55)
      acd55(56)=acd55(33)*acd55(36)
      acd55(57)=acd55(31)*acd55(35)
      acd55(58)=acd55(23)*acd55(27)
      acd55(59)=acd55(22)*acd55(26)
      acd55(60)=acd55(9)*acd55(8)
      acd55(61)=acd55(5)*acd55(3)
      acd55(62)=acd55(34)*acd55(29)
      acd55(63)=acd55(11)*acd55(12)
      acd55(56)=acd55(63)+acd55(62)+acd55(61)+acd55(60)+acd55(59)+acd55(58)+acd&
      &55(56)+acd55(57)
      acd55(56)=acd55(4)*acd55(56)
      acd55(57)=-acd55(52)*acd55(51)
      acd55(58)=acd55(50)*acd55(49)
      acd55(59)=acd55(32)*acd55(48)
      acd55(60)=acd55(30)*acd55(46)
      acd55(57)=acd55(60)+acd55(59)+acd55(57)+acd55(58)
      acd55(57)=acd55(47)*acd55(57)
      acd55(58)=-acd55(52)*acd55(54)
      acd55(59)=acd55(50)*acd55(53)
      acd55(60)=acd55(36)*acd55(48)
      acd55(61)=acd55(35)*acd55(46)
      acd55(58)=acd55(61)+acd55(60)+acd55(58)+acd55(59)
      acd55(58)=acd55(45)*acd55(58)
      acd55(59)=acd55(42)*acd55(41)
      acd55(60)=acd55(40)*acd55(39)
      acd55(61)=acd55(19)*acd55(38)
      acd55(62)=acd55(17)*acd55(37)
      acd55(59)=acd55(62)+acd55(61)+acd55(59)+acd55(60)
      acd55(59)=acd55(34)*acd55(59)
      acd55(60)=acd55(42)*acd55(44)
      acd55(61)=acd55(40)*acd55(43)
      acd55(62)=acd55(25)*acd55(38)
      acd55(63)=acd55(24)*acd55(37)
      acd55(60)=acd55(63)+acd55(62)+acd55(60)+acd55(61)
      acd55(60)=acd55(28)*acd55(60)
      acd55(61)=acd55(20)*acd55(25)
      acd55(62)=acd55(18)*acd55(24)
      acd55(63)=acd55(16)*acd55(23)
      acd55(64)=acd55(14)*acd55(22)
      acd55(61)=acd55(64)+acd55(63)+acd55(61)+acd55(62)
      acd55(61)=acd55(21)*acd55(61)
      acd55(62)=acd55(19)*acd55(20)
      acd55(63)=acd55(17)*acd55(18)
      acd55(64)=acd55(15)*acd55(16)
      acd55(65)=acd55(13)*acd55(14)
      acd55(62)=acd55(65)+acd55(64)+acd55(62)+acd55(63)
      acd55(62)=acd55(11)*acd55(62)
      acd55(63)=acd55(1)*acd55(2)
      brack=acd55(55)+acd55(56)+acd55(57)+acd55(58)+acd55(59)+acd55(60)+acd55(6&
      &1)+acd55(62)+2.0_ki*acd55(63)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd55h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(45) :: acd55
      complex(ki) :: brack
      acd55(1)=d(iv1,iv2)
      acd55(2)=e6(iv3)
      acd55(3)=abb55(25)
      acd55(4)=spvak1k2(iv3)
      acd55(5)=abb55(16)
      acd55(6)=spvak1k5(iv3)
      acd55(7)=abb55(22)
      acd55(8)=spvak4k2(iv3)
      acd55(9)=abb55(46)
      acd55(10)=spvak1e6(iv3)
      acd55(11)=abb55(62)
      acd55(12)=spvae6k2(iv3)
      acd55(13)=abb55(61)
      acd55(14)=spvak4e6(iv3)
      acd55(15)=abb55(35)
      acd55(16)=spvae6k5(iv3)
      acd55(17)=abb55(33)
      acd55(18)=d(iv1,iv3)
      acd55(19)=e6(iv2)
      acd55(20)=spvak1k2(iv2)
      acd55(21)=spvak1k5(iv2)
      acd55(22)=spvak4k2(iv2)
      acd55(23)=spvak1e6(iv2)
      acd55(24)=spvae6k2(iv2)
      acd55(25)=spvak4e6(iv2)
      acd55(26)=spvae6k5(iv2)
      acd55(27)=d(iv2,iv3)
      acd55(28)=e6(iv1)
      acd55(29)=spvak1k2(iv1)
      acd55(30)=spvak1k5(iv1)
      acd55(31)=spvak4k2(iv1)
      acd55(32)=spvak1e6(iv1)
      acd55(33)=spvae6k2(iv1)
      acd55(34)=spvak4e6(iv1)
      acd55(35)=spvae6k5(iv1)
      acd55(36)=acd55(17)*acd55(35)
      acd55(37)=acd55(15)*acd55(34)
      acd55(38)=acd55(13)*acd55(33)
      acd55(39)=acd55(11)*acd55(32)
      acd55(40)=acd55(9)*acd55(31)
      acd55(41)=acd55(7)*acd55(30)
      acd55(42)=acd55(5)*acd55(29)
      acd55(43)=acd55(3)*acd55(28)
      acd55(36)=acd55(43)+acd55(42)+acd55(41)+acd55(40)+acd55(39)+acd55(38)+acd&
      &55(36)+acd55(37)
      acd55(36)=acd55(27)*acd55(36)
      acd55(37)=acd55(17)*acd55(26)
      acd55(38)=acd55(15)*acd55(25)
      acd55(39)=acd55(13)*acd55(24)
      acd55(40)=acd55(11)*acd55(23)
      acd55(41)=acd55(9)*acd55(22)
      acd55(42)=acd55(7)*acd55(21)
      acd55(43)=acd55(5)*acd55(20)
      acd55(44)=acd55(3)*acd55(19)
      acd55(37)=acd55(44)+acd55(43)+acd55(42)+acd55(41)+acd55(40)+acd55(39)+acd&
      &55(37)+acd55(38)
      acd55(37)=acd55(18)*acd55(37)
      acd55(38)=acd55(17)*acd55(16)
      acd55(39)=acd55(15)*acd55(14)
      acd55(40)=acd55(13)*acd55(12)
      acd55(41)=acd55(11)*acd55(10)
      acd55(42)=acd55(9)*acd55(8)
      acd55(43)=acd55(7)*acd55(6)
      acd55(44)=acd55(5)*acd55(4)
      acd55(45)=acd55(3)*acd55(2)
      acd55(38)=acd55(45)+acd55(44)+acd55(43)+acd55(42)+acd55(41)+acd55(40)+acd&
      &55(38)+acd55(39)
      acd55(38)=acd55(1)*acd55(38)
      acd55(36)=acd55(38)+acd55(36)+acd55(37)
      brack=2.0_ki*acd55(36)
   end function brack_4
!---#] function brack_4:
!---#[ function brack_5:
   pure function brack_5(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd55h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd55
      complex(ki) :: brack
      brack=0.0_ki
   end function brack_5
!---#] function brack_5:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3,i4) result(numerator)
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd55h2
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
!---#[ subroutine reconstruct_d55:
   subroutine     reconstruct_d55(coeffs)
      use p0_dbard_hepemg_groups, only: tensrec_info_group3
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group3), intent(out) :: coeffs
      ! rank 4 case :
      !---[# reconstruct coeffs%coeffs_55:
      coeffs%coeffs_55%c0 = derivative(czip)
      coeffs%coeffs_55%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_55%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_55%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_55%c1(1,4) = derivative(czip,1,1,1,1)/ 24.0_ki
      coeffs%coeffs_55%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_55%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_55%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_55%c1(2,4) = derivative(czip,2,2,2,2)/ 24.0_ki
      coeffs%coeffs_55%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_55%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_55%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_55%c1(3,4) = derivative(czip,3,3,3,3)/ 24.0_ki
      coeffs%coeffs_55%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_55%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_55%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_55%c1(4,4) = derivative(czip,4,4,4,4)/ 24.0_ki
      coeffs%coeffs_55%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_55%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_55%c2(1,3) = -derivative(czip,1,2,2,2)/ 6.0_ki
      coeffs%coeffs_55%c2(1,4) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_55%c2(1,5) = derivative(czip,1,1,2,2)/ 4.0_ki
      coeffs%coeffs_55%c2(1,6) = -derivative(czip,1,1,1,2)/ 6.0_ki
      coeffs%coeffs_55%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_55%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_55%c2(2,3) = -derivative(czip,1,3,3,3)/ 6.0_ki
      coeffs%coeffs_55%c2(2,4) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_55%c2(2,5) = derivative(czip,1,1,3,3)/ 4.0_ki
      coeffs%coeffs_55%c2(2,6) = -derivative(czip,1,1,1,3)/ 6.0_ki
      coeffs%coeffs_55%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_55%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_55%c2(3,3) = -derivative(czip,1,4,4,4)/ 6.0_ki
      coeffs%coeffs_55%c2(3,4) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_55%c2(3,5) = derivative(czip,1,1,4,4)/ 4.0_ki
      coeffs%coeffs_55%c2(3,6) = -derivative(czip,1,1,1,4)/ 6.0_ki
      coeffs%coeffs_55%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_55%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_55%c2(4,3) = derivative(czip,2,3,3,3)/ 6.0_ki
      coeffs%coeffs_55%c2(4,4) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_55%c2(4,5) = derivative(czip,2,2,3,3)/ 4.0_ki
      coeffs%coeffs_55%c2(4,6) = derivative(czip,2,2,2,3)/ 6.0_ki
      coeffs%coeffs_55%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_55%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_55%c2(5,3) = derivative(czip,2,4,4,4)/ 6.0_ki
      coeffs%coeffs_55%c2(5,4) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_55%c2(5,5) = derivative(czip,2,2,4,4)/ 4.0_ki
      coeffs%coeffs_55%c2(5,6) = derivative(czip,2,2,2,4)/ 6.0_ki
      coeffs%coeffs_55%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_55%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_55%c2(6,3) = derivative(czip,3,4,4,4)/ 6.0_ki
      coeffs%coeffs_55%c2(6,4) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_55%c2(6,5) = derivative(czip,3,3,4,4)/ 4.0_ki
      coeffs%coeffs_55%c2(6,6) = derivative(czip,3,3,3,4)/ 6.0_ki
      coeffs%coeffs_55%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_55%c3(1,2) = -derivative(czip,1,2,3,3)/ 2.0_ki
      coeffs%coeffs_55%c3(1,3) = -derivative(czip,1,2,2,3)/ 2.0_ki
      coeffs%coeffs_55%c3(1,4) = derivative(czip,1,1,2,3)/ 2.0_ki
      coeffs%coeffs_55%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_55%c3(2,2) = -derivative(czip,1,2,4,4)/ 2.0_ki
      coeffs%coeffs_55%c3(2,3) = -derivative(czip,1,2,2,4)/ 2.0_ki
      coeffs%coeffs_55%c3(2,4) = derivative(czip,1,1,2,4)/ 2.0_ki
      coeffs%coeffs_55%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_55%c3(3,2) = -derivative(czip,1,3,4,4)/ 2.0_ki
      coeffs%coeffs_55%c3(3,3) = -derivative(czip,1,3,3,4)/ 2.0_ki
      coeffs%coeffs_55%c3(3,4) = derivative(czip,1,1,3,4)/ 2.0_ki
      coeffs%coeffs_55%c3(4,1) = -derivative(czip,2,3,4)
      coeffs%coeffs_55%c3(4,2) = derivative(czip,2,3,4,4)/ 2.0_ki
      coeffs%coeffs_55%c3(4,3) = derivative(czip,2,3,3,4)/ 2.0_ki
      coeffs%coeffs_55%c3(4,4) = derivative(czip,2,2,3,4)/ 2.0_ki
      coeffs%coeffs_55%c4(1,1) = -derivative(czip,1,2,3,4)
      !---#] reconstruct coeffs%coeffs_55:
   end subroutine reconstruct_d55
!---#] subroutine reconstruct_d55:
end module     p0_dbard_hepemg_d55h2l1d