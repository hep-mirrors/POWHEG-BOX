module     p0_dbaru_hepneg_d61h1l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p0_dbaru_hepneg/helicity1/d61h1l1d.f90
   ! generator: haggies (1.1)
   use p0_dbaru_hepneg_config, only: ki
   use p0_dbaru_hepneg_util, only: cond, d => metric_tensor
   

   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d61
contains
!---#[ function brack_1:
   pure function brack_1(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      real(ki) :: t4
      real(ki) :: t5
      real(ki) :: t6
      t4 = dotproduct(qshift, qshift)
      t1 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t5 = dotproduct(k6, qshift)
      t6 = dotproduct(k2, qshift)
      t2 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t3 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      brack = (abb61n17+t1*abb61n39+t1*t1*abb61n27+t4*abb61n25+t5*abb61n32+t6*ab&
      &b61n41+t1*t2*abb61n20+t1*t3*abb61n30+t1*t4*abb61n36+t1*t5*abb61n29+t1*abb&
      &61n19*(spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-&
      &spvak1k4(4)*qshift(4))+t1*abb61n29*dotproduct(k1, qshift)+t2*t4*abb61n44+&
      &t2*t5*abb61n21+t2*t6*abb61n23+t1*t3*abb61n49*(spvak5k4(1)*qshift(1)-spvak&
      &5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-spvak5k4(4)*qshift(4)))
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8
      complex(ki) :: t9
      complex(ki) :: t10
      complex(ki) :: t11
      real(ki) :: t12
      real(ki) :: t13
      t1 = k2(iv1)
      t2 = k6(iv1)
      t3 = qshift(iv1)
      t4 = spvak6k2(iv1)
      t5 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t6 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t7 = spvak1k2(iv1)
      t8 = spvak5k2(iv1)
      t12 = dotproduct(qshift, qshift)
      t13 = dotproduct(k6, qshift)
      t9 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t10 = t4*abb61n34
      t11 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      brack = (t1*abb61n24+t10*t13+t10*dotproduct(k1, qshift)+t2*abb61n31+t3*abb&
      &61n40+t4*abb61n26+k1(iv1)*t5*abb61n34+spvak1k4(iv1)*t5*abb61n48+t1*t6*abb&
      &61n42+t12*t4*abb61n28+t12*t8*abb61n22+t13*t8*abb61n46+t2*t5*abb61n34+t2*t&
      &6*abb61n46+t3*t5*abb61n35+t3*t6*abb61n43+t4*t5*abb61n38+t4*t6*abb61n47+t4&
      &*t9*abb61n33+t4*abb61n48*(spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spv&
      &ak1k4(3)*qshift(3)-spvak1k4(4)*qshift(4))+t5*t7*abb61n33+t5*t8*abb61n47+t&
      &8*abb61n42*dotproduct(k2, qshift)+spvak5k4(iv1)*t5*t9*abb61n18+t11*t4*t9*&
      &abb61n18+t11*t5*t7*abb61n18)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8
      complex(ki) :: t9
      complex(ki) :: t10
      complex(ki) :: t11
      complex(ki) :: t12
      complex(ki) :: t13
      complex(ki) :: t14
      complex(ki) :: t15
      complex(ki) :: t16
      complex(ki) :: t17
      real(ki) :: t18
      t18 = d(iv1,iv2)
      t1 = spvak6k2(iv1)
      t2 = spvak6k2(iv2)
      t3 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t4 = spvak5k2(iv2)
      t5 = spvak5k2(iv1)
      t6 = k6(iv1)
      t7 = k6(iv2)
      t8 = qshift(iv1)
      t9 = qshift(iv2)
      t10 = spvak1k2(iv1)
      t11 = t10*t2
      t12 = spvak1k2(iv2)
      t13 = t1*t12
      t14 = spvak5k4(iv2)
      t15 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t16 = spvak5k4(iv1)
      t17 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      brack = ((2.0_ki)*(t18*abb61n25+t1*t2*abb61n27+t18*t3*abb61n36+t18*abb61n4&
      &4*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-spva&
      &k5k2(4)*qshift(4)))+t11*abb61n30+t13*abb61n30+k1(iv1)*t2*abb61n29+k1(iv2)&
      &*t1*abb61n29+k2(iv1)*t4*abb61n23+k2(iv2)*t5*abb61n23+spvak1k4(iv1)*t2*abb&
      &61n19+spvak1k4(iv2)*t1*abb61n19+t1*t4*abb61n20+t1*t7*abb61n29+t1*t9*abb61&
      &n37+t11*t15*abb61n49+t13*t15*abb61n49+t2*t5*abb61n20+t2*t6*abb61n29+t2*t8&
      &*abb61n37+t4*t6*abb61n21+t4*t8*abb61n45+t5*t7*abb61n21+t5*t9*abb61n45+t1*&
      &t14*t17*abb61n49+t10*t14*t3*abb61n49+t12*t16*t3*abb61n49+t16*t17*t2*abb61&
      &n49)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8
      complex(ki) :: t9
      real(ki) :: t10
      real(ki) :: t11
      real(ki) :: t12
      t10 = d(iv2,iv3)
      t11 = d(iv1,iv3)
      t12 = d(iv1,iv2)
      t1 = spvak6k2(iv1)
      t2 = spvak6k2(iv2)
      t3 = spvak6k2(iv3)
      t4 = spvak1k2(iv1)
      t5 = spvak5k4(iv2)
      t6 = spvak5k4(iv3)
      t7 = spvak1k2(iv2)
      t8 = spvak5k4(iv1)
      t9 = spvak1k2(iv3)
      brack = ((2.0_ki)*(spvak5k2(iv1)*t10*abb61n22+spvak5k2(iv2)*t11*abb61n22+s&
      &pvak5k2(iv3)*t12*abb61n22+t1*t10*abb61n28+t11*t2*abb61n28+t12*t3*abb61n28&
      &)+t1*t5*t9*abb61n18+t1*t6*t7*abb61n18+t2*t4*t6*abb61n18+t2*t8*t9*abb61n18&
      &+t3*t4*t5*abb61n18+t3*t7*t8*abb61n18)
   end  function brack_4
!---#] function brack_4:

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh1
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = k6
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
         iv1 = i1
         deg = 1
      else
         iv1 = 1
      end if
      if(present(i2)) then
         iv2 = i2
         deg = 2
      else
         iv2 = 1
      end if
      if(present(i3)) then
         iv3 = i3
         deg = 3
      else
         iv3 = 1
      end if
      t1 = 0
      if(deg.eq.0) then
         numerator = (cond(epspow.eq.t1,brack_1(Q),Q,mu2))
         return
      end if
      if(deg.eq.1) then
         numerator = (cond(epspow.eq.t1,brack_2(Q),Q,mu2))
         return
      end if
      if(deg.eq.2) then
         numerator = (cond(epspow.eq.t1,brack_3(Q),Q,mu2))
         return
      end if
      if(deg.eq.3) then
         numerator = (cond(epspow.eq.t1,brack_4(Q),Q,mu2))
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d61:
   subroutine     reconstruct_d61(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 3 case:
      !---[# reconstruct coeffs%coeffs_61:
      coeffs%coeffs_61%c0 = derivative(czip)
      coeffs%coeffs_61%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_61%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_61%c1(1,3) = derivative(czip,1,1,1) / 6.0_ki
      coeffs%coeffs_61%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_61%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_61%c1(2,3) = -derivative(czip,2,2,2) / 6.0_ki
      coeffs%coeffs_61%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_61%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_61%c1(3,3) = -derivative(czip,3,3,3) / 6.0_ki
      coeffs%coeffs_61%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_61%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_61%c1(4,3) = -derivative(czip,4,4,4) / 6.0_ki
      coeffs%coeffs_61%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_61%c2(1,2) = derivative(czip,1,2,2) / 2.0_ki
      coeffs%coeffs_61%c2(1,3) = -derivative(czip,1,1,2) / 2.0_ki
      coeffs%coeffs_61%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_61%c2(2,2) = derivative(czip,1,3,3) / 2.0_ki
      coeffs%coeffs_61%c2(2,3) = -derivative(czip,1,1,3) / 2.0_ki
      coeffs%coeffs_61%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_61%c2(3,2) = derivative(czip,1,4,4) / 2.0_ki
      coeffs%coeffs_61%c2(3,3) = -derivative(czip,1,1,4) / 2.0_ki
      coeffs%coeffs_61%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_61%c2(4,2) = -derivative(czip,2,3,3) / 2.0_ki
      coeffs%coeffs_61%c2(4,3) = -derivative(czip,2,2,3) / 2.0_ki
      coeffs%coeffs_61%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_61%c2(5,2) = -derivative(czip,2,4,4) / 2.0_ki
      coeffs%coeffs_61%c2(5,3) = -derivative(czip,2,2,4) / 2.0_ki
      coeffs%coeffs_61%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_61%c2(6,2) = -derivative(czip,3,4,4) / 2.0_ki
      coeffs%coeffs_61%c2(6,3) = -derivative(czip,3,3,4) / 2.0_ki
      coeffs%coeffs_61%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_61%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_61%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_61%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_61:
   end subroutine reconstruct_d61
!---#] subroutine reconstruct_d61:
end module p0_dbaru_hepneg_d61h1l1d
