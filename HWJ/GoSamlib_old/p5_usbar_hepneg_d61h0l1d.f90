module     p5_usbar_hepneg_d61h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p5_
   ! usbar_hepneg/helicity0/d61h0l1d.f90
   ! generator: haggies (1.1)
   use p5_usbar_hepneg_config, only: ki
   use p5_usbar_hepneg_util, only: cond, d => metric_tensor
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

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p5_usbar_hepneg_globalsl1, only: epspow
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_abbrevh0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
      complex(ki) :: numerator
      complex(ki) :: loc
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
      complex(ki) :: t18
      complex(ki) :: t19
      complex(ki) :: t20
      complex(ki) :: t21
      real(ki) :: t22
      real(ki) :: t23
      real(ki) :: t24
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
      t22 = dotproduct(qshift, qshift)
      t1 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t23 = dotproduct(k2, qshift)
      t24 = dotproduct(k6, qshift)
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t3 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t4 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t5 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb61n20+t1*abb61n40+t1*t1*abb61n31+t2*abb61n41+t22*abb61n&
         &28+t23*abb61n38+t24*abb61n39+t3*abb61n47+abb61n48*dotproduct(k1, qshif&
         &t)+t1*t2*abb61n27+t1*t22*abb61n43+t1*t23*abb61n32+t1*t24*abb61n32+t1*t&
         &3*abb61n26+t1*t4*abb61n22+t1*t5*abb61n25+t22*t3*abb61n54+t23*t3*abb61n&
         &25+t24*t3*abb61n23+t1*t2*t3*abb61n58)
         return
      end if
      t6 = k2(iv1)
      t7 = k6(iv1)
      t8 = qshift(iv1)
      t9 = spvak2k1(iv1)
      t10 = spvak2k6(iv1)
      t11 = spvak5k4(iv1)
      t12 = t10*abb61n45
      t13 = spvak5k1(iv1)
      t14 = spvak6k1(iv1)
      if(deg.eq.1) then
         numerator = (k1(iv1)*abb61n29+t10*abb61n35+t11*abb61n34+t12*t23+t12*t24&
         &+t6*abb61n37+t7*abb61n36+t8*abb61n49+t9*abb61n30+t1*t10*abb61n46+t1*t1&
         &1*abb61n50+t1*t13*abb61n57+t1*t14*abb61n52+t1*t6*abb61n45+t1*t7*abb61n&
         &45+t1*t8*abb61n42+t1*t9*abb61n51+t10*t2*abb61n50+t10*t22*abb61n33+t10*&
         &t3*abb61n51+t10*t4*abb61n57+t10*t5*abb61n52+t22*t9*abb61n24+t23*t9*abb&
         &61n52+t24*t9*abb61n56+t3*t6*abb61n52+t3*t7*abb61n56+t3*t8*abb61n53+t1*&
         &t11*t3*abb61n21+t1*t2*t9*abb61n21+t10*t2*t3*abb61n21)
         return
      end if
      t22 = d(iv1,iv2)
      t4 = spvak2k6(iv2)
      t5 = spvak2k1(iv2)
      t12 = k2(iv2)
      t15 = k6(iv2)
      t16 = qshift(iv2)
      t17 = t4*t9
      t18 = t10*t5
      t19 = spvak5k4(iv2)
      t20 = t10*t19
      t21 = t11*t4
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t22*abb61n28+t1*t22*abb61n43+t10*t4*abb61n31+t22&
         &*t3*abb61n54)+t17*abb61n26+t18*abb61n26+t20*abb61n27+t21*abb61n27+spva&
         &k5k1(iv2)*t10*abb61n22+spvak6k1(iv2)*t10*abb61n25+t10*t12*abb61n32+t10&
         &*t15*abb61n32+t10*t16*abb61n44+t12*t9*abb61n25+t13*t4*abb61n22+t14*t4*&
         &abb61n25+t15*t9*abb61n23+t16*t9*abb61n55+t17*t2*abb61n58+t18*t2*abb61n&
         &58+t20*t3*abb61n58+t21*t3*abb61n58+t4*t6*abb61n32+t4*t7*abb61n32+t4*t8&
         &*abb61n44+t5*t6*abb61n25+t5*t7*abb61n23+t5*t8*abb61n55+t1*t11*t5*abb61&
         &n58+t1*t19*t9*abb61n58)
         return
      end if
      t23 = d(iv2,iv3)
      t24 = d(iv1,iv3)
      t1 = spvak2k1(iv3)
      t2 = spvak2k6(iv3)
      t3 = spvak5k4(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(t1*t22*abb61n24+t10*t23*abb61n33+t2*t22*abb61n33&
         &+t23*t9*abb61n24+t24*t4*abb61n33+t24*t5*abb61n24)+t17*t3*abb61n21+t18*&
         &t3*abb61n21+t1*t10*t19*abb61n21+t1*t11*t4*abb61n21+t11*t2*t5*abb61n21+&
         &t19*t2*t9*abb61n21)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d61:
   subroutine     reconstruct_d61(coeffs)
      use p5_usbar_hepneg_groups, only: tensrec_info_group1
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
end module p5_usbar_hepneg_d61h0l1d
