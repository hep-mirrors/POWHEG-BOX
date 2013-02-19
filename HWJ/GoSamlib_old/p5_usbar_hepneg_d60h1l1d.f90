module     p5_usbar_hepneg_d60h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p5_
   ! usbar_hepneg/helicity1/d60h1l1d.f90
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


   public :: derivative, reconstruct_d60
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p5_usbar_hepneg_globalsl1, only: epspow
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_abbrevh1
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
      complex(ki) :: t22
      complex(ki) :: t23
      complex(ki) :: t24
      complex(ki) :: t25
      complex(ki) :: t26
      real(ki) :: t27
      real(ki) :: t28
      real(ki) :: t29
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = -k2
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
      t27 = dotproduct(k2, qshift)
      t28 = dotproduct(qshift, qshift)
      t29 = dotproduct(k6, qshift)
      t1 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t2 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t3 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t4 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t5 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t6 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb60n24+t1*abb60n40+t2*abb60n41+t27*abb60n39+t27*t27*abb6&
         &0n32+t28*abb60n36+t29*abb60n37+t3*abb60n42+t1*t27*abb60n29+t1*t28*abb6&
         &0n48+t1*t29*abb60n31+t1*t3*abb60n26+t2*t27*abb60n28+t2*t28*abb60n50+t2&
         &*t29*abb60n27+t2*t3*abb60n25+t27*t28*abb60n45+t27*t29*abb60n34+t27*t3*&
         &abb60n35+t28*t3*abb60n43+t3*t4*abb60n30+t3*t5*abb60n33+t3*t4*t6*abb60n&
         &52)
         return
      end if
      t7 = k2(iv1)
      t8 = k6(iv1)
      t9 = qshift(iv1)
      t10 = spvak5k1(iv1)
      t11 = spvak6k1(iv1)
      t12 = spvak6k2(iv1)
      t13 = spvak2k1(iv1)
      t14 = spvak2k6(iv1)
      t15 = spvak5k4(iv1)
      if(deg.eq.1) then
         numerator = (t10*abb60n41+t11*abb60n40+t12*abb60n42+t7*abb60n39+t8*abb6&
         &0n37+t9*abb60n38+t1*t12*abb60n26+t1*t7*abb60n29+t1*t8*abb60n31+t1*t9*a&
         &bb60n49+t10*t27*abb60n28+t10*t28*abb60n50+t10*t29*abb60n27+t10*t3*abb6&
         &0n25+t11*t27*abb60n29+t11*t28*abb60n48+t11*t29*abb60n31+t11*t3*abb60n2&
         &6+t12*t2*abb60n25+t12*t27*abb60n35+t12*t28*abb60n43+t12*t4*abb60n30+t1&
         &2*t5*abb60n33+t13*t3*abb60n30+t14*t3*abb60n33+t2*t7*abb60n28+t2*t8*abb&
         &60n27+t2*t9*abb60n51+t27*t7*abb60n47+t27*t8*abb60n34+t27*t9*abb60n46+t&
         &28*t7*abb60n45+t29*t7*abb60n34+t3*t7*abb60n35+t3*t9*abb60n44+t12*t4*t6&
         &*abb60n52+t13*t3*t6*abb60n52+t15*t3*t4*abb60n52)
         return
      end if
      t28 = d(iv1,iv2)
      t5 = k2(iv2)
      t16 = k6(iv2)
      t17 = qshift(iv2)
      t18 = spvak5k1(iv2)
      t19 = spvak6k1(iv2)
      t20 = spvak6k2(iv2)
      t21 = t13*t20
      t22 = spvak2k1(iv2)
      t23 = t12*t22
      t24 = spvak5k4(iv2)
      t25 = t13*t24
      t26 = t15*t22
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t28*abb60n36+t1*t28*abb60n48+t2*t28*abb60n50+t27&
         &*t28*abb60n45+t28*t3*abb60n43+t5*t7*abb60n32)+t21*abb60n30+t23*abb60n3&
         &0+spvak2k6(iv2)*t12*abb60n33+t10*t16*abb60n27+t10*t17*abb60n51+t10*t20&
         &*abb60n25+t10*t5*abb60n28+t11*t16*abb60n31+t11*t17*abb60n49+t11*t20*ab&
         &b60n26+t11*t5*abb60n29+t12*t17*abb60n44+t12*t18*abb60n25+t12*t19*abb60&
         &n26+t12*t5*abb60n35+t14*t20*abb60n33+t16*t7*abb60n34+t17*t7*abb60n46+t&
         &18*t7*abb60n28+t18*t8*abb60n27+t18*t9*abb60n51+t19*t7*abb60n29+t19*t8*&
         &abb60n31+t19*t9*abb60n49+t20*t7*abb60n35+t20*t9*abb60n44+t21*t6*abb60n&
         &52+t23*t6*abb60n52+t25*t3*abb60n52+t26*t3*abb60n52+t5*t8*abb60n34+t5*t&
         &9*abb60n46+t12*t24*t4*abb60n52+t15*t20*t4*abb60n52)
         return
      end if
      t27 = d(iv2,iv3)
      t29 = d(iv1,iv3)
      t1 = spvak6k2(iv3)
      t2 = spvak5k4(iv3)
      t3 = spvak2k1(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(k2(iv3)*t28*abb60n45+spvak5k1(iv3)*t28*abb60n50+&
         &spvak6k1(iv3)*t28*abb60n48+t1*t28*abb60n43+t10*t27*abb60n50+t11*t27*ab&
         &b60n48+t12*t27*abb60n43+t18*t29*abb60n50+t19*t29*abb60n48+t20*t29*abb6&
         &0n43+t27*t7*abb60n45+t29*t5*abb60n45)+t1*t25*abb60n52+t1*t26*abb60n52+&
         &t12*t2*t22*abb60n52+t12*t24*t3*abb60n52+t13*t2*t20*abb60n52+t15*t20*t3&
         &*abb60n52)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d60:
   subroutine     reconstruct_d60(coeffs)
      use p5_usbar_hepneg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 3 case:
      !---[# reconstruct coeffs%coeffs_60:
      coeffs%coeffs_60%c0 = derivative(czip)
      coeffs%coeffs_60%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_60%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_60%c1(1,3) = derivative(czip,1,1,1) / 6.0_ki
      coeffs%coeffs_60%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_60%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_60%c1(2,3) = -derivative(czip,2,2,2) / 6.0_ki
      coeffs%coeffs_60%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_60%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_60%c1(3,3) = -derivative(czip,3,3,3) / 6.0_ki
      coeffs%coeffs_60%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_60%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_60%c1(4,3) = -derivative(czip,4,4,4) / 6.0_ki
      coeffs%coeffs_60%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_60%c2(1,2) = derivative(czip,1,2,2) / 2.0_ki
      coeffs%coeffs_60%c2(1,3) = -derivative(czip,1,1,2) / 2.0_ki
      coeffs%coeffs_60%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_60%c2(2,2) = derivative(czip,1,3,3) / 2.0_ki
      coeffs%coeffs_60%c2(2,3) = -derivative(czip,1,1,3) / 2.0_ki
      coeffs%coeffs_60%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_60%c2(3,2) = derivative(czip,1,4,4) / 2.0_ki
      coeffs%coeffs_60%c2(3,3) = -derivative(czip,1,1,4) / 2.0_ki
      coeffs%coeffs_60%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_60%c2(4,2) = -derivative(czip,2,3,3) / 2.0_ki
      coeffs%coeffs_60%c2(4,3) = -derivative(czip,2,2,3) / 2.0_ki
      coeffs%coeffs_60%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_60%c2(5,2) = -derivative(czip,2,4,4) / 2.0_ki
      coeffs%coeffs_60%c2(5,3) = -derivative(czip,2,2,4) / 2.0_ki
      coeffs%coeffs_60%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_60%c2(6,2) = -derivative(czip,3,4,4) / 2.0_ki
      coeffs%coeffs_60%c2(6,3) = -derivative(czip,3,3,4) / 2.0_ki
      coeffs%coeffs_60%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_60%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_60%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_60%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_60:
   end subroutine reconstruct_d60
!---#] subroutine reconstruct_d60:
end module p5_usbar_hepneg_d60h1l1d
