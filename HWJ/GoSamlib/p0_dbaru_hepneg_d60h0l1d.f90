module     p0_dbaru_hepneg_d60h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p0_
   ! dbaru_hepneg/helicity0/d60h0l1d.f90
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


   public :: derivative, reconstruct_d60
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh0
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
      real(ki) :: t30
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
      t1 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t2 = (spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-sp&
      &vak1k4(4)*qshift(4))
      t3 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t29 = dotproduct(k1, qshift)
      t4 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t30 = dotproduct(k6, qshift)
      t5 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t6 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t7 = (spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-sp&
      &vak1k6(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb60n30+t1*abb60n45+t2*abb60n46+t27*abb60n50+t27*t27*abb6&
         &0n37+t28*abb60n44+t29*abb60n49+t3*abb60n48+t30*abb60n52+t4*abb60n51+t5&
         &*abb60n53+t1*t27*abb60n40+t1*t28*abb60n56+t1*t30*abb60n41+t1*t5*abb60n&
         &34+t2*t27*abb60n32+t2*t28*abb60n61+t2*t30*abb60n33+t2*t5*abb60n31+t27*&
         &t28*abb60n59+t27*t3*abb60n42+t27*t30*abb60n35+t27*t5*abb60n38+t28*t3*a&
         &bb60n54+t29*t5*abb60n38+t3*t7*abb60n43+t4*t5*abb60n36+t5*t6*abb60n39+t&
         &3*t5*t6*abb60n63)
         return
      end if
      t8 = k1(iv1)
      t9 = k2(iv1)
      t10 = k6(iv1)
      t11 = qshift(iv1)
      t12 = spvak1k4(iv1)
      t13 = spvak2k6(iv1)
      t14 = spvak5k2(iv1)
      t15 = spvak5k4(iv1)
      t16 = spvak6k2(iv1)
      t17 = spvak1k2(iv1)
      t18 = spvak1k6(iv1)
      t19 = t13*abb60n38
      if(deg.eq.1) then
         numerator = (t10*abb60n52+t11*abb60n47+t12*abb60n46+t13*abb60n53+t14*ab&
         &b60n45+t15*abb60n48+t16*abb60n51+t19*t27+t19*t29+t8*abb60n49+t9*abb60n&
         &50+t1*t10*abb60n41+t1*t11*abb60n57+t1*t13*abb60n34+t1*t9*abb60n40+t10*&
         &t2*abb60n33+t10*t27*abb60n35+t11*t2*abb60n62+t11*t27*abb60n60+t11*t3*a&
         &bb60n55+t12*t27*abb60n32+t12*t28*abb60n61+t12*t30*abb60n33+t12*t5*abb6&
         &0n31+t13*t2*abb60n31+t13*t4*abb60n36+t13*t6*abb60n39+t14*t27*abb60n40+&
         &t14*t28*abb60n56+t14*t30*abb60n41+t14*t5*abb60n34+t15*t27*abb60n42+t15&
         &*t28*abb60n54+t15*t7*abb60n43+t16*t5*abb60n36+t17*t5*abb60n39+t18*t3*a&
         &bb60n43+t2*t9*abb60n32+t27*t9*abb60n58+t28*t9*abb60n59+t3*t9*abb60n42+&
         &t30*t9*abb60n35+t5*t8*abb60n38+t5*t9*abb60n38+t13*t3*t6*abb60n63+t15*t&
         &5*t6*abb60n63+t17*t3*t5*abb60n63)
         return
      end if
      t28 = d(iv1,iv2)
      t4 = k2(iv2)
      t7 = spvak2k6(iv2)
      t19 = k6(iv2)
      t20 = qshift(iv2)
      t21 = spvak1k4(iv2)
      t22 = spvak5k2(iv2)
      t23 = spvak5k4(iv2)
      t24 = t17*t7
      t25 = spvak1k2(iv2)
      t26 = t13*t25
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t28*abb60n44+t1*t28*abb60n56+t2*t28*abb60n61+t27&
         &*t28*abb60n59+t28*t3*abb60n54+t4*t9*abb60n37)+t24*abb60n39+t26*abb60n3&
         &9+k1(iv2)*t13*abb60n38+spvak1k6(iv2)*t15*abb60n43+spvak6k2(iv2)*t13*ab&
         &b60n36+t10*t21*abb60n33+t10*t22*abb60n41+t10*t4*abb60n35+t11*t21*abb60&
         &n62+t11*t22*abb60n57+t11*t23*abb60n55+t11*t4*abb60n60+t12*t19*abb60n33&
         &+t12*t20*abb60n62+t12*t4*abb60n32+t12*t7*abb60n31+t13*t21*abb60n31+t13&
         &*t22*abb60n34+t13*t4*abb60n38+t14*t19*abb60n41+t14*t20*abb60n57+t14*t4&
         &*abb60n40+t14*t7*abb60n34+t15*t20*abb60n55+t15*t4*abb60n42+t16*t7*abb6&
         &0n36+t18*t23*abb60n43+t19*t9*abb60n35+t20*t9*abb60n60+t21*t9*abb60n32+&
         &t22*t9*abb60n40+t23*t9*abb60n42+t24*t3*abb60n63+t26*t3*abb60n63+t7*t8*&
         &abb60n38+t7*t9*abb60n38+t13*t23*t6*abb60n63+t15*t25*t5*abb60n63+t15*t6&
         &*t7*abb60n63+t17*t23*t5*abb60n63)
         return
      end if
      t27 = d(iv2,iv3)
      t29 = d(iv1,iv3)
      t1 = spvak5k4(iv3)
      t2 = spvak2k6(iv3)
      t3 = spvak1k2(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(k2(iv3)*t28*abb60n59+spvak1k4(iv3)*t28*abb60n61+&
         &spvak5k2(iv3)*t28*abb60n56+t1*t28*abb60n54+t12*t27*abb60n61+t14*t27*ab&
         &b60n56+t15*t27*abb60n54+t21*t29*abb60n61+t22*t29*abb60n56+t23*t29*abb6&
         &0n54+t27*t9*abb60n59+t29*t4*abb60n59)+t1*t24*abb60n63+t1*t26*abb60n63+&
         &t13*t23*t3*abb60n63+t15*t2*t25*abb60n63+t15*t3*t7*abb60n63+t17*t2*t23*&
         &abb60n63)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d60:
   subroutine     reconstruct_d60(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group2
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
end module p0_dbaru_hepneg_d60h0l1d
