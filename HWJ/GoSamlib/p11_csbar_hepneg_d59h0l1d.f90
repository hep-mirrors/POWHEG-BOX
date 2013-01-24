module     p11_csbar_hepneg_d59h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p11
   ! _csbar_hepneg/helicity0/d59h0l1d.f90
   ! generator: haggies (1.1)
   use p11_csbar_hepneg_config, only: ki
   use p11_csbar_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d59
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p11_csbar_hepneg_globalsl1, only: epspow
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_abbrevh0
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
      complex(ki) :: t27
      real(ki) :: t28
      real(ki) :: t29
      real(ki) :: t30
      real(ki) :: t31
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = k2
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
      t28 = dotproduct(qshift, qshift)
      t1 = (spvak2k4(1)*qshift(1)-spvak2k4(2)*qshift(2)-spvak2k4(3)*qshift(3)-sp&
      &vak2k4(4)*qshift(4))
      t29 = dotproduct(k6, qshift)
      t30 = dotproduct(k1, qshift)
      t2 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t3 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t4 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t31 = dotproduct(k2, qshift)
      t5 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t6 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb59n25+t1*abb59n48+t2*abb59n52+t28*abb59n45+t29*abb59n49&
         &+t3*abb59n53+t30*abb59n51+t31*abb59n57+t4*abb59n54+t1*t2*abb59n33+t1*t&
         &28*abb59n60+t1*t29*abb59n35+t1*t30*abb59n37+t2*t3*abb59n32+t2*t30*abb5&
         &9n39+t2*t31*abb59n39+t2*t4*abb59n34+t2*t5*abb59n27+t2*t6*abb59n29+t28*&
         &t3*abb59n68+t29*t3*abb59n31+t3*t30*abb59n28+t3*t4*abb59n40+t2*t3*t4*ab&
         &b59n73)
         return
      end if
      t7 = k1(iv1)
      t8 = k2(iv1)
      t9 = k6(iv1)
      t10 = qshift(iv1)
      t11 = spvak2k1(iv1)
      t12 = spvak2k4(iv1)
      t13 = spvak2k6(iv1)
      t14 = spvak5k4(iv1)
      t15 = t13*abb59n56
      t16 = spvak5k1(iv1)
      t17 = spvak6k1(iv1)
      if(deg.eq.1) then
         numerator = (t10*abb59n50+t11*abb59n42+t12*abb59n47+t13*abb59n43+t14*ab&
         &b59n41+t15*t30+t15*t31+t7*abb59n44+t8*abb59n38+t9*abb59n46+t1*t10*abb5&
         &9n59+t1*t13*abb59n64+t1*t7*abb59n58+t1*t9*abb59n62+t10*t3*abb59n67+t11&
         &*t2*abb59n65+t11*t28*abb59n30+t11*t29*abb59n66+t11*t30*abb59n71+t11*t4&
         &*abb59n55+t12*t2*abb59n64+t12*t28*abb59n36+t12*t29*abb59n62+t12*t30*ab&
         &b59n58+t13*t3*abb59n65+t13*t4*abb59n63+t13*t5*abb59n72+t13*t6*abb59n70&
         &+t14*t2*abb59n63+t14*t3*abb59n55+t16*t2*abb59n72+t17*t2*abb59n70+t2*t7&
         &*abb59n56+t2*t8*abb59n56+t3*t7*abb59n71+t3*t9*abb59n66+t11*t2*t4*abb59&
         &n26+t13*t3*t4*abb59n26+t14*t2*t3*abb59n26)
         return
      end if
      t28 = d(iv1,iv2)
      t5 = spvak2k1(iv2)
      t6 = spvak2k4(iv2)
      t15 = spvak2k6(iv2)
      t18 = k1(iv2)
      t19 = k6(iv2)
      t20 = qshift(iv2)
      t21 = t11*t15
      t22 = spvak5k4(iv2)
      t23 = t11*t22
      t24 = t13*t5
      t25 = t14*t5
      t26 = t13*t22
      t27 = t14*t15
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t28*abb59n45+t1*t28*abb59n60+t28*t3*abb59n68)+t2&
         &1*abb59n32+t23*abb59n40+t24*abb59n32+t25*abb59n40+t26*abb59n34+t27*abb&
         &59n34+k2(iv2)*t13*abb59n39+spvak5k1(iv2)*t13*abb59n27+spvak6k1(iv2)*t1&
         &3*abb59n29+t10*t5*abb59n69+t10*t6*abb59n61+t11*t18*abb59n28+t11*t19*ab&
         &b59n31+t11*t20*abb59n69+t12*t15*abb59n33+t12*t18*abb59n37+t12*t19*abb5&
         &9n35+t12*t20*abb59n61+t13*t18*abb59n39+t13*t6*abb59n33+t15*t16*abb59n2&
         &7+t15*t17*abb59n29+t15*t7*abb59n39+t15*t8*abb59n39+t2*t23*abb59n73+t2*&
         &t25*abb59n73+t21*t4*abb59n73+t24*t4*abb59n73+t26*t3*abb59n73+t27*t3*ab&
         &b59n73+t5*t7*abb59n28+t5*t9*abb59n31+t6*t7*abb59n37+t6*t9*abb59n35)
         return
      end if
      t29 = d(iv2,iv3)
      t30 = d(iv1,iv3)
      t1 = spvak2k1(iv3)
      t2 = spvak5k4(iv3)
      t3 = spvak2k6(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(spvak2k4(iv3)*t28*abb59n36+t1*t28*abb59n30+t11*t&
         &29*abb59n30+t12*t29*abb59n36+t30*t5*abb59n30+t30*t6*abb59n36)+t2*t21*a&
         &bb59n26+t2*t24*abb59n26+t1*t13*t22*abb59n26+t1*t14*t15*abb59n26+t11*t2&
         &2*t3*abb59n26+t14*t3*t5*abb59n26)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d59:
   subroutine     reconstruct_d59(coeffs)
      use p11_csbar_hepneg_groups, only: tensrec_info_group3
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group3), intent(out) :: coeffs
      ! rank 3 case:
      !---[# reconstruct coeffs%coeffs_59:
      coeffs%coeffs_59%c0 = derivative(czip)
      coeffs%coeffs_59%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_59%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_59%c1(1,3) = derivative(czip,1,1,1) / 6.0_ki
      coeffs%coeffs_59%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_59%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_59%c1(2,3) = -derivative(czip,2,2,2) / 6.0_ki
      coeffs%coeffs_59%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_59%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_59%c1(3,3) = -derivative(czip,3,3,3) / 6.0_ki
      coeffs%coeffs_59%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_59%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_59%c1(4,3) = -derivative(czip,4,4,4) / 6.0_ki
      coeffs%coeffs_59%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_59%c2(1,2) = derivative(czip,1,2,2) / 2.0_ki
      coeffs%coeffs_59%c2(1,3) = -derivative(czip,1,1,2) / 2.0_ki
      coeffs%coeffs_59%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_59%c2(2,2) = derivative(czip,1,3,3) / 2.0_ki
      coeffs%coeffs_59%c2(2,3) = -derivative(czip,1,1,3) / 2.0_ki
      coeffs%coeffs_59%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_59%c2(3,2) = derivative(czip,1,4,4) / 2.0_ki
      coeffs%coeffs_59%c2(3,3) = -derivative(czip,1,1,4) / 2.0_ki
      coeffs%coeffs_59%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_59%c2(4,2) = -derivative(czip,2,3,3) / 2.0_ki
      coeffs%coeffs_59%c2(4,3) = -derivative(czip,2,2,3) / 2.0_ki
      coeffs%coeffs_59%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_59%c2(5,2) = -derivative(czip,2,4,4) / 2.0_ki
      coeffs%coeffs_59%c2(5,3) = -derivative(czip,2,2,4) / 2.0_ki
      coeffs%coeffs_59%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_59%c2(6,2) = -derivative(czip,3,4,4) / 2.0_ki
      coeffs%coeffs_59%c2(6,3) = -derivative(czip,3,3,4) / 2.0_ki
      coeffs%coeffs_59%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_59%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_59%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_59%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_59:
   end subroutine reconstruct_d59
!---#] subroutine reconstruct_d59:
end module p11_csbar_hepneg_d59h0l1d
