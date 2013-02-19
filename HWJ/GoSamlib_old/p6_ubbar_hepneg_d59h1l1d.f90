module     p6_ubbar_hepneg_d59h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p6_
   ! ubbar_hepneg/helicity1/d59h1l1d.f90
   ! generator: haggies (1.1)
   use p6_ubbar_hepneg_config, only: ki
   use p6_ubbar_hepneg_util, only: cond, d => metric_tensor
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
      use p6_ubbar_hepneg_globalsl1, only: epspow
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_abbrevh1
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
      complex(ki) :: t28
      complex(ki) :: t29
      real(ki) :: t30
      real(ki) :: t31
      real(ki) :: t32
      real(ki) :: t33
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
      t30 = dotproduct(qshift, qshift)
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t2 = (spvak2k4(1)*qshift(1)-spvak2k4(2)*qshift(2)-spvak2k4(3)*qshift(3)-sp&
      &vak2k4(4)*qshift(4))
      t3 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t31 = dotproduct(k1, qshift)
      t32 = dotproduct(k6, qshift)
      t4 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t5 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t33 = dotproduct(k2, qshift)
      t6 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb59n31+t1*abb59n60+t2*abb59n61+t3*abb59n62+t30*abb59n49+&
         &t31*abb59n63+t32*abb59n64+t33*abb59n68+t4*abb59n66+t5*abb59n67+t1*t30*&
         &abb59n72+t1*t31*abb59n47+t1*t5*abb59n54+t1*t6*abb59n50+t2*t30*abb59n77&
         &+t2*t31*abb59n44+t2*t32*abb59n46+t2*t4*abb59n42+t3*t30*abb59n90+t3*t31&
         &*abb59n35+t3*t32*abb59n37+t3*t4*abb59n33+t30*t5*abb59n85+t31*t4*abb59n&
         &43+t31*t5*abb59n41+t32*t5*abb59n38+t33*t4*abb59n43+t4*t5*abb59n34+t4*t&
         &6*abb59n40+t1*t4*t6*abb59n95)
         return
      end if
      t7 = k1(iv1)
      t8 = k2(iv1)
      t9 = k6(iv1)
      t10 = qshift(iv1)
      t11 = spvak2k4(iv1)
      t12 = spvak5k1(iv1)
      t13 = spvak5k4(iv1)
      t14 = spvak6k1(iv1)
      t15 = spvak6k2(iv1)
      t16 = spvak2k1(iv1)
      t17 = t15*abb59n80
      if(deg.eq.1) then
         numerator = (t10*abb59n70+t11*abb59n58+t12*abb59n57+t13*abb59n59+t14*ab&
         &b59n52+t15*abb59n53+t17*t31+t17*t33+t7*abb59n56+t8*abb59n51+t9*abb59n5&
         &5+t1*t10*abb59n71+t1*t14*abb59n65+t1*t16*abb59n69+t1*t7*abb59n74+t10*t&
         &2*abb59n76+t10*t3*abb59n89+t10*t5*abb59n84+t11*t30*abb59n45+t11*t31*ab&
         &b59n79+t11*t32*abb59n75+t11*t4*abb59n81+t12*t30*abb59n36+t12*t31*abb59&
         &n92+t12*t32*abb59n88+t12*t4*abb59n94+t13*t30*abb59n48+t13*t31*abb59n74&
         &+t13*t5*abb59n65+t13*t6*abb59n69+t14*t30*abb59n39+t14*t31*abb59n82+t14&
         &*t32*abb59n87+t14*t4*abb59n93+t15*t2*abb59n81+t15*t3*abb59n94+t15*t5*a&
         &bb59n93+t15*t6*abb59n83+t16*t4*abb59n83+t2*t7*abb59n79+t2*t9*abb59n75+&
         &t3*t7*abb59n92+t3*t9*abb59n88+t4*t7*abb59n80+t4*t8*abb59n80+t5*t7*abb5&
         &9n82+t5*t9*abb59n87+t1*t15*t6*abb59n32+t1*t16*t4*abb59n32+t13*t4*t6*ab&
         &b59n32)
         return
      end if
      t30 = d(iv1,iv2)
      t17 = spvak2k4(iv2)
      t18 = spvak5k1(iv2)
      t19 = spvak5k4(iv2)
      t20 = spvak6k1(iv2)
      t21 = spvak6k2(iv2)
      t22 = k1(iv2)
      t23 = k6(iv2)
      t24 = qshift(iv2)
      t25 = t16*t19
      t26 = t16*t21
      t27 = spvak2k1(iv2)
      t28 = t13*t27
      t29 = t15*t27
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t30*abb59n49+t1*t30*abb59n72+t2*t30*abb59n77+t3*&
         &t30*abb59n90+t30*t5*abb59n85)+t25*abb59n50+t26*abb59n40+t28*abb59n50+t&
         &29*abb59n40+k2(iv2)*t15*abb59n43+t1*t26*abb59n95+t1*t29*abb59n95+t10*t&
         &17*abb59n78+t10*t18*abb59n91+t10*t19*abb59n73+t10*t20*abb59n86+t11*t21&
         &*abb59n42+t11*t22*abb59n44+t11*t23*abb59n46+t11*t24*abb59n78+t12*t21*a&
         &bb59n33+t12*t22*abb59n35+t12*t23*abb59n37+t12*t24*abb59n91+t13*t20*abb&
         &59n54+t13*t22*abb59n47+t13*t24*abb59n73+t14*t19*abb59n54+t14*t21*abb59&
         &n34+t14*t22*abb59n41+t14*t23*abb59n38+t14*t24*abb59n86+t15*t17*abb59n4&
         &2+t15*t18*abb59n33+t15*t20*abb59n34+t15*t22*abb59n43+t17*t7*abb59n44+t&
         &17*t9*abb59n46+t18*t7*abb59n35+t18*t9*abb59n37+t19*t7*abb59n47+t20*t7*&
         &abb59n41+t20*t9*abb59n38+t21*t7*abb59n43+t21*t8*abb59n43+t25*t4*abb59n&
         &95+t28*t4*abb59n95+t13*t21*t6*abb59n95+t15*t19*t6*abb59n95)
         return
      end if
      t31 = d(iv2,iv3)
      t32 = d(iv1,iv3)
      t1 = spvak5k4(iv3)
      t2 = spvak6k2(iv3)
      t3 = spvak2k1(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(spvak2k4(iv3)*t30*abb59n45+spvak5k1(iv3)*t30*abb&
         &59n36+spvak6k1(iv3)*t30*abb59n39+t1*t30*abb59n48+t11*t31*abb59n45+t12*&
         &t31*abb59n36+t13*t31*abb59n48+t14*t31*abb59n39+t17*t32*abb59n45+t18*t3&
         &2*abb59n36+t19*t32*abb59n48+t20*t32*abb59n39)+t2*t25*abb59n32+t2*t28*a&
         &bb59n32+t1*t15*t27*abb59n32+t1*t16*t21*abb59n32+t13*t21*t3*abb59n32+t1&
         &5*t19*t3*abb59n32)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d59:
   subroutine     reconstruct_d59(coeffs)
      use p6_ubbar_hepneg_groups, only: tensrec_info_group3
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
end module p6_ubbar_hepneg_d59h1l1d
