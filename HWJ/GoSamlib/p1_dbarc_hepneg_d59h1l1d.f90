module     p1_dbarc_hepneg_d59h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity1/d59h1l1d.f90
   ! generator: haggies (1.1)
   use p1_dbarc_hepneg_config, only: ki
   use p1_dbarc_hepneg_util, only: cond, d => metric_tensor
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
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh1
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
      real(ki) :: t24
      real(ki) :: t25
      real(ki) :: t26
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
      t1 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t24 = dotproduct(qshift, qshift)
      t2 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t3 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t25 = dotproduct(k1, qshift)
      t26 = dotproduct(k6, qshift)
      t4 = (spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-sp&
      &vak1k6(4)*qshift(4))
      t5 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb59n22+t1*abb59n45+t1*t1*abb59n33+t2*abb59n42+t24*abb59n&
         &40+t25*abb59n53+t3*abb59n47+t1*t2*abb59n37+t1*t24*abb59n50+t1*t25*abb5&
         &9n35+t1*t26*abb59n34+t1*t3*abb59n28+t1*t4*abb59n26+t2*t24*abb59n63+t2*&
         &t25*abb59n27+t2*t26*abb59n24+t2*t5*abb59n39+t24*t3*abb59n56+t25*t3*abb&
         &59n29+t26*t3*abb59n31+t1*t2*t5*abb59n66)
         return
      end if
      t6 = k1(iv1)
      t7 = qshift(iv1)
      t8 = spvak1k2(iv1)
      t9 = spvak5k2(iv1)
      t10 = spvak6k2(iv1)
      t11 = k6(iv1)
      t12 = spvak1k6(iv1)
      t13 = spvak5k4(iv1)
      if(deg.eq.1) then
         numerator = (t10*abb59n38+t6*abb59n32+t7*abb59n43+t8*abb59n41+t9*abb59n&
         &36+t1*t10*abb59n52+t1*t11*abb59n50+t1*t12*abb59n61+t1*t6*abb59n48+t1*t&
         &7*abb59n49+t1*t8*abb59n46+t1*t9*abb59n59+t10*t2*abb59n46+t10*t24*abb59&
         &n34+t10*t25*abb59n48+t10*t26*abb59n50+t10*t3*abb59n59+t10*t4*abb59n61+&
         &t11*t2*abb59n65+t11*t3*abb59n54+t13*t2*abb59n44+t2*t6*abb59n60+t2*t7*a&
         &bb59n62+t24*t8*abb59n25+t24*t9*abb59n30+t25*t8*abb59n60+t25*t9*abb59n5&
         &8+t26*t8*abb59n65+t26*t9*abb59n54+t3*t6*abb59n58+t3*t7*abb59n55+t5*t8*&
         &abb59n44+t1*t13*t2*abb59n23+t1*t5*t8*abb59n23+t10*t2*t5*abb59n23)
         return
      end if
      t24 = d(iv1,iv2)
      t4 = spvak6k2(iv2)
      t14 = spvak1k2(iv2)
      t15 = spvak5k2(iv2)
      t16 = k1(iv2)
      t17 = k6(iv2)
      t18 = qshift(iv2)
      t19 = spvak5k4(iv2)
      t20 = t19*t8
      t21 = t4*t8
      t22 = t13*t14
      t23 = t10*t14
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t24*abb59n40+t1*t24*abb59n50+t10*t4*abb59n33+t2*&
         &t24*abb59n63+t24*t3*abb59n56)+t20*abb59n39+t21*abb59n37+t22*abb59n39+t&
         &23*abb59n37+spvak1k6(iv2)*t10*abb59n26+t1*t20*abb59n66+t1*t22*abb59n66&
         &+t10*t15*abb59n28+t10*t16*abb59n35+t10*t17*abb59n34+t10*t18*abb59n51+t&
         &11*t14*abb59n24+t11*t15*abb59n31+t11*t4*abb59n34+t12*t4*abb59n26+t14*t&
         &6*abb59n27+t14*t7*abb59n64+t15*t6*abb59n29+t15*t7*abb59n57+t16*t8*abb5&
         &9n27+t16*t9*abb59n29+t17*t8*abb59n24+t17*t9*abb59n31+t18*t8*abb59n64+t&
         &18*t9*abb59n57+t21*t5*abb59n66+t23*t5*abb59n66+t4*t6*abb59n35+t4*t7*ab&
         &b59n51+t4*t9*abb59n28+t10*t19*t2*abb59n66+t13*t2*t4*abb59n66)
         return
      end if
      t25 = d(iv2,iv3)
      t26 = d(iv1,iv3)
      t1 = spvak1k2(iv3)
      t2 = spvak6k2(iv3)
      t3 = spvak5k4(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(spvak5k2(iv3)*t24*abb59n30+t1*t24*abb59n25+t10*t&
         &25*abb59n34+t14*t26*abb59n25+t15*t26*abb59n30+t2*t24*abb59n34+t25*t8*a&
         &bb59n25+t25*t9*abb59n30+t26*t4*abb59n34)+t2*t20*abb59n23+t2*t22*abb59n&
         &23+t1*t10*t19*abb59n23+t1*t13*t4*abb59n23+t10*t14*t3*abb59n23+t3*t4*t8&
         &*abb59n23)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d59:
   subroutine     reconstruct_d59(coeffs)
      use p1_dbarc_hepneg_groups, only: tensrec_info_group3
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
end module p1_dbarc_hepneg_d59h1l1d
