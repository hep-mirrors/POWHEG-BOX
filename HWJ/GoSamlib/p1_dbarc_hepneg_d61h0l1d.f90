module     p1_dbarc_hepneg_d61h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity0/d61h0l1d.f90
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


   public :: derivative, reconstruct_d61
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh0
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
      t27 = dotproduct(qshift, qshift)
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t28 = dotproduct(k1, qshift)
      t2 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t29 = dotproduct(k2, qshift)
      t30 = dotproduct(k6, qshift)
      t3 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t4 = (spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-sp&
      &vak1k4(4)*qshift(4))
      t5 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t6 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb61n26+t1*abb61n50+t2*abb61n52+t27*abb61n33+t28*abb61n51&
         &+t29*abb61n53+t3*abb61n76+t30*abb61n75+t1*t27*abb61n56+t1*t29*abb61n45&
         &+t1*t3*abb61n30+t1*t30*abb61n45+t2*t3*abb61n36+t27*t29*abb61n71+t27*t3&
         &*abb61n67+t27*t5*abb61n60+t28*t29*abb61n34+t28*t3*abb61n38+t29*t5*abb6&
         &1n41+t3*t30*abb61n39+t3*t4*abb61n28+t3*t5*abb61n29+t3*t6*abb61n40+t30*&
         &t5*abb61n43+t1*t3*t6*abb61n80)
         return
      end if
      t7 = k1(iv1)
      t8 = k2(iv1)
      t9 = k6(iv1)
      t10 = qshift(iv1)
      t11 = spvak2k6(iv1)
      t12 = spvak5k4(iv1)
      t13 = spvak6k2(iv1)
      t14 = spvak1k2(iv1)
      t15 = spvak1k4(iv1)
      t16 = spvak5k2(iv1)
      t17 = t12*abb61n54
      if(deg.eq.1) then
         numerator = (t10*abb61n74+t11*abb61n31+t12*abb61n49+t13*abb61n47+t17*t2&
         &9+t17*t30+t7*abb61n48+t8*abb61n46+t9*abb61n32+t1*t10*abb61n55+t1*t11*a&
         &bb61n77+t1*t8*abb61n54+t1*t9*abb61n54+t10*t29*abb61n70+t10*t3*abb61n66&
         &+t10*t5*abb61n59+t11*t2*abb61n69+t11*t27*abb61n37+t11*t28*abb61n65+t11&
         &*t30*abb61n64+t11*t4*abb61n79+t11*t5*abb61n78+t11*t6*abb61n63+t12*t27*&
         &abb61n44+t12*t3*abb61n77+t13*t3*abb61n69+t14*t3*abb61n63+t15*t3*abb61n&
         &79+t16*t27*abb61n42+t16*t29*abb61n62+t16*t3*abb61n78+t16*t30*abb61n58+&
         &t27*t8*abb61n35+t28*t8*abb61n73+t29*t7*abb61n73+t3*t7*abb61n65+t3*t9*a&
         &bb61n64+t5*t8*abb61n62+t5*t9*abb61n58+t1*t11*t6*abb61n27+t1*t14*t3*abb&
         &61n27+t12*t3*t6*abb61n27)
         return
      end if
      t27 = d(iv1,iv2)
      t2 = k2(iv2)
      t4 = spvak2k6(iv2)
      t17 = k1(iv2)
      t18 = qshift(iv2)
      t19 = spvak5k2(iv2)
      t20 = spvak5k4(iv2)
      t21 = k6(iv2)
      t22 = t14*t4
      t23 = spvak1k2(iv2)
      t24 = t11*t23
      t25 = t11*t20
      t26 = t12*t4
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t27*abb61n33+t1*t27*abb61n56+t27*t29*abb61n71+t2&
         &7*t3*abb61n67+t27*t5*abb61n60)+t22*abb61n40+t24*abb61n40+t25*abb61n30+&
         &t26*abb61n30+spvak1k4(iv2)*t11*abb61n28+spvak6k2(iv2)*t11*abb61n36+t1*&
         &t22*abb61n80+t1*t24*abb61n80+t10*t19*abb61n61+t10*t2*abb61n72+t10*t20*&
         &abb61n57+t10*t4*abb61n68+t11*t17*abb61n38+t11*t18*abb61n68+t11*t19*abb&
         &61n29+t11*t21*abb61n39+t12*t18*abb61n57+t12*t2*abb61n45+t12*t21*abb61n&
         &45+t13*t4*abb61n36+t15*t4*abb61n28+t16*t18*abb61n61+t16*t2*abb61n41+t1&
         &6*t21*abb61n43+t16*t4*abb61n29+t17*t8*abb61n34+t18*t8*abb61n72+t19*t8*&
         &abb61n41+t19*t9*abb61n43+t2*t7*abb61n34+t20*t8*abb61n45+t20*t9*abb61n4&
         &5+t25*t6*abb61n80+t26*t6*abb61n80+t4*t7*abb61n38+t4*t9*abb61n39+t12*t2&
         &3*t3*abb61n80+t14*t20*t3*abb61n80)
         return
      end if
      t28 = d(iv2,iv3)
      t29 = d(iv1,iv3)
      t1 = spvak2k6(iv3)
      t3 = spvak5k4(iv3)
      t5 = spvak1k2(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(k2(iv3)*t27*abb61n35+spvak5k2(iv3)*t27*abb61n42+&
         &t1*t27*abb61n37+t11*t28*abb61n37+t12*t28*abb61n44+t16*t28*abb61n42+t19&
         &*t29*abb61n42+t2*t29*abb61n35+t20*t29*abb61n44+t27*t3*abb61n44+t28*t8*&
         &abb61n35+t29*t4*abb61n37)+t22*t3*abb61n27+t24*t3*abb61n27+t1*t12*t23*a&
         &bb61n27+t1*t14*t20*abb61n27+t11*t20*t5*abb61n27+t12*t4*t5*abb61n27)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d61:
   subroutine     reconstruct_d61(coeffs)
      use p1_dbarc_hepneg_groups, only: tensrec_info_group1
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
end module p1_dbarc_hepneg_d61h0l1d
