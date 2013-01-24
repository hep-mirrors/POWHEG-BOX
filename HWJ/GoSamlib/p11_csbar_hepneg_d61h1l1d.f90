module     p11_csbar_hepneg_d61h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p11
   ! _csbar_hepneg/helicity1/d61h1l1d.f90
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


   public :: derivative, reconstruct_d61
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p11_csbar_hepneg_globalsl1, only: epspow
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_abbrevh1
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
      complex(ki) :: t30
      real(ki) :: t31
      real(ki) :: t32
      real(ki) :: t33
      real(ki) :: t34
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
      t31 = dotproduct(qshift, qshift)
      t1 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t32 = dotproduct(k2, qshift)
      t33 = dotproduct(k6, qshift)
      t2 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t34 = dotproduct(k1, qshift)
      t3 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t4 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t5 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t6 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb61n25+t1*abb61n49+t2*abb61n62+t31*abb61n36+t32*abb61n50&
         &+t33*abb61n51+t34*abb61n64+t1*t2*abb61n28+t1*t3*abb61n27+t1*t31*abb61n&
         &58+t1*t32*abb61n39+t1*t33*abb61n39+t1*t4*abb61n33+t1*t5*abb61n34+t1*t6&
         &*abb61n38+t2*t31*abb61n68+t2*t32*abb61n31+t2*t33*abb61n33+t2*t5*abb61n&
         &47+t3*t31*abb61n72+t3*t34*abb61n29+t31*t5*abb61n53+t33*t5*abb61n41+t34&
         &*t5*abb61n42+t1*t4*t5*abb61n77)
         return
      end if
      t7 = k1(iv1)
      t8 = k2(iv1)
      t9 = k6(iv1)
      t10 = qshift(iv1)
      t11 = spvak6k1(iv1)
      t12 = spvak6k2(iv1)
      t13 = spvak2k1(iv1)
      t14 = spvak2k6(iv1)
      t15 = spvak5k1(iv1)
      t16 = spvak5k4(iv1)
      t17 = t12*abb61n60
      if(deg.eq.1) then
         numerator = (t10*abb61n63+t11*abb61n37+t12*abb61n46+t17*t32+t17*t33+t7*&
         &abb61n35+t8*abb61n45+t9*abb61n44+t1*t10*abb61n57+t1*t11*abb61n75+t1*t1&
         &3*abb61n66+t1*t14*abb61n61+t1*t15*abb61n76+t1*t16*abb61n65+t1*t8*abb61&
         &n60+t1*t9*abb61n60+t10*t2*abb61n67+t10*t3*abb61n71+t10*t5*abb61n52+t11&
         &*t31*abb61n32+t11*t32*abb61n70+t11*t33*abb61n66+t11*t5*abb61n48+t12*t2&
         &*abb61n75+t12*t3*abb61n76+t12*t31*abb61n40+t12*t4*abb61n66+t12*t5*abb6&
         &1n65+t12*t6*abb61n61+t15*t31*abb61n30+t15*t34*abb61n74+t16*t2*abb61n48&
         &+t16*t31*abb61n43+t16*t33*abb61n56+t16*t34*abb61n55+t2*t8*abb61n70+t2*&
         &t9*abb61n66+t3*t7*abb61n74+t5*t7*abb61n55+t5*t9*abb61n56+t1*t13*t5*abb&
         &61n26+t1*t16*t4*abb61n26+t12*t4*t5*abb61n26)
         return
      end if
      t31 = d(iv1,iv2)
      t6 = spvak5k1(iv2)
      t17 = spvak5k4(iv2)
      t18 = k1(iv2)
      t19 = spvak6k1(iv2)
      t20 = spvak6k2(iv2)
      t21 = k2(iv2)
      t22 = k6(iv2)
      t23 = qshift(iv2)
      t24 = t13*t20
      t25 = spvak2k1(iv2)
      t26 = t12*t25
      t27 = t16*t20
      t28 = t12*t17
      t29 = t13*t17
      t30 = t16*t25
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t31*abb61n36+t1*t31*abb61n58+t2*t31*abb61n68+t3*&
         &t31*abb61n72+t31*t5*abb61n53)+t24*abb61n33+t26*abb61n33+t27*abb61n34+t&
         &28*abb61n34+spvak2k6(iv2)*t12*abb61n38+t1*t29*abb61n77+t1*t30*abb61n77&
         &+t10*t17*abb61n54+t10*t19*abb61n69+t10*t20*abb61n59+t10*t6*abb61n73+t1&
         &1*t17*abb61n47+t11*t20*abb61n28+t11*t21*abb61n31+t11*t22*abb61n33+t11*&
         &t23*abb61n69+t12*t19*abb61n28+t12*t21*abb61n39+t12*t22*abb61n39+t12*t2&
         &3*abb61n59+t12*t6*abb61n27+t14*t20*abb61n38+t15*t18*abb61n29+t15*t20*a&
         &bb61n27+t15*t23*abb61n73+t16*t18*abb61n42+t16*t19*abb61n47+t16*t22*abb&
         &61n41+t16*t23*abb61n54+t17*t7*abb61n42+t17*t9*abb61n41+t19*t8*abb61n31&
         &+t19*t9*abb61n33+t20*t8*abb61n39+t20*t9*abb61n39+t24*t5*abb61n77+t26*t&
         &5*abb61n77+t27*t4*abb61n77+t28*t4*abb61n77+t6*t7*abb61n29)
         return
      end if
      t32 = d(iv2,iv3)
      t33 = d(iv1,iv3)
      t1 = spvak5k4(iv3)
      t2 = spvak6k2(iv3)
      t3 = spvak2k1(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(spvak5k1(iv3)*t31*abb61n30+spvak6k1(iv3)*t31*abb&
         &61n32+t1*t31*abb61n43+t11*t32*abb61n32+t12*t32*abb61n40+t15*t32*abb61n&
         &30+t16*t32*abb61n43+t17*t33*abb61n43+t19*t33*abb61n32+t2*t31*abb61n40+&
         &t20*t33*abb61n40+t33*t6*abb61n30)+t2*t29*abb61n26+t2*t30*abb61n26+t1*t&
         &12*t25*abb61n26+t1*t13*t20*abb61n26+t12*t17*t3*abb61n26+t16*t20*t3*abb&
         &61n26)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d61:
   subroutine     reconstruct_d61(coeffs)
      use p11_csbar_hepneg_groups, only: tensrec_info_group1
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
end module p11_csbar_hepneg_d61h1l1d
