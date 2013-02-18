module     p6_ubbar_hepneg_d61h1l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p6_ubbar_hepneg/helicity1/d61h1l1d.f90
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


   public :: derivative, reconstruct_d61
contains
!---#[ function brack_1:
   pure function brack_1(Q) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      real(ki) :: t6
      real(ki) :: t7
      real(ki) :: t8
      real(ki) :: t9
      t6 = dotproduct(qshift, qshift)
      t1 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t7 = dotproduct(k2, qshift)
      t8 = dotproduct(k6, qshift)
      t2 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t9 = dotproduct(k1, qshift)
      t3 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t4 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t5 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      brack = (abb61n25+t1*abb61n49+t2*abb61n62+t6*abb61n36+t7*abb61n50+t8*abb61&
      &n51+t9*abb61n64+t1*t2*abb61n28+t1*t3*abb61n27+t1*t4*abb61n33+t1*t5*abb61n&
      &34+t1*t6*abb61n58+t1*t7*abb61n39+t1*t8*abb61n39+t1*abb61n38*(spvak2k6(1)*&
      &qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-spvak2k6(4)*qshift(&
      &4))+t2*t5*abb61n47+t2*t6*abb61n68+t2*t7*abb61n31+t2*t8*abb61n33+t3*t6*abb&
      &61n72+t3*t9*abb61n29+t5*t6*abb61n53+t5*t8*abb61n41+t5*t9*abb61n42+t1*t4*t&
      &5*abb61n77)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh1
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
      real(ki) :: t16
      real(ki) :: t17
      real(ki) :: t18
      real(ki) :: t19
      t1 = k1(iv1)
      t2 = k2(iv1)
      t3 = k6(iv1)
      t4 = qshift(iv1)
      t5 = spvak6k1(iv1)
      t6 = spvak6k2(iv1)
      t7 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t8 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t9 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t10 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-s&
      &pvak6k1(4)*qshift(4))
      t11 = spvak2k1(iv1)
      t12 = spvak5k1(iv1)
      t16 = dotproduct(qshift, qshift)
      t17 = dotproduct(k1, qshift)
      t13 = spvak5k4(iv1)
      t18 = dotproduct(k6, qshift)
      t19 = dotproduct(k2, qshift)
      t14 = t6*abb61n60
      t15 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      brack = (t1*abb61n35+t14*t18+t14*t19+t2*abb61n45+t3*abb61n44+t4*abb61n63+t&
      &5*abb61n37+t6*abb61n46+spvak2k6(iv1)*t9*abb61n61+t1*t7*abb61n55+t1*t8*abb&
      &61n74+t10*t13*abb61n48+t10*t2*abb61n70+t10*t3*abb61n66+t10*t4*abb61n67+t1&
      &0*t6*abb61n75+t11*t9*abb61n66+t12*t16*abb61n30+t12*t17*abb61n74+t12*t9*ab&
      &b61n76+t13*t16*abb61n43+t13*t17*abb61n55+t13*t18*abb61n56+t13*t9*abb61n65&
      &+t15*t6*abb61n66+t16*t5*abb61n32+t16*t6*abb61n40+t18*t5*abb61n66+t19*t5*a&
      &bb61n70+t2*t9*abb61n60+t3*t7*abb61n56+t3*t9*abb61n60+t4*t7*abb61n52+t4*t8&
      &*abb61n71+t4*t9*abb61n57+t5*t7*abb61n48+t5*t9*abb61n75+t6*t7*abb61n65+t6*&
      &t8*abb61n76+t6*abb61n61*(spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spva&
      &k2k6(3)*qshift(3)-spvak2k6(4)*qshift(4))+t11*t7*t9*abb61n26+t13*t15*t9*ab&
      &b61n26+t15*t6*t7*abb61n26)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh1
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
      complex(ki) :: t18
      complex(ki) :: t19
      complex(ki) :: t20
      complex(ki) :: t21
      complex(ki) :: t22
      complex(ki) :: t23
      complex(ki) :: t24
      complex(ki) :: t25
      real(ki) :: t26
      t26 = d(iv1,iv2)
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t2 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t3 = k1(iv1)
      t4 = spvak5k1(iv2)
      t5 = spvak5k4(iv2)
      t6 = k1(iv2)
      t7 = spvak5k1(iv1)
      t8 = spvak5k4(iv1)
      t9 = k2(iv1)
      t10 = spvak6k1(iv2)
      t11 = spvak6k2(iv2)
      t12 = k2(iv2)
      t13 = spvak6k1(iv1)
      t14 = spvak6k2(iv1)
      t15 = k6(iv1)
      t16 = k6(iv2)
      t17 = qshift(iv1)
      t18 = qshift(iv2)
      t19 = spvak2k1(iv1)
      t20 = t11*t19
      t21 = spvak2k1(iv2)
      t22 = t14*t21
      t23 = t11*t8
      t24 = t14*t5
      t25 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      brack = ((2.0_ki)*(t26*abb61n36+t1*t26*abb61n53+t2*t26*abb61n58+t26*abb61n&
      &68*(spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-spv&
      &ak6k1(4)*qshift(4))+t26*abb61n72*(spvak5k1(1)*qshift(1)-spvak5k1(2)*qshif&
      &t(2)-spvak5k1(3)*qshift(3)-spvak5k1(4)*qshift(4)))+t20*abb61n33+t22*abb61&
      &n33+t23*abb61n34+t24*abb61n34+spvak2k6(iv1)*t11*abb61n38+spvak2k6(iv2)*t1&
      &4*abb61n38+t1*t20*abb61n77+t1*t22*abb61n77+t10*t14*abb61n28+t10*t15*abb61&
      &n33+t10*t17*abb61n69+t10*t8*abb61n47+t10*t9*abb61n31+t11*t13*abb61n28+t11&
      &*t15*abb61n39+t11*t17*abb61n59+t11*t7*abb61n27+t11*t9*abb61n39+t12*t13*ab&
      &b61n31+t12*t14*abb61n39+t13*t16*abb61n33+t13*t18*abb61n69+t13*t5*abb61n47&
      &+t14*t16*abb61n39+t14*t18*abb61n59+t14*t4*abb61n27+t15*t5*abb61n41+t16*t8&
      &*abb61n41+t17*t4*abb61n73+t17*t5*abb61n54+t18*t7*abb61n73+t18*t8*abb61n54&
      &+t23*t25*abb61n77+t24*t25*abb61n77+t3*t4*abb61n29+t3*t5*abb61n42+t6*t7*ab&
      &b61n29+t6*t8*abb61n42+t19*t2*t5*abb61n77+t2*t21*t8*abb61n77)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh1
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
      t1 = spvak5k4(iv1)
      t2 = spvak5k4(iv2)
      t3 = spvak5k4(iv3)
      t4 = spvak6k2(iv1)
      t5 = spvak6k2(iv2)
      t6 = spvak6k2(iv3)
      t7 = spvak2k1(iv1)
      t8 = spvak2k1(iv2)
      t9 = spvak2k1(iv3)
      brack = ((2.0_ki)*(spvak5k1(iv1)*t10*abb61n30+spvak5k1(iv2)*t11*abb61n30+s&
      &pvak5k1(iv3)*t12*abb61n30+spvak6k1(iv1)*t10*abb61n32+spvak6k1(iv2)*t11*ab&
      &b61n32+spvak6k1(iv3)*t12*abb61n32+t1*t10*abb61n43+t10*t4*abb61n40+t11*t2*&
      &abb61n43+t11*t5*abb61n40+t12*t3*abb61n43+t12*t6*abb61n40)+t1*t5*t9*abb61n&
      &26+t1*t6*t8*abb61n26+t2*t4*t9*abb61n26+t2*t6*t7*abb61n26+t3*t4*t8*abb61n2&
      &6+t3*t5*t7*abb61n26)
   end  function brack_4
!---#] function brack_4:

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
      use p6_ubbar_hepneg_groups, only: tensrec_info_group1
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
end module p6_ubbar_hepneg_d61h1l1d
