module     p6_ubbar_hepneg_d60h1l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p6_ubbar_hepneg/helicity1/d60h1l1d.f90
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


   public :: derivative, reconstruct_d60
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
      real(ki) :: t5
      real(ki) :: t6
      real(ki) :: t7
      t5 = dotproduct(k2, qshift)
      t6 = dotproduct(qshift, qshift)
      t7 = dotproduct(k6, qshift)
      t1 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t2 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t3 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t4 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      brack = (abb60n24+t1*abb60n40+t2*abb60n41+t3*abb60n42+t5*abb60n39+t5*t5*ab&
      &b60n32+t6*abb60n36+t7*abb60n37+t1*t3*abb60n26+t1*t5*abb60n29+t1*t6*abb60n&
      &48+t1*t7*abb60n31+t2*t3*abb60n25+t2*t5*abb60n28+t2*t6*abb60n50+t2*t7*abb6&
      &0n27+t3*t4*abb60n30+t3*t5*abb60n35+t3*t6*abb60n43+t3*abb60n33*(spvak2k6(1&
      &)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-spvak2k6(4)*qshif&
      &t(4))+t5*t6*abb60n45+t5*t7*abb60n34+t3*t4*abb60n52*(spvak5k4(1)*qshift(1)&
      &-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-spvak5k4(4)*qshift(4)))
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
      real(ki) :: t13
      real(ki) :: t14
      real(ki) :: t15
      t1 = k2(iv1)
      t2 = k6(iv1)
      t3 = qshift(iv1)
      t4 = spvak5k1(iv1)
      t5 = spvak6k1(iv1)
      t6 = spvak6k2(iv1)
      t7 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t8 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t13 = dotproduct(k6, qshift)
      t9 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t14 = dotproduct(qshift, qshift)
      t15 = dotproduct(k2, qshift)
      t10 = spvak2k1(iv1)
      t11 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      t12 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      brack = (t1*abb60n39+t2*abb60n37+t3*abb60n38+t4*abb60n41+t5*abb60n40+t6*ab&
      &b60n42+spvak2k6(iv1)*t9*abb60n33+t1*t13*abb60n34+t1*t14*abb60n45+t1*t15*a&
      &bb60n47+t1*t7*abb60n28+t1*t8*abb60n29+t1*t9*abb60n35+t10*t9*abb60n30+t11*&
      &t6*abb60n30+t13*t4*abb60n27+t13*t5*abb60n31+t14*t4*abb60n50+t14*t5*abb60n&
      &48+t14*t6*abb60n43+t15*t2*abb60n34+t15*t3*abb60n46+t15*t4*abb60n28+t15*t5&
      &*abb60n29+t15*t6*abb60n35+t2*t7*abb60n27+t2*t8*abb60n31+t3*t7*abb60n51+t3&
      &*t8*abb60n49+t3*t9*abb60n44+t4*t9*abb60n25+t5*t9*abb60n26+t6*t7*abb60n25+&
      &t6*t8*abb60n26+t6*abb60n33*(spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-s&
      &pvak2k6(3)*qshift(3)-spvak2k6(4)*qshift(4))+spvak5k4(iv1)*t11*t9*abb60n52&
      &+t10*t12*t9*abb60n52+t11*t12*t6*abb60n52)
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
      real(ki) :: t22
      t22 = d(iv1,iv2)
      t1 = k2(iv1)
      t2 = k2(iv2)
      t3 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t4 = k6(iv2)
      t5 = qshift(iv2)
      t6 = spvak5k1(iv2)
      t7 = spvak6k1(iv2)
      t8 = spvak6k2(iv2)
      t9 = k6(iv1)
      t10 = qshift(iv1)
      t11 = spvak5k1(iv1)
      t12 = spvak6k1(iv1)
      t13 = spvak6k2(iv1)
      t14 = spvak2k1(iv1)
      t15 = t14*t8
      t16 = spvak2k1(iv2)
      t17 = t13*t16
      t18 = spvak5k4(iv2)
      t19 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t20 = spvak5k4(iv1)
      t21 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      brack = ((2.0_ki)*(t22*abb60n36+t1*t2*abb60n32+t22*t3*abb60n43+t22*abb60n4&
      &5*dotproduct(k2, qshift)+t22*abb60n48*(spvak6k1(1)*qshift(1)-spvak6k1(2)*&
      &qshift(2)-spvak6k1(3)*qshift(3)-spvak6k1(4)*qshift(4))+t22*abb60n50*(spva&
      &k5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-spvak5k1(4)&
      &*qshift(4)))+t15*abb60n30+t17*abb60n30+spvak2k6(iv1)*t8*abb60n33+spvak2k6&
      &(iv2)*t13*abb60n33+t1*t4*abb60n34+t1*t5*abb60n46+t1*t6*abb60n28+t1*t7*abb&
      &60n29+t1*t8*abb60n35+t10*t2*abb60n46+t10*t6*abb60n51+t10*t7*abb60n49+t10*&
      &t8*abb60n44+t11*t2*abb60n28+t11*t4*abb60n27+t11*t5*abb60n51+t11*t8*abb60n&
      &25+t12*t2*abb60n29+t12*t4*abb60n31+t12*t5*abb60n49+t12*t8*abb60n26+t13*t2&
      &*abb60n35+t13*t5*abb60n44+t13*t6*abb60n25+t13*t7*abb60n26+t15*t19*abb60n5&
      &2+t17*t19*abb60n52+t2*t9*abb60n34+t6*t9*abb60n27+t7*t9*abb60n31+t13*t18*t&
      &21*abb60n52+t14*t18*t3*abb60n52+t16*t20*t3*abb60n52+t20*t21*t8*abb60n52)
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
      t1 = spvak6k2(iv1)
      t2 = spvak6k2(iv2)
      t3 = spvak6k2(iv3)
      t4 = spvak2k1(iv1)
      t5 = spvak5k4(iv2)
      t6 = spvak5k4(iv3)
      t7 = spvak2k1(iv2)
      t8 = spvak5k4(iv1)
      t9 = spvak2k1(iv3)
      brack = ((2.0_ki)*(k2(iv1)*t10*abb60n45+k2(iv2)*t11*abb60n45+k2(iv3)*t12*a&
      &bb60n45+spvak5k1(iv1)*t10*abb60n50+spvak5k1(iv2)*t11*abb60n50+spvak5k1(iv&
      &3)*t12*abb60n50+spvak6k1(iv1)*t10*abb60n48+spvak6k1(iv2)*t11*abb60n48+spv&
      &ak6k1(iv3)*t12*abb60n48+t1*t10*abb60n43+t11*t2*abb60n43+t12*t3*abb60n43)+&
      &t1*t5*t9*abb60n52+t1*t6*t7*abb60n52+t2*t4*t6*abb60n52+t2*t8*t9*abb60n52+t&
      &3*t4*t5*abb60n52+t3*t7*t8*abb60n52)
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
!---#[ subroutine reconstruct_d60:
   subroutine     reconstruct_d60(coeffs)
      use p6_ubbar_hepneg_groups, only: tensrec_info_group2
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
end module p6_ubbar_hepneg_d60h1l1d
