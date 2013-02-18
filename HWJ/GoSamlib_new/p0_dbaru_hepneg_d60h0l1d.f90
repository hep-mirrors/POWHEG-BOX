module     p0_dbaru_hepneg_d60h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p0_dbaru_hepneg/helicity0/d60h0l1d.f90
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
!---#[ function brack_1:
   pure function brack_1(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      real(ki) :: t7
      real(ki) :: t8
      real(ki) :: t9
      real(ki) :: t10
      t7 = dotproduct(k2, qshift)
      t8 = dotproduct(qshift, qshift)
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t9 = dotproduct(k1, qshift)
      t2 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t10 = dotproduct(k6, qshift)
      t3 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t4 = (spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-sp&
      &vak1k4(4)*qshift(4))
      t5 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t6 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      brack = (abb60n28+t1*abb60n44+t10*abb60n48+t2*abb60n47+t3*abb60n49+t7*abb6&
      &0n46+t7*t7*abb60n35+t8*abb60n42+t9*abb60n45+t1*t7*abb60n40+t1*t8*abb60n50&
      &+t1*abb60n41*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qsh&
      &ift(3)-spvak1k6(4)*qshift(4))+t10*t4*abb60n31+t10*t5*abb60n39+t10*t7*abb6&
      &0n33+t2*t3*abb60n34+t3*t4*abb60n29+t3*t5*abb60n32+t3*t6*abb60n37+t3*t7*ab&
      &b60n36+t3*t9*abb60n36+t4*t7*abb60n30+t4*t8*abb60n57+t5*t7*abb60n38+t5*t8*&
      &abb60n52+t7*t8*abb60n55+t1*t3*t6*abb60n59)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
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
      real(ki) :: t17
      real(ki) :: t18
      real(ki) :: t19
      t1 = k1(iv1)
      t2 = k2(iv1)
      t3 = k6(iv1)
      t4 = qshift(iv1)
      t5 = spvak2k6(iv1)
      t6 = spvak5k4(iv1)
      t7 = spvak6k2(iv1)
      t8 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t9 = (spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-sp&
      &vak1k4(4)*qshift(4))
      t17 = dotproduct(k6, qshift)
      t10 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-s&
      &pvak5k2(4)*qshift(4))
      t11 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t18 = dotproduct(k2, qshift)
      t19 = dotproduct(qshift, qshift)
      t12 = spvak1k2(iv1)
      t13 = spvak1k4(iv1)
      t14 = t5*abb60n36
      t15 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      t16 = spvak5k2(iv1)
      brack = (t1*abb60n45+t14*t18+t14*dotproduct(k1, qshift)+t2*abb60n46+t3*abb&
      &60n48+t4*abb60n43+t5*abb60n49+t6*abb60n44+t7*abb60n47+spvak1k6(iv1)*t11*a&
      &bb60n41+t1*t8*abb60n36+t10*t2*abb60n38+t10*t3*abb60n39+t10*t4*abb60n53+t1&
      &0*t5*abb60n32+t11*t2*abb60n40+t11*t4*abb60n51+t12*t8*abb60n37+t13*t17*abb&
      &60n31+t13*t18*abb60n30+t13*t19*abb60n57+t13*t8*abb60n29+t15*t5*abb60n37+t&
      &16*t17*abb60n39+t16*t18*abb60n38+t16*t19*abb60n52+t16*t8*abb60n32+t17*t2*&
      &abb60n33+t18*t2*abb60n54+t18*t3*abb60n33+t18*t4*abb60n56+t18*t6*abb60n40+&
      &t19*t2*abb60n55+t19*t6*abb60n50+t2*t8*abb60n36+t2*t9*abb60n30+t3*t9*abb60&
      &n31+t4*t9*abb60n58+t5*t9*abb60n29+t5*abb60n34*(spvak6k2(1)*qshift(1)-spva&
      &k6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-spvak6k2(4)*qshift(4))+t6*abb60n4&
      &1*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spva&
      &k1k6(4)*qshift(4))+t7*t8*abb60n34+t11*t12*t8*abb60n59+t11*t15*t5*abb60n59&
      &+t15*t6*t8*abb60n59)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
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
      t3 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t4 = spvak2k6(iv2)
      t5 = spvak2k6(iv1)
      t6 = k6(iv2)
      t7 = qshift(iv2)
      t8 = spvak1k4(iv2)
      t9 = spvak5k2(iv2)
      t10 = spvak5k4(iv2)
      t11 = k6(iv1)
      t12 = qshift(iv1)
      t13 = spvak1k4(iv1)
      t14 = spvak5k2(iv1)
      t15 = spvak5k4(iv1)
      t16 = spvak1k2(iv1)
      t17 = t16*t4
      t18 = spvak1k2(iv2)
      t19 = t18*t5
      t20 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-s&
      &pvak2k6(4)*qshift(4))
      t21 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      brack = ((2.0_ki)*(t22*abb60n42+t1*t2*abb60n35+t22*t3*abb60n50+t22*abb60n5&
      &2*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-spva&
      &k5k2(4)*qshift(4))+t22*abb60n55*dotproduct(k2, qshift)+t22*abb60n57*(spva&
      &k1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-spvak1k4(4)&
      &*qshift(4)))+t17*abb60n37+t19*abb60n37+k1(iv1)*t4*abb60n36+k1(iv2)*t5*abb&
      &60n36+spvak1k6(iv1)*t10*abb60n41+spvak1k6(iv2)*t15*abb60n41+spvak6k2(iv1)&
      &*t4*abb60n34+spvak6k2(iv2)*t5*abb60n34+t1*t10*abb60n40+t1*t4*abb60n36+t1*&
      &t6*abb60n33+t1*t7*abb60n56+t1*t8*abb60n30+t1*t9*abb60n38+t10*t12*abb60n51&
      &+t11*t2*abb60n33+t11*t8*abb60n31+t11*t9*abb60n39+t12*t2*abb60n56+t12*t8*a&
      &bb60n58+t12*t9*abb60n53+t13*t2*abb60n30+t13*t4*abb60n29+t13*t6*abb60n31+t&
      &13*t7*abb60n58+t14*t2*abb60n38+t14*t4*abb60n32+t14*t6*abb60n39+t14*t7*abb&
      &60n53+t15*t2*abb60n40+t15*t7*abb60n51+t17*t3*abb60n59+t19*t3*abb60n59+t2*&
      &t5*abb60n36+t5*t8*abb60n29+t5*t9*abb60n32+t10*t16*t20*abb60n59+t10*t21*t5&
      &*abb60n59+t15*t18*t20*abb60n59+t15*t21*t4*abb60n59)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
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
      t4 = spvak1k2(iv1)
      t5 = spvak2k6(iv2)
      t6 = spvak2k6(iv3)
      t7 = spvak1k2(iv2)
      t8 = spvak2k6(iv1)
      t9 = spvak1k2(iv3)
      brack = ((2.0_ki)*(k2(iv1)*t10*abb60n55+k2(iv2)*t11*abb60n55+k2(iv3)*t12*a&
      &bb60n55+spvak1k4(iv1)*t10*abb60n57+spvak1k4(iv2)*t11*abb60n57+spvak1k4(iv&
      &3)*t12*abb60n57+spvak5k2(iv1)*t10*abb60n52+spvak5k2(iv2)*t11*abb60n52+spv&
      &ak5k2(iv3)*t12*abb60n52+t1*t10*abb60n50+t11*t2*abb60n50+t12*t3*abb60n50)+&
      &t1*t5*t9*abb60n59+t1*t6*t7*abb60n59+t2*t4*t6*abb60n59+t2*t8*t9*abb60n59+t&
      &3*t4*t5*abb60n59+t3*t7*t8*abb60n59)
   end  function brack_4
!---#] function brack_4:

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
