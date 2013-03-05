module     p0_dbaru_hepneg_d60h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p0_dbaru_hepneg/helicity0/d60h0l1d.f90
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
   pure function brack_1(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8
      real(ki) :: t9
      real(ki) :: t10
      real(ki) :: t11
      real(ki) :: t12
      t9 = dotproduct(qshift, qshift)
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t10 = dotproduct(k6, qshift)
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t3 = (spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-sp&
      &vae6k2(4)*qshift(4))
      t11 = dotproduct(k2, qshift)
      t4 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t12 = dotproduct(k1, qshift)
      t5 = (spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-sp&
      &vak1k4(4)*qshift(4))
      t6 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t7 = t1*abb60n46
      t8 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      brack = (abb60n31+t1*abb60n48+t10*abb60n49+t11*t7+t11*abb60n52+t12*t7+t12*&
      &abb60n54+t2*abb60n50+t3*abb60n51+t4*abb60n53+t9*abb60n40+t1*t4*abb60n45+t&
      &1*t5*abb60n39+t1*t6*abb60n42+t1*t8*abb60n47+t10*t3*abb60n43+t10*t5*abb60n&
      &33+t10*t6*abb60n37+t11*t2*abb60n38+t11*t3*abb60n44+t11*t5*abb60n34+t11*t6&
      &*abb60n36+t2*t3*abb60n41+t2*t4*abb60n35+t2*t9*abb60n58+t2*abb60n32*(spvak&
      &1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*&
      &qshift(4))+t3*t9*abb60n55+t5*t9*abb60n62+t6*t9*abb60n60+t1*t2*t8*abb60n64&
      &)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
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
      real(ki) :: t20
      real(ki) :: t21
      real(ki) :: t22
      t1 = e6(iv1)
      t2 = k1(iv1)
      t3 = k2(iv1)
      t4 = k6(iv1)
      t5 = qshift(iv1)
      t6 = spvae6k2(iv1)
      t7 = spvak5k4(iv1)
      t8 = spvak6k2(iv1)
      t9 = (spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-sp&
      &vak1k4(4)*qshift(4))
      t10 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-s&
      &pvak5k2(4)*qshift(4))
      t11 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-s&
      &pvak6k2(4)*qshift(4))
      t12 = t1*abb60n46
      t20 = dotproduct(k2, qshift)
      t13 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      t14 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t15 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t16 = (spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-s&
      &pvae6k2(4)*qshift(4))
      t21 = dotproduct(k6, qshift)
      t22 = dotproduct(qshift, qshift)
      t17 = spvak1k2(iv1)
      t18 = spvak1k4(iv1)
      t19 = spvak5k2(iv1)
      brack = (t1*abb60n48+t12*t20+t12*dotproduct(k1, qshift)+t2*abb60n54+t3*abb&
      &60n52+t4*abb60n49+t5*abb60n57+t6*abb60n51+t7*abb60n50+t8*abb60n53+spvak1k&
      &6(iv1)*t15*abb60n32+t1*t10*abb60n42+t1*t11*abb60n45+t1*t13*abb60n47+t1*t9&
      &*abb60n39+t10*t3*abb60n36+t10*t4*abb60n37+t10*t5*abb60n61+t11*t7*abb60n35&
      &+t14*t17*abb60n47+t14*t18*abb60n39+t14*t19*abb60n42+t14*t2*abb60n46+t14*t&
      &3*abb60n46+t14*t8*abb60n45+t15*t3*abb60n38+t15*t5*abb60n59+t15*t6*abb60n4&
      &1+t15*t8*abb60n35+t16*t3*abb60n44+t16*t4*abb60n43+t16*t5*abb60n56+t16*t7*&
      &abb60n41+t18*t20*abb60n34+t18*t21*abb60n33+t18*t22*abb60n62+t19*t20*abb60&
      &n36+t19*t21*abb60n37+t19*t22*abb60n60+t20*t6*abb60n44+t20*t7*abb60n38+t21&
      &*t6*abb60n43+t22*t6*abb60n55+t22*t7*abb60n58+t3*t9*abb60n34+t4*t9*abb60n3&
      &3+t5*t9*abb60n63+t7*abb60n32*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)&
      &-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshift(4))+t1*t13*t15*abb60n64+t13*t14&
      &*t7*abb60n64+t14*t15*t17*abb60n64)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
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
      t2 = e6(iv1)
      t3 = k2(iv2)
      t4 = spvak1k2(iv2)
      t5 = t2*t4
      t6 = spvak1k4(iv2)
      t7 = spvak5k2(iv2)
      t8 = spvak6k2(iv2)
      t9 = e6(iv2)
      t10 = k2(iv1)
      t11 = spvak1k2(iv1)
      t12 = t11*t9
      t13 = spvak1k4(iv1)
      t14 = spvak5k2(iv1)
      t15 = spvak6k2(iv1)
      t16 = spvae6k2(iv2)
      t17 = spvak5k4(iv2)
      t18 = spvae6k2(iv1)
      t19 = spvak5k4(iv1)
      t20 = k6(iv1)
      t21 = k6(iv2)
      t22 = qshift(iv1)
      t23 = qshift(iv2)
      t24 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      t25 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      brack = ((2.0_ki)*(t26*abb60n40+t1*t26*abb60n58+t26*abb60n55*(spvae6k2(1)*&
      &qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-spvae6k2(4)*qshift(&
      &4))+t26*abb60n60*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)&
      &*qshift(3)-spvak5k2(4)*qshift(4))+t26*abb60n62*(spvak1k4(1)*qshift(1)-spv&
      &ak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-spvak1k4(4)*qshift(4)))+t12*abb6&
      &0n47+t5*abb60n47+k1(iv1)*t9*abb60n46+k1(iv2)*t2*abb60n46+spvak1k6(iv1)*t1&
      &7*abb60n32+spvak1k6(iv2)*t19*abb60n32+t1*t12*abb60n64+t1*t5*abb60n64+t10*&
      &t16*abb60n44+t10*t17*abb60n38+t10*t6*abb60n34+t10*t7*abb60n36+t10*t9*abb6&
      &0n46+t13*t21*abb60n33+t13*t23*abb60n63+t13*t3*abb60n34+t13*t9*abb60n39+t1&
      &4*t21*abb60n37+t14*t23*abb60n61+t14*t3*abb60n36+t14*t9*abb60n42+t15*t17*a&
      &bb60n35+t15*t9*abb60n45+t16*t19*abb60n41+t16*t20*abb60n43+t16*t22*abb60n5&
      &6+t17*t18*abb60n41+t17*t22*abb60n59+t18*t21*abb60n43+t18*t23*abb60n56+t18&
      &*t3*abb60n44+t19*t23*abb60n59+t19*t3*abb60n38+t19*t8*abb60n35+t2*t3*abb60&
      &n46+t2*t6*abb60n39+t2*t7*abb60n42+t2*t8*abb60n45+t20*t6*abb60n33+t20*t7*a&
      &bb60n37+t22*t6*abb60n63+t22*t7*abb60n61+t11*t17*t25*abb60n64+t17*t2*t24*a&
      &bb60n64+t19*t24*t9*abb60n64+t19*t25*t4*abb60n64)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
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
      t4 = e6(iv1)
      t5 = spvak1k2(iv2)
      t6 = spvak1k2(iv3)
      t7 = e6(iv2)
      t8 = spvak1k2(iv1)
      t9 = e6(iv3)
      brack = ((2.0_ki)*(spvae6k2(iv1)*t10*abb60n55+spvae6k2(iv2)*t11*abb60n55+s&
      &pvae6k2(iv3)*t12*abb60n55+spvak1k4(iv1)*t10*abb60n62+spvak1k4(iv2)*t11*ab&
      &b60n62+spvak1k4(iv3)*t12*abb60n62+spvak5k2(iv1)*t10*abb60n60+spvak5k2(iv2&
      &)*t11*abb60n60+spvak5k2(iv3)*t12*abb60n60+t1*t10*abb60n58+t11*t2*abb60n58&
      &+t12*t3*abb60n58)+t1*t5*t9*abb60n64+t1*t6*t7*abb60n64+t2*t4*t6*abb60n64+t&
      &2*t8*t9*abb60n64+t3*t4*t5*abb60n64+t3*t7*t8*abb60n64)
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
         numerator = (cond(epspow.eq.t1,brack_1,Q,mu2))
         return
      end if
      if(deg.eq.1) then
         numerator = (cond(epspow.eq.t1,brack_2,Q,mu2))
         return
      end if
      if(deg.eq.2) then
         numerator = (cond(epspow.eq.t1,brack_3,Q,mu2))
         return
      end if
      if(deg.eq.3) then
         numerator = (cond(epspow.eq.t1,brack_4,Q,mu2))
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
