module     p0_dbaru_hepneg_d61h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p0_dbaru_hepneg/helicity0/d61h0l1d.f90
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


   public :: derivative, reconstruct_d61
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
      real(ki) :: t7
      real(ki) :: t8
      real(ki) :: t9
      real(ki) :: t10
      t7 = dotproduct(qshift, qshift)
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t8 = dotproduct(k1, qshift)
      t2 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t3 = (spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-sp&
      &vae6k2(4)*qshift(4))
      t9 = dotproduct(k6, qshift)
      t4 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t10 = dotproduct(k2, qshift)
      t5 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t6 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      brack = (abb61n29+t1*abb61n56+t10*abb61n76+t2*abb61n69+t3*abb61n71+t4*abb6&
      &1n75+t7*abb61n38+t8*abb61n63+t9*abb61n72+t1*t10*abb61n36+t1*t2*abb61n45+t&
      &1*t3*abb61n41+t1*t4*abb61n31+t1*t7*abb61n81+t1*t9*abb61n36+t10*t5*abb61n3&
      &2+t2*t4*abb61n49+t2*t5*abb61n42+t2*t6*abb61n54+t2*t7*abb61n61+t2*t8*abb61&
      &n52+t2*t9*abb61n53+t2*abb61n37*(spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(&
      &2)-spvak1k4(3)*qshift(3)-spvak1k4(4)*qshift(4))+t3*t7*abb61n66+t3*t8*abb6&
      &1n47+t5*t7*abb61n85+t5*t9*abb61n34+t1*t2*t6*abb61n89)
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
      real(ki) :: t18
      real(ki) :: t19
      real(ki) :: t20
      real(ki) :: t21
      t1 = e6(iv1)
      t2 = k1(iv1)
      t3 = k2(iv1)
      t4 = k6(iv1)
      t5 = qshift(iv1)
      t6 = spvae6k2(iv1)
      t7 = spvak5k4(iv1)
      t8 = spvak6k2(iv1)
      t18 = dotproduct(qshift, qshift)
      t9 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t19 = dotproduct(k6, qshift)
      t20 = dotproduct(k1, qshift)
      t10 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-s&
      &pvak6k2(4)*qshift(4))
      t11 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t12 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-s&
      &pvak5k2(4)*qshift(4))
      t13 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t14 = (spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-s&
      &pvae6k2(4)*qshift(4))
      t15 = spvak1k2(iv1)
      t16 = spvak5k2(iv1)
      t21 = dotproduct(k2, qshift)
      t17 = t7*abb61n79
      brack = (t1*abb61n46+t17*t19+t17*t21+t2*abb61n50+t3*abb61n39+t4*abb61n43+t&
      &5*abb61n77+t6*abb61n44+t7*abb61n55+t8*abb61n40+spvak1k4(iv1)*t13*abb61n78&
      &+t1*t10*abb61n64+t1*t11*abb61n70+t1*t12*abb61n73+t1*t18*abb61n51+t1*t19*a&
      &bb61n58+t1*t20*abb61n59+t1*t9*abb61n57+t1*abb61n78*(spvak1k4(1)*qshift(1)&
      &-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-spvak1k4(4)*qshift(4))+t10*t&
      &7*abb61n88+t11*t3*abb61n79+t11*t4*abb61n79+t11*t5*abb61n80+t11*t6*abb61n7&
      &4+t11*t8*abb61n88+t12*t3*abb61n87+t12*t4*abb61n83+t12*t5*abb61n84+t13*t15&
      &*abb61n57+t13*t16*abb61n73+t13*t2*abb61n59+t13*t4*abb61n58+t13*t5*abb61n6&
      &0+t13*t7*abb61n70+t13*t8*abb61n64+t14*t2*abb61n68+t14*t5*abb61n65+t14*t7*&
      &abb61n74+t16*t18*abb61n33+t16*t19*abb61n83+t16*t21*abb61n87+t18*t6*abb61n&
      &48+t18*t7*abb61n35+t20*t6*abb61n68+t1*t11*t9*abb61n30+t11*t13*t15*abb61n3&
      &0+t13*t7*t9*abb61n30)
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
      complex(ki) :: t26
      complex(ki) :: t27
      real(ki) :: t28
      t28 = d(iv1,iv2)
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t3 = e6(iv1)
      t4 = k1(iv2)
      t5 = k6(iv2)
      t6 = qshift(iv2)
      t7 = spvak1k2(iv2)
      t8 = t3*t7
      t9 = spvak5k2(iv2)
      t10 = spvak5k4(iv2)
      t11 = t10*t3
      t12 = spvak6k2(iv2)
      t13 = e6(iv2)
      t14 = k1(iv1)
      t15 = k6(iv1)
      t16 = qshift(iv1)
      t17 = spvak1k2(iv1)
      t18 = t13*t17
      t19 = spvak5k2(iv1)
      t20 = spvak5k4(iv1)
      t21 = t13*t20
      t22 = spvak6k2(iv1)
      t23 = spvae6k2(iv2)
      t24 = spvae6k2(iv1)
      t25 = k2(iv1)
      t26 = k2(iv2)
      t27 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      brack = ((2.0_ki)*(t28*abb61n38+t1*t28*abb61n61+t2*t28*abb61n81+t28*abb61n&
      &66*(spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-spv&
      &ae6k2(4)*qshift(4))+t28*abb61n85*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshif&
      &t(2)-spvak5k2(3)*qshift(3)-spvak5k2(4)*qshift(4)))+t11*abb61n45+t18*abb61&
      &n54+t21*abb61n45+t8*abb61n54+spvak1k4(iv1)*t13*abb61n37+spvak1k4(iv2)*t3*&
      &abb61n37+t10*t15*abb61n36+t10*t16*abb61n82+t10*t22*abb61n31+t10*t24*abb61&
      &n41+t10*t25*abb61n36+t11*t27*abb61n89+t12*t20*abb61n31+t12*t3*abb61n49+t1&
      &3*t14*abb61n52+t13*t15*abb61n53+t13*t16*abb61n62+t13*t19*abb61n42+t13*t22&
      &*abb61n49+t14*t23*abb61n47+t15*t9*abb61n34+t16*t23*abb61n67+t16*t9*abb61n&
      &86+t18*t2*abb61n89+t19*t26*abb61n32+t19*t5*abb61n34+t19*t6*abb61n86+t2*t8&
      &*abb61n89+t20*t23*abb61n41+t20*t26*abb61n36+t20*t5*abb61n36+t20*t6*abb61n&
      &82+t21*t27*abb61n89+t24*t4*abb61n47+t24*t6*abb61n67+t25*t9*abb61n32+t3*t4&
      &*abb61n52+t3*t5*abb61n53+t3*t6*abb61n62+t3*t9*abb61n42+t1*t10*t17*abb61n8&
      &9+t1*t20*t7*abb61n89)
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
      t1 = e6(iv1)
      t10 = d(iv2,iv3)
      t2 = e6(iv2)
      t11 = d(iv1,iv3)
      t3 = e6(iv3)
      t12 = d(iv1,iv2)
      t4 = spvak5k4(iv1)
      t5 = spvak5k4(iv2)
      t6 = spvak5k4(iv3)
      t7 = spvak1k2(iv2)
      t8 = spvak1k2(iv3)
      t9 = spvak1k2(iv1)
      brack = ((2.0_ki)*(spvae6k2(iv1)*t10*abb61n48+spvae6k2(iv2)*t11*abb61n48+s&
      &pvae6k2(iv3)*t12*abb61n48+spvak5k2(iv1)*t10*abb61n33+spvak5k2(iv2)*t11*ab&
      &b61n33+spvak5k2(iv3)*t12*abb61n33+t1*t10*abb61n51+t10*t4*abb61n35+t11*t2*&
      &abb61n51+t11*t5*abb61n35+t12*t3*abb61n51+t12*t6*abb61n35)+t1*t5*t8*abb61n&
      &30+t1*t6*t7*abb61n30+t2*t4*t8*abb61n30+t2*t6*t9*abb61n30+t3*t4*t7*abb61n3&
      &0+t3*t5*t9*abb61n30)
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
!---#[ subroutine reconstruct_d61:
   subroutine     reconstruct_d61(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group1
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
end module p0_dbaru_hepneg_d61h0l1d
