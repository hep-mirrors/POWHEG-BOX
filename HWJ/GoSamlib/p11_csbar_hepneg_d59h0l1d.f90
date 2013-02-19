module     p11_csbar_hepneg_d59h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p11_csbar_hepneg/helicity0/d59h0l1d.f90
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
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
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
      t10 = dotproduct(k1, qshift)
      t11 = dotproduct(k6, qshift)
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t3 = (spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*qshift(3)-sp&
      &vae6k1(4)*qshift(4))
      t12 = dotproduct(k2, qshift)
      t4 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t5 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t6 = (spvak2k4(1)*qshift(1)-spvak2k4(2)*qshift(2)-spvak2k4(3)*qshift(3)-sp&
      &vak2k4(4)*qshift(4))
      t7 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t8 = t1*abb59n55
      brack = (abb59n31+t1*abb59n60+t10*t8+t10*abb59n61+t11*abb59n62+t12*t8+t12*&
      &abb59n69+t2*abb59n63+t3*abb59n68+t9*abb59n54+t1*t4*abb59n49+t1*t5*abb59n4&
      &5+t1*t6*abb59n53+t1*t7*abb59n52+t10*t2*abb59n41+t10*t3*abb59n46+t10*t5*ab&
      &b59n36+t10*t6*abb59n38+t11*t3*abb59n48+t11*t5*abb59n34+t11*t6*abb59n40+t2&
      &*t3*abb59n44+t2*t4*abb59n33+t2*t7*abb59n43+t2*t9*abb59n80+t2*abb59n37*(sp&
      &vak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-spvak2k6(&
      &4)*qshift(4))+t3*t9*abb59n73+t5*t9*abb59n91+t6*t9*abb59n85+t1*t2*t7*abb59&
      &n95)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
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
      t6 = spvae6k1(iv1)
      t7 = spvak5k4(iv1)
      t20 = dotproduct(k1, qshift)
      t8 = t1*abb59n64
      t9 = (spvak2k4(1)*qshift(1)-spvak2k4(2)*qshift(2)-spvak2k4(3)*qshift(3)-sp&
      &vak2k4(4)*qshift(4))
      t10 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      t11 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-s&
      &pvak6k1(4)*qshift(4))
      t12 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-s&
      &pvak5k1(4)*qshift(4))
      t13 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t14 = (spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*qshift(3)-s&
      &pvae6k1(4)*qshift(4))
      t15 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t21 = dotproduct(qshift, qshift)
      t22 = dotproduct(k6, qshift)
      t16 = spvak2k1(iv1)
      t17 = spvak2k4(iv1)
      t18 = spvak5k1(iv1)
      t19 = spvak6k1(iv1)
      brack = (t1*abb59n59+t2*abb59n58+t20*t8+t3*abb59n50+t4*abb59n57+t5*abb59n6&
      &5+t6*abb59n51+t7*abb59n56+t8*dotproduct(k2, qshift)+spvak2k6(iv1)*t15*abb&
      &59n88+t1*t10*abb59n67+t1*t11*abb59n70+t1*t12*abb59n76+t1*t9*abb59n66+t10*&
      &t7*abb59n78+t11*t7*abb59n94+t12*t2*abb59n89+t12*t4*abb59n93+t12*t5*abb59n&
      &90+t13*t16*abb59n67+t13*t17*abb59n66+t13*t18*abb59n76+t13*t19*abb59n70+t1&
      &3*t2*abb59n64+t13*t3*abb59n64+t14*t2*abb59n75+t14*t4*abb59n71+t14*t5*abb5&
      &9n72+t14*t7*abb59n77+t15*t16*abb59n78+t15*t19*abb59n94+t15*t2*abb59n82+t1&
      &5*t5*abb59n79+t15*t6*abb59n77+t17*t20*abb59n87+t17*t21*abb59n39+t17*t22*a&
      &bb59n83+t18*t20*abb59n89+t18*t21*abb59n35+t18*t22*abb59n93+t2*t9*abb59n87&
      &+t20*t6*abb59n75+t20*t7*abb59n82+t21*t6*abb59n47+t21*t7*abb59n42+t22*t6*a&
      &bb59n71+t4*t9*abb59n83+t5*t9*abb59n84+t7*abb59n88*(spvak2k6(1)*qshift(1)-&
      &spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-spvak2k6(4)*qshift(4))+t1*t10&
      &*t15*abb59n32+t10*t13*t7*abb59n32+t13*t15*t16*abb59n32)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
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
      t3 = k1(iv2)
      t4 = spvak2k1(iv2)
      t5 = t2*t4
      t6 = spvak2k4(iv2)
      t7 = spvak5k1(iv2)
      t8 = spvak6k1(iv2)
      t9 = e6(iv2)
      t10 = k1(iv1)
      t11 = spvak2k1(iv1)
      t12 = t11*t9
      t13 = spvak2k4(iv1)
      t14 = spvak5k1(iv1)
      t15 = spvak6k1(iv1)
      t16 = spvae6k1(iv2)
      t17 = spvak5k4(iv2)
      t18 = spvae6k1(iv1)
      t19 = spvak5k4(iv1)
      t20 = k6(iv1)
      t21 = k6(iv2)
      t22 = qshift(iv1)
      t23 = qshift(iv2)
      t11 = t11*t17
      t4 = t19*t4
      t24 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      t25 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      brack = ((2.0_ki)*(t26*abb59n54+t1*t26*abb59n80+t26*abb59n73*(spvae6k1(1)*&
      &qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*qshift(3)-spvae6k1(4)*qshift(&
      &4))+t26*abb59n85*(spvak2k4(1)*qshift(1)-spvak2k4(2)*qshift(2)-spvak2k4(3)&
      &*qshift(3)-spvak2k4(4)*qshift(4))+t26*abb59n91*(spvak5k1(1)*qshift(1)-spv&
      &ak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-spvak5k1(4)*qshift(4)))+t11*abb5&
      &9n43+t12*abb59n52+t4*abb59n43+t5*abb59n52+k2(iv1)*t9*abb59n55+k2(iv2)*t2*&
      &abb59n55+spvak2k6(iv1)*t17*abb59n37+spvak2k6(iv2)*t19*abb59n37+t1*t12*abb&
      &59n95+t1*t5*abb59n95+t10*t16*abb59n46+t10*t17*abb59n41+t10*t6*abb59n38+t1&
      &0*t7*abb59n36+t10*t9*abb59n55+t11*t25*abb59n95+t13*t21*abb59n40+t13*t23*a&
      &bb59n86+t13*t3*abb59n38+t13*t9*abb59n53+t14*t21*abb59n34+t14*t23*abb59n92&
      &+t14*t3*abb59n36+t14*t9*abb59n45+t15*t17*abb59n33+t15*t9*abb59n49+t16*t19&
      &*abb59n44+t16*t20*abb59n48+t16*t22*abb59n74+t17*t18*abb59n44+t17*t22*abb5&
      &9n81+t18*t21*abb59n48+t18*t23*abb59n74+t18*t3*abb59n46+t19*t23*abb59n81+t&
      &19*t3*abb59n41+t19*t8*abb59n33+t2*t3*abb59n55+t2*t6*abb59n53+t2*t7*abb59n&
      &45+t2*t8*abb59n49+t20*t6*abb59n40+t20*t7*abb59n34+t22*t6*abb59n86+t22*t7*&
      &abb59n92+t25*t4*abb59n95+t17*t2*t24*abb59n95+t19*t24*t9*abb59n95)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
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
      t5 = spvak2k1(iv2)
      t6 = spvak2k1(iv3)
      t7 = e6(iv2)
      t8 = spvak2k1(iv1)
      t9 = e6(iv3)
      brack = ((2.0_ki)*(spvae6k1(iv1)*t10*abb59n47+spvae6k1(iv2)*t11*abb59n47+s&
      &pvae6k1(iv3)*t12*abb59n47+spvak2k4(iv1)*t10*abb59n39+spvak2k4(iv2)*t11*ab&
      &b59n39+spvak2k4(iv3)*t12*abb59n39+spvak5k1(iv1)*t10*abb59n35+spvak5k1(iv2&
      &)*t11*abb59n35+spvak5k1(iv3)*t12*abb59n35+t1*t10*abb59n42+t11*t2*abb59n42&
      &+t12*t3*abb59n42)+t1*t5*t9*abb59n32+t1*t6*t7*abb59n32+t2*t4*t6*abb59n32+t&
      &2*t8*t9*abb59n32+t3*t4*t5*abb59n32+t3*t7*t8*abb59n32)
   end  function brack_4
!---#] function brack_4:

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
      integer :: t1
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
