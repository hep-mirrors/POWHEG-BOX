module     p11_csbar_hepneg_d61h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p11_csbar_hepneg/helicity0/d61h0l1d.f90
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
      real(ki) :: t8
      real(ki) :: t9
      real(ki) :: t10
      real(ki) :: t11
      t8 = dotproduct(qshift, qshift)
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t9 = dotproduct(k2, qshift)
      t10 = dotproduct(k6, qshift)
      t2 = (spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*qshift(3)-sp&
      &vae6k1(4)*qshift(4))
      t3 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t11 = dotproduct(k1, qshift)
      t4 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t5 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t6 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t7 = t1*abb61n52
      brack = (abb61n29+t1*abb61n56+t10*t7+t10*abb61n62+t11*abb61n75+t2*abb61n64&
      &+t3*abb61n65+t4*abb61n78+t7*t9+t8*abb61n41+t9*abb61n57+t1*t3*abb61n47+t1*&
      &t4*abb61n45+t1*t5*abb61n39+t1*t6*abb61n46+t1*t8*abb61n59+t1*abb61n50*(spv&
      &ak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-spvak2k6(4&
      &)*qshift(4))+t10*t2*abb61n42+t10*t3*abb61n34+t11*t3*abb61n35+t11*t5*abb61&
      &n33+t2*t3*abb61n38+t2*t8*abb61n71+t2*t9*abb61n44+t3*t4*abb61n31+t3*t8*abb&
      &61n80+t5*t8*abb61n86+t1*t3*t6*abb61n89)
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
      real(ki) :: t18
      real(ki) :: t19
      real(ki) :: t20
      real(ki) :: t21
      t1 = e6(iv1)
      t2 = k1(iv1)
      t3 = k2(iv1)
      t4 = k6(iv1)
      t5 = qshift(iv1)
      t6 = spvae6k1(iv1)
      t7 = spvak5k4(iv1)
      t8 = spvak6k1(iv1)
      t18 = dotproduct(qshift, qshift)
      t19 = dotproduct(k2, qshift)
      t9 = t1*abb61n61
      t20 = dotproduct(k6, qshift)
      t10 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t11 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      t12 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-s&
      &pvak6k1(4)*qshift(4))
      t13 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-s&
      &pvak5k1(4)*qshift(4))
      t14 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t15 = (spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*qshift(3)-s&
      &pvae6k1(4)*qshift(4))
      t16 = spvak2k1(iv1)
      t17 = spvak5k1(iv1)
      t21 = dotproduct(k1, qshift)
      brack = (t1*abb61n55+t19*t9+t2*abb61n40+t20*t9+t3*abb61n54+t4*abb61n51+t5*&
      &abb61n74+t6*abb61n49+t7*abb61n48+t8*abb61n37+spvak2k6(iv1)*t14*abb61n63+t&
      &1*t10*abb61n66+t1*t11*abb61n67+t1*t12*abb61n68+t1*t13*abb61n76+t1*t18*abb&
      &61n53+t1*abb61n63*(spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3&
      &)*qshift(3)-spvak2k6(4)*qshift(4))+t10*t2*abb61n82+t10*t4*abb61n83+t10*t5&
      &*abb61n79+t10*t6*abb61n77+t10*t8*abb61n88+t12*t7*abb61n88+t13*t2*abb61n84&
      &+t13*t5*abb61n85+t14*t16*abb61n67+t14*t17*abb61n76+t14*t3*abb61n61+t14*t4&
      &*abb61n61+t14*t5*abb61n58+t14*t7*abb61n66+t14*t8*abb61n68+t15*t3*abb61n69&
      &+t15*t4*abb61n73+t15*t5*abb61n70+t15*t7*abb61n77+t17*t18*abb61n32+t17*t21&
      &*abb61n84+t18*t6*abb61n43+t18*t7*abb61n36+t19*t6*abb61n69+t20*t6*abb61n73&
      &+t20*t7*abb61n83+t21*t7*abb61n82+t1*t10*t11*abb61n30+t10*t14*t16*abb61n30&
      &+t11*t14*t7*abb61n30)
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
      complex(ki) :: t26
      complex(ki) :: t27
      real(ki) :: t28
      t28 = d(iv1,iv2)
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t3 = e6(iv1)
      t4 = k2(iv2)
      t5 = k6(iv2)
      t6 = qshift(iv2)
      t7 = spvak2k1(iv2)
      t8 = t3*t7
      t9 = spvak5k1(iv2)
      t10 = spvak5k4(iv2)
      t11 = t10*t3
      t12 = spvak6k1(iv2)
      t13 = e6(iv2)
      t14 = k2(iv1)
      t15 = k6(iv1)
      t16 = qshift(iv1)
      t17 = spvak2k1(iv1)
      t18 = t13*t17
      t19 = spvak5k1(iv1)
      t20 = spvak5k4(iv1)
      t21 = t13*t20
      t22 = spvak6k1(iv1)
      t23 = k1(iv1)
      t24 = k1(iv2)
      t25 = spvae6k1(iv2)
      t26 = spvae6k1(iv1)
      t27 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-s&
      &pvak2k1(4)*qshift(4))
      brack = ((2.0_ki)*(t28*abb61n41+t1*t28*abb61n59+t2*t28*abb61n80+t28*abb61n&
      &71*(spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*qshift(3)-spv&
      &ae6k1(4)*qshift(4))+t28*abb61n86*(spvak5k1(1)*qshift(1)-spvak5k1(2)*qshif&
      &t(2)-spvak5k1(3)*qshift(3)-spvak5k1(4)*qshift(4)))+t11*abb61n47+t18*abb61&
      &n46+t21*abb61n47+t8*abb61n46+spvak2k6(iv1)*t13*abb61n50+spvak2k6(iv2)*t3*&
      &abb61n50+t10*t15*abb61n34+t10*t16*abb61n81+t10*t22*abb61n31+t10*t23*abb61&
      &n35+t10*t26*abb61n38+t11*t27*abb61n89+t12*t20*abb61n31+t12*t3*abb61n45+t1&
      &3*t14*abb61n52+t13*t15*abb61n52+t13*t16*abb61n60+t13*t19*abb61n39+t13*t22&
      &*abb61n45+t14*t25*abb61n44+t15*t25*abb61n42+t16*t25*abb61n72+t16*t9*abb61&
      &n87+t18*t2*abb61n89+t19*t24*abb61n33+t19*t6*abb61n87+t2*t8*abb61n89+t20*t&
      &24*abb61n35+t20*t25*abb61n38+t20*t5*abb61n34+t20*t6*abb61n81+t21*t27*abb6&
      &1n89+t23*t9*abb61n33+t26*t4*abb61n44+t26*t5*abb61n42+t26*t6*abb61n72+t3*t&
      &4*abb61n52+t3*t5*abb61n52+t3*t6*abb61n60+t3*t9*abb61n39+t1*t10*t17*abb61n&
      &89+t1*t20*t7*abb61n89)
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
      t1 = e6(iv1)
      t10 = d(iv2,iv3)
      t2 = e6(iv2)
      t11 = d(iv1,iv3)
      t3 = e6(iv3)
      t12 = d(iv1,iv2)
      t4 = spvak5k4(iv1)
      t5 = spvak5k4(iv2)
      t6 = spvak5k4(iv3)
      t7 = spvak2k1(iv2)
      t8 = spvak2k1(iv3)
      t9 = spvak2k1(iv1)
      brack = ((2.0_ki)*(spvae6k1(iv1)*t10*abb61n43+spvae6k1(iv2)*t11*abb61n43+s&
      &pvae6k1(iv3)*t12*abb61n43+spvak5k1(iv1)*t10*abb61n32+spvak5k1(iv2)*t11*ab&
      &b61n32+spvak5k1(iv3)*t12*abb61n32+t1*t10*abb61n53+t10*t4*abb61n36+t11*t2*&
      &abb61n53+t11*t5*abb61n36+t12*t3*abb61n53+t12*t6*abb61n36)+t1*t5*t8*abb61n&
      &30+t1*t6*t7*abb61n30+t2*t4*t8*abb61n30+t2*t6*t9*abb61n30+t3*t4*t7*abb61n3&
      &0+t3*t5*t9*abb61n30)
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
end module p11_csbar_hepneg_d61h0l1d
