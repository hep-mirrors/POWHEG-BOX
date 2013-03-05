module     p1_dbarc_hepneg_d59h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p1_dbarc_hepneg/helicity0/d59h0l1d.f90
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
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      t8 = dotproduct(qshift, qshift)
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t9 = dotproduct(k6, qshift)
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t3 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t4 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t5 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t10 = dotproduct(k1, qshift)
      t6 = (spvak1e6(1)*qshift(1)-spvak1e6(2)*qshift(2)-spvak1e6(3)*qshift(3)-sp&
      &vak1e6(4)*qshift(4))
      t7 = (spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-sp&
      &vae6k2(4)*qshift(4))
      brack = (abb59n32+t1*abb59n62+t10*abb59n82+t2*abb59n64+t3*abb59n65+t4*abb5&
      &9n66+t5*abb59n68+t8*abb59n52+t9*abb59n63+t1*t10*abb59n54+t1*t3*abb59n56+t&
      &1*t4*abb59n51+t1*t5*abb59n47+t1*t8*abb59n71+t1*abb59n44*(spvak1k6(1)*qshi&
      &ft(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshift(4))+&
      &t10*t2*abb59n38+t10*t5*abb59n37+t10*t6*abb59n43+t10*t7*abb59n48+t2*t3*abb&
      &59n39+t2*t4*abb59n34+t2*t6*abb59n40+t2*t7*abb59n46+t5*t8*abb59n94+t5*t9*a&
      &bb59n35+t6*t8*abb59n86+t6*t9*abb59n41+t7*t8*abb59n77+t7*t9*abb59n50+t1*t2&
      &*t3*abb59n98)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      t1 = e6(iv1)
      t2 = k1(iv1)
      t3 = k6(iv1)
      t4 = qshift(iv1)
      t5 = spvak1k2(iv1)
      t6 = spvak5k2(iv1)
      t7 = spvak5k4(iv1)
      t8 = spvak6k2(iv1)
      t18 = dotproduct(qshift, qshift)
      t9 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t19 = dotproduct(k1, qshift)
      t10 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-s&
      &pvak6k2(4)*qshift(4))
      t11 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-s&
      &pvak5k2(4)*qshift(4))
      t12 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t13 = (spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-s&
      &pvae6k2(4)*qshift(4))
      t14 = (spvak1e6(1)*qshift(1)-spvak1e6(2)*qshift(2)-spvak1e6(3)*qshift(3)-s&
      &pvak1e6(4)*qshift(4))
      t15 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t16 = spvae6k2(iv1)
      t20 = dotproduct(k6, qshift)
      t17 = spvak1e6(iv1)
      brack = (t1*abb59n61+t2*abb59n45+t3*abb59n60+t4*abb59n73+t5*abb59n58+t6*ab&
      &b59n55+t7*abb59n59+t8*abb59n57+spvak1k6(iv1)*t12*abb59n83+t1*t10*abb59n74&
      &+t1*t11*abb59n80+t1*t18*abb59n53+t1*t19*abb59n69+t1*t9*abb59n67+t1*abb59n&
      &83*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spv&
      &ak1k6(4)*qshift(4))+t10*t7*abb59n97+t11*t2*abb59n92+t11*t3*abb59n96+t11*t&
      &4*abb59n93+t12*t2*abb59n69+t12*t4*abb59n70+t12*t5*abb59n67+t12*t6*abb59n8&
      &0+t12*t8*abb59n74+t13*t2*abb59n79+t13*t3*abb59n75+t13*t4*abb59n76+t13*t7*&
      &abb59n81+t14*t2*abb59n84+t14*t3*abb59n88+t14*t4*abb59n85+t14*t7*abb59n89+&
      &t15*t16*abb59n81+t15*t17*abb59n89+t15*t2*abb59n91+t15*t5*abb59n90+t15*t8*&
      &abb59n97+t16*t18*abb59n49+t16*t19*abb59n79+t16*t20*abb59n75+t17*t18*abb59&
      &n42+t17*t19*abb59n84+t17*t20*abb59n88+t18*t6*abb59n36+t19*t6*abb59n92+t19&
      &*t7*abb59n91+t20*t6*abb59n96+t7*t9*abb59n90+t1*t15*t9*abb59n33+t12*t15*t5&
      &*abb59n33+t12*t7*t9*abb59n33)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t2 = e6(iv1)
      t3 = k1(iv2)
      t4 = qshift(iv2)
      t5 = spvak1k2(iv2)
      t6 = t2*t5
      t7 = spvak5k2(iv2)
      t8 = spvak6k2(iv2)
      t9 = e6(iv2)
      t10 = k1(iv1)
      t11 = qshift(iv1)
      t12 = spvak1k2(iv1)
      t13 = t12*t9
      t14 = spvak5k2(iv1)
      t15 = spvak6k2(iv1)
      t16 = spvae6k2(iv2)
      t17 = spvak1e6(iv2)
      t18 = spvak5k4(iv2)
      t19 = spvae6k2(iv1)
      t20 = spvak1e6(iv1)
      t21 = spvak5k4(iv1)
      t22 = k6(iv1)
      t23 = k6(iv2)
      t12 = t12*t18
      t5 = t21*t5
      t24 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t25 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      brack = ((2.0_ki)*(t26*abb59n52+t1*t26*abb59n71+t26*abb59n77*(spvae6k2(1)*&
      &qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-spvae6k2(4)*qshift(&
      &4))+t26*abb59n86*(spvak1e6(1)*qshift(1)-spvak1e6(2)*qshift(2)-spvak1e6(3)&
      &*qshift(3)-spvak1e6(4)*qshift(4))+t26*abb59n94*(spvak5k2(1)*qshift(1)-spv&
      &ak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-spvak5k2(4)*qshift(4)))+t12*abb5&
      &9n39+t13*abb59n56+t5*abb59n39+t6*abb59n56+spvak1k6(iv1)*t9*abb59n44+spvak&
      &1k6(iv2)*t2*abb59n44+t1*t12*abb59n98+t1*t5*abb59n98+t10*t16*abb59n48+t10*&
      &t17*abb59n43+t10*t18*abb59n38+t10*t7*abb59n37+t10*t9*abb59n54+t11*t16*abb&
      &59n78+t11*t17*abb59n87+t11*t7*abb59n95+t11*t9*abb59n72+t13*t24*abb59n98+t&
      &14*t23*abb59n35+t14*t3*abb59n37+t14*t4*abb59n95+t14*t9*abb59n47+t15*t18*a&
      &bb59n34+t15*t9*abb59n51+t16*t21*abb59n46+t16*t22*abb59n50+t17*t21*abb59n4&
      &0+t17*t22*abb59n41+t18*t19*abb59n46+t18*t20*abb59n40+t19*t23*abb59n50+t19&
      &*t3*abb59n48+t19*t4*abb59n78+t2*t3*abb59n54+t2*t4*abb59n72+t2*t7*abb59n47&
      &+t2*t8*abb59n51+t20*t23*abb59n41+t20*t3*abb59n43+t20*t4*abb59n87+t21*t3*a&
      &bb59n38+t21*t8*abb59n34+t22*t7*abb59n35+t24*t6*abb59n98+t18*t2*t25*abb59n&
      &98+t21*t25*t9*abb59n98)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      t4 = spvak1k2(iv2)
      t5 = spvak5k4(iv3)
      t6 = spvak1k2(iv3)
      t7 = spvak5k4(iv2)
      t8 = spvak1k2(iv1)
      t9 = spvak5k4(iv1)
      brack = ((2.0_ki)*(spvae6k2(iv1)*t10*abb59n49+spvae6k2(iv2)*t11*abb59n49+s&
      &pvae6k2(iv3)*t12*abb59n49+spvak1e6(iv1)*t10*abb59n42+spvak1e6(iv2)*t11*ab&
      &b59n42+spvak1e6(iv3)*t12*abb59n42+spvak5k2(iv1)*t10*abb59n36+spvak5k2(iv2&
      &)*t11*abb59n36+spvak5k2(iv3)*t12*abb59n36+t1*t10*abb59n53+t11*t2*abb59n53&
      &+t12*t3*abb59n53)+t1*t4*t5*abb59n33+t1*t6*t7*abb59n33+t2*t5*t8*abb59n33+t&
      &2*t6*t9*abb59n33+t3*t4*t9*abb59n33+t3*t7*t8*abb59n33)
   end  function brack_4
!---#] function brack_4:

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
end module p1_dbarc_hepneg_d59h0l1d
