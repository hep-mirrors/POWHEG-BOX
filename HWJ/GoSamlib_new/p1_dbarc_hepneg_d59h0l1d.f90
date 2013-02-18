module     p1_dbarc_hepneg_d59h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p1_dbarc_hepneg/helicity0/d59h0l1d.f90
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
   pure function brack_1(Q) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t2 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t3 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t7 = dotproduct(k1, qshift)
      t8 = dotproduct(k6, qshift)
      t4 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t5 = (spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-sp&
      &vak1k6(4)*qshift(4))
      t9 = dotproduct(k2, qshift)
      brack = (abb59n29+t1*abb59n56+t2*abb59n58+t3*abb59n59+t4*abb59n64+t6*abb59&
      &n49+t7*abb59n61+t8*abb59n63+t1*t3*abb59n51+t1*t5*abb59n54+t1*t7*abb59n46+&
      &t2*t4*abb59n35+t2*t6*abb59n68+t2*t7*abb59n45+t2*t8*abb59n43+t3*t4*abb59n4&
      &2+t4*t5*abb59n31+t4*t6*abb59n74+t4*t7*abb59n41+t4*abb59n37*(spvak6k2(1)*q&
      &shift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-spvak6k2(4)*qshift(4&
      &))+t5*t6*abb59n85+t5*t7*abb59n32+t5*t8*abb59n34+t6*t9*abb59n78+t7*t9*abb5&
      &9n36+t8*t9*abb59n39+t1*t3*t4*abb59n89)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      real(ki) :: t15
      real(ki) :: t16
      real(ki) :: t17
      real(ki) :: t18
      t1 = k1(iv1)
      t2 = k6(iv1)
      t3 = qshift(iv1)
      t4 = spvak1k2(iv1)
      t5 = spvak2k6(iv1)
      t6 = spvak5k2(iv1)
      t7 = spvak5k4(iv1)
      t8 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t9 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t10 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-s&
      &pvak2k6(4)*qshift(4))
      t15 = dotproduct(k2, qshift)
      t11 = (spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-s&
      &pvak1k6(4)*qshift(4))
      t12 = k2(iv1)
      t16 = dotproduct(qshift, qshift)
      t17 = dotproduct(k6, qshift)
      t18 = dotproduct(k1, qshift)
      t13 = spvak1k6(iv1)
      t14 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      brack = (t1*abb59n50+t2*abb59n48+t3*abb59n62+t4*abb59n52+t5*abb59n47+t6*ab&
      &b59n53+t7*abb59n55+spvak6k2(iv1)*t10*abb59n80+t1*t10*abb59n72+t1*t11*abb5&
      &9n87+t1*t15*abb59n81+t1*t8*abb59n65+t1*t9*abb59n66+t10*t13*abb59n88+t10*t&
      &3*abb59n73+t10*t4*abb59n71+t10*t6*abb59n82+t11*t2*abb59n83+t11*t3*abb59n8&
      &4+t11*t5*abb59n88+t11*t7*abb59n57+t12*t16*abb59n38+t12*t17*abb59n76+t12*t&
      &18*abb59n81+t13*t16*abb59n33+t13*t17*abb59n83+t13*t18*abb59n87+t13*t8*abb&
      &59n57+t14*t5*abb59n71+t14*t7*abb59n60+t15*t2*abb59n76+t15*t3*abb59n77+t16&
      &*t5*abb59n40+t16*t6*abb59n44+t17*t6*abb59n70+t18*t5*abb59n72+t18*t6*abb59&
      &n66+t18*t7*abb59n65+t2*t9*abb59n70+t3*t9*abb59n67+t4*t8*abb59n60+t5*t9*ab&
      &b59n82+t5*abb59n80*(spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(&
      &3)*qshift(3)-spvak6k2(4)*qshift(4))+t10*t14*t7*abb59n30+t10*t4*t8*abb59n3&
      &0+t14*t5*t8*abb59n30)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      real(ki) :: t24
      t24 = d(iv1,iv2)
      t1 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t2 = k1(iv1)
      t3 = k2(iv2)
      t4 = spvak1k6(iv2)
      t5 = spvak2k6(iv2)
      t6 = spvak5k2(iv2)
      t7 = spvak5k4(iv2)
      t8 = k1(iv2)
      t9 = k2(iv1)
      t10 = spvak1k6(iv1)
      t11 = spvak2k6(iv1)
      t12 = spvak5k2(iv1)
      t13 = spvak5k4(iv1)
      t14 = k6(iv2)
      t15 = qshift(iv2)
      t16 = k6(iv1)
      t17 = qshift(iv1)
      t18 = spvak1k2(iv1)
      t19 = t18*t5
      t18 = t18*t7
      t20 = spvak1k2(iv2)
      t21 = t11*t20
      t20 = t13*t20
      t22 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t23 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-s&
      &pvak1k2(4)*qshift(4))
      brack = ((2.0_ki)*(t24*abb59n49+t1*t24*abb59n74+t24*abb59n68*(spvak5k2(1)*&
      &qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-spvak5k2(4)*qshift(&
      &4))+t24*abb59n78*dotproduct(k2, qshift)+t24*abb59n85*(spvak1k6(1)*qshift(&
      &1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshift(4)))+t1&
      &8*abb59n51+t19*abb59n42+t20*abb59n51+t21*abb59n42+spvak6k2(iv1)*t5*abb59n&
      &37+spvak6k2(iv2)*t11*abb59n37+t1*t18*abb59n89+t1*t20*abb59n89+t10*t14*abb&
      &59n34+t10*t15*abb59n86+t10*t5*abb59n31+t10*t7*abb59n54+t10*t8*abb59n32+t1&
      &1*t15*abb59n75+t11*t4*abb59n31+t11*t6*abb59n35+t11*t8*abb59n41+t12*t14*ab&
      &b59n43+t12*t15*abb59n69+t12*t5*abb59n35+t12*t8*abb59n45+t13*t4*abb59n54+t&
      &13*t8*abb59n46+t14*t9*abb59n39+t15*t9*abb59n79+t16*t3*abb59n39+t16*t4*abb&
      &59n34+t16*t6*abb59n43+t17*t3*abb59n79+t17*t4*abb59n86+t17*t5*abb59n75+t17&
      &*t6*abb59n69+t19*t22*abb59n89+t2*t3*abb59n36+t2*t4*abb59n32+t2*t5*abb59n4&
      &1+t2*t6*abb59n45+t2*t7*abb59n46+t21*t22*abb59n89+t8*t9*abb59n36+t11*t23*t&
      &7*abb59n89+t13*t23*t5*abb59n89)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh0
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
      t1 = spvak2k6(iv1)
      t2 = spvak2k6(iv2)
      t3 = spvak2k6(iv3)
      t4 = spvak1k2(iv1)
      t5 = spvak5k4(iv3)
      t6 = spvak5k4(iv2)
      t7 = spvak1k2(iv2)
      t8 = spvak5k4(iv1)
      t9 = spvak1k2(iv3)
      brack = ((2.0_ki)*(k2(iv1)*t10*abb59n38+k2(iv2)*t11*abb59n38+k2(iv3)*t12*a&
      &bb59n38+spvak1k6(iv1)*t10*abb59n33+spvak1k6(iv2)*t11*abb59n33+spvak1k6(iv&
      &3)*t12*abb59n33+spvak5k2(iv1)*t10*abb59n44+spvak5k2(iv2)*t11*abb59n44+spv&
      &ak5k2(iv3)*t12*abb59n44+t1*t10*abb59n40+t11*t2*abb59n40+t12*t3*abb59n40)+&
      &t1*t5*t7*abb59n30+t1*t6*t9*abb59n30+t2*t4*t5*abb59n30+t2*t8*t9*abb59n30+t&
      &3*t4*t6*abb59n30+t3*t7*t8*abb59n30)
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
