module     p1_dbarc_hepneg_d59h1l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p1_dbarc_hepneg/helicity1/d59h1l1d.f90
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
      use p1_dbarc_hepneg_abbrevh1
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
      t1 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t5 = dotproduct(qshift, qshift)
      t2 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t3 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t6 = dotproduct(k1, qshift)
      t7 = dotproduct(k6, qshift)
      t4 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      brack = (abb59n22+t1*abb59n45+t1*t1*abb59n33+t2*abb59n42+t3*abb59n47+t5*ab&
      &b59n40+t6*abb59n53+t1*t2*abb59n37+t1*t3*abb59n28+t1*t5*abb59n50+t1*t6*abb&
      &59n35+t1*t7*abb59n34+t1*abb59n26*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshif&
      &t(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshift(4))+t2*t4*abb59n39+t2*t5*ab&
      &b59n63+t2*t6*abb59n27+t2*t7*abb59n24+t3*t5*abb59n56+t3*t6*abb59n29+t3*t7*&
      &abb59n31+t1*t2*t4*abb59n66)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh1
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
      real(ki) :: t12
      real(ki) :: t13
      real(ki) :: t14
      t1 = k1(iv1)
      t2 = qshift(iv1)
      t3 = spvak1k2(iv1)
      t4 = spvak5k2(iv1)
      t5 = spvak6k2(iv1)
      t6 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t7 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t8 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t9 = k6(iv1)
      t12 = dotproduct(qshift, qshift)
      t10 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t13 = dotproduct(k1, qshift)
      t14 = dotproduct(k6, qshift)
      t11 = spvak5k4(iv1)
      brack = (t1*abb59n32+t2*abb59n43+t3*abb59n41+t4*abb59n36+t5*abb59n38+spvak&
      &1k6(iv1)*t6*abb59n61+t1*t6*abb59n48+t1*t7*abb59n58+t1*t8*abb59n60+t10*t3*&
      &abb59n44+t11*t8*abb59n44+t12*t3*abb59n25+t12*t4*abb59n30+t12*t5*abb59n34+&
      &t13*t3*abb59n60+t13*t4*abb59n58+t13*t5*abb59n48+t14*t3*abb59n65+t14*t4*ab&
      &b59n54+t14*t5*abb59n50+t2*t6*abb59n49+t2*t7*abb59n55+t2*t8*abb59n62+t3*t6&
      &*abb59n46+t4*t6*abb59n59+t5*t6*abb59n52+t5*t7*abb59n59+t5*t8*abb59n46+t5*&
      &abb59n61*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(&
      &3)-spvak1k6(4)*qshift(4))+t6*t9*abb59n50+t7*t9*abb59n54+t8*t9*abb59n65+t1&
      &0*t3*t6*abb59n23+t10*t5*t8*abb59n23+t11*t6*t8*abb59n23)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh1
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
      t1 = spvak6k2(iv1)
      t2 = spvak6k2(iv2)
      t3 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t4 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t5 = k1(iv1)
      t6 = spvak1k2(iv2)
      t7 = spvak5k2(iv2)
      t8 = k1(iv2)
      t9 = spvak1k2(iv1)
      t10 = spvak5k2(iv1)
      t11 = k6(iv1)
      t12 = k6(iv2)
      t13 = qshift(iv1)
      t14 = qshift(iv2)
      t15 = spvak5k4(iv2)
      t16 = t15*t9
      t17 = t2*t9
      t18 = spvak5k4(iv1)
      t19 = t18*t6
      t20 = t1*t6
      t21 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      brack = ((2.0_ki)*(t22*abb59n40+t1*t2*abb59n33+t22*t3*abb59n50+t22*t4*abb5&
      &9n63+t22*abb59n56*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3&
      &)*qshift(3)-spvak5k2(4)*qshift(4)))+t16*abb59n39+t17*abb59n37+t19*abb59n3&
      &9+t20*abb59n37+spvak1k6(iv1)*t2*abb59n26+spvak1k6(iv2)*t1*abb59n26+t1*t12&
      &*abb59n34+t1*t14*abb59n51+t1*t7*abb59n28+t1*t8*abb59n35+t10*t12*abb59n31+&
      &t10*t14*abb59n57+t10*t2*abb59n28+t10*t8*abb59n29+t11*t2*abb59n34+t11*t6*a&
      &bb59n24+t11*t7*abb59n31+t12*t9*abb59n24+t13*t2*abb59n51+t13*t6*abb59n64+t&
      &13*t7*abb59n57+t14*t9*abb59n64+t16*t3*abb59n66+t17*t21*abb59n66+t19*t3*ab&
      &b59n66+t2*t5*abb59n35+t20*t21*abb59n66+t5*t6*abb59n27+t5*t7*abb59n29+t8*t&
      &9*abb59n27+t1*t15*t4*abb59n66+t18*t2*t4*abb59n66)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q) result(brack)
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_abbrevh1
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
      t1 = spvak1k2(iv1)
      t10 = d(iv2,iv3)
      t2 = spvak1k2(iv2)
      t11 = d(iv1,iv3)
      t3 = spvak1k2(iv3)
      t12 = d(iv1,iv2)
      t4 = spvak6k2(iv1)
      t5 = spvak6k2(iv2)
      t6 = spvak6k2(iv3)
      t7 = spvak5k4(iv2)
      t8 = spvak5k4(iv3)
      t9 = spvak5k4(iv1)
      brack = ((2.0_ki)*(spvak5k2(iv1)*t10*abb59n30+spvak5k2(iv2)*t11*abb59n30+s&
      &pvak5k2(iv3)*t12*abb59n30+t1*t10*abb59n25+t10*t4*abb59n34+t11*t2*abb59n25&
      &+t11*t5*abb59n34+t12*t3*abb59n25+t12*t6*abb59n34)+t1*t5*t8*abb59n23+t1*t6&
      &*t7*abb59n23+t2*t4*t8*abb59n23+t2*t6*t9*abb59n23+t3*t4*t7*abb59n23+t3*t5*&
      &t9*abb59n23)
   end  function brack_4
!---#] function brack_4:

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh1
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
end module p1_dbarc_hepneg_d59h1l1d
