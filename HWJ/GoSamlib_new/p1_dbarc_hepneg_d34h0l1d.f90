module     p1_dbarc_hepneg_d34h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p1_dbarc_hepneg/helicity0/d34h0l1d.f90
   ! generator: haggies (1.1)
   use p1_dbarc_hepneg_config, only: ki
   use p1_dbarc_hepneg_util, only: cond, d => metric_tensor
   

   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d34
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
      real(ki) :: t4
      real(ki) :: t5
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t2 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t3 = (spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qshift(3)-sp&
      &vae6k2(4)*qshift(4))
      t4 = dotproduct(k6, qshift)
      t5 = dotproduct(k2, qshift)
      brack = (abb34n17+t1*abb34n31+t2*abb34n34+t3*abb34n36+t4*abb34n37+t5*abb34&
      &n38+abb34n29*dotproduct(qshift, qshift)+abb34n33*dotproduct(k1, qshift)+a&
      &bb34n40*(spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3&
      &)-spvak1k4(4)*qshift(4))+t1*t2*abb34n26+t1*t3*abb34n22+t1*t4*abb34n19+t1*&
      &t5*abb34n20+t1*abb34n18*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spva&
      &k1k6(3)*qshift(3)-spvak1k6(4)*qshift(4)))
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
      t1 = e6(iv1)
      t2 = k2(iv1)
      t3 = k6(iv1)
      t4 = spvae6k2(iv1)
      t5 = spvak5k4(iv1)
      t6 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      brack = (k1(iv1)*abb34n28+qshift(iv1)*abb34n32+spvak1k4(iv1)*abb34n21+t1*a&
      &bb34n27+t2*abb34n23+t3*abb34n24+t4*abb34n25+t5*abb34n30+spvak1k6(iv1)*t6*&
      &abb34n43+t1*t6*abb34n35+t2*t6*abb34n41+t3*t6*abb34n42+t4*t6*abb34n39+t5*a&
      &bb34n35*(e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))&
      &+t5*abb34n39*(spvae6k2(1)*qshift(1)-spvae6k2(2)*qshift(2)-spvae6k2(3)*qsh&
      &ift(3)-spvae6k2(4)*qshift(4))+t5*abb34n41*dotproduct(k2, qshift)+t5*abb34&
      &n42*dotproduct(k6, qshift)+t5*abb34n43*(spvak1k6(1)*qshift(1)-spvak1k6(2)&
      &*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshift(4)))
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
      t1 = spvak5k4(iv2)
      t2 = spvak5k4(iv1)
      brack = ((2.0_ki)*d(iv1,iv2)*abb34n29+e6(iv1)*t1*abb34n26+e6(iv2)*t2*abb34&
      &n26+k2(iv1)*t1*abb34n20+k2(iv2)*t2*abb34n20+k6(iv1)*t1*abb34n19+k6(iv2)*t&
      &2*abb34n19+spvae6k2(iv1)*t1*abb34n22+spvae6k2(iv2)*t2*abb34n22+spvak1k6(i&
      &v1)*t1*abb34n18+spvak1k6(iv2)*t2*abb34n18)
   end  function brack_3
!---#] function brack_3:

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
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
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d34:
   subroutine     reconstruct_d34(coeffs)
      use p1_dbarc_hepneg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case:
      !---[# reconstruct coeffs%coeffs_34:
      coeffs%coeffs_34%c0 = derivative(czip)
      coeffs%coeffs_34%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_34%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_34%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_34%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_34%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_34%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_34%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_34%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_34%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_34%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_34%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_34%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_34%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_34%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_34:
   end subroutine reconstruct_d34
!---#] subroutine reconstruct_d34:
end module p1_dbarc_hepneg_d34h0l1d
