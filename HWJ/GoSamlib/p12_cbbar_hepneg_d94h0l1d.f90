module     p12_cbbar_hepneg_d94h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p12_cbbar_hepneg/helicity0/d94h0l1d.f90
   ! generator: haggies (1.1)
   use p12_cbbar_hepneg_config, only: ki
   use p12_cbbar_hepneg_util, only: cond, d => metric_tensor
   

   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d94
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_cbbar_hepneg_model
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_color
      use p12_cbbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      real(ki) :: t3
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t3 = dotproduct(k1, qshift)
      t2 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      brack = (abb94n13+t1*abb94n17+t2*abb94n20+t3*abb94n18+abb94n15*dotproduct(&
      &qshift, qshift)+abb94n19*(spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spv&
      &ae6k1(3)*qshift(3)-spvae6k1(4)*qshift(4))+abb94n21*(spvak1k6(1)*qshift(1)&
      &-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshift(4))+abb94&
      &n22*dotproduct(k6, qshift)+t1*t2*abb94n14+t1*t3*abb94n16)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_cbbar_hepneg_model
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_color
      use p12_cbbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      t1 = e6(iv1)
      t2 = k1(iv1)
      t3 = spvak6k1(iv1)
      t4 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      brack = (k6(iv1)*abb94n22+qshift(iv1)*abb94n23+spvae6k1(iv1)*abb94n19+spva&
      &k1k6(iv1)*abb94n21+t1*abb94n17+t2*abb94n18+t3*abb94n20+t1*abb94n14*(spvak&
      &6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-spvak6k1(4)*&
      &qshift(4))+t1*abb94n16*dotproduct(k1, qshift)+t2*t4*abb94n16+t3*t4*abb94n&
      &14)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_cbbar_hepneg_model
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_color
      use p12_cbbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      t1 = e6(iv1)
      t2 = e6(iv2)
      brack = ((2.0_ki)*d(iv1,iv2)*abb94n15+k1(iv1)*t2*abb94n16+k1(iv2)*t1*abb94&
      &n16+spvak6k1(iv1)*t2*abb94n14+spvak6k1(iv2)*t1*abb94n14)
   end  function brack_3
!---#] function brack_3:

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p12_cbbar_hepneg_globalsl1, only: epspow
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_abbrevh0
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
      qshift = -k3-k6-k5-k4
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
!---#[ subroutine reconstruct_d94:
   subroutine     reconstruct_d94(coeffs)
      use p12_cbbar_hepneg_groups, only: tensrec_info_group3
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group3), intent(out) :: coeffs
      ! rank 2 case:
      !---[# reconstruct coeffs%coeffs_94:
      coeffs%coeffs_94%c0 = derivative(czip)
      coeffs%coeffs_94%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_94%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_94%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_94%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_94%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_94%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_94%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_94%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_94%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_94%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_94%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_94%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_94%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_94%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_94:
   end subroutine reconstruct_d94
!---#] subroutine reconstruct_d94:
end module p12_cbbar_hepneg_d94h0l1d
