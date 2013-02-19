module     p6_ubbar_hepneg_d92h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p6_ubbar_hepneg/helicity0/d92h0l1d.f90
   ! generator: haggies (1.1)
   use p6_ubbar_hepneg_config, only: ki
   use p6_ubbar_hepneg_util, only: cond, d => metric_tensor
   

   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d92
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      brack = abb92n16
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      brack = (e6(iv1)*abb92n26+k2(iv1)*abb92n20+k6(iv1)*abb92n22+spvak2e6(iv1)*&
      &abb92n25+spvak5k1(iv1)*abb92n27+spvak5k4(iv1)*abb92n28)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      t1 = e6(iv1)
      t2 = e6(iv2)
      brack = ((2.0_ki)*d(iv1,iv2)*abb92n21+k2(iv1)*t2*abb92n24+k2(iv2)*t1*abb92&
      &n24+k6(iv1)*t2*abb92n23+k6(iv2)*t1*abb92n23+spvak2k6(iv1)*t2*abb92n19+spv&
      &ak2k6(iv2)*t1*abb92n19+spvak5k1(iv1)*t2*abb92n17+spvak5k1(iv2)*t1*abb92n1&
      &7+spvak5k4(iv1)*t2*abb92n18+spvak5k4(iv2)*t1*abb92n18)
   end  function brack_3
!---#] function brack_3:

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p6_ubbar_hepneg_globalsl1, only: epspow
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_abbrevh0
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
      qshift(:) = 0.0_ki
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
!---#[ subroutine reconstruct_d92:
   subroutine     reconstruct_d92(coeffs)
      use p6_ubbar_hepneg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case:
      !---[# reconstruct coeffs%coeffs_92:
      coeffs%coeffs_92%c0 = derivative(czip)
      coeffs%coeffs_92%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_92%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_92%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_92%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_92%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_92%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_92%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_92%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_92%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_92%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_92%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_92%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_92%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_92%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_92:
   end subroutine reconstruct_d92
!---#] subroutine reconstruct_d92:
end module p6_ubbar_hepneg_d92h0l1d
