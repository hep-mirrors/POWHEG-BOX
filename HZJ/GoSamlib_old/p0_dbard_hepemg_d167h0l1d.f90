module     p0_dbard_hepemg_d167h0l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p0_db &
   ! &ard_hepemg/helicity0d167h0l1d.f90
   ! generator: buildfortran_d.py
   use p0_dbard_hepemg_config, only: ki
   use p0_dbard_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d167
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd167h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(19) :: acd167
      complex(ki) :: brack
      acd167(1)=dotproduct(k1,qshift)
      acd167(2)=abb167(15)
      acd167(3)=dotproduct(k6,qshift)
      acd167(4)=abb167(12)
      acd167(5)=dotproduct(qshift,spvak1k6)
      acd167(6)=abb167(5)
      acd167(7)=dotproduct(qshift,spvak5k2)
      acd167(8)=abb167(6)
      acd167(9)=dotproduct(qshift,spvak5k4)
      acd167(10)=abb167(8)
      acd167(11)=dotproduct(qshift,spvak6k1)
      acd167(12)=abb167(9)
      acd167(13)=abb167(7)
      acd167(14)=-acd167(11)*acd167(12)
      acd167(15)=-acd167(9)*acd167(10)
      acd167(16)=-acd167(7)*acd167(8)
      acd167(17)=-acd167(5)*acd167(6)
      acd167(18)=-acd167(3)*acd167(4)
      acd167(19)=-acd167(1)*acd167(2)
      brack=acd167(13)+acd167(14)+acd167(15)+acd167(16)+acd167(17)+acd167(18)+a&
      &cd167(19)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd167h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(18) :: acd167
      complex(ki) :: brack
      acd167(1)=k1(iv1)
      acd167(2)=abb167(15)
      acd167(3)=k6(iv1)
      acd167(4)=abb167(12)
      acd167(5)=spvak1k6(iv1)
      acd167(6)=abb167(5)
      acd167(7)=spvak5k2(iv1)
      acd167(8)=abb167(6)
      acd167(9)=spvak5k4(iv1)
      acd167(10)=abb167(8)
      acd167(11)=spvak6k1(iv1)
      acd167(12)=abb167(9)
      acd167(13)=-acd167(11)*acd167(12)
      acd167(14)=-acd167(9)*acd167(10)
      acd167(15)=-acd167(7)*acd167(8)
      acd167(16)=-acd167(5)*acd167(6)
      acd167(17)=-acd167(3)*acd167(4)
      acd167(18)=-acd167(1)*acd167(2)
      brack=acd167(13)+acd167(14)+acd167(15)+acd167(16)+acd167(17)+acd167(18)
   end function brack_2
!---#] function brack_2:
!---#[ function derivative:
   function derivative(mu2,i1) result(numerator)
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd167h0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/ (0.0_ki,0.0_ki),(0.0_ki,0.&
      &0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = -k3-k5-k4
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
          iv1=i1
          deg=1
      else
          iv1=1
      end if
      t1 = 0
      if(deg.eq.0) then
         numerator = cond(epspow.eq.t1,brack_1,Q,mu2)
         return
      end if
      if(deg.eq.1) then
         numerator = cond(epspow.eq.t1,brack_2,Q,mu2)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d167:
   subroutine     reconstruct_d167(coeffs)
      use p0_dbard_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 1 case :
      !---[# reconstruct coeffs%coeffs_167:
      coeffs%coeffs_167%c0 = derivative(czip)
      coeffs%coeffs_167%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_167%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_167%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_167%c1(4,1) = -derivative(czip,4)
      !---#] reconstruct coeffs%coeffs_167:
   end subroutine reconstruct_d167
!---#] subroutine reconstruct_d167:
end module     p0_dbard_hepemg_d167h0l1d