module     p12_sbars_hepemg_d170h2l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p12_s &
   ! &bars_hepemg/helicity2d170h2l1d.f90
   ! generator: buildfortran_d.py
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d170
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd170h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(16) :: acd170
      complex(ki) :: brack
      acd170(1)=dotproduct(k2,qshift)
      acd170(2)=abb170(8)
      acd170(3)=dotproduct(k6,qshift)
      acd170(4)=abb170(9)
      acd170(5)=dotproduct(e6,qshift)
      acd170(6)=abb170(6)
      acd170(7)=dotproduct(qshift,spvak2k6)
      acd170(8)=abb170(5)
      acd170(9)=dotproduct(qshift,spvae6k2)
      acd170(10)=abb170(7)
      acd170(11)=abb170(10)
      acd170(12)=-acd170(9)*acd170(10)
      acd170(13)=-acd170(7)*acd170(8)
      acd170(14)=-acd170(5)*acd170(6)
      acd170(15)=-acd170(3)*acd170(4)
      acd170(16)=-acd170(1)*acd170(2)
      brack=acd170(11)+acd170(12)+acd170(13)+acd170(14)+acd170(15)+acd170(16)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd170h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(15) :: acd170
      complex(ki) :: brack
      acd170(1)=k2(iv1)
      acd170(2)=abb170(8)
      acd170(3)=k6(iv1)
      acd170(4)=abb170(9)
      acd170(5)=e6(iv1)
      acd170(6)=abb170(6)
      acd170(7)=spvak2k6(iv1)
      acd170(8)=abb170(5)
      acd170(9)=spvae6k2(iv1)
      acd170(10)=abb170(7)
      acd170(11)=acd170(9)*acd170(10)
      acd170(12)=acd170(7)*acd170(8)
      acd170(13)=acd170(5)*acd170(6)
      acd170(14)=acd170(3)*acd170(4)
      acd170(15)=acd170(1)*acd170(2)
      brack=acd170(11)+acd170(12)+acd170(13)+acd170(14)+acd170(15)
   end function brack_2
!---#] function brack_2:
!---#[ function derivative:
   function derivative(mu2,i1) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd170h2
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/ (0.0_ki,0.0_ki),(0.0_ki,0.&
      &0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = k6
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
!---#[ subroutine reconstruct_d170:
   subroutine     reconstruct_d170(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 1 case :
      !---[# reconstruct coeffs%coeffs_170:
      coeffs%coeffs_170%c0 = derivative(czip)
      coeffs%coeffs_170%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_170%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_170%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_170%c1(4,1) = -derivative(czip,4)
      !---#] reconstruct coeffs%coeffs_170:
   end subroutine reconstruct_d170
!---#] subroutine reconstruct_d170:
end module     p12_sbars_hepemg_d170h2l1d
