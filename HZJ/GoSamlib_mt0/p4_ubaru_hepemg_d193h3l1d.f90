module     p4_ubaru_hepemg_d193h3l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p4_ubaru_hepemg/helicity3d193h3l1d.f90
   ! generator: buildfortran_d.py
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d193
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd193h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(19) :: acd193
      complex(ki) :: brack
      acd193(1)=dotproduct(k1,qshift)
      acd193(2)=abb193(15)
      acd193(3)=dotproduct(k6,qshift)
      acd193(4)=abb193(12)
      acd193(5)=dotproduct(qshift,spvak1k6)
      acd193(6)=abb193(9)
      acd193(7)=dotproduct(qshift,spvak2k5)
      acd193(8)=abb193(6)
      acd193(9)=dotproduct(qshift,spvak4k5)
      acd193(10)=abb193(8)
      acd193(11)=dotproduct(qshift,spvak6k1)
      acd193(12)=abb193(5)
      acd193(13)=abb193(7)
      acd193(14)=-acd193(2)*acd193(1)
      acd193(15)=-acd193(4)*acd193(3)
      acd193(16)=-acd193(6)*acd193(5)
      acd193(17)=-acd193(8)*acd193(7)
      acd193(18)=-acd193(10)*acd193(9)
      acd193(19)=-acd193(12)*acd193(11)
      brack=acd193(13)+acd193(14)+acd193(15)+acd193(16)+acd193(17)+acd193(18)+a&
      &cd193(19)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd193h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(18) :: acd193
      complex(ki) :: brack
      acd193(1)=k1(iv1)
      acd193(2)=abb193(15)
      acd193(3)=k6(iv1)
      acd193(4)=abb193(12)
      acd193(5)=spvak1k6(iv1)
      acd193(6)=abb193(9)
      acd193(7)=spvak2k5(iv1)
      acd193(8)=abb193(6)
      acd193(9)=spvak4k5(iv1)
      acd193(10)=abb193(8)
      acd193(11)=spvak6k1(iv1)
      acd193(12)=abb193(5)
      acd193(13)=-acd193(2)*acd193(1)
      acd193(14)=-acd193(4)*acd193(3)
      acd193(15)=-acd193(6)*acd193(5)
      acd193(16)=-acd193(8)*acd193(7)
      acd193(17)=-acd193(10)*acd193(9)
      acd193(18)=-acd193(12)*acd193(11)
      brack=acd193(13)+acd193(14)+acd193(15)+acd193(16)+acd193(17)+acd193(18)
   end function brack_2
!---#] function brack_2:
!---#[ function derivative:
   function derivative(mu2,i1) result(numerator)
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd193h3
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
!---#[ subroutine reconstruct_d193:
   subroutine     reconstruct_d193(coeffs)
      use p4_ubaru_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 1 case :
      !---[# reconstruct coeffs%coeffs_193:
      coeffs%coeffs_193%c0 = derivative(czip)
      coeffs%coeffs_193%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_193%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_193%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_193%c1(4,1) = -derivative(czip,4)
      !---#] reconstruct coeffs%coeffs_193:
   end subroutine reconstruct_d193
!---#] subroutine reconstruct_d193:
end module     p4_ubaru_hepemg_d193h3l1d
