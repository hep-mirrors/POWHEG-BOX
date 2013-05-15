module     p16_bbarb_hepemg_d196h3l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p16_bbarb_hepemg/helicity3d196h3l1d.f90
   ! generator: buildfortran_d.py
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d196
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p16_bbarb_hepemg_model
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_color
      use p16_bbarb_hepemg_abbrevd196h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(16) :: acd196
      complex(ki) :: brack
      acd196(1)=dotproduct(k2,qshift)
      acd196(2)=abb196(8)
      acd196(3)=dotproduct(k6,qshift)
      acd196(4)=abb196(9)
      acd196(5)=dotproduct(e6,qshift)
      acd196(6)=abb196(6)
      acd196(7)=dotproduct(qshift,spvak6k2)
      acd196(8)=abb196(5)
      acd196(9)=dotproduct(qshift,spvak2e6)
      acd196(10)=abb196(7)
      acd196(11)=abb196(10)
      acd196(12)=-acd196(2)*acd196(1)
      acd196(13)=-acd196(4)*acd196(3)
      acd196(14)=-acd196(6)*acd196(5)
      acd196(15)=-acd196(8)*acd196(7)
      acd196(16)=-acd196(10)*acd196(9)
      brack=acd196(11)+acd196(12)+acd196(13)+acd196(14)+acd196(15)+acd196(16)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p16_bbarb_hepemg_model
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_color
      use p16_bbarb_hepemg_abbrevd196h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(15) :: acd196
      complex(ki) :: brack
      acd196(1)=k2(iv1)
      acd196(2)=abb196(8)
      acd196(3)=k6(iv1)
      acd196(4)=abb196(9)
      acd196(5)=e6(iv1)
      acd196(6)=abb196(6)
      acd196(7)=spvak6k2(iv1)
      acd196(8)=abb196(5)
      acd196(9)=spvak2e6(iv1)
      acd196(10)=abb196(7)
      acd196(11)=acd196(9)*acd196(10)
      acd196(12)=acd196(7)*acd196(8)
      acd196(13)=acd196(5)*acd196(6)
      acd196(14)=acd196(3)*acd196(4)
      acd196(15)=acd196(1)*acd196(2)
      brack=acd196(11)+acd196(12)+acd196(13)+acd196(14)+acd196(15)
   end function brack_2
!---#] function brack_2:
!---#[ function derivative:
   function derivative(mu2,i1) result(numerator)
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd196h3
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
!---#[ subroutine reconstruct_d196:
   subroutine     reconstruct_d196(coeffs)
      use p16_bbarb_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 1 case :
      !---[# reconstruct coeffs%coeffs_196:
      coeffs%coeffs_196%c0 = derivative(czip)
      coeffs%coeffs_196%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_196%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_196%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_196%c1(4,1) = -derivative(czip,4)
      !---#] reconstruct coeffs%coeffs_196:
   end subroutine reconstruct_d196
!---#] subroutine reconstruct_d196:
end module     p16_bbarb_hepemg_d196h3l1d
