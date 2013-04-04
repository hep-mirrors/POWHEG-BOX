module     p12_sbars_hepemg_d266h2l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity2d266h2l1d.f90
   ! generator: buildfortran_d.py
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d266
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd266h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd266
      complex(ki) :: brack
      acd266(1)=abb266(13)
      brack=acd266(1)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd266h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(18) :: acd266
      complex(ki) :: brack
      acd266(1)=k2(iv1)
      acd266(2)=abb266(11)
      acd266(3)=k6(iv1)
      acd266(4)=abb266(14)
      acd266(5)=e6(iv1)
      acd266(6)=abb266(12)
      acd266(7)=spvak2k6(iv1)
      acd266(8)=abb266(5)
      acd266(9)=spvak6k2(iv1)
      acd266(10)=abb266(9)
      acd266(11)=spvae6k2(iv1)
      acd266(12)=abb266(18)
      acd266(13)=acd266(11)*acd266(12)
      acd266(14)=acd266(9)*acd266(10)
      acd266(15)=acd266(7)*acd266(8)
      acd266(16)=acd266(5)*acd266(6)
      acd266(17)=acd266(3)*acd266(4)
      acd266(18)=acd266(1)*acd266(2)
      brack=acd266(13)+acd266(14)+acd266(15)+acd266(16)+acd266(17)+acd266(18)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd266h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(13) :: acd266
      complex(ki) :: brack
      acd266(1)=d(iv1,iv2)
      acd266(2)=abb266(17)
      acd266(3)=k2(iv1)
      acd266(4)=e6(iv2)
      acd266(5)=abb266(16)
      acd266(6)=k2(iv2)
      acd266(7)=e6(iv1)
      acd266(8)=spvak6k2(iv2)
      acd266(9)=abb266(6)
      acd266(10)=spvak6k2(iv1)
      acd266(11)=acd266(9)*acd266(8)
      acd266(12)=acd266(5)*acd266(6)
      acd266(11)=acd266(12)+acd266(11)
      acd266(11)=acd266(7)*acd266(11)
      acd266(12)=acd266(9)*acd266(10)
      acd266(13)=acd266(5)*acd266(3)
      acd266(12)=acd266(12)+acd266(13)
      acd266(12)=acd266(4)*acd266(12)
      acd266(13)=acd266(1)*acd266(2)
      brack=acd266(11)+acd266(12)+2.0_ki*acd266(13)
   end function brack_3
!---#] function brack_3:
!---#[ function derivative:
   function derivative(mu2,i1,i2) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd266h2
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/ (0.0_ki,0.0_ki),(0.0_ki,0.&
      &0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = 0
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
          iv1=i1
          deg=1
      else
          iv1=1
      end if
      if(present(i2)) then
          iv2=i2
          deg=2
      else
          iv2=1
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
      if(deg.eq.2) then
         numerator = cond(epspow.eq.t1,brack_3,Q,mu2)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d266:
   subroutine     reconstruct_d266(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 2 case :
      !---[# reconstruct coeffs%coeffs_266:
      coeffs%coeffs_266%c0 = derivative(czip)
      coeffs%coeffs_266%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_266%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_266%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_266%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_266%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_266%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_266%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_266%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_266%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_266%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_266%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_266%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_266%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_266%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_266:
   end subroutine reconstruct_d266
!---#] subroutine reconstruct_d266:
end module     p12_sbars_hepemg_d266h2l1d
