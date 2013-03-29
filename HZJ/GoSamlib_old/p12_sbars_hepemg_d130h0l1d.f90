module     p12_sbars_hepemg_d130h0l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p12_s &
   ! &bars_hepemg/helicity0d130h0l1d.f90
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
   public :: derivative , reconstruct_d130
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd130h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(28) :: acd130
      complex(ki) :: brack
      acd130(1)=dotproduct(k1,qshift)
      acd130(2)=abb130(8)
      acd130(3)=dotproduct(k6,qshift)
      acd130(4)=abb130(6)
      acd130(5)=dotproduct(qshift,qshift)
      acd130(6)=abb130(14)
      acd130(7)=dotproduct(qshift,spvak1k2)
      acd130(8)=dotproduct(qshift,spvak5k4)
      acd130(9)=abb130(7)
      acd130(10)=abb130(9)
      acd130(11)=dotproduct(qshift,spvak6k2)
      acd130(12)=abb130(17)
      acd130(13)=dotproduct(qshift,spvak1k6)
      acd130(14)=abb130(12)
      acd130(15)=dotproduct(qshift,spvak5k2)
      acd130(16)=abb130(5)
      acd130(17)=abb130(15)
      acd130(18)=dotproduct(qshift,spvak6k1)
      acd130(19)=abb130(16)
      acd130(20)=abb130(10)
      acd130(21)=acd130(8)*acd130(12)
      acd130(21)=acd130(21)-acd130(17)
      acd130(21)=acd130(11)*acd130(21)
      acd130(22)=-acd130(18)*acd130(19)
      acd130(23)=-acd130(15)*acd130(16)
      acd130(24)=-acd130(13)*acd130(14)
      acd130(25)=acd130(5)*acd130(6)
      acd130(26)=-acd130(3)*acd130(4)
      acd130(27)=-acd130(1)*acd130(2)
      acd130(28)=acd130(8)*acd130(9)
      acd130(28)=-acd130(10)+acd130(28)
      acd130(28)=acd130(7)*acd130(28)
      brack=acd130(20)+acd130(21)+acd130(22)+acd130(23)+acd130(24)+acd130(25)+a&
      &cd130(26)+acd130(27)+acd130(28)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd130h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(31) :: acd130
      complex(ki) :: brack
      acd130(1)=k1(iv1)
      acd130(2)=abb130(8)
      acd130(3)=k6(iv1)
      acd130(4)=abb130(6)
      acd130(5)=qshift(iv1)
      acd130(6)=abb130(14)
      acd130(7)=spvak1k2(iv1)
      acd130(8)=dotproduct(qshift,spvak5k4)
      acd130(9)=abb130(7)
      acd130(10)=abb130(9)
      acd130(11)=spvak5k4(iv1)
      acd130(12)=dotproduct(qshift,spvak1k2)
      acd130(13)=dotproduct(qshift,spvak6k2)
      acd130(14)=abb130(17)
      acd130(15)=spvak1k6(iv1)
      acd130(16)=abb130(12)
      acd130(17)=spvak5k2(iv1)
      acd130(18)=abb130(5)
      acd130(19)=spvak6k2(iv1)
      acd130(20)=abb130(15)
      acd130(21)=spvak6k1(iv1)
      acd130(22)=abb130(16)
      acd130(23)=acd130(12)*acd130(9)
      acd130(24)=acd130(13)*acd130(14)
      acd130(23)=acd130(24)+acd130(23)
      acd130(23)=acd130(11)*acd130(23)
      acd130(24)=acd130(9)*acd130(8)
      acd130(24)=-acd130(10)+acd130(24)
      acd130(24)=acd130(7)*acd130(24)
      acd130(25)=acd130(14)*acd130(8)
      acd130(25)=-acd130(20)+acd130(25)
      acd130(25)=acd130(19)*acd130(25)
      acd130(26)=-acd130(2)*acd130(1)
      acd130(27)=-acd130(4)*acd130(3)
      acd130(28)=acd130(6)*acd130(5)
      acd130(29)=-acd130(16)*acd130(15)
      acd130(30)=-acd130(18)*acd130(17)
      acd130(31)=-acd130(22)*acd130(21)
      brack=acd130(23)+acd130(24)+acd130(25)+acd130(26)+acd130(27)+2.0_ki*acd13&
      &0(28)+acd130(29)+acd130(30)+acd130(31)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd130h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(13) :: acd130
      complex(ki) :: brack
      acd130(1)=d(iv1,iv2)
      acd130(2)=abb130(14)
      acd130(3)=spvak1k2(iv1)
      acd130(4)=spvak5k4(iv2)
      acd130(5)=abb130(7)
      acd130(6)=spvak1k2(iv2)
      acd130(7)=spvak5k4(iv1)
      acd130(8)=spvak6k2(iv2)
      acd130(9)=abb130(17)
      acd130(10)=spvak6k2(iv1)
      acd130(11)=acd130(3)*acd130(4)
      acd130(12)=acd130(6)*acd130(7)
      acd130(11)=acd130(12)+acd130(11)
      acd130(11)=acd130(5)*acd130(11)
      acd130(12)=acd130(8)*acd130(7)
      acd130(13)=acd130(10)*acd130(4)
      acd130(12)=acd130(13)+acd130(12)
      acd130(12)=acd130(9)*acd130(12)
      acd130(13)=acd130(2)*acd130(1)
      brack=acd130(11)+acd130(12)+2.0_ki*acd130(13)
   end function brack_3
!---#] function brack_3:
!---#[ function derivative:
   function derivative(mu2,i1,i2) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd130h0
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
      qshift = -k2
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
!---#[ subroutine reconstruct_d130:
   subroutine     reconstruct_d130(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case :
      !---[# reconstruct coeffs%coeffs_130:
      coeffs%coeffs_130%c0 = derivative(czip)
      coeffs%coeffs_130%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_130%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_130%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_130%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_130%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_130%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_130%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_130%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_130%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_130%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_130%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_130%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_130%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_130%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_130:
   end subroutine reconstruct_d130
!---#] subroutine reconstruct_d130:
end module     p12_sbars_hepemg_d130h0l1d
