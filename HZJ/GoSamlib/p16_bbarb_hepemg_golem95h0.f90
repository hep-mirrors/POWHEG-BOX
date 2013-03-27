module     p16_bbarb_hepemg_golem95h0
   use precision_golem, only: ki_gol => ki
   use p16_bbarb_hepemg_config, only: ki
   implicit none
   private
   interface reconstruct_group
      module procedure reconstruct_group0
      module procedure reconstruct_group1
      module procedure reconstruct_group2
   end interface

   public :: reconstruct_group
contains
!---#[ subroutine reconstruct_group0:
subroutine     reconstruct_group0(coeffs)
   use tens_rec
   use p16_bbarb_hepemg_config
   use p16_bbarb_hepemg_groups, only: tensrec_info_group0
   use p16_bbarb_hepemg_d206h0l1, only: numerator_d206 => numerator_golem95
   use p16_bbarb_hepemg_d206h0l1d, only: reconstruct_d206
   implicit none
   type(tensrec_info_group0), intent(out) :: coeffs
   !------#[ Diagram 206:
      if (tens_rec_by_derivatives) then
         call reconstruct_d206(coeffs)
      else
         call reconstruct3(numerator_d206, coeffs%coeffs_206)
      end if
   !------#] Diagram 206:
end subroutine reconstruct_group0
!---#] subroutine reconstruct_group0:
!---#[ subroutine reconstruct_group1:
subroutine     reconstruct_group1(coeffs)
   use tens_rec
   use p16_bbarb_hepemg_config
   use p16_bbarb_hepemg_groups, only: tensrec_info_group1
   use p16_bbarb_hepemg_d117h0l1, only: numerator_d117 => numerator_golem95
   use p16_bbarb_hepemg_d117h0l1d, only: reconstruct_d117
   use p16_bbarb_hepemg_d134h0l1, only: numerator_d134 => numerator_golem95
   use p16_bbarb_hepemg_d134h0l1d, only: reconstruct_d134
   use p16_bbarb_hepemg_d170h0l1, only: numerator_d170 => numerator_golem95
   use p16_bbarb_hepemg_d170h0l1d, only: reconstruct_d170
   use p16_bbarb_hepemg_d205h0l1, only: numerator_d205 => numerator_golem95
   use p16_bbarb_hepemg_d205h0l1d, only: reconstruct_d205
   use p16_bbarb_hepemg_d233h0l1, only: numerator_d233 => numerator_golem95
   use p16_bbarb_hepemg_d233h0l1d, only: reconstruct_d233
   implicit none
   type(tensrec_info_group1), intent(out) :: coeffs
   !------#[ Diagram 117:
      if (tens_rec_by_derivatives) then
         call reconstruct_d117(coeffs)
      else
         call reconstruct3(numerator_d117, coeffs%coeffs_117)
      end if
   !------#] Diagram 117:
   !------#[ Diagram 134:
      if (tens_rec_by_derivatives) then
         call reconstruct_d134(coeffs)
      else
         call reconstruct2(numerator_d134, coeffs%coeffs_134)
      end if
   !------#] Diagram 134:
   !------#[ Diagram 170:
      if (tens_rec_by_derivatives) then
         call reconstruct_d170(coeffs)
      else
         call reconstruct1(numerator_d170, coeffs%coeffs_170)
      end if
   !------#] Diagram 170:
   !------#[ Diagram 205:
      if (tens_rec_by_derivatives) then
         call reconstruct_d205(coeffs)
      else
         call reconstruct3(numerator_d205, coeffs%coeffs_205)
      end if
   !------#] Diagram 205:
   !------#[ Diagram 233:
      if (tens_rec_by_derivatives) then
         call reconstruct_d233(coeffs)
      else
         call reconstruct2(numerator_d233, coeffs%coeffs_233)
      end if
   !------#] Diagram 233:
end subroutine reconstruct_group1
!---#] subroutine reconstruct_group1:
!---#[ subroutine reconstruct_group2:
subroutine     reconstruct_group2(coeffs)
   use tens_rec
   use p16_bbarb_hepemg_config
   use p16_bbarb_hepemg_groups, only: tensrec_info_group2
   use p16_bbarb_hepemg_d130h0l1, only: numerator_d130 => numerator_golem95
   use p16_bbarb_hepemg_d130h0l1d, only: reconstruct_d130
   use p16_bbarb_hepemg_d167h0l1, only: numerator_d167 => numerator_golem95
   use p16_bbarb_hepemg_d167h0l1d, only: reconstruct_d167
   use p16_bbarb_hepemg_d204h0l1, only: numerator_d204 => numerator_golem95
   use p16_bbarb_hepemg_d204h0l1d, only: reconstruct_d204
   use p16_bbarb_hepemg_d235h0l1, only: numerator_d235 => numerator_golem95
   use p16_bbarb_hepemg_d235h0l1d, only: reconstruct_d235
   implicit none
   type(tensrec_info_group2), intent(out) :: coeffs
   !------#[ Diagram 130:
      if (tens_rec_by_derivatives) then
         call reconstruct_d130(coeffs)
      else
         call reconstruct2(numerator_d130, coeffs%coeffs_130)
      end if
   !------#] Diagram 130:
   !------#[ Diagram 167:
      if (tens_rec_by_derivatives) then
         call reconstruct_d167(coeffs)
      else
         call reconstruct1(numerator_d167, coeffs%coeffs_167)
      end if
   !------#] Diagram 167:
   !------#[ Diagram 204:
      if (tens_rec_by_derivatives) then
         call reconstruct_d204(coeffs)
      else
         call reconstruct3(numerator_d204, coeffs%coeffs_204)
      end if
   !------#] Diagram 204:
   !------#[ Diagram 235:
      if (tens_rec_by_derivatives) then
         call reconstruct_d235(coeffs)
      else
         call reconstruct2(numerator_d235, coeffs%coeffs_235)
      end if
   !------#] Diagram 235:
end subroutine reconstruct_group2
!---#] subroutine reconstruct_group2:
end module p16_bbarb_hepemg_golem95h0
