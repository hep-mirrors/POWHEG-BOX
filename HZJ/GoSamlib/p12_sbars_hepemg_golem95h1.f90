module     p12_sbars_hepemg_golem95h1
   use precision_golem, only: ki_gol => ki
   use p12_sbars_hepemg_config, only: ki
   implicit none
   private
   interface reconstruct_group
      module procedure reconstruct_group0
      module procedure reconstruct_group1
      module procedure reconstruct_group2
      module procedure reconstruct_group3
      module procedure reconstruct_group4
      module procedure reconstruct_group5
   end interface

   public :: reconstruct_group
contains
!---#[ subroutine reconstruct_group0:
subroutine     reconstruct_group0(coeffs)
   use tens_rec
   use p12_sbars_hepemg_config
   use p12_sbars_hepemg_groups, only: tensrec_info_group0
   use p12_sbars_hepemg_d235h1l1, only: numerator_d235 => numerator_golem95
   use p12_sbars_hepemg_d235h1l1d, only: reconstruct_d235
   implicit none
   type(tensrec_info_group0), intent(out) :: coeffs
   !------#[ Diagram 235:
      if (tens_rec_by_derivatives) then
         call reconstruct_d235(coeffs)
      else
         call reconstruct3(numerator_d235, coeffs%coeffs_235)
      end if
   !------#] Diagram 235:
end subroutine reconstruct_group0
!---#] subroutine reconstruct_group0:
!---#[ subroutine reconstruct_group1:
subroutine     reconstruct_group1(coeffs)
   use tens_rec
   use p12_sbars_hepemg_config
   use p12_sbars_hepemg_groups, only: tensrec_info_group1
   use p12_sbars_hepemg_d145h1l1, only: numerator_d145 => numerator_golem95
   use p12_sbars_hepemg_d145h1l1d, only: reconstruct_d145
   use p12_sbars_hepemg_d154h1l1, only: numerator_d154 => numerator_golem95
   use p12_sbars_hepemg_d154h1l1d, only: reconstruct_d154
   use p12_sbars_hepemg_d196h1l1, only: numerator_d196 => numerator_golem95
   use p12_sbars_hepemg_d196h1l1d, only: reconstruct_d196
   use p12_sbars_hepemg_d234h1l1, only: numerator_d234 => numerator_golem95
   use p12_sbars_hepemg_d234h1l1d, only: reconstruct_d234
   use p12_sbars_hepemg_d266h1l1, only: numerator_d266 => numerator_golem95
   use p12_sbars_hepemg_d266h1l1d, only: reconstruct_d266
   implicit none
   type(tensrec_info_group1), intent(out) :: coeffs
   !------#[ Diagram 145:
      if (tens_rec_by_derivatives) then
         call reconstruct_d145(coeffs)
      else
         call reconstruct3(numerator_d145, coeffs%coeffs_145)
      end if
   !------#] Diagram 145:
   !------#[ Diagram 154:
      if (tens_rec_by_derivatives) then
         call reconstruct_d154(coeffs)
      else
         call reconstruct2(numerator_d154, coeffs%coeffs_154)
      end if
   !------#] Diagram 154:
   !------#[ Diagram 196:
      if (tens_rec_by_derivatives) then
         call reconstruct_d196(coeffs)
      else
         call reconstruct1(numerator_d196, coeffs%coeffs_196)
      end if
   !------#] Diagram 196:
   !------#[ Diagram 234:
      if (tens_rec_by_derivatives) then
         call reconstruct_d234(coeffs)
      else
         call reconstruct3(numerator_d234, coeffs%coeffs_234)
      end if
   !------#] Diagram 234:
   !------#[ Diagram 266:
      if (tens_rec_by_derivatives) then
         call reconstruct_d266(coeffs)
      else
         call reconstruct2(numerator_d266, coeffs%coeffs_266)
      end if
   !------#] Diagram 266:
end subroutine reconstruct_group1
!---#] subroutine reconstruct_group1:
!---#[ subroutine reconstruct_group2:
subroutine     reconstruct_group2(coeffs)
   use tens_rec
   use p12_sbars_hepemg_config
   use p12_sbars_hepemg_groups, only: tensrec_info_group2
   use p12_sbars_hepemg_d150h1l1, only: numerator_d150 => numerator_golem95
   use p12_sbars_hepemg_d150h1l1d, only: reconstruct_d150
   use p12_sbars_hepemg_d193h1l1, only: numerator_d193 => numerator_golem95
   use p12_sbars_hepemg_d193h1l1d, only: reconstruct_d193
   use p12_sbars_hepemg_d233h1l1, only: numerator_d233 => numerator_golem95
   use p12_sbars_hepemg_d233h1l1d, only: reconstruct_d233
   use p12_sbars_hepemg_d268h1l1, only: numerator_d268 => numerator_golem95
   use p12_sbars_hepemg_d268h1l1d, only: reconstruct_d268
   implicit none
   type(tensrec_info_group2), intent(out) :: coeffs
   !------#[ Diagram 150:
      if (tens_rec_by_derivatives) then
         call reconstruct_d150(coeffs)
      else
         call reconstruct2(numerator_d150, coeffs%coeffs_150)
      end if
   !------#] Diagram 150:
   !------#[ Diagram 193:
      if (tens_rec_by_derivatives) then
         call reconstruct_d193(coeffs)
      else
         call reconstruct1(numerator_d193, coeffs%coeffs_193)
      end if
   !------#] Diagram 193:
   !------#[ Diagram 233:
      if (tens_rec_by_derivatives) then
         call reconstruct_d233(coeffs)
      else
         call reconstruct3(numerator_d233, coeffs%coeffs_233)
      end if
   !------#] Diagram 233:
   !------#[ Diagram 268:
      if (tens_rec_by_derivatives) then
         call reconstruct_d268(coeffs)
      else
         call reconstruct2(numerator_d268, coeffs%coeffs_268)
      end if
   !------#] Diagram 268:
end subroutine reconstruct_group2
!---#] subroutine reconstruct_group2:
!---#[ subroutine reconstruct_group3:
subroutine     reconstruct_group3(coeffs)
   use tens_rec
   use p12_sbars_hepemg_config
   use p12_sbars_hepemg_groups, only: tensrec_info_group3
   use p12_sbars_hepemg_d55h1l1, only: numerator_d55 => numerator_golem95
   use p12_sbars_hepemg_d55h1l1d, only: reconstruct_d55
   use p12_sbars_hepemg_d244h1l1, only: numerator_d244 => numerator_golem95
   use p12_sbars_hepemg_d244h1l1d, only: reconstruct_d244
   implicit none
   type(tensrec_info_group3), intent(out) :: coeffs
   !------#[ Diagram 55:
      if (tens_rec_by_derivatives) then
         call reconstruct_d55(coeffs)
      else
         call reconstruct4(numerator_d55, coeffs%coeffs_55)
      end if
   !------#] Diagram 55:
   !------#[ Diagram 244:
      if (tens_rec_by_derivatives) then
         call reconstruct_d244(coeffs)
      else
         call reconstruct3(numerator_d244, coeffs%coeffs_244)
      end if
   !------#] Diagram 244:
end subroutine reconstruct_group3
!---#] subroutine reconstruct_group3:
!---#[ subroutine reconstruct_group4:
subroutine     reconstruct_group4(coeffs)
   use tens_rec
   use p12_sbars_hepemg_config
   use p12_sbars_hepemg_groups, only: tensrec_info_group4
   use p12_sbars_hepemg_d43h1l1, only: numerator_d43 => numerator_golem95
   use p12_sbars_hepemg_d43h1l1d, only: reconstruct_d43
   implicit none
   type(tensrec_info_group4), intent(out) :: coeffs
   !------#[ Diagram 43:
      if (tens_rec_by_derivatives) then
         call reconstruct_d43(coeffs)
      else
         call reconstruct4(numerator_d43, coeffs%coeffs_43)
      end if
   !------#] Diagram 43:
end subroutine reconstruct_group4
!---#] subroutine reconstruct_group4:
!---#[ subroutine reconstruct_group5:
subroutine     reconstruct_group5(coeffs)
   use tens_rec
   use p12_sbars_hepemg_config
   use p12_sbars_hepemg_groups, only: tensrec_info_group5
   use p12_sbars_hepemg_d27h1l1, only: numerator_d27 => numerator_golem95
   use p12_sbars_hepemg_d27h1l1d, only: reconstruct_d27
   use p12_sbars_hepemg_d143h1l1, only: numerator_d143 => numerator_golem95
   use p12_sbars_hepemg_d143h1l1d, only: reconstruct_d143
   implicit none
   type(tensrec_info_group5), intent(out) :: coeffs
   !------#[ Diagram 27:
      if (tens_rec_by_derivatives) then
         call reconstruct_d27(coeffs)
      else
         call reconstruct4(numerator_d27, coeffs%coeffs_27)
      end if
   !------#] Diagram 27:
   !------#[ Diagram 143:
      if (tens_rec_by_derivatives) then
         call reconstruct_d143(coeffs)
      else
         call reconstruct3(numerator_d143, coeffs%coeffs_143)
      end if
   !------#] Diagram 143:
end subroutine reconstruct_group5
!---#] subroutine reconstruct_group5:
end module p12_sbars_hepemg_golem95h1
