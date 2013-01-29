module    p6_ubbar_hepneg_amplitudeh1
   use p6_ubbar_hepneg_config, only: ki, &
       & reduction_interoperation
   use p6_ubbar_hepneg_color, only: numcs
   use p6_ubbar_hepneg_groups
   use precision, only: ki_sam => ki
   use p6_ubbar_hepneg_samuraih1
   use precision_golem, only: ki_gol => ki
   use p6_ubbar_hepneg_golem95h1
   implicit none
   private

   public :: finite_renormalisation, samplitude
contains
!---#[ function finite_renormalisation:
   function     finite_renormalisation(scale2) result(amp)
      use p6_ubbar_hepneg_util, only: square
      use p6_ubbar_hepneg_color, only: CF, CA
      use p6_ubbar_hepneg_kinematics, only: &
      & num_light_quarks, num_gluons
      use p6_ubbar_hepneg_diagramsh1l0, only: amplitudel0 => amplitude
      use p6_ubbar_hepneg_diagramsh1l0fr5, only: amplitudel0fr5 => amplitude
      implicit none
      real(ki), intent(in) :: scale2
      real(ki) :: amp
      complex(ki),  dimension(numcs) :: amp0, amp5
      real(ki) :: deltaZ5
      amp = 0.0_ki
      amp0 = amplitudel0()
      !---#[ finite renormalisation of gamma5:
      ! We need to replace gamma5 by
      ! (1 + alpha_s/2pi deltaZ5) gamma5, where deltaZ5 is scheme
      ! dependend. See for example
      !    S. Weinzierl, ``Equivariant dimensional regularization,''
      !    arXiv:hep-ph/9903380
      !
      deltaZ5 = 0.0_ki * CF

      amp5 = amplitudel0fr5()
      amp = amp + deltaZ5 * square(amp0, amp5)
      !---#] finite renormalisation of gamma5:
   end function finite_renormalisation
   !---#] function finite_renormalisation:

   !---#[ function samplitude:
   function     samplitude(scale2,ok,opt_amp0,opt_perm)
      use p6_ubbar_hepneg_config, only: include_eps_terms, include_eps2_terms, &
      & logfile, debug_nlo_diagrams
      use p6_ubbar_hepneg_globalsl1, only: amp0,perm, use_perm, epspow
      use p6_ubbar_hepneg_globalsh1, &
     & only: init_lo, rat2
      use p6_ubbar_hepneg_abbrevh1, only: init_abbrev
      use p6_ubbar_hepneg_diagramsh1l0, only: amplitudel0 => amplitude
      use p6_ubbar_hepneg_groups
      implicit none
      real(ki), intent(in) :: scale2
      logical, intent(out) :: ok
      complex(ki), dimension(numcs), intent(in), optional :: opt_amp0
      integer, dimension(numcs), intent(in), optional :: opt_perm
      real(ki), dimension(-2:0) :: samplitude

      real(ki), dimension(-2:0) :: acc
      real(ki), dimension(0:2,-2:0) :: samp_part
      logical :: acc_ok

      ok = .true.
      samplitude(:) = 0.0_ki
      if (present(opt_amp0)) then
         amp0 = opt_amp0
      else
         amp0 = amplitudel0()
      end if
      if (present(opt_perm)) then
         use_perm = .true.
         perm = opt_perm
      else
         use_perm = .false.
      end if

      rat2 = (0.0_ki, 0.0_ki)
      call init_lo()
      call init_abbrev()
      epspow=0
      samplitude(-2) = 0.0_ki
      samplitude(-1) = 0.0_ki
      if(debug_nlo_diagrams) then
         write(logfile,'(A22,G24.16,A6,G24.16,A4)') &
         & "<result name='r2' re='", real(rat2, ki), &
         &                 "' im='", aimag(rat2), "' />"
      end if
      samplitude(0) = 2.0_ki * real(rat2, ki)
         call evaluate_group0(scale2, acc, acc_ok)
         ok = ok .and. acc_ok
         samplitude(:) = samplitude(:) + acc
         call evaluate_group1(scale2, acc, acc_ok)
         ok = ok .and. acc_ok
         samplitude(:) = samplitude(:) + acc
         call evaluate_group2(scale2, acc, acc_ok)
         ok = ok .and. acc_ok
         samplitude(:) = samplitude(:) + acc
         call evaluate_group3(scale2, acc, acc_ok)
         ok = ok .and. acc_ok
         samplitude(:) = samplitude(:) + acc
   end function samplitude
   !---#] function samplitude:
!---#[ subroutine evaluate_group0:
subroutine     evaluate_group0(scale2,samplitude,ok)
   use p6_ubbar_hepneg_config, only: &
      & logfile, debug_nlo_diagrams
   use p6_ubbar_hepneg_globalsl1, only: epspow
   use parametre, only: mu2_scale_par
   use form_factor_type, only: form_factor
   use p6_ubbar_hepneg_golem95h1, only: reconstruct_golem95 => reconstruct_group
   use p6_ubbar_hepneg_groups, only: contract_golem95, &
      & global_coeffs => coeffs_group0, &
      & reduce_numetens => reduce_numetens_group0
   use p6_ubbar_hepneg_samuraih1, only: samurai_reduce => reduce_group0
   use options, only: samurai_out => iout
   implicit none
   real(ki), intent(in) :: scale2
   logical, intent(out) :: ok
   real(ki), dimension(-2:0), intent(out) :: samplitude
   type(tensrec_info_group0), target :: coeffs
   type(form_factor) :: gres
   complex(ki_sam), dimension(-2:0) :: tot
   complex(ki_sam) :: totr
   logical :: samurai_ok

   if(debug_nlo_diagrams) then
      write(logfile,*) "<diagram-group index='0'>"
      write(logfile,*) "<param name='epspow' value='", epspow, "'/>"
   end if
   select case(reduction_interoperation)
   case(0) ! use Samurai only
      call samurai_reduce(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
   case(1) ! use Golem95 only
      call reconstruct_golem95(coeffs)
      mu2_scale_par = real(scale2, ki_gol)
      gres = contract_golem95(coeffs)
      samplitude(-2) = 2.0_ki * real(gres%A, ki)
      samplitude(-1) = 2.0_ki * real(gres%B, ki)
      samplitude( 0) = 2.0_ki * real(gres%C, ki)
      ok = .true.
   ! Modes which require Golem95 and Samurai
   case(2) ! Try Samurai first, use Golem95 is samurai fails
      call samurai_reduce(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         call reconstruct_golem95(coeffs)
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case(3) ! Tensorial Reconstruction + Samurai on numetens
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
      nullify(global_coeffs)
   case(4) ! Tensorial Reconstruction + Samurai on numetens
           ! + Golem95 on failure
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case default
      print*, "Your current choice of reduction_interoperation is", &
            & reduction_interoperation
      print*, "This choice is not valid for your current setup."
      print*, "* This code was generated with support for Samurai."
      print*, "* This code was generated with support for Golem95."
      print*, "* This code was generated without support for PJFry."
   end select

   if(debug_nlo_diagrams) then
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-finite' value='", samplitude(0), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-single' value='", samplitude(-1), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-double' value='", samplitude(-2), "'/>"
      if(ok) then
         write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
      else
         write(logfile,'(A29)') "<flag name='ok' status='no'/>"
      end if
      write(logfile,*) "</diagram-group>"
   end if
end subroutine evaluate_group0
!---#] subroutine evaluate_group0:
!---#[ subroutine evaluate_group1:
subroutine     evaluate_group1(scale2,samplitude,ok)
   use p6_ubbar_hepneg_config, only: &
      & logfile, debug_nlo_diagrams
   use p6_ubbar_hepneg_globalsl1, only: epspow
   use parametre, only: mu2_scale_par
   use form_factor_type, only: form_factor
   use p6_ubbar_hepneg_golem95h1, only: reconstruct_golem95 => reconstruct_group
   use p6_ubbar_hepneg_groups, only: contract_golem95, &
      & global_coeffs => coeffs_group1, &
      & reduce_numetens => reduce_numetens_group1
   use p6_ubbar_hepneg_samuraih1, only: samurai_reduce => reduce_group1
   use options, only: samurai_out => iout
   implicit none
   real(ki), intent(in) :: scale2
   logical, intent(out) :: ok
   real(ki), dimension(-2:0), intent(out) :: samplitude
   type(tensrec_info_group1), target :: coeffs
   type(form_factor) :: gres
   complex(ki_sam), dimension(-2:0) :: tot
   complex(ki_sam) :: totr
   logical :: samurai_ok

   if(debug_nlo_diagrams) then
      write(logfile,*) "<diagram-group index='1'>"
      write(logfile,*) "<param name='epspow' value='", epspow, "'/>"
   end if
   select case(reduction_interoperation)
   case(0) ! use Samurai only
      call samurai_reduce(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
   case(1) ! use Golem95 only
      call reconstruct_golem95(coeffs)
      mu2_scale_par = real(scale2, ki_gol)
      gres = contract_golem95(coeffs)
      samplitude(-2) = 2.0_ki * real(gres%A, ki)
      samplitude(-1) = 2.0_ki * real(gres%B, ki)
      samplitude( 0) = 2.0_ki * real(gres%C, ki)
      ok = .true.
   ! Modes which require Golem95 and Samurai
   case(2) ! Try Samurai first, use Golem95 is samurai fails
      call samurai_reduce(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         call reconstruct_golem95(coeffs)
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case(3) ! Tensorial Reconstruction + Samurai on numetens
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
      nullify(global_coeffs)
   case(4) ! Tensorial Reconstruction + Samurai on numetens
           ! + Golem95 on failure
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case default
      print*, "Your current choice of reduction_interoperation is", &
            & reduction_interoperation
      print*, "This choice is not valid for your current setup."
      print*, "* This code was generated with support for Samurai."
      print*, "* This code was generated with support for Golem95."
      print*, "* This code was generated without support for PJFry."
   end select

   if(debug_nlo_diagrams) then
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-finite' value='", samplitude(0), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-single' value='", samplitude(-1), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-double' value='", samplitude(-2), "'/>"
      if(ok) then
         write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
      else
         write(logfile,'(A29)') "<flag name='ok' status='no'/>"
      end if
      write(logfile,*) "</diagram-group>"
   end if
end subroutine evaluate_group1
!---#] subroutine evaluate_group1:
!---#[ subroutine evaluate_group2:
subroutine     evaluate_group2(scale2,samplitude,ok)
   use p6_ubbar_hepneg_config, only: &
      & logfile, debug_nlo_diagrams
   use p6_ubbar_hepneg_globalsl1, only: epspow
   use parametre, only: mu2_scale_par
   use form_factor_type, only: form_factor
   use p6_ubbar_hepneg_golem95h1, only: reconstruct_golem95 => reconstruct_group
   use p6_ubbar_hepneg_groups, only: contract_golem95, &
      & global_coeffs => coeffs_group2, &
      & reduce_numetens => reduce_numetens_group2
   use p6_ubbar_hepneg_samuraih1, only: samurai_reduce => reduce_group2
   use options, only: samurai_out => iout
   implicit none
   real(ki), intent(in) :: scale2
   logical, intent(out) :: ok
   real(ki), dimension(-2:0), intent(out) :: samplitude
   type(tensrec_info_group2), target :: coeffs
   type(form_factor) :: gres
   complex(ki_sam), dimension(-2:0) :: tot
   complex(ki_sam) :: totr
   logical :: samurai_ok

   if(debug_nlo_diagrams) then
      write(logfile,*) "<diagram-group index='2'>"
      write(logfile,*) "<param name='epspow' value='", epspow, "'/>"
   end if
   select case(reduction_interoperation)
   case(0) ! use Samurai only
      call samurai_reduce(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
   case(1) ! use Golem95 only
      call reconstruct_golem95(coeffs)
      mu2_scale_par = real(scale2, ki_gol)
      gres = contract_golem95(coeffs)
      samplitude(-2) = 2.0_ki * real(gres%A, ki)
      samplitude(-1) = 2.0_ki * real(gres%B, ki)
      samplitude( 0) = 2.0_ki * real(gres%C, ki)
      ok = .true.
   ! Modes which require Golem95 and Samurai
   case(2) ! Try Samurai first, use Golem95 is samurai fails
      call samurai_reduce(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         call reconstruct_golem95(coeffs)
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case(3) ! Tensorial Reconstruction + Samurai on numetens
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
      nullify(global_coeffs)
   case(4) ! Tensorial Reconstruction + Samurai on numetens
           ! + Golem95 on failure
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case default
      print*, "Your current choice of reduction_interoperation is", &
            & reduction_interoperation
      print*, "This choice is not valid for your current setup."
      print*, "* This code was generated with support for Samurai."
      print*, "* This code was generated with support for Golem95."
      print*, "* This code was generated without support for PJFry."
   end select

   if(debug_nlo_diagrams) then
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-finite' value='", samplitude(0), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-single' value='", samplitude(-1), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-double' value='", samplitude(-2), "'/>"
      if(ok) then
         write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
      else
         write(logfile,'(A29)') "<flag name='ok' status='no'/>"
      end if
      write(logfile,*) "</diagram-group>"
   end if
end subroutine evaluate_group2
!---#] subroutine evaluate_group2:
!---#[ subroutine evaluate_group3:
subroutine     evaluate_group3(scale2,samplitude,ok)
   use p6_ubbar_hepneg_config, only: &
      & logfile, debug_nlo_diagrams
   use p6_ubbar_hepneg_globalsl1, only: epspow
   use parametre, only: mu2_scale_par
   use form_factor_type, only: form_factor
   use p6_ubbar_hepneg_golem95h1, only: reconstruct_golem95 => reconstruct_group
   use p6_ubbar_hepneg_groups, only: contract_golem95, &
      & global_coeffs => coeffs_group3, &
      & reduce_numetens => reduce_numetens_group3
   use p6_ubbar_hepneg_samuraih1, only: samurai_reduce => reduce_group3
   use options, only: samurai_out => iout
   implicit none
   real(ki), intent(in) :: scale2
   logical, intent(out) :: ok
   real(ki), dimension(-2:0), intent(out) :: samplitude
   type(tensrec_info_group3), target :: coeffs
   type(form_factor) :: gres
   complex(ki_sam), dimension(-2:0) :: tot
   complex(ki_sam) :: totr
   logical :: samurai_ok

   if(debug_nlo_diagrams) then
      write(logfile,*) "<diagram-group index='3'>"
      write(logfile,*) "<param name='epspow' value='", epspow, "'/>"
   end if
   select case(reduction_interoperation)
   case(0) ! use Samurai only
      call samurai_reduce(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
   case(1) ! use Golem95 only
      call reconstruct_golem95(coeffs)
      mu2_scale_par = real(scale2, ki_gol)
      gres = contract_golem95(coeffs)
      samplitude(-2) = 2.0_ki * real(gres%A, ki)
      samplitude(-1) = 2.0_ki * real(gres%B, ki)
      samplitude( 0) = 2.0_ki * real(gres%C, ki)
      ok = .true.
   ! Modes which require Golem95 and Samurai
   case(2) ! Try Samurai first, use Golem95 is samurai fails
      call samurai_reduce(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         call reconstruct_golem95(coeffs)
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case(3) ! Tensorial Reconstruction + Samurai on numetens
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, ok)
      samplitude(:) = 2.0_ki * real(tot(:), ki)
      nullify(global_coeffs)
   case(4) ! Tensorial Reconstruction + Samurai on numetens
           ! + Golem95 on failure
      call reconstruct_golem95(coeffs)
      global_coeffs => coeffs
      call reduce_numetens(real(scale2, ki_sam), tot, totr, samurai_ok)
      if(samurai_ok) then
         samplitude(:) = 2.0_ki * real(tot(:), ki)
         ok = .true.
      else
         mu2_scale_par = real(scale2, ki_gol)
         gres = contract_golem95(coeffs)
         samplitude(-2) = 2.0_ki * real(gres%A, ki)
         samplitude(-1) = 2.0_ki * real(gres%B, ki)
         samplitude( 0) = 2.0_ki * real(gres%C, ki)
         ok = .true.
      end if
   case default
      print*, "Your current choice of reduction_interoperation is", &
            & reduction_interoperation
      print*, "This choice is not valid for your current setup."
      print*, "* This code was generated with support for Samurai."
      print*, "* This code was generated with support for Golem95."
      print*, "* This code was generated without support for PJFry."
   end select

   if(debug_nlo_diagrams) then
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-finite' value='", samplitude(0), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-single' value='", samplitude(-1), "'/>"
      write(logfile,'(A33,E24.16,A3)') &
         & "<result kind='nlo-double' value='", samplitude(-2), "'/>"
      if(ok) then
         write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
      else
         write(logfile,'(A29)') "<flag name='ok' status='no'/>"
      end if
      write(logfile,*) "</diagram-group>"
   end if
end subroutine evaluate_group3
!---#] subroutine evaluate_group3:
end module p6_ubbar_hepneg_amplitudeh1