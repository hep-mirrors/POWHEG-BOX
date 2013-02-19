module     p11_csbar_hepneg_diagramsh0l0fr5
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p11
   ! _csbar_hepneg/helicity0/diagramsl0fr5.f90
   ! generator: haggies (1.1)
   use p11_csbar_hepneg_color, only: numcs
   use p11_csbar_hepneg_config, only: ki
   implicit none

   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   complex(ki), dimension(numcs), parameter :: zero_col = 0.0_ki

   public :: amplitude

contains
   function     amplitude()
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_config, only: debug_lo_diagrams, &
        & use_sorted_sum
      use p11_csbar_hepneg_accu, only: sorted_sum
      use p11_csbar_hepneg_util, only: inspect_lo_diagram
      implicit none
      complex(ki), dimension(numcs) :: amplitude
      complex(ki), dimension(2,numcs) :: diagrams
      integer :: i
      complex(ki) :: t1
      real(ki) :: t2
      real(ki) :: t3
      diagrams(:,:) = (0.0_ki, 0.0_ki)
      diagrams(2, :) = (0.0_ki)
      t2 = mW*mW
      t1 = i_*mW*wW
      t3 = gW*gW
      t1 = (t1+es345-t2)*(t1+es45-t2)*es61
      diagrams(1, :) = (spak2k5*i_*(4.0_ki)*(t3/(t1*sqrt2)*gHWW*spbk6k1*spbk6k4-&
      &t3/(t1*spak2k6*sqrt2)*gHWW*spak1k2*spbk4k1*spbk6k1)*CVSC*c1)
      if (debug_lo_diagrams) then
         call inspect_lo_diagram(diagrams(1,:), 4, 0, 19)
         call inspect_lo_diagram(diagrams(2,:), 7, 0, 19)
      end if

      if (use_sorted_sum) then
         do i=1,numcs
            amplitude(i) = sorted_sum(diagrams(:,i))
         end do
      else
         do i=1,numcs
            amplitude(i) = sum(diagrams(:,i))
         end do
      end if
   end function     amplitude
end module p11_csbar_hepneg_diagramsh0l0fr5
