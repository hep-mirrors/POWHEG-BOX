module     p1_dbarc_hepneg_diagramsh1l0
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p1_dbarc_hepneg/helicity1/diagramsl0.f90
   ! generator: haggies (1.1)
   use p1_dbarc_hepneg_color, only: numcs
   use p1_dbarc_hepneg_config, only: ki
   
   implicit none

   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   complex(ki), dimension(numcs), parameter :: zero_col = 0.0_ki

   public :: amplitude

contains
   function     amplitude()
      use p1_dbarc_hepneg_model
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_color
      use p1_dbarc_hepneg_config, only: debug_lo_diagrams, &
        & use_sorted_sum
      use p1_dbarc_hepneg_accu, only: sorted_sum
      use p1_dbarc_hepneg_util, only: inspect_lo_diagram
      implicit none
      complex(ki), dimension(numcs) :: amplitude
      complex(ki), dimension(2,numcs) :: diagrams
      integer :: i
      complex(ki) :: t1
      real(ki) :: t2
      real(ki) :: t3
      diagrams(:,:) = (0.0_ki, 0.0_ki)
      t2 = mW*mW
      t1 = i_*mW*wW
      t3 = gW*gW
      t1 = (t1+es345-t2)*(t1+es45-t2)*es61
      diagrams(1, :) = (spak1k6*i_*(4.0_ki)*(t3/(t1*sqrt2)*gHWW*spak5k6*spbk4k2-&
      &t3/(t1*spbk6k2*sqrt2)*gHWW*spak1k5*spbk2k1*spbk4k2)*CVDC*c1)
      if (debug_lo_diagrams) then
         call inspect_lo_diagram(diagrams(1,:), 4, 1, 19)
         call inspect_lo_diagram(diagrams(2,:), 7, 1, 19)
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
end module p1_dbarc_hepneg_diagramsh1l0
