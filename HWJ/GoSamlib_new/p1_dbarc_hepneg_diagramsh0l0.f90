module     p1_dbarc_hepneg_diagramsh0l0
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p1_dbarc_hepneg/helicity0/diagramsl0.f90
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
      complex(ki) :: t2
      complex(ki) :: t3
      real(ki) :: t4
      diagrams(:,:) = (0.0_ki, 0.0_ki)
      t4 = mW*mW
      t1 = i_*mW*wW
      t2 = t1+es345-t4
      t1 = t1+es45-t4
      t4 = gW*gW
      diagrams(2, :) = spbk6k4*spbe6k2*spak1k5*spae6k6*i_*gHWW*(-2.0_ki)/((es345&
      &-(es61+es12))*t1*t2)*t4*CVDC*c1
      t3 = gHWW*spak1e6
      t1 = t1*t2*es61
      diagrams(1, :) = (i_*(2.0_ki)*(t3/t1*t4*spak5k6*spbk4k2*spbk6e6-t3/t1*t4*s&
      &pak1k5*spbe6k1*spbk4k2)*CVDC*c1)
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
end module p1_dbarc_hepneg_diagramsh0l0
