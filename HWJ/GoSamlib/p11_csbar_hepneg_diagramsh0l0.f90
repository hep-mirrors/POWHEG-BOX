module     p11_csbar_hepneg_diagramsh0l0
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p11_csbar_hepneg/helicity0/diagramsl0.f90
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
      complex(ki) :: t2
      real(ki) :: t3
      diagrams(:,:) = (0.0_ki, 0.0_ki)
      t3 = mW*mW
      t1 = i_*mW*wW
      t2 = t1+es345-t3
      t1 = t1+es45-t3
      t3 = gW*gW
      diagrams(2, :) = spbk6e6*spbk4k1*spak5k6*spak2e6*i_*gHWW*(-2.0_ki)/((es345&
      &-(es61+es12))*t1*t2)*t3*CVSC*c1
      t1 = t1*t2*es61
      diagrams(1, :) = (spak2k5*i_*(2.0_ki)*(t3/t1*gHWW*spae6k6*spbe6k1*spbk6k4-&
      &t3/t1*gHWW*spak1e6*spbe6k1*spbk4k1)*CVSC*c1)
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
end module p11_csbar_hepneg_diagramsh0l0
