module     p12_cbbar_hepneg_d92h0l1
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p12_cbbar_hepneg/helicity0/d92h0l1.f90
   ! generator: haggies (1.1)
   use p12_cbbar_hepneg_config, only: ki
   use p12_cbbar_hepneg_util, only: cond
   
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_cbbar_hepneg_model
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_color
      use p12_cbbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      t1 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t2 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      t3 = (Q(1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4))
      t4 = (Q(1)*e6(1)-Q(2)*e6(2)-Q(3)*e6(3)-Q(4)*e6(4))
      t5 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      brack = (abb92n2+t1*abb92n11+t2*abb92n3+t3*abb92n4+t4*abb92n5+t5*abb92n9+a&
      &bb92n10*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))+abb92n6*(Q(1)*spvak2e6(&
      &1)-Q(2)*spvak2e6(2)-Q(3)*spvak2e6(3)-Q(4)*spvak2e6(4))+t1*t4*abb92n7+t2*t&
      &4*abb92n13+t3*t4*abb92n14+t4*t5*abb92n8+t4*abb92n12*(Q(1)*spvak2k6(1)-Q(2&
      &)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4)))
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_cbbar_hepneg_groups, only: &
!           & sign => diagram92_sign, shift => diagram92_shift
      use p12_cbbar_hepneg_globalsl1, only: epspow
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_abbrevh0
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d92

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2

      Q(1)   = cmplx(real(+Q_ext(4),   ki_sam), aimag(+Q_ext(4)),   ki)
      Q(2:4) = cmplx(real(+Q_ext(1:3), ki_sam), aimag(+Q_ext(1:3)), ki)
      d92 = 0.0_ki
      d92 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d92, ki), aimag(d92), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_cbbar_hepneg_globalsl1, only: epspow
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_abbrevh0
      implicit none

      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d92

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:) = cmplx(real(+Q_ext(:), ki_gol), 0.0_ki_gol, ki)
      d92 = 0.0_ki
      d92 = (cond(epspow.eq.0,brack_1,Q,mu2))

      numerator = cmplx(real(d92, ki), aimag(d92), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_cbbar_hepneg_d92h0l1