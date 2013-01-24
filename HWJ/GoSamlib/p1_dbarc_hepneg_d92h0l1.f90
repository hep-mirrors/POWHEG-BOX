module     p1_dbarc_hepneg_d92h0l1
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity0/d92h0l1.f90
   ! generator: haggies (1.1)
   use p1_dbarc_hepneg_config, only: ki
   use p1_dbarc_hepneg_util, only: cond
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p1_dbarc_hepneg_groups, only: &
!           & sign => diagram92_sign, shift => diagram92_shift
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh0
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d92
      complex(ki) :: t1
      complex(ki) :: t2

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q

      Q(1)   = cmplx(real(+Q_ext(4),   ki_sam), aimag(+Q_ext(4)),   ki)
      Q(2:4) = cmplx(real(+Q_ext(1:3), ki_sam), aimag(+Q_ext(1:3)), ki)
      d92 = 0.0_ki
      t1 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t2 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      d92 = (abb92n2+t1*abb92n3+t2*abb92n4+abb92n5*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3&
      &)-Q(4)*Q(4))+abb92n6*(Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))+t1*t2*&
      &abb92n7+t2*abb92n8*(Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*spvak6k2(3)-Q(&
      &4)*spvak6k2(4)))
      numerator = cmplx(real(d92, ki), aimag(d92), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh0
      implicit none

      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d92
      complex(ki) :: t1
      complex(ki) :: t2

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      Q(:) = cmplx(real(+Q_ext(:), ki_gol), 0.0_ki_gol, ki)
      d92 = 0.0_ki
      t1 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t2 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      d92 = (abb92n2+t1*abb92n3+t2*abb92n4+abb92n5*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3&
      &)-Q(4)*Q(4))+abb92n6*(Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))+t1*t2*&
      &abb92n7+t2*abb92n8*(Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*spvak6k2(3)-Q(&
      &4)*spvak6k2(4)))

      numerator = cmplx(real(d92, ki), aimag(d92), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p1_dbarc_hepneg_d92h0l1
