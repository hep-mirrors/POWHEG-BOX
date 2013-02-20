module     p1_dbarc_hepneg_d60h1l1
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity1/d60h1l1.f90
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
!           & sign => diagram60_sign, shift => diagram60_shift
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh1
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d60
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(0:3) :: qshift

      qshift = -k2
      Q(1)   = cmplx(real(-Q_ext(4) - qshift(0),   ki_sam), aimag(-Q_ext(4)),   &
      &ki)
      Q(2:4) = cmplx(real(-Q_ext(1:3) - qshift(1:3), ki_sam), aimag(-Q_ext(1:3))&
      &, ki)
      d60 = 0.0_ki
      t1 = (Q(1)*spvak5k2(1)-Q(2)*spvak5k2(2)-Q(3)*spvak5k2(3)-Q(4)*spvak5k2(4))
      t2 = (Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*spvak6k2(3)-Q(4)*spvak6k2(4))
      t3 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t4 = (Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))
      t5 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t6 = (Q(1)*spvak1k2(1)-Q(2)*spvak1k2(2)-Q(3)*spvak1k2(3)-Q(4)*spvak1k2(4))
      d60 = (abb60n1+t1*abb60n2+t2*abb60n3+t2*t2*abb60n8+t3*abb60n4+t4*abb60n9+t&
      &1*t2*abb60n13+t1*t3*abb60n10+t1*t4*abb60n11+t1*t5*abb60n12+t2*t4*abb60n7+&
      &t2*t5*abb60n6+t2*t6*abb60n5+t2*abb60n14*(Q(1)*spvak1k4(1)-Q(2)*spvak1k4(2&
      &)-Q(3)*spvak1k4(3)-Q(4)*spvak1k4(4))+t2*abb60n6*(Q(1)*k1(1)-Q(2)*k1(2)-Q(&
      &3)*k1(3)-Q(4)*k1(4))+t2*t6*abb60n15*(Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(&
      &3)*spvak5k4(3)-Q(4)*spvak5k4(4)))
      numerator = cmplx(real(d60, ki), aimag(d60), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh1
      implicit none

      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d60
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(4) :: qshift

      qshift = -k2
      Q(:) = cmplx(real(-Q_ext(:) - qshift(:), ki_gol), 0.0_ki_gol, ki)
      d60 = 0.0_ki
      t1 = (Q(1)*spvak5k2(1)-Q(2)*spvak5k2(2)-Q(3)*spvak5k2(3)-Q(4)*spvak5k2(4))
      t2 = (Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*spvak6k2(3)-Q(4)*spvak6k2(4))
      t3 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t4 = (Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))
      t5 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t6 = (Q(1)*spvak1k2(1)-Q(2)*spvak1k2(2)-Q(3)*spvak1k2(3)-Q(4)*spvak1k2(4))
      d60 = (abb60n1+t1*abb60n2+t2*abb60n3+t2*t2*abb60n8+t3*abb60n4+t4*abb60n9+t&
      &1*t2*abb60n13+t1*t3*abb60n10+t1*t4*abb60n11+t1*t5*abb60n12+t2*t4*abb60n7+&
      &t2*t5*abb60n6+t2*t6*abb60n5+t2*abb60n14*(Q(1)*spvak1k4(1)-Q(2)*spvak1k4(2&
      &)-Q(3)*spvak1k4(3)-Q(4)*spvak1k4(4))+t2*abb60n6*(Q(1)*k1(1)-Q(2)*k1(2)-Q(&
      &3)*k1(3)-Q(4)*k1(4))+t2*t6*abb60n15*(Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(&
      &3)*spvak5k4(3)-Q(4)*spvak5k4(4)))

      numerator = cmplx(real(d60, ki), aimag(d60), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p1_dbarc_hepneg_d60h1l1