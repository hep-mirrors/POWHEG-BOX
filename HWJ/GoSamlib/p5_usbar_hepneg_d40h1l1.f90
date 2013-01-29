module     p5_usbar_hepneg_d40h1l1
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p5_
   ! usbar_hepneg/helicity1/d40h1l1.f90
   ! generator: haggies (1.1)
   use p5_usbar_hepneg_config, only: ki
   use p5_usbar_hepneg_util, only: cond
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
!      use p5_usbar_hepneg_groups, only: &
!           & sign => diagram40_sign, shift => diagram40_shift
      use p5_usbar_hepneg_globalsl1, only: epspow
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_abbrevh1
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d40

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(0:3) :: qshift

      qshift = k6
      Q(1)   = cmplx(real(+Q_ext(4) - qshift(0),   ki_sam), aimag(+Q_ext(4)),   &
      &ki)
      Q(2:4) = cmplx(real(+Q_ext(1:3) - qshift(1:3), ki_sam), aimag(+Q_ext(1:3))&
      &, ki)
      d40 = 0.0_ki
      d40 = (abb40n1+abb40n2*(Q(1)*spvak5k2(1)-Q(2)*spvak5k2(2)-Q(3)*spvak5k2(3)&
      &-Q(4)*spvak5k2(4))+abb40n3*(Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5&
      &k4(3)-Q(4)*spvak5k4(4))+abb40n4*(Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*s&
      &pvak6k2(3)-Q(4)*spvak6k2(4))+abb40n5*(Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(&
      &4)*k2(4))+abb40n6*(Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))+abb40n7*(&
      &Q(1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4)))
      numerator = cmplx(real(d40, ki), aimag(d40), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p5_usbar_hepneg_globalsl1, only: epspow
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_abbrevh1
      implicit none

      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d40

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(4) :: qshift

      qshift = k6
      Q(:) = cmplx(real(+Q_ext(:) - qshift(:), ki_gol), 0.0_ki_gol, ki)
      d40 = 0.0_ki
      d40 = (abb40n1+abb40n2*(Q(1)*spvak5k2(1)-Q(2)*spvak5k2(2)-Q(3)*spvak5k2(3)&
      &-Q(4)*spvak5k2(4))+abb40n3*(Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5&
      &k4(3)-Q(4)*spvak5k4(4))+abb40n4*(Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*s&
      &pvak6k2(3)-Q(4)*spvak6k2(4))+abb40n5*(Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(&
      &4)*k2(4))+abb40n6*(Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))+abb40n7*(&
      &Q(1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4)))

      numerator = cmplx(real(d40, ki), aimag(d40), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p5_usbar_hepneg_d40h1l1