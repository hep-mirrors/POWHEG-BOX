module     p12_cbbar_hepneg_d59h0l1
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p12
   ! _cbbar_hepneg/helicity0/d59h0l1.f90
   ! generator: haggies (1.1)
   use p12_cbbar_hepneg_config, only: ki
   use p12_cbbar_hepneg_util, only: cond
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
!      use p12_cbbar_hepneg_groups, only: &
!           & sign => diagram59_sign, shift => diagram59_shift
      use p12_cbbar_hepneg_globalsl1, only: epspow
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_abbrevh0
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d59
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(0:3) :: qshift

      qshift = k2
      Q(1)   = cmplx(real(+Q_ext(4) - qshift(0),   ki_sam), aimag(+Q_ext(4)),   &
      &ki)
      Q(2:4) = cmplx(real(+Q_ext(1:3) - qshift(1:3), ki_sam), aimag(+Q_ext(1:3))&
      &, ki)
      d59 = 0.0_ki
      t1 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t2 = (Q(1)*spvak2k4(1)-Q(2)*spvak2k4(2)-Q(3)*spvak2k4(3)-Q(4)*spvak2k4(4))
      t3 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t4 = (Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))
      t5 = (Q(1)*k1(1)-Q(2)*k1(2)-Q(3)*k1(3)-Q(4)*k1(4))
      t6 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      t7 = (Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)*spvak2k1(3)-Q(4)*spvak2k1(4))
      t8 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      d59 = (abb59n1+t1*abb59n11+t2*abb59n2+t3*abb59n3+t4*abb59n4+t5*abb59n5+t6*&
      &abb59n6+t7*abb59n7+t8*abb59n8+t1*t6*abb59n10+t2*t3*abb59n14+t2*t4*abb59n1&
      &3+t2*t5*abb59n12+t2*t6*abb59n16+t3*t7*abb59n18+t4*t7*abb59n19+t5*t6*abb59&
      &n10+t5*t7*abb59n21+t6*t7*abb59n17+t6*t8*abb59n15+t6*abb59n20*(Q(1)*spvak6&
      &k1(1)-Q(2)*spvak6k1(2)-Q(3)*spvak6k1(3)-Q(4)*spvak6k1(4))+t6*abb59n22*(Q(&
      &1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4))+t7*t8*&
      &abb59n9+t6*t7*t8*abb59n23)
      numerator = cmplx(real(d59, ki), aimag(d59), ki_sam)
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
      complex(ki) :: d59
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(4) :: qshift

      qshift = k2
      Q(:) = cmplx(real(+Q_ext(:) - qshift(:), ki_gol), 0.0_ki_gol, ki)
      d59 = 0.0_ki
      t1 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t2 = (Q(1)*spvak2k4(1)-Q(2)*spvak2k4(2)-Q(3)*spvak2k4(3)-Q(4)*spvak2k4(4))
      t3 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t4 = (Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))
      t5 = (Q(1)*k1(1)-Q(2)*k1(2)-Q(3)*k1(3)-Q(4)*k1(4))
      t6 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      t7 = (Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)*spvak2k1(3)-Q(4)*spvak2k1(4))
      t8 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      d59 = (abb59n1+t1*abb59n11+t2*abb59n2+t3*abb59n3+t4*abb59n4+t5*abb59n5+t6*&
      &abb59n6+t7*abb59n7+t8*abb59n8+t1*t6*abb59n10+t2*t3*abb59n14+t2*t4*abb59n1&
      &3+t2*t5*abb59n12+t2*t6*abb59n16+t3*t7*abb59n18+t4*t7*abb59n19+t5*t6*abb59&
      &n10+t5*t7*abb59n21+t6*t7*abb59n17+t6*t8*abb59n15+t6*abb59n20*(Q(1)*spvak6&
      &k1(1)-Q(2)*spvak6k1(2)-Q(3)*spvak6k1(3)-Q(4)*spvak6k1(4))+t6*abb59n22*(Q(&
      &1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4))+t7*t8*&
      &abb59n9+t6*t7*t8*abb59n23)

      numerator = cmplx(real(d59, ki), aimag(d59), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_cbbar_hepneg_d59h0l1
