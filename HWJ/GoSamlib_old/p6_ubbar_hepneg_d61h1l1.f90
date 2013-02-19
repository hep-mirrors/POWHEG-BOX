module     p6_ubbar_hepneg_d61h1l1
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p6_
   ! ubbar_hepneg/helicity1/d61h1l1.f90
   ! generator: haggies (1.1)
   use p6_ubbar_hepneg_config, only: ki
   use p6_ubbar_hepneg_util, only: cond
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
!      use p6_ubbar_hepneg_groups, only: &
!           & sign => diagram61_sign, shift => diagram61_shift
      use p6_ubbar_hepneg_globalsl1, only: epspow
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_abbrevh1
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d61
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8
      complex(ki) :: t9

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(0:3) :: qshift

      qshift = k6
      Q(1)   = cmplx(real(+Q_ext(4) - qshift(0),   ki_sam), aimag(+Q_ext(4)),   &
      &ki)
      Q(2:4) = cmplx(real(+Q_ext(1:3) - qshift(1:3), ki_sam), aimag(+Q_ext(1:3))&
      &, ki)
      d61 = 0.0_ki
      t1 = (Q(1)*spvak6k1(1)-Q(2)*spvak6k1(2)-Q(3)*spvak6k1(3)-Q(4)*spvak6k1(4))
      t2 = (Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))
      t3 = (Q(1)*k1(1)-Q(2)*k1(2)-Q(3)*k1(3)-Q(4)*k1(4))
      t4 = (Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*spvak6k2(3)-Q(4)*spvak6k2(4))
      t5 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t6 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t7 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      t8 = (Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)*spvak2k1(3)-Q(4)*spvak2k1(4))
      t9 = (Q(1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4))
      d61 = (abb61n1+t1*abb61n12+t2*abb61n13+t3*abb61n14+t4*abb61n3+t5*abb61n4+t&
      &6*abb61n5+t1*t2*abb61n17+t1*t4*abb61n21+t1*t5*abb61n18+t1*t6*abb61n16+t1*&
      &t7*abb61n2+t2*t4*abb61n9+t2*t7*abb61n6+t2*t9*abb61n19+t3*t7*abb61n7+t3*t9&
      &*abb61n20+t4*t5*abb61n10+t4*t6*abb61n10+t4*t7*abb61n15+t4*t8*abb61n16+t4*&
      &t9*abb61n22+t4*abb61n11*(Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(&
      &3)-Q(4)*spvak2k6(4))+t6*t7*abb61n8+t4*t7*t8*abb61n23)
      numerator = cmplx(real(d61, ki), aimag(d61), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p6_ubbar_hepneg_globalsl1, only: epspow
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_abbrevh1
      implicit none

      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d61
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8
      complex(ki) :: t9

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(4) :: qshift

      qshift = k6
      Q(:) = cmplx(real(+Q_ext(:) - qshift(:), ki_gol), 0.0_ki_gol, ki)
      d61 = 0.0_ki
      t1 = (Q(1)*spvak6k1(1)-Q(2)*spvak6k1(2)-Q(3)*spvak6k1(3)-Q(4)*spvak6k1(4))
      t2 = (Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))
      t3 = (Q(1)*k1(1)-Q(2)*k1(2)-Q(3)*k1(3)-Q(4)*k1(4))
      t4 = (Q(1)*spvak6k2(1)-Q(2)*spvak6k2(2)-Q(3)*spvak6k2(3)-Q(4)*spvak6k2(4))
      t5 = (Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))
      t6 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t7 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      t8 = (Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)*spvak2k1(3)-Q(4)*spvak2k1(4))
      t9 = (Q(1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4))
      d61 = (abb61n1+t1*abb61n12+t2*abb61n13+t3*abb61n14+t4*abb61n3+t5*abb61n4+t&
      &6*abb61n5+t1*t2*abb61n17+t1*t4*abb61n21+t1*t5*abb61n18+t1*t6*abb61n16+t1*&
      &t7*abb61n2+t2*t4*abb61n9+t2*t7*abb61n6+t2*t9*abb61n19+t3*t7*abb61n7+t3*t9&
      &*abb61n20+t4*t5*abb61n10+t4*t6*abb61n10+t4*t7*abb61n15+t4*t8*abb61n16+t4*&
      &t9*abb61n22+t4*abb61n11*(Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(&
      &3)-Q(4)*spvak2k6(4))+t6*t7*abb61n8+t4*t7*t8*abb61n23)

      numerator = cmplx(real(d61, ki), aimag(d61), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p6_ubbar_hepneg_d61h1l1
