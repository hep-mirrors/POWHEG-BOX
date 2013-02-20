module     p12_cbbar_hepneg_d70h0l1
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p12
   ! _cbbar_hepneg/helicity0/d70h0l1.f90
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
!           & sign => diagram70_sign, shift => diagram70_shift
      use p12_cbbar_hepneg_globalsl1, only: epspow
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_abbrevh0
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d70
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q

      Q(1)   = cmplx(real(-Q_ext(4),   ki_sam), aimag(-Q_ext(4)),   ki)
      Q(2:4) = cmplx(real(-Q_ext(1:3), ki_sam), aimag(-Q_ext(1:3)), ki)
      d70 = 0.0_ki
      t1 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      t2 = (Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)*spvak2k1(3)-Q(4)*spvak2k1(4))
      t3 = (Q(1)*spvak2k4(1)-Q(2)*spvak2k4(2)-Q(3)*spvak2k4(3)-Q(4)*spvak2k4(4))
      d70 = (abb70n7+t1*abb70n10+t2*abb70n14+t3*abb70n2+abb70n11*(Q(1)*k2(1)-Q(2&
      &)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))+abb70n4*(Q(1)*spval3k6(1)-Q(2)*spval3k6(2)&
      &-Q(3)*spval3k6(3)-Q(4)*spval3k6(4))+abb70n5*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3&
      &)-Q(4)*Q(4))+abb70n6*(Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))+abb70n&
      &8*(Q(1)*spvak5k6(1)-Q(2)*spvak5k6(2)-Q(3)*spvak5k6(3)-Q(4)*spvak5k6(4))+a&
      &bb70n9*(Q(1)*spvak2l3(1)-Q(2)*spvak2l3(2)-Q(3)*spvak2l3(3)-Q(4)*spvak2l3(&
      &4))+t1*t2*abb70n12+t1*t3*abb70n3+t1*abb70n13*(Q(1)*spvak5k1(1)-Q(2)*spvak&
      &5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4)))
      numerator = cmplx(real(d70, ki), aimag(d70), ki_sam)
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
      complex(ki) :: d70
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      Q(:) = cmplx(real(-Q_ext(:), ki_gol), 0.0_ki_gol, ki)
      d70 = 0.0_ki
      t1 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      t2 = (Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)*spvak2k1(3)-Q(4)*spvak2k1(4))
      t3 = (Q(1)*spvak2k4(1)-Q(2)*spvak2k4(2)-Q(3)*spvak2k4(3)-Q(4)*spvak2k4(4))
      d70 = (abb70n7+t1*abb70n10+t2*abb70n14+t3*abb70n2+abb70n11*(Q(1)*k2(1)-Q(2&
      &)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))+abb70n4*(Q(1)*spval3k6(1)-Q(2)*spval3k6(2)&
      &-Q(3)*spval3k6(3)-Q(4)*spval3k6(4))+abb70n5*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3&
      &)-Q(4)*Q(4))+abb70n6*(Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))+abb70n&
      &8*(Q(1)*spvak5k6(1)-Q(2)*spvak5k6(2)-Q(3)*spvak5k6(3)-Q(4)*spvak5k6(4))+a&
      &bb70n9*(Q(1)*spvak2l3(1)-Q(2)*spvak2l3(2)-Q(3)*spvak2l3(3)-Q(4)*spvak2l3(&
      &4))+t1*t2*abb70n12+t1*t3*abb70n3+t1*abb70n13*(Q(1)*spvak5k1(1)-Q(2)*spvak&
      &5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1(4)))

      numerator = cmplx(real(d70, ki), aimag(d70), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_cbbar_hepneg_d70h0l1