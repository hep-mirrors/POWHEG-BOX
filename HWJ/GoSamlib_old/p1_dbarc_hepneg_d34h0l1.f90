module     p1_dbarc_hepneg_d34h0l1
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity0/d34h0l1.f90
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
!           & sign => diagram34_sign, shift => diagram34_shift
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh0
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d34
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(0:3) :: qshift

      qshift = k2
      Q(1)   = cmplx(real(+Q_ext(4) - qshift(0),   ki_sam), aimag(+Q_ext(4)),   &
      &ki)
      Q(2:4) = cmplx(real(+Q_ext(1:3) - qshift(1:3), ki_sam), aimag(+Q_ext(1:3))&
      &, ki)
      d34 = 0.0_ki
      t1 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t2 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      t3 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      d34 = (abb34n2+t1*abb34n10+t2*abb34n11+t3*abb34n3+abb34n4*(Q(1)*k2(1)-Q(2)&
      &*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))+abb34n5*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)&
      &*Q(4))+abb34n6*(Q(1)*k1(1)-Q(2)*k1(2)-Q(3)*k1(3)-Q(4)*k1(4))+abb34n7*(Q(1&
      &)*spvak1k4(1)-Q(2)*spvak1k4(2)-Q(3)*spvak1k4(3)-Q(4)*spvak1k4(4))+t1*t3*a&
      &bb34n9+t2*t3*abb34n12+t3*abb34n8*(Q(1)*spvak1k6(1)-Q(2)*spvak1k6(2)-Q(3)*&
      &spvak1k6(3)-Q(4)*spvak1k6(4)))
      numerator = cmplx(real(d34, ki), aimag(d34), ki_sam)
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
      complex(ki) :: d34
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      real(ki), dimension(4) :: qshift

      qshift = k2
      Q(:) = cmplx(real(+Q_ext(:) - qshift(:), ki_gol), 0.0_ki_gol, ki)
      d34 = 0.0_ki
      t1 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t2 = (Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4))
      t3 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      d34 = (abb34n2+t1*abb34n10+t2*abb34n11+t3*abb34n3+abb34n4*(Q(1)*k2(1)-Q(2)&
      &*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))+abb34n5*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)&
      &*Q(4))+abb34n6*(Q(1)*k1(1)-Q(2)*k1(2)-Q(3)*k1(3)-Q(4)*k1(4))+abb34n7*(Q(1&
      &)*spvak1k4(1)-Q(2)*spvak1k4(2)-Q(3)*spvak1k4(3)-Q(4)*spvak1k4(4))+t1*t3*a&
      &bb34n9+t2*t3*abb34n12+t3*abb34n8*(Q(1)*spvak1k6(1)-Q(2)*spvak1k6(2)-Q(3)*&
      &spvak1k6(3)-Q(4)*spvak1k6(4)))

      numerator = cmplx(real(d34, ki), aimag(d34), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p1_dbarc_hepneg_d34h0l1
