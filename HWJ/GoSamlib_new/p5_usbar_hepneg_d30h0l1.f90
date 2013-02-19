module     p5_usbar_hepneg_d30h0l1
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p5_usbar_hepneg/helicity0/d30h0l1.f90
   ! generator: haggies (1.1)
   use p5_usbar_hepneg_config, only: ki
   use p5_usbar_hepneg_util, only: cond
   
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p5_usbar_hepneg_model
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_color
      use p5_usbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      t1 = (Q(1)*k1(1)-Q(2)*k1(2)-Q(3)*k1(3)-Q(4)*k1(4))
      t2 = (Q(1)*k6(1)-Q(2)*k6(2)-Q(3)*k6(3)-Q(4)*k6(4))
      t3 = (Q(1)*e6(1)-Q(2)*e6(2)-Q(3)*e6(3)-Q(4)*e6(4))
      t4 = (Q(1)*spvak5k4(1)-Q(2)*spvak5k4(2)-Q(3)*spvak5k4(3)-Q(4)*spvak5k4(4))
      t5 = (Q(1)*spvae6k1(1)-Q(2)*spvae6k1(2)-Q(3)*spvae6k1(3)-Q(4)*spvae6k1(4))
      brack = (abb30n2+t1*abb30n10+t2*abb30n11+t3*abb30n3+t4*abb30n4+t5*abb30n9+&
      &abb30n6*(Q(1)*spvak2k4(1)-Q(2)*spvak2k4(2)-Q(3)*spvak2k4(3)-Q(4)*spvak2k4&
      &(4))+abb30n7*(Q(1)*k2(1)-Q(2)*k2(2)-Q(3)*k2(3)-Q(4)*k2(4))+abb30n8*(Q(1)*&
      &Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*Q(4))+t1*t4*abb30n15+t2*t4*abb30n14+t3*t4*a&
      &bb30n5+t4*t5*abb30n12+t4*abb30n13*(Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)&
      &*spvak2k1(3)-Q(4)*spvak2k1(4))+t4*abb30n16*(Q(1)*spvak2k6(1)-Q(2)*spvak2k&
      &6(2)-Q(3)*spvak2k6(3)-Q(4)*spvak2k6(4)))
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p5_usbar_hepneg_groups, only: &
!           & sign => diagram30_sign, shift => diagram30_shift
      use p5_usbar_hepneg_globalsl1, only: epspow
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_abbrevh0
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d30

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift

      qshift = -k2
      Q(1)   = cmplx(real(-Q_ext(4) - qshift(0),   ki_sam), aimag(-Q_ext(4)),   &
      &ki)
      Q(2:4) = cmplx(real(-Q_ext(1:3) - qshift(1:3), ki_sam), aimag(-Q_ext(1:3))&
      &, ki)
      d30 = 0.0_ki
      d30 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d30, ki), aimag(d30), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p5_usbar_hepneg_globalsl1, only: epspow
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_abbrevh0
      implicit none

      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d30

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift

      qshift = -k2
      Q(:) = cmplx(real(-Q_ext(:) - qshift(:), ki_gol), 0.0_ki_gol, ki)
      d30 = 0.0_ki
      d30 = (cond(epspow.eq.0,brack_1,Q,mu2))

      numerator = cmplx(real(d30, ki), aimag(d30), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p5_usbar_hepneg_d30h0l1