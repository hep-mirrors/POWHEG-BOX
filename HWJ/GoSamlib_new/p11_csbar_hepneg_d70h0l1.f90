module     p11_csbar_hepneg_d70h0l1
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p11_csbar_hepneg/helicity0/d70h0l1.f90
   ! generator: haggies (1.1)
   use p11_csbar_hepneg_config, only: ki
   use p11_csbar_hepneg_util, only: cond
   
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      t1 = (Q(1)*e6(1)-Q(2)*e6(2)-Q(3)*e6(3)-Q(4)*e6(4))
      brack = (abb70n10+t1*abb70n7+abb70n11*(Q(1)*spvak2k6(1)-Q(2)*spvak2k6(2)-Q&
      &(3)*spvak2k6(3)-Q(4)*spvak2k6(4))+abb70n13*(Q(1)*spvae6l3(1)-Q(2)*spvae6l&
      &3(2)-Q(3)*spvae6l3(3)-Q(4)*spvae6l3(4))+abb70n14*(Q(1)*spvae6k6(1)-Q(2)*s&
      &pvae6k6(2)-Q(3)*spvae6k6(3)-Q(4)*spvae6k6(4))+abb70n15*(Q(1)*k6(1)-Q(2)*k&
      &6(2)-Q(3)*k6(3)-Q(4)*k6(4))+abb70n16*(Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)-Q(4)*&
      &Q(4))+abb70n17*(Q(1)*spvak5k6(1)-Q(2)*spvak5k6(2)-Q(3)*spvak5k6(3)-Q(4)*s&
      &pvak5k6(4))+abb70n18*(Q(1)*spvak6k1(1)-Q(2)*spvak6k1(2)-Q(3)*spvak6k1(3)-&
      &Q(4)*spvak6k1(4))+abb70n2*(Q(1)*spvae6k4(1)-Q(2)*spvae6k4(2)-Q(3)*spvae6k&
      &4(3)-Q(4)*spvae6k4(4))+abb70n20*(Q(1)*spvae6k2(1)-Q(2)*spvae6k2(2)-Q(3)*s&
      &pvae6k2(3)-Q(4)*spvae6k2(4))+abb70n21*(Q(1)*spvae6k1(1)-Q(2)*spvae6k1(2)-&
      &Q(3)*spvae6k1(3)-Q(4)*spvae6k1(4))+abb70n3*(Q(1)*spvak6k4(1)-Q(2)*spvak6k&
      &4(2)-Q(3)*spvak6k4(3)-Q(4)*spvak6k4(4))+abb70n5*(Q(1)*spval3e6(1)-Q(2)*sp&
      &val3e6(2)-Q(3)*spval3e6(3)-Q(4)*spval3e6(4))+abb70n6*(Q(1)*spvak2e6(1)-Q(&
      &2)*spvak2e6(2)-Q(3)*spvak2e6(3)-Q(4)*spvak2e6(4))+abb70n8*(Q(1)*spvak6e6(&
      &1)-Q(2)*spvak6e6(2)-Q(3)*spvak6e6(3)-Q(4)*spvak6e6(4))+abb70n9*(Q(1)*spva&
      &k5e6(1)-Q(2)*spvak5e6(2)-Q(3)*spvak5e6(3)-Q(4)*spvak5e6(4))+t1*abb70n12*(&
      &Q(1)*spvak2k1(1)-Q(2)*spvak2k1(2)-Q(3)*spvak2k1(3)-Q(4)*spvak2k1(4))+t1*a&
      &bb70n19*(Q(1)*spvak5k1(1)-Q(2)*spvak5k1(2)-Q(3)*spvak5k1(3)-Q(4)*spvak5k1&
      &(4))+t1*abb70n4*(Q(1)*spvak2k4(1)-Q(2)*spvak2k4(2)-Q(3)*spvak2k4(3)-Q(4)*&
      &spvak2k4(4)))
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p11_csbar_hepneg_groups, only: &
!           & sign => diagram70_sign, shift => diagram70_shift
      use p11_csbar_hepneg_globalsl1, only: epspow
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_abbrevh0
      implicit none

      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d70

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2

      Q(1)   = cmplx(real(-Q_ext(4),   ki_sam), aimag(-Q_ext(4)),   ki)
      Q(2:4) = cmplx(real(-Q_ext(1:3), ki_sam), aimag(-Q_ext(1:3)), ki)
      d70 = 0.0_ki
      d70 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d70, ki), aimag(d70), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p11_csbar_hepneg_globalsl1, only: epspow
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_abbrevh0
      implicit none

      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d70

      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:) = cmplx(real(-Q_ext(:), ki_gol), 0.0_ki_gol, ki)
      d70 = 0.0_ki
      d70 = (cond(epspow.eq.0,brack_1,Q,mu2))

      numerator = cmplx(real(d70, ki), aimag(d70), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p11_csbar_hepneg_d70h0l1
