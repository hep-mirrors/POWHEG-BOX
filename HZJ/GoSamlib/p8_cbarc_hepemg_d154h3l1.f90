module     p8_cbarc_hepemg_d154h3l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity3d154h3l1.f90
   ! generator: buildfortran.py
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd154h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc154(19)
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspk6
      complex(ki) :: Qspk2
      complex(ki) :: Qspvak4k5
      complex(ki) :: QspQ
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak4k1
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspe6 = dotproduct(Q,e6)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspk6 = dotproduct(Q,k6)
      Qspk2 = dotproduct(Q,k2)
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      QspQ = dotproduct(Q,Q)
      Qspk1 = dotproduct(Q,k1)
      Qspvak4k1 = dotproduct(Q,spvak4k1)
      acc154(1)=abb154(5)
      acc154(2)=abb154(6)
      acc154(3)=abb154(7)
      acc154(4)=abb154(8)
      acc154(5)=abb154(10)
      acc154(6)=abb154(11)
      acc154(7)=abb154(13)
      acc154(8)=abb154(14)
      acc154(9)=abb154(15)
      acc154(10)=abb154(16)
      acc154(11)=abb154(17)
      acc154(12)=abb154(18)
      acc154(13)=acc154(6)*Qspvak6k1
      acc154(14)=Qspe6*acc154(3)
      acc154(15)=Qspvak2e6*acc154(10)
      acc154(16)=Qspk6-Qspk2
      acc154(17)=acc154(7)*acc154(16)
      acc154(13)=acc154(17)+acc154(15)+acc154(14)+acc154(8)+acc154(13)
      acc154(13)=Qspvak4k5*acc154(13)
      acc154(14)=acc154(12)*QspQ
      acc154(15)=acc154(11)*Qspk1
      acc154(17)=acc154(4)*Qspvak4k1
      acc154(18)=Qspe6*acc154(2)
      acc154(19)=Qspvak2e6*acc154(5)
      acc154(16)=-acc154(9)*acc154(16)
      brack=acc154(1)+acc154(13)+acc154(14)+acc154(15)+acc154(16)+acc154(17)+ac&
      &c154(18)+acc154(19)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p8_cbarc_hepemg_groups, only: &
!           & sign => diagram154_sign, shift => diagram154_shift
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd154h3
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d154
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k2
      Q(1)  =cmplx(real(+Q_ext(4)  -qshift(0),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3)-qshift(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d154 = 0.0_ki
      d154 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d154, ki), aimag(d154), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd154h3
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d154
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k2
      Q(:)  =cmplx(real(+Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d154 = 0.0_ki
      d154 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d154, ki), aimag(d154), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p8_cbarc_hepemg_d154h3l1
