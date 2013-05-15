module     p0_dbard_hepemg_d266h2l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p0_dbard_hepemg/helicity2d266h2l1.f90
   ! generator: buildfortran.py
   use p0_dbard_hepemg_config, only: ki
   use p0_dbard_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd266h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc266(17)
      complex(ki) :: Qspk2
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspe6
      complex(ki) :: Qspvae6k2
      complex(ki) :: QspQ
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak2k6
      Qspk2 = dotproduct(Q,k2)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspe6 = dotproduct(Q,e6)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      QspQ = dotproduct(Q,Q)
      Qspk6 = dotproduct(Q,k6)
      Qspvak2k6 = dotproduct(Q,spvak2k6)
      acc266(1)=abb266(5)
      acc266(2)=abb266(6)
      acc266(3)=abb266(9)
      acc266(4)=abb266(11)
      acc266(5)=abb266(12)
      acc266(6)=abb266(13)
      acc266(7)=abb266(14)
      acc266(8)=abb266(16)
      acc266(9)=abb266(17)
      acc266(10)=abb266(18)
      acc266(11)=Qspk2*acc266(8)
      acc266(12)=Qspvak6k2*acc266(2)
      acc266(11)=acc266(12)+acc266(5)+acc266(11)
      acc266(11)=Qspe6*acc266(11)
      acc266(12)=acc266(10)*Qspvae6k2
      acc266(13)=acc266(9)*QspQ
      acc266(14)=acc266(7)*Qspk6
      acc266(15)=acc266(1)*Qspvak2k6
      acc266(16)=Qspk2*acc266(4)
      acc266(17)=Qspvak6k2*acc266(3)
      brack=acc266(6)+acc266(11)+acc266(12)+acc266(13)+acc266(14)+acc266(15)+ac&
      &c266(16)+acc266(17)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p0_dbard_hepemg_groups, only: &
!           & sign => diagram266_sign, shift => diagram266_shift
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd266h2
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d266
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(1)  =cmplx(real(+Q_ext(4),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d266 = 0.0_ki
      d266 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d266, ki), aimag(d266), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd266h2
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d266
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:)  =cmplx(real(+Q_ext(:),  ki_gol), 0.0_ki_gol, ki)
      d266 = 0.0_ki
      d266 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d266, ki), aimag(d266), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p0_dbard_hepemg_d266h2l1
