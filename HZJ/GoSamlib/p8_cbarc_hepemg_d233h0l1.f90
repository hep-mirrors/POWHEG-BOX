module     p8_cbarc_hepemg_d233h0l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p8_cb &
   ! &arc_hepemg/helicity0d233h0l1.f90
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
      use p8_cbarc_hepemg_abbrevd233h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc233(17)
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
      acc233(1)=abb233(5)
      acc233(2)=abb233(6)
      acc233(3)=abb233(9)
      acc233(4)=abb233(11)
      acc233(5)=abb233(12)
      acc233(6)=abb233(13)
      acc233(7)=abb233(14)
      acc233(8)=abb233(16)
      acc233(9)=abb233(17)
      acc233(10)=abb233(18)
      acc233(11)=Qspk2*acc233(8)
      acc233(12)=Qspvak6k2*acc233(2)
      acc233(11)=acc233(12)+acc233(5)+acc233(11)
      acc233(11)=Qspe6*acc233(11)
      acc233(12)=acc233(10)*Qspvae6k2
      acc233(13)=acc233(9)*QspQ
      acc233(14)=acc233(7)*Qspk6
      acc233(15)=acc233(1)*Qspvak2k6
      acc233(16)=Qspk2*acc233(4)
      acc233(17)=Qspvak6k2*acc233(3)
      brack=acc233(6)+acc233(11)+acc233(12)+acc233(13)+acc233(14)+acc233(15)+ac&
      &c233(16)+acc233(17)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p8_cbarc_hepemg_groups, only: &
!           & sign => diagram233_sign, shift => diagram233_shift
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd233h0
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d233
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(1)  =cmplx(real(+Q_ext(4),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d233 = 0.0_ki
      d233 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d233, ki), aimag(d233), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd233h0
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d233
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:)  =cmplx(real(+Q_ext(:),  ki_gol), 0.0_ki_gol, ki)
      d233 = 0.0_ki
      d233 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d233, ki), aimag(d233), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p8_cbarc_hepemg_d233h0l1
