module     p0_dbard_hepemg_util
   use p0_dbard_hepemg_color, only: numcs
   use p0_dbard_hepemg_config, only: ki
   implicit none
   private

   interface     square
      module procedure square_0l_0l
      module procedure square_0l_1l
      module procedure square_0l_0l_mat
   end interface square

   interface     cond
      module procedure cond_q_mu2
      module procedure cond_mu2
   end interface

   public :: square
   public :: inspect_lo_diagram
   public :: cond
   public :: cmplx_sam, cmplx_ki, metric_tensor
contains
   pure function metric_tensor(mu,nu) result(d)
      implicit none
      integer, intent(in) :: mu, nu
      real(ki) :: d
      if (mu.ne.nu) then
         d = 0.0_ki
      elseif (mu.eq.1) then
         d = 1.0_ki
      else
         d = -1.0_ki
      end if
   end  function metric_tensor

   pure function cond_q_mu2(cnd, brack, Q, mu2) result(cond)
      implicit none
      logical, intent(in) :: cnd
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2

      complex(ki) :: cond

      interface
         pure function brack(inner_Q, inner_mu2)
            use p0_dbard_hepemg_config, only: ki
            implicit none
            complex(ki), dimension(4), intent(in) :: inner_Q
            complex(ki), intent(in) :: inner_mu2
            complex(ki) :: brack
         end  function brack
      end interface

      if (cnd) then
         cond = brack(Q, mu2)
      else
         cond = (0.0_ki, 0.0_ki)
      end if
   end  function cond_q_mu2

   pure function cond_mu2(cnd, brack, mu2) result(cond)
      implicit none
      logical, intent(in) :: cnd
      complex(ki), intent(in) :: mu2

      complex(ki) :: cond

      interface
         pure function brack(inner_mu2)
            use p0_dbard_hepemg_config, only: ki
            implicit none
            complex(ki), intent(in) :: inner_mu2
            complex(ki) :: brack
         end  function brack
      end interface

      if (cnd) then
         cond = brack(mu2)
      else
         cond = (0.0_ki, 0.0_ki)
      end if
   end  function cond_mu2

   subroutine     inspect_lo_diagram(values, d, h, unit)
      implicit none

      complex(ki), dimension(numcs), intent(in) :: values
      integer, intent(in) :: d, h
      integer, intent(in), optional :: unit

      integer :: ch, i

      if(present(unit)) then
              ch = unit
      else
              ch = 5
      end if

      write(ch,'(A19,I3,A2)') "<lo-diagram index='", d, "'>"
      do i=1,numcs
         write(ch,'(A21,I3,A6,G23.16,A6,G23.16,A3)') &
            & "<result color-index='", i-1, "' re='", real(values(i)), &
            & "' im='", aimag(values(i)), "'/>"
      end do
      write(ch,'(A13)') "</lo-diagram>"
   end subroutine inspect_lo_diagram

!   subroutine     inspect_nlo_diagram(values, d, h, unit)
!      implicit none
!
!      complex(ki), dimension(0:2), intent(in) :: values
!      integer, intent(in) :: d, h
!      integer, intent(in), optional :: unit
!
!      integer :: ch
!
!      if(present(unit)) then
!              ch = unit
!      else
!              ch = 5
!      end if
!
!      write(ch,'(A12,I6,A1,I3,A11,G23.16,A1,G23.16,A2)') &
!         & "evt.set_nlo(", d, ",", h, &
!         & ",2,complex(", real(values(2)), ",", aimag(values(2)), "))"
!      write(ch,'(A12,I6,A1,I3,A11,G23.16,A1,G23.16,A2)') &
!         & "evt.set_nlo(", d, ",", h, &
!         & ",1,complex(", real(values(1)), ",", aimag(values(1)), "))"
!      write(ch,'(A12,I6,A1,I3,A11,G23.16,A1,G23.16,A2)') &
!         & "evt.set_nlo(", d, ",", h, &
!         & ",0,complex(", real(values(0)), ",", aimag(values(0)), "))"
!   end subroutine inspect_nlo_diagram

   !---#[ function cmplx_sam:
   pure elemental function cmplx_sam(z) result(res)
      use precision, only: ki_sam => ki
      implicit none
      complex(ki), intent(in) :: z
      complex(ki_sam) :: res

      res = cmplx(real(z, ki_sam), aimag(z), ki_sam)
   end function cmplx_sam
   !---#] function cmplx_sam:
   !---#[ function cmplx_ki:
   pure elemental function cmplx_ki(z) result(res)
      use precision, only: ki_sam => ki
      implicit none
      complex(ki_sam), intent(in) :: z
      complex(ki) :: res

      res = cmplx(real(z, ki), aimag(z), ki)
   end function cmplx_ki
   !---#] function cmplx_ki:
   !---#[ function square :
   pure function square_0l_0l(color_vector) result(amp)
      use p0_dbard_hepemg_color, only: cmat => CC
      implicit none
      complex(ki), dimension(numcs), intent(in) :: color_vector
      real(ki) :: amp
      complex(ki), dimension(numcs) :: v1, v2

      v1 = matmul(cmat, color_vector)
      v2 = conjg(color_vector)
      amp = real(sum(v1(:) * v2(:)), ki)
   end function  square_0l_0l
   pure function square_0l_1l(color_vector1, color_vector2) result(amp)
      use p0_dbard_hepemg_color, only: cmat => CC
      implicit none
      complex(ki), dimension(numcs), intent(in) :: color_vector1
      complex(ki), dimension(numcs), intent(in) :: color_vector2
      real(ki) :: amp
      complex(ki), dimension(numcs) :: v1, v2

      v1 = matmul(cmat, color_vector1)
      v2 = conjg(color_vector2)
      amp = 2.0_ki * real(sum(v1(:) * v2(:)), ki)
   end function  square_0l_1l

   pure function square_0l_0l_mat(color_vector, cmat) result(amp)
      implicit none
      complex(ki), dimension(numcs), intent(in) :: color_vector
      complex(ki), dimension(numcs,numcs), intent(in) :: cmat
      real(ki) :: amp
      complex(ki), dimension(numcs) :: v1, v2

      v1 = matmul(cmat, color_vector)
      v2 = conjg(color_vector)
      amp = real(sum(v1(:) * v2(:)), ki)
   end function  square_0l_0l_mat
   !---#] function square :
end module p0_dbard_hepemg_util
