module     p0_dbaru_hepneg_matrix
   use msamurai, only: initsamurai, exitsamurai
   use p0_dbaru_hepneg_util, only: square
   use p0_dbaru_hepneg_config, only: ki, &
     & include_helicity_avg_factor, include_color_avg_factor, &
     & debug_lo_diagrams, debug_nlo_diagrams, &
     & include_symmetry_factor, &
     & SP_check, SP_verbosity, SP_rescue, SP_chk_threshold1, &
     & SP_chk_threshold2, reduction_interoperation, &
     & convert_to_cdr, &
     & samurai_verbosity, samurai_test, samurai_scalar
   use p0_dbaru_hepneg_kinematics, only: &
       in_helicities, symmetry_factor, num_legs, &
       lo_qcd_couplings, corrections_are_qcd, num_light_quarks, num_gluons
   use p0_dbaru_hepneg_model, only: Nf, NC, sqrt2, init_functions
   use p0_dbaru_hepneg_color, only: TR, CA, CF, numcs, &
     & incolors, init_color
   use p0_dbaru_hepneg_diagramsh0l0, only: amplitude0l0 => amplitude
   use p0_dbaru_hepneg_amplitudeh0, only: samplitudeh0l1 => samplitude, &
        &   finite_renormalisation0 => finite_renormalisation
   use p0_dbaru_hepneg_diagramsh1l0, only: amplitude1l0 => amplitude
   use p0_dbaru_hepneg_amplitudeh1, only: samplitudeh1l1 => samplitude, &
        &   finite_renormalisation1 => finite_renormalisation
   use p0_dbaru_hepneg_dipoles, only: insertion_operator

   implicit none
   save

   private

   integer :: banner_ch = 6

   public :: initgolem, exitgolem, samplitude
   public :: samplitudel0, samplitudel1
   public :: ir_subtraction, color_correlated_lo2, spin_correlated_lo2
contains
   !---#[ subroutine banner:
   subroutine     banner()
      implicit none

      character(len=72) :: frame = "+" // repeat("-", 70) // "+"

      if (banner_ch .le. 0) return

      write(banner_ch,'(A72)') frame
      write(banner_ch,'(A72)') "|   __   __   ___   __   __  __                  GoSam                 |"
      write(banner_ch,'(A72)') "|  / _) /  \ / __) (  ) (  \/  )         An Automated One-Loop         |"
      write(banner_ch,'(A72)') "| ( (/\( () )\__ \ /__\  )    (         Matrix Element Generator       |"
      write(banner_ch,'(A72)') "|  \__/ \__/ (___/(_)(_)(_/\/\_)              Version 1.0              |"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|                                   (c) The GoSam Collaboration 2011   |"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|                AUTHORS:                                              |"
      write(banner_ch,'(A72)') "|                * Gavin Cullen         <gavin.cullen@desy.de>         |"
      write(banner_ch,'(A72)') "|                * Nicolas Greiner      <greiner@mpp.mpg.de>           |"
      write(banner_ch,'(A72)') "|                * Gudrun Heinrich      <gudrun@mpp.mpg.de>            |"
      write(banner_ch,'(A72)') "|                * Gionata Luisoni      <gionata.luisoni@durham.ac.uk> |"
      write(banner_ch,'(A72)') "|                * Pierpaolo Mastrolia  <pierpaolo.mastrolia@cern.ch>  |"
      write(banner_ch,'(A72)') "|                * Giovanni Ossola      <gossola@citytech.cuny.edu>    |"
      write(banner_ch,'(A72)') "|                * Thomas Reiter        <reiterth@mpp.mpg.de>          |"
      write(banner_ch,'(A72)') "|                * Francesco Tramontano <francesco.tramontano@cern.ch> |"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|    This program is free software: you can redistribute it and/or modify|"
      write(banner_ch,'(A72)') "|    it under the terms of the GNU General Public License as published by|"
      write(banner_ch,'(A72)') "|    the Free Software Foundation, either version 3 of the License, or |"
      write(banner_ch,'(A72)') "|    (at your option) any later version.                               |"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|    This program is distributed in the hope that it will be useful,   |"
      write(banner_ch,'(A72)') "|    but WITHOUT ANY WARRANTY; without even the implied warranty of    |"
      write(banner_ch,'(A72)') "|    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     |"
      write(banner_ch,'(A72)') "|    GNU General Public License for more details.                      |"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|    You should have received a copy of the GNU General Public License |"
      write(banner_ch,'(A72)') "|    along with this program.  If not, see <http://www.gnu.org/licenses/>.|"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|    Scientific publications prepared using the present version of     |"
      write(banner_ch,'(A72)') "|    GoSam or any modified version of it or any code linking to        |"
      write(banner_ch,'(A72)') "|    GoSam or parts of it should make a clear reference to the publication:|"
      write(banner_ch,'(A72)') "|                                                                      |"
      write(banner_ch,'(A72)') "|        G. Cullen et al.,                                             |"
      write(banner_ch,'(A72)') "|        ``Automated One-Loop Calculations with GoSam,''               |"
      write(banner_ch,'(A72)') "|        arXiv:1111.2034 [hep-ph]                                      |"
      write(banner_ch,'(A72)') frame

      banner_ch = 0
   end subroutine banner
   !---#] subroutine banner:

   !---#[ subroutine initgolem :
   subroutine     initgolem(is_first)
      implicit none
      logical, optional, intent(in) :: is_first

      logical :: init_third_party

      if(present(is_first)) then
         init_third_party = is_first
      else
         init_third_party = .true.
      end if
      if (init_third_party) then
         call initsamurai('diag',samurai_scalar,&
         &                samurai_verbosity,samurai_test)
      if(SP_check) then
         open(unit=42, file='bad.pts', status='unknown', action='write', access='append')
      end if
      end if

      call init_functions()
      call init_color()

      ! call our banner last
      call banner()
   end subroutine initgolem
   !---#] subroutine initgolem :
   !---#[ subroutine exitgolem :
   subroutine     exitgolem(is_last)
      use p0_dbaru_hepneg_groups, only: tear_down_golem95
      implicit none
      logical, optional, intent(in) :: is_last

      logical :: exit_third_party

      if(present(is_last)) then
         exit_third_party = is_last
      else
         exit_third_party = .true.
      end if
      if (exit_third_party) then
         call exitsamurai()
         call tear_down_golem95()
         close(unit=42)
      end if
   end subroutine exitgolem
   !---#] subroutine exitgolem :

   !---#[ subroutine samplitude :
   subroutine     samplitude(vecs, scale2, amp, ok, h)
      use p0_dbaru_hepneg_config, only: &
         & reduction_interoperation, SP_check, SP_verbosity, &
         & SP_chk_threshold1, SP_chk_threshold2
      implicit none
      real(ki), dimension(6, 4), intent(in) :: vecs
      real(ki), intent(in) :: scale2
      real(ki), dimension(1:4), intent(out) :: amp
      logical, intent(out), optional :: ok
      integer, intent(in), optional :: h
      real(ki), dimension(2:3) :: irp
      integer :: tmp_red_int,  i 
      call samplitudel01(vecs, scale2, amp, ok, h)
      if(SP_check) then
      tmp_red_int=reduction_interoperation
      call ir_subtraction(vecs, scale2, irp)
      if(abs((amp(3)-irp(2))/amp(1)) .gt. SP_chk_threshold1) then
      if(SP_verbosity .eq. 3) write(*,*) "SINGLE POLE CHECK FAILED !!"
      if(SP_rescue) then
         reduction_interoperation = 1
         call samplitudel01(vecs, scale2, amp, ok, h)
         if(abs((amp(3)-irp(2))/amp(1)) .gt. (SP_chk_threshold2)) then
            if(SP_verbosity .ge. 2) then
               write(*,*) "RESCUE FAILED !!"
               write(*,*) "data:"
               write(*,*) "Single pol rel.dif., SP_chk_threshold2"
               write(*,*) amp(3)/amp(1)-irp(2)/amp(1), SP_chk_threshold2
               write(*,*)
               write(42,'(A7)')"<event>"
               write(42,'(A29,A30)') "Single pole check failed for:", "p0_dbaru_hepneg"
               write(42,'(A4,4A23)') "legs","SP_chk_threshold2", "Born       ", "single pole   ", "IR single pole"
               write(42,'(A2,4(2x,D23.16))') "6", SP_chk_threshold2, amp(1), amp(3)/amp(1), irp(2)/amp(1)
               write(42,'(A9)')"<momenta>"
               do i=1,6
                  write(42,'(2x,4(2x,D23.16))') vecs(i,:)
               enddo
               write(42,'(A10)')"</momenta>"
               write(42,'(A8)')"</event>"
            endif
         else
            if(SP_verbosity .eq. 3) write(*,*) "POINT SAVED !!"
            if(SP_verbosity .ge. 3) write(*,*)
         end if
         reduction_interoperation = tmp_red_int
      end if
      end if
      end if
   end subroutine samplitude
   !---#] subroutine samplitude :

   !---#[ subroutine samplitudel01 :
   subroutine     samplitudel01(vecs, scale2, amp, ok, h)
      use p0_dbaru_hepneg_config, only: &
         & debug_lo_diagrams, debug_nlo_diagrams, logfile, deltaOS, &
         & renormalisation, renorm_beta, renorm_mqwf, renorm_decoupling, &
         & renorm_logs, renorm_mqse, nlo_prefactors
      use p0_dbaru_hepneg_kinematics, only: &
         & inspect_kinematics, init_event
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_dipoles, only: pi
      implicit none
      real(ki), dimension(6, 4), intent(in) :: vecs
      real(ki), intent(in) :: scale2
      real(ki), dimension(4), intent(out) :: amp
      logical, intent(out), optional :: ok
      integer, intent(in), optional :: h
      real(ki) :: nlo_coupling

      ! Number of heavy quark flavours in loops.
      real(ki), parameter :: NFh = 0.0_ki

      logical :: my_ok

      ! used for m=0 QCD renormalisation
      real(ki) :: beta0

      if(corrections_are_qcd) then
         nlo_coupling = 1.0_ki
      else
         nlo_coupling = 1.0_ki
      end if

      call init_event(vecs)

      if(debug_lo_diagrams .or. debug_nlo_diagrams) then
         write(logfile,'(A7)') "<event>"
         call inspect_kinematics(logfile)
      end if

      
      if (present(h)) then
         amp(1) = samplitudel0(vecs, h)
      else
         amp(1)   = samplitudel0(vecs)
      end if
      select case (renormalisation)
      case (0)
         ! no renormalisation
         deltaOS = 0.0_ki
      case (1)
         ! fully renormalized
         if(renorm_mqse) then
            deltaOS = 1.0_ki
         else
            deltaOS = 0.0_ki
         end if
      case (2)
         ! massive quark counterterms only
         deltaOS = 1.0_ki
      case default
         ! not implemented
         print*, "In p0_dbaru_hepneg_matrix:"
         print*, "  invalid value for renormalisation=", renormalisation
         stop
      end select

      if (present(h)) then
         amp((/4,3,2/)) = samplitudel1(vecs, scale2, my_ok, h)/nlo_coupling
      else
         amp((/4,3,2/)) = samplitudel1(vecs, scale2, my_ok)/nlo_coupling
      end if
      select case (renormalisation)
      case (0)
         ! no renormalisation
      case (1)
         ! fully renormalized
         if(corrections_are_qcd) then
            if (renorm_beta) then
               beta0 = (11.0_ki * CA - 4.0_ki * TR * (NF + NFh)) / 6.0_ki
               amp(3) = amp(3) - lo_qcd_couplings * beta0 * amp(1)
               amp(2) = amp(2) + lo_qcd_couplings * CA / 6.0_ki * amp(1)
            end if
            if (renorm_mqwf) then
            end if
         end if
      case (2)
         ! massive quark counterterms only
      case default
         ! not implemented
         print*, "In p0_dbaru_hepneg_matrix:"
         print*, "  invalid value for renormalisation=", renormalisation
         stop
      end select
      if (convert_to_cdr) then
         ! Scheme conversion for infrared structure
         ! Reference:
         ! S. Catani, M. H. Seymour, Z. Trocsanyi,
         ! ``Regularisation scheme independence and unitarity
         !   in QCD cross-sections,''
         ! Phys.Rev. D 55 (1997) 6819
         ! arXiv:hep-ph/9610553
         amp(2) = amp(2) - amp(1) * (&
           &          num_light_quarks * 0.5_ki * CF &
           &        + num_gluons * 1.0_ki/6.0_ki * CA)
      end if
      if (present(ok)) ok = my_ok

      if(debug_lo_diagrams .or. debug_nlo_diagrams) then
         write(logfile,'(A25,E24.16,A3)') &
            & "<result kind='lo' value='", amp(1), "'/>"
         write(logfile,'(A33,E24.16,A3)') &
            & "<result kind='nlo-finite' value='", amp(2), "'/>"
         write(logfile,'(A33,E24.16,A3)') &
            & "<result kind='nlo-single' value='", amp(3), "'/>"
         write(logfile,'(A33,E24.16,A3)') &
            & "<result kind='nlo-double' value='", amp(4), "'/>"
         if(my_ok) then
            write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
         else
            write(logfile,'(A29)') "<flag name='ok' status='no'/>"
         end if
         write(logfile,'(A8)') "</event>"
      end if
      select case(nlo_prefactors)
      case(0)
         ! The result is already in its desired form
      case(1)
         amp(2:4) = amp(2:4) * nlo_coupling
      case(2)
         amp(2:4) = amp(2:4) * nlo_coupling / 8.0_ki / pi / pi
      end select
   end subroutine samplitudel01
   !---#] subroutine samplitudel01 :
   !---#[ function samplitudel0 :
   function     samplitudel0(vecs, h) result(amp)
      use p0_dbaru_hepneg_config, only: logfile
      use p0_dbaru_hepneg_kinematics, only: init_event
      implicit none
      real(ki), dimension(6, 4), intent(in) :: vecs
      integer, optional, intent(in) :: h
      real(ki) :: amp, heli_amp
      complex(ki), dimension(numcs) :: color_vector
      logical, dimension(0:31) :: eval_heli
      real(ki), dimension(6, 4) :: pvecs

      if (present(h)) then
         eval_heli(:) = .false.
         eval_heli(h) = .true.
      else
         eval_heli(:) = .true.
      end if

      amp = 0.0_ki
      if (eval_heli(0)) then
         if (debug_lo_diagrams) then
            write(logfile,*) "<helicity index='0' >"
         end if
         !---#[ reinitialize kinematics:
         pvecs(1,:) = vecs(1,:)
         pvecs(2,:) = vecs(2,:)
         pvecs(3,:) = vecs(3,:)
         pvecs(4,:) = vecs(4,:)
         pvecs(5,:) = vecs(5,:)
         pvecs(6,:) = vecs(6,:)
         call init_event(pvecs, -1)
         !---#] reinitialize kinematics:
         color_vector = amplitude0l0()
         heli_amp = square(color_vector)
         if (debug_lo_diagrams) then
            write(logfile,'(A25,E24.16,A3)') &
                & "<result kind='lo' value='", heli_amp, "'/>"
            write(logfile,*) "</helicity>"
         end if
         amp = amp + heli_amp
      end if
      if (eval_heli(1)) then
         if (debug_lo_diagrams) then
            write(logfile,*) "<helicity index='1' >"
         end if
         !---#[ reinitialize kinematics:
         pvecs(1,:) = vecs(1,:)
         pvecs(2,:) = vecs(2,:)
         pvecs(3,:) = vecs(3,:)
         pvecs(4,:) = vecs(4,:)
         pvecs(5,:) = vecs(5,:)
         pvecs(6,:) = vecs(6,:)
         call init_event(pvecs, +1)
         !---#] reinitialize kinematics:
         color_vector = amplitude1l0()
         heli_amp = square(color_vector)
         if (debug_lo_diagrams) then
            write(logfile,'(A25,E24.16,A3)') &
                & "<result kind='lo' value='", heli_amp, "'/>"
            write(logfile,*) "</helicity>"
         end if
         amp = amp + heli_amp
      end if
      if (include_helicity_avg_factor) then
         amp = amp / real(in_helicities, ki)
      end if
      if (include_color_avg_factor) then
         amp = amp / incolors
      end if
      if (include_symmetry_factor) then
         amp = amp / real(symmetry_factor, ki)
      end if
   end function samplitudel0
   !---#] function samplitudel0 :
   !---#[ function samplitudel1 :
   function     samplitudel1(vecs,scale2,ok,h) result(amp)
      use p0_dbaru_hepneg_config, only: &
         & debug_nlo_diagrams, logfile, renorm_gamma5
      use p0_dbaru_hepneg_kinematics, only: init_event
      implicit none
      real(ki), dimension(6, 4), intent(in) :: vecs
      logical, intent(out) :: ok
      real(ki), intent(in) :: scale2
      integer, optional, intent(in) :: h
      real(ki), dimension(6, 4) :: pvecs
      real(ki), dimension(-2:0) :: amp, heli_amp
      logical :: my_ok
      logical, dimension(0:31) :: eval_heli
      real(ki) :: fr

      if (present(h)) then
         eval_heli(:) = .false.
         eval_heli(h) = .true.
      else
         eval_heli(:) = .true.
      end if

      amp(:) = 0.0_ki
      ok = .true.
      if (eval_heli(0)) then
         if(debug_nlo_diagrams) then
            write(logfile,*) "<helicity index='0'>"
         end if
         !---#[ reinitialize kinematics:
         pvecs(1,:) = vecs(1,:)
         pvecs(2,:) = vecs(2,:)
         pvecs(3,:) = vecs(3,:)
         pvecs(4,:) = vecs(4,:)
         pvecs(5,:) = vecs(5,:)
         pvecs(6,:) = vecs(6,:)
         call init_event(pvecs, -1)
         !---#] reinitialize kinematics:
         heli_amp = samplitudeh0l1(real(scale2,ki),my_ok)
         if (corrections_are_qcd .and. renorm_gamma5) then
            !---#[ reinitialize kinematics:
            pvecs(1,:) = vecs(1,:)
            pvecs(2,:) = vecs(2,:)
            pvecs(3,:) = vecs(3,:)
            pvecs(4,:) = vecs(4,:)
            pvecs(5,:) = vecs(5,:)
            pvecs(6,:) = vecs(6,:)
            call init_event(pvecs, -1)
            !---#] reinitialize kinematics:
            fr = finite_renormalisation0(real(scale2,ki))
            heli_amp(0) = heli_amp(0) + fr
         end if
         ok = ok .and. my_ok
         amp = amp + heli_amp

         if(debug_nlo_diagrams) then
            write(logfile,'(A33,E24.16,A3)') &
                & "<result kind='nlo-finite' value='", heli_amp(0), "'/>"
            write(logfile,'(A33,E24.16,A3)') &
                & "<result kind='nlo-single' value='", heli_amp(-1), "'/>"
            write(logfile,'(A33,E24.16,A3)') &
                & "<result kind='nlo-double' value='", heli_amp(-2), "'/>"
            if (corrections_are_qcd .and. renorm_gamma5) then
               write(logfile,'(A30,E24.16,A3)') &
                   & "<result kind='fin-ren' value='", fr, "'/>"
            end if
            if(my_ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</helicity>"
         end if
      end if
      if (eval_heli(1)) then
         if(debug_nlo_diagrams) then
            write(logfile,*) "<helicity index='1'>"
         end if
         !---#[ reinitialize kinematics:
         pvecs(1,:) = vecs(1,:)
         pvecs(2,:) = vecs(2,:)
         pvecs(3,:) = vecs(3,:)
         pvecs(4,:) = vecs(4,:)
         pvecs(5,:) = vecs(5,:)
         pvecs(6,:) = vecs(6,:)
         call init_event(pvecs, +1)
         !---#] reinitialize kinematics:
         heli_amp = samplitudeh1l1(real(scale2,ki),my_ok)
         if (corrections_are_qcd .and. renorm_gamma5) then
            !---#[ reinitialize kinematics:
            pvecs(1,:) = vecs(1,:)
            pvecs(2,:) = vecs(2,:)
            pvecs(3,:) = vecs(3,:)
            pvecs(4,:) = vecs(4,:)
            pvecs(5,:) = vecs(5,:)
            pvecs(6,:) = vecs(6,:)
            call init_event(pvecs, +1)
            !---#] reinitialize kinematics:
            fr = finite_renormalisation1(real(scale2,ki))
            heli_amp(0) = heli_amp(0) + fr
         end if
         ok = ok .and. my_ok
         amp = amp + heli_amp

         if(debug_nlo_diagrams) then
            write(logfile,'(A33,E24.16,A3)') &
                & "<result kind='nlo-finite' value='", heli_amp(0), "'/>"
            write(logfile,'(A33,E24.16,A3)') &
                & "<result kind='nlo-single' value='", heli_amp(-1), "'/>"
            write(logfile,'(A33,E24.16,A3)') &
                & "<result kind='nlo-double' value='", heli_amp(-2), "'/>"
            if (corrections_are_qcd .and. renorm_gamma5) then
               write(logfile,'(A30,E24.16,A3)') &
                   & "<result kind='fin-ren' value='", fr, "'/>"
            end if
            if(my_ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</helicity>"
         end if
      end if
      if (include_helicity_avg_factor) then
         amp = amp / real(in_helicities, ki)
      end if
      if (include_color_avg_factor) then
         amp = amp / incolors
      end if
      if (include_symmetry_factor) then
         amp = amp / real(symmetry_factor, ki)
      end if
   end function samplitudel1
   !---#] function samplitudel1 :
   !---#[ subroutine ir_subtraction :
   subroutine     ir_subtraction(vecs,scale2,amp)
      use p0_dbaru_hepneg_config, only: &
         & nlo_prefactors
      use p0_dbaru_hepneg_dipoles, only: pi
      use p0_dbaru_hepneg_kinematics, only: &
         & init_event, corrections_are_qcd
      use p0_dbaru_hepneg_model
      implicit none
      real(ki), dimension(6, 4), intent(in) :: vecs
      real(ki), intent(in) :: scale2
      real(ki), dimension(2), intent(out) :: amp
      real(ki), dimension(2) :: heli_amp
      real(ki), dimension(6, 4) :: pvecs
      complex(ki), dimension(numcs,numcs,2) :: oper
      complex(ki), dimension(numcs) :: color_vectorl0, pcolor
      real(ki) :: nlo_coupling

      call init_event(vecs)

      if(corrections_are_qcd) then
         nlo_coupling = 1.0_ki
      else
         nlo_coupling = 1.0_ki
      end if

      oper = insertion_operator(real(scale2,ki), vecs)
      amp(:) = 0.0_ki
      !---#[ reinitialize kinematics:
      pvecs(1,:) = vecs(1,:)
      pvecs(2,:) = vecs(2,:)
      pvecs(3,:) = vecs(3,:)
      pvecs(4,:) = vecs(4,:)
      pvecs(5,:) = vecs(5,:)
      pvecs(6,:) = vecs(6,:)
      call init_event(pvecs, -1)
      !---#] reinitialize kinematics:
      pcolor = amplitude0l0()
      color_vectorl0(1) = pcolor(1)
      heli_amp(1) = square(color_vectorl0, oper(:,:,1))
      heli_amp(2) = square(color_vectorl0, oper(:,:,2))
      amp = amp + heli_amp
      !---#[ reinitialize kinematics:
      pvecs(1,:) = vecs(1,:)
      pvecs(2,:) = vecs(2,:)
      pvecs(3,:) = vecs(3,:)
      pvecs(4,:) = vecs(4,:)
      pvecs(5,:) = vecs(5,:)
      pvecs(6,:) = vecs(6,:)
      call init_event(pvecs, +1)
      !---#] reinitialize kinematics:
      pcolor = amplitude1l0()
      color_vectorl0(1) = pcolor(1)
      heli_amp(1) = square(color_vectorl0, oper(:,:,1))
      heli_amp(2) = square(color_vectorl0, oper(:,:,2))
      amp = amp + heli_amp
      if (include_helicity_avg_factor) then
         amp = amp / real(in_helicities, ki)
      end if
      if (include_color_avg_factor) then
         amp = amp / incolors
      end if
      if (include_symmetry_factor) then
         amp = amp / real(symmetry_factor, ki)
      end if
      select case(nlo_prefactors)
      case(0)
         ! The result is already in its desired form
      case(1)
         amp(:) = amp(:) * nlo_coupling
      case(2)
         amp(:) = amp(:) * nlo_coupling / 8.0_ki / pi / pi
      end select
   end subroutine ir_subtraction
   !---#] subroutine ir_subtraction :
   !---#[ color correlated ME :
   pure subroutine color_correlated_lo(color_vector,res)
      use p0_dbaru_hepneg_color, only: T1T1, &
      & T1T2, &
      & T1T6, &
      & T2T2, &
      & T2T6, &
      & T6T6
      implicit none
      complex(ki), dimension(numcs), intent(in) :: color_vector
      real(ki), dimension(num_legs,num_legs), intent(out) :: res
      res(:,:)=0.0_ki
      res(1,1) = square(color_vector,T1T1)
      res(1,1) = res(1,1)
      res(1,2) = square(color_vector,T1T2)
      res(2,1) = res(1,2)
      res(1,6) = square(color_vector,T1T6)
      res(6,1) = res(1,6)
      res(2,2) = square(color_vector,T2T2)
      res(2,2) = res(2,2)
      res(2,6) = square(color_vector,T2T6)
      res(6,2) = res(2,6)
      res(6,6) = square(color_vector,T6T6)
      res(6,6) = res(6,6)
   end subroutine color_correlated_lo

   subroutine     color_correlated_lo2(vecs,borncc)
      use p0_dbaru_hepneg_kinematics, only: init_event
      implicit none
      real(ki), dimension(num_legs, 4), intent(in) :: vecs
      real(ki), dimension(num_legs,num_legs), intent(out) :: borncc
      real(ki), dimension(num_legs,num_legs) :: borncc_heli
      real(ki), dimension(num_legs, 4) :: pvecs
      complex(ki), dimension(numcs) :: color_vector

      borncc(:,:) = 0.0_ki
      !---#[ reinitialize kinematics:
      pvecs(1,:) = vecs(1,:)
      pvecs(2,:) = vecs(2,:)
      pvecs(3,:) = vecs(3,:)
      pvecs(4,:) = vecs(4,:)
      pvecs(5,:) = vecs(5,:)
      pvecs(6,:) = vecs(6,:)
      call init_event(pvecs, -1)
      !---#] reinitialize kinematics:
      color_vector = amplitude0l0()
      call color_correlated_lo(color_vector,borncc_heli)
      ! The minus is part in the definition according to PowHEG Box.
      ! Since they use it we include it:
      borncc(:,:) = borncc(:,:) - borncc_heli(:,:)
      !---#[ reinitialize kinematics:
      pvecs(1,:) = vecs(1,:)
      pvecs(2,:) = vecs(2,:)
      pvecs(3,:) = vecs(3,:)
      pvecs(4,:) = vecs(4,:)
      pvecs(5,:) = vecs(5,:)
      pvecs(6,:) = vecs(6,:)
      call init_event(pvecs, +1)
      !---#] reinitialize kinematics:
      color_vector = amplitude1l0()
      call color_correlated_lo(color_vector,borncc_heli)
      borncc(:,:) = borncc(:,:) - borncc_heli(:,:)
      if (include_helicity_avg_factor) then
         borncc = borncc / real(in_helicities, ki)
      end if
      if (include_color_avg_factor) then
         borncc = borncc / incolors
      end if
      if (include_symmetry_factor) then
         borncc = borncc / real(symmetry_factor, ki)
      end if
   end subroutine color_correlated_lo2
   !---#] color correlated ME :
   !---#[ spin correlated ME :
   subroutine spin_correlated_lo2(vecs, bornsc)
      use p0_dbaru_hepneg_kinematics
      implicit none
      real(ki), dimension(num_legs, 4), intent(in) :: vecs
      real(ki), dimension(num_legs,4,4) :: bornsc
      real(ki), dimension(num_legs, 4) :: pvecs
      complex(ki), dimension(4,4) :: tens
      complex(ki) :: pp, pm, mp, mm
      complex(ki), dimension(numcs) :: heli_amp0
      complex(ki), dimension(numcs) :: heli_amp1
      complex(ki), dimension(4) :: eps6

      bornsc(:,:,:) = 0.0_ki
      !---#[ Initialize helicity amplitudes :
      !---#[ reinitialize kinematics:
      pvecs(1,:) = vecs(1,:)
      pvecs(2,:) = vecs(2,:)
      pvecs(3,:) = vecs(3,:)
      pvecs(4,:) = vecs(4,:)
      pvecs(5,:) = vecs(5,:)
      pvecs(6,:) = vecs(6,:)
      call init_event(pvecs, -1)
      !---#] reinitialize kinematics:
      heli_amp0 = amplitude0l0()
      !---#[ reinitialize kinematics:
      pvecs(1,:) = vecs(1,:)
      pvecs(2,:) = vecs(2,:)
      pvecs(3,:) = vecs(3,:)
      pvecs(4,:) = vecs(4,:)
      pvecs(5,:) = vecs(5,:)
      pvecs(6,:) = vecs(6,:)
      call init_event(pvecs, +1)
      !---#] reinitialize kinematics:
      heli_amp1 = amplitude1l0()
      !---#] Initialize helicity amplitudes :
      !---#[ Initialize polarization vectors :
      eps6 = conjg(spvak2k6/Spaa(k2,k6)/sqrt2)
      !---#] Initialize polarization vectors :
      ! Note: By omitting the imaginary parts we lose a term:
      !   Imag(B_j(mu,nu)) = i_ * e_(k_j, mu, q_j, nu) * |Born|^2
      ! where q_j is the reference momentum chosen for the paticle
      ! of momentum k_j. This term should, however not be phenomenologically
      ! relevant.
      !---#[ particle 6 :
      pp  = 0.0_ki &
      &          + square_0l_0l_sc(heli_amp1,heli_amp1)
      pm  = 0.0_ki &
      &          + square_0l_0l_sc(heli_amp1,heli_amp0)
      mp  = 0.0_ki &
      &          + square_0l_0l_sc(heli_amp0,heli_amp1)
      mm  = 0.0_ki &
      &          + square_0l_0l_sc(heli_amp0,heli_amp0)

      call construct_polarization_tensor(conjg(eps6),eps6,tens)
      bornsc(6,:,:) = bornsc(6,:,:) + real(tens(:,:) * pp, ki)
      call construct_polarization_tensor(conjg(eps6),conjg(eps6),tens)
      bornsc(6,:,:) = bornsc(6,:,:) + real(tens(:,:) * pm, ki)
      call construct_polarization_tensor(eps6,eps6,tens)
      bornsc(6,:,:) = bornsc(6,:,:) + real(tens(:,:) * mp, ki)
      call construct_polarization_tensor(eps6,conjg(eps6),tens)
      bornsc(6,:,:) = bornsc(6,:,:) + real(tens(:,:) * mm, ki)
      !---#] particle 6 :
      if (include_helicity_avg_factor) then
         bornsc = bornsc / real(in_helicities, ki)
      end if
      if (include_color_avg_factor) then
         bornsc = bornsc / incolors
      end if
      if (include_symmetry_factor) then
         bornsc = bornsc / real(symmetry_factor, ki)
      end if
   end subroutine spin_correlated_lo2
   !---#] spin correlated ME :
   !---#[ construct polarisation tensor :
   pure subroutine construct_polarization_tensor(eps1, eps2, tens)
      implicit none
      complex(ki), dimension(0:3), intent(in) :: eps1, eps2
      complex(ki), dimension(0:3,0:3), intent(out) :: tens

      integer :: mu, nu

      do mu = 0,3
         do nu = 0, 3
            tens(mu,nu) = eps1(mu) * eps2(nu)
         end do
      end do
   end  subroutine construct_polarization_tensor
   !---#] construct polarisation tensor :
   pure function square_0l_0l_sc(color_vector1, color_vector2) result(amp)
      use p0_dbaru_hepneg_color, only: cmat => CC
      implicit none
      complex(ki), dimension(numcs), intent(in) :: color_vector1, color_vector2
      complex(ki) :: amp
      complex(ki), dimension(numcs) :: v1, v2

      v1 = matmul(cmat, color_vector2)
      v2 = conjg(color_vector1)
      amp = sum(v1(:) * v2(:))
   end function  square_0l_0l_sc

end module p0_dbaru_hepneg_matrix
