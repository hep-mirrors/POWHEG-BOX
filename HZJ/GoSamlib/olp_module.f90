module     olp_module
   implicit none
   private
   public :: OLP_Start, OLP_EvalSubProcess, OLP_Finalize, OLP_Option

contains

   subroutine     OLP_Start(contract_file_name,ierr,stage,rndseed) &
   & bind(C,name="olp_start_")
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_matrix, only: p8_cbarc_hepemg_initgolem => initgolem
      use p8_cbarc_hepemg_config, only: p8_cbarc_hepemg_PSP_rescue => PSP_rescue, &
           & p8_cbarc_hepemg_PSP_verbosity => PSP_verbosity, &
           & p8_cbarc_hepemg_PSP_chk_threshold1 => PSP_chk_threshold1, &
           & p8_cbarc_hepemg_PSP_chk_threshold2 => PSP_chk_threshold2, &
           & p8_cbarc_hepemg_PSP_chk_kfactor => PSP_chk_kfactor
      use p12_sbars_hepemg_matrix, only: p12_sbars_hepemg_initgolem => initgolem
      use p12_sbars_hepemg_config, only: p12_sbars_hepemg_PSP_rescue => PSP_rescue, &
           & p12_sbars_hepemg_PSP_verbosity => PSP_verbosity, &
           & p12_sbars_hepemg_PSP_chk_threshold1 => PSP_chk_threshold1, &
           & p12_sbars_hepemg_PSP_chk_threshold2 => PSP_chk_threshold2, &
           & p12_sbars_hepemg_PSP_chk_kfactor => PSP_chk_kfactor
      use p0_dbard_hepemg_matrix, only: p0_dbard_hepemg_initgolem => initgolem
      use p0_dbard_hepemg_config, only: p0_dbard_hepemg_PSP_rescue => PSP_rescue, &
           & p0_dbard_hepemg_PSP_verbosity => PSP_verbosity, &
           & p0_dbard_hepemg_PSP_chk_threshold1 => PSP_chk_threshold1, &
           & p0_dbard_hepemg_PSP_chk_threshold2 => PSP_chk_threshold2, &
           & p0_dbard_hepemg_PSP_chk_kfactor => PSP_chk_kfactor
      use p16_bbarb_hepemg_matrix, only: p16_bbarb_hepemg_initgolem => initgolem
      use p16_bbarb_hepemg_config, only: p16_bbarb_hepemg_PSP_rescue => PSP_rescue, &
           & p16_bbarb_hepemg_PSP_verbosity => PSP_verbosity, &
           & p16_bbarb_hepemg_PSP_chk_threshold1 => PSP_chk_threshold1, &
           & p16_bbarb_hepemg_PSP_chk_threshold2 => PSP_chk_threshold2, &
           & p16_bbarb_hepemg_PSP_chk_kfactor => PSP_chk_kfactor
      use p4_ubaru_hepemg_matrix, only: p4_ubaru_hepemg_initgolem => initgolem
      use p4_ubaru_hepemg_config, only: p4_ubaru_hepemg_PSP_rescue => PSP_rescue, &
           & p4_ubaru_hepemg_PSP_verbosity => PSP_verbosity, &
           & p4_ubaru_hepemg_PSP_chk_threshold1 => PSP_chk_threshold1, &
           & p4_ubaru_hepemg_PSP_chk_threshold2 => PSP_chk_threshold2, &
           & p4_ubaru_hepemg_PSP_chk_kfactor => PSP_chk_kfactor
      implicit none
      character(kind=c_char,len=1), intent(in) :: contract_file_name
      integer(kind=c_int), intent(out) :: ierr
      integer(kind=c_int), intent(in) :: stage, rndseed
      interface
         function strlen(s) bind(C,name='strlen')
            use, intrinsic :: iso_c_binding
            implicit none
            character(kind=c_char,len=1), intent(in) :: s
            integer(kind=c_int) :: strlen
         end function strlen
      end interface

      integer :: l, ferr
      character(len=128) :: line_buf
      character(len=9) :: kw
      integer :: PSP_verbosity, PSP_chk_threshold1, PSP_chk_threshold2
      logical :: PSP_rescue

      ierr = 1
      l = strlen(contract_file_name)

      open(unit=21, file=contract_file_name(1:l), &
          & status='old', action='read', iostat=ferr)

      if (ferr .ne. 0) then
         write(7,*) "In OLP_Start: ", contract_file_name(1:l), " not found!"
         ierr = -1
      end if

      do while (ferr .eq. 0)
         read(unit=21,fmt='(A128)',iostat=ferr) line_buf
         if (ferr .ne. 0) exit
         line_buf = adjustl(line_buf)
         kw = line_buf(1:9)
         do
            l = scan(kw, "DEFILMO")
            if (l .eq. 0) exit
            kw(l:l) = achar(ichar(kw(l:l)) - ichar('A') + ichar('a'))
         end do
         if (kw .eq. "modelfile") then
            line_buf = adjustl(line_buf(10:128))
            l = scan(line_buf, "|") - 1
            if(l .lt. 1) l = len(line_buf)
            l = len_trim(line_buf(1:l))
            exit
         end if
      end do

      close(unit=21)

      if (ierr .eq. 1) then
         call read_slha_file(line_buf(1:l))
      end if
      if(stage.lt.0) then
         call p8_cbarc_hepemg_initgolem(.true.)
         call p12_sbars_hepemg_initgolem(.false.)
         call p0_dbard_hepemg_initgolem(.false.)
         call p16_bbarb_hepemg_initgolem(.false.)
         call p4_ubaru_hepemg_initgolem(.false.)
      else
         call p8_cbarc_hepemg_initgolem(.true.,stage,rndseed)
         call p12_sbars_hepemg_initgolem(.false.,stage,rndseed)
         call p0_dbard_hepemg_initgolem(.false.,stage,rndseed)
         call p16_bbarb_hepemg_initgolem(.false.,stage,rndseed)
         call p4_ubaru_hepemg_initgolem(.false.,stage,rndseed)
      end if


      ! Uncomment to change rescue system setting on all suprocesses
      ! PSP_rescue = .true.
      ! PSP_verbosity = 1
      ! PSP_chk_threshold1 = 3
      ! PSP_chk_threshold2 = 4
      ! PSP_chk_kfactor = -1
      ! p8_cbarc_hepemg_PSP_rescue = PSP_rescue
      ! p8_cbarc_hepemg_PSP_verbosity =  PSP_verbosity
      ! p8_cbarc_hepemg_PSP_chk_threshold1 = PSP_chk_threshold1
      ! p8_cbarc_hepemg_PSP_chk_threshold2 = PSP_chk_threshold2
      ! p8_cbarc_hepemg_PSP_chk_kfactor = PSP_chk_kfactor
      ! p12_sbars_hepemg_PSP_rescue = PSP_rescue
      ! p12_sbars_hepemg_PSP_verbosity =  PSP_verbosity
      ! p12_sbars_hepemg_PSP_chk_threshold1 = PSP_chk_threshold1
      ! p12_sbars_hepemg_PSP_chk_threshold2 = PSP_chk_threshold2
      ! p12_sbars_hepemg_PSP_chk_kfactor = PSP_chk_kfactor
      ! p0_dbard_hepemg_PSP_rescue = PSP_rescue
      ! p0_dbard_hepemg_PSP_verbosity =  PSP_verbosity
      ! p0_dbard_hepemg_PSP_chk_threshold1 = PSP_chk_threshold1
      ! p0_dbard_hepemg_PSP_chk_threshold2 = PSP_chk_threshold2
      ! p0_dbard_hepemg_PSP_chk_kfactor = PSP_chk_kfactor
      ! p16_bbarb_hepemg_PSP_rescue = PSP_rescue
      ! p16_bbarb_hepemg_PSP_verbosity =  PSP_verbosity
      ! p16_bbarb_hepemg_PSP_chk_threshold1 = PSP_chk_threshold1
      ! p16_bbarb_hepemg_PSP_chk_threshold2 = PSP_chk_threshold2
      ! p16_bbarb_hepemg_PSP_chk_kfactor = PSP_chk_kfactor
      ! p4_ubaru_hepemg_PSP_rescue = PSP_rescue
      ! p4_ubaru_hepemg_PSP_verbosity =  PSP_verbosity
      ! p4_ubaru_hepemg_PSP_chk_threshold1 = PSP_chk_threshold1
      ! p4_ubaru_hepemg_PSP_chk_threshold2 = PSP_chk_threshold2
      ! p4_ubaru_hepemg_PSP_chk_kfactor = PSP_chk_kfactor

   end subroutine OLP_Start

   subroutine     OLP_EvalSubProcess(label, momenta, mu, parameters, res) &
   & bind(C,name="olp_evalsubprocess_")
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_model,  only: p8_mH => mH
      use p12_sbars_hepemg_model, only: p12_mH => mH
      use p0_dbard_hepemg_model,  only: p0_mH => mH
      use p16_bbarb_hepemg_model, only: p16_mH => mH
      use p4_ubaru_hepemg_model,  only: p4_mH => mH

      implicit none
      integer(kind=c_int), intent(in) :: label
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(50), intent(in) :: momenta
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=c_double) :: alpha_s, hmass
      real(kind=c_double), parameter :: one_over_2pi = 0.15915494309189533577d0

      hmass = sqrt(momenta(11)**2 - momenta(12)**2 - momenta(13)**2 - momenta(14)**2)

      alpha_s = parameters(1)

      p8_mH = hmass
      p12_mH = hmass
      p0_mH = hmass
      p16_mH = hmass
      p4_mH = hmass

      select case(label)
      case(0)
              call eval8(momenta(1:30), mu, parameters, res)
      case(1)
              call eval9(momenta(1:30), mu, parameters, res)
      case(2)
              call eval10(momenta(1:30), mu, parameters, res)
      case(3)
              call eval11(momenta(1:30), mu, parameters, res)
      case(4)
              call eval24(momenta(1:30), mu, parameters, res)
      case(5)
              call eval25(momenta(1:30), mu, parameters, res)
      case(6)
              call eval12(momenta(1:30), mu, parameters, res)
      case(7)
              call eval13(momenta(1:30), mu, parameters, res)
      case(8)
              call eval14(momenta(1:30), mu, parameters, res)
      case(9)
              call eval15(momenta(1:30), mu, parameters, res)
      case(10)
              call eval26(momenta(1:30), mu, parameters, res)
      case(11)
              call eval27(momenta(1:30), mu, parameters, res)
      case(12)
              call eval0(momenta(1:30), mu, parameters, res)
      case(13)
              call eval1(momenta(1:30), mu, parameters, res)
      case(14)
              call eval2(momenta(1:30), mu, parameters, res)
      case(15)
              call eval3(momenta(1:30), mu, parameters, res)
      case(16)
              call eval20(momenta(1:30), mu, parameters, res)
      case(17)
              call eval21(momenta(1:30), mu, parameters, res)
      case(18)
              call eval16(momenta(1:30), mu, parameters, res)
      case(19)
              call eval17(momenta(1:30), mu, parameters, res)
      case(20)
              call eval18(momenta(1:30), mu, parameters, res)
      case(21)
              call eval19(momenta(1:30), mu, parameters, res)
      case(22)
              call eval28(momenta(1:30), mu, parameters, res)
      case(23)
              call eval29(momenta(1:30), mu, parameters, res)
      case(24)
              call eval4(momenta(1:30), mu, parameters, res)
      case(25)
              call eval5(momenta(1:30), mu, parameters, res)
      case(26)
              call eval6(momenta(1:30), mu, parameters, res)
      case(27)
              call eval7(momenta(1:30), mu, parameters, res)
      case(28)
              call eval22(momenta(1:30), mu, parameters, res)
      case(29)
              call eval23(momenta(1:30), mu, parameters, res)
      case default
         res(:) = 0.0d0
      end select

      res(1:3) = alpha_s * one_over_2pi * res(1:3)
   end subroutine OLP_EvalSubProcess

   subroutine     OLP_Finalize() &
   & bind(C,name="olp_finalize_")
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_matrix, only: p8_cbarc_hepemg_exitgolem => exitgolem
      use p12_sbars_hepemg_matrix, only: p12_sbars_hepemg_exitgolem => exitgolem
      use p0_dbard_hepemg_matrix, only: p0_dbard_hepemg_exitgolem => exitgolem
      use p16_bbarb_hepemg_matrix, only: p16_bbarb_hepemg_exitgolem => exitgolem
      use p4_ubaru_hepemg_matrix, only: p4_ubaru_hepemg_exitgolem => exitgolem
      implicit none
      call p8_cbarc_hepemg_exitgolem(.false.)
      call p12_sbars_hepemg_exitgolem(.false.)
      call p0_dbard_hepemg_exitgolem(.false.)
      call p16_bbarb_hepemg_exitgolem(.false.)
      call p4_ubaru_hepemg_exitgolem(.true.)
   end subroutine OLP_Finalize

   subroutine     OLP_Option(line,stat) &
   & bind(C,name="olp_option_")
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_model, only: p8_cbarc_hepemg_parseline => parseline
      use p12_sbars_hepemg_model, only: p12_sbars_hepemg_parseline => parseline
      use p0_dbard_hepemg_model, only: p0_dbard_hepemg_parseline => parseline
      use p16_bbarb_hepemg_model, only: p16_bbarb_hepemg_parseline => parseline
      use p4_ubaru_hepemg_model, only: p4_ubaru_hepemg_parseline => parseline
      implicit none
      character(kind=c_char,len=1), intent(in) :: line
      integer(kind=c_int), intent(out) :: stat
      integer :: l, ios

      interface
         function strlen(s) bind(C,name='strlen')
            use, intrinsic :: iso_c_binding
            implicit none
            character(kind=c_char,len=1), intent(in) :: s
            integer(kind=c_int) :: strlen
         end function strlen
      end interface

      l = strlen(line)
      call p8_cbarc_hepemg_parseline(line(1:l),ios)
      if (ios .ne. 0) then
         stat = 0
         return
      end if
      call p12_sbars_hepemg_parseline(line(1:l),ios)
      if (ios .ne. 0) then
         stat = 0
         return
      end if
      call p0_dbard_hepemg_parseline(line(1:l),ios)
      if (ios .ne. 0) then
         stat = 0
         return
      end if
      call p16_bbarb_hepemg_parseline(line(1:l),ios)
      if (ios .ne. 0) then
         stat = 0
         return
      end if
      call p4_ubaru_hepemg_parseline(line(1:l),ios)
      if (ios .ne. 0) then
         stat = 0
         return
      end if
      stat = 1
   end subroutine OLP_Option
   !---#[ init_event_parameters :
   subroutine     init_event_parameters(sp, parameters)
      use, intrinsic :: iso_c_binding
      implicit none
      integer, intent(in) :: sp
      real(kind=c_double), dimension(10), intent(in) :: parameters
      !
      ! User hook for propagating scale dependent parameters to the
      ! model parameters in the subprocesses.
      !
      ! sp specifies the subprocess
      !
   end subroutine init_event_parameters
   !---#] init_event_parameters :

   !---#[ subroutine eval8 :
   subroutine     eval8(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_config, only: ki
      use p8_cbarc_hepemg_model, only: parseline
      use p8_cbarc_hepemg_matrix, only: samplitude
      use p8_cbarc_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(8, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval8
   !---#] subroutine eval8 :
   !---#[ subroutine eval9 :
   subroutine     eval9(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_config, only: ki
      use p8_cbarc_hepemg_model, only: parseline
      use p9_cbarg_hepemcbar_matrix, only: samplitude
      use p8_cbarc_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(9, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval9
   !---#] subroutine eval9 :
   !---#[ subroutine eval10 :
   subroutine     eval10(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_config, only: ki
      use p8_cbarc_hepemg_model, only: parseline
      use p10_ccbar_hepemg_matrix, only: samplitude
      use p8_cbarc_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(10, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval10
   !---#] subroutine eval10 :
   !---#[ subroutine eval11 :
   subroutine     eval11(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_config, only: ki
      use p8_cbarc_hepemg_model, only: parseline
      use p11_cg_hepemc_matrix, only: samplitude
      use p8_cbarc_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(11, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval11
   !---#] subroutine eval11 :
   !---#[ subroutine eval24 :
   subroutine     eval24(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_config, only: ki
      use p8_cbarc_hepemg_model, only: parseline
      use p24_gcbar_hepemcbar_matrix, only: samplitude
      use p8_cbarc_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(24, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval24
   !---#] subroutine eval24 :
   !---#[ subroutine eval25 :
   subroutine     eval25(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p8_cbarc_hepemg_config, only: ki
      use p8_cbarc_hepemg_model, only: parseline
      use p25_gc_hepemc_matrix, only: samplitude
      use p8_cbarc_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(25, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval25
   !---#] subroutine eval25 :
   !---#[ subroutine eval12 :
   subroutine     eval12(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p12_sbars_hepemg_config, only: ki
      use p12_sbars_hepemg_model, only: parseline
      use p12_sbars_hepemg_matrix, only: samplitude
      use p12_sbars_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(12, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval12
   !---#] subroutine eval12 :
   !---#[ subroutine eval13 :
   subroutine     eval13(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p12_sbars_hepemg_config, only: ki
      use p12_sbars_hepemg_model, only: parseline
      use p13_sbarg_hepemsbar_matrix, only: samplitude
      use p12_sbars_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(13, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval13
   !---#] subroutine eval13 :
   !---#[ subroutine eval14 :
   subroutine     eval14(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p12_sbars_hepemg_config, only: ki
      use p12_sbars_hepemg_model, only: parseline
      use p14_ssbar_hepemg_matrix, only: samplitude
      use p12_sbars_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(14, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval14
   !---#] subroutine eval14 :
   !---#[ subroutine eval15 :
   subroutine     eval15(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p12_sbars_hepemg_config, only: ki
      use p12_sbars_hepemg_model, only: parseline
      use p15_sg_hepems_matrix, only: samplitude
      use p12_sbars_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(15, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval15
   !---#] subroutine eval15 :
   !---#[ subroutine eval26 :
   subroutine     eval26(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p12_sbars_hepemg_config, only: ki
      use p12_sbars_hepemg_model, only: parseline
      use p26_gsbar_hepemsbar_matrix, only: samplitude
      use p12_sbars_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(26, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval26
   !---#] subroutine eval26 :
   !---#[ subroutine eval27 :
   subroutine     eval27(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p12_sbars_hepemg_config, only: ki
      use p12_sbars_hepemg_model, only: parseline
      use p27_gs_hepems_matrix, only: samplitude
      use p12_sbars_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(27, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval27
   !---#] subroutine eval27 :
   !---#[ subroutine eval0 :
   subroutine     eval0(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p0_dbard_hepemg_config, only: ki
      use p0_dbard_hepemg_model, only: parseline
      use p0_dbard_hepemg_matrix, only: samplitude
      use p0_dbard_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(0, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval0
   !---#] subroutine eval0 :
   !---#[ subroutine eval1 :
   subroutine     eval1(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p0_dbard_hepemg_config, only: ki
      use p0_dbard_hepemg_model, only: parseline
      use p1_dbarg_hepemdbar_matrix, only: samplitude
      use p0_dbard_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(1, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval1
   !---#] subroutine eval1 :
   !---#[ subroutine eval2 :
   subroutine     eval2(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p0_dbard_hepemg_config, only: ki
      use p0_dbard_hepemg_model, only: parseline
      use p2_ddbar_hepemg_matrix, only: samplitude
      use p0_dbard_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(2, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval2
   !---#] subroutine eval2 :
   !---#[ subroutine eval3 :
   subroutine     eval3(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p0_dbard_hepemg_config, only: ki
      use p0_dbard_hepemg_model, only: parseline
      use p3_dg_hepemd_matrix, only: samplitude
      use p0_dbard_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(3, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval3
   !---#] subroutine eval3 :
   !---#[ subroutine eval20 :
   subroutine     eval20(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p0_dbard_hepemg_config, only: ki
      use p0_dbard_hepemg_model, only: parseline
      use p20_gdbar_hepemdbar_matrix, only: samplitude
      use p0_dbard_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(20, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval20
   !---#] subroutine eval20 :
   !---#[ subroutine eval21 :
   subroutine     eval21(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p0_dbard_hepemg_config, only: ki
      use p0_dbard_hepemg_model, only: parseline
      use p21_gd_hepemd_matrix, only: samplitude
      use p0_dbard_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(21, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval21
   !---#] subroutine eval21 :
   !---#[ subroutine eval16 :
   subroutine     eval16(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p16_bbarb_hepemg_config, only: ki
      use p16_bbarb_hepemg_model, only: parseline
      use p16_bbarb_hepemg_matrix, only: samplitude
      use p16_bbarb_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(16, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval16
   !---#] subroutine eval16 :
   !---#[ subroutine eval17 :
   subroutine     eval17(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p16_bbarb_hepemg_config, only: ki
      use p16_bbarb_hepemg_model, only: parseline
      use p17_bbarg_hepembbar_matrix, only: samplitude
      use p16_bbarb_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(17, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval17
   !---#] subroutine eval17 :
   !---#[ subroutine eval18 :
   subroutine     eval18(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p16_bbarb_hepemg_config, only: ki
      use p16_bbarb_hepemg_model, only: parseline
      use p18_bbbar_hepemg_matrix, only: samplitude
      use p16_bbarb_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(18, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval18
   !---#] subroutine eval18 :
   !---#[ subroutine eval19 :
   subroutine     eval19(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p16_bbarb_hepemg_config, only: ki
      use p16_bbarb_hepemg_model, only: parseline
      use p19_bg_hepemb_matrix, only: samplitude
      use p16_bbarb_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(19, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval19
   !---#] subroutine eval19 :
   !---#[ subroutine eval28 :
   subroutine     eval28(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p16_bbarb_hepemg_config, only: ki
      use p16_bbarb_hepemg_model, only: parseline
      use p28_gbbar_hepembbar_matrix, only: samplitude
      use p16_bbarb_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(28, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval28
   !---#] subroutine eval28 :
   !---#[ subroutine eval29 :
   subroutine     eval29(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p16_bbarb_hepemg_config, only: ki
      use p16_bbarb_hepemg_model, only: parseline
      use p29_gb_hepemb_matrix, only: samplitude
      use p16_bbarb_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(29, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval29
   !---#] subroutine eval29 :
   !---#[ subroutine eval4 :
   subroutine     eval4(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p4_ubaru_hepemg_config, only: ki
      use p4_ubaru_hepemg_model, only: parseline
      use p4_ubaru_hepemg_matrix, only: samplitude
      use p4_ubaru_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(4, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval4
   !---#] subroutine eval4 :
   !---#[ subroutine eval5 :
   subroutine     eval5(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p4_ubaru_hepemg_config, only: ki
      use p4_ubaru_hepemg_model, only: parseline
      use p5_ubarg_hepemubar_matrix, only: samplitude
      use p4_ubaru_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(5, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval5
   !---#] subroutine eval5 :
   !---#[ subroutine eval6 :
   subroutine     eval6(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p4_ubaru_hepemg_config, only: ki
      use p4_ubaru_hepemg_model, only: parseline
      use p6_uubar_hepemg_matrix, only: samplitude
      use p4_ubaru_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(6, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval6
   !---#] subroutine eval6 :
   !---#[ subroutine eval7 :
   subroutine     eval7(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p4_ubaru_hepemg_config, only: ki
      use p4_ubaru_hepemg_model, only: parseline
      use p7_ug_hepemu_matrix, only: samplitude
      use p4_ubaru_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(7, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval7
   !---#] subroutine eval7 :
   !---#[ subroutine eval22 :
   subroutine     eval22(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p4_ubaru_hepemg_config, only: ki
      use p4_ubaru_hepemg_model, only: parseline
      use p22_gubar_hepemubar_matrix, only: samplitude
      use p4_ubaru_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(22, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval22
   !---#] subroutine eval22 :
   !---#[ subroutine eval23 :
   subroutine     eval23(momenta, mu, parameters, res)
      use, intrinsic :: iso_c_binding
      use p4_ubaru_hepemg_config, only: ki
      use p4_ubaru_hepemg_model, only: parseline
      use p23_gu_hepemu_matrix, only: samplitude
      use p4_ubaru_hepemg_groups, only: tear_down_golem95
      implicit none
      real(kind=c_double), dimension(30), intent(in) :: momenta
      real(kind=c_double), intent(in) :: mu
      real(kind=c_double), dimension(10), intent(in) :: parameters
      real(kind=c_double), dimension(4), intent(out) :: res

      real(kind=ki), dimension(6,4) :: vecs
      real(kind=ki), dimension(4) :: amp
      logical :: ok

      call init_event_parameters(23, parameters)

      vecs(:,1) = real(momenta(1::5),ki)
      vecs(:,2) = real(momenta(2::5),ki)
      vecs(:,3) = real(momenta(3::5),ki)
      vecs(:,4) = real(momenta(4::5),ki)

      call samplitude(vecs, mu*mu, amp, ok)
      call tear_down_golem95()

      if (ok) then
         !
      else
         !
      end if

      res(1) = real(amp(4), c_double)
      res(2) = real(amp(3), c_double)
      res(3) = real(amp(2), c_double)
      res(4) = real(amp(1), c_double)
   end subroutine eval23
   !---#] subroutine eval23 :

   subroutine     read_slha_file(line)
      use p8_cbarc_hepemg_model, only: p8_cbarc_hepemg_read_slha => read_slha
      use p12_sbars_hepemg_model, only: p12_sbars_hepemg_read_slha => read_slha
      use p0_dbard_hepemg_model, only: p0_dbard_hepemg_read_slha => read_slha
      use p16_bbarb_hepemg_model, only: p16_bbarb_hepemg_read_slha => read_slha
      use p4_ubaru_hepemg_model, only: p4_ubaru_hepemg_read_slha => read_slha
      implicit none
      character(len=*), intent(in) :: line
      character(len=512) :: file_name
      integer :: ierr

      call unescape_file_name(line, file_name)
      open(unit=27,file=file_name,status='old',iostat=ierr)
      if(ierr.ne.0) then
         print*, "Could not find SLHA model file"
      else
         call p8_cbarc_hepemg_read_slha(27)
         rewind(unit=27)
         call p12_sbars_hepemg_read_slha(27)
         rewind(unit=27)
         call p0_dbard_hepemg_read_slha(27)
         rewind(unit=27)
         call p16_bbarb_hepemg_read_slha(27)
         rewind(unit=27)
         call p4_ubaru_hepemg_read_slha(27)
         close(27)
      end if
   end subroutine read_slha_file

   subroutine     unescape_file_name(source, dest)
      implicit none
      character(len=*), intent(in) :: source
      character(len=512), intent(out) :: dest
      integer :: is, id, l, hex, hexdigit, hexpos
      character(len=512) :: buf
      logical :: special

      is = scan(source, "|")

      if (is > 1) then
         buf = trim(source(1:is-1))
      else
         buf = trim(source)
      end if

      l = len(buf)
      id = 1
      special = .false.
      hexpos = 0
      if (buf(1:1) .eq. '"') then
         ! double quoted string
         do is = 2, l - 1
            if (special) then
               ! after a backslash or in \x.. escape
               if (hexpos == 1 .or. hexpos == 2) then
                  ! interpret hex digit
                  if ("0" .le. buf(is:is) .and. buf(is:is) .le. "9") then
                     hexdigit = ichar(buf(is:is)) - ichar("0")
                  elseif ("A" .le. buf(is:is) .and. buf(is:is) .le. "F") then
                     hexdigit = ichar(buf(is:is)) - ichar("A") + 10
                  elseif ("a" .le. buf(is:is) .and. buf(is:is) .le. "f") then
                     hexdigit = ichar(buf(is:is)) - ichar("a") + 10
                  else
                     print*, "Invalid hex escape sequence in file name"
                     stop
                  end if

                  if (hexpos == 1) then
                     hex = 16 * hexdigit
                     hexpos = 2
                  else
                     hex = hex + hexdigit
                     hexpos = 0
                     special = .false.
                     dest(id:id) = achar(hex)
                     id = id + 1
                  end if
               elseif (buf(is:is) .eq. "n") then
                  dest(id:id) = achar(10)
                  id = id + 1
                  special = .false.
               elseif (buf(is:is) .eq. "r") then
                  dest(id:id) = achar(13)
                  id = id + 1
                  special = .false.
               elseif (buf(is:is) .eq. "f") then
                  dest(id:id) = achar(12)
                  id = id + 1
                  special = .false.
               elseif (buf(is:is) .eq. "t") then
                  dest(id:id) = achar(9)
                  id = id + 1
                  special = .false.
               elseif (buf(is:is) .eq. "x") then
                  hexpos = 1
               else
                  dest(id:id) = buf(is:is)
                  id = id + 1
                  special = .false.
               end if
            else
               if(buf(is:is) .eq. "\") then
                  special = .true.
               else
                  dest(id:id) = source(is:is)
                  id = id + 1
               end if
            end if
         end do
      elseif (buf(1:1) .eq. '"') then
         ! single quoted string
         do is = 2, l - 1
            if (special) then
               dest(id:id) = buf(is:is)
               id = id + 1
               special = .false.
            elseif (buf(is:is) .eq. "'") then
               special = .true.
            else
               dest(id:id) = buf(is:is)
               id = id + 1
            end if
         end do
      else
         ! assume backslash escaped string
         do is = 1, l
            if (special) then
               dest(id:id) = buf(is:is)
               id = id + 1
               special = .false.
            elseif (buf(is:is) .eq. "\") then
               special = .true.
            else
               dest(id:id) = buf(is:is)
               id = id + 1
            end if
         end do
      end if
   end subroutine unescape_file_name
end module olp_module

