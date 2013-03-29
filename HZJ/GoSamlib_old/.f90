program test
   use p12_sbars_hepemg_config, only: ki, logfile
   use p12_sbars_hepemg_kinematics, only: dotproduct
   use p12_sbars_hepemg_model, only: parse
   use p12_sbars_hepemg_matrix, only: samplitude, &
     & initgolem, ir_subtraction
   use p12_sbars_hepemg_color, only: numcs, CA
   use p12_sbars_hepemg_rambo, only: ramb

   implicit none

   integer :: NEVT = 1

   integer :: ievt, ierr
   real(ki), dimension(6, 4) :: vecs
   real(ki) :: scale2
   real(ki), dimension(0:3) :: amp
   real(ki), dimension(2:3) :: irp
   real :: t1, t2

   open(unit=logfile,status='unknown',action='write',file='debug.xml')

   open(unit=10,status='old',action='read',file='param.dat',iostat=ierr)
   if(ierr .eq. 0) then
      call parse(10)
      close(unit=10)
   else
      print*, "No file 'param.dat' found. Using defaults"
   end if

   call initgolem()

   call random_seed

   call cpu_time(t1)
   do ievt = 1, NEVT
      call ramb(5.0E+02_ki**2, vecs)

      scale2 = 2.0_ki * dotproduct(vecs(1,:), vecs(2,:))

      call samplitude(vecs, scale2, amp)
      call ir_subtraction(vecs, scale2, irp)
      if(ievt.eq.NEVT) then
         write(*,'(A1,1x,A17,1x,G23.16)') "#", "LO:", amp(0)
         write(*,'(A1,1x,A17,1x,G23.16)') "#", "NLO, finite part:", &
             &    amp(1)/amp(0)
         write(*,'(A1,1x,A17,1x,G23.16)') "#", "NLO, single pole:", &
             &    amp(2)/amp(0)
         write(*,'(A1,1x,A17,1x,G23.16)') "#", "NLO, double pole:", &
             &    amp(3)/amp(0)
      
      
         write(*,'(A1,1x,A17,1x,G23.16)') "#", "IR,  single pole:", &
            & irp(2)/amp(0)
         write(*,'(A1,1x,A17,1x,G23.16)') "#", "IR,  double pole:", &
            & irp(3)/amp(0)
      
      end if
   end do
   call cpu_time(t2)
   write(*,'(A1,1x,A17,1x,F23.3)') "#", "Time/Event [ms]:", &
      & 1.0E+03 * (t2 - t1) / real(NEVT)

   close(logfile)
end program test
