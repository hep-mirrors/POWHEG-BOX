module msamurai
   use precision
   use constants
   use options
   use ncuts
   use notfirst
   use ltest
   use madds
   use mgetqs
   use mgetbase
   use mrestore
   use mgetc5
   use mgetc4
   use mgetc3
   use mgetc2
   use mgetc1
   use mtests
   use mtens,  only: tensor_reconstruction, numetens
   implicit none

   private

   interface samurai
      module procedure samurai_rm
      module procedure samurai_cm
   end interface samurai

   interface InitDenominators
      module procedure InitDenominators_rm
      module procedure InitDenominators_cm
   end interface InitDenominators

   public :: initsamurai, exitsamurai, InitDenominators, samurai
   public :: samurai_cm, samurai_rm

contains

   !---#[ subroutine initsamurai:
   subroutine initsamurai(ameth, asca, averb, atest, aresc)
          use avh_olo, only: olo_onshell
      implicit none
      integer, intent(in) :: asca, averb, atest
      character(len=4), intent(in) :: ameth
      integer, intent(in), optional :: aresc

      call banner


      if (ameth.eq.'diag') then
         imeth = ameth
         meth_is_tree = .false.
         meth_is_diag = .true.
      elseif (ameth.eq.'tree') then
         imeth = ameth
         meth_is_tree = .true.
         meth_is_diag = .false.
      else
         write(6,*) 'incompatible value for imeth'
         write(6,*) 'imeth =','ameth'
         stop
      endif

      if ((asca.ge.1).and.(asca.le.4)) then
         isca = asca
      else
         write(6,*) 'incompatible value for isca'
         write(6,*) 'isca =',asca
         stop
      endif

      if ((averb.ge.0).or.(averb.le.3)) then
         verbosity = averb
      else
         write(6,*) 'incompatible value for verbosity'
         write(6,*) 'verbosity =',averb
         stop
      endif

      if ((atest.ge.0).or.(atest.le.3)) then
         itest = atest
      else
         write(6,*) 'incompatible value for itest'
         write(6,*) 'itest =',atest
         stop
      endif

      if (present(aresc)) then
       if ((0.le.aresc).and.(aresc.le.3)) then
       iresc = aresc
       else
         write(6,*) 'incompatible value for iresc'
         write(6,*) 'iresc =',aresc
         stop
       endif
      else
       iresc=0
      endif

      if (imeth.eq.'tree') itest=2

      if (isca .eq. 1) then
             call qlinit
      elseif (isca .eq. 2) then
             call olo_onshell(1.d-8)
      elseif (isca .eq. 4) then
!AC!         call ltini
      end if

 901  format(a40,a4,a10,I1,a15,I1,a11,I1)

      if (verbosity.gt.0) then
         if (notfirstp.eqv.(.false.)) then
            open(unit=iout,file='output.dat',status='unknown')
            write(iout,*) '-------------------------------------------&
            &----------------------- ---------------'
            write(iout,901) ' SAMURAI called with arguments: imeth = ',imeth,&
            & ' ; isca = ',isca,' ; verbosity = ',verbosity,' ; itest = ',itest
            write(iout,*) '-------------------------------------------------&
            &----------------- ---------------'
            notfirstp = .TRUE.
         endif
      endif
      if (itest.gt.0 .and. verbosity.gt.0) then
         if (notfirstd.eqv.(.false.)) then
            if (ibad.gt.0) then
               open(unit=ibad,file='bad.points',status='unknown')
               write(ibad,*) '-------------------------------------------------&
               &-------------------'
               write(ibad,904) ' Points that have been discarded by &
               &SAMURAI because failing itest = ',itest
               write(ibad,*) '-------------------------------------------------&
               &-------------------'
            endif
            notfirstd = .true.
         endif
      endif

 904  format(a68,I1)

      call rtlimit

   end subroutine initsamurai
   !---#] subroutine initsamurai:
   !---#[ subroutine rtlimit:
   subroutine rtlimit
     implicit none
     real :: tpwlimit,tnnlimit,tnnlimit4,tnnlimit3,tnnlimit2,tnnlimit1
     integer :: ierr
     open(unit=10,file='ltest.dat',status='old',iostat=ierr)

     if (ierr.eq.0) then
        read(10,*)
        read(10,*)
        read(10,*)  tpwlimit
        pwlimit   = real(tpwlimit, ki)
        read(10,*)  tnnlimit
        nnlimit   = real(tnnlimit, ki)
        read(10,*)  tnnlimit4
        lnnlimit4 = real(tnnlimit4, ki)
        read(10,*)  tnnlimit3
        lnnlimit3 = real(tnnlimit3, ki)
        read(10,*)  tnnlimit2
        lnnlimit2 = real(tnnlimit2, ki)
        read(10,*)  tnnlimit1
        lnnlimit1 = real(tnnlimit1, ki)

        close(10)
     else
        pwlimit   = 1.0E-03_ki
        nnlimit   = 1.0E-03_ki
        lnnlimit4 = 1.0E-02_ki
        lnnlimit3 = 1.0E-02_ki
        lnnlimit2 = 1.0E-02_ki
        lnnlimit1 = 1.0E+01_ki
     end if
   end subroutine rtlimit
   !---#] subroutine rtlimit:
   !---#[ subroutine exitsamurai:
  subroutine exitsamurai()
      implicit none

      if (verbosity.gt.0) then
         close(iout)
      endif

      if (itest.gt.0) then
         if (ibad.gt.0) then
            close(ibad)
         endif
      endif

      if (isca.eq.4) then
!AC!         call ltexi
      endif
   end subroutine exitsamurai
   !---#] subroutine exitsamurai:
   !---#[ subroutine banner:
   subroutine banner
     implicit none

     print*
     print*, ' *******************************************************&
     &*************'
     print*, ' ********************** SAMURAI - version 2.1.1'
     print*, ' *******************************************************&
     &*************'
     print*, ' *                                                      &
     &            *'
     print*, ' *                                                      &
     &            *'
     print*, ' * Authors: P. Mastrolia, G. Ossola, T. Reiter and F. Tr&
     &amontano    *'
     print*, ' *                                                      &
     &            *'
     print*, ' * pierpaolo.mastrolia@cern.ch                          &
     &            *'
     print*, ' * gossola@citytech.cuny.edu                            &
     &            *'
     print*, ' * reiterth@mpp.mpg.de                                  &
     &            *'
     print*, ' * francesco.tramontano@cern.ch                         &
     &            *'
     print*, ' *                                                      &
     &            *'
     print*, ' *  For details please see: arXiv:1006.0710             &
     &            *'
     print*, ' *                                                      &
     &            *'
     print*, ' *  On the web:  http://cern.ch/samurai                 &
     &            *'
     print*, ' *                                                      &
     &            *'
     print*, ' *******************************************************&
     &*************'
     print*, ' *                                                      &
     &            *'
     print*, ' * output files: <output.log>   [ for verbosity.gt.0 ]  &
     &            *'
     print*, ' *                                                      &
     &            *'
     print*, ' *               <bad.points>   [ for itest.gt.0     ]  &
     &            *'
     print*, ' *                                                      &
     &            *'
     print*, ' *******************************************************&
     &*************'
   end subroutine banner
   !---#] subroutine banner:
   !---#[ subroutine InitDenominators_cm:
   pure subroutine InitDenominators_cm(nleg,Vi,msq,&
      &Q0,m0,Q1,m1,Q2,m2,Q3,m3,Q4,m4,Q5,m5,Q6,m6,Q7,m7)
      implicit none
      integer, intent(in) :: nleg
      real(ki), dimension(0:nleg-1,4), intent(out) :: Vi
      complex(ki), dimension(0:nleg-1), intent(out) :: msq

      real(ki), dimension(4), intent(in), optional :: Q0
      complex(ki), intent(in), optional :: m0
      real(ki), dimension(4), intent(in), optional :: Q1
      complex(ki), intent(in), optional :: m1
      real(ki), dimension(4), intent(in), optional :: Q2
      complex(ki), intent(in), optional :: m2
      real(ki), dimension(4), intent(in), optional :: Q3
      complex(ki), intent(in), optional :: m3
      real(ki), dimension(4), intent(in), optional :: Q4
      complex(ki), intent(in), optional :: m4
      real(ki), dimension(4), intent(in), optional :: Q5
      complex(ki), intent(in), optional :: m5
      real(ki), dimension(4), intent(in), optional :: Q6
      complex(ki), intent(in), optional :: m6
      real(ki), dimension(4), intent(in), optional :: Q7
      complex(ki), intent(in), optional :: m7

      if(present(Q0) .and. present(m0)) then
         Vi(0,:) = Q0
         msq(0) = m0*m0
      end if
      if(present(Q1) .and. present(m1)) then
         Vi(1,:) = Q1
         msq(1) = m1*m1
      end if
      if(present(Q2) .and. present(m2)) then
         Vi(2,:) = Q2
         msq(2) = m2*m2
      end if
      if(present(Q3) .and. present(m3)) then
         Vi(3,:) = Q3
         msq(3) = m3*m3
      end if
      if(present(Q4) .and. present(m4)) then
         Vi(4,:) = Q4
         msq(4) = m4*m4
      end if
      if(present(Q5) .and. present(m5)) then
         Vi(5,:) = Q5
         msq(5) = m5*m5
      end if
      if(present(Q6) .and. present(m6)) then
         Vi(6,:) = Q6
         msq(6) = m6*m6
      end if
      if(present(Q7) .and. present(m7)) then
         Vi(7,:) = Q7
         msq(7) = m7*m7
      end if

   end  subroutine InitDenominators_cm
   !---#] subroutine InitDenominators_cm:
   !---#[ subroutine InitDenominators_rm:
   pure subroutine InitDenominators_rm(nleg,Vi,msq,&
      &Q0,m0,Q1,m1,Q2,m2,Q3,m3,Q4,m4,Q5,m5,Q6,m6,Q7,m7)
      implicit none
      integer, intent(in) :: nleg
      real(ki), dimension(0:nleg-1,4), intent(out) :: Vi
      real(ki), dimension(0:nleg-1), intent(out) :: msq

      real(ki), dimension(4), intent(in), optional :: Q0
      real(ki), intent(in), optional :: m0
      real(ki), dimension(4), intent(in), optional :: Q1
      real(ki), intent(in), optional :: m1
      real(ki), dimension(4), intent(in), optional :: Q2
      real(ki), intent(in), optional :: m2
      real(ki), dimension(4), intent(in), optional :: Q3
      real(ki), intent(in), optional :: m3
      real(ki), dimension(4), intent(in), optional :: Q4
      real(ki), intent(in), optional :: m4
      real(ki), dimension(4), intent(in), optional :: Q5
      real(ki), intent(in), optional :: m5
      real(ki), dimension(4), intent(in), optional :: Q6
      real(ki), intent(in), optional :: m6
      real(ki), dimension(4), intent(in), optional :: Q7
      real(ki), intent(in), optional :: m7

      if(present(Q0) .and. present(m0)) then
         Vi(0,:) = Q0
         msq(0) = m0*m0
      end if
      if(present(Q1) .and. present(m1)) then
         Vi(1,:) = Q1
         msq(1) = m1*m1
      end if
      if(present(Q2) .and. present(m2)) then
         Vi(2,:) = Q2
         msq(2) = m2*m2
      end if
      if(present(Q3) .and. present(m3)) then
         Vi(3,:) = Q3
         msq(3) = m3*m3
      end if
      if(present(Q4) .and. present(m4)) then
         Vi(4,:) = Q4
         msq(4) = m4*m4
      end if
      if(present(Q5) .and. present(m5)) then
         Vi(5,:) = Q5
         msq(5) = m5*m5
      end if
      if(present(Q6) .and. present(m6)) then
         Vi(6,:) = Q6
         msq(6) = m6*m6
      end if
      if(present(Q7) .and. present(m7)) then
         Vi(7,:) = Q7
         msq(7) = m7*m7
      end if

   end  subroutine InitDenominators_rm
   !---#] subroutine InitDenominators_rm:
   !---#[ subroutine samurai_cm:
   subroutine samurai_cm(numeval,tot,totr,Vi,msq,nleg,rank,istop,scale2,ok,&
         cache_flag, scalar_cache)
      use options, only: use_maccu
      use maccu
      implicit none

      integer, intent(in) :: nleg, rank, istop
      real(ki), intent(in) :: scale2
      complex(ki), intent(out) :: totr
      complex(ki), dimension(0:nleg-1), intent(in) :: msq
      real(ki), dimension(0:nleg-1,4), intent(in) :: Vi
      complex(ki), dimension(-2:0), intent(out) :: tot
      logical, intent(out) :: ok
      logical, intent(inout), optional :: cache_flag
      complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache

      type(accumulator_type), dimension(-2:0) :: acc_re, acc_im
      type(accumulator_type) :: accr_re, accr_im

      integer :: i,j,k,j1,j2,j3,j4,j5,icut5,icut4,icut3,icut2,icut1
      integer :: cut5,cut4,cut3,cut2,cut1,n1,ep
      integer :: diff 
      integer :: cache_offset

      complex(ki) :: mu2, mu2test
      real(ki) :: r1, r2, factor
      real(ki), dimension(4):: k1, k2, k3, e1, e2, p0, L3

      complex(ki) :: c5,tot2r,tot3r,tot4r
      complex(ki), dimension(5,4) :: q4, q1
      complex(ki), dimension(10,4) :: q3, q2
      complex(ki), dimension(4):: q5, e3, e4, qt, qtest
      complex(ki), dimension(0:4) :: c4, c1
      complex(ki), dimension(0:9) :: c3, c2
      complex(ki), dimension(-2:0) :: tot4, tot3, tot2, tot1

      logical :: rescue

      interface
         function     numeval(ncut, Q, mu2)
           use precision
           implicit none
           integer, intent(in) :: ncut
           complex(ki), dimension(4), intent(in) :: Q
           complex(ki), intent(in) :: mu2
           complex(ki) :: numeval
         end function numeval
      end interface

      if (nleg.gt.maxleg) then
         write(6,*) 'reduction called for nleg.gt.maxleg'
         write(6,*) 'maxleg, nleg ',maxleg,nleg
         write(6,*) 'please change the values max1,..,max5,maxleg &
         &in constants.f90'
         write(6,*) 'and compile again the library'
         stop
      endif

      cache_offset = 0
      diff = nleg-rank
      
      if(verbosity.gt.0)then
         write(iout,*)
         write(iout,*)
         write(iout,*) 'Denominators: '
         do k=0,nleg-1
            write(iout,902) ' Pi(',k,') = ', Vi(k,:)
            write(iout,903) 'msq(',k,') = ', msq(k)
            write(iout,*)
         enddo
         write(iout,*)' '
      endif

      totr=czip
      tot4r=czip
      tot3r=czip
      tot2r=czip
     
      tot(:)=czip
      tot4(:)=czip
      tot3(:)=czip
      tot2(:)=czip
      tot1(:)=czip

      rescue = .false.

      if (use_maccu) then
         do ep=-2,0
            acc_re(ep)%a(:) = 0.0_ki
            acc_im(ep)%a(:) = 0.0_ki
         end do
         accr_re%a(:) = 0.0_ki
         accr_im%a(:) = 0.0_ki
      end if
 
!  5   continue

!!$      if (  (iresc.eq.1).or.(iresc.eq.2).or.&
!!$           ((iresc.eq.3).and.(rescue))) then
!!$         call tensor_reconstruction(numeval,nleg,rank)
!!$         if   (iresc.eq.1) goto 6
!!$         if ( (iresc.eq.2).or.&
!!$             ((iresc.eq.3).and.(rescue)) ) then
!!$
!!$         ! Last parameter sets tot_or_rat in golem95
!!$         !    false -- full reconstruction
!!$         !    true  -- rational part only
!!$         tot  = addtens(rank,nleg,Vi,msq,scale2,.false.)
!!$         ok = .true.
!!$         goto 100
!!$         endif

      notfirsti = .false.
     
      if (iresc.eq.1) then
         call tensor_reconstruction(numeval,nleg,rank) 
      endif

!  6   continue

      if      (nleg.ge.5) then
         goto 10
      elseif  (nleg.eq.4) then
         goto 20
      elseif  (nleg.eq.3) then
         goto 30
      elseif  (nleg.eq.2) then
         goto 40
      elseif  ((nleg.eq.1).or.(nleg.le.0)) then
         goto 50
      endif

 10   continue

      if(verbosity.gt.0)then
         write(iout,*) 'Pentagon coefficients: '
      endif

!--- Quintuple cuts
      icut5=1
      do j5=4,nleg-1
       do j4=3,j5-1
        do j3=2,j4-1
         do j2=1,j3-1
          do j1=0,j2-1

      cut5=j5*10000+j4*1000+j3*100+j2*10+j1

      do n1=1,4
         k1(n1)=Vi(j2,n1)-Vi(j1,n1)
         k2(n1)=Vi(j1,n1)-Vi(j5,n1)
         p0(n1)=Vi(j1,n1)
      enddo

      call getbase(k1,k2,r1,r2,e1,e2,e3,e4)
      call getq5(nleg,cut5,e1,e2,e3,e4,p0,Vi,msq,r1,r2,q5,mu2)

      if (iresc.eq.1) then
         call getc5(numetens,nleg,c5,cut5,Vi,msq,q5,mu2)
      else
         call getc5(numeval,nleg,c5,cut5,Vi,msq,q5,mu2)
      endif

      call store5(icut5,cut5,p0,e1,e2,e3,e4,c5)

      if(verbosity.gt.0)then
         write(iout,9005) 'c5(', cut5,')  = (',real(c5),',',aimag(c5),'  )'
         write(iout,*)
      endif

!--- nullify all
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      k1(:)=0.0_ki
      k2(:)=0.0_ki
      p0(:)=0.0_ki

      icut5=icut5+1
          enddo
         enddo
        enddo
       enddo
      enddo
      nc5=icut5-1

      if (istop.eq.5) goto 99

 20   continue

      if(verbosity.gt.0)then
         write(iout,*) 'Box coefficients: '
      endif
      

!--- Quadruple cuts
      icut4=1
      do j4=3,nleg-1
       do j3=2,j4-1
        do j2=1,j3-1
         do j1=0,j2-1

      cut4=j4*1000+j3*100+j2*10+j1

      do n1=1,4
         k1(n1)=Vi(j2,n1)-Vi(j1,n1)
         k2(n1)=Vi(j1,n1)-Vi(j4,n1)
         k3(n1)=Vi(j3,n1)-Vi(j1,n1)
         p0(n1)=Vi(j1,n1)
         L3(n1)=Vi(j4,n1)-Vi(j3,n1)
      enddo

      call getbase(k1,k2,r1,r2,e1,e2,e3,e4)
      call getq4(nleg,cut4,e1,e2,e3,e4,p0,k1,k2,k3,L3,r1,r2,q4,qt,msq)

      if (iresc.eq.1) then
         call getc4(numetens,nleg,rank,c4,cut4,q4,qt,p0,Vi,msq)
      else
         call getc4(numeval,nleg,rank,c4,cut4,q4,qt,p0,Vi,msq)
      endif

      if (itest.eq.2) call lnntest4(numeval,cut4,c4,qt,p0,L3,e3,e4,ok)
      call store4(icut4,cut4,L3,p0,e1,e2,e3,e4,c4)
      if (present(cache_flag)) then
         call add4_cm(nleg,c4,cut4,Vi,msq,tot4,tot4r,scale2,&
              cache_flag, cache_offset, scalar_cache)
      else
         call add4_cm(nleg,c4,cut4,Vi,msq,tot4,tot4r,scale2)
      end if

      if(verbosity.gt.0)then
         do i=0,4
         write(iout,9004) 'c4(', cut4,',',i,') = (',real(c4(i)),',',aimag(c4(i)),'  )'
         enddo
      write(iout,*)
      endif
      
      if (use_maccu) then
         call add_accu(accr_re, accr_im, tot4r)
         call add_accu(acc_re(:), acc_im(:), tot4(:))
      else 
         tot(:)=tot(:)+tot4(:)
         totr=totr+tot4r
      end if

!--- nullify all
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      k1(:)=0.0_ki
      k2(:)=0.0_ki
      k3(:)=0.0_ki
      p0(:)=0.0_ki
      L3(:)=0.0_ki

      icut4=icut4+1
         enddo
        enddo
       enddo
      enddo
      nc4=icut4-1


      if (istop.eq.4) goto 99

 30   continue

      if (diff.ge.4.and.itest.ne.1) goto 93
      
      if(verbosity.gt.0.and.diff.le.3)then
         write(iout,*) 'Triangle coefficients: '
      endif
      
!--- Triple cuts
      icut3=1
      do j3=2,nleg-1
       do j2=1,j3-1
        do j1=0,j2-1

      cut3=j3*100+j2*10+j1

      k1(:)=Vi(j2,:)-Vi(j1,:)
      k2(:)=Vi(j1,:)-Vi(j3,:)
      p0(:)=Vi(j1,:)

      call getbase(k1,k2,r1,r2,e1,e2,e3,e4)

      call getq3(nleg,rank,cut3,e1,e2,e3,e4,p0,k1,k2,msq,r1,r2,q3,qt)
         
      if (iresc.eq.1) then
         call getc3(numetens,nleg,rank,c3,cut3,q3,qt,Vi,msq)
      else 
         call getc3(numeval,nleg,rank,c3,cut3,q3,qt,Vi,msq)
      endif

      if (itest.eq.2) call lnntest3(numeval,cut3,c3,qt,p0,e3,e4,ok)
      call store3(icut3,cut3,p0,e1,e2,e3,e4,c3)
      if (present(cache_flag)) then
         call add3_cm(nleg,c3,cut3,Vi,msq,tot3,tot3r,scale2,&
              cache_flag, cache_offset, scalar_cache)
      else
         call add3_cm(nleg,c3,cut3,Vi,msq,tot3,tot3r,scale2)
      end if

      if(verbosity.gt.0.and.diff.le.3)then
         do i=0,9
         write(iout,9003) 'c3(',cut3,',',i,')  = (',real(c3(i)),',',aimag(c3(i)),'  )'
         enddo
      write(iout,*)
      endif

      if (use_maccu) then
         call add_accu(accr_re, accr_im, tot3r)
         call add_accu(acc_re(:), acc_im(:), tot3(:))
      else
         tot(:)=tot(:)+tot3(:)
         totr=totr+tot3r
      end if

!--- nullify all
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      k1(:)=0.0_ki
      k2(:)=0.0_ki
      p0(:)=0.0_ki

       icut3=icut3+1
         enddo
        enddo
       enddo
      nc3=icut3-1

      if (istop.eq.3) goto 99

 40   continue

      if (diff.ge.3) goto 95
      
      if(verbosity.gt.0)then
         write(iout,*) 'Bubble coefficients: '
      endif

!--- Double cuts
      icut2=1
      do j2=1,nleg-1
         do j1=0,j2-1

            cut2=j2*10+j1

            k1(:)=Vi(j2,:)-Vi(j1,:)
            p0(:)=Vi(j1,:)


            if (abs(abs(k1(4))-1.0_ki) .lt. 0.1_ki) then
               factor = 0.345_ki
            else
               factor = one
            end if
            k2(1)=-sign(factor/rt3,k1(1))
            k2(2)=-sign(factor/rt3,k1(2))
            k2(3)=-sign(factor/rt3,k1(3))
            k2(4)= sign(factor,k1(4))


            call getbase(k1,k2,r1,r2,e1,e2,e3,e4)

            call getq2(nleg,rank,cut2,e1,e2,e3,e4,p0,k1,msq,q2,qt)

            if (iresc.eq.1) then
               call getc2(numetens,nleg,rank,c2,cut2,q2,qt,Vi,msq)
            else
               call getc2(numeval,nleg,rank,c2,cut2,q2,qt,Vi,msq)
            endif

            if (itest.eq.2) call lnntest2(numeval,cut2,c2,qt,p0,e2,e3,e4,ok)
               call store2(icut2,cut2,p0,e1,e2,e3,e4,c2)
               if (present(cache_flag)) then
                  call add2_cm(nleg,c2,cut2,k1,k2,msq,tot2,tot2r,scale2, &
                       & cache_flag, cache_offset, scalar_cache)
               else
                  call add2_cm(nleg,c2,cut2,k1,k2,msq,tot2,tot2r,scale2)
               end if

               if(verbosity.gt.0)then
                  do i=0,9
                     write(iout,9002) 'c2(',cut2,',',i,')   = (', &
                     & real(c2(i)),',',aimag(c2(i)),'  )'
                  enddo
                  write(iout,*)
               endif

               if (use_maccu) then
                  call add_accu(accr_re, accr_im, tot2r)
                  call add_accu(acc_re(:), acc_im(:), tot2(:))
               else 
                  tot(:)=tot(:)+tot2(:)
                  totr=totr+tot2r
               end if

!--- nullify k1,k2,p0
               r1=zip
               r2=zip
               e1(:)=0.0_ki
               e2(:)=0.0_ki
               e3(:)=czip
               e4(:)=czip
               k1(:)=0.0_ki
               p0(:)=0.0_ki

               icut2=icut2+1
            enddo
         enddo
         nc2=icut2-1

         if (istop.eq.2) goto 99

    50   continue

         if (diff.ge.2) goto 97

         if(verbosity.gt.0)then
            write(iout,*) 'Tadpole coefficients: '
         endif


!--- Single cut
         icut1=1
         do j1=0,nleg-1

            cut1=j1

            k1(1)=+one/rt3
            k1(2)=-one/rt3
            k1(3)=+one/rt3
            k1(4)=+two
            k2(1)=+one/rt2
            k2(2)=-one/rt2
            k2(3)=+one/rt2
            k2(4)=+rt3

            p0(:)=Vi(j1,:)

            call getbase(k1,k2,r1,r2,e1,e2,e3,e4)    
            call getq1(nleg,cut1,e1,e2,e3,e4,p0,msq,q1,qt)

            if (iresc.eq.1) then
               call getc1(numetens,nleg,rank,c1,cut1,q1,qt,Vi,msq)
            else
               call getc1(numeval,nleg,rank,c1,cut1,q1,qt,Vi,msq)
            endif

            if (itest.eq.2) call lnntest1(numeval,cut1,c1,qt,p0,e1,e2,e3,e4,ok)
            call store1(icut1,cut1,p0,e1,e2,e3,e4,c1)

            if (present(cache_flag)) then
               call add1_cm(nleg,c1,cut1,msq,tot1,scale2,&
                  & cache_flag, cache_offset, scalar_cache)
            else
               call add1_cm(nleg,c1,cut1,msq,tot1,scale2)
            end if

      if(verbosity.gt.0)then
         do i=0,4
            write(iout,9002) 'c1(',cut1,',',i,')   = (',real(c1(i)),',',aimag(c1(i)),'  )'
         enddo
            write(iout,*)
      endif

      if (use_maccu) then
         call add_accu(acc_re, acc_im, tot1)
      else 
         tot(:)=tot(:)+tot1(:)
      end if

!--- nullify k1,k2,p0
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      p0(:)=0.0_ki

      icut1=icut1+1
      enddo
      nc1=icut1-1

 93   continue
  
      do j=0,9
         c3(j)=czip
      enddo
      
 95   continue

      do j=0,9
         c2(j)=czip
      enddo
 
 97   continue
 
      do j=0,4
         c1(j)=czip
      enddo

 99   continue

      if (use_maccu) then
         tot = reduce_accu(acc_re, acc_im)
         totr = reduce_accu(accr_re, accr_im)
      end if

   
! TEST N=N -------------------------------------------
      if (itest.eq.1) then
         qtest=10.3_ki*cone
         mu2test=13.0_ki
         call nntest(numeval,qtest,mu2test,nleg,Vi,msq,ok)
      endif
!-----------------------------------------------------         

! POWER test ----------------------------------
      if (itest.eq.3) call pwtest(nleg,rank,ok)
! ---------------------------------------------   

!!$      if ((itest.eq.3).and.(iresc.eq.3).and.(ok.eqv.(.false.))) then
!!$      rescue = .true.
!!$      print*, 'eccolo'
!!$      goto 5
!!$      endif

 ! 100  continue

      if (ok) then
         
         if(verbosity.gt.0)then
            write(iout,*)
            write(iout,*)' Result: '
            write(iout,*)' Double   Pole = ', tot(-2)
            write(iout,*)' Single   Pole = ', tot(-1)
            write(iout,*)' Finite   Part = ', tot(0)
            write(iout,*)
            write(iout,*)'[Rational Part = ', totr,']'
            write(iout,*)
            write(iout,*)
         endif

      else    

         if(ibad.gt.0.and.verbosity.gt.0)then
            write(ibad,*) 'Denominators: '
            do k=0,nleg-1
               write(ibad,902) ' Pi(',k,') = ', Vi(k,:)
               write(ibad,903) 'msq(',k,') = ', msq(k)
               write(ibad,*)
            enddo
            write(ibad,*)'---------------------------------------- '
            write(ibad,*)' '
         endif
      endif

      if(present(cache_flag)) cache_flag = .true.

 902  format(a4,I1,a4,4(D24.15))
 903  format(a4,I1,a4,2(D24.15))

 9005 format(A3,I5,A6,D24.15,A1,D24.15,A3)
 9004 format(A3,I4,A1,I1,A5,D24.15,A1,D24.15,A3)
 9003 format(A3,I3,A1,I1,A6,D24.15,A1,D24.15,A3)
 9002 format(A3,I2,A1,I1,A7,D24.15,A1,D24.15,A3)

   end subroutine samurai_cm
   !---#] subroutine samurai_cm:
   !---#[ subroutine samurai_rm:
   subroutine samurai_rm(numeval,tot,totr,Vi,msq,nleg,rank,istop,scale2,ok,&
         cache_flag, scalar_cache)
      use options, only: use_maccu
      use maccu
      implicit none

      integer, intent(in) :: nleg, rank, istop
      real(ki), intent(in) :: scale2
      complex(ki), intent(out) :: totr
      real(ki), dimension(0:nleg-1), intent(in) :: msq
      real(ki), dimension(0:nleg-1,4), intent(in) :: Vi
      complex(ki), dimension(-2:0), intent(out) :: tot
      logical, intent(out) :: ok
      logical, intent(inout), optional :: cache_flag
      complex(ki), intent(inout), optional, dimension(-2:0,*) :: scalar_cache

      type(accumulator_type), dimension(-2:0) :: acc_re, acc_im
      type(accumulator_type) :: accr_re, accr_im

      integer :: i,j,k,j1,j2,j3,j4,j5,icut5,icut4,icut3,icut2,icut1
      integer :: cut5,cut4,cut3,cut2,cut1,n1,ep
      integer :: diff 
      integer :: cache_offset

      complex(ki) :: mu2, mu2test
      real(ki) :: r1, r2, factor
      real(ki), dimension(4):: k1, k2, k3, e1, e2, p0, L3

      complex(ki) :: c5,tot2r,tot3r,tot4r
      complex(ki), dimension(5,4) :: q4, q1
      complex(ki), dimension(10,4) :: q3, q2
      complex(ki), dimension(4):: q5, e3, e4, qt, qtest
      complex(ki), dimension(0:4) :: c4, c1
      complex(ki), dimension(0:9) :: c3, c2
      complex(ki), dimension(-2:0) :: tot4, tot3, tot2, tot1

      logical :: rescue

      interface
         function     numeval(ncut, Q, mu2)
           use precision
           implicit none
           integer, intent(in) :: ncut
           complex(ki), dimension(4), intent(in) :: Q
           complex(ki), intent(in) :: mu2
           complex(ki) :: numeval
         end function numeval
      end interface

      if (nleg.gt.maxleg) then
         write(6,*) 'reduction called for nleg.gt.maxleg'
         write(6,*) 'maxleg, nleg ',maxleg,nleg
         write(6,*) 'please change the values max1,..,max5,maxleg &
         &in constants.f90'
         write(6,*) 'and compile again the library'
         stop
      endif

      cache_offset = 0
      diff = nleg-rank
      
      if(verbosity.gt.0)then
         write(iout,*)
         write(iout,*)
         write(iout,*) 'Denominators: '
         do k=0,nleg-1
            write(iout,902) ' Pi(',k,') = ', Vi(k,:)
            write(iout,903) 'msq(',k,') = ', msq(k)
            write(iout,*)
         enddo
         write(iout,*)' '
      endif

      totr=czip
      tot4r=czip
      tot3r=czip
      tot2r=czip
     
      tot(:)=czip
      tot4(:)=czip
      tot3(:)=czip
      tot2(:)=czip
      tot1(:)=czip

      rescue = .false.

      if (use_maccu) then
         do ep=-2,0
            acc_re(ep)%a(:) = 0.0_ki
            acc_im(ep)%a(:) = 0.0_ki
         end do
         accr_re%a(:) = 0.0_ki
         accr_im%a(:) = 0.0_ki
      end if
 
!  5   continue

!!$      if (  (iresc.eq.1).or.(iresc.eq.2).or.&
!!$           ((iresc.eq.3).and.(rescue))) then
!!$         call tensor_reconstruction(numeval,nleg,rank)
!!$         if   (iresc.eq.1) goto 6
!!$         if ( (iresc.eq.2).or.&
!!$             ((iresc.eq.3).and.(rescue)) ) then
!!$
!!$         ! Last parameter sets tot_or_rat in golem95
!!$         !    false -- full reconstruction
!!$         !    true  -- rational part only
!!$         tot  = addtens(rank,nleg,Vi,msq,scale2,.false.)
!!$         ok = .true.
!!$         goto 100
!!$         endif

      notfirsti = .false.
     
      if (iresc.eq.1) then
         call tensor_reconstruction(numeval,nleg,rank) 
      endif

!  6   continue

      if      (nleg.ge.5) then
         goto 10
      elseif  (nleg.eq.4) then
         goto 20
      elseif  (nleg.eq.3) then
         goto 30
      elseif  (nleg.eq.2) then
         goto 40
      elseif  ((nleg.eq.1).or.(nleg.le.0)) then
         goto 50
      endif

 10   continue

      if(verbosity.gt.0)then
         write(iout,*) 'Pentagon coefficients: '
      endif

!--- Quintuple cuts
      icut5=1
      do j5=4,nleg-1
       do j4=3,j5-1
        do j3=2,j4-1
         do j2=1,j3-1
          do j1=0,j2-1

      cut5=j5*10000+j4*1000+j3*100+j2*10+j1

      do n1=1,4
         k1(n1)=Vi(j2,n1)-Vi(j1,n1)
         k2(n1)=Vi(j1,n1)-Vi(j5,n1)
         p0(n1)=Vi(j1,n1)
      enddo

      call getbase(k1,k2,r1,r2,e1,e2,e3,e4)
      call getq5(nleg,cut5,e1,e2,e3,e4,p0,Vi,msq,r1,r2,q5,mu2)

      if (iresc.eq.1) then
         call getc5(numetens,nleg,c5,cut5,Vi,msq,q5,mu2)
      else
         call getc5(numeval,nleg,c5,cut5,Vi,msq,q5,mu2)
      endif

      call store5(icut5,cut5,p0,e1,e2,e3,e4,c5)

      if(verbosity.gt.0)then
         write(iout,9005) 'c5(', cut5,')  = (',real(c5),',',aimag(c5),'  )'
         write(iout,*)
      endif

!--- nullify all
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      k1(:)=0.0_ki
      k2(:)=0.0_ki
      p0(:)=0.0_ki

      icut5=icut5+1
          enddo
         enddo
        enddo
       enddo
      enddo
      nc5=icut5-1

      if (istop.eq.5) goto 99

 20   continue

      if(verbosity.gt.0)then
         write(iout,*) 'Box coefficients: '
      endif
      

!--- Quadruple cuts
      icut4=1
      do j4=3,nleg-1
       do j3=2,j4-1
        do j2=1,j3-1
         do j1=0,j2-1

      cut4=j4*1000+j3*100+j2*10+j1

      do n1=1,4
         k1(n1)=Vi(j2,n1)-Vi(j1,n1)
         k2(n1)=Vi(j1,n1)-Vi(j4,n1)
         k3(n1)=Vi(j3,n1)-Vi(j1,n1)
         p0(n1)=Vi(j1,n1)
         L3(n1)=Vi(j4,n1)-Vi(j3,n1)
      enddo

      call getbase(k1,k2,r1,r2,e1,e2,e3,e4)
      call getq4(nleg,cut4,e1,e2,e3,e4,p0,k1,k2,k3,L3,r1,r2,q4,qt,msq)

      if (iresc.eq.1) then
         call getc4(numetens,nleg,rank,c4,cut4,q4,qt,p0,Vi,msq)
      else
         call getc4(numeval,nleg,rank,c4,cut4,q4,qt,p0,Vi,msq)
      endif

      if (itest.eq.2) call lnntest4(numeval,cut4,c4,qt,p0,L3,e3,e4,ok)
      call store4(icut4,cut4,L3,p0,e1,e2,e3,e4,c4)

      if (present(cache_flag)) then
         call add4_rm(nleg,c4,cut4,Vi,msq,tot4,tot4r,scale2,&
              cache_flag, cache_offset, scalar_cache)
      else
         call add4_rm(nleg,c4,cut4,Vi,msq,tot4,tot4r,scale2)
      end if

      if(verbosity.gt.0)then
         do i=0,4
         write(iout,9004) 'c4(', cut4,',',i,') = (',real(c4(i)),',',aimag(c4(i)),'  )'
         enddo
      write(iout,*)
      endif
     
      if (use_maccu) then
         call add_accu(accr_re, accr_im, tot4r)
         call add_accu(acc_re(:), acc_im(:), tot4(:))
      else 
         tot(:)=tot(:)+tot4(:)
         totr=totr+tot4r
      end if

!--- nullify all
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      k1(:)=0.0_ki
      k2(:)=0.0_ki
      k3(:)=0.0_ki
      p0(:)=0.0_ki
      L3(:)=0.0_ki

      icut4=icut4+1
         enddo
        enddo
       enddo
      enddo
      nc4=icut4-1


      if (istop.eq.4) goto 99

 30   continue

      if (diff.ge.4.and.itest.ne.1) goto 93
      
      if(verbosity.gt.0.and.diff.le.3)then
         write(iout,*) 'Triangle coefficients: '
      endif
      
!--- Triple cuts
      icut3=1
      do j3=2,nleg-1
       do j2=1,j3-1
        do j1=0,j2-1

      cut3=j3*100+j2*10+j1

      do n1=1,4
         k1(n1)=Vi(j2,n1)-Vi(j1,n1)
         k2(n1)=Vi(j1,n1)-Vi(j3,n1)
         p0(n1)=Vi(j1,n1)
      enddo

      call getbase(k1,k2,r1,r2,e1,e2,e3,e4)
      call getq3(nleg,rank,cut3,e1,e2,e3,e4,p0,k1,k2,msq,r1,r2,q3,qt)

      if (iresc.eq.1) then
         call getc3(numetens,nleg,rank,c3,cut3,q3,qt,Vi,msq)
      else 
         call getc3(numeval,nleg,rank,c3,cut3,q3,qt,Vi,msq)
      endif

      if (itest.eq.2) call lnntest3(numeval,cut3,c3,qt,p0,e3,e4,ok)
      call store3(icut3,cut3,p0,e1,e2,e3,e4,c3)
      if (present(cache_flag)) then
         call add3_rm(nleg,c3,cut3,Vi,msq,tot3,tot3r,scale2,&
              cache_flag, cache_offset, scalar_cache)
      else
         call add3_rm(nleg,c3,cut3,Vi,msq,tot3,tot3r,scale2)
      end if

      if(verbosity.gt.0.and.diff.le.3)then
         do i=0,9
         write(iout,9003) 'c3(',cut3,',',i,')  = (',real(c3(i)),',',aimag(c3(i)),'  )'
         enddo
      write(iout,*)
      endif

      if (use_maccu) then
         call add_accu(accr_re, accr_im, tot3r)
         call add_accu(acc_re(:), acc_im(:), tot3(:))
      else
         tot(:)=tot(:)+tot3(:)
         totr=totr+tot3r
      end if

!--- nullify all
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      k1(:)=0.0_ki
      k2(:)=0.0_ki
      p0(:)=0.0_ki

      icut3=icut3+1
         enddo
        enddo
       enddo
      nc3=icut3-1

      if (istop.eq.3) goto 99

 40   continue

      if (diff.ge.3) goto 95
      
      if(verbosity.gt.0)then
         write(iout,*) 'Bubble coefficients: '
      endif

!--- Double cuts
      icut2=1
      do j2=1,nleg-1
        do j1=0,j2-1

      cut2=j2*10+j1

      do n1=1,4
         k1(n1)=Vi(j2,n1)-Vi(j1,n1)
         p0(n1)=Vi(j1,n1)
      enddo


     if (abs(abs(k1(4))-1.0_ki) .lt. 0.1_ki) then
        factor = 0.345_ki
     else
        factor = one
     end if

     k2(1)=-sign(factor/rt3,k1(1))
     k2(2)=-sign(factor/rt3,k1(2))
     k2(3)=-sign(factor/rt3,k1(3))
     k2(4)= sign(factor,k1(4))

      call getbase(k1,k2,r1,r2,e1,e2,e3,e4)

      call getq2(nleg,rank,cut2,e1,e2,e3,e4,p0,k1,msq,q2,qt)

      if (iresc.eq.1) then
         call getc2(numetens,nleg,rank,c2,cut2,q2,qt,Vi,msq)
      else
         call getc2(numeval,nleg,rank,c2,cut2,q2,qt,Vi,msq)
      endif

      if (itest.eq.2) call lnntest2(numeval,cut2,c2,qt,p0,e2,e3,e4,ok)
      call store2(icut2,cut2,p0,e1,e2,e3,e4,c2)

      if (present(cache_flag)) then
         call add2_rm(nleg,c2,cut2,k1,k2,msq,tot2,tot2r,scale2,&
              cache_flag, cache_offset, scalar_cache)
      else
         call add2_rm(nleg,c2,cut2,k1,k2,msq,tot2,tot2r,scale2)
      end if

      if(verbosity.gt.0)then
         do i=0,9
            write(iout,9002) 'c2(',cut2,',',i,')   = (',real(c2(i)),',',aimag(c2(i)),'  )'
         enddo
            write(iout,*)
      endif

      if (use_maccu) then
         call add_accu(accr_re, accr_im, tot2r)
         call add_accu(acc_re(:), acc_im(:), tot2(:))
      else 
         tot(:)=tot(:)+tot2(:)
         totr=totr+tot2r
      end if

!--- nullify k1,k2,p0
      r1=zip
      r2=zip
      e1(:)=0.0_ki
      e2(:)=0.0_ki
      e3(:)=czip
      e4(:)=czip
      k1(:)=0.0_ki
      p0(:)=0.0_ki

      icut2=icut2+1
        enddo
       enddo
      nc2=icut2-1

      if (istop.eq.2) goto 99

 50   continue

      if (diff.ge.2) goto 97

      if(verbosity.gt.0)then
         write(iout,*) 'Tadpole coefficients: '
      endif


!--- Single cut
      icut1=1
      do j1=0,nleg-1

         cut1=j1

         k1(1)=+one/rt3
         k1(2)=-one/rt3
         k1(3)=+one/rt3
         k1(4)=+two

         k2(1)=+one/rt2
         k2(2)=-one/rt2
         k2(3)=+one/rt2
         k2(4)=+rt3

         do n1=1,4
            p0(n1)=Vi(j1,n1)
         enddo

         call getbase(k1,k2,r1,r2,e1,e2,e3,e4)    
         call getq1(nleg,cut1,e1,e2,e3,e4,p0,msq,q1,qt)

         if (iresc.eq.1) then
            call getc1(numetens,nleg,rank,c1,cut1,q1,qt,Vi,msq)
         else
            call getc1(numeval,nleg,rank,c1,cut1,q1,qt,Vi,msq)
         endif

         if (itest.eq.2) call lnntest1(numeval,cut1,c1,qt,p0,e1,e2,e3,e4,ok)
         call store1(icut1,cut1,p0,e1,e2,e3,e4,c1)

         if (present(cache_flag)) then
            call add1_rm(nleg,c1,cut1,msq,tot1,scale2,&
                 cache_flag, cache_offset, scalar_cache)
         else
            call add1_rm(nleg,c1,cut1,msq,tot1,scale2)
         end if

         if(verbosity.gt.0)then
            do i=0,4
               write(iout,9002) 'c1(',cut1,',',i,')   = (',real(c1(i)),',',&
              & aimag(c1(i)),'  )'
            enddo
            write(iout,*)
         endif

         if (use_maccu) then
            call add_accu(acc_re, acc_im, tot1)
         else 
            tot(:)=tot(:)+tot1(:)
         end if


!--- nullify k1,k2,p0
         r1=zip
         r2=zip
         e1(:)=0.0_ki
         e2(:)=0.0_ki
         e3(:)=czip
         e4(:)=czip
         p0(:)=0.0_ki

         icut1=icut1+1
      enddo
      nc1=icut1-1

 93   continue
  
      do j=0,9
         c3(j)=czip
      enddo
      
 95   continue

      do j=0,9
         c2(j)=czip
      enddo
 
 97   continue
 
      do j=0,4
         c1(j)=czip
      enddo
      
 99   continue

      if (use_maccu) then
         tot = reduce_accu(acc_re, acc_im)
         totr = reduce_accu(accr_re, accr_im)
      end if

   
! TEST N=N -------------------------------------------
      if (itest.eq.1) then
         qtest=10.3_ki*cone
         mu2test=13.0_ki
         call nntest(numeval,qtest,mu2test,nleg,Vi,msq,ok)
      endif
!-----------------------------------------------------         

! POWER test ----------------------------------
      if (itest.eq.3) call pwtest(nleg,rank,ok)
! ---------------------------------------------   

!!$      if ((itest.eq.3).and.(iresc.eq.3).and.(ok.eqv.(.false.))) then
!!$      rescue = .true.
!!$      print*, 'eccolo'
!!$      goto 5
!!$      endif

 ! 100  continue

      if (ok) then
         
         if(verbosity.gt.0)then
            write(iout,*)
            write(iout,*)' Result: '
            write(iout,*)' Double   Pole = ', tot(-2)
            write(iout,*)' Single   Pole = ', tot(-1)
            write(iout,*)' Finite   Part = ', tot(0)
            write(iout,*)
            write(iout,*)'[Rational Part = ', totr,']'
            write(iout,*)
            write(iout,*)
         endif

      else    

         if(ibad.gt.0.and.verbosity.gt.0)then
            write(ibad,*) 'Denominators: '
            do k=0,nleg-1
               write(ibad,902) ' Pi(',k,') = ', Vi(k,:)
               write(ibad,903) 'msq(',k,') = ', msq(k)
               write(ibad,*)
            enddo
            write(ibad,*)'---------------------------------------- '
            write(ibad,*)' '
         endif
      endif

      if (present(cache_flag)) cache_flag = .true.

 902  format(a4,I1,a4,4(D24.15))
 903  format(a4,I1,a4,1(D24.15))

 9005 format(A3,I5,A6,D24.15,A1,D24.15,A3)
 9004 format(A3,I4,A1,I1,A5,D24.15,A1,D24.15,A3)
 9003 format(A3,I3,A1,I1,A6,D24.15,A1,D24.15,A3)
 9002 format(A3,I2,A1,I1,A7,D24.15,A1,D24.15,A3)

   end subroutine samurai_rm
   !---#] subroutine samurai_rm:
end module msamurai

