      subroutine init_processes
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      include '../include/pwhg_flg.h'
      include '../include/pwhg_st.h'
      integer i1,i2,i3,i4,i5,k,ii(nlegreal)
      equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),
     #  (i4,ii(4)),(i5,ii(5))
      logical debug
      parameter (debug=.true.)
      integer j
      logical condition
      logical flavequiv
      external flavequiv
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  DON'T FORGET TO  SET THIS TO THE NUMBER OF FLAVOURS REQUIRED 
      integer nflav
      parameter (nflav=2)
C  AND CHANGE THE madgraph SYMBOLIC LINK ACCORDINGLY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      integer three_ch(-6:6)
      data three_ch /-2,1,-2,1,-2,1,0,-1,2,-1,2,-1,2/
c     check nlegborn. This is only a sanity check while we are TESTING 
c     the code and we change often from one process to the other
      if (nlegborn.ne.4) then
         write(*,*) ' ERROR: set nlegborn to the appropriate value'
         write(*,*) ' for this process in nlegborn.h'
         stop
      endif

*********************************************************************
c     number of light flavors
C      st_nlight = nflav
      st_nlight=3
*********************************************************************
*********************************************************************
c     index of the first LIGHT coloured parton in the final state
      flst_lightpart=3
*********************************************************************

*********************************************************************
***********            BORN SUBPROCESSES              ***************
*********************************************************************
      flst_nborn=0
      do i1=-nflav,nflav
         do i2=-nflav,nflav
            do i3=-nflav,nflav
               do i4=-nflav,nflav
                  condition=(i1+i2).eq.(i3+i4)
                  condition=condition.and.((
     $                 ((i1.eq.i3).or.(i1.eq.i4))
     $                 .and.
     $                 (i2.eq.i3).or.(i2.eq.i4))
     $                 .or.
     $             ((abs(i1).eq.abs(i2)).and.(abs(i3).eq.abs(i4))))
                  if(condition) then
                     do j=1,flst_nborn
c     Check that an inequivalent configuration is generated
                        if(flavequiv(nlegborn,flst_born(1,j),ii(1)))
     $                       goto 20
                     enddo
                     flst_nborn=flst_nborn+1
                     if(flst_nborn.gt.maxprocborn) goto 999
                     do k=1,nlegborn
                        flst_born(k,flst_nborn)=ii(k)
                     enddo
 20                  continue
                  endif
               enddo
            enddo
         enddo
      enddo
      if (debug) then
         write(*,*) ' born processes',flst_nborn
         do j=1,flst_nborn
            write(*,*) (flst_born(k,j),k=1,nlegborn)
         enddo
      endif
 
*********************************************************************
***********             REAL SUBPROCESSES              ***************
*********************************************************************
      flst_nreal=0
      condition=.false.
      do i1=-nflav,nflav
         do i2=-nflav,nflav
            do i3=-nflav,nflav
               do i4=-nflav,nflav 
                  do i5=-nflav,nflav
                     condition=(i1+i2).eq.(i3+i4+i5)
                     condition=condition.and.((three_ch(i1) +
     $                    three_ch(i2)).eq. (three_ch(i3) + three_ch(i4)
     $                    + three_ch(i5)))
                     condition=condition.and.(((((i1.eq.i3).or.(i1.eq.i4
     $                    ).or.(i1.eq.i5)) .or. (i2.eq.i3).or.(i2.eq.i4
     $                    ).or.(i2.eq.i5))).or.((i1+i2.eq.0).and.(i3+i4
     $                    +i5.eq.0)))
                     if(condition) then
                        do j=1,flst_nreal
c     Check that an inequivalent configuration is generated
                           if(flavequiv(nlegreal,flst_real(1,j),ii(1)))
     $                          goto 10
                        enddo
                        flst_nreal=flst_nreal+1
                        if(flst_nreal.gt.maxprocreal) goto 998
                        do k=1,nlegreal
                           flst_real(k,flst_nreal)=ii(k)
                        enddo
 10                     continue
                     endif
                  enddo
               enddo
            enddo
         enddo
      enddo
       if (debug) then
         write(*,*) ' real processes',flst_nreal
         do j=1,flst_nreal
            write(*,*) (flst_real(k,j),k=1,nlegreal)
         enddo
      endif
     
      return
 998  write(*,*) 'init_processes: increase maxprocreal'
      stop
 999  write(*,*) 'init_processes: increase maxprocborn'
      stop
      end
 
 
