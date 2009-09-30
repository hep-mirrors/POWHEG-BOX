      subroutine init_processes
      implicit none
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      integer i1,i2,i3,i4,i5,k,ii(nlegreal)
      equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),
     #  (i4,ii(4)),(i5,ii(5))
      logical debug
      parameter (debug=.false.)
      integer j
      integer charge3(-6:6)
      data charge3 /-2,1,-2,1,-2,1,0,-1,2,-1,2,-1,2/
      logical condition
c     check nlegborn
      if (nlegborn.ne.4) then
         write(*,*) ' ERROR: set nlegborn to the appropriate value'
         write(*,*) ' for this process in include/pwhg_flst.h '
         stop
      endif
c     index of the first coloured particle in the final state
c     (all subsequent particles are coloured)
      flst_lightpart=5
      i3=-11
      i4=12
c     Born graphs
      flst_nborn=0
      condition=.false.
      do i1=-5,5
         do i2=-5,5
            condition=(charge3(i1)+charge3(i2)).eq.3
            if(condition) then
c     q qbar'
               flst_nborn=flst_nborn+1
               if(flst_nborn.gt.maxprocborn) goto 999
               do k=1,nlegborn
                  flst_born(k,flst_nborn)=ii(k)
               enddo
            endif
         enddo
      enddo
      if (debug) then
         write(*,*) ' born processes',flst_nborn
         do j=1,flst_nborn
            write(*,*) (flst_born(k,j),k=1,nlegborn)
         enddo
      endif
     
c     Real graphs    
      flst_nreal=0
      condition=.false.
      do i1=-5,5
         do i2=-5,5
            if (abs(i1).eq.abs(i2)) goto 11
            do i5=-5,5
               condition=.false.
               if ((i1.eq.0).and.(i2.ne.0)) then
                  condition=(charge3(i2)-charge3(i5)).eq.3    
               endif
               if ((i2.eq.0).and.(i1.ne.0)) then
                  condition=(charge3(i1)-charge3(i5)).eq.3    
               endif
               if (i5.eq.0) then
                condition=(charge3(i1)+charge3(i2)).eq.3    
               endif   
               if(condition) then
                  flst_nreal=flst_nreal+1
                  if(flst_nreal.gt.maxprocreal) goto 998
                  do k=1,nlegreal
                     flst_real(k,flst_nreal)=ii(k)
                  enddo
               endif
            enddo
 11         continue
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
      end
      
