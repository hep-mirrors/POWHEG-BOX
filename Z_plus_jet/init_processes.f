      subroutine init_processes
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      include '../include/pwhg_flg.h'
      include '../include/LesHouches.h'
      integer i1,i2,i3,i4,i5,i6,k,ii(6)
      equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),
     #  (i4,ii(4)),(i5,ii(5)),(i6,ii(6))
      logical debug
      parameter (debug=.false.)
      integer j
      real * 8 powheginput
      external powheginput
c     vector boson id and decay
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode
c     lepton masses
      real *8 lepmass(3),decmass
      common/clepmass/lepmass,decmass
      logical condition

c     check nlegborn. This is only a sanity check while we are TESTING 
c     the code and we change often from one process to the other
      if (nlegborn.ne.5) then
         write(*,*) ' ERROR: set nlegborn to the appropriate value'
         write(*,*) ' for this process in nlegborn.h'
         stop
      endif



c******************************************************
c     Choose the process to be implemented
c******************************************************
c     ID of vector boson produced
      idvecbos=powheginput('idvecbos')
c     decay products of the vector boson
      vdecaymode=powheginput('vdecaymode')

      if (lepmass(1).ne.0.51099891d-3) then
         write(*,*) 'block data lepmass not loaded. stop running' 
         stop
      endif

      if ((vdecaymode.lt.11).or.(vdecaymode.gt.16)) then
         write(*,*) 'ERROR: The decay mode you selected'
     #  //' is not allowed (Up to now only leptonic decays)'
         stop
      endif
            
      if(idvecbos.eq.23) then
         write(*,*) 
         write(*,*) ' POWHEG:  Z+jet production and decay'
         if (vdecaymode.eq.11) write(*,*) '         to e- e+ '
         if (vdecaymode.eq.12) write(*,*) '         to ve ve~ '
         if (vdecaymode.eq.13) write(*,*) '         to mu- mu+ '
         if (vdecaymode.eq.14) write(*,*) '         to vmu vmu~ '
         if (vdecaymode.eq.15) write(*,*) '         to tau- tau+ '
         if (vdecaymode.eq.16) write(*,*) '         to vtau vtau~ '
         write(*,*) 
      else
         write(*,*) 'ERROR: The ID of vector boson you selected'
     #  //' is not admitted (23: Z)'
         stop
      endif

c     change the LHUPI id of the process according to vector boson id
c     and decay
      lprup(1)=10000+vdecaymode ! 10000+idup of first decay product of the Z

      if(lprup(1).eq.10011) then
         decmass=lepmass(1)
      elseif(lprup(1).eq.10012) then
         decmass=0d0   
      elseif(lprup(1).eq.10013) then
         decmass=lepmass(2)
      elseif(lprup(1).eq.10014) then
         decmass=0d0   
      elseif(lprup(1).eq.10015) then
         decmass=lepmass(3)     
      elseif(lprup(1).eq.10016) then
         decmass=0d0   
      else
c     not yet implemented
         write(*,*) 'non leptonic Z decays '//
     #        'not yet implemented'
         stop
      endif

c     index of the first coloured particle in the final state
c     (all subsequent particles are coloured)
      flst_lightpart=5
c     Z decay products
      i3=vdecaymode
      i4=-i3

*********************************************************************
***********            REAL SUBPROCESSES              ***************
*********************************************************************
      i3=11
      i4=-11
      flst_nreal=0
      do i1=-5,5
         do i2=-5,5
            if(i1*i2.gt.0) then
c both quarks or antiquarks
               i5=i1
               i6=i2
               goto 10
            endif
c a quark and a gluon
            if(i1*i2.eq.0) then
               if(i1.ne.0) then
                  i5=i1
                  i6=i2
                  goto 10
               elseif(i2.ne.0) then
                  i5=i2
                  i6=i1
                  goto 10
               endif
            endif
c two gluons or quark antiquark from here on
            if(i1+i2.ne.0) then
c different quark antiquark
               i5=i1
               i6=i2
               goto 10
            endif
c two gluons or quark antiquark of the same flavour
            do i5=0,5
               i6=-i5
c     exclude gg -> Z gg
               if (i1.eq.0.and.i5.eq.0) goto 1234                  
               flst_nreal=flst_nreal+1
               if(flst_nreal.gt.maxprocreal) goto 998
               do k=1,6
                  flst_real(k,flst_nreal)=ii(k)
               enddo
 1234          continue
            enddo
            goto 11
 10         continue
            flst_nreal=flst_nreal+1
            if(flst_nreal.gt.maxprocreal) goto 998
            do k=1,6
               flst_real(k,flst_nreal)=ii(k)
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


*********************************************************************
***********            BORN SUBPROCESSES              ***************
*********************************************************************
      flst_nborn=0
      do i1=-5,5
         do i2=-5,5
            if(i1.ne.0.and.i1+i2.eq.0) then
c q qbar
               i5=0
               goto 20
            endif
c a quark and a gluon
            if(i1*i2.eq.0) then
               if(i1.ne.0) then
                  i5=i1
                  goto 20
               elseif(i2.ne.0) then
                  i5=i2
                  goto 20
               endif
            endif
            goto 21
 20         continue
            flst_nborn=flst_nborn+1
            if(flst_nborn.gt.maxprocborn) goto 999
            do k=1,5
               flst_born(k,flst_nborn)=ii(k)
            enddo
 21         continue
         enddo
      enddo
      if (debug) then
         write(*,*) ' born processes',flst_nborn
         do j=1,flst_nborn
            write(*,*) (flst_born(k,j),k=1,5)
         enddo
      endif
      return
 998  write(*,*) 'init_processes: increase maxprocreal'
      stop
 999  write(*,*) 'init_processes: increase maxprocborn'
      end
 

      block data lepmass_data
      real *8 lepmass(3),decmass
      common/clepmass/lepmass,decmass
      data lepmass /0.51099891d-3,0.1056583668d0,1.77684d0/
      end
