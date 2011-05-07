      subroutine init_processes
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      include 'LesHouches.h'
      integer i1,i2,i3,i4,i5,i6,i7,k,ii(7)
      equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),
     #  (i4,ii(4)),(i5,ii(5)),(i6,ii(6)),(i7,ii(7))
      logical debug
      parameter (debug=.true.)
      integer j
      integer charge3(-6:6)
      data charge3 /-2,1,-2,1,-2,1,0,-1,2,-1,2,-1,2/
      real * 8 powheginput
      external powheginput
c     vector boson id and decay
c     lepton masses
      include 'cvecbos.h'
      real *8 lepmass(3),decmass,decmass1,decmass2
      common/clepmass/lepmass,decmass,decmass1,decmass2
      logical condition

      !TM WHAT IS THIS?
      integer lprup1(100),lprup2(100)

c******************************************************
c     Choose the process to be implemented
c******************************************************

c     ID of vector boson produced
c     decay products of the vector boson
      vdecaymode1=powheginput('vdecaymode1')
      vdecaymode2=powheginput('vdecaymode2')

      if(vdecaymode1.eq.vdecaymode2) then
         vsymfact=0.5d0
      else
         vsymfact=1
      endif

      idvecbos1 = 23
      idvecbos2 = 23

      lprup(1) = 10013

      if (lepmass(1).ne.0.51099891d-3) then
         write(*,*) 'block data lepmass not loaded. stop running' 
         call exit(-1)
      endif

      if ((vdecaymode1.lt.11).or.(vdecaymode1.gt.16)
     # .or.(vdecaymode2.lt.11).or.(vdecaymode2.gt.16)) then
         write(*,*) 'ERROR: The decay mode you selected'
     #  //' is not allowed (Up to now only leptonic decays)'
         call exit(-1)
      endif
            
      if((idvecbos1.eq.23).and.(idvecbos2.eq.23)) then
         write(*,*) 
         write(*,*) ' POWHEG: ZZ production and decay'
         if (vdecaymode1.eq.11) write(*,*) '         to e- e+ '
         if (vdecaymode1.eq.12) write(*,*) '         to ve ve~ '
         if (vdecaymode1.eq.13) write(*,*) '         to mu- mu+ '
         if (vdecaymode1.eq.14) write(*,*) '         to vmu vmu~ '
         if (vdecaymode1.eq.15) write(*,*) '         to tau- tau+ '
         if (vdecaymode1.eq.16) write(*,*) '         to vtau vtau~ '
         write(*,*)'            and'
         if (vdecaymode2.eq.11) write(*,*) '            e- e+ '
         if (vdecaymode2.eq.12) write(*,*) '            ve ve~ '
         if (vdecaymode2.eq.13) write(*,*) '            mu- mu+ '
         if (vdecaymode2.eq.14) write(*,*) '            vmu vmu~ '
         if (vdecaymode2.eq.15) write(*,*) '            tau- tau+ '
         if (vdecaymode2.eq.16) write(*,*) '            vtau vtau~ '
         write(*,*) 
      else
         write(*,*) 'ERROR: The ID of vector boson you selected'
     #  //' is not admitted (23: Z)'
         stop
      endif

c     change the LHUPI id of the process according to vector boson id
c     and decay
      lprup1(1)=10000+vdecaymode1 ! 10000+idup of first decay product of Z1
      lprup2(1)=10000+vdecaymode2 ! 10000+idup of first decay product of Z2

      if(lprup1(1).eq.10011) then
         decmass1=lepmass(1)
      elseif(lprup1(1).eq.10012) then
         decmass1=0d0   
      elseif(lprup1(1).eq.10013) then
         decmass1=lepmass(2)
      elseif(lprup1(1).eq.10014) then
         decmass1=0d0   
      elseif(lprup1(1).eq.10015) then
         decmass1=lepmass(3)     
      elseif(lprup1(1).eq.10016) then
         decmass1=0d0   
      else
c     not yet implemented
         write(*,*) 'non leptonic Z decays '//
     #        'not yet implemented'
         stop
      endif
      if(lprup2(1).eq.10011) then
         decmass2=lepmass(1)
      elseif(lprup2(1).eq.10012) then
         decmass2=0d0   
      elseif(lprup2(1).eq.10013) then
         decmass2=lepmass(2)
      elseif(lprup2(1).eq.10014) then
         decmass2=0d0   
      elseif(lprup2(1).eq.10015) then
         decmass2=lepmass(3)     
      elseif(lprup2(1).eq.10016) then
         decmass2=0d0   
      else
c     not yet implemented
         write(*,*) 'non leptonic Z decays '//
     #        'not yet implemented'
         stop
      endif


c     index of the first coloured particle in the final state
c     (all subsequent particles are coloured)
      flst_lightpart=7
c     Z decay products
      i3=vdecaymode1
      i4=-i3
      i5=vdecaymode2
      i6=-i5

*********************************************************************
***********            BORN SUBPROCESSES              ***************
*********************************************************************
      flst_nborn=0
      do i1=-5,5
         do i2=-5,5
            if(i1.ne.0.and.i1+i2.eq.0) then
c     q qbar
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

*********************************************************************
***********            REAL SUBPROCESSES              ***************
*********************************************************************
      flst_nreal=0
      do i1=-5,5
         do i2=-5,5
            do i7=-5,5
               condition=.false.
               if(.not.(i1.eq.0.and.i2.eq.0)) then
c     exclude gg
                  if((i1.ne.0).and.(i1+i2.eq.0).and.(i7.eq.0)) then
c     q qbar -> g
                     condition=.true.
                  elseif((i1.eq.0).and.(i2.eq.i7)) then
c     g q
                     condition=.true.
                  elseif((i2.eq.0).and.(i1.eq.i7)) then
c     q g
                     condition=.true.
                  endif
               endif
               if(condition) then
                  flst_nreal=flst_nreal+1
                  if(flst_nreal.gt.maxprocreal) goto 998
                  do k=1,nlegreal
                     flst_real(k,flst_nreal)=ii(k)
                  enddo
               endif
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
      call exit(-1)
 999  write(*,*) 'init_processes: increase maxprocborn'
      call exit(-1)
      end
 

      block data lepmass_data
      real *8 lepmass(3),decmass
      common/clepmass/lepmass,decmass
      data lepmass /0.51099891d-3,0.1056583668d0,1.77684d0/
      end
