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

      data (flav(i),i=-5,5) 
     #     /'b~','c~','s~','u~','d~','g','d','u','s','c','b'/
c      data (charge(i),i=-5,5) 
c     #     / 0.33333333333333333333d0, !   1d0/3
c     #      -0.66666666666666666667d0, !  -2d0/3
c     #       0.33333333333333333333d0, !   1d0/3 
c     #      -0.66666666666666666667d0, !   -2d0/3
c     #       0.33333333333333333333d0, !   1d0/3 
c     #       0d0,                      !   0d0   
c     #      -0.33333333333333333333d0, !   -1d0/3
c     #       0.66666666666666666667d0, !   2d0/3   
c     #      -0.33333333333333333333d0, !   -1d0/3
c     #       0.66666666666666666667d0, !   2d0/3 
c     #      -0.33333333333333333333d0/ !   -1d0/3



c     index of the first coloured particle in the final state
c     (all subsequent particles are coloured)
      flst_lightpart=5
      i3=-11
      i4=12
c     Born graphs
      flst_nborn=0
      condition=.false.

      max_flav = flst_lightpart

      emit_Wp_upper = .true.
      emit_Wm_upper = .true.
      emit_Wp_lower = .true.
      emit_Wm_lower = .true.
 

      do i=-max_flav,max_flav
         do j=-max_flav,max_flav
            do ii=-max_flav,max_flav
               do jj=-max_flav,max_flav               
                  if (.not.((i.eq.0).or.(j.eq.0).or.
     #                    (ii.eq.0).or.(jj.eq.0))) then ! NOT a gluon
                     ferm_charge(1) = charge3(i)
                     ferm_charge(2) = charge3(j)
                     ferm_charge(3) = charge3(ii)
                     ferm_charge(4) = charge3(jj)
                     
                     ferm_type(1) = signn(i)
                     ferm_type(2) = signn(j)
                     ferm_type(3) = signn(ii)
                     ferm_type(4) = signn(jj)
              
                     condition = 
c     W+ emission from upper leg                        
     #                    (((ferm_charge(1)-(ferm_charge(3)+3)
     #                    .eq.0).and.(emit_Wp_upper)) .and.
c     W- emission from lower leg                        
     #                    ((ferm_charge(2)-(ferm_charge(4)-3)
     #                    .eq.0).and.(emit_Wm_lower)))
     #                    .or.
c     W- emission from upper leg                        
     #                    (((ferm_charge(1)-(ferm_charge(3)-3)
     #                    .eq.0).and.(emit_Wm_upper)) .and.
c     W+ emission from lower leg                        
     #                    ((ferm_charge(2)-(ferm_charge(4)+3)
     #                    .eq.0).and.(emit_Wp_lower)))
                     
                     if (condition) then
                        flst_nborn=flst_nborn+1
                        if(flst_nborn.gt.maxprocborn) goto 999
                        flst_born(1,flst_nborn)=i
                        flst_born(2,flst_nborn)=j
                        flst_born(3,flst_nborn)=23 ! Higgs
                        flst_born(4,flst_nborn)=ii
                        flst_born(5,flst_nborn)=jj
                     endif
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
      

c     Real graphs    
      flst_nreal=0
c     q q -> H q q g
      do i=-max_flav,max_flav
         do j=-max_flav,max_flav
            do ii=-max_flav,max_flav
               do jj=-max_flav,max_flav               
                  if (.not.((i.eq.0).or.(j.eq.0).or.
     #                    (ii.eq.0).or.(jj.eq.0))) then ! NOT a gluon
                     ferm_charge(1) = charge3(i)
                     ferm_charge(2) = charge3(j)
                     ferm_charge(3) = charge3(ii)
                     ferm_charge(4) = charge3(jj)
                     
                     ferm_type(1) = signn(i)
                     ferm_type(2) = signn(j)
                     ferm_type(3) = signn(ii)
                     ferm_type(4) = signn(jj)
                     
                     condition = 
c     W+ emission from upper leg                        
     #                    (((ferm_charge(1)-(ferm_charge(3)+3)
     #                    .eq.0).and.(emit_Wp_upper)) .and.
c     W- emission from lower leg                        
     #                    ((ferm_charge(2)-(ferm_charge(4)-3)
     #                    .eq.0).and.(emit_Wm_lower)))
     #                    .or.
c     W- emission from upper leg                        
     #                    (((ferm_charge(1)-(ferm_charge(3)-3)
     #                    .eq.0).and.(emit_Wm_upper)) .and.
c     W+ emission from lower leg                        
     #                    ((ferm_charge(2)-(ferm_charge(4)+3)
     #                    .eq.0).and.(emit_Wp_lower)))
                     
                     if (condition) then
                        flst_nreal=flst_nreal+1
                        if(flst_nreal.gt.maxprocreal) goto 999
                        flst_real(1,flst_nreal)=i
                        flst_real(2,flst_nreal)=j
                        flst_real(3,flst_nreal)=23 ! Higgs
                        flst_real(4,flst_nreal)=ii
                        flst_real(5,flst_nreal)=jj
                        flst_real(6,flst_nreal)=0 ! gluon
                     endif
                  endif
               enddo
            enddo
         enddo
      enddo
c     g q -> H q q q
c     loop on only HALF of the incoming upper-line quark, not to double count!
c     In fact, the real-radiation term contains TWO Feynman diagrams.
      do i=1,max_flav
         do j=-max_flav,max_flav
            do ii=-max_flav,max_flav
               do jj=-max_flav,max_flav               
                  if (.not.((i.eq.0).or.(j.eq.0).or.
     #                    (ii.eq.0).or.(jj.eq.0))) then ! NOT a gluon
                     ferm_charge(1) = 0
                     ferm_charge(2) = charge3(j)
                     ferm_charge(3) = charge3(ii)
                     ferm_charge(4) = charge3(jj)
                     ferm_charge(7) = charge3(-i)

                     ferm_type(1) = 0
                     ferm_type(2) = signn(j)
                     ferm_type(3) = signn(ii)
                     ferm_type(4) = signn(jj)
                     ferm_type(7) = signn(-i)
                     
                     condition = 
c     W+ emission from upper leg                        
     #                    (((-ferm_charge(7)-(ferm_charge(3)+3)
     #                    .eq.0).and.(emit_Wp_upper)) .and.
c     W- emission from lower leg                        
     #                    ((ferm_charge(2)-(ferm_charge(4)-3)
     #                    .eq.0).and.(emit_Wm_lower)))
     #                    .or.
c     W- emission from upper leg                        
     #                    (((-ferm_charge(7)-(ferm_charge(3)-3)
     #                    .eq.0).and.(emit_Wm_upper)) .and.
c     W+ emission from lower leg                        
     #                    ((ferm_charge(2)-(ferm_charge(4)+3)
     #                    .eq.0).and.(emit_Wp_lower)))
                     
                     if (condition) then
                        flst_nreal=flst_nreal+1
                        if(flst_nreal.gt.maxprocreal) goto 999
                        flst_real(1,flst_nreal)=0 ! gluon
                        flst_real(2,flst_nreal)=j
                        flst_real(3,flst_nreal)=23 ! Higgs
                        flst_real(4,flst_nreal)=ii
                        flst_real(5,flst_nreal)=jj
                        flst_real(6,flst_nreal)=-i
                     endif
                  endif
               enddo
            enddo
         enddo
      enddo

      do i=-max_flav,max_flav
c     loop on only HALF of the incoming lower-line quark, not to double count!
c     In fact, the real-radiation term contains TWO Feynman diagrams.
         do j=1,max_flav             
            do ii=-max_flav,max_flav
               do jj=-max_flav,max_flav               
                  if (.not.((i.eq.0).or.(j.eq.0).or.
     #                    (ii.eq.0).or.(jj.eq.0))) then ! NOT a gluon
                     ferm_charge(1) = charge3(i)
                     ferm_charge(2) = 0
                     ferm_charge(3) = charge3(ii)
                     ferm_charge(4) = charge3(jj)
                     ferm_charge(7) = charge3(-j)
                     
                     ferm_type(1) = signn(i)
                     ferm_type(2) = 0
                     ferm_type(3) = signn(ii)
                     ferm_type(4) = signn(jj)
                     ferm_type(7) = signn(-j)
                     
                     condition = 
c     W+ emission from upper leg                        
     #                    (((ferm_charge(1)-(ferm_charge(3)+3)
     #                    .eq.0).and.(emit_Wp_upper)) .and.
c     W- emission from lower leg                        
     #                    ((-ferm_charge(7)-(ferm_charge(4)-3)
     #                    .eq.0).and.(emit_Wm_lower)))
     #                    .or.
c     W- emission from upper leg                        
     #                    (((ferm_charge(1)-(ferm_charge(3)-3)
     #                    .eq.e0).and.(emit_Wm_upper)) .and.
c     W+ emission from lower leg                        
     #                    ((-ferm_charge(7)-(ferm_charge(4)+3)
     #                    .eq.0).and.(emit_Wp_lower)))
                     if (condition) then
                        flst_nreal=flst_nreal+1
                        if(flst_nreal.gt.maxprocreal) goto 999
                        flst_real(1,flst_nreal)=i
                        flst_real(2,flst_nreal)=0 ! gluon
                        flst_real(3,flst_nreal)=23 ! Higgs
                        flst_real(4,flst_nreal)=ii
                        flst_real(5,flst_nreal)=jj
                        flst_real(6,flst_nreal)=-j
                     endif
                  endif
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
      end
      
