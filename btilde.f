      function btilde(xx,www0,ifirst)
      implicit none
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_flg.h'
      include 'include/pwhg_math.h'
c     independent variables for real graph: number of final state
c     legs times 3, take away 4 for 4-momentum conservation, add 2
c     for x_1 and x_2, and take away an overall azimuth
      real * 8 btilde,xx(ndiminteg),www0
      real * 8 xrad(3)
      real * 8 xborn(ndiminteg-3)
      integer ifirst
      real * 8 resborn(maxprocborn),resvirt(maxprocborn),
     #     resreal(maxprocborn),rescoll(maxprocborn)
      real * 8 results(maxprocborn)
      logical negflag
      common /cbbarra/negflag      
      real * 8 tmp,suppfact,www,wwwtot
      integer j
      save resborn,resvirt,wwwtot
      real *8 totborn,totvirt

      www=www0*hc2
      do j=1,ndiminteg-3
         xborn(j)=xx(j)
      enddo
      do j=1,3
         xrad(j)=xx(ndiminteg-3 + j)
      enddo
      if(ifirst.eq.0) then
         wwwtot=www
c     sets born momenta in kin. common block
         call gen_born_phsp(xborn)
         call born_suppression(suppfact)
c set scales
         call setscalesbtilde
         call allborn
c     sets xscaled, y, phi in kinematics common block
         call btildeborn(resborn)
         if (.not.flg_bornonly) then
            call btildevirt(resvirt)
            call btildecoll(xrad,rescoll,www)
            call btildereal(xrad,resreal,www)
         endif
c     accumulate values
         btilde=0
         do j=1,flst_nborn
c     jacobians are already included in rescoll and resreal
            tmp=resborn(j)
            if (.not.flg_bornonly) then
               tmp = tmp   
     #              + resvirt(j)
     #              + rescoll(j) 
     #              + resreal(j)
            endif
c     initial value in results
            results(j)=tmp*www*suppfact
            btilde=btilde+tmp*www*suppfact
         enddo
      elseif(ifirst.eq.1) then
c     subsequent calls:
c     In case of folding the call to btildeborn and btildevirt can be
c     avoided, since results are the same.
c     If the NLO calculation is performed also (flg_nlotest is set)
c     we need to accumulate all weight within a single folding sequence
c     in order to later output the correct Born and Virtual contribution
c     to the NLO analysis routine.
         wwwtot=wwwtot+www
         if (.not.flg_bornonly) then
c btildecoll and btildereal take care themselves to invoke the NLO
c analysis if required.
            call btildecoll(xrad,rescoll,www)
            call btildereal(xrad,resreal,www)
         endif
         btilde=0
         do j=1,flst_nborn
            tmp=resborn(j)
            if (.not.flg_bornonly) then
               tmp = tmp   
     #              + resvirt(j)
     #              + rescoll(j) 
     #              + resreal(j)
            endif
c     accumulate values in results
            results(j)=results(j)+tmp*www*suppfact
            btilde=btilde+tmp*www*suppfact
         enddo
      elseif(ifirst.eq.2) then
         if(flg_nlotest) then
c output Born
            totborn=0d0
            do j=1,flst_nborn
               totborn=totborn+resborn(j)
            enddo
            totborn=totborn*wwwtot
            call analysis_driver(totborn,0)
            if(.not.flg_bornonly) then
c output virtual
               totvirt=0d0
               do j=1,flst_nborn
                  totvirt=totvirt+resvirt(j)
               enddo
               totvirt=totvirt*wwwtot
               call analysis_driver(totvirt,0)
            endif
c closing call to end a sequence of correlated events in the
c analysis routines.
            call pwhgaccumup
         endif
c     closing call: accumulate values with correct signs
         btilde=0
         do j=1,flst_nborn
            if(negflag) then
               if(results(j).lt.0) then
                  results(j)=-results(j)
               else
                  results(j)=0
               endif
            else
               if(results(j).lt.0) then
                  results(j)=0
               endif
            endif
            btilde=btilde+results(j)
c     Transfer all flavour components of btilde to the array
c     in common block; will be used to decide the underlying
c     flavour of the event
            rad_btilde_arr(j)=results(j)
         enddo
      else
         write(*,*) 'wrong value of ifirst in btilde => ',ifirst
         stop
      endif
      end
      
