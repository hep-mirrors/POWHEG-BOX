      subroutine setscalesbtlreal
      implicit none
      include 'nlegborn.h'
      include 'pwhg_kn.h'
      include 'pwhg_st.h'
      real * 8 pt2
      logical ini,btlscalereal
      data ini/.true./
      save ini,btlscalereal
      real * 8 pwhg_alphas,powheginput
      external pwhg_alphas,powheginput
      if(ini) then
         if(powheginput("#btlscalereal").gt.0) then
            write(*,*) ' btilde real scale set to max pt'
            btlscalereal=.true.
         else
            btlscalereal=.false.
         endif
         ini=.false.
      endif
      if(btlscalereal) then
         pt2=max(kn_cmpreal(1,7)**2+kn_cmpreal(2,7)**2,10d0)
         st_mufact2= pt2*st_facfact**2
         st_muren2 = pt2*st_renfact**2
         st_alpha  = pwhg_alphas(st_muren2,st_lambda5MSB,st_nlight)
      endif
      end

      subroutine setscalesbtlct
      implicit none
      real * 8 powheginput
      logical ini,btlscalect
      data ini/.true./
      save ini,btlscalect
      external powheginput
      if(ini) then
         if(powheginput("#btlscalect").gt.0) then
            write(*,*) ' counterterm scale set as in Born scale'
            btlscalect=.true.
         else
            btlscalect=.false.
         endif
         ini=.false.
      endif
      if(btlscalect) then
         call setscalesbtilde
      endif
      end
