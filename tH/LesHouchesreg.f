      subroutine gen_leshouches_reg
      implicit none
      include '../include/pwhg_math.h'
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      include '../include/pwhg_rad.h'
      include '../include/LesHouches.h'
      integer ireg
      nup=nlegreal

      do ireg=1,nup
c Remember: gluons are marked 0 here!
         idup(ireg)=flst_regular(ireg,rad_realreg)
         if(ireg.le.2) then
            istup(ireg)=-1
            mothup(1,ireg)=0
            mothup(2,ireg)=0            
         else
            istup(ireg)=1
            mothup(1,ireg)=1
            mothup(2,ireg)=2            
         endif
         spinup(ireg)=9
         vtimup(ireg)=0
      enddo
c     set color connections for all particles
c     neutral particles
c     Higgs Boson         
      icolup(1,3)=0
      icolup(2,3)=0
c     colored particles
      if((idup(1).gt.0).and.(idup(2).lt.0)) then
         icolup(1,1)=501
         icolup(2,1)=0
         icolup(1,2)=0
         icolup(2,2)=502
         icolup(1,4)=501
         icolup(2,4)=0      
         icolup(1,5)=0
         icolup(2,5)=502      
      elseif((idup(1).lt.0).and.(idup(2).gt.0)) then
         icolup(1,1)=0
         icolup(2,1)=502
         icolup(1,2)=501
         icolup(2,2)=0
         icolup(1,4)=501
         icolup(2,4)=0      
         icolup(1,5)=0
         icolup(2,5)=502      
      else
         write(*,*) ' invalid flavour'
         stop
      endif         
c     add resonance 
      call resonances_lh 
c     Don't forget to set scale for scalup equal to the pt of the 
c     radiation (whatever it is now!)
      scalup=sqrt(rad_pt2max)
c
      end
