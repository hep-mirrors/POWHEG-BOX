      subroutine bornzerodamp(alr,r0,rc,rs,dampfac)
c given the R_alpha region (i.e. the alr) and the associated
c real contribution r (without pdf factor),
c returns in dampfac the damping factor to be applied to
c the real contribution to implement Born zero suppression
      implicit none
      integer alr
      real * 8 r0,rc,rs,dampfac,h,pwhg_pt2,pt2,powheginput
      logical ini
      data ini/.true./
      save ini,h
      external pwhg_pt2,powheginput
cccccccccccccccccccccccccccccccccccccccccccccccc
      include 'include/pwhg_flg.h'
      include 'include/pwhg_dbg.h'
c     CAVEAT: Problem when flg_bornonly =.true.!
      if (flg_bornonly) then
         write(*,*) 'ERROR in bornzerodamp:'
         write(*,*) 'if bornolny is set to true  '
         write(*,*) 'this subroutine should not be called'
         call exit(1)
      endif   
c     CAVEAT: Problem when dbg_*test =.true.!
c     when the checklims is called  
      if (dbg_colltest.or.dbg_softtest) then
         write(*,*) 'ERROR in bornzerodamp:'
         write(*,*) 'when doing checklims   '
         write(*,*) 'no damping factor for reals'
         call exit(1)
      endif   
   
cccccccccccccccccccccccccccccccccccccccccccccccc
      if(ini) then
         h=powheginput('#hfact')
      endif
c local variables
      if(h.gt.0) then
         pt2=pwhg_pt2()
         dampfac=h**2/(pt2+h**2)
      else
         dampfac=1
      endif
      if(r0.gt.5*rc.and.r0.gt.5*rs) then
         dampfac=0
      endif
c      write(*,*) ' bornzerodamp: r0=',r0,'  rc=',rc,'  rs=',rs,
c     # ' dampfac=',dampfac
      end
