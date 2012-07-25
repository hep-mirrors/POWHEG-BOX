      subroutine bornzerodamp(alr,r0,rc,rs,dampfac)
c given the R_alpha region (i.e. the alr) and the associated
c real contribution r (without pdf factor),
c returns in dampfac the damping factor to be applied to
c the real contribution to implement Born zero suppression
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      include 'pwhg_flg.h'
      integer alr
      real * 8 r0,rc,rs,dampfac,h,pwhg_pt2,pt2,powheginput
      logical ini
      data ini/.true./
      save ini,h
      external pwhg_pt2,powheginput
      integer ig,ib,ibbar
      real * 8 par,dg,db,dbbar,Eg,Eb,Ebbar,y
      parameter (par=1d0)
      real * 8 dotp
      external dotp

      real *8 pg(0:3),pb(0:3),betab
      integer mu

      if(ini) then
         h=powheginput("#hfact")
         if(h.gt.0) then
            write(*,*)'***********************************************'
            write(*,*)' Bornzerodamp: Using 5-factor AND h-factor'
            write(*,*)'***********************************************'
	    write(*,*) 'NOT TO BE USED'
	    stop
         else
            write(*,*)'***********************************************'
            write(*,*)' Bornzerodamp: Using 5-factor ONLY'
            write(*,*)'***********************************************'
         endif
         ini=.false.
      endif

      if(h.le.0) then
         if(r0.gt.5*rc.and.r0.gt.5*rs) then
            dampfac=0
         endif
      endif
c      write(*,*) ' bornzerodamp: r0=',r0,'  rc=',rc,'  rs=',rs,
c     # ' dampfac=',dampfac
      end
