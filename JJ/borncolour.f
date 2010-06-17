      subroutine borncolour_lh
c Sets up the colour for the given flavour configuration
c already filled in the Les Houches interface.
c In case there are several colour structure, one
c should pick one with a probability proportional to
c the value of the corresponding cross section, for the
c kinematics defined in the Les Houches interface
      implicit none
      include '../include/LesHouches.h'
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      integer iq,ia,iq1,iq2,ia1,ia2,ig1,ig2,j,itmp
      real * 8 s,t,u
      real * 8 dotp
c g g g g
      if(idup(1).eq.21.and.idup(2).eq.21
     1       .and.idup(3).eq.21.and.idup(4).eq.21) then
         s=2*dotp(kn_cmpborn(0,1),kn_cmpborn(0,2))
         t=-2*dotp(kn_cmpborn(0,2),kn_cmpborn(0,3))
         u=-s-t
         call borncolour4g(icolup(1,1),icolup(1,2),
     1                     icolup(1,3),icolup(1,4),s,t,u)
c q qb g g or permutations-crossing
      elseif(idup(1).eq.21.or.idup(2).eq.21
     1       .or.idup(3).eq.21.or.idup(4).eq.21) then
c find the quarks and gluons
         ig1=-1
         do j=1,4
            if(idup(j).eq.21) then
               if(ig1.lt.0) then
                  ig1=j
               else
                  ig2=j
               endif
            elseif(idup(j)*istup(j).gt.0) then
               iq=j
            elseif(idup(j)*istup(j).lt.0) then
               ia=j
            else
               write(*,*) 'borncolour_lh: should not be here!'
               call exit(1)
            endif
         enddo
         s=istup(iq)*istup(ia)*2*dotp(kn_cmpborn(0,iq),kn_cmpborn(0,ia))
         t=istup(ia)*istup(ig1)*2*
     1          dotp(kn_cmpborn(0,ia),kn_cmpborn(0,ig1))
         u=-s-t
         call borncolour2g(icolup(1,iq),icolup(1,ia),
     1        icolup(1,ig1),icolup(1,ig2),s,t,u)
c q q qb qb, or q Q qb Qb, plus permutations-crossing
      else
         iq1=-1
         iq2=-1
         ia1=-1
         ia2=-1
         do j=1,4
            if(idup(j)*istup(j).gt.0) then
               if(iq1.lt.0) then
                  iq1=j
               else
                  iq2=j
               endif
            else
               if(ia1.lt.0) then
                  ia1=j
               else
                  ia2=j
               endif
            endif
         enddo
         if(iq1.eq.iq2) then
c     q q qb qb
            s=istup(iq1)*istup(iq2)*2*
     1          dotp(kn_cmpborn(0,iq1),kn_cmpborn(0,iq2))
            t=istup(iq2)*istup(ia1)*2*
     1          dotp(kn_cmpborn(0,iq2),kn_cmpborn(0,ia1))
            u=-s-t
            call borncolour4q(icolup(1,iq1),icolup(1,iq2),
     1           icolup(1,ia1),icolup(1,ia2),s,t,u)
         else
c     q Q qb Qb
            if(idup(iq1).ne.-idup(ia1)) then
               itmp=ia1
               ia1=ia2
               ia2=itmp
            endif
            call colourjoin4q(icolup(1,iq1),icolup(1,iq2),
     1           icolup(1,ia1),icolup(1,ia2))
         endif
      endif
c Conjugate incoming colours
      call colour_conj(icolup(1,1))
      call colour_conj(icolup(1,2))   
      end

      subroutine borncolour4g(icol1,icol2,icol3,icol4,s,t,u)
c g g g g
      implicit none
      integer icol1(2),icol2(2),icol3(2),icol4(2)
      real * 8 s,t,u
      real * 8 rst,rtu,rsu,r
      real * 8 random
c planar results for st channel
      rst=(t/s+s/t+1)**2
c su channel
      rsu=(u/s+s/u+1)**2
c tu channel
      rtu=(t/u+u/t+1)**2
c Obtained by maxima; check:
c rst+rsu+rtu=2(3-us/t^2-ut/s^2-st/u^2)
      r=random()*(rst+rsu+rtu)
      if(r.lt.rst) then
         call colourjoin4g(icol1,icol2,icol3,icol4)
      elseif(r.lt.rst+rsu) then
         call colourjoin4g(icol1,icol2,icol4,icol3)
      else
         call colourjoin4g(icol1,icol3,icol2,icol4)
      endif
      end

      subroutine colourjoin4g(icol1,icol2,icol3,icol4)
c perform a planar colour connection on the planar sequence
c of gluons
      implicit none
      integer icol1(2),icol2(2),icol3(2),icol4(2)
      integer newcolor
      call getnewcolor(newcolor)
      icol1(2)=newcolor
      icol2(1)=newcolor
      call getnewcolor(newcolor)
      icol2(2)=newcolor
      icol3(1)=newcolor
      call getnewcolor(newcolor)
      icol3(2)=newcolor
      icol4(1)=newcolor
      call getnewcolor(newcolor)
      icol4(2)=newcolor
      icol1(1)=newcolor
      end

      subroutine borncolour2g(icol1,icol2,icol3,icol4,s,t,u)
c                             q     qbar  g     g
c q qb g g
      implicit none
      integer icol1(2),icol2(2),icol3(2),icol4(2)
      real * 8 s,t,u
      real * 8 rt,ru,r
      real * 8 random
c      rt=u/t*(u**2+t**2)/s**2
c      ru=t/u*(u**2+t**2)/s**2
c obtained by maxima; check: rt+ru=(1/(tu)-2/s^2)*(u^2+t^2)
c watch out! crossin a fermion line to get q g->q g needs an
c extra - sign;
      rt=abs(u/t)
      ru=abs(t/u)
      r=random()*(rt+ru)
      if(r.lt.rt) then
         call colourjoin2g(icol1,icol2,icol3,icol4)
      else
         call colourjoin2g(icol1,icol2,icol4,icol3)
      endif
      end

      subroutine colourjoin2g(icol1,icol2,icol3,icol4)
c                             q     qbar  g     g
c perform a planar colour connection on the planar sequence
c q qbar g g
      implicit none
      integer icol1(2),icol2(2),icol3(2),icol4(2)
      integer newcolor
      icol1(2)=0
      icol2(1)=0
      call getnewcolor(newcolor)
      icol1(1)=newcolor
      icol4(2)=newcolor
      call getnewcolor(newcolor)
      icol4(1)=newcolor
      icol3(2)=newcolor
      call getnewcolor(newcolor)
      icol3(1)=newcolor
      icol2(2)=newcolor
      end

      
      subroutine borncolour4q(icol1,icol2,icol3,icol4,s,t,u)
c                             q     q     qbar  qbar
      implicit none
      integer icol1(2),icol2(2),icol3(2),icol4(2)
      real * 8 s,t,u
      real * 8 rt,ru,r
      real * 8 random
c the following is from q Q -> Q q
      rt=(s**2+u**2)/t**2
c q Q->q Q
      ru=(s**2+t**2)/u**2
      r=random()*(rt+ru)
      if(r.lt.rt) then
c t channel gluon (23 channel; thus colour is exchanged
c 2->4 and 1->3
         call colourjoin4q(icol1,icol2,icol4,icol3)
      else
         call colourjoin4q(icol1,icol2,icol3,icol4)
      endif
      end

      subroutine colourjoin4q(icol1,icol2,icol3,icol4)
c                             q     q     qbar  qbar
c perform a planar colour connection on the planar sequence
c q qbar g g
      implicit none
      integer icol1(2),icol2(2),icol3(2),icol4(2)
      integer newcolor
      icol1(2)=0
      icol2(2)=0
      icol3(1)=0
      icol4(1)=0
      call getnewcolor(newcolor)
      icol1(1)=newcolor
      icol4(2)=newcolor
      call getnewcolor(newcolor)
      icol2(1)=newcolor
      icol3(2)=newcolor
      end
