      subroutine setborn(p,bflav,born,bornjk,bmunu)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_kn.h'
      include 'PhysPars.h'

      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 p(0:3,nlegs),bornjk(nlegs,nlegs)
      integer bflav(nlegs)
      real * 8 bmunu(0:3,0:3,nlegs),born
      real *8 borntmp
      integer mu,nu,j,k

      integer ileg,ioleg
      real *8 kb_mad(0:3,nlegs)
C     define a real *8 value for nc in order
C     to avoid integer by integer division
      real *8 ncol
      parameter (ncol=nc)
      real *8 VC
      parameter(VC=ncol**2-1)

      real * 8 gtens(0:3,0:3),ap
      data gtens/1d0, 0d0, 0d0, 0d0,
     #           0d0,-1d0, 0d0, 0d0,
     #           0d0, 0d0,-1d0, 0d0,
     #           0d0, 0d0, 0d0,-1d0/
      save gtens

      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 Hat,Hau,HB,HCs,HCt,HCu,HDs,HDt,HDu,dotp
      external Hat,Hau,HB,HCs,HCt,HCu,HDs,HDt,HDu,dotp
      real * 8 symfac

   
c     set madgraph parameters that can change on an event-by-event basis
      call mad_setparam

      do ileg=1,4
         do mu=0,3
            kb_mad(mu,ileg)=p(mu,ileg)
         enddo
      enddo
c     to avoid bugs in HELAS, restore exact masslessness of incoming partons 
      kb_mad(0,1)=dabs(kb_mad(3,1))
      kb_mad(0,2)=dabs(kb_mad(3,2))

c$$$c    also outgoing ?    
c$$$       kb_mad(0,3)=sqrt(dabs(kb_mad(1,3)**2+kb_mad(2,3)**2+kb_mad(3,3)
c$$$     $     **2))
c$$$      kb_mad(0,4)=sqrt(dabs(kb_mad(1,4)**2+kb_mad(2,4)**2+kb_mad(3,4)
c$$$     $     **2))


      call compborn(kb_mad,bflav,borntmp)
      born=borntmp

c Colour factors for colour-correlated Born amplitudes;
c Rule from Kunszt-Soper.
c First, identify the flavour structure

c     A-type, qq'->qq'
      if(
     $(bflav(1)*bflav(2)*bflav(3).ne.0).and.
     $(abs(bflav(1)).ne.abs(bflav(2))).and.
     $(bflav(1).eq.bflav(3)).and.
     $(bflav(2).eq.bflav(4))) then

         if((bflav(1).gt.0).and.(bflav(2).gt.0)) then
c         write(*,*) 'A1a, qqp->qqp ', (qcd_name(bflav(ileg)),ileg=1,4)
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=-p(mu,4)
         enddo
         
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
         bornjk(2,4)=bornjk(1,3)
         bornjk(2,3)=bornjk(1,4)
         bornjk(3,4)=bornjk(1,2)


         elseif((bflav(1).lt.0).and.(bflav(2).gt.0)) then
c         write(*,*) 'A1b, qbarqp->qbarqp ', (qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     1 -> 3
c     3 -> 1
         do mu=0,3
            p1(mu)=-p(mu,3)
            p2(mu)=p(mu,2)
            p3(mu)=p(mu,1)
            p4(mu)=-p(mu,4)
         enddo
         
c         bornjk(3,2)=(4.*pi*st_alpha)**2 * ......
         bornjk(2,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
c         bornjk(3,1)=(4.*pi*st_alpha)**2 * .....
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(3,4)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
c         bornjk(2,4)=bornjk(3,1)
         bornjk(2,4)=bornjk(1,3)
c         bornjk(2,1)=bornjk(3,4)
         bornjk(1,2)=bornjk(3,4)
c         bornjk(1,4)=bornjk(3,2)
         bornjk(1,4)=bornjk(2,3)


         elseif((bflav(1).gt.0).and.(bflav(2).lt.0)) then
c         write(*,*) 'A1c, qqbarp->qqbarp ', (qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     2 -> 4
c     4 -> 2
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,4)
            p3(mu)=-p(mu,3)
            p4(mu)=p(mu,2)
         enddo
         
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
c         bornjk(4,2)=bornjk(1,3)
         bornjk(2,4)=bornjk(1,3)
c         bornjk(4,3)=bornjk(1,2)
         bornjk(3,4)=bornjk(1,2)
c         bornjk(3,2)=bornjk(1,4)
         bornjk(2,3)=bornjk(1,4)


         elseif((bflav(1).lt.0).and.(bflav(2).lt.0)) then
c         write(*,*) 'A1d, qbar qbarp->qbar qbarp ', (qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     1 -> 3
c     2 -> 4
c     3 -> 1
c     4 -> 2
         do mu=0,3
            p1(mu)=-p(mu,3)
            p2(mu)=-p(mu,4)
            p3(mu)=p(mu,1)
            p4(mu)=p(mu,2)
         enddo

         bornjk(3,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol  
c         bornjk(3,1)=(4.*pi*st_alpha)**2 * 
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol   
c         bornjk(3,2)=(4.*pi*st_alpha)**2 * 
         bornjk(2,3)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol  
c         bornjk(4,2)=bornjk(3,1)
         bornjk(2,4)=bornjk(1,3)
c         bornjk(4,1)=bornjk(3,2)
         bornjk(1,4)=bornjk(2,3)
c         bornjk(1,2)=bornjk(3,4)
         bornjk(1,2)=bornjk(3,4)

      else
         write(*,*) 'Error in Born.f, A1-type eikonal'
         call exit(1)
      endif



c     A-type, qq'->q'q
      elseif(
     $(bflav(1)*bflav(2)*bflav(3).ne.0).and.
     $(abs(bflav(1)).ne.abs(bflav(2))).and.
     $(bflav(1).eq.bflav(4)).and.
     $(bflav(2).eq.bflav(3))) then


         if((bflav(1).gt.0).and.(bflav(3).gt.0)) then
c         write(*,*) 'A2a, qqp->qpq ', (qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     3 -> 4
c     4 -> 3
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,4)
            p4(mu)=-p(mu,3)
         enddo
         
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
         bornjk(2,3)=bornjk(1,4)
         bornjk(2,4)=bornjk(1,3)
         bornjk(3,4)=bornjk(1,2)


         elseif((bflav(1).gt.0).and.(bflav(3).lt.0)) then
c         write(*,*) 'A2b, q qbarp->qbarp q ', 
c     $           (qcd_name(bflav(ileg)),ileg=1,4)
c     As before, but exchanging 2 with 3 ONLY in the Bjk, AND NOT in the
c     momenta assignment.
c     !ER: non ho capito bene perche', ma cosi funziona.
c     Permutando dalla fondamentale, NON VA.
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,4)
            p4(mu)=-p(mu,3)
         enddo
         
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
         bornjk(2,3)=bornjk(1,4)
         bornjk(3,4)=bornjk(1,2)
         bornjk(2,4)=bornjk(1,3)


         elseif((bflav(1).lt.0).and.(bflav(3).lt.0)) then
c         write(*,*) 'A2b, qbar qbarp->qbarp qbar ', 
c     $           (qcd_name(bflav(ileg)),ileg=1,4)
c     As before, but exchanging 1 with 4 ONLY in the Bjk, AND NOT in the
c     momenta assignment.
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,4)
            p4(mu)=-p(mu,3)
         enddo
         
c         bornjk(4,3)=(4.*pi*st_alpha)**2 * ......
         bornjk(3,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
c         bornjk(4,2)=(4.*pi*st_alpha)**2 * ........
         bornjk(2,4)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
c         bornjk(2,3)=bornjk(4,1)
         bornjk(2,3)=bornjk(1,4)
c         bornjk(3,1)=bornjk(4,2)
         bornjk(1,3)=bornjk(2,4)
c         bornjk(2,1)=bornjk(4,3)
         bornjk(1,2)=bornjk(3,4)





      else
         write(*,*) 'Error in Born.f, A2-type eikonal'
         call exit(1)
      endif



c     A-type, qqbar->q'qbar'
      elseif(
     $(bflav(1)*bflav(2)*bflav(3).ne.0).and.
     $(bflav(1).eq.-bflav(2)).and.
     $(bflav(3).eq.-bflav(4)).and.
     $(abs(bflav(1)).ne.abs(bflav(3)))) then

         if((bflav(1).gt.0).and.(bflav(3).gt.0)) then
c      write(*,*) 'A3a, qqbar->qp qpbar ',(qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     4 -> 2
c     3 -> 4
c     2 -> 3
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,4)
            p3(mu)=p(mu,2)
            p4(mu)=-p(mu,3)
         enddo
         
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
         bornjk(3,4)=bornjk(1,2)
c         bornjk(3,2)=bornjk(1,4)
         bornjk(2,3)=bornjk(1,4)
c         bornjk(4,2)=bornjk(1,3)
         bornjk(2,4)=bornjk(1,3)


         elseif((bflav(1).gt.0).and.(bflav(3).lt.0)) then
c      write(*,*) 'A3b, qqbar->qpbar qp ',(qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     3 -> 2
c     2 -> 3
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,3)
            p3(mu)=p(mu,2)
            p4(mu)=-p(mu,4)
         enddo
         
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
         bornjk(3,4)=bornjk(1,2)
c         bornjk(3,2)=bornjk(1,4)
         bornjk(2,3)=bornjk(1,4)
         bornjk(2,4)=bornjk(1,3)


         elseif((bflav(1).lt.0).and.(bflav(3).gt.0)) then
c      write(*,*) 'A3c, qbar q->qp qpbar ',(qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     2 -> 1
c     4 -> 2
c     1 -> 3
c     3 -> 4
         do mu=0,3
            p1(mu)=p(mu,2)
            p2(mu)=-p(mu,4)
            p3(mu)=p(mu,1)
            p4(mu)=-p(mu,3)
         enddo
         
c         bornjk(3,1)=(4.*pi*st_alpha)**2 * ..........
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(3,4)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
c         bornjk(3,2)=(4.*pi*st_alpha)**2 *........ 
         bornjk(2,3)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
         bornjk(1,2)=bornjk(3,4)
c         bornjk(1,4)=bornjk(3,2)
         bornjk(1,4)=bornjk(2,3)
c         bornjk(4,2)=bornjk(3,1)
         bornjk(2,4)=bornjk(1,3)


         elseif((bflav(1).lt.0).and.(bflav(3).lt.0)) then
c      write(*,*) 'A3d, qbar q->qpbar qp ',
c     $           (qcd_name(bflav(ileg)),ileg=1,4)
c     As before, but exchanging 3 with 4 ONLY in the Bjk, AND NOT in the
c     momenta assignment.
c     !ER: non ho capito bene perche', ma cosi funziona.
c     Permutando dalla fondamentale, NON VA.
         do mu=0,3
            p1(mu)=p(mu,2)
            p2(mu)=-p(mu,4)
            p3(mu)=p(mu,1)
            p4(mu)=-p(mu,3)
         enddo
         
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc*HAt(p1,p2,p3,p4))     /4./ncol/ncol
         bornjk(3,4)=(4.*pi*st_alpha)**2 * 
     $        (-VC/nc*HAt(p1,p2,p3,p4))       /4./ncol/ncol
         bornjk(2,4)=(4.*pi*st_alpha)**2 * 
     $        (VC/nc*(nc**2-2)*HAt(p1,p2,p3,p4))      /4./ncol/ncol
         bornjk(1,2)=bornjk(3,4)
         bornjk(1,3)=bornjk(2,4)
         bornjk(2,3)=bornjk(1,4)


      else
         write(*,*) 'Error in Born.f, A3-type eikonal'
         call exit(1)
      endif




c     B-type, qq->qq
      elseif((bflav(1).eq.bflav(2)).and.
     $(bflav(1).eq.bflav(3)).and.
     $(bflav(1).eq.bflav(4)).and.
     $(bflav(1).ne.0)) then
c      write(*,*) 'B1a, qq->qq ',(qcd_name(bflav(ileg)),ileg=1,4)

c     no exchange wrt fundamental order:
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=-p(mu,4)
         enddo
         
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(-HB(p1,p2,p3,p4)+nc*(HAt(p1,p2,p3,p4)+HAu(p1,p2,p3,p4))
     $-nc**2*HB(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(HB(p1,p2,p3,p4)-nc*(HAu(p1,p2,p3,p4)+0.5*HAt(p1,p2,p3,p4))
     $+0.5*nc**3*HAu(p1,p2,p3,p4))
     $     /4./ncol/ncol


         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(HB(p1,p2,p3,p4)-nc*(HAt(p1,p2,p3,p4)+0.5*HAu(p1,p2,p3,p4))
     $+0.5*nc**3*HAt(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(2,4)=bornjk(1,3)
         bornjk(2,3)=bornjk(1,4)
         bornjk(3,4)=bornjk(1,2)


c     B-type, qqbar->qqbar
      elseif((bflav(1).eq.-bflav(2)).and.
     $(bflav(1).eq.bflav(3)).and.
     $(bflav(1).eq.-bflav(4)).and.
     $(bflav(1).ne.0)) then
c      write(*,*) 'B2, qqbar->qqbar ',(qcd_name(bflav(ileg)),ileg=1,4)

c     exchange wrt fundamental order:
c     2 -> 4
c     4 -> 2
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,4)
            p3(mu)=-p(mu,3)
            p4(mu)=p(mu,2)
         enddo
         
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(-HB(p1,p2,p3,p4)+nc*(HAt(p1,p2,p3,p4)+HAu(p1,p2,p3,p4))
     $-nc**2*HB(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(HB(p1,p2,p3,p4)-nc*(HAu(p1,p2,p3,p4)+0.5*HAt(p1,p2,p3,p4))
     $+0.5*nc**3*HAu(p1,p2,p3,p4))
     $     /4./ncol/ncol


         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(HB(p1,p2,p3,p4)-nc*(HAt(p1,p2,p3,p4)+0.5*HAu(p1,p2,p3,p4))
     $+0.5*nc**3*HAt(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(2,4)=bornjk(1,3)
c         bornjk(4,3)=bornjk(1,2)
         bornjk(3,4)=bornjk(1,2)
c         bornjk(3,2)=bornjk(1,4)
         bornjk(2,3)=bornjk(1,4)


c     B-type, qqbar->qbar q
      elseif((bflav(1).eq.-bflav(2)).and.
     $(bflav(1).eq.-bflav(3)).and.
     $(bflav(1).eq.bflav(4)).and.
     $(bflav(1).ne.0)) then
c      write(*,*) 'B3, qqbar->qbar q ',(qcd_name(bflav(ileg)),ileg=1,4)


c     exchange wrt fundamental order:
c     2 -> 3
c     3 -> 2
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,3)
            p3(mu)=p(mu,2)
            p4(mu)=-p(mu,4)
         enddo
         
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(-HB(p1,p2,p3,p4)+nc*(HAt(p1,p2,p3,p4)+HAu(p1,p2,p3,p4))
     $-nc**2*HB(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(HB(p1,p2,p3,p4)-nc*(HAu(p1,p2,p3,p4)+0.5*HAt(p1,p2,p3,p4))
     $+0.5*nc**3*HAu(p1,p2,p3,p4))
     $     /4./ncol/ncol


         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC/nc/nc) *
     $(HB(p1,p2,p3,p4)-nc*(HAt(p1,p2,p3,p4)+0.5*HAu(p1,p2,p3,p4))
     $+0.5*nc**3*HAt(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(3,4)=bornjk(1,2)
c         bornjk(3,2)=bornjk(1,4)
         bornjk(2,3)=bornjk(1,4)
         bornjk(2,4)=bornjk(1,3)


c     C-type, qqbar->gg
      elseif((bflav(1).eq.-bflav(2)).and.
     $(bflav(3)*bflav(4).eq.0).and.
     $(bflav(1).ne.0)) then
c         write(*,*) 'C1, qqbar->gg ', (qcd_name(bflav(ileg)),ileg=1,4)
c     no exchange wrt fundamental order:
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=-p(mu,4)
         enddo
         
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(-(HCt(p1,p2,p3,p4)+HCu(p1,p2,p3,p4)-HCs(p1,p2,p3,p4))
     $+1./nc/nc * HCs(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCu(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCt(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/ncol

         bornjk(2,4)=bornjk(1,3)
         bornjk(2,3)=bornjk(1,4)
         bornjk(3,4)=(4.*pi*st_alpha)**2 * 
     $        ( VC*nc**2) *
     $(HCt(p1,p2,p3,p4) + HCu(p1,p2,p3,p4))
     $     /4./ncol/ncol


c     C-type, qg->qg
      elseif((bflav(1).ne.0).and.
     $(bflav(2).eq.0).and.
     $(bflav(3).ne.0).and.
     $(bflav(4).eq.0)) then
c         write(*,*) 'C2, qg->qg ', (qcd_name(bflav(ileg)),ileg=1,4)

c     exchange wrt fundamental order (qqb-gg):
c     3 -> 2
c     2 -> 3
c     Here also a - sign (a gluon is crossed) and
c     a change in the color-spin average is needed
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,3)
            p3(mu)=p(mu,2)
            p4(mu)=-p(mu,4)
         enddo
         
         bornjk(1,3)= -  (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(-(HCt(p1,p2,p3,p4)+HCu(p1,p2,p3,p4)-HCs(p1,p2,p3,p4))
     $+1./nc/nc * HCs(p1,p2,p3,p4))
     $     /4./ncol/VC                                         

         bornjk(1,2)= -  (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCu(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC                                          

         bornjk(1,4)= -  (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCt(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC                                          

         bornjk(3,4)=bornjk(1,2)
c         bornjk(3,2)=bornjk(1,4)
         bornjk(2,3)=bornjk(1,4)
         bornjk(2,4)= -  (4.*pi*st_alpha)**2 * 
     $        ( VC*nc**2) *
     $(HCt(p1,p2,p3,p4) + HCu(p1,p2,p3,p4))
     $     /4./ncol/VC                                          


c     C-type, qg->gq
      elseif((bflav(1).ne.0).and.
     $(bflav(2).eq.0).and.
     $(bflav(3).eq.0).and.
     $(bflav(4).ne.0)) then
c         write(*,*) 'C3, qg->gq ', (qcd_name(bflav(ileg)),ileg=1,4)

c     exchange wrt fundamental order (qqb-gg):
c     4 -> 2
c     2 -> 4
c     Here also a - sign (a gluon is crossed) and
c     a change in the color-spin average is needed
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,4)
            p3(mu)=-p(mu,3)
            p4(mu)=p(mu,2)
         enddo

         bornjk(1,4)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(-(HCt(p1,p2,p3,p4)+HCu(p1,p2,p3,p4)-HCs(p1,p2,p3,p4))
     $+1./nc/nc * HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

         bornjk(1,3)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCu(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

         bornjk(1,2)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCt(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

         bornjk(2,4)=bornjk(1,3)
c         bornjk(4,3)=bornjk(1,2)
         bornjk(3,4)=bornjk(1,2)
c         bornjk(3,2)=(4.*pi*st_alpha)**2 * ......
         bornjk(2,3)= - (4.*pi*st_alpha)**2 * 
     $        ( VC*nc**2) *
     $(HCt(p1,p2,p3,p4) + HCu(p1,p2,p3,p4))
     $     /4./ncol/VC


c     C-type, gg->qqbar
      elseif((bflav(3).eq.-bflav(4)).and.
     $(bflav(1)*bflav(2).eq.0).and.
     $(bflav(3).ne.0)) then
c         write(*,*) 'C4, gg->qqbar ', (qcd_name(bflav(ileg)),ileg=1,4)

c     exchange wrt fundamental order (qqb-gg):
c     4 -> 1
c     3 -> 2
c     1 -> 3
c     2 -> 4
c     Here a change in the color-spin average is needed

         do mu=0,3
            p1(mu)=-p(mu,4)
            p2(mu)=-p(mu,3)
            p3(mu)=p(mu,1)
            p4(mu)=p(mu,2)
         enddo


         bornjk(3,4)=(4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(-(HCt(p1,p2,p3,p4)+HCu(p1,p2,p3,p4)-HCs(p1,p2,p3,p4))
     $+1./nc/nc * HCs(p1,p2,p3,p4))
     $     /4./VC/VC

c         bornjk(3,2)=(4.*pi*st_alpha)**2 * ....
         bornjk(2,3)=(4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCu(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./VC/VC

c         bornjk(3,1)=(4.*pi*st_alpha)**2 * ......
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCt(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./VC/VC

c         bornjk(4,1)=bornjk(3,2)
         bornjk(1,4)=bornjk(2,3)
c         bornjk(4,2)=bornjk(3,1)
         bornjk(2,4)=bornjk(1,3)
c         bornjk(2,1)=(4.*pi*st_alpha)**2 *..... 
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( VC*nc**2) *
     $(HCt(p1,p2,p3,p4) + HCu(p1,p2,p3,p4))
     $     /4./VC/VC


c     C-type, gqbar->qbarg
      elseif((bflav(1).eq.0).and.
     $(bflav(2).ne.0).and.
     $(bflav(3).ne.0).and.
     $(bflav(4).eq.0)) then
c         write(*,*) 'C5, gqbar->qbarg ',(qcd_name(bflav(ileg)),ileg=1,4)

c     exchange wrt fundamental order (qqb-gg):
c     1 -> 3
c     3 -> 1
c     Here also a - sign (a gluon is crossed) and
c     a change in the color-spin average is needed
         do mu=0,3
            p1(mu)=-p(mu,3)
            p2(mu)=p(mu,2)
            p3(mu)=p(mu,1)
            p4(mu)=-p(mu,4)
         enddo

c         bornjk(3,2)=(4.*pi*st_alpha)**2 * ......
         bornjk(2,3)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(-(HCt(p1,p2,p3,p4)+HCu(p1,p2,p3,p4)-HCs(p1,p2,p3,p4))
     $+1./nc/nc * HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

c         bornjk(3,1)=(4.*pi*st_alpha)**2 * ......
         bornjk(1,3)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCu(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

         bornjk(3,4)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCt(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

c         bornjk(2,4)=bornjk(3,1)
         bornjk(2,4)=bornjk(1,3)
c         bornjk(2,1)=bornjk(3,4)
         bornjk(1,2)=bornjk(3,4)
         bornjk(1,4)= - (4.*pi*st_alpha)**2 * 
     $        ( VC*nc**2) *
     $(HCt(p1,p2,p3,p4) + HCu(p1,p2,p3,p4))
     $     /4./ncol/VC



c     C-type, gq->gq
      elseif((bflav(1).eq.0).and.
     $(bflav(2).ne.0).and.
     $(bflav(3).eq.0).and.
     $(bflav(4).ne.0)) then
c         write(*,*) 'C6, gq->gq ', (qcd_name(bflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order (qqb-gg):
c     1 -> 4
c     4 -> 1
c     Here also a - sign (a gluon is crossed) and
c     a change in the color-spin average is needed
         do mu=0,3
            p1(mu)=-p(mu,4)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=p(mu,1)
         enddo
         
c         bornjk(4,2)=(4.*pi*st_alpha)**2 * .........
         bornjk(2,4)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(-(HCt(p1,p2,p3,p4)+HCu(p1,p2,p3,p4)-HCs(p1,p2,p3,p4))
     $+1./nc/nc * HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

c         bornjk(4,3)=(4.*pi*st_alpha)**2 * .........
         bornjk(3,4)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCu(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

c         bornjk(4,1)=(4.*pi*st_alpha)**2 * ........
         bornjk(1,4)= - (4.*pi*st_alpha)**2 * 
     $        ( VC) *
     $(nc**2 *HCt(p1,p2,p3,p4) - HCs(p1,p2,p3,p4))
     $     /4./ncol/VC

c         bornjk(2,1)=bornjk(4,3)
         bornjk(1,2)=bornjk(3,4)
c         bornjk(2,3)=bornjk(4,1)
         bornjk(2,3)=bornjk(1,4)
c         bornjk(3,1)=(4.*pi*st_alpha)**2 * .......
         bornjk(1,3)= - (4.*pi*st_alpha)**2 * 
     $        ( VC*nc**2) *
     $(HCt(p1,p2,p3,p4) + HCu(p1,p2,p3,p4))
     $     /4./ncol/VC



c     D-type
      elseif((bflav(1).eq.0).and.
     $(bflav(2).eq.0).and.
     $(bflav(3).eq.0).and.
     $(bflav(4).eq.0)) then

c         write(*,*) 'D ', (qcd_name(bflav(ileg)),ileg=1,4)

         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=-p(mu,4)
         enddo
         
         bornjk(1,2)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC*nc**3*HDs(p1,p2,p3,p4)) /4./VC/VC
         bornjk(1,3)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC*nc**3*HDt(p1,p2,p3,p4)) /4./VC/VC
         bornjk(1,4)=(4.*pi*st_alpha)**2 * 
     $        ( 2*VC*nc**3*HDu(p1,p2,p3,p4)) /4./VC/VC

         bornjk(2,4)=bornjk(1,3)
         bornjk(2,3)=bornjk(1,4)
         bornjk(3,4)=bornjk(1,2)        

      else
         write(*,*) 'Error in clbs'
         call exit(1)
      endif



c     Bkj=Bjk
      do j=1,nlegborn
c     bornjk(j,j) is not used in soft
         bornjk(j,j)=0d0
         do k=j+1,nlegborn
            bornjk(k,j)=bornjk(j,k)
         enddo
      enddo

c normalize: kunszt and Soper have an extra 2, see eq A8 and A11 in
c PRD46-192
      if(bflav(3).eq.bflav(4)) then
         symfac=0.5d0
      else
         symfac=1
      endif
      do j=1,nlegborn
         do k=1,nlegborn
c     bornjk(j,j) is not used in soft
            bornjk(j,k)=bornjk(j,k)/2*symfac
         enddo
      enddo

      

c     spin-projected here are not needed 
      do ileg=1,nlegborn
         if(bflav(ileg).eq.0) then
c find opposite leg
            if(ileg.eq.1) then
               ioleg=2
            elseif(ileg.eq.2) then
               ioleg=1
            elseif(ileg.eq.3) then
               ioleg=4
            elseif(ileg.eq.4) then
               ioleg=3
            endif
            do mu=0,3
               do nu=0,3
c     Since we use a modified version of sigcollsoft, we do not need
c     bmunu anymore
                  bmunu(mu,nu,ileg)=(-gtens(mu,nu)+
     1           kn_cmpborn(mu,ileg)*kn_cmpborn(nu,ioleg)/
     2            dotp(kn_cmpborn(0,ileg),kn_cmpborn(0,ioleg)))*born/2
               enddo
            enddo
         endif
      enddo
      

      end


C THE FOLLOWIG FUNCTIONS ARE TAKEN FROM KUNSZT-SOPER PHYS.REV. D46,1 192 

      function HAt(p1,p2,p3,p4)
      real *8 Hat
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)

      HAt=2.*(s**2+u**2)/t**2
      return
      end


      function HAu(p1,p2,p3,p4)
      real *8 Hau
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)

      HAu=2.*(s**2+t**2)/u**2
      return
      end

      function HB(p1,p2,p3,p4)
      real *8 HB
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)

      HB=2.*s**2/t/u
      return
      end


      function HCt(p1,p2,p3,p4)
      real *8 HCt
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)

      HCt=(2.*(t**2+u**2)/s**2 )*t/u
      return
      end


      function HCu(p1,p2,p3,p4)
      real *8 HCu
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)

      HCu=(2.*(t**2+u**2)/s**2 )*u/t

      return
      end

      function HCs(p1,p2,p3,p4)
      real *8 HCs
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)

      HCs=2.*(t**2+u**2)/t/u

      return
      end



      function HDs(p1,p2,p3,p4)
      real *8 HDs
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)

      HDs=2.*(t**2+u**2) *(s**4+t**4+u**4)/(s*t*u)**2

      return
      end




      function HDt(p1,p2,p3,p4)
      real *8 HDt
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)


      HDt=2.*(s**2+u**2) *(s**4+t**4+u**4)/(s*t*u)**2


      return
      end


      function HDu(p1,p2,p3,p4)
      real *8 HDu
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)


      HDu=2.*(s**2+t**2) *(s**4+t**4+u**4)/(s*t*u)**2

      return
      end


      subroutine borncolour_lh
c Sets up the colour for the given flavour configuration
c already filled in the Les Houches interface.
c In case there are several colour structure, one
c should pick one with a probability proportional to
c the value of the corresponding cross section, for the
c kinematics defined in the Les Houches interface
      implicit none

c     TO BE COMPLETED
      
      write(*,*) "borncolour_lh TO BE COMPLETED!!" 
      call exit(1)

      end


      subroutine resonances_lh
c     Set up the resonances whose mass must be preserved
c     on the Les Houches interface.
c     Before that, call the routine that generates the decay.
c     Notice that the current subroutine is called at the end
c     of gen_leshouches (or gen_leshouches_reg). This means
c     that the overall azimuthal rotation has been already
c     performed (add_azimuth called in pwhgevent). 
      implicit none

c     TO BE COMPLETED
      
      write(*,*) "resonances_lh TO BE COMPLETED (not needed ??) " 
      call exit(1)

      end






