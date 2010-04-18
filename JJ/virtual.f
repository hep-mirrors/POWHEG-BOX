c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point
      subroutine setvirtual(p,vflav,virtual)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      include 'PhysPars.h'
      integer nleg
      parameter (nleg=nlegborn)
      real * 8 p(0:3,nleg)
      integer vflav(nleg)
      real * 8 virtual
      integer mu
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 psitilde6ns_a,psitilde6ns_b,psitilde6ns_c,psitilde6ns_d
      external psitilde6ns_a,psitilde6ns_b,psitilde6ns_c,psitilde6ns_d
C     define a real *8 value for nc in order
C     to avoid integer by integer division
      real *8 ncol
      parameter (ncol=nc)
      real *8 VC
      parameter(VC=ncol**2-1)

c     Virtual contributions;
c     Rules from Kunszt-Soper.
c     First, identify the flavour structure

c     A-type, qq'->qq'
      if(
     $     (vflav(1)*vflav(2)*vflav(3).ne.0).and.
     $     (abs(vflav(1)).ne.abs(vflav(2))).and.
     $     (vflav(1).eq.vflav(3)).and.
     $     (vflav(2).eq.vflav(4))) then

         if((vflav(1).gt.0).and.(vflav(2).gt.0)) then
c     write(*,*) 'A1a, qqp->qqp ', (qcd_name(vflav(ileg)),ileg=1,4)
            do mu=0,3
               p1(mu)=p(mu,1)
               p2(mu)=p(mu,2)
               p3(mu)=-p(mu,3)
               p4(mu)=-p(mu,4)
            enddo
            
         elseif((vflav(1).lt.0).and.(vflav(2).gt.0)) then
c     write(*,*) 'A1b, qbarqp->qbarqp ', (qcd_name(vflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     1 -> 3
c     3 -> 1
            do mu=0,3
               p1(mu)=-p(mu,3)
               p2(mu)=p(mu,2)
               p3(mu)=p(mu,1)
               p4(mu)=-p(mu,4)
            enddo

           
         elseif((vflav(1).gt.0).and.(vflav(2).lt.0)) then
c     write(*,*) 'A1c, qqbarp->qqbarp ', (qcd_name(vflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     2 -> 4
c     4 -> 2
            do mu=0,3
               p1(mu)=p(mu,1)
               p2(mu)=-p(mu,4)
               p3(mu)=-p(mu,3)
               p4(mu)=p(mu,2)
            enddo
            
         elseif((vflav(1).lt.0).and.(vflav(2).lt.0)) then
c     write(*,*) 'A1d, qbar qbarp->qbar qbarp ', (qcd_name(vflav(ileg)),ileg=1,4)
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
            
         else
            write(*,*) 'Error in virtual.f, A1-type virtual'
            call exit(1)
         endif
         
C     At this point  the function
C     psitilde6ns_a(p1,p2,p3,p4) can be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
            virtual= (4.*pi*st_alpha)**2 * psitilde6ns_a(p1,p2,p3,p4)
     $           /4./ncol/ncol         
         

c     A-type, qq'->q'q
      elseif(
     $        (vflav(1)*vflav(2)*vflav(3).ne.0).and.
     $        (abs(vflav(1)).ne.abs(vflav(2))).and.
     $        (vflav(1).eq.vflav(4)).and.
     $        (vflav(2).eq.vflav(3))) then
         
         
         if((vflav(1).gt.0).and.(vflav(3).gt.0)) then
c     write(*,*) 'A2a, qqp->qpq ', (qcd_name(vflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     3 -> 4
c     4 -> 3
            do mu=0,3
               p1(mu)=p(mu,1)
               p2(mu)=p(mu,2)
               p3(mu)=-p(mu,4)
               p4(mu)=-p(mu,3)
            enddo
            

            
         elseif((vflav(1).gt.0).and.(vflav(3).lt.0)) then
c     write(*,*) 'A2b, q qbarp->qbarp q ', 
c     $           (qcd_name(vflav(ileg)),ileg=1,4)
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
            

            
         elseif((vflav(1).lt.0).and.(vflav(3).lt.0)) then
c     write(*,*) 'A2b, qbar qbarp->qbarp qbar ', 
c     $           (qcd_name(vflav(ileg)),ileg=1,4)
c     As before, but exchanging 1 with 4 ONLY in the Bjk, AND NOT in the
c     momenta assignment.
            do mu=0,3
               p1(mu)=p(mu,1)
               p2(mu)=p(mu,2)
               p3(mu)=-p(mu,4)
               p4(mu)=-p(mu,3)
            enddo    

            
         else
            write(*,*) 'Error in virtual.f, A2-type virtual'
            call exit(1)
         endif

C     At this point  the function
C     psitilde6ns_a(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_a(p1,p2,p3,p4)
     $        /4./ncol/ncol         


c     A-type, qqbar->q'qbar'
      elseif(
     $        (vflav(1)*vflav(2)*vflav(3).ne.0).and.
     $        (vflav(1).eq.-vflav(2)).and.
     $        (vflav(3).eq.-vflav(4)).and.
     $        (abs(vflav(1)).ne.abs(vflav(3)))) then

         if((vflav(1).gt.0).and.(vflav(3).gt.0)) then
c     write(*,*) 'A3a, qqbar->qp qpbar ',(qcd_name(vflav(ileg)),ileg=1,4)
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

            
         elseif((vflav(1).gt.0).and.(vflav(3).lt.0)) then
c     write(*,*) 'A3b, qqbar->qpbar qp ',(qcd_name(vflav(ileg)),ileg=1,4)
c     exchange wrt fundamental order:
c     3 -> 2
c     2 -> 3
            do mu=0,3
               p1(mu)=p(mu,1)
               p2(mu)=-p(mu,3)
               p3(mu)=p(mu,2)
               p4(mu)=-p(mu,4)
            enddo


            
         elseif((vflav(1).lt.0).and.(vflav(3).gt.0)) then
c     write(*,*) 'A3c, qbar q->qp qpbar ',(qcd_name(vflav(ileg)),ileg=1,4)
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

            
         elseif((vflav(1).lt.0).and.(vflav(3).lt.0)) then
c     write(*,*) 'A3d, qbar q->qpbar qp ',
c     $           (qcd_name(vflav(ileg)),ileg=1,4)
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
            
         else
            write(*,*) 'Error in virtual.f, A3-type virtual'
            call exit(1)
         endif

C     At this point  the function
C     psitilde6ns_a(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_a(p1,p2,p3,p4)
     $        /4./ncol/ncol         
         
c     B-type, qq->qq
      elseif((vflav(1).eq.vflav(2)).and.
     $        (vflav(1).eq.vflav(3)).and.
     $        (vflav(1).eq.vflav(4)).and.
     $        (vflav(1).ne.0)) then
c     write(*,*) 'B1a, qq->qq ',(qcd_name(vflav(ileg)),ileg=1,4)
         
c     no exchange wrt fundamental order:
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=-p(mu,4)
         enddo
         
C     At this point  the function
C     psitilde6ns_b(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_b(p1,p2,p3,p4)
     $        /4./ncol/ncol               
c     B-type, qqbar->qqbar
      elseif((vflav(1).eq.-vflav(2)).and.
     $        (vflav(1).eq.vflav(3)).and.
     $        (vflav(1).eq.-vflav(4)).and.
     $        (vflav(1).ne.0)) then
c     write(*,*) 'B2, qqbar->qqbar ',(qcd_name(vflav(ileg)),ileg=1,4)

c     exchange wrt fundamental order:
c     2 -> 4
c     4 -> 2
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,4)
            p3(mu)=-p(mu,3)
            p4(mu)=p(mu,2)
         enddo

C     At this point  the function
C     psitilde6ns_b(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_b(p1,p2,p3,p4)
     $        /4./ncol/ncol       

c     B-type, qqbar->qbar q
      elseif((vflav(1).eq.-vflav(2)).and.
     $        (vflav(1).eq.-vflav(3)).and.
     $        (vflav(1).eq.vflav(4)).and.
     $        (vflav(1).ne.0)) then
c     write(*,*) 'B3, qqbar->qbar q ',(qcd_name(vflav(ileg)),ileg=1,4)


c     exchange wrt fundamental order:
c     2 -> 3
c     3 -> 2
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=-p(mu,3)
            p3(mu)=p(mu,2)
            p4(mu)=-p(mu,4)
         enddo

C     At this point  the function
C     psitilde6ns_b(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_b(p1,p2,p3,p4)
     $        /4./ncol/ncol       

c     C-type, qqbar->gg
      elseif((vflav(1).eq.-vflav(2)).and.
     $        (vflav(3)*vflav(4).eq.0).and.
     $        (vflav(1).ne.0)) then
c     write(*,*) 'C1, qqbar->gg ', (qcd_name(vflav(ileg)),ileg=1,4)
c     no exchange wrt fundamental order:
         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=-p(mu,4)
         enddo

C     At this point  the function
C     psitilde6ns_c(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_c(p1,p2,p3,p4)         
     $        /4./ncol/ncol       

c     C-type, qg->qg
      elseif((vflav(1).ne.0).and.
     $        (vflav(2).eq.0).and.
     $        (vflav(3).ne.0).and.
     $        (vflav(4).eq.0)) then
c     write(*,*) 'C2, qg->qg ', (qcd_name(vflav(ileg)),ileg=1,4)

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
C     At this point  the function
C     psitilde6ns_c(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_c(p1,p2,p3,p4)         
     $        /4./ncol/VC 

c     C-type, qg->gq
      elseif((vflav(1).ne.0).and.
     $        (vflav(2).eq.0).and.
     $        (vflav(3).eq.0).and.
     $        (vflav(4).ne.0)) then
c     write(*,*) 'C3, qg->gq ', (qcd_name(vflav(ileg)),ileg=1,4)

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
C     At this point  the function
C     psitilde6ns_c(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_c(p1,p2,p3,p4)         
     $        /4./ncol/VC      

c     C-type, gg->qqbar
      elseif((vflav(3).eq.-vflav(4)).and.
     $        (vflav(1)*vflav(2).eq.0).and.
     $        (vflav(3).ne.0)) then
c     write(*,*) 'C4, gg->qqbar ', (qcd_name(vflav(ileg)),ileg=1,4)

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
C     At this point  the function
C     psitilde6ns_c(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_c(p1,p2,p3,p4)         
     $        /4./VC/VC

c     C-type, gqbar->qbarg
      elseif((vflav(1).eq.0).and.
     $        (vflav(2).ne.0).and.
     $        (vflav(3).ne.0).and.
     $        (vflav(4).eq.0)) then
c     write(*,*) 'C5, gqbar->qbarg ',(qcd_name(vflav(ileg)),ileg=1,4)

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
C     At this point  the function
C     psitilde6ns_c(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_c(p1,p2,p3,p4)         
     $        /4./ncol/VC

c     C-type, gq->gq
      elseif((vflav(1).eq.0).and.
     $        (vflav(2).ne.0).and.
     $        (vflav(3).eq.0).and.
     $        (vflav(4).ne.0)) then
c     write(*,*) 'C6, gq->gq ', (qcd_name(vflav(ileg)),ileg=1,4)
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
C     At this point  the function
C     psitilde6ns_c(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_c(p1,p2,p3,p4)         
     $        /4./ncol/VC

c     D-type
      elseif((vflav(1).eq.0).and.
     $        (vflav(2).eq.0).and.
     $        (vflav(3).eq.0).and.
     $        (vflav(4).eq.0)) then

c     write(*,*) 'D ', (qcd_name(vflav(ileg)),ileg=1,4)

         do mu=0,3
            p1(mu)=p(mu,1)
            p2(mu)=p(mu,2)
            p3(mu)=-p(mu,3)
            p4(mu)=-p(mu,4)
         enddo
C     At this point  the function
C     psitilde6ns_d(p1,p2,p3,p4) can always be called 
C     with the given order. WARNING: the st_alpha/2*pi
C     missing factor is provided later on by the program
         virtual= (4.*pi*st_alpha)**2 * psitilde6ns_d(p1,p2,p3,p4)         
     $     /4./VC/VC    
      else
         write(*,*) 'Error in virtuals'
         call exit(1)
      endif

      end



C THE FOLLOWING FUNCTIONS ARE TAKEN FROM ELLIS-SEXTON CODE
C AND MODIFIED ACCORDING POWHEG STRUCTURE
C WARNING: THE CLASSIFICATION IN TERMS OF A,B,C,D
C PROCESSES IS ACCORDING TO KUNSZT-SOPER AND NOT 
C ACCORDING ELLIS-SEXTON  A,B,C,D FUNCTIONS
 
      function psitilde6ns_a(p1,p2,p3,p4)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      real *8  psitilde6ns_a
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      DOUBLE PRECISION THETA
      DOUBLE PRECISION LS,LT,LU,LMU,L2S,L2T,L2U
      DOUBLE PRECISION QES,MUUV
      DOUBLE PRECISION N,V,NFLAVOR
      EXTERNAL THETA
C     Invariant quantities
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
C     Renormalization scale 
      MUUV=sqrt(st_muren2)
C     In the POWHEG-BOX we always assume that QES=st_muren
      QES=sqrt(st_muren2)
C     SU(3) constants
      N=nc
      V=nc**2-1
C     NUMBER OF LIGHT FLAVOURS
      NFLAVOR=st_nlight
C  Here are the Ellis and Sexton log functions
C
      LS = DLOG(DABS(S/QES**2))
      LT = DLOG(DABS(T/QES**2))
      LU = DLOG(DABS(U/QES**2))
      LMU = DLOG(DABS(MUUV**2/QES**2))
      L2S = LS**2 - PI**2 * THETA(S.GT.0.0D0)
      L2T = LT**2 - PI**2 * THETA(T.GT.0.0D0)
      L2U = LU**2 - PI**2 * THETA(U.GT.0.0D0)
      
      PSITILDE6NS_A =
     >     V/(9.0D0*N*T**2)
     >     *(-  9*PI**2*(N**2-4 )*(S**2 - U**2)
     >     +  2*(72 + 13*N**2 - 10*N*NFLAVOR + 9*N**2*PI**2)
     >     *(S**2 + U**2)
     >     +  6*N*(11*N - 2*NFLAVOR)*(S**2 + U**2)*LMU
     >     - 36*T*U*LS
     >     +  3*(- 6*S**2 - 30*U**2 + 4*N*NFLAVOR*(S**2 + U**2)
     >     - N**2*(7*S**2 + 3*T**2 + U**2))*LT
     >     - 18*S*T*(N**2 -2)*LU
     >     - 36*(3*S**2 + U**2)*LS*LT
     >     - 18*(N**2 -2)*(S**2 + 3*U**2)*LT*LU
     >     + 18*(S**2 - U**2)*L2S
     >     +  9*(N**2*(S**2 + 3*U**2) + 2*(3*S**2 - U**2))*L2T
     >     -  9*(N**2 - 2)*(S**2 - U**2)*L2U)

      return
      end


      function psitilde6ns_b(p1,p2,p3,p4)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      real *8  psitilde6ns_b
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      DOUBLE PRECISION THETA
      DOUBLE PRECISION LS,LT,LU,LMU,L2S,L2T,L2U
      DOUBLE PRECISION QES,MUUV
      DOUBLE PRECISION N,V,NFLAVOR
      EXTERNAL THETA
C     Invariant quantities
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
C     Renormalization scale 
      MUUV=sqrt(st_muren2)
C     In the POWHEG-BOX we always assume that QES=st_muren
      QES=sqrt(st_muren2)
C     SU(3) constants
      N=nc
      V=nc**2-1
C     NUMBER OF LIGHT FLAVOURS
      NFLAVOR=st_nlight
C  Here are the Ellis and Sexton log functions
C
      LS = DLOG(DABS(S/QES**2))
      LT = DLOG(DABS(T/QES**2))
      LU = DLOG(DABS(U/QES**2))
      LMU = DLOG(DABS(MUUV**2/QES**2))
      L2S = LS**2 - PI**2 * THETA(S.GT.0.0D0)
      L2T = LT**2 - PI**2 * THETA(T.GT.0.0D0)
      L2U = LU**2 - PI**2 * THETA(U.GT.0.0D0)
      
      PSITILDE6NS_B =
     >     40*N*NFLAVOR*S**2*T*U - 4*N**2*S**2*T*U*(13 + 9*PI**2)
     >   - 9*T*U*(32*S**2 + PI**2*(S**2 + 2*T*U))
     >   + 36*N*(S**2*(4 + PI**2) * (T**2 + U**2)
     >   + (4 - PI**2)  *  (T**4 + U**4))
     >   + N**3*(S**2*(26 + 9*PI**2)*(T**2 + U**2)
     >   + (26 + 27*PI**2)*(T**4 + U**4))
     >   - 20*N**2*NFLAVOR*(T**4 + U**4 + S**2*(T**2 + U**2))
C
      PSITILDE6NS_B =  PSITILDE6NS_B
     >  + 6*N*(11*N - 2*NFLAVOR)
     >     *(-2*S**2*T*U + N*(T**4 + U**4 + S**2*(T**2 + U**2))) *LMU
     >  - 36*N*T*U*(T**2 + U**2)*LS
     >  - 6*U*(- 2*N**2*S**2*T + 2*N*NFLAVOR*S**2*T
     >         - 2*N**2*NFLAVOR*U*(S**2 + U**2)
     >         + 6*S*T*(T + 2*U) + N**3*(-3*T**3 + 2*T**2*U
     >         + 7*T*U**2 + 4*U**3)
     >         + 3*N*(2*T**3 + 3*T**2*U + 2*T*U**2 + 6*U**3)) *LT
     >  - 6*T*(- 2*N**2*S**2*U + 2*N*NFLAVOR*S**2*U
     >         - 2*N**2*NFLAVOR*T*(S**2 + T**2)
     >         + 6*S*U*(2*T + U)
     >         + N**3*(4*T**3 + 7*T**2*U + 2*T*U**2 - 3*U**3)
     >         + 3*N*(6*T**3 + 2*T**2*U + 3*T*U**2 + 2*U**3)) * LU
C
       PSITILDE6NS_B =  PSITILDE6NS_B
     >  - 36*U*(- S**2*T - N**2*S**2*T + N*U*(3*S**2 + U**2)) *LS*LT
     >  - 36*T*(- S**2*U - N**2*S**2*U + N*T*(3*S**2 + T**2)) *LS*LU
     >  - 18*(  2*N*(-2 + N**2)*(T**2 - T*U + U**2)
     >          *(2*T**2 + 3*T*U + 2*U**2)
     >        + T*U*(3*T**2 + 4*T*U + 3*U**2)) * LT*LU
     >  + 18*N*T*U*(S**2 + T**2 + U**2) * L2S
     >  + 9*U*(- 2*N**2*S**2*T - 4*N*(S**3 - S*T**2 - T**3)
     >         - T*(3*T**2 + 8*T*U + 3*U**2)
     >         - 2*N**3*(T**3 - T*U**2 - 2*U**3)) * L2T
     >  + 9*T*(- 2*N**2*S**2*U - 4*N*(S**3 - S*U**2 - U**3)
     >         - U*(3*T**2 + 8*T*U + 3*U**2)
     >         + 2*N**3*(2*T**3 + T**2*U - U**3)) * L2U
C
       PSITILDE6NS_B = V/(9*N**2*T**2*U**2) * PSITILDE6NS_B

      return
      end


      function psitilde6ns_c(p1,p2,p3,p4)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      real *8  psitilde6ns_c
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      DOUBLE PRECISION THETA
      DOUBLE PRECISION LS,LT,LU,LMU,L2S,L2T,L2U
      DOUBLE PRECISION QES,MUUV
      DOUBLE PRECISION N,V,NFLAVOR
      EXTERNAL THETA
C     Invariant quantities
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
C     Renormalization scale 
      MUUV=sqrt(st_muren2)
C     In the POWHEG-BOX we always assume that QES=st_muren
      QES=sqrt(st_muren2)
C     SU(3) constants
      N=nc
      V=nc**2-1
C     NUMBER OF LIGHT FLAVOURS
      NFLAVOR=st_nlight
C  Here are the Ellis and Sexton log functions
C
      LS = DLOG(DABS(S/QES**2))
      LT = DLOG(DABS(T/QES**2))
      LU = DLOG(DABS(U/QES**2))
      LMU = DLOG(DABS(MUUV**2/QES**2))
      L2S = LS**2 - PI**2 * THETA(S.GT.0.0D0)
      L2T = LT**2 - PI**2 * THETA(T.GT.0.0D0)
      L2U = LU**2 - PI**2 * THETA(U.GT.0.0D0)
      
      PSITILDE6NS_C =
     >   3*S**2*( -7*(T**2 + U**2) + PI**2*(3*S**2 -2*T*U) )
     >   + 3*N**2*( 14*(T**4 + U**4) + T*U*(13*S**2 + 4*T*U )
     >            + PI**2*(T**3*U + 2*T**2*U**2 + T*U**3)    )
     >   - 3*N**4*( 7*(T**4 + U**4)+ T*U*(S**2 + 10*T*U)
     >            - PI**2*(T - U)**2*(T**2 + U**2)  )
     >   + 2*N*(11*N - 2*NFLAVOR)*(-S**2 + N**2*(T**2 + U**2))
     >         *(T**2 + U**2) * LMU
     >   - 3*T*U*(  4*T**2 + 8*T*U + 4*U**2
     >         + N**2*(-1 + N**2)*(T**2 - 10*T*U + U**2) ) * LS
     >   + 3*S*U*(S + N**2*U)*(2*T + 3*U + N**2*(2*T - 3*U)) * LT
     >   + 3*S*T*(S + N**2*T)*(3*T + 2*U + N**2*(-3*T + 2*U)) * LU
C
       PSITILDE6NS_C = PSITILDE6NS_C 
     >   - 6*(  N**4*U*(U - T)*(2*T**2 + T*U + U**2)
     >     + S*(S - N**2*T)*(2*T**2 + 2*T*U + U**2)  ) * LS*LT
     >   - 6*(  N**4*T*(T - U)*(T**2 + T*U + 2*U**2)
     >   + S*(S - N**2*U)*(T**2 + 2*T*U + 2*U**2)  ) * LS*LU
     >   + 12*N**2*S**2*(T**2 + U**2) * LT*LU
     >   + 3*( 2*S**4 - 2*N**4*T*U*(T**2 + U**2)
     >     + N**2*(T**2 + T*U + 2*U**2)*(2*T**2 + T*U + U**2) ) *L2S
     >   - 3*S*(- N**4*U*(2*T**2 - T*U + U**2)
     >       + (N**2*T - S)*(2*T**2 + 2*T*U + U**2) ) *L2T
     >   - 3*S*(- N**4*T*(2*U**2 - T*U + T**2 )
     >       + (N**2*U - S)*( 2*U**2 + 2*T*U + T**2)) *L2U
C
       PSITILDE6NS_C = V/(3.0D0*N**2*S**2*T*U) * PSITILDE6NS_C

      return
      end


      function psitilde6ns_d(p1,p2,p3,p4)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      real *8  psitilde6ns_d
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      DOUBLE PRECISION THETA
      DOUBLE PRECISION LS,LT,LU,LMU,L2S,L2T,L2U
      DOUBLE PRECISION QES,MUUV
      DOUBLE PRECISION N,V,NFLAVOR
      EXTERNAL THETA
C     Invariant quantities
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
C     Renormalization scale 
      MUUV=sqrt(st_muren2)
C     In the POWHEG-BOX we always assume that QES=st_muren
      QES=sqrt(st_muren2)
C     SU(3) constants
      N=nc
      V=nc**2-1
C     NUMBER OF LIGHT FLAVOURS
      NFLAVOR=st_nlight
C  Here are the Ellis and Sexton log functions
C
      LS = DLOG(DABS(S/QES**2))
      LT = DLOG(DABS(T/QES**2))
      LU = DLOG(DABS(U/QES**2))
      LMU = DLOG(DABS(MUUV**2/QES**2))
      L2S = LS**2 - PI**2 * THETA(S.GT.0.0D0)
      L2T = LT**2 - PI**2 * THETA(T.GT.0.0D0)
      L2U = LU**2 - PI**2 * THETA(U.GT.0.0D0)
      
  
       PSITILDE6NS_D =
     >  2*N**2*NFLAVOR*( - (66 + 27*PI**2)*S**2*T**2*U**2
     >           + 40*(S**6 + T**6 + U**6))
     >   + 2*N**3*(  6*(125 - 27*PI**2)*S**2*T**2*U**2
     >         - 4*(67 - 9*PI**2)*(S**6 + T**6 + U**6))
     >   + 6*N**2*(11*N - 2*NFLAVOR)*(S**2 + T**2 + U**2)**3 *LMU
     >   + 6*N**2*T*U*(S**2 + T**2 + U**2)
     >       *(  NFLAVOR *(5*T**2 + 2*T*U + 5*U**2)
     >         - 2*N*(7*T**2 - 8*T*U + 7*U**2) ) *LS
C
       PSITILDE6NS_D = PSITILDE6NS_D 
     >   + 6*N**2*S*U*(S**2 + T**2 + U**2)
     >       *(  NFLAVOR *(5*S**2 + 2*S*U + 5*U**2)
     >         - 2*N*(7*S**2 - 8*S*U + 7*U**2) ) *LT
     >   + 6*N**2*S*T*(S**2 + T**2 + U**2)
     >       *(  NFLAVOR* (5*S**2 + 2*S*T + 5*T**2)
     >         - 2*N*(7*S**2 - 8*S*T + 7*T**2) ) *LU
     >   - 36*N**2*U**2*( NFLAVOR*S*T*(S**2 + T**2)
     >             + 2*N*(2*S**4 + 2*S**3*T + 3*S**2*T**2 + 2*S*T**3
     >             + 2*T**4))*LS*LT
C
       PSITILDE6NS_D = PSITILDE6NS_D 
     >   - 36*N**2*S**2*(  NFLAVOR*T*U*(T**2 + U**2)
     >             + 2*N*(2*T**4 + 2*T**3*U + 3*T**2*U**2 + 2*T*U**3
     >             + 2*U**4))*LT*LU
     >   - 36*N**2*T**2*(  NFLAVOR*S*U*(S**2 + U**2)
     >             + 2*N*(2*S**4 + 2*S**3*U + 3*S**2*U**2 + 2*S*U**3
     >             + 2*U**4))*LS*LU
     >   + 18*N**2*S**2*T*U*(4*N*(T**2 + U**2)
     >             - NFLAVOR*(T**2 + 3*T*U + U**2))*L2S
     >   + 18*N**2*S*T**2*U*(4*N*(S**2 + U**2)
     >             - NFLAVOR*(S**2 + 3*S*U + U**2))*L2T
     >   + 18*N**2*S*T*U**2*(4*N*(S**2 + T**2)
     >             - NFLAVOR*(S**2 + 3*S*T + T**2))*L2U
C
       PSITILDE6NS_D = V/(9.0D0*S**2*T**2*U**2) * PSITILDE6NS_D
      return
      end


      DOUBLE PRECISION FUNCTION THETA(BOOL)
C     
      LOGICAL BOOL
      IF (BOOL) THEN
         THETA = 1.0D0
      ELSE
         THETA = 0.0D0
      ENDIF
      RETURN
      END
C
