      subroutine setreal(p,rflav,amp2)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      include 'PhysPars.h'

      real * 8 p(0:3,nlegreal)
      integer rflav(nlegreal)
      real * 8 amp2,amp2mad,tmp
      integer nleg
      parameter (nleg=nlegreal)
      integer mu,ileg

      real *8 kr_mad(0:3,nleg)


c     set madgraph parameters that can change on an event-by-event basis
      call mad_setparam

      do ileg=1,5
         do mu=0,3
            kr_mad(mu,ileg)=p(mu,ileg)
         enddo
      enddo
c     to avoid bugs in HELAS, restore exact masslessness of  incoming partons 
      kr_mad(0,1)=dabs(kr_mad(3,1))
      kr_mad(0,2)=dabs(kr_mad(3,2))

c$$$c     also for outgoig ?
c$$$      kr_mad(0,3)=sqrt(dabs(kr_mad(1,3)**2+kr_mad(2,3)**2+kr_mad(3,3)
c$$$     $     **2))
c$$$      kr_mad(0,4)=sqrt(dabs(kr_mad(1,4)**2+kr_mad(2,4)**2+kr_mad(3,4)
c$$$     $     **2))
c$$$      kr_mad(0,5)=sqrt(dabs(kr_mad(1,5)**2+kr_mad(2,5)**2+kr_mad(3,5)
c$$$     $     **2))

      
      call compreal(kr_mad,rflav,amp2mad)
c$$$      call alt_compreal(kr_mad,rflav,amp2mad)
c$$$      if(abs(amp2mad-tmp)/abs(tmp).gt.0.001) then
c$$$         write(*,*) 
c$$$         write(*,*) 'frac diff = ',abs(amp2mad-tmp)/abs(tmp)
c$$$         write(*,*) 'rflav = ',rflav
c$$$         write(*,*) 'old = ',amp2mad
c$$$         write(*,*) 'new = ',tmp
c$$$
c$$$      endif

      amp2=amp2mad/(st_alpha/2./pi)

      return
      end



      SUBROUTINE ALT_COMPREAL(P,FLAV,AMP2)

      IMPLICIT NONE
C -   *************

      REAL*8 P(0:3,1:5)
      INTEGER FLAV(5)
      REAL*8 AMP2
      CHARACTER*1 KS_LABEL
      INTEGER KS_MAP(5),V(5),VV(5)
      INTEGER I,ITMP
      REAL*8  AVG,SYM

      DO I=1,5
         IF(I.GT.2) THEN
            V(I)=-FLAV(I)
         ELSE
            V(I)= FLAV(I)
         ENDIF
c Set gluon smaller than any flavour
         IF(V(I).EQ.0) V(I)=-21
      ENDDO
c Find permutation to put them in descending order:
c gluons last, flavours in decreasing order
c CERNLIB routine:
      CALL SORTZV(V,KS_MAP,5,-1,1,0)
      DO I=1,5
         VV(I)=V(KS_MAP(I))
      ENDDO
c Classify processes
      IF(VV(1).EQ.-21) THEN
         KS_LABEL='D'
      ELSEIF(VV(3).EQ.-21) THEN
         KS_LABEL='C'
      ELSEIF(VV(1).EQ.VV(2)) THEN
         KS_LABEL='B'
      ELSE
         KS_label='A'
c Kunszt Soper have last two particles exchanged in this case (i.e. q Q ->
c -q -Q rather than q Q -> -Q -q (if q>Q) )
         ITMP=VV(3)
         VV(3)=VV(4)
         VV(4)=ITMP
         ITMP=KS_MAP(3)
         KS_MAP(3)=KS_MAP(4)
         KS_MAP(4)=ITMP
      ENDIF

      AVG=1.0d0
      DO I=1,2
         IF(FLAV(I).NE.0) THEN
c - Sign for crossing fermion line:
            AVG = AVG*-6.
         ELSE
            AVG = AVG*16.
         ENDIF
      ENDDO
      IF((FLAV(3).EQ.FLAV(4)).AND.(FLAV(3).EQ.FLAV(5))) THEN
         SYM=6.0D0
      ELSEIF((FLAV(3).EQ.FLAV(4)).AND.(FLAV(3).NE.FLAV(5))) THEN
         SYM=2.0D0
      ELSEIF((FLAV(3).EQ.FLAV(5)).AND.(FLAV(3).NE.FLAV(4))) THEN
         SYM=2.0D0
      ELSEIF((FLAV(4).EQ.FLAV(5)).AND.(FLAV(3).NE.FLAV(4))) THEN
         SYM=2.0D0
      ELSE
         SYM=1.0D0
      ENDIF

c - KH: don't understand why I find I need this extra factor two 
      IF(KS_LABEL.EQ.'C') SYM=SYM/2.0D0

      DO I=1,5
         IF(KS_MAP(I).GT.2) KS_MAP(I)=-KS_MAP(I)
      ENDDO
      IF(KS_LABEL.EQ.'A') THEN
         CALL ES_A(P,AMP2,KS_MAP,AVG,SYM)
      ELSEIF(ks_label.EQ.'B') THEN
         CALL ES_B(P,AMP2,KS_MAP,AVG,SYM)
      ELSEIF(ks_label.EQ.'C') THEN
         CALL ES_C(P,AMP2,KS_MAP,AVG,SYM)
      ELSEIF(ks_label.EQ.'D') THEN
         CALL ES_D(P,AMP2,KS_MAP,AVG,SYM)
      ELSE
         WRITE(*,*) 'compreal: fatal error.',FLAV
         WRITE(*,*) 'Failed to determine real emission proc. Quitting.'
         CALL EXIT(1)
      ENDIF

      RETURN
      END



C ----------------------------------------------------------------- C
      SUBROUTINE ES_D(P1,RESULT,ES_MAP,AVG,SYM)
C - The squared, spin and colour summed matrix element for gg->ggg
C - , averaged over initial state colours and spins. The final state
C - symmetry factor 1/3! is included at the end. This derives from
C - formulae in Ellis & Sexton, NPB269:445,1986. 
C ----------------------------------------------------------------- C

      IMPLICIT NONE
C -   *************

      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      Include "madgraph_5_flavours/genps.inc"
      REAL*8 P1(0:3,NEXTERNAL_REAL),AVG,SYM
      REAL*8 FD_INVARIANT,FD_NONINVARIANT,FD_DENOM
      REAL*8 DOTP
      EXTERNAL DOTP
      REAL*8 S(5,5),RESULT
      INTEGER ES_MAP(5)
      INTEGER IXX,JXX,KXX,LXX,MXX
      REAL*8 GS6

C - Calculate the strong coupling constant prefactor
      GS6 = (4.*PI*ST_ALPHA)**3
C - First calculate the kinematic invariants once and for all (in
C - Ellis sextion conventions).
      DO IXX=1,5
         DO JXX=1,5 
            IF(IXX.EQ.JXX) THEN
               S(IXX,JXX) = 0.0
            ELSE
               S(IXX,JXX) = DOTP(P1(0,ABS(ES_MAP(IXX))),
     $                           P1(0,ABS(ES_MAP(JXX))))
               IF((ES_MAP(IXX)*ES_MAP(JXX)).LT.0) S(IXX,JXX)=-S(IXX,JXX)
            ENDIF
         ENDDO
      ENDDO
C - Then we calculate the Ellis Sexton D-function (Eq. 3.11) as a sum
C - over permutations of the arguments of the F^D function (Eq. 3.12).
C - We calculate the FD function as a product of two parts to save time:
C - a permuation invariant part FD_INVARIANT and a part which is not
C - invariant under permutations of the momenta FD_NONINVARIANT. The
C - latter is the denominator of the prefactor in Eq. 3.12 of the Ellis,
C - Sexton paper. 
      FD_INVARIANT = 4. * 8. * 3. * 3. * 3. 
     $     * 0.5 * ( S(1,2)**4 + S(1,3)**4 + S(1,4)**4 + S(1,5)**4
     $             + S(2,3)**4 + S(2,4)**4 + S(2,5)**4
     $             + S(3,4)**4 + S(3,5)**4
     $             + S(4,5)**4 )
C - Initialise non invariant part and loop over permutations 
      FD_NONINVARIANT = 0.
      DO IXX=1,5
         DO JXX=1,5
            IF(JXX.EQ.IXX) CYCLE
            DO KXX=1,5
               IF((KXX.EQ.IXX).OR.
     $            (KXX.EQ.JXX)) CYCLE
               DO LXX=1,5
                  IF((LXX.EQ.IXX).OR.
     $               (LXX.EQ.JXX).OR.
     $               (LXX.EQ.KXX)) CYCLE
                  DO MXX=1,5
                     IF((MXX.EQ.IXX).OR.
     $                  (MXX.EQ.JXX).OR.
     $                  (MXX.EQ.KXX).OR.
     $                  (MXX.EQ.LXX)) CYCLE
                     FD_NONINVARIANT = FD_NONINVARIANT
     $                               + FD_DENOM(IXX,JXX,KXX,LXX,MXX,S)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C - On the next line we obtain the D function (Eq.3.11).
      RESULT=GS6/10.*FD_INVARIANT*FD_NONINVARIANT
C - To get the matrix element squared we need to divide by the
C - spin colour averaging factor 4V^2 (see Table 7).
      RESULT=RESULT/AVG
C - We also need to include another symmetry factor of 3! to get
C - agreement with the MadGraph (and, hopefully, reality).
      RESULT=RESULT/SYM

      END
       
       
      FUNCTION FD_DENOM(I,J,K,L,M,S)
      IMPLICIT NONE
      REAL*8 FD_DENOM
      INTEGER I,J,K,L,M
      REAL*8 S(5,5)
      FD_DENOM = 1.0/(S(I,J)*S(J,K)*S(K,L)*S(L,M)*S(M,I))
      RETURN
      END
       


C ----------------------------------------------------------------- C
      SUBROUTINE ES_C(P1,RESULT,ES_MAP,AVG,SYM)
C - The squared, spin and colour summed matrix element for gu->ugg
C - , averaged over initial state colours and spins. This comes from
C - formulae in Ellis & Sexton, NPB269:445,1986. Checked agreement
C - with Madgraph matrix subroutine. One instance occurred prior to
C - "POWHEG:initialization" with fractional differences at the level
C - of 1/10^7 but after then in the grid search phase and all
C - integrations the fractional difference was better than 1*10^7.
C ----------------------------------------------------------------- C

      IMPLICIT NONE
C -   *************
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      Include "madgraph_5_flavours/genps.inc"
      REAL*8 P1(0:3,NEXTERNAL_REAL),AVG,SYM
      REAL*8 DOTP
      EXTERNAL DOTP
      REAL*8 S(5,5),FC_FN,RESULT
      INTEGER ES_MAP(5)
      INTEGER IXX,JXX,LXX,MXX
      REAL*8 GS6
C - Calculate the strong coupling constant prefactor
      GS6 = (4.*PI*ST_ALPHA)**3
C - First calculate the kinematic invariants once and for all (in
C - Ellis sextion conventions).
      DO IXX=1,5
         DO JXX=1,5 
            IF(IXX.EQ.JXX) THEN
               S(IXX,JXX) = 0.0
            ELSE
               S(IXX,JXX) = DOTP(P1(0,ABS(ES_MAP(IXX))),
     $                           P1(0,ABS(ES_MAP(JXX))))
               IF((ES_MAP(IXX)*ES_MAP(JXX)).LT.0) S(IXX,JXX)=-S(IXX,JXX)
            ENDIF
         ENDDO
      ENDDO
C - Then we calculate the Ellis Sexton C-function (Eq. 3.6) as a sum
C - over permutations of the arguments of the F^C function (Eq. 3.7)
C - for the gluons only.
      RESULT=0.
      DO IXX=3,5
         DO LXX=3,5
            IF(LXX.EQ.IXX) CYCLE
            DO MXX=3,5
               IF((MXX.EQ.IXX).OR.
     $            (MXX.EQ.LXX)) CYCLE
               RESULT = RESULT + FC_FN(1,2,IXX,LXX,MXX,S)
            ENDDO
         ENDDO
      ENDDO
C - On the next line we obtain the C function (Eq.3.6).
      RESULT=GS6*RESULT
C - To get the matrix element squared we need to divide by the
C - spin colour averaging factor 4V^2 (see Table 7).
      RESULT=RESULT/AVG
C - We also need to include another symmetry factor of 3! to get
C - agreement with the MadGraph (and, hopefully, reality).
      RESULT=RESULT/SYM

      END


      FUNCTION FC_FN(I1,I2,I3,I4,I5,S)
      IMPLICIT NONE
      REAL*8 FC_FN
      INTEGER I1,I2,I3,I4,I5
      REAL*8 S(5,5),AP,AM,BP,BM,CP,CM
      REAL*8 N_GLUONS,N_COLOURS

      N_GLUONS  = 8.
      N_COLOURS = 3.

      AP = (S(I1,I3)+S(I2,I3))/2.
      AM = (S(I1,I3)-S(I2,I3))/2.
      BP = (S(I1,I4)+S(I2,I4))/2.
      BM = (S(I1,I4)-S(I2,I4))/2.
      CP = (S(I1,I5)+S(I2,I5))/2.
      CM = (S(I1,I5)-S(I2,I5))/2.
      
      FC_FN = N_GLUONS/N_COLOURS/N_COLOURS 
     $      * ( 
     $          -N_COLOURS*N_COLOURS*(AP*BP-AM*BM)/S(I3,I4)
     $          * ( 2.*(AP*AP+AM*AM)/S(I1,I4)/S(I2,I4)/S(I1,I5)/S(I2,I5)
     $            +    (CP*CP+CM*CM)/S(I1,I3)/S(I2,I3)/S(I1,I4)/S(I2,I4)
     $            )
     $          + N_COLOURS**4*(AP*BP-AM*BM)/S(I1,I2)/S(I4,I5)/S(I3,I5)
     $          * ( 2.*(BP*BP+BM*BM)/S(I1,I3)/S(I2,I3)
     $            + (CP**4-CM**4)/S(I1,I4)/S(I2,I4)/S(I1,I3)/S(I2,I3)
     $            )
     $          + (N_COLOURS*N_COLOURS+1.)*0.25*S(I1,I2)
     $          * 2.*(CP*CP+CM*CM)/S(I1,I3)/S(I2,I3)/S(I1,I4)/S(I2,I4)  
     $        )
      RETURN
      END


C ----------------------------------------------------------------- C
      SUBROUTINE ES_A(P1,RESULT,ES_MAP,AVG,SYM)
C - The squared, spin and colour summed matrix element for qq'->qq'g
C - , averaged over initial state colours and spins. This comes from
C - formulae in Ellis & Sexton, NPB269:445,1986 (Section 3). Agreement
C - with MadGraph was very satisfactory except for the bxb_uuxg type
C - process where agreement was off by up to 5%. However this disagreement
C - came only in the phase -before- "POWHEG: initialization", where I
C - think the limit checking is being done, I also note the fractional
C - differences were worst in these cases when the ME value was very
C - largest (which makes sense). Once POWHEG initialization agreement
C - the ME's agreed to better than 1 in 10^6.
C ----------------------------------------------------------------- C

      IMPLICIT NONE
C -   *************
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      Include "madgraph_5_flavours/genps.inc"
      REAL*8 P1(0:3,NEXTERNAL_REAL),AVG,SYM
      REAL*8 DOTP
      EXTERNAL DOTP
      REAL*8 S(5,5),FA_FN,RESULT
      INTEGER ES_MAP(5)
      INTEGER IXX,JXX
      REAL*8 GS6
C - Calculate the strong coupling constant prefactor
      GS6 = (4.*PI*ST_ALPHA)**3
C - First calculate the kinematic invariants once and for all (in
C - Ellis sextion conventions).
      DO IXX=1,5
         DO JXX=1,5 
            IF(IXX.EQ.JXX) THEN
               S(IXX,JXX) = 0.0
            ELSE
               S(IXX,JXX) = DOTP(P1(0,ABS(ES_MAP(IXX))),
     $                           P1(0,ABS(ES_MAP(JXX))))
               IF((ES_MAP(IXX)*ES_MAP(JXX)).LT.0) S(IXX,JXX)=-S(IXX,JXX)
            ENDIF
         ENDDO
      ENDDO
C - Then we calculate the Ellis Sexton A-function (Eq. 3.4/g_S^6).
      RESULT = FA_FN(1,2,3,4,5,S)
C - On the next line we obtain the A function (Eq.3.4).
      RESULT=GS6*RESULT
C - To get the matrix element squared we need to divide by the
C - spin colour averaging factor (see Table 7).
      RESULT=RESULT/AVG
C - We may also need to include another symmetry factor to get
C - agreement with the MadGraph (and, hopefully, reality).
      RESULT=RESULT/SYM

      END

      FUNCTION FA_FN(I1,I2,I3,I4,I5,S)
      IMPLICIT NONE
      REAL*8 FA_FN
      INTEGER I1,I2,I3,I4,I5
      REAL*8 S(5,5),SP,SM,TP,TM,UP,UM
      REAL*8 V1,V2

      V1 = 64./3.
      V2 =  8./3.

      SP = S(I1,I2)+S(I3,I4)
      SM = S(I1,I2)-S(I3,I4)
      TP = S(I1,I3)+S(I2,I4)
      TM = S(I1,I3)-S(I2,I4)
      UP = S(I1,I4)+S(I2,I3)
      UM = S(I1,I4)-S(I2,I3)
      
      FA_FN = V1*(SP*SP+SM*SM+UP*UP+UM*UM)
     $      * ( 2.00*UP*( S(I1,I2)*S(I3,I4)
     $                  + S(I1,I3)*S(I2,I4)
     $                  - S(I1,I4)*S(I2,I3) )
     $        + 2.00*S(I1,I4)*(S(I1,I2)*S(I1,I3)+S(I3,I4)*S(I2,I4))
     $        + 2.00*S(I2,I3)*(S(I1,I2)*S(I2,I4)+S(I3,I4)*S(I1,I3))
     $        )
     $      - V2*(SP*SP+SM*SM+UP*UP+UM*UM)
     $      * ( 2.00*SP*( S(I1,I2)*S(I3,I4)
     $                  - S(I1,I3)*S(I2,I4)
     $                  - S(I1,I4)*S(I2,I3)
     $                  )
     $        + UP*2.*S(I1,I3)*2.*S(I2,I4)
     $        + TP*2.*S(I1,I4)*2.*S(I2,I3)
     $        )

      FA_FN = FA_FN
     $     / ( 2. * S(I1,I3) * 2. * S(I2,I4) * S(I1,I5) 
     $            * S(I2,I5)      * S(I3,I5) * S(I4,I5)
     $       )
      RETURN
      END


C ----------------------------------------------------------------- C
      SUBROUTINE ES_B(P1,RESULT,ES_MAP,AVG,SYM)
C - The squared, spin and colour summed matrix element for qq->qqg
C - , averaged over initial state colours and spins. This comes from
C - formulae in Ellis & Sexton, NPB269:445,1986 (Section 3).
C ----------------------------------------------------------------- C

      IMPLICIT NONE
C -   *************
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      Include "madgraph_5_flavours/genps.inc"
      REAL*8 P1(0:3,NEXTERNAL_REAL),AVG,SYM
      REAL*8 DOTP
      EXTERNAL DOTP
      REAL*8 S(5,5),FB_FN,RESULT
      INTEGER ES_MAP(5)
      INTEGER IXX,JXX
      REAL*8 GS6
C - Calculate the strong coupling constant prefactor
      GS6 = (4.*PI*ST_ALPHA)**3
C - First calculate the kinematic invariants once and for all (in
C - Ellis sextion conventions).
      DO IXX=1,5
         DO JXX=1,5 
            IF(IXX.EQ.JXX) THEN
               S(IXX,JXX) = 0.0
            ELSE
               S(IXX,JXX) = DOTP(P1(0,ABS(ES_MAP(IXX))),
     $                           P1(0,ABS(ES_MAP(JXX))))
               IF((ES_MAP(IXX)*ES_MAP(JXX)).LT.0) S(IXX,JXX)=-S(IXX,JXX)
            ENDIF
         ENDDO
      ENDDO
C - Then we calculate the Ellis Sexton B-function (Eq. 3.5/g_S^6).
      RESULT = FB_FN(1,2,3,4,5,S)
C - On the next line we obtain the A function (Eq.3.4).
      RESULT=GS6*RESULT
C - To get the matrix element squared we need to divide by the
C - spin colour averaging factor (see Table 7).
      RESULT=RESULT/AVG
C - We may also need to include another symmetry factor to get
C - agreement with the MadGraph (and, hopefully, reality).
      RESULT=RESULT/SYM

      END

      FUNCTION FB_FN(I1,I2,I3,I4,I5,S)
      IMPLICIT NONE
      REAL*8 FB_FN,FA_FN
      INTEGER I1,I2,I3,I4,I5
      REAL*8 S(5,5),SP,SM,TP,TM,UP,UM
      REAL*8 V3,V4

      V3 = 80./18.
      V4 = 64./18.

      SP = S(I1,I2)+S(I3,I4)
      SM = S(I1,I2)-S(I3,I4)
      TP = S(I1,I3)+S(I2,I4)
      TM = S(I1,I3)-S(I2,I4)
      UP = S(I1,I4)+S(I2,I3)
      UM = S(I1,I4)-S(I2,I3)
      
      FB_FN = ( 2. * S(I1,I2) * 2. * S(I3,I4)
     $        - 2. * S(I1,I3) * 2. * S(I2,I4)
     $        - 2. * S(I1,I4) * 2. * S(I2,I3)
     $        )
     $        *
     $        ( 2. * S(I1,I2) * 2.* S(I1,I2)
     $        + 2. * S(I3,I4) * 2.* S(I3,I4) 
     $        )
     $        *
     $        V3
     $        * 
     $        ( 0.25 * SP * ( 2. * S(I1,I2) * 2. * S(I3,I4)
     $                      - 2. * S(I1,I3) * 2. * S(I2,I4)
     $                      - 2. * S(I1,I4) * 2. * S(I2,I3)
     $                      )
     $        + 0.5 * UP * 2. * S(I1,I3) * 2. * S(I2,I4)
     $        + 0.5 * TP * 2. * S(I1,I4) * 2. * S(I2,I3)
     $        )
     $        +
     $        ( 2. * S(I1,I2) * 2. * S(I3,I4)
     $        - 2. * S(I1,I3) * 2. * S(I2,I4)
     $        - 2. * S(I1,I4) * 2. * S(I2,I3)
     $        )
     $        *
     $        ( 2. * S(I1,I2) * 2.* S(I1,I2)
     $        + 2. * S(I3,I4) * 2.* S(I3,I4) 
     $        )
     $        *
     $        V4
     $        *
     $        ( 0.25 * SP * ( 2. * S(I1,I2) * 2. * S(I3,I4)
     $                      - 2. * S(I1,I3) * 2. * S(I2,I4)
     $                      - 2. * S(I1,I4) * 2. * S(I2,I3)
     $                      )
     $        - 0.50 * UP * 2. * S(I1,I3) * 2. * S(I2,I4)
     $        - 0.50 * TP * 2. * S(I1,I4) * 2. * S(I2,I3)
     $        - 0.25 * 2. * S(I1,I2) * ( 2. * S(I1,I3) * 2. * S(I1,I4)
     $                                 + 2. * S(I2,I4) * 2. * S(I2,I3)
     $                                 )
     $        - 0.25 * 2. * S(I3,I4) * ( 2. * S(I1,I3) * 2. * S(I2,I3)
     $                                 + 2. * S(I1,I4) * 2. * S(I2,I4)
     $                                 )
     $        )

      FB_FN = FB_FN
     $      / ( 2. * S(I1,I3) * 2. * S(I2,I4)
     $        * 2. * S(I1,I4) * 2. * S(I2,I3)
     $        * S(I1,I5) * S(I2,I5)
     $        * S(I3,I5) * S(I4,I5)
     $        )

      FB_FN = FB_FN
     $      + FA_FN(I1,I2,I3,I4,I5,S)
     $      + FA_FN(I1,I2,I4,I3,I5,S)

      RETURN
      END
