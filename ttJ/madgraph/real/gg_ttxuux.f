      SUBROUTINE Sgg_ttxuux(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL_REAL)
C  
C FOR PROCESS : g g -> t t~ u u~  
C  
C Crossing   1 is g g -> t t~ u u~  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      Include "../genps.inc"
      INTEGER                 NCOMB,     NCROSS         
      PARAMETER (             NCOMB=  64, NCROSS=  1)
      INTEGER    THEL
      PARAMETER (THEL=NCOMB*NCROSS)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL_REAL),ANS(NCROSS)
C  
C LOCAL VARIABLES 
C  
      INTEGER NHEL(NEXTERNAL_REAL,NCOMB),NTRY
      REAL*8 T, P(0:3,NEXTERNAL_REAL)
      REAL*8 gg_ttxuux
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL_REAL,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL_REAL), I
      LOGICAL GOODHEL(NCOMB,NCROSS)
      INTEGER NGRAPHS
      REAL*8 hwgt, xtot, xtry, xrej, xr, yfrac(0:ncomb)
      INTEGER idum, ngood, igood(ncomb), jhel, j, jj
      LOGICAL warned
      REAL     xran1
      EXTERNAL xran1
C  
C GLOBAL VARIABLES
C  
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_amps_gg_ttxuux/  amp2,       jamp2

      character*79         hel_buff
      common/to_helicity/  hel_buff

      REAL*8 POL(2)
      common/to_polarization/ POL

      integer          isum_hel
      logical                    multi_channel
      common/to_matrix/isum_hel, multi_channel
      INTEGER MAPCONFIG(0:LMAXCONFIGS), ICONFIG
      common/to_mconfigs/mapconfig, iconfig
      DATA NTRY,IDUM /0,-1/
      DATA xtry, xrej, ngood /0,0,0/
      DATA warned/.false./
      
      SAVE yfrac, igood, jhel
      DATA NGRAPHS /   38/          
      DATA jamp2(0) /  20/          
      DATA GOODHEL/THEL*.FALSE./
      DATA (NHEL(IHEL,   1),IHEL=1, 6) /-1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,   2),IHEL=1, 6) /-1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,   3),IHEL=1, 6) /-1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,   4),IHEL=1, 6) /-1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,   5),IHEL=1, 6) /-1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,   6),IHEL=1, 6) /-1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,   7),IHEL=1, 6) /-1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,   8),IHEL=1, 6) /-1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,   9),IHEL=1, 6) /-1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  10),IHEL=1, 6) /-1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  11),IHEL=1, 6) /-1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  12),IHEL=1, 6) /-1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  13),IHEL=1, 6) /-1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  14),IHEL=1, 6) /-1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  15),IHEL=1, 6) /-1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  16),IHEL=1, 6) /-1,-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  17),IHEL=1, 6) /-1, 1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  18),IHEL=1, 6) /-1, 1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  19),IHEL=1, 6) /-1, 1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  20),IHEL=1, 6) /-1, 1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  21),IHEL=1, 6) /-1, 1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  22),IHEL=1, 6) /-1, 1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  23),IHEL=1, 6) /-1, 1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  24),IHEL=1, 6) /-1, 1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  25),IHEL=1, 6) /-1, 1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  26),IHEL=1, 6) /-1, 1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  27),IHEL=1, 6) /-1, 1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  28),IHEL=1, 6) /-1, 1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  29),IHEL=1, 6) /-1, 1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  30),IHEL=1, 6) /-1, 1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  31),IHEL=1, 6) /-1, 1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  32),IHEL=1, 6) /-1, 1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  33),IHEL=1, 6) / 1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  34),IHEL=1, 6) / 1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  35),IHEL=1, 6) / 1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  36),IHEL=1, 6) / 1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  37),IHEL=1, 6) / 1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  38),IHEL=1, 6) / 1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  39),IHEL=1, 6) / 1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  40),IHEL=1, 6) / 1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  41),IHEL=1, 6) / 1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  42),IHEL=1, 6) / 1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  43),IHEL=1, 6) / 1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  44),IHEL=1, 6) / 1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  45),IHEL=1, 6) / 1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  46),IHEL=1, 6) / 1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  47),IHEL=1, 6) / 1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  48),IHEL=1, 6) / 1,-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  49),IHEL=1, 6) / 1, 1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  50),IHEL=1, 6) / 1, 1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  51),IHEL=1, 6) / 1, 1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  52),IHEL=1, 6) / 1, 1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  53),IHEL=1, 6) / 1, 1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  54),IHEL=1, 6) / 1, 1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  55),IHEL=1, 6) / 1, 1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  56),IHEL=1, 6) / 1, 1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  57),IHEL=1, 6) / 1, 1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  58),IHEL=1, 6) / 1, 1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  59),IHEL=1, 6) / 1, 1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  60),IHEL=1, 6) / 1, 1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  61),IHEL=1, 6) / 1, 1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  62),IHEL=1, 6) / 1, 1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  63),IHEL=1, 6) / 1, 1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  64),IHEL=1, 6) / 1, 1, 1, 1, 1, 1/
      DATA (  IC(IHEL,  1),IHEL=1, 6) / 1, 2, 3, 4, 5, 6/
      DATA (IDEN(IHEL),IHEL=  1,  1) / 256/
C ----------
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      DO IPROC=1,NCROSS
      CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL_REAL)
      DO IHEL=1,NEXTERNAL_REAL
         JC(IHEL) = +1
      ENDDO
       
      IF (multi_channel) THEN
          DO IHEL=1,NGRAPHS
              amp2(ihel)=0d0
              jamp2(ihel)=0d0
          ENDDO
          DO IHEL=1,int(jamp2(0))
              jamp2(ihel)=0d0
          ENDDO
      ENDIF
      ANS(IPROC) = 0D0
      write(hel_buff,'(16i5)') (0,i=1,NEXTERNAL_REAL)
      IF (ISUM_HEL .EQ. 0 .OR. NTRY .LT. 10) THEN
          DO IHEL=1,NCOMB
             IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN
                 T=gg_ttxuux(P ,NHEL(1,IHEL),JC(1))            
               DO JJ=1,nincoming
                 IF(POL(JJ).NE.1d0.AND.
     &              NHEL(JJ,IHEL).EQ.INT(SIGN(1d0,POL(JJ)))) THEN
                   T=T*ABS(POL(JJ))
                 ELSE IF(POL(JJ).NE.1d0)THEN
                   T=T*(2d0-ABS(POL(JJ)))
                 ENDIF
               ENDDO
               ANS(IPROC)=ANS(IPROC)+T
               IF (T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL,IPROC)) THEN
                   GOODHEL(IHEL,IPROC)=.TRUE.
                   NGOOD = NGOOD +1
                   IGOOD(NGOOD) = IHEL
               ENDIF
             ENDIF
          ENDDO
          JHEL = 1
          ISUM_HEL=MIN(ISUM_HEL,NGOOD)
      ELSE              !RANDOM HELICITY
          DO J=1,ISUM_HEL
              JHEL=JHEL+1
              IF (JHEL .GT. NGOOD) JHEL=1
              HWGT = REAL(NGOOD)/REAL(ISUM_HEL)
              IHEL = IGOOD(JHEL)
              T=gg_ttxuux(P ,NHEL(1,IHEL),JC(1))            
              DO JJ=1,nincoming
                IF(POL(JJ).NE.1d0.AND.
     &             NHEL(JJ,IHEL).EQ.INT(SIGN(1d0,POL(JJ)))) THEN
                  T=T*ABS(POL(JJ))
                ELSE IF(POL(JJ).NE.1d0)THEN
                  T=T*(2d0-ABS(POL(JJ)))
                ENDIF
              ENDDO
              ANS(IPROC)=ANS(IPROC)+T*HWGT
          ENDDO
          IF (ISUM_HEL .EQ. 1) THEN
              WRITE(HEL_BUFF,'(16i5)')(NHEL(i,IHEL),i=1,NEXTERNAL_REAL)
          ENDIF
      ENDIF
      IF (MULTI_CHANNEL) THEN
          XTOT=0D0
          DO IHEL=1,MAPCONFIG(0)
              XTOT=XTOT+AMP2(MAPCONFIG(IHEL))
          ENDDO
          IF (XTOT.NE.0D0) THEN
              ANS(IPROC)=ANS(IPROC)*AMP2(MAPCONFIG(ICONFIG))/XTOT
          ELSE
              ANS(IPROC)=0D0
          ENDIF
      ENDIF
      ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))
      ENDDO
      END
       
       
      REAL*8 FUNCTION gg_ttxuux(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL_REAL)
C  
C FOR PROCESS : g g -> t t~ u u~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=  38,NEIGEN= 20) 
      include "../genps.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  58, NCOLOR=  20) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL_REAL)
      INTEGER NHEL(NEXTERNAL_REAL), IC(NEXTERNAL_REAL)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
C  
C GLOBAL VARIABLES
C  
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_amps_gg_ttxuux/  amp2,       jamp2
      include "../coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1  )/           18/                                       
      DATA (CF(i,1  ),i=1  ,6  ) /    64,  -16,   56,   -8,   -6,    2/    
      DATA (CF(i,1  ),i=7  ,12 ) /    12,   -8,    2,   -7,    1,   -6/    
      DATA (CF(i,1  ),i=13 ,18 ) /    -7,   -6,   -7,    2,  -16,    2/    
      DATA (CF(i,1  ),i=19 ,20 ) /    56,   -7/                            
C               T[ 5, 4]T[ 3, 6, 2, 1]                                     
      DATA Denom(2  )/           18/                                       
      DATA (CF(i,2  ),i=1  ,6  ) /   -16,   64,   -8,   56,   56,   -8/    
      DATA (CF(i,2  ),i=7  ,12 ) /   -16,    2,    1,   10,   -7,   -7/    
      DATA (CF(i,2  ),i=13 ,18 ) /     1,    2,   57,   -6,   12,   -6/    
      DATA (CF(i,2  ),i=19 ,20 ) /    -6,   -6/                            
C               T[ 5, 4, 1]T[ 3, 6, 2]                                     
      DATA Denom(3  )/           18/                                       
      DATA (CF(i,3  ),i=1  ,6  ) /    56,   -8,   64,  -16,   -7,    1/    
      DATA (CF(i,3  ),i=7  ,12 ) /     2,   -7,   10,    1,    2,   -7/    
      DATA (CF(i,3  ),i=13 ,18 ) /    -8,    2,   -6,   12,   -6,   -6/    
      DATA (CF(i,3  ),i=19 ,20 ) /    57,   -6/                            
C               T[ 5, 4]T[ 3, 6, 2, 1]                                     
      DATA Denom(4  )/           18/                                       
      DATA (CF(i,4  ),i=1  ,6  ) /    -8,   56,  -16,   64,   57,   -7/    
      DATA (CF(i,4  ),i=7  ,12 ) /    -6,    1,   -7,    2,   -8,   -6/    
      DATA (CF(i,4  ),i=13 ,18 ) /     2,   -6,   56,  -16,    2,    2/    
      DATA (CF(i,4  ),i=19 ,20 ) /    -7,   -7/                            
C               T[ 5, 4, 1]T[ 3, 6, 2]                                     
      DATA Denom(5  )/           18/                                       
      DATA (CF(i,5  ),i=1  ,6  ) /    -6,   56,   -7,   57,   64,  -16/    
      DATA (CF(i,5  ),i=7  ,12 ) /    -8,   -6,    2,    2,   -6,   -8/    
      DATA (CF(i,5  ),i=13 ,18 ) /    -7,    1,   56,   -7,    2,   -7/    
      DATA (CF(i,5  ),i=19 ,20 ) /     2,  -16/                            
C               T[ 5, 4, 1]T[ 3, 6, 2]                                     
      DATA Denom(6  )/           18/                                       
      DATA (CF(i,6  ),i=1  ,6  ) /     2,   -8,    1,   -7,  -16,   64/    
      DATA (CF(i,6  ),i=7  ,12 ) /    56,    2,   -8,    1,   -7,    2/    
      DATA (CF(i,6  ),i=13 ,18 ) /    10,   -7,   -6,   -6,   -6,   57/    
      DATA (CF(i,6  ),i=19 ,20 ) /    -6,   12/                            
C               T[ 5, 4, 2, 1]T[ 3, 6]                                     
      DATA Denom(7  )/           18/                                       
      DATA (CF(i,7  ),i=1  ,6  ) /    12,  -16,    2,   -6,   -8,   56/    
      DATA (CF(i,7  ),i=7  ,12 ) /    64,   -6,   -7,   -7,   -6,    1/    
      DATA (CF(i,7  ),i=13 ,18 ) /     2,   -8,   -7,   -7,  -16,   56/    
      DATA (CF(i,7  ),i=19 ,20 ) /     2,    2/                            
C               T[ 3, 6]T[ 5, 4, 2, 1]                                     
      DATA Denom(8  )/           18/                                       
      DATA (CF(i,8  ),i=1  ,6  ) /    -8,    2,   -7,    1,   -6,    2/    
      DATA (CF(i,8  ),i=7  ,12 ) /    -6,   64,  -16,   56,   -8,   -6/    
      DATA (CF(i,8  ),i=13 ,18 ) /    -7,   57,    2,   -7,   56,   -7/    
      DATA (CF(i,8  ),i=19 ,20 ) /   -16,    2/                            
C               T[ 5, 4, 2]T[ 3, 6, 1]                                     
      DATA Denom(9  )/           18/                                       
      DATA (CF(i,9  ),i=1  ,6  ) /     2,    1,   10,   -7,    2,   -8/    
      DATA (CF(i,9  ),i=7  ,12 ) /    -7,  -16,   64,   -8,   56,    2/    
      DATA (CF(i,9  ),i=13 ,18 ) /     1,   -7,   -6,   57,   -6,   -6/    
      DATA (CF(i,9  ),i=19 ,20 ) /    12,   -6/                            
C               T[ 5, 4, 1, 2]T[ 3, 6]                                     
      DATA Denom(10 )/           18/                                       
      DATA (CF(i,10 ),i=1  ,6  ) /    -7,   10,    1,    2,    2,    1/    
      DATA (CF(i,10 ),i=7  ,12 ) /    -7,   56,   -8,   64,  -16,  -16/    
      DATA (CF(i,10 ),i=13 ,18 ) /    -8,   56,   12,   -6,   57,   -6/    
      DATA (CF(i,10 ),i=19 ,20 ) /    -6,   -6/                            
C               T[ 5, 4, 2]T[ 3, 6, 1]                                     
      DATA Denom(11 )/           18/                                       
      DATA (CF(i,11 ),i=1  ,6  ) /     1,   -7,    2,   -8,   -6,   -7/    
      DATA (CF(i,11 ),i=7  ,12 ) /    -6,   -8,   56,  -16,   64,   12/    
      DATA (CF(i,11 ),i=13 ,18 ) /     2,   -6,  -16,   56,   -7,   -7/    
      DATA (CF(i,11 ),i=19 ,20 ) /     2,    2/                            
C               T[ 3, 6]T[ 5, 4, 1, 2]                                     
      DATA Denom(12 )/           18/                                       
      DATA (CF(i,12 ),i=1  ,6  ) /    -6,   -7,   -7,   -6,   -8,    2/    
      DATA (CF(i,12 ),i=7  ,12 ) /     1,   -6,    2,  -16,   12,   64/    
      DATA (CF(i,12 ),i=13 ,18 ) /    56,   -8,  -16,    2,   -7,    2/    
      DATA (CF(i,12 ),i=19 ,20 ) /    -7,   56/                            
C               T[ 5, 4]T[ 3, 6, 1, 2]                                     
      DATA Denom(13 )/           18/                                       
      DATA (CF(i,13 ),i=1  ,6  ) /    -7,    1,   -8,    2,   -7,   10/    
      DATA (CF(i,13 ),i=7  ,12 ) /     2,   -7,    1,   -8,    2,   56/    
      DATA (CF(i,13 ),i=13 ,18 ) /    64,  -16,   -6,   -6,   -6,   12/    
      DATA (CF(i,13 ),i=19 ,20 ) /    -6,   57/                            
C               T[ 5, 4]T[ 3, 6, 1, 2]                                     
      DATA Denom(14 )/           18/                                       
      DATA (CF(i,14 ),i=1  ,6  ) /    -6,    2,    2,   -6,    1,   -7/    
      DATA (CF(i,14 ),i=7  ,12 ) /    -8,   57,   -7,   56,   -6,   -8/    
      DATA (CF(i,14 ),i=13 ,18 ) /   -16,   64,    2,    2,   56,  -16/    
      DATA (CF(i,14 ),i=19 ,20 ) /    -7,   -7/                            
C               T[ 5, 4, 2]T[ 3, 6, 1]                                     
      DATA Denom(15 )/           18/                                       
      DATA (CF(i,15 ),i=1  ,6  ) /    -7,   57,   -6,   56,   56,   -6/    
      DATA (CF(i,15 ),i=7  ,12 ) /    -7,    2,   -6,   12,  -16,  -16/    
      DATA (CF(i,15 ),i=13 ,18 ) /    -6,    2,   64,   -8,   10,    1/    
      DATA (CF(i,15 ),i=19 ,20 ) /     1,   -8/                            
C               T[ 3, 6, 2]T[ 5, 4, 1]                                     
      DATA Denom(16 )/           18/                                       
      DATA (CF(i,16 ),i=1  ,6  ) /     2,   -6,   12,  -16,   -7,   -6/    
      DATA (CF(i,16 ),i=7  ,12 ) /    -7,   -7,   57,   -6,   56,    2/    
      DATA (CF(i,16 ),i=13 ,18 ) /    -6,    2,   -8,   64,    1,   -8/    
      DATA (CF(i,16 ),i=19 ,20 ) /    10,    1/                            
C               T[ 3, 6]T[ 5, 4, 1, 2]                                     
      DATA Denom(17 )/           18/                                       
      DATA (CF(i,17 ),i=1  ,6  ) /   -16,   12,   -6,    2,    2,   -6/    
      DATA (CF(i,17 ),i=7  ,12 ) /   -16,   56,   -6,   57,   -7,   -7/    
      DATA (CF(i,17 ),i=13 ,18 ) /    -6,   56,   10,    1,   64,   -8/    
      DATA (CF(i,17 ),i=19 ,20 ) /    -8,    1/                            
C               T[ 3, 6, 1]T[ 5, 4, 2]                                     
      DATA Denom(18 )/           18/                                       
      DATA (CF(i,18 ),i=1  ,6  ) /     2,   -6,   -6,    2,   -7,   57/    
      DATA (CF(i,18 ),i=7  ,12 ) /    56,   -7,   -6,   -6,   -7,    2/    
      DATA (CF(i,18 ),i=13 ,18 ) /    12,  -16,    1,   -8,   -8,   64/    
      DATA (CF(i,18 ),i=19 ,20 ) /     1,   10/                            
C               T[ 3, 6]T[ 5, 4, 2, 1]                                     
      DATA Denom(19 )/           18/                                       
      DATA (CF(i,19 ),i=1  ,6  ) /    56,   -6,   57,   -7,    2,   -6/    
      DATA (CF(i,19 ),i=7  ,12 ) /     2,  -16,   12,   -6,    2,   -7/    
      DATA (CF(i,19 ),i=13 ,18 ) /    -6,   -7,    1,   10,   -8,    1/    
      DATA (CF(i,19 ),i=19 ,20 ) /    64,   -8/                            
C               T[ 5, 4]T[ 3, 6, 2, 1]                                     
      DATA Denom(20 )/           18/                                       
      DATA (CF(i,20 ),i=1  ,6  ) /    -7,   -6,   -6,   -7,  -16,   12/    
      DATA (CF(i,20 ),i=7  ,12 ) /     2,    2,   -6,   -6,    2,   56/    
      DATA (CF(i,20 ),i=13 ,18 ) /    57,   -7,   -8,    1,    1,   10/    
      DATA (CF(i,20 ),i=19 ,20 ) /    -8,   64/                            
C               T[ 5, 4]T[ 3, 6, 1, 2]                                     
C ----------
C BEGIN CODE
C ----------
      CALL VXXXXX(P(0,1   ),ZERO ,NHEL(1   ),-1*IC(1   ),W(1,1   ))        
      CALL VXXXXX(P(0,2   ),ZERO ,NHEL(2   ),-1*IC(2   ),W(1,2   ))        
      CALL OXXXXX(P(0,3   ),TMASS ,NHEL(3   ),+1*IC(3   ),W(1,3   ))       
      CALL IXXXXX(P(0,4   ),TMASS ,NHEL(4   ),-1*IC(4   ),W(1,4   ))       
      CALL OXXXXX(P(0,5   ),ZERO ,NHEL(5   ),+1*IC(5   ),W(1,5   ))        
      CALL IXXXXX(P(0,6   ),ZERO ,NHEL(6   ),-1*IC(6   ),W(1,6   ))        
      CALL FVOXXX(W(1,3   ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,7   ))     
      CALL JIOXXX(W(1,4   ),W(1,7   ),GG ,ZERO    ,ZERO    ,W(1,8   ))     
      CALL FVOXXX(W(1,5   ),W(1,8   ),GG ,ZERO    ,ZERO    ,W(1,9   ))     
      CALL IOVXXX(W(1,6   ),W(1,9   ),W(1,1   ),GG ,AMP(1   ))             
      CALL JVVXXX(W(1,8   ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,10  ))      
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,10  ),GG ,AMP(2   ))             
      CALL FVOXXX(W(1,5   ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,11  ))     
      CALL IOVXXX(W(1,6   ),W(1,11  ),W(1,8   ),GG ,AMP(3   ))             
      CALL FVIXXX(W(1,4   ),W(1,1   ),GG ,TMASS   ,TWIDTH  ,W(1,12  ))     
      CALL JIOXXX(W(1,12  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,13  ))     
      CALL FVOXXX(W(1,5   ),W(1,13  ),GG ,ZERO    ,ZERO    ,W(1,14  ))     
      CALL IOVXXX(W(1,6   ),W(1,14  ),W(1,2   ),GG ,AMP(4   ))             
      CALL JVVXXX(W(1,13  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,15  ))      
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,15  ),GG ,AMP(5   ))             
      CALL FVOXXX(W(1,5   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,16  ))     
      CALL IOVXXX(W(1,6   ),W(1,16  ),W(1,13  ),GG ,AMP(6   ))             
      CALL FVIXXX(W(1,12  ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,17  ))     
      CALL JIOXXX(W(1,17  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,18  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,18  ),GG ,AMP(7   ))             
      CALL FVOXXX(W(1,7   ),W(1,1   ),GG ,TMASS   ,TWIDTH  ,W(1,19  ))     
      CALL JIOXXX(W(1,4   ),W(1,19  ),GG ,ZERO    ,ZERO    ,W(1,20  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,20  ),GG ,AMP(8   ))             
      CALL JIOXXX(W(1,12  ),W(1,7   ),GG ,ZERO    ,ZERO    ,W(1,21  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,21  ),GG ,AMP(9   ))             
      CALL FVIXXX(W(1,4   ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,22  ))     
      CALL JIOXXX(W(1,22  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,23  ))     
      CALL FVOXXX(W(1,5   ),W(1,23  ),GG ,ZERO    ,ZERO    ,W(1,24  ))     
      CALL IOVXXX(W(1,6   ),W(1,24  ),W(1,1   ),GG ,AMP(10  ))             
      CALL JVVXXX(W(1,23  ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,25  ))      
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,25  ),GG ,AMP(11  ))             
      CALL IOVXXX(W(1,6   ),W(1,11  ),W(1,23  ),GG ,AMP(12  ))             
      CALL FVOXXX(W(1,3   ),W(1,1   ),GG ,TMASS   ,TWIDTH  ,W(1,26  ))     
      CALL JIOXXX(W(1,4   ),W(1,26  ),GG ,ZERO    ,ZERO    ,W(1,27  ))     
      CALL FVOXXX(W(1,5   ),W(1,27  ),GG ,ZERO    ,ZERO    ,W(1,28  ))     
      CALL IOVXXX(W(1,6   ),W(1,28  ),W(1,2   ),GG ,AMP(13  ))             
      CALL JVVXXX(W(1,27  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,29  ))      
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,29  ),GG ,AMP(14  ))             
      CALL IOVXXX(W(1,6   ),W(1,16  ),W(1,27  ),GG ,AMP(15  ))             
      CALL FVIXXX(W(1,22  ),W(1,1   ),GG ,TMASS   ,TWIDTH  ,W(1,30  ))     
      CALL JIOXXX(W(1,30  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,31  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,31  ),GG ,AMP(16  ))             
      CALL FVOXXX(W(1,26  ),W(1,2   ),GG ,TMASS   ,TWIDTH  ,W(1,32  ))     
      CALL JIOXXX(W(1,4   ),W(1,32  ),GG ,ZERO    ,ZERO    ,W(1,33  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,33  ),GG ,AMP(17  ))             
      CALL JIOXXX(W(1,22  ),W(1,26  ),GG ,ZERO    ,ZERO    ,W(1,34  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,34  ),GG ,AMP(18  ))             
      CALL JIOXXX(W(1,4   ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,35  ))     
      CALL JVVXXX(W(1,35  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,36  ))      
      CALL FVOXXX(W(1,5   ),W(1,36  ),GG ,ZERO    ,ZERO    ,W(1,37  ))     
      CALL IOVXXX(W(1,6   ),W(1,37  ),W(1,1   ),GG ,AMP(19  ))             
      CALL FVOXXX(W(1,11  ),W(1,35  ),GG ,ZERO    ,ZERO    ,W(1,38  ))     
      CALL IOVXXX(W(1,6   ),W(1,38  ),W(1,2   ),GG ,AMP(20  ))             
      CALL JVVXXX(W(1,36  ),W(1,1   ),G ,ZERO    ,ZERO    ,W(1,39  ))      
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,39  ),GG ,AMP(21  ))             
      CALL FVOXXX(W(1,11  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,40  ))     
      CALL IOVXXX(W(1,6   ),W(1,40  ),W(1,35  ),GG ,AMP(22  ))             
      CALL IOVXXX(W(1,6   ),W(1,11  ),W(1,36  ),GG ,AMP(23  ))             
      CALL FVOXXX(W(1,16  ),W(1,35  ),GG ,ZERO    ,ZERO    ,W(1,41  ))     
      CALL IOVXXX(W(1,6   ),W(1,41  ),W(1,1   ),GG ,AMP(24  ))             
      CALL JVVXXX(W(1,1   ),W(1,35  ),G ,ZERO    ,ZERO    ,W(1,42  ))      
      CALL FVOXXX(W(1,5   ),W(1,42  ),GG ,ZERO    ,ZERO    ,W(1,43  ))     
      CALL IOVXXX(W(1,6   ),W(1,43  ),W(1,2   ),GG ,AMP(25  ))             
      CALL JVVXXX(W(1,42  ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,44  ))      
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,44  ),GG ,AMP(26  ))             
      CALL FVOXXX(W(1,16  ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,45  ))     
      CALL IOVXXX(W(1,6   ),W(1,45  ),W(1,35  ),GG ,AMP(27  ))             
      CALL IOVXXX(W(1,6   ),W(1,16  ),W(1,42  ),GG ,AMP(28  ))             
      CALL JVVXXX(W(1,1   ),W(1,2   ),G ,ZERO    ,ZERO    ,W(1,46  ))      
      CALL FVIXXX(W(1,4   ),W(1,46  ),GG ,TMASS   ,TWIDTH  ,W(1,47  ))     
      CALL JIOXXX(W(1,47  ),W(1,3   ),GG ,ZERO    ,ZERO    ,W(1,48  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,48  ),GG ,AMP(29  ))             
      CALL FVOXXX(W(1,3   ),W(1,46  ),GG ,TMASS   ,TWIDTH  ,W(1,49  ))     
      CALL JIOXXX(W(1,4   ),W(1,49  ),GG ,ZERO    ,ZERO    ,W(1,50  ))     
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,50  ),GG ,AMP(30  ))             
      CALL JGGGXX(W(1,1   ),W(1,2   ),W(1,35  ),G ,W(1,51  ))              
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,51  ),GG ,AMP(31  ))             
      CALL JGGGXX(W(1,35  ),W(1,1   ),W(1,2   ),G ,W(1,52  ))              
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,52  ),GG ,AMP(32  ))             
      CALL JGGGXX(W(1,2   ),W(1,35  ),W(1,1   ),G ,W(1,53  ))              
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,53  ),GG ,AMP(33  ))             
      CALL FVOXXX(W(1,5   ),W(1,35  ),GG ,ZERO    ,ZERO    ,W(1,54  ))     
      CALL FVOXXX(W(1,54  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,55  ))     
      CALL IOVXXX(W(1,6   ),W(1,55  ),W(1,1   ),GG ,AMP(34  ))             
      CALL FVOXXX(W(1,54  ),W(1,1   ),GG ,ZERO    ,ZERO    ,W(1,56  ))     
      CALL IOVXXX(W(1,6   ),W(1,56  ),W(1,2   ),GG ,AMP(35  ))             
      CALL JVVXXX(W(1,35  ),W(1,46  ),G ,ZERO    ,ZERO    ,W(1,57  ))      
      CALL IOVXXX(W(1,6   ),W(1,5   ),W(1,57  ),GG ,AMP(36  ))             
      CALL IOVXXX(W(1,6   ),W(1,54  ),W(1,46  ),GG ,AMP(37  ))             
      CALL FVOXXX(W(1,5   ),W(1,46  ),GG ,ZERO    ,ZERO    ,W(1,58  ))     
      CALL IOVXXX(W(1,6   ),W(1,58  ),W(1,35  ),GG ,AMP(38  ))             
      JAMP(   1) = +AMP(   1)+AMP(  19)
      JAMP(   2) = -AMP(   2)+AMP(   5)+AMP(   9)-AMP(  21)+AMP(  26)
     &             +AMP(  32)-AMP(  33)
      JAMP(   3) = +AMP(   2)+AMP(   8)+AMP(  21)-AMP(  30)-AMP(  31)
     &             +AMP(  33)-AMP(  36)
      JAMP(   4) = +AMP(   3)+AMP(  23)
      JAMP(   5) = +AMP(   4)+AMP(  25)
      JAMP(   6) = -AMP(   5)+AMP(   7)-AMP(  26)-AMP(  29)+AMP(  31)
     &             -AMP(  32)+AMP(  36)
      JAMP(   7) = +AMP(   6)+AMP(  28)
      JAMP(   8) = +AMP(  10)-AMP(  19)
      JAMP(   9) = -AMP(  11)+AMP(  16)+AMP(  21)+AMP(  29)-AMP(  31)
     &             +AMP(  33)-AMP(  36)
      JAMP(  10) = +AMP(  11)-AMP(  14)+AMP(  18)-AMP(  21)+AMP(  26)
     &             +AMP(  32)-AMP(  33)
      JAMP(  11) = +AMP(  12)-AMP(  23)
      JAMP(  12) = +AMP(  13)-AMP(  25)
      JAMP(  13) = +AMP(  14)+AMP(  17)-AMP(  26)+AMP(  30)+AMP(  31)
     &             -AMP(  32)+AMP(  36)
      JAMP(  14) = +AMP(  15)-AMP(  28)
      JAMP(  15) = +AMP(  20)
      JAMP(  16) = +AMP(  22)+AMP(  38)
      JAMP(  17) = +AMP(  24)
      JAMP(  18) = +AMP(  27)-AMP(  38)
      JAMP(  19) = +AMP(  34)-AMP(  37)
      JAMP(  20) = +AMP(  35)+AMP(  37)
      gg_ttxuux = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          gg_ttxuux =gg_ttxuux+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
