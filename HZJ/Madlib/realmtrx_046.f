      SUBROUTINE SREALMTRX_046(P1,ANS)
C  
C Generated by MadGraph II                                              
C MadGraph StandAlone Version
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : u~ b~ -> h e+ e- u~ b~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      Include "nexternal.inc"
      INTEGER                 NCOMB,     NCROSS         
      PARAMETER (             NCOMB=  64, NCROSS=  1)
      INTEGER    THEL
      PARAMETER (THEL=NCOMB*NCROSS)
      INTEGER NGRAPHS
      PARAMETER (NGRAPHS=  24)
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL),ANS(NCROSS)
C  
C LOCAL VARIABLES 
C  
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T
      REAL*8 REALMTRX_046
      REAL*8 ZERO
      PARAMETER(ZERO=0d0)
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL), I,L,K
      LOGICAL GOODHEL(NCOMB,NCROSS)
      DATA NTRY/0/
      INTEGER NGOOD,igood(ncomb),jhel
      data ngood /0/
      save igood,jhel
      REAL*8 hwgt
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_Ramps_046/  amp2,       jamp2

      integer j,jj
      integer max_bhel
      parameter ( max_bhel =          64 )

      INTEGER NCOLOR
      DATA NCOLOR   /   1/          
      DATA GOODHEL/THEL*.FALSE./
      DATA (NHEL(IHEL,   1),IHEL=1, 7) /-1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,   2),IHEL=1, 7) /-1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,   3),IHEL=1, 7) /-1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,   4),IHEL=1, 7) /-1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,   5),IHEL=1, 7) /-1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,   6),IHEL=1, 7) /-1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,   7),IHEL=1, 7) /-1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,   8),IHEL=1, 7) /-1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,   9),IHEL=1, 7) /-1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  10),IHEL=1, 7) /-1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  11),IHEL=1, 7) /-1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  12),IHEL=1, 7) /-1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  13),IHEL=1, 7) /-1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  14),IHEL=1, 7) /-1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  15),IHEL=1, 7) /-1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  16),IHEL=1, 7) /-1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  17),IHEL=1, 7) /-1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  18),IHEL=1, 7) /-1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  19),IHEL=1, 7) /-1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  20),IHEL=1, 7) /-1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  21),IHEL=1, 7) /-1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  22),IHEL=1, 7) /-1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  23),IHEL=1, 7) /-1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  24),IHEL=1, 7) /-1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  25),IHEL=1, 7) /-1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  26),IHEL=1, 7) /-1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  27),IHEL=1, 7) /-1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  28),IHEL=1, 7) /-1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  29),IHEL=1, 7) /-1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  30),IHEL=1, 7) /-1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  31),IHEL=1, 7) /-1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  32),IHEL=1, 7) /-1, 1,-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  33),IHEL=1, 7) / 1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  34),IHEL=1, 7) / 1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  35),IHEL=1, 7) / 1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  36),IHEL=1, 7) / 1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  37),IHEL=1, 7) / 1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  38),IHEL=1, 7) / 1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  39),IHEL=1, 7) / 1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  40),IHEL=1, 7) / 1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  41),IHEL=1, 7) / 1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  42),IHEL=1, 7) / 1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  43),IHEL=1, 7) / 1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  44),IHEL=1, 7) / 1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  45),IHEL=1, 7) / 1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  46),IHEL=1, 7) / 1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  47),IHEL=1, 7) / 1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  48),IHEL=1, 7) / 1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(IHEL,  49),IHEL=1, 7) / 1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(IHEL,  50),IHEL=1, 7) / 1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(IHEL,  51),IHEL=1, 7) / 1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(IHEL,  52),IHEL=1, 7) / 1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(IHEL,  53),IHEL=1, 7) / 1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(IHEL,  54),IHEL=1, 7) / 1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(IHEL,  55),IHEL=1, 7) / 1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(IHEL,  56),IHEL=1, 7) / 1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(IHEL,  57),IHEL=1, 7) / 1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(IHEL,  58),IHEL=1, 7) / 1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(IHEL,  59),IHEL=1, 7) / 1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(IHEL,  60),IHEL=1, 7) / 1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(IHEL,  61),IHEL=1, 7) / 1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(IHEL,  62),IHEL=1, 7) / 1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(IHEL,  63),IHEL=1, 7) / 1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(IHEL,  64),IHEL=1, 7) / 1, 1,-1, 1, 1, 1, 1/
      DATA (  IC(IHEL,  1),IHEL=1, 7) / 1, 2, 3, 4, 5, 6, 7/
      DATA (IDEN(IHEL),IHEL=  1,  1) /  36/
C ----------
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      DO IPROC=1,NCROSS
      DO IHEL=1,NEXTERNAL
         JC(IHEL) = +1
      ENDDO
      DO IHEL=1,NGRAPHS
          amp2(ihel)=0d0
      ENDDO
      jamp2(0)=dble(NCOLOR)
      DO IHEL=1,int(jamp2(0))
          jamp2(ihel)=0d0
      ENDDO
      ANS(IPROC) = 0D0
          DO IHEL=1,NCOMB
             IF (GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN
                 T=REALMTRX_046(P1,NHEL(1,IHEL),IHEL,JC(1))              
               ANS(IPROC)=ANS(IPROC)+T
               IF (T .GT. 0D0 .AND. .NOT. GOODHEL(IHEL,IPROC)) THEN
                   GOODHEL(IHEL,IPROC)=.TRUE.
                   NGOOD = NGOOD +1
                   IGOOD(NGOOD) = IHEL
               ENDIF
             ENDIF
          ENDDO
      ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))
      ENDDO
      END
       
       
      REAL*8 FUNCTION REALMTRX_046(P,NHEL,HELL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : u~ b~ -> h e+ e- u~ b~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER (NGRAPHS=  24,NEIGEN=  1) 
      include "nexternal.inc"
      INTEGER    NWAVEFUNCS     , NCOLOR
      PARAMETER (NWAVEFUNCS=  49, NCOLOR=   1) 
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL), HELL
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
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2(maxamps), jamp2(0:maxamps)
      common/to_Ramps_046/  amp2,       jamp2
      integer max_bhel
      parameter ( max_bhel =          64 )
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1  )/            1/                                       
      DATA (CF(i,1  ),i=1  ,1  ) /     2/                                  
C               T[ 2, 6]T[ 1, 7]                                           
C ----------
C BEGIN CODE
C ----------
      CALL OXXXXX(P(0,1   ),ZERO ,NHEL(1   ),-1*IC(1   ),W(1,1   ))        
      CALL OXXXXX(P(0,2   ),BMASS ,NHEL(2   ),-1*IC(2   ),W(1,2   ))       
      CALL SXXXXX(P(0,3   ),+1*IC(3   ),W(1,3   ))                         
      CALL IXXXXX(P(0,4   ),ZERO ,NHEL(4   ),-1*IC(4   ),W(1,4   ))        
      CALL OXXXXX(P(0,5   ),ZERO ,NHEL(5   ),+1*IC(5   ),W(1,5   ))        
      CALL IXXXXX(P(0,6   ),ZERO ,NHEL(6   ),-1*IC(6   ),W(1,6   ))        
      CALL IXXXXX(P(0,7   ),BMASS ,NHEL(7   ),-1*IC(7   ),W(1,7   ))       
      CALL FSOXXX(W(1,2   ),W(1,3   ),GHBOT ,BMASS   ,ZERO    ,W(1,        
     &     8   ))                                                          
      CALL JIOXXX(W(1,4   ),W(1,5   ),GAL ,ZERO    ,AWIDTH  ,W(1,9   ))    
      CALL FVIXXX(W(1,7   ),W(1,9   ),GAD ,BMASS   ,ZERO    ,W(1,10  ))    
      CALL JIOXXX(W(1,10  ),W(1,8   ),GG ,ZERO    ,ZERO    ,W(1,11  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,11  ),GG ,AMP(1   ))             
      CALL JIOXXX(W(1,4   ),W(1,5   ),GZL ,ZMASS   ,ZWIDTH  ,W(1,12  ))    
      CALL FVIXXX(W(1,7   ),W(1,12  ),GZD ,BMASS   ,ZERO    ,W(1,13  ))    
      CALL JIOXXX(W(1,13  ),W(1,8   ),GG ,ZERO    ,ZERO    ,W(1,14  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,14  ),GG ,AMP(2   ))             
      CALL FVOXXX(W(1,8   ),W(1,9   ),GAD ,BMASS   ,ZERO    ,W(1,15  ))    
      CALL JIOXXX(W(1,7   ),W(1,15  ),GG ,ZERO    ,ZERO    ,W(1,16  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,16  ),GG ,AMP(3   ))             
      CALL FVOXXX(W(1,8   ),W(1,12  ),GZD ,BMASS   ,ZERO    ,W(1,17  ))    
      CALL JIOXXX(W(1,7   ),W(1,17  ),GG ,ZERO    ,ZERO    ,W(1,18  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,18  ),GG ,AMP(4   ))             
      CALL FSIXXX(W(1,7   ),W(1,3   ),GHBOT ,BMASS   ,ZERO    ,W(1,        
     &     19  ))                                                          
      CALL JIOXXX(W(1,19  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,20  ))     
      CALL FVOXXX(W(1,1   ),W(1,9   ),GAU ,ZERO    ,ZERO    ,W(1,21  ))    
      CALL IOVXXX(W(1,6   ),W(1,21  ),W(1,20  ),GG ,AMP(5   ))             
      CALL FVOXXX(W(1,1   ),W(1,12  ),GZU ,ZERO    ,ZERO    ,W(1,22  ))    
      CALL IOVXXX(W(1,6   ),W(1,22  ),W(1,20  ),GG ,AMP(6   ))             
      CALL JIOXXX(W(1,7   ),W(1,8   ),GG ,ZERO    ,ZERO    ,W(1,23  ))     
      CALL IOVXXX(W(1,6   ),W(1,21  ),W(1,23  ),GG ,AMP(7   ))             
      CALL IOVXXX(W(1,6   ),W(1,22  ),W(1,23  ),GG ,AMP(8   ))             
      CALL FVOXXX(W(1,1   ),W(1,20  ),GG ,ZERO    ,ZERO    ,W(1,24  ))     
      CALL IOVXXX(W(1,6   ),W(1,24  ),W(1,9   ),GAU ,AMP(9   ))            
      CALL IOVXXX(W(1,6   ),W(1,24  ),W(1,12  ),GZU ,AMP(10  ))            
      CALL FVOXXX(W(1,1   ),W(1,23  ),GG ,ZERO    ,ZERO    ,W(1,25  ))     
      CALL IOVXXX(W(1,6   ),W(1,25  ),W(1,9   ),GAU ,AMP(11  ))            
      CALL IOVXXX(W(1,6   ),W(1,25  ),W(1,12  ),GZU ,AMP(12  ))            
      CALL FVOXXX(W(1,2   ),W(1,9   ),GAD ,BMASS   ,ZERO    ,W(1,26  ))    
      CALL JIOXXX(W(1,19  ),W(1,26  ),GG ,ZERO    ,ZERO    ,W(1,27  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,27  ),GG ,AMP(13  ))             
      CALL FVOXXX(W(1,2   ),W(1,12  ),GZD ,BMASS   ,ZERO    ,W(1,28  ))    
      CALL JIOXXX(W(1,19  ),W(1,28  ),GG ,ZERO    ,ZERO    ,W(1,29  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,29  ),GG ,AMP(14  ))             
      CALL FSOXXX(W(1,26  ),W(1,3   ),GHBOT ,BMASS   ,ZERO    ,W(1,        
     &     30  ))                                                          
      CALL JIOXXX(W(1,7   ),W(1,30  ),GG ,ZERO    ,ZERO    ,W(1,31  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,31  ),GG ,AMP(15  ))             
      CALL FSOXXX(W(1,28  ),W(1,3   ),GHBOT ,BMASS   ,ZERO    ,W(1,        
     &     32  ))                                                          
      CALL JIOXXX(W(1,7   ),W(1,32  ),GG ,ZERO    ,ZERO    ,W(1,33  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,33  ),GG ,AMP(16  ))             
      CALL FVIXXX(W(1,19  ),W(1,9   ),GAD ,BMASS   ,ZERO    ,W(1,34  ))    
      CALL JIOXXX(W(1,34  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,35  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,35  ),GG ,AMP(17  ))             
      CALL FVIXXX(W(1,19  ),W(1,12  ),GZD ,BMASS   ,ZERO    ,W(1,36  ))    
      CALL JIOXXX(W(1,36  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,37  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,37  ),GG ,AMP(18  ))             
      CALL JVSXXX(W(1,12  ),W(1,3   ),GZZH ,ZMASS   ,ZWIDTH  ,W(1,         
     &     38  ))                                                          
      CALL FVIXXX(W(1,7   ),W(1,38  ),GZD ,BMASS   ,ZERO    ,W(1,39  ))    
      CALL JIOXXX(W(1,39  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,40  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,40  ),GG ,AMP(19  ))             
      CALL FSIXXX(W(1,10  ),W(1,3   ),GHBOT ,BMASS   ,ZERO    ,W(1,        
     &     41  ))                                                          
      CALL JIOXXX(W(1,41  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,42  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,42  ),GG ,AMP(20  ))             
      CALL FSIXXX(W(1,13  ),W(1,3   ),GHBOT ,BMASS   ,ZERO    ,W(1,        
     &     43  ))                                                          
      CALL JIOXXX(W(1,43  ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,44  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,44  ),GG ,AMP(21  ))             
      CALL FVOXXX(W(1,2   ),W(1,38  ),GZD ,BMASS   ,ZERO    ,W(1,45  ))    
      CALL JIOXXX(W(1,7   ),W(1,45  ),GG ,ZERO    ,ZERO    ,W(1,46  ))     
      CALL IOVXXX(W(1,6   ),W(1,1   ),W(1,46  ),GG ,AMP(22  ))             
      CALL JIOXXX(W(1,7   ),W(1,2   ),GG ,ZERO    ,ZERO    ,W(1,47  ))     
      CALL FVOXXX(W(1,1   ),W(1,38  ),GZU ,ZERO    ,ZERO    ,W(1,48  ))    
      CALL IOVXXX(W(1,6   ),W(1,48  ),W(1,47  ),GG ,AMP(23  ))             
      CALL FVOXXX(W(1,1   ),W(1,47  ),GG ,ZERO    ,ZERO    ,W(1,49  ))     
      CALL IOVXXX(W(1,6   ),W(1,49  ),W(1,38  ),GZU ,AMP(24  ))            
      JAMP(   1) = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
     &             -AMP(   6)-AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)
     &             -AMP(  11)-AMP(  12)-AMP(  13)-AMP(  14)-AMP(  15)
     &             -AMP(  16)-AMP(  17)-AMP(  18)-AMP(  19)-AMP(  20)
     &             -AMP(  21)-AMP(  22)-AMP(  23)-AMP(  24)
      REALMTRX_046 = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP = (0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          REALMTRX_046 =REALMTRX_046+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
      END
       
       
