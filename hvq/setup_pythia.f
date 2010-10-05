      subroutine setup_pythia
      include '../include/LesHouches.h'
      integer MDCY,MDME,KFDP,pycomp
      double precision brat
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
C   100       A : Rick Field's CDF Tune A                     (Oct 2002)
C   103      DW : Rick Field's CDF Tune DW                    (Apr 2006)
C   320 Perugia 0 : "Perugia" update of S0-Pro                (Feb 2009)
c      call PYTUNE(320)
      call PYTUNE(100)
      if(lprup(1).eq.1004) then
c make D+ and D- stable (using PDG codes)
         mdcy(pycomp(411),1)=0
         mdcy(pycomp(-411),1)=0
c make D0 and D0bar stable
         mdcy(pycomp(421),1)=0
         mdcy(pycomp(-421),1)=0
c see if we want to make also the D* stable
         if(powheginput('#dstarstable').eq.1) then
            mdcy(pycomp(413),1)=0
            mdcy(pycomp(-413),1)=0
            mdcy(pycomp(423),1)=0
            mdcy(pycomp(-423),1)=0         
         endif
      elseif(lprup(1).eq.1005) then
c make B+ and B- stable
         mdcy(pycomp(521),1)=0
         mdcy(pycomp(-521),1)=0
c make B0 and B0bar stable
         mdcy(pycomp(511),1)=0
         mdcy(pycomp(-511),1)=0
      endif
      end
