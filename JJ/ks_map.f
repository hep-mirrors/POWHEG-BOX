C --------------------------------------------------------------------
      SUBROUTINE KS_2TO2_MAP(VFLAV,KS_LABEL,MG_LABEL,KS_MAP)
C --------------------------------------------------------------------
C
C  Input:
C  -------
C  vflav     -  2 -> 2 flavour list
C
C  Output:
C  -------
C  ks_label - character string identifying vflav with a process in
C             the Kunszt-Soper paper (see below).
C  mg_label - character string identifying vflav with a
C             2->2 Madgraph process, using the filename
C             to which that process corresponds.
C  ks_map  -  a 1D integer array with 4 entries for mapping a particle
C             in vflav and its powheg momentum to the corresponding
C             Kunszt-Soper process. For example taking a process
C             u+d->d+u this has vflav = (2,1,1,2), with POWHEG-BOX
C             momenta p(mu,1),p(mu,2),p(mu,3),p(mu,4) respectively.
C             In KS notation this process is reordered u+d+ubar+dbar -> 0 
C             So KS particle 1 & k(mu,1) = POWHEG BOX particle 1 p(mu,1)
C                KS particle 2 & k(mu,2) = POWHEG BOX particle 2 p(mu,2)
C                KS particle 3 & k(mu,3) = POWHEG BOX particle 4 p(mu,4)
C                KS particle 4 & k(mu,4) = POWHEG BOX particle 3 p(mu,3)
C
C             So we define ks_map= (1,2,4,3) . 
C             Now provided we remember to flip the outgoing BOX momenta
C             p(mu,3/4)->-p(mu,3/4) first.
C             We need only ever call functions taken from the KS paper:
C             A_Fn(p(mu,ks_map(1)),p(mu,ks_map(2)),p(mu,ks_map(3)),p(mu,ks_map(4)))
C             i.e. no need to worry about crossing.
C             The same goes for the Bornjk(j,k) array indices.
C
C --------------------------------------------------------------------

      implicit none

      include 'nlegborn.h'
      include 'PhysPars.h'

      integer nleg
      parameter (nleg=nlegborn)

      integer vflav(nleg)   ! The flavour list
      character*3  ks_label ! Kunszt-Soper A1a/... /B1 ... /C1 ... /D etc
      character*10 mg_label ! Madgraph process file for vflav
      integer ixx           ! loop indices
      integer ngluons       ! number of gluons in vflav
      integer ks_map(4)     ! the map *FROM* POWHEG-BOX particles and
                            ! their momentum *TO* Kunszt Soper ones.
 
C --------------------------------------------------------------------
C     Sanity check: necessary but not sufficient condition for fermion
C                   number and flavor conservation
C --------------------------------------------------------------------
      if((vflav(1)+vflav(2)-vflav(3)-vflav(4)).ne.0) then
         write(*,*) 'setvirtual: flavour / fermion number violation!'
         write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
         call exit(1)
      endif

C --------------------------------------------------------------------
C     First identify broad A1/A2/A3/B/C1/C2/C3/C4/C5/C6/D structure
C     according to quark / gluon content.
C --------------------------------------------------------------------
      ngluons = 0
      do ixx = 1,4
         if(vflav(ixx).eq.0) ngluons = ngluons + 1
      enddo
C - Given the sanity check was passed, the following are -sufficient-
C - conditions to uniquely determine whether a process is B/C/D:
      if(ngluons.eq.4) then
         ks_label='D' 
      elseif(ngluons.eq.2) then
         ks_label='C'
         if((vflav(1).eq.-vflav(2)).and.(vflav(1).ne.0)) then
            ks_label='C1'
         elseif((vflav(1).eq.vflav(3)).and.(vflav(1).ne.0)) then
            ks_label='C2'
         elseif((vflav(2).eq.vflav(3)).and.(vflav(2).ne.0)) then
            ks_label='C3'
         elseif((vflav(3).eq.-vflav(4)).and.(vflav(3).ne.0)) then
            ks_label='C4'
         elseif((vflav(2).eq.vflav(4)).and.(vflav(2).ne.0)) then
            ks_label='C5'
         elseif((vflav(1).eq.vflav(4)).and.(vflav(1).ne.0)) then
            ks_label='C6'
         else
            write(*,*) 'setvirtual: failed to identify'
     $               //' C1/.../C6 substructure!'
            write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
            call exit(1)
         endif
      elseif(ngluons.eq.0) then
         if((abs(vflav(1)).eq.abs(vflav(2))).and.
     $      (abs(vflav(1)).eq.abs(vflav(3))).and.
     $      (abs(vflav(1)).eq.abs(vflav(4)))) then
            ks_label='B' 
         else
C - For A-type processes, garbage could potentially come through, e.g.
C - u+s->d+c, d+d->dbar+s, u+dbar->ubar+s, without additional checks:
            if((abs(vflav(1)).eq.abs(vflav(2))).and.
     $         (abs(vflav(3)).eq.abs(vflav(4)))) then
               ks_label='A3'   ! q + q -> Q + Q
            elseif((abs(vflav(1)).eq.abs(vflav(3))).and.
     $             (abs(vflav(2)).eq.abs(vflav(4)))) then
               ks_label='A1'   ! q + Q -> q + Q
            elseif((abs(vflav(1)).eq.abs(vflav(4))).and.
     $             (abs(vflav(2)).eq.abs(vflav(3)))) then 
               ks_label='A2'   ! Q + q -> q + Q
            else
               write(*,*) 'setvirtual: failed to identify'
     $                  //' A1/A2/A3 substructure!'
               write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
               call exit(1)
            endif
         endif
      else
         write(*,*) 'setvirtual: failed to identify A/B/C/D structure!'
         write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
         call exit(1)
      endif


C --------------------------------------------------------------------
C     A1-type: q + Q -> q + Q plus charge conjugations
C --------------------------------------------------------------------
      if(ks_label.eq.'A1') then

         if((vflav(1).gt.0).and.(vflav(2).gt.0)) then
C           q(p(mu,1))  + Q(p(mu,2))  -> q(p(mu,3))   + Q(p(mu,4))
C     ==>   q(p(mu,1))  + Q(p(mu,2))  +  qb(-p(mu,3)) + Qb(-p(mu,4)) -> 0
            mg_label  = 'ud_ud'
            ks_label  = 'A1a'
            ks_map(1) = 1
            ks_map(2) = 2
            ks_map(3) = 3
            ks_map(4) = 4
         elseif((vflav(1).lt.0).and.(vflav(2).gt.0)) then
C           qb(p(mu,1)) + Q(p(mu,2))  -> qb(p(mu,3)) + Q(p(mu,4))
C     ==>   q(-p(mu,3)) + Q(p(mu,2))  +  qb(p(mu,1)) + Qb(-p(mu,4)) -> 0
            mg_label  = 'uxd_uxd'
            ks_label  = 'A1b'
            ks_map(1) = 3
            ks_map(2) = 2
            ks_map(3) = 1
            ks_map(4) = 4
         elseif((vflav(1).gt.0).and.(vflav(2).lt.0)) then
C           q(p(mu,1))  + Qb(p(mu,2)) -> q(p(mu,3))   + Qb(p(mu,4))
C     ==>   q(p(mu,1))  + Q(-p(mu,4))  + qb(-p(mu,3)) + Qb(p(mu,2)) -> 0
            mg_label  = 'udx_udx'
            ks_label  = 'A1c'
            ks_map(1) = 1
            ks_map(2) = 4
            ks_map(3) = 3
            ks_map(4) = 2
         elseif((vflav(1).lt.0).and.(vflav(2).lt.0)) then
C           qb(p(mu,1)) + Qb(p(mu,2)) -> qb(p(mu,3)) + Qb(p(mu,4))
C     ==>   q(-p(mu,3)) + Q(-p(mu,4))  + qb(p(mu,1)) + Qb(p(mu,2)) -> 0
            mg_label  = 'uxdx_uxdx'
            ks_label  = 'A1d'
            ks_map(1) = 3
            ks_map(2) = 4
            ks_map(3) = 1
            ks_map(4) = 2
         else
            write(*,*) 'setvirtual: unidentified A1-type structure!'
            write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
            call exit(1)
         endif
         
C --------------------------------------------------------------------
C     A2-type: Q + q -> q + Q  (A1 with incoming beams swapped)
C --------------------------------------------------------------------
      elseif(ks_label.eq.'A2') then
         if((vflav(1).gt.0).and.(vflav(2).gt.0)) then
C           Q(p(mu,1)) + q(p(mu,2)) -> q(p(mu,3)) + Q(p(mu,4))
C     ==>   q(p(mu,2)) + Q(p(mu,1)) + qb(-p(mu,3)) + Qb(-p(mu,4)) -> 0
            mg_label  = 'du_ud'
            ks_label  = 'A2a'
            ks_map(1) = 2    ! KH: Should be equivalent to E.R.'s assignment 
            ks_map(2) = 1
            ks_map(3) = 3
            ks_map(4) = 4
         elseif((vflav(1).gt.0).and.(vflav(2).lt.0)) then
C           Q(p(mu,1)) + qb(p(mu,2)) -> qb(p(mu,3)) + Q(p(mu,4))
C     ==>   q(-p(mu,3)) + Q(p(mu,1)) + qb(p(mu,2)) + Qb(-p(mu,4)) -> 0
            mg_label  = 'dux_uxd'
            ks_label  = 'A2b'
            ks_map(1) = 3
            ks_map(2) = 1    ! KH: Should be equivalent to E.R.'s assignment 
            ks_map(3) = 2
            ks_map(4) = 4
         elseif((vflav(1).lt.0).and.(vflav(2).gt.0)) then
C           Qb(p(mu,1)) + q(p(mu,2)) -> q(p(mu,3)) + Qb(p(mu,4))
C     ==>   q(p(mu,2)) + Q(-p(mu,4)) + qb(-p(mu,3)) + Qb(p(mu,1)) -> 0
            mg_label  = 'dxu_udx'
            ks_label  = 'A2c'
            ks_map(1) = 2
            ks_map(2) = 4    ! KH: DIFFERENT to E.R.'s assignment 
            ks_map(3) = 3    ! N.B. original has A2b twice but no A2c/d
            ks_map(4) = 1
         elseif((vflav(1).lt.0).and.(vflav(2).lt.0)) then
C           Qb(p(mu,1)) + qb(p(mu,2)) -> qb(p(mu,3)) + Qb(p(mu,4))
C     ==>   q(-p(mu,3)) + Q(-p(mu,4)) + qb(p(mu,2)) + Qb(p(mu,1)) -> 0
            mg_label  = 'dxux_uxdx'
            ks_label  = 'A2d'
            ks_map(1) = 3
            ks_map(2) = 4    ! KH: DIFFERENT to E.R.'s assignment 
            ks_map(3) = 2    ! N.B. original has A2b twice but no A2c/d
            ks_map(4) = 1
         else
            write(*,*) 'setvirtual: unidentified A2-type structure!'
            write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
            call exit(1)
         endif

C --------------------------------------------------------------------
C     A3-type: q + q -> Q + Q plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'A3') then
         if((vflav(1).gt.0).and.(vflav(3).gt.0)) then
C           q(p(mu,1)) + qb(p(mu,2)) -> Q(p(mu,3)) + Qb(p(mu,4))
C     ==>   q(p(mu,1)) + Q(-p(mu,4)) + qb(p(mu,2)) + Qb(-p(mu,3)) -> 0
            mg_label  = 'uux_ddx'
            ks_label  = 'A3a'
            ks_map(1) = 1
            ks_map(2) = 4
            ks_map(3) = 2
            ks_map(4) = 3
         elseif((vflav(1).gt.0).and.(vflav(3).lt.0)) then
C           q(p(mu,1)) + qb(p(mu,2)) -> Qb(p(mu,3)) + Q(p(mu,4))
C     ==>   q(p(mu,1)) + Q(-p(mu,3)) + qb(p(mu,2)) + Qb(-p(mu,4)) -> 0
c$$$            write(*,*) 'Flavour list has no analogous MadGraph fn!' 
c$$$            write(*,*) 'A3b, q qb -> Qb Q : ', 
c$$$     $                 (vflav(ixx),ixx=1,4)
            mg_label  = 'NOT_IN_MG!'
            ks_label  = 'A3b'
            ks_map(1) = 1
            ks_map(2) = 3
            ks_map(3) = 2
            ks_map(4) = 4
         elseif((vflav(1).lt.0).and.(vflav(3).gt.0)) then
C           qb(p(mu,1)) + q(p(mu,2)) -> Q(p(mu,3)) + Qb(p(mu,4))
C     ==>   q(p(mu,2)) + Q(-p(mu,4)) + qb(p(mu,1)) + Qb(-p(mu,3)) -> 0
            mg_label  = 'uxu_ddx'
            ks_label  = 'A3c'
            ks_map(1) = 2
            ks_map(2) = 4
            ks_map(3) = 1
            ks_map(4) = 3
         elseif((vflav(1).lt.0).and.(vflav(3).lt.0)) then
C           qb(p(mu,1)) + q(p(mu,2)) -> Qb(p(mu,3)) + Q(p(mu,4))
C     ==>   q(p(mu,2)) + Q(-p(mu,3)) + qb(p(mu,1)) + Qb(-p(mu,4)) -> 0
c$$$            write(*,*) 'Flavour list has no analogous MadGraph fn!' 
c$$$            write(*,*) 'A3d, qb q -> Qb Q : ', 
c$$$     $                 (vflav(ixx),ixx=1,4)
            mg_label  = 'NOT_IN_MG!'
            ks_label  = 'A3d'
            ks_map(1) = 2
            ks_map(2) = 3    ! KH: DIFFERENT to E.R.'s assignment 
            ks_map(3) = 1
            ks_map(4) = 4
         else
            write(*,*) 'setvirtual: unidentified A3-type structure!'
            write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
            call exit(1)
         endif

C --------------------------------------------------------------------
C     B-type: q + q -> q + q plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'B') then

C - N.B. Momentum assignments for this class of processes are ambiguous
C - under swapping p1<->p2 and / or p3<->p4. I checked that the analytic
C - expression for the virtual here reflects this ambiguity i.e. it's
C - t<->u symmetric.

         if((vflav(1).gt.0).and.(vflav(2).gt.0)) then
C           q(p(mu,1)) + q(p(mu,2)) -> q(p(mu,3)) + q(p(mu,4))
C     ==>   q(p(mu,1)) + q(p(mu,2)) + qb(-p(mu,3)) + qb(-p(mu,4)) -> 0
            mg_label  = 'uu_uu'
            ks_label  = 'Ba'
            ks_map(1) = 1
            ks_map(2) = 2
            ks_map(3) = 3
            ks_map(4) = 4
         elseif((vflav(1).gt.0).and.(vflav(2).lt.0)) then
C           q(p(mu,1)) + qb(p(mu,2)) -> q(p(mu,3)) + qb(p(mu,4))
C     ==>   q(p(mu,1)) + q(-p(mu,4)) + qb(p(mu,2)) + qb(-p(mu,3)) -> 0
            mg_label  = 'uux_uux'
            ks_label  = 'Bb'
            ks_map(1) = 1
            ks_map(2) = 4
            ks_map(3) = 2
            ks_map(4) = 3
         elseif((vflav(1).lt.0).and.(vflav(2).gt.0)) then
C           qb(p(mu,1)) + q(p(mu,2)) -> q(p(mu,3)) + qb(p(mu,4))
C     ==>   q(p(mu,2)) + q(-p(mu,4)) + qb(p(mu,1)) + qb(-p(mu,3)) -> 0
            mg_label  = 'uxu_uux'
            ks_label  = 'Bc'
            ks_map(1) = 2
            ks_map(2) = 4
            ks_map(3) = 1
            ks_map(4) = 3
         elseif((vflav(1).lt.0).and.(vflav(2).lt.0)) then
C           qb(p(mu,1)) + qb(p(mu,2)) -> qb(p(mu,3)) + qb(p(mu,4))
C     ==>   q(-p(mu,3)) + q(-p(mu,4)) + qb(p(mu,1)) + qb(p(mu,2)) -> 0
            mg_label  = 'uxux_uxux'
            ks_label  = 'Bd'
            ks_map(1) = 3
            ks_map(2) = 4
            ks_map(3) = 1
            ks_map(4) = 2
         else
            write(*,*) 'setvirtual: unidentified B-type structure!'
            write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
            call exit(1)
         endif

C --------------------------------------------------------------------
C     C1-type: q + qb -> g + g plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'C1') then

C - N.B. - 
C - The following discrimination is not necessary from the point
C - of the virtual corrections since the \psi^(6)_{NS}(C;k) 
C - function is t<->u symmetric, however, not the same is not
C - true for all of the \psi^(4,c)(C;k) colour linked Born
C - functions. Besides which it is good to know the corresponding
C - MadGraph Born function.

         if(vflav(1).gt.0) then
C           q(p(mu,1)) + qb(p(mu,2)) -> g(p(mu,3)) + g(p(mu,4))
C     ==>   q(p(mu,1)) + qb(p(mu,2)) + g(-p(mu,3)) + g(-p(mu,4)) -> 0
            mg_label  = 'uux_gg'
            ks_label  = 'C1a'
            ks_map(1) = 1
            ks_map(2) = 2
            ks_map(3) = 3
            ks_map(4) = 4
         else
C           qb(p(mu,1)) + q(p(mu,2)) -> g(p(mu,3)) + g(p(mu,4))
C     ==>   q(p(mu,2)) + qb(p(mu,1)) + g(-p(mu,3)) + g(-p(mu,4)) -> 0
            mg_label  = 'uxu_gg'
            ks_label  = 'C1b'
            ks_map(1) = 2
            ks_map(2) = 1
            ks_map(3) = 3
            ks_map(4) = 4
         endif

C --------------------------------------------------------------------
C     C2-type: q + g -> q + g plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'C2') then
         if(vflav(1).gt.0) then
C           q(p(mu,1)) + g(p(mu,2)) -> q(p(mu,3)) + g(p(mu,4))
C     ==>   q(p(mu,1)) + qb(-p(mu,3)) + g(p(mu,2)) + g(-p(mu,4)) -> 0
            mg_label  = 'ug_ug'
            ks_label  = 'C2a'
            ks_map(1) = 1
            ks_map(2) = 3
            ks_map(3) = 2
            ks_map(4) = 4
         else
C           qb(p(mu,1)) + g(p(mu,2)) -> qb(p(mu,3)) + g(p(mu,4))
C     ==>   q(-p(mu,3)) + qb(p(mu,1)) + g(p(mu,2)) + g(-p(mu,4)) -> 0
            mg_label  = 'uxg_uxg'
            ks_label  = 'C2b'
            ks_map(1) = 3
            ks_map(2) = 1
            ks_map(3) = 2
            ks_map(4) = 4
         endif

C     Here also a - sign (a gluon is crossed) and
C     a change in the color-spin average is needed

C --------------------------------------------------------------------
C     C3-type: g + q -> q + g plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'C3') then
         if(vflav(2).gt.0) then
C           g(p(mu,1)) + q(p(mu,2)) -> q(p(mu,3)) + g(p(mu,4))
C     ==>   q(p(mu,2)) + qb(-p(mu,3)) + g(p(mu,1)) + g(-p(mu,4)) -> 0
            mg_label  = 'gu_ug'
            ks_label  = 'C3a'
            ks_map(1) = 2
            ks_map(2) = 3
            ks_map(3) = 1
            ks_map(4) = 4
         else
C           g(p(mu,1)) + qb(p(mu,2)) -> qb(p(mu,3)) + g(p(mu,4))
C     ==>   q(-p(mu,3)) + qb(p(mu,2)) + g(p(mu,1)) + g(-p(mu,4)) -> 0
            mg_label  = 'gux_uxg'
            ks_label  = 'C3b'
            ks_map(1) = 3
            ks_map(2) = 2
            ks_map(3) = 1
            ks_map(4) = 4
         endif

C     Here also a - sign (a gluon is crossed) and
C     a change in the color-spin average is needed

C --------------------------------------------------------------------
C     C4-type: g + g -> q + qb
C --------------------------------------------------------------------
      elseif(ks_label.eq.'C4') then
C        g(p(mu,1)) + g(p(mu,2)) -> q(p(mu,3)) + qb(p(mu,4))
C  ==>   q(-p(mu,4)) + qb(-p(mu,3)) + g(p(mu,1)) + g(p(mu,2)) -> 0
         mg_label  = 'gg_uux'
         ks_label  = 'C4'
         ks_map(1) = 4
         ks_map(2) = 3
         ks_map(3) = 1
         ks_map(4) = 2

C     Here a change in the color-spin average is needed

C --------------------------------------------------------------------
C     C5-type: g + q -> g + q plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'C5') then
         if(vflav(2).gt.0) then
C           g(p(mu,1)) + q(p(mu,2)) -> g(p(mu,3)) + q(p(mu,4))
C     ==>   q(p(mu,2)) + qb(-p(mu,4)) + g(p(mu,1)) + g(-p(mu,3)) -> 0
c$$$            write(*,*) 'Flavour list has no analogous MadGraph fn!' 
c$$$            write(*,*) 'C5a, g q -> g q : ', 
c$$$     $                 (vflav(ixx),ixx=1,4)
            mg_label  = 'NOT_IN_MG!'
            ks_label  = 'C5a'
            ks_map(1) = 2
            ks_map(2) = 4
            ks_map(3) = 1
            ks_map(4) = 3
         else
C           g(p(mu,1)) + qb(p(mu,2)) -> g(p(mu,3)) + qb(p(mu,4))
C     ==>   q(-p(mu,4)) + qb(p(mu,2)) + g(p(mu,1)) + g(-p(mu,3)) -> 0
c$$$            write(*,*) 'Flavour list has no analogous MadGraph fn!' 
c$$$            write(*,*) 'C5b, g qb -> g qb : ', 
c$$$     $                 (vflav(ixx),ixx=1,4)
            mg_label  = 'NOT_IN_MG!'
            ks_label  = 'C5b'
            ks_map(1) = 4
            ks_map(2) = 2
            ks_map(3) = 1
            ks_map(4) = 3
         endif

C     Here also a - sign (a gluon is crossed) and
C     a change in the color-spin average is needed

C --------------------------------------------------------------------
C     C6-type: q + g -> g + q plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'C6') then

         if(vflav(1).gt.0) then
C           q(p(mu,1)) + g(p(mu,2)) -> g(p(mu,3)) + q(p(mu,4))
C     ==>   q(p(mu,1)) + qb(-p(mu,4)) + g(p(mu,2)) + g(-p(mu,3)) -> 0
c$$$            write(*,*) 'Flavour list has no analogous MadGraph fn!' 
c$$$            write(*,*) 'C6a, q g -> g q : ', 
c$$$     $                 (vflav(ixx),ixx=1,4)
            mg_label  = 'NOT_IN_MG!'
            ks_label  = 'C6a'
            ks_map(1) = 1
            ks_map(2) = 4
            ks_map(3) = 2
            ks_map(4) = 3
         else
C           qb(p(mu,1)) + g(p(mu,2)) -> g(p(mu,3)) + qb(p(mu,4))
C     ==>   q(-p(mu,4)) + qb(p(mu,1)) + g(p(mu,2)) + g(-p(mu,3)) -> 0
c$$$            write(*,*) 'Flavour list has no analogous MadGraph fn!' 
c$$$            write(*,*) 'C6b, qb g -> g qb : ', 
c$$$     $                 (vflav(ixx),ixx=1,4)
            mg_label  = 'NOT_IN_MG!'
            ks_label  = 'C6a'
            ks_map(1) = 4
            ks_map(2) = 1
            ks_map(3) = 2
            ks_map(4) = 3
         endif

C     Here also a - sign (a gluon is crossed) and
C     a change in the color-spin average is needed

C --------------------------------------------------------------------
C     D-type: g + g -> g + g
C --------------------------------------------------------------------
      elseif(ks_label.eq.'D') then
C        g(p(mu,1)) + g(p(mu,2)) -> g(p(mu,3)) + g(p(mu,4))
C  ==>   g(p(mu,1)) + g(p(mu,2)) + g(-p(mu,3)) + g(-p(mu,4)) -> 0
         mg_label='gg_gg'
         ks_label  = 'D'
         ks_map(1) = 1
         ks_map(2) = 2
         ks_map(3) = 3
         ks_map(4) = 4

      else
         write(*,*) 'KS_MAP: Could not identify process type!'
         write(*,*) 'vflav = ',(vflav(ixx),ixx=1,4)
         call exit(1)
      endif
      
      end



