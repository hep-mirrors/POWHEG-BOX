c      program testfindregions
c      implicit none
c      integer n
c      parameter (n=6)
c      integer rflav(n),nregions,iregions(2,n*(n-1)/2),i
c      character  * 30 process
c
c     process = 'u~ u  -> e- e+ u  u~'
c     call from_madgraph_to_number(process,rflav) 
c     call find_regions(n,rflav,nregions,iregions)
c      
c     write(*,*) nregions
c     do i=1,nregions
c        write(*,*) iregions(1,i),iregions(2,i)
c     enddo
c      call genflavreglist
c      end



      subroutine mapflavours
c Sometimes it is convenient to treat lines with the same
c flavour as if they were different. In particular, in Higgs
c production by vector boson fusion, it is convenient to treat
c the lines attatched to the vector bosons as if they were all
c different. This is done by setting the arrays
c flst_realtags(nlegreal,flst_nreal) and flst_borntags(nlegborn,flst_nborn)
c to an integer labeling the particular fermion line.
c If the flst_borntags and flst_realtags are set, lines with different
c tags are treated as different lines from the point of view of finding
c the singular regions.
c
c The routine mapflavours changes the flavour value of each line in such
c a way that all lines that should be treated as different get a unique
c internal flavour number. The mapping between internal flavour number
c and real flavour and tags is held in the arrays fllist (flavour list),
c taglist (list of corresponding tags) and intfl (internal flavour list).
c
c Once the regions are all found, the internal flavour numbers are replaced
c by the original flavour numbers (routine unmapflavour)
c
      implicit none
      include 'pwhg_flg.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer fllist(nlegreal*maxprocreal),
     1    taglist(nlegreal*maxprocreal),intfl(nlegreal*maxprocreal),
     2    nflmap
      common/cmapflavours/fllist,taglist,intfl,nflmap
      integer l,ip,nflmapsav,k
      logical debug
      parameter(debug=.false.)
      nflmap=0
      do ip=1,flst_nreal
         do l=1,nlegreal
            call addflavour(flst_real(l,ip),flst_realtags(l,ip))
         enddo
      enddo
c now do the same with Born terms; nflmap should not change;
      nflmapsav=nflmap
      do ip=1,flst_nborn
         do l=1,nlegborn
            call addflavour(flst_born(l,ip),flst_borntags(l,ip))
         enddo
      enddo
      if(nflmap.gt.nflmapsav) then
         write(*,*) ' found Born flavour not present in real graphs'
         stop
      endif
      if (debug) then 
         write(*,*) ' flavour mapping'
         do k=1,nflmap
            write(*,*) fllist(k),taglist(k),intfl(k)
         enddo
         write(*,*) ' end flavour mapping'
      endif
      end

      subroutine unmapflavours
c Replace internal flavour numbers with real ones
c in all relevant flavour arrays
      implicit none
      include 'pwhg_flg.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      call unmaparr(flst_nreal,nlegreal,flst_real)
      call unmaparr(flst_nalr,nlegreal,flst_alr)
      call unmaparr(flst_nregular,nlegreal,flst_regular)
      call unmaparr(flst_nalr,nlegborn,flst_uborn)
      call unmaparr(flst_nborn,nlegborn,flst_born)
      end
      
      subroutine unmaparr(n,nlegs,arr)
      implicit none
      integer n,nlegs,arr(nlegs,n)
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer fllist(nlegreal*maxprocreal),
     1    taglist(nlegreal*maxprocreal),intfl(nlegreal*maxprocreal),
     2    nflmap
      common/cmapflavours/fllist,taglist,intfl,nflmap
      integer ileg,iproc,getrealflav
      external getrealflav
      do iproc=1,n
         do ileg=1,nlegs
            arr(ileg,iproc)=getrealflav(arr(ileg,iproc))
         enddo
      enddo
      end

      function getrealflav(fl)
      implicit none
      include 'pwhg_flg.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer getrealflav,fl
      integer fllist(nlegreal*maxprocreal),
     1    taglist(nlegreal*maxprocreal),intfl(nlegreal*maxprocreal),
     2    nflmap
      common/cmapflavours/fllist,taglist,intfl,nflmap
      integer ifl
      if(fl.eq.0) then
         getrealflav=0
         return
      endif
      do ifl=1,nflmap
         if(abs(fl).eq.intfl(ifl)) then
            getrealflav=fllist(ifl)*(fl/abs(fl))
            return
         endif
      enddo
      write(*,*) ' internal flavour not found in list'
      stop
      end

      subroutine addflavour(flav,tag)
c look if the pair flav, tag is already in the list fllist,taglist.
c If so, replace flav with the corresponding intfl. If it corresponds
c to the antiparticle of something in the list, replace it with minus
c the corresponding intfl.
      implicit none
      integer flav,tag
      include 'pwhg_flg.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer fllist(nlegreal*maxprocreal),
     1    taglist(nlegreal*maxprocreal),intfl(nlegreal*maxprocreal),
     2    nflmap
      common/cmapflavours/fllist,taglist,intfl,nflmap
      integer jfl
      if(flav.eq.0) then
         if(tag.ne.0) then
            write(*,*) ' addflavour: should not tag a gluon!'
            stop
         endif
      endif
      do jfl=1,nflmap
         if(flav.eq.fllist(jfl).and.tag.eq.taglist(jfl)) then
            flav=intfl(jfl)
            return
         elseif(flav.eq.-fllist(jfl).and.tag.eq.taglist(jfl)) then
            flav=-intfl(jfl)
            return
         endif
      enddo
c     new flavour to add
      nflmap=nflmap+1
      taglist(nflmap)=tag
      fllist(nflmap)=abs(flav)
      intfl(nflmap)=abs(flav)+tag*10000
      if(flav.eq.0) return
c see if this value is already in use; make it unique
 10   continue
      do jfl=1,nflmap-1
         if(intfl(nflmap).eq.intfl(jfl)) then
            intfl(nflmap)=intfl(nflmap)+1000
            if(intfl(nflmap).gt.1000000000) then
               write(*,*) ' cannot make unique id!'
               stop
            endif
            goto 10
         endif
      enddo
c     now it is unique
      flav=intfl(nflmap)*(flav/abs(flav))
      end
                  

      subroutine find_regions(nleg,rflav,nregions,iregions)
c the process has n particles, 1,2: initial, > 2 final
c integer rflav(n):  flavour of particles; 1,2: incoming flavour,
c                    > 2 outgoing flavour.
c                    Particles are labelled with PDG number scheme,
c                    EXCEPT for gluons, that are numbered ZERO
c                    (instead of 21)
c
c It returns:
c integer nregions
c integer iregion(2,nregions): the indices of particles forming singular
c                              regions i,j (i<j).
c                              For initial state singularities, if the
c                              emitter can be both of the initial state 
c                              particles, 
c                              and if the radiated particle is a gluon,
c                              only one region is generated with first
c                              index equal to zero.
c It calls: logical validBorn(n-1,bflav), that returns true if the flavour
c                                         configuration bflav admits
c                                         a non-vanishing Born amplitude.
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer nleg,rflav(nleg),nregions,iregions(2,maxregions)
      logical ireg(2)
      logical validBorn
      external validBorn
      integer iret,i,j,k,ibornfl,iborn,bflav(100)
      nregions=0
c final state regions
      do i=flst_lightpart,nleg
         do j=i+1,nleg
c find if they can arise from the same splitting
            call same_splitting_fsr(rflav(i),rflav(j),ibornfl,iret)
c cannot come from the same splitting
            if(iret.lt.0) then
               goto 10
            endif
c build the underlying born flavour structure in bflav
            iborn=0
            do k=1,nleg
               if(k.eq.i) then
                  iborn=iborn+1
                  bflav(iborn)=ibornfl
               elseif(k.ne.j) then
                  iborn=iborn+1
                  bflav(iborn)=rflav(k)
               endif
            enddo
            if(validBorn(nleg-1,bflav)) then
               nregions=nregions+1
               iregions(1,nregions)=i
               iregions(2,nregions)=j
            endif
 10         continue
         enddo
      enddo
c initial state region
      do j=flst_lightpart,nleg
         do i=1,2
            ireg(i)=.false.
            call same_splitting_isr(rflav(i),rflav(j),ibornfl,iret)
            if(iret.lt.0) then
               goto 11
            endif
            iborn=0
            do k=1,nleg
               if(k.eq.i) then
                  iborn=iborn+1
                  bflav(iborn)=ibornfl
               elseif(k.ne.j) then
                  iborn=iborn+1
                  bflav(iborn)=rflav(k)
               endif
            enddo
            if(validBorn(nleg-1,bflav)) then
               ireg(i)=.true.
            endif
 11         continue
         enddo
         if(ireg(1).and.ireg(2).and.(rflav(j).eq.0.or.rflav(j).eq.22))
     1        then
c if both regions are singular and the radiated parton is a gluon
c or a photon emit a single region with emitter 0
            nregions=nregions+1
            iregions(1,nregions)=0
            iregions(2,nregions)=j
         else
            if(ireg(1)) then
               nregions=nregions+1
               iregions(1,nregions)=1
               iregions(2,nregions)=j
            endif
            if(ireg(2)) then
               nregions=nregions+1
               iregions(1,nregions)=2
               iregions(2,nregions)=j
            endif
         endif
      enddo
      end

      subroutine ubornflav(n,j,rflav,bflav)
c finds the underlying Born flavour
c integer n: number of legs in real graph
c integer rflav(n): flavours of legs in real graph
c                   (1 and 2 incoming)
c integer j:        singularity in region j,n
c                   j=0 (1 and 2), j=1, j=2: initial state sing.
c                   j>2 final state sing.
      integer n,j,rflav(n),bflav(n-1)
      integer ibornfl
      iret=0
      if(j.eq.1.or.j.eq.2) then
         call same_splitting_isr(rflav(j),rflav(n),ibornfl,iret)
      elseif(j.gt.2) then
         call same_splitting_fsr(rflav(j),rflav(n),ibornfl,iret)
      endif
      if(iret.lt.0) then
         write(*,*) ' ubornflav: error'
         write(*,*) ' rflav:',rflav
         write(*,*) ' j: ',j
         call exit(-1)
      endif
      do l=1,n-1
         if(l.eq.j) then
            bflav(l)=ibornfl
         else
            bflav(l)=rflav(l)
         endif
      enddo
      return
 998  continue
      end

      function flavequiv(n,aflav,bflav)
c returns true if the flavour structures aflav and bflav are
c equivalent up to a permutation of the final state lines,
c false otherwise.
      implicit none
      logical flavequiv
      integer n, aflav(n),bflav(n)
c we need the parameter nlegreal
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer j,k,itmp,ib(nlegreal)
      call intassign(n,bflav,ib)
      do j=1,n
         if(aflav(j).ne.ib(j)) then
            if(j.le.2) then
               flavequiv=.false.
               return
            endif
            do k=j+1,n
               if(aflav(j).eq.ib(k)) then
                  itmp=ib(j)
                  ib(j)=ib(k)
                  ib(k)=itmp
                  goto 10
               endif
            enddo
            flavequiv=.false.
            return
         endif
 10      continue
      enddo
      flavequiv=.true.
      end

      function validBorn(n,bflav)
c Find if the flavour structure bflav is equivalent to an element
c in the list of Born processes. Equivalence means that it can be
c made identical with a permutation of final state particles.
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer n, bflav(n)
      logical validBorn
      integer j,k,kb,itmp,ib(nlegborn)
      call intassign(n,bflav,ib)
      do kb=1,flst_nborn
         do j=1,n
c recursive exit
            if(j.eq.n.and.flst_born(j,kb).eq.ib(j)) then
               validBorn=.true.
               return
            endif
            if(flst_born(j,kb).ne.ib(j)) then
               if(j.le.2) then
                  goto 999
               endif
               do k=j+1,n
                  if(flst_born(j,kb).eq.ib(k)) then
                     itmp=ib(j)
                     ib(j)=ib(k)
                     ib(k)=itmp
                     goto 10
                  endif
               enddo
               goto 999
            endif
 10         continue
         enddo         
 999     continue
      enddo
c      write(*,*) ' validBorn false,',bflav
      validBorn=.false.
      end

c      -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6                    
c      t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t                    
      subroutine from_madgraph_to_number(stringa,ferm_flav)
      implicit none
      integer nmax
      parameter (nmax=30)
      character stringa(nmax)
      integer ferm_flav(*)

      integer i, parton
      character *2 flav(-5:5)
      real * 8 charge(-5:5)
      common/flav_ordering/charge,flav

      parton = 0
      do i=1,nmax
         if (stringa(i).eq.'g') then
            parton = parton + 1
            ferm_flav(parton) = 0
         elseif (stringa(i).eq.'H') then
            parton = parton + 1
            ferm_flav(parton) = 503
         elseif (stringa(i).eq.'d') then
            parton = parton + 1
            ferm_flav(parton) = +1
         elseif (stringa(i).eq.'u') then
            parton = parton + 1
            ferm_flav(parton) = +2
         elseif (stringa(i).eq.'s') then
            parton = parton + 1
            ferm_flav(parton) = +3
         elseif (stringa(i).eq.'c') then
            parton = parton + 1
            ferm_flav(parton) = +4
         elseif (stringa(i).eq.'b') then
            parton = parton + 1
            ferm_flav(parton) = +5
         elseif (stringa(i).eq.'t') then
            parton = parton + 1
            ferm_flav(parton) = +6
         elseif (stringa(i).eq.'~') then
            ferm_flav(parton) = -ferm_flav(parton)
         elseif (stringa(i).eq.' ') then
         elseif (stringa(i).eq.'Z') then
            parton = parton + 1
            ferm_flav(parton) = +10
            parton = parton + 1
            ferm_flav(parton) = -10
          elseif (stringa(i).eq.'e') then
            parton = parton + 1
            ferm_flav(parton) = +10
          elseif (stringa(i).eq.'+') then
            ferm_flav(parton) = -ferm_flav(parton)           
          elseif (stringa(i).eq.'/') then
             return
        endif            
      enddo

      end
      function isalightparton(ipart)
      implicit none
      include 'pwhg_st.h'
      logical isalightparton
      integer ipart
      if(abs(ipart).le.st_nlight) then
         isalightparton=.true.
         return
      endif
      if(ipart.eq.22) then
         isalightparton=.true.
         return
      endif
      if(abs(ipart).ge.11.and.abs(ipart).le.16) then
         isalightparton=.true.
         return
      endif
      isalightparton=.false.
      end

      subroutine genflavreglist
      implicit none
      include 'pwhg_flg.h'
      include 'pwhg_st.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer nregions,iregions(2,maxregions)
      integer iflregl,k,l,ipart,j,itmp,nreg,iret,tmpfl,fl1,fl2
      logical equalintlists
      external equalintlists
      logical verbose
      parameter (verbose=.true.)
      logical flavequiv,isalightparton,is_lepton
      external flavequiv,isalightparton,is_lepton
c check that there are no coloured light partons before flst_lightpart
      do j=1,flst_nreal
         do ipart=3,flst_lightpart -1
            if(  isalightparton(flst_real(ipart,j))
     1         .and. .not.is_lepton(flst_real(ipart,j))  ) then
               write(*,*) 
     1      ' genflavreglist: light parton before flst_lightpart'
c               stop
            endif
         enddo
         do ipart=flst_lightpart,nlegreal
            if(.not.isalightparton(flst_real(ipart,j))) then
               write(*,*) 
     1      ' genflavreglist: not a light parton after flst_lightpart'
c               stop
            endif
         enddo
      enddo
      do j=1,flst_nborn
         do ipart=3,flst_lightpart-1
            if(isalightparton(flst_born(ipart,j))
     1         .and. .not.is_lepton(flst_born(ipart,j))  ) then
               write(*,*) 
     1      ' genflavreglist: light parton before flst_lightpart'
c               stop
            endif
         enddo
         do ipart=flst_lightpart,nlegborn
            if(.not.isalightparton(flst_born(ipart,j))) then
               write(*,*) 
     1      ' genflavreglist: not a light parton after flst_lightpart'
c               stop
            endif
         enddo
      enddo
c map flavours to internal flavour numbers      
      call mapflavours
c sanity check on real flavour configurations;
c they should all be inequivalent
      do j=1,flst_nreal
         do k=j+1,flst_nreal
            if(flavequiv(nlegreal,flst_real(1,j),flst_real(1,k))) then
               write(*,*)'found two equivalent real flavour processes:'
               write(*,*)'process',j,', flavours ',
     1                     (flst_real(l,j),l=1,nlegreal)
               write(*,*)'process',k,', flavours ',
     1                     (flst_real(l,k),l=1,nlegreal)
               stop
            endif
         enddo
      enddo
c sanity check on Born flavour configurations;
c they should all be inequivalent
      do j=1,flst_nborn
         do k=j+1,flst_nborn
            if(flavequiv(nlegborn,flst_born(1,j),flst_born(1,k))) then
               write(*,*)'found two equivalent Born flavour processes:'
               write(*,*)'process',j,', flavours ',
     1                     (flst_born(l,j),l=1,nlegborn)
               write(*,*)'process',k,', flavours ',
     1                     (flst_born(l,k),l=1,nlegborn)
               stop
            endif
         enddo
      enddo
c Start search for regions (i.e. alr)
c current number of alr found
      iflregl=0
      flst_nregular=0
      if(flst_nreal.gt.maxprocreal) then
         write(*,*)' genflavreglist: increase maxprocreal'
         stop
      endif
      flg_withreg=.false.
      do k=1,flst_nreal
         call find_regions(nlegreal,flst_real(1,k),nregions,iregions)
         if(nregions.eq.0) then
            flst_nregular=flst_nregular+1
c There are remnants! set up the appropriate flag:
            flg_withreg=.true.
            call intassign
     #(nlegreal,flst_real(1,k),flst_regular(1,flst_nregular))
         endif
         do l=1,nregions
            iflregl=iflregl+1
            if(iregions(1,l).le.2) then
               flst_emitter(iflregl)=iregions(1,l)
            else
               flst_emitter(iflregl)=nlegreal-1
            endif
            if(iflregl.ge.maxalr) then
               write(*,*)' genflavreglist: increase maxalr'
               stop
            endif
            ipart=0
c final state singularity
            if(iregions(1,l).gt.2) then
               do j=1,nlegreal
                  if(j.ne.iregions(1,l)
     #                  .and.j.ne.iregions(2,l)) then
                     ipart=ipart+1
                     flst_alr(ipart,iflregl)=flst_real(j,k)
                  endif
               enddo
               ipart=ipart+1
               flst_alr(ipart,iflregl)=flst_real(iregions(1,l),k)
               ipart=ipart+1
               flst_alr(ipart,iflregl)=flst_real(iregions(2,l),k)
c put always in the order q g and q q~, i.e. fl(i)>fl(j)
               fl1=flst_alr(nlegreal-1,iflregl)
               fl2=flst_alr(nlegreal,iflregl)
               if(  (fl2.ne.22 .and. fl2.ne.0)  .and.
     1           (  (fl1.eq.0 .or. fl1.eq.22) .or.
     1              (fl1.lt.fl2)    )  ) then
                  itmp=flst_alr(nlegreal,iflregl)
                  flst_alr(nlegreal,iflregl)=
     #                flst_alr(nlegreal-1,iflregl)
                  flst_alr(nlegreal-1,iflregl)=itmp
               endif
            else
c initial state singularity
               do j=1,nlegreal
                  if(j.ne.iregions(2,l)) then
                     ipart=ipart+1
                     flst_alr(ipart,iflregl)=flst_real(j,k)
                  endif
               enddo
               ipart=ipart+1
               flst_alr(ipart,iflregl)=flst_real(iregions(2,l),k)
            endif
c            write(*,*) (flst_alr(ipart,iflregl),ipart=1,nlegreal),
c     #     '   em:',flst_emitter(iflregl)
         enddo
      enddo
      nreg=iflregl
c bunch together identical elements, increasing their multiplicities
      do j=1,nreg
         flst_mult(j)=1
      enddo
      do j=1,nreg
         if(flst_mult(j).gt.0) then
            do k=j+1,nreg
c Previously was:
c               if(flst_emitter(j).eq.flst_emitter(k).and.
c     #  equalintlists(nlegreal,flst_alr(1,j),flst_alr(1,k))) then
c now accounts for equivalence by permutation of final state lines.
c Notice: identity of emitter and radiated parton must be valid
c  without permutations
               if(flst_mult(k).ne.0) then
                  if(flst_emitter(j).eq.flst_emitter(k).and.
c     ISR: is ISR, has same radiated parton, is equivalent
c          (excluding the radiated parton)
     1           ( (flst_emitter(j).lt.3 .and.
     2              flst_alr(nlegreal,j).eq.flst_alr(nlegreal,k).and.
     3              flavequiv(nlegreal-1,flst_alr(1,j),flst_alr(1,k)))
     4                   .or.
c     FSR: has the same radiated and emitter parton, is equivalent
c          (excluding emitter and emitted parton)
     5             (flst_alr(nlegreal,j).eq.flst_alr(nlegreal,k).and.
     6              flst_alr(nlegborn,j).eq.flst_alr(nlegborn,k).and.
     7              flavequiv(nlegreal-2,flst_alr(1,j),flst_alr(1,k)))
     8           )) then
                     flst_mult(j)=flst_mult(j)+flst_mult(k)
                     flst_mult(k)=0
                  endif
               endif
            enddo
         endif
      enddo
c browse the list, put together identical elements, compute
c associated underlying Born
      iflregl=0
      do j=1,nreg
         if(flst_mult(j).gt.0) then
            iflregl=iflregl+1
            if(j.gt.iflregl) then
               flst_emitter(iflregl)=flst_emitter(j)
               call intassign
     #          (nlegreal,flst_alr(1,j),flst_alr(1,iflregl))
               flst_mult(iflregl) = flst_mult(j)
            endif
            call ubornflav(nlegreal,flst_emitter(iflregl),
     #flst_alr(1,iflregl),flst_uborn(1,iflregl))
         endif
      enddo
      flst_nalr=iflregl

c
c Build unique list of underlying Born; reorder flavours in alpha_r, uborn, emitter
c so that the underlying Born matches exactly a Born flavour structure in the flst_born array
c flavour structures arising as underlying Born
      do j=1,flst_nalr
         do k=1,flst_nborn
c are they the same permutation?
            call reorder_regions(nlegborn,flst_born(1,k),
     #  flst_uborn(1,j),flst_alr(1,j),flst_emitter(j),iret)
c            if(iret.eq.1) write(*,*) ' reordering took place'
            if(iret.ne.-1) goto 11
         enddo
c they are inequivalent
         write(*,*) ' error: underlying born not present in born list'
         write(*,*) (flst_uborn(l,k),l=1,nlegborn)
         stop
 11      continue
      enddo


c Build pointers from alpha_r -> born
      do j=1,flst_nalr
         do k=1,flst_nborn
            if(equalintlists(nlegborn,flst_uborn(1,j),flst_born(1,k)))
     #      then
               flst_alr2born(j)=k
            endif
         enddo
      enddo
c Build pointers from born -> alpha_r
      do j=1,flst_nborn
         flst_born2alr(0,j)=0
         do k=1,flst_nalr
            if(equalintlists(nlegborn,flst_uborn(1,k),flst_born(1,j)))
     #      then
               flst_born2alr(0,j)=flst_born2alr(0,j)+1
               flst_born2alr(flst_born2alr(0,j),j)=k
            endif
         enddo
c Sanity check: each Born should be the underlying Born of some alr
         if(flst_born2alr(0,j).eq.0) then
            write(*,*) ' Born graph ',j,' is never the underlying Born'
     #           //' of some alr'
            stop
         endif
      enddo
c Find regions for each alpha_r
      do j=1,flst_nalr
         call find_regions(nlegreal,flst_alr(1,j),nregions,iregions)
         do k=1,nregions
            flst_allreg(1,k,j)=iregions(1,k)
            flst_allreg(2,k,j)=iregions(2,k)
         enddo
         flst_allreg(1,0,j)=nregions
      enddo
c For each region, compute the underlying Born multiplicity
      do j=1,flst_nalr
         if(flst_emitter(j).gt.2) then
            flst_ubmult(j)=0
c find flavour of emitter IN THE UNDERLYING BORN
            tmpfl=flst_uborn(flst_emitter(j),j)
            do k=3,nlegborn
               if(flst_uborn(k,j).eq.tmpfl) then
                  flst_ubmult(j)=flst_ubmult(j)+1
               endif
            enddo
         else
            flst_ubmult(j)=1
         endif
      enddo
      call unmapflavours
c     debug information
      if (verbose) then
         call pretty_print_flst
      endif
      end

      subroutine from_number_to_madgraph(n,flav,emitter,string)
      implicit none
      integer n,flav(n),emitter
      include 'nlegborn.h'
      character * (*) string
      integer min_partnames,max_partnames
      parameter (min_partnames=-16)
      parameter (max_partnames=22)
      character * 3 partnames(min_partnames:max_partnames)
      data partnames/'vt~','ta+','vm~','mu+','ve~','e+',' ','  ',' ',
     $     '  ','t~','b~','c~','s~','u~','d~','g ','d ','u ','s ' ,'c ',
     $     'b ','t ','  ','  ','',' ','e-','ve','mu-','vmu','ta-','vta',
     $     '  ','  ','  ','  ','  ','gam'/
      integer j,nsp
      parameter (nsp=4)
      if(len(string).lt.nsp*(n+1)+7) then
         write(*,*)'from_number_to_madgraph: string too short;'
         write(*,*)'Increase its size'
         call exit(-1)
      endif
      string=' '
      do j=1,n
         if (abs(flav(j)).le.max_partnames) then
            string(nsp*j:nsp*j+1)=partnames(flav(j))
         else 
            string(nsp*j:nsp*j+1)='**'
         endif
      enddo
      string(nsp*j:nsp*j)='|'
      if(emitter.gt.0) then
         string(nsp*emitter-1:nsp*emitter-1)='('
         string(nsp*emitter+2:nsp*emitter+2)=')'
      elseif(emitter.eq.0) then
         string(nsp-1:nsp-1)='('
         string(3*nsp-2:3*nsp-2)=')'
      endif
      do j=len(string)-7,2*nsp+1,-1
         string(j+7:j+7)=string(j:j)
      enddo
      string(2*nsp+5:2*nsp+8)='==>'
      end

      subroutine pretty_print_flst
      implicit none
      include 'nlegborn.h'
      character * 200 string,stringb
      include 'pwhg_flst.h'
      integer j,k,l,iun,lstring,lstringb
      call newunit(iun)
      open(unit=iun,file='FlavRegList',status='unknown')
      do j=1,flst_nalr
         call from_number_to_madgraph
     #         (nlegreal,flst_alr(1,j),flst_emitter(j),string)
         call from_number_to_madgraph
     #         (nlegborn,flst_uborn(1,j),-1,stringb)
         do lstring=len(string),0,-1
            if(string(lstring:lstring).ne.' ') goto 10
         enddo
 10      continue
         do lstringb=len(stringb),0,-1
            if(stringb(lstringb:lstringb).ne.' ') goto 11
         enddo
 11      continue
         write(iun,'(a,i3)') string(1:lstring)//' mult=', flst_mult(j)
         write(iun,'(a,i3)') stringb(1:lstringb)//' uborn, mult=',
     1        flst_ubmult(j)
         write(iun,'(20(1x,2(1x,i1)))')
     #   ((flst_allreg(l,k,j),l=1,2),k=1,flst_allreg(1,0,j))
      enddo
      close(iun)
      end


      subroutine intassign(n,iarr1,iarr2)
      implicit none
      integer n,iarr1(n),iarr2(n)
      integer j
      do j=1,n
         iarr2(j)=iarr1(j)
      enddo
      end

      function equalintlists(n,iarr1,iarr2)
      implicit none
      integer n,iarr1(n),iarr2(n)
      logical equalintlists
      integer j
      do j=1,n
         if(iarr2(j).ne.iarr1(j)) then
            equalintlists=.false.
            return
         endif
      enddo
      equalintlists=.true.
      end

      subroutine reorder_regions(n,uborn0,uborn,rflav,emit,iret)
c reorders the amplitude in uborn according to the amplitude in uborn0
c It also reorders rflav, and sets the emitter to its appropriate value
c If the amplitudes have inequivalent flavour structures, it returns -1
c without any other action.
c If the flavour structures are identical, it returns 0, with no other action.
c If the flavour structures have been reordered, it returns 1.
      implicit none
      integer n,uborn0(n),uborn(n),rflav(n),emit,iret
      integer j,k,itmp,ib(100)
      iret=0
      call intassign(n,uborn,ib)
      do j=1,n
         if(uborn0(j).ne.ib(j)) then
            if(j.le.2) then
               iret=-1
               goto 999
            endif
            iret=1
            do k=j+1,n
               if(uborn0(j).eq.ib(k)) then
                  itmp=ib(j)
                  ib(j)=ib(k)
                  ib(k)=itmp
                  goto 10
               endif
            enddo
c they differ in flavour content
            iret=-1
            goto 999
 10         continue
         endif
      enddo
c they are identical; no reordering needed
      if(iret.eq.0) return
c reorder
      do j=3,n
         if(uborn0(j).ne.uborn(j)) then
            iret=1
            do k=j+1,n
               if(uborn0(j).eq.uborn(k)) then
                  itmp=uborn(j)
                  uborn(j)=uborn(k)
                  uborn(k)=itmp
                  itmp=rflav(j)
                  rflav(j)=rflav(k)
                  rflav(k)=itmp
                  if(emit.eq.j) emit=k
                  if(emit.eq.k) emit=j
                  goto 11
               endif
            enddo
            write(*,*) ' should never get here'
 11         continue
         endif
      enddo
 999  continue
      end

      function valid_emitter(j)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      logical valid_emitter
      integer j,alr
      integer validarr(0:nlegborn)
      data validarr/nlegreal*0/
      save validarr
      if(validarr(j).eq.1) then
         valid_emitter=.true.
         return
      elseif(validarr(j).eq.-1) then
         valid_emitter=.false.
         return
      else
         do alr=1,flst_nalr
            if(j.eq.flst_emitter(alr)) then
               valid_emitter=.true.
               validarr(j)=1
               return
            endif
         enddo
         valid_emitter=.false.
         validarr(j)=-1
         return
      endif
      end


      subroutine same_splitting_fsr(fl1,fl2,ibornfl,iret)
      implicit none
      integer fl1,fl2,ibornfl,iret
      logical is_charged, is_coloured
      external is_charged, is_coloured
      iret=1
CCCCC     !!!!!!!!!!! WARNING !!!!!!!!!!!     CCCCCCCCC
C    for NC DY the quark-quark splitting is always    C
C    due to the photon. uncomment these 2 lines below C
C    for any other process
c      if(fl1+fl2.eq.0.and.is_coloured(fl1)) then
c         ibornfl=0
      if(fl1+fl2.eq.0.and.is_charged(fl1)) then
          ibornfl=22
      elseif(fl2.eq.0.and.is_coloured(fl1)) then
         ibornfl=fl1
      elseif(fl1.eq.0.and.is_coloured(fl2)) then
         ibornfl=fl2
      elseif(fl1.eq.22.and.is_charged(fl2)) then
         ibornfl=fl2
      elseif(fl2.eq.22.and.is_charged(fl1)) then
         ibornfl=fl1
      else
c cannot come from the same splitting
         iret=-1
      endif
      end

      subroutine same_splitting_isr(flin,flout,fl,iret)
      implicit none
      integer flin,flout,fl,iret
      if(flout.eq.22) then
          call same_splitting_fsr(flin,flout,fl,iret)
      else
          call same_splitting_fsr(flin,-flout,fl,iret)
      endif
      end

      function is_charged(fl)
      implicit none
      logical is_charged
      integer fl
      if(fl.eq.0) then
         is_charged=.false.
      elseif(abs(fl).le.6) then
         is_charged=.true.
      elseif(abs(fl).ge.11.and.abs(fl).le.15.and.2*(fl/2).ne.fl) then
         is_charged=.true.
      else
         is_charged=.false.
      endif
      end

      function is_coloured(fl)
      implicit none
      logical is_coloured
      integer fl
      if(abs(fl).le.6) then
         is_coloured=.true.
      else
         is_coloured=.false.
      endif
      end

      function is_lepton(fl)
      implicit none
      logical is_lepton
      integer fl
      if(abs(fl).le.16.and.abs(fl).ge.11) then
         is_lepton=.true.
      else
         is_lepton=.false.
      endif
      end
