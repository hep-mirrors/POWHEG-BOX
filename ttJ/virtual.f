c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point
      subroutine setvirtual(p,vflav,virtual)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      include 'pwhg_br.h'
      include 'pwhg_kn.h'
      include 'PhysPars.h'
      integer nlegs
      parameter (nlegs=nlegborn)
      double precision p(0:3,nlegs)
      integer vflav(nlegs)
      double precision virtual,powheginput
      external powheginput
      logical ini
      data ini/.true./ 
      save ini
      real *8 dotp
      external dotp
c     
      logical use_OLP_Interface
      common /colp/use_OLP_Interface
c
      integer OLP_code,jb,j
c     from born flavour structure to jborn (for simplicity, here
c     use only MASSLESS colored particles, sorted as:
c     id_plus,id_minus,id_final)
c     Particle 3 and 4 are always heavy colored particles
      integer bornflst2pwhgcode(-5:5,-5:5,-5:5)
      common/cbornflst2pwhgcode/bornflst2pwhgcode
c     from powheg born code to the OLP code
      integer pwhgcode2OLPcode(maxprocborn)
      common/cpwhgcode2OLPcode/pwhgcode2OLPcode
c     
      real * 8 c(-6:6),gamma(-6:6),gammap(-6:6)
      save c,gamma,gammap
      double precision bcut,largecorrfact
      logical checklargecorr
      common /ccheckvirtuals/bcut,largecorrfact,checklargecorr
      save /ccheckvirtuals/
      logical fftestflag
      common /cfftestflag/fftestflag
      save /cfftestflag/
      if (ini) then
         bcut=powheginput("#bcut") 
         largecorrfact=powheginput("#largecorrfact") 
         fftestflag=.false.
         if(powheginput("#ffltest").eq.1d0) fftestflag=.true.
c from 2.100 of FNO2007
         do j=-6,6
            if(j.eq.0) then
               c(j)=ca
               gamma(j)=(11*ca-4*tf*st_nlight)/6
               gammap(j)=(67d0/9-2*pi**2/3)*ca-23d0/9*tf*st_nlight
            else
               c(j)=cf
               gamma(j)=3d0/2*cf
               gammap(j)=(13d0/2-2*pi**2/3)*cf
            endif
         enddo
         if(use_OLP_interface) then
            call virtual_initialize_OLP
         else
c     initialize SM parameters and Uwer's routines
            call virtual_initialize
         endif
         ini=.false.
      endif

      if(.not.use_OLP_interface) then
         call virtual_evaluate(p,vflav,virtual)
      else
         OLP_code=pwhgcode2OLPcode(bornflst2pwhgcode(vflav(1),vflav(2)
     $        ,vflav(5)))
         call virtual_OLP(p,OLP_code,virtual)
      endif


c     Add couplings
!     This is the finite part of the virtual contribution ===> Vfin
!     a factor as/(2pi) is missing as required by sigsoftvirt
!     The division by 2 is the leftover from having factorized out
!     as/(2pi) instead of as/(4pi) 
      virtual=(4*pi*st_alpha)**3 * virtual  /2d0

c     missing terms due to different definitions of Vfin in Uwer's code
c     first identify the flst label
      jb=bornflst2pwhgcode(vflav(1),vflav(2),vflav(5))
c     then subtract the missing term avoiding recalculating born
      virtual= virtual - 4d0*dotp(p(0,1),p(0,2)) * br_born(jb)
     $     *(c(flst_born(1,jb))+c(flst_born(2,jb))+c(flst_born(5,jb)))
     $     *pi*pi/6d0

      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

ccccccccccccccccccccccccccccccccccccccccc
c     Generic virtual interface
ccccccccccccccccccccccccccccccccccccccccc
      subroutine virtual_initialize
      implicit none
      write(*,*) 'Error in virtual_initialize (virtual.f):'
      write(*,*) 'No virtual corrections present'
      write(*,*) 'A ttj virtual library is available' 
      write(*,*) 'from the authors upon request'
      call exit(-1)
      return
      end

      subroutine virtual_evaluate(p,vflav,virtual)
      implicit none
      double precision p(0:3,6)
      integer vflav(6)
      double precision virtual
      double precision born,dummy(0:3,0:3,6),bornjk(6,6)
      virtual=0.0
      write(*,*) 'Error in virtual_evaluate:'
      write(*,*) 'No virtual corrections present'
      write(*,*) 'A C++ library for ttbar+jet virtuals' 
      write(*,*) 'is available from the authors upon request' 
      call exit(-1)
      return
      end



      subroutine virtual_initialize_OLP
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      logical debug
      parameter (debug=.false.)
c     from powheg born code to the OLP code
      integer pwhgcode2OLPcode(maxprocborn)
      common/cpwhgcode2OLPcode/pwhgcode2OLPcode


      character*11 filename
      integer iproc
      integer iun
      integer ios,j,k,l
      character *5 status
      integer code,foundbproc

      character * 100 line,line0

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c  Comment this box and uncomment OLP_start calls
c  to link against an OLP            
      write(*,*) 'Error in virtual_initialize_OLP (virtual.f):'
      write(*,*) 'No virtual corrections present'
      write(*,*) 'An external OLP should be linked'
      call exit(-1)
      return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      write(*,*) 
      write(*,*) ' Checking contract file for external OLP '
      write(*,*) 
      filename="contract.lh"
c      call OLP_Start(ios,filename//CHAR(0))
      if (ios.ne.1) then
         write(*,*) ' Error: OLP cannot handle contract file'
         call exit(1)
      endif
      
c     open the negotiation file, read it and
c     fill the array pwhgbcode2OLPcode properly
      call newunit(iun)
      open(unit=iun,file='contract.lh',status='old',iostat=ios)
      if(ios.ne.0) then
         write(*,*) 'cannot open contract.lh'
         call exit(1)
      endif

      foundbproc=0
      do l=1,maxprocborn
 111     continue
         line0=' '
         read(unit=iun,fmt='(a)',iostat=ios) line0
         if(debug) write(*,*) line0
         if(ios.ne.0.and.line0.eq.' ') goto 10
c     this means end of file...
         line=line0
         if(line(11:12).ne.'->') then
c     this means that current line is not a line with a 2->3 subprocess
            if(debug) write(*,*) 'Found a line without subprocess'
            goto 111
         endif
         do k=1,100
            if(line(k:k).eq.'#'.or.line(k:k).eq.'!') then
               print*, 'commented line'
               line(k:)=' '
               goto 123
            endif
            if(line(k:k).eq.'|') then
               line=line(k+1:)
               read(unit=line,fmt=*,iostat=ios) status,code
               if(debug) then
                  write(*,*) '\t line with info: '//
     $                 'status,OLPcode: ',status,code
               endif
               if(status.eq."OK") then
                  foundbproc=foundbproc+1
                  pwhgcode2OLPcode(foundbproc)=code
                  goto 123                  
               endif
            endif
         enddo
 123     continue
      enddo

 10   continue

      if(foundbproc.ne.flst_nborn) then
         write(*,*)'**********************************'
         write(*,*)
     $ 'ERROR: OLP and POWHEG-BOX have different number of'//
     $  ' born subprocesses: ',foundbproc,flst_nborn
         write(*,*)'**********************************'
         call exit(1)
      endif

      close(iun)

c     After the mapping between OLP and POWHEG code has been 
c     performed call OLP_Start again to initialize the virtual 
c     routines inside the OLP
c      call OLP_Start(ios,filename//CHAR(0))
      if (ios.ne.-1) then
         write(*,*) ' Error: OLP cannot handle contract file'
         call exit(1)
      endif
      end


      subroutine virtual_OLP(p,code,virtual)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_kn.h'
      real *8 p(0:3,nlegborn)
      integer code
      real *8 virtual
      real *8 p_olp(0:4,nlegborn)
      real *8 scales(4) ! these are: mu_ren,mu_fac,Q_ES,mu_reg
      real *8 virt_wgts(4) ! these are: pole2,pole1,pole0,born
      integer mu,ileg
      real *8 s
      real *8 dotp
      external dotp

c     Since the definition of spinors in Uwer's library
c     does not allow for massless spinors parallel to z-axis
c     we apply a set of rotations x->y , y->z, z->x
      do ileg=1, nlegborn
         p_olp(0,ileg)=p(0,ileg)
         p_olp(1,ileg)=p(3,ileg)
         p_olp(2,ileg)=p(1,ileg)
         p_olp(3,ileg)=p(2,ileg)
         p_olp(4,ileg)=0d0
      enddo
c     to avoid bugs, restore exact masslessness of incoming partons 
      p_olp(0,1)=dabs(p_olp(1,1))
      p_olp(0,2)=dabs(p_olp(1,2))
c     sets the masses of heavy particles
      p_olp(4,3)=kn_masses(3)
      p_olp(4,4)=kn_masses(4)

c     assign scales
      scales(1)=sqrt(st_muren2)
      scales(2)=sqrt(st_mufact2)
      scales(3)=sqrt(st_muren2)
      scales(4)=sqrt(st_muren2)

c  uncomment the following line to link against an OLP program
c      call OLP_EvalSubProcess(code,p_olp,scales,1d0,virt_wgts)

      
      virtual=virt_wgts(3)

      end



     
