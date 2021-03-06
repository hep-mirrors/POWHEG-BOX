      function btilde(xx,www0,ifirst)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_rad.h'
      include 'pwhg_flg.h'
      include 'pwhg_math.h'
c     independent variables for real graph: number of final state
c     legs times 3, take away 4 for 4-momentum conservation, add 2
c     for x_1 and x_2, and take away an overall azimuth
      real * 8 btilde,xx(ndiminteg),www0
      real * 8 xrad(3)
      real * 8 xborn(ndiminteg-3)
      integer ifirst
      real * 8 resborn(maxprocborn),resvirt(maxprocborn),
     #     resreal(maxprocborn),rescoll(maxprocborn)
      real * 8 results(maxprocborn)
      real * 8 tmp,suppfact,www,wwwtot
      integer j
      save resborn,resvirt,wwwtot
      real *8 totborn,totvirt
c     begin WZGRAD EDIT-------------
      real*8 powheginput
      external powheginput
      logical flg_inbtilde,flg_inequiv
      common/pwhg_flg_EW/flg_inbtilde,flg_inequiv
      logical flg_saverand
      common/pwhg_flg_saverand/flg_saverand
c     end WZGRAD EDIT-------------

      flg_inbtilde=.true. !WZGRAD EDIT
      flg_saverand=.true. !WZGRAD EDIT

      www=www0*hc2
      do j=1,ndiminteg-3
         xborn(j)=xx(j)
      enddo
      do j=1,3
         xrad(j)=xx(ndiminteg-3 + j)
      enddo
      if(ifirst.eq.0) then
         wwwtot=www
c     sets born momenta in kin. common block
         call gen_born_phsp(xborn)
         call born_suppression(suppfact)
c set scales
         call setscalesbtilde
         call allborn
c     sets xscaled, y, phi in kinematics common block
         call btildeborn(resborn)

         if (.not.flg_bornonly) then
            call btildevirt(resvirt)
            call btildecoll(xrad,rescoll,www)
            call btildereal(xrad,resreal,www)
         endif
c     accumulate values
         btilde=0
         do j=1,flst_nborn
c     jacobians are already included in rescoll and resreal
            tmp=resborn(j)
            if (.not.flg_bornonly) then
               tmp = tmp   
     #              + resvirt(j)
     #              + rescoll(j) 
     #              + resreal(j)
            endif
c     initial value in results
            results(j)=tmp*www*suppfact
            btilde=btilde+tmp*www*suppfact
c         print*, 'j = ', j
c         print*, 'resborn(j) = ', resborn(j)
c         print*, 'resvirt(j) = ', resvirt(j)
c         print*, 'rescoll(j) = ', rescoll(j)
c         print*, 'resreal(j) = ', resreal(j)
         enddo
c      print*, 'stopping line 65 of btilde.f'
c      stop
      elseif(ifirst.eq.1) then
c     subsequent calls:
c     In case of folding the call to btildeborn and btildevirt can be
c     avoided, since results are the same.
c     If the NLO calculation is performed also (flg_nlotest is set)
c     we need to accumulate all weight within a single folding sequence
c     in order to later output the correct Born and Virtual contribution
c     to the NLO analysis routine.
         wwwtot=wwwtot+www
         if (.not.flg_bornonly) then
c btildecoll and btildereal take care themselves to invoke the NLO
c analysis if required.
            call btildecoll(xrad,rescoll,www)
            call btildereal(xrad,resreal,www)
         endif
         btilde=0
         do j=1,flst_nborn
            tmp=resborn(j)
            if (.not.flg_bornonly) then
               tmp = tmp   
     #              + resvirt(j)
     #              + rescoll(j) 
     #              + resreal(j)
            endif
c     accumulate values in results
            results(j)=results(j)+tmp*www*suppfact
            btilde=btilde+tmp*www*suppfact
         enddo
      elseif(ifirst.eq.2) then
         if(flg_nlotest) then
c        output Born
         totborn=0d0
            do j=1,flst_nborn
            totborn=totborn+resborn(j)
            enddo
         totborn=totborn*wwwtot
         call analysis_driver(totborn,0)
            if(.not.flg_bornonly) then
c           output virtual
            totvirt=0d0
               do j=1,flst_nborn
               totvirt=totvirt+resvirt(j)
               enddo
            totvirt=totvirt*wwwtot
            call analysis_driver(totvirt,0)
            endif
c        closing call to end a sequence of correlated events in the
c        analysis routines.
         call pwhgaccumup
         endif
c     closing call: accumulate values with correct signs
      btilde=0
      call adduptotals(results,flst_nborn)
         do j=1,flst_nborn
c        this is only useful if withnegweights on (i.e. =1 in powheg.input,
c        logical true here). However, better set a default (Les Houches
c        interface will simply output this sign for the event.)
         rad_btilde_sign(j)=1
            if(flg_withnegweights) then
               if(results(j).lt.0) then
               results(j)=-results(j)
               rad_btilde_sign(j)=-1
               endif                  
            else
               if(results(j).lt.0) then
               results(j)=0
               endif
            endif
         btilde=btilde+results(j)
c        Transfer all flavour components of btilde to the array
c        in common block; will be used to decide the underlying
c        flavour of the event
         rad_btilde_arr(j)=results(j)
         enddo
      else
         write(*,*) 'wrong value of ifirst in btilde => ',ifirst
         stop
      endif
      flg_inbtilde=.false. !WZGRAD EDIT
      end
      

      subroutine adduptotals(results,n)
      implicit none
      include 'nlegborn.h'
      integer n
      real * 8 results(n)
      real * 8 tot,totabs,totpos,totneg,etot,etotabs,etotpos,etotneg
      real * 8 totj(maxprocborn),totabsj(maxprocborn),
     1     totposj(maxprocborn),totnegj(maxprocborn),
     2     etotj(maxprocborn),etotabsj(maxprocborn),
     3     etotposj(maxprocborn),etotnegj(maxprocborn)
      integer nentries
      common/cadduptotals/tot,totabs,totpos,totneg,etot,etotabs,
     1     etotpos,etotneg,totj,totabsj,totposj,totnegj,
     1         etotj,etotabsj,etotposj,etotnegj,nentries
      real * 8 dtot,dtotabs,dtotpos,dtotneg
      integer j
      nentries=nentries+1
      dtot=0
      dtotabs=0
      dtotpos=0
      dtotneg=0
      do j=1,n
         dtot=dtot+results(j)
         dtotabs=dtotabs+abs(results(j))
         if(results(j).gt.0) then
            dtotpos=dtotpos+results(j)
         else
            dtotneg=dtotneg-results(j)
         endif
      enddo
      tot=tot+dtot
      totabs=totabs+dtotabs
      totpos=totpos+dtotpos
      totneg=totneg+dtotneg      
      etot=etot+dtot**2
      etotabs=etotabs+dtotabs**2
      etotpos=etotpos+dtotpos**2
      etotneg=etotneg+dtotneg**2
c j contributions
      do j=1,n
         dtot=results(j)
         dtotabs=abs(results(j))
         if(results(j).gt.0) then
            dtotpos=results(j)
         else
            dtotneg=-results(j)
         endif
         totj(j)=totj(j)+dtot
         totabsj(j)=totabsj(j)+dtotabs
         totposj(j)=totposj(j)+dtotpos
         totnegj(j)=totnegj(j)+dtotneg     
         etotj(j)=etotj(j)+dtot**2
         etotabsj(j)=etotabsj(j)+dtotabs**2
         etotposj(j)=etotposj(j)+dtotpos**2
         etotnegj(j)=etotnegj(j)+dtotneg**2
      enddo
      end

      subroutine resettotals
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      real * 8 tot,totabs,totpos,totneg,etot,etotabs,etotpos,etotneg
      real * 8 totj(maxprocborn),totabsj(maxprocborn),
     1     totposj(maxprocborn),totnegj(maxprocborn),
     2     etotj(maxprocborn),etotabsj(maxprocborn),
     3     etotposj(maxprocborn),etotnegj(maxprocborn)
      integer nentries
      common/cadduptotals/tot,totabs,totpos,totneg,etot,etotabs,
     1     etotpos,etotneg,totj,totabsj,totposj,totnegj,
     1         etotj,etotabsj,etotposj,etotnegj,nentries
      integer j
      nentries=0
      tot=0
      etot=0
      totabs=0
      etotabs=0
      totpos=0
      etotpos=0
      totneg=0
      etotneg=0
      do j=1,flst_nborn
         totj(j)=0
         etotj(j)=0
         totabsj(j)=0
         etotabsj(j)=0
         totposj(j)=0
         etotposj(j)=0
         totnegj(j)=0
         etotnegj(j)=0
      enddo
      end

      subroutine finaltotals
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_rad.h'
      real * 8 tot,totabs,totpos,totneg,etot,etotabs,etotpos,etotneg
      real * 8 totj(maxprocborn),totabsj(maxprocborn),
     1     totposj(maxprocborn),totnegj(maxprocborn),
     2     etotj(maxprocborn),etotabsj(maxprocborn),
     3     etotposj(maxprocborn),etotnegj(maxprocborn)
      integer nentries
      common/cadduptotals/tot,totabs,totpos,totneg,etot,etotabs,
     1     etotpos,etotneg,totj,totabsj,totposj,totnegj,
     1         etotj,etotabsj,etotposj,etotnegj,nentries
      integer n,j,k
      character * 40 format
      real * 8 tmp_totbtlj,tmp_etotbtlj,tmp_totabsbtlj,
     1     tmp_etotabsbtlj,tmp_totposbtlj,tmp_etotposbtlj,
     2     tmp_totnegbtlj,tmp_etotnegbtlj
      real * 8 tmp
      integer iun
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      real * 8 powheginput
      external powheginput
      n=nentries
      rad_totbtl=tot/n
      rad_etotbtl=sqrt((etot/n-(tot/n)**2)/n)
      rad_totabsbtl=totabs/n
      rad_etotabsbtl=sqrt((etotabs/n-(totabs/n)**2)/n)
      rad_totposbtl=totpos/n
      rad_etotposbtl=sqrt((etotpos/n-(totpos/n)**2)/n)
      rad_totnegbtl=totneg/n
      rad_etotnegbtl=sqrt((etotneg/n-(totneg/n)**2)/n)
      write(*,*) 'tot:',rad_totbtl,'+-',rad_etotbtl
      write(*,*) 'abs:',rad_totabsbtl,'+-',rad_etotabsbtl
      write(*,*) 'pos:',rad_totposbtl,'+-',rad_etotposbtl
      write(*,*) 'neg:',rad_totnegbtl,'+-',rad_etotnegbtl
c
      if(powheginput('#ubsigmadetails').eq.1) then
         call newunit(iun)
         open(iun,file=pwgprefix(1:lprefix)//'ubsigma.dat')
         format='(      (i2,1x),4(a,1x,d10.4,a,d7.1))'
         write(format(2:4),'(i3)')nlegborn
         tmp=0
         do j=1,flst_nborn
            tmp_totbtlj=totj(j)/n
            tmp_etotbtlj=sqrt((etotj(j)/n-(totj(j)/n)**2)/n)
            tmp_totabsbtlj=totabsj(j)/n
            tmp_etotabsbtlj=sqrt((etotabsj(j)/n-(totabsj(j)/n)**2)/n)
            tmp_totposbtlj=totpos/n
            tmp_etotposbtlj=sqrt((etotposj(j)/n-(totposj(j)/n)**2)/n)
            tmp_totnegbtlj=totneg/n
            tmp_etotnegbtlj=sqrt((etotnegj(j)/n-(totnegj(j)/n)**2)/n)
            write(iun,format) (flst_born(k,j),k=1,nlegborn),
     1           'tot:',tmp_totbtlj,' +- ',tmp_etotbtlj,
     2           '; abs:',tmp_totabsbtlj,' +- ',tmp_etotabsbtlj,
     3           '; pos:',tmp_totposbtlj,' +- ',tmp_etotposbtlj,
     4           '; neg:',tmp_totnegbtlj,' +- ',tmp_etotnegbtlj
            tmp=tmp+tmp_totbtlj
         enddo
         write(iun,*) tmp
      endif
      end

