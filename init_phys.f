      subroutine init_phys
      implicit none
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_st.h'
      include 'include/pwhg_rad.h'
      character * 5 scheme
      character * 3 whichpdfpk
      real * 8 powheginput
      integer iorder,iret,iun
      external whichpdfpk,powheginput
      logical debug
      parameter (debug=.false.)
      pdf_ih1=powheginput('ih1')
      pdf_ih2=powheginput('ih2')
      if(whichpdfpk().eq.'lha') then
         pdf_ndns1=powheginput('lhans1')
         pdf_ndns2=powheginput('lhans2')
      elseif(whichpdfpk().eq.'mlm') then
         pdf_ndns1=powheginput('ndns1')
         pdf_ndns2=powheginput('ndns2')
      else
         write(*,*) ' unimplemented pdf package',whichpdfpk()
         stop
      endif
      if(pdf_ndns1.ne.pdf_ndns2) then
         st_lambda5MSB=powheginput('QCDLambda5')
      else
         call genericpdfpar(pdf_ndns1,pdf_ih1,st_lambda5MSB,
     1                      scheme,iorder,iret)
         if(iret.ne.0) then
            write(*,*) ' faulty pdf number ',pdf_ndns1
            stop
         endif
      endif
      kn_beams(0,1)=powheginput('ebeam1')
      kn_beams(0,2)=powheginput('ebeam2')
      kn_beams(1,1)=0
      kn_beams(1,2)=0
      kn_beams(2,1)=0
      kn_beams(2,2)=0
      kn_beams(3,1)=kn_beams(0,1)
      kn_beams(3,2)=-kn_beams(0,2)
      kn_sbeams=4*kn_beams(0,1)*kn_beams(0,2)

c generation cut: see Gen_born_phsp.f
      kn_ktmin=powheginput("#bornktmin")
      if(kn_ktmin.lt.0) kn_ktmin=0

c thresholds 
      rad_ptsqmin=powheginput('#ptsqmin')
      if(rad_ptsqmin.lt.0) rad_ptsqmin=0.8d0
      rad_charmthr2=powheginput('#charmthr')
      if(rad_charmthr2.lt.0) rad_charmthr2=1.5d0
      rad_charmthr2=rad_charmthr2**2
      rad_bottomthr2=powheginput('#bottomthr')
      if(rad_bottomthr2.lt.0) rad_bottomthr2=5d0
      rad_bottomthr2=rad_bottomthr2**2
c scale factors
      st_renfact=powheginput('#renscfact')
      st_facfact=powheginput('#facscfact')
      if(st_facfact.lt.0) st_facfact=1
      if(st_renfact.lt.0) st_renfact=1
c
c initialize Lambda values for radiation
      call init_rad_lambda
c
      call init_processes

      call init_couplings

c initialize number of singular regions
      rad_nkinreg=1+(nlegborn-flst_lightpart+1)
      call genflavreglist


c todo: debug should be in pwhg_dbg.h
c check soft and collinear limits
      if (debug) then
         call newunit(iun)
         open(unit=iun,file='pwhg_checklimits')
         call checklims(iun)
      endif

      end

