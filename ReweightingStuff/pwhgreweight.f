      subroutine pwhgnewweight
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_rad.h'
      include 'pwhg_flg.h'
      logical ini
      data ini/.true./
      save ini
      integer maxev
      if(ini) then
         call opencount(maxev)
         call openoutputrw
         ini=.false.
      endif
      call lhefreadevnew(97,99)
      end


      subroutine openoutputrw
      implicit none
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      include 'pwhg_rnd.h'
      if(rnd_cwhichseed.ne.'none') then
         open(unit=99,file=pwgprefix(1:lprefix)//'eventsww-'
     1        //rnd_cwhichseed//'.lhe'
     2     ,status='unknown')
      else
         open(unit=99,file=pwgprefix(1:lprefix)//'eventsww.lhe'
     1     ,status='unknown')
      endif
      close(99)
      end

c...reads event information from a les houches events file on unit nlf. 
      subroutine lhefreadevnew(nlf,nuo)
      implicit none
      integer nlf,nuo
      character * 100 string
      include 'LesHouches.h'
      integer i,j,lenocc
      external lenocc
 1    continue
      string=' '
      read(nlf,fmt='(a)',err=777,end=666) string
      write(nuo,'(a)') string(1:lenocc(string))
      if(string.eq.'</LesHouchesEvents>') then
         goto 998
      endif
      if(string(1:6).eq.'<event') then
c on error try next event. The error may be cause by merging
c truncated event files. On EOF return with no event found
         read(nlf,*,end=998,err=1)nup,idprup,xwgtup,scalup,aqedup,aqcdup
         do i=1,nup
            read(nlf,*,end=998,err=1) idup(i),istup(i),mothup(1,i),
     &           mothup(2,i),icolup(1,i),icolup(2,i),(pup(j,i),j=1,5),
     &           vtimup(i),spinup(i)
         enddo
         call lhefreadextra(nlf)
         goto 999
      else
         goto 1
      endif
c no event found:
 777  continue
      print *,"Error in reading"
      print *,string
      stop
 666  continue
      print *,"reached EOF"
      print *,string
      stop
 998  continue
      print *,"read </LesHouchesEvents>"
      nup=0      
 999  end


      subroutine lhefreadextra(nlf)
      implicit none
      include 'LesHouches.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_rad.h'
      include 'pwhg_st.h'
      include 'pwhg_kn.h'
      include 'pwhg_flg.h'
      character * 100 string
      integer nlf
      logical readrw
 1    continue
      read(unit=nlf,fmt='(a)',end=998) string
      if(string.eq.'<event>') then
         backspace nlf
         return
      endif
      if(string.eq.'# Start extra-info-previous-event') then
         read(nlf,'(a)') string
         read(string(3:),*) rad_kinreg
         read(nlf,'(a)') string
         read(string(3:),*) rad_type
      endif
      readrw = .false.
      if(flg_newweight) then
c read a string; if it starts with #rwg, read first rad_type from the
c string, then all other information, depending upon rad_type.
c set readrw to true  
         if(string(2:5).eq.'#rwg') then
c     do things
            print*, 'FOUND'
c     if all went ok, set readrw to true
            readrw=.true.
            read(unit=nlf,fmt='(a)',end=998) string
         endif
         if(.not.readrw) then
            write(*,*) 
     $ 'Error in lhefreadextra, while reading rwg informations'
            write(*,*)'Abort run'
            call exit(-1)
         endif
      endif
      goto 1
 998  continue
      end
