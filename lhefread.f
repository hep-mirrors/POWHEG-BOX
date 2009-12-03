c...lhefheader(nlf)
c...reads initialization information from a les houches events file on unit nlf. 
      subroutine lhefreadhdr(nlf)
      implicit none
      integer nlf
      character * 100 string
      integer ipr
      include 'include/LesHouches.h'
 1    read(nlf,fmt='(a)',err=998,end=998) string
      if(string(1:5).eq.'<init') then
         read(nlf,*) idbmup(1),idbmup(2),ebmup(1),ebmup(2),
     &        pdfgup(1),pdfgup(2),pdfsup(1),pdfsup(2),idwtup,nprup
         do ipr=1,nprup
            read(nlf,*) xsecup(ipr),xerrup(ipr),xmaxup(ipr),
     &           lprup(ipr)
         enddo
         goto 999
      else
         goto 1
      endif
 998  write(*,*) 'lhefreadhdr: could not find <init> data'
      call exit(1)
 999  end


c...reads event information from a les houches events file on unit nlf. 
      subroutine lhefreadev(nlf)
      implicit none
      integer nlf
      character * 100 string
      include 'include/LesHouches.h'
      integer i,j
 1    read(nlf,fmt='(a)',err=998,end=998) string
      if(string.eq.'</LesHouchesEvents>') then
         goto 998
      endif
      if(string(1:6).eq.'<event') then
         read(nlf,*) nup,idprup,xwgtup,scalup,aqedup,aqcdup
         do i=1,nup
            read(nlf,*) idup(i),istup(i),mothup(1,i),
     &           mothup(2,i),icolup(1,i),icolup(2,i),(pup(j,i),j=1,5),
     &           vtimup(i),spinup(i)
         enddo
         goto 999
      else
         goto 1
      endif
c no event found:
 998  nup=0      
 999  end


      subroutine lhefreadextra(nlf)
      implicit none
      include 'include/LesHouches.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_st.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_flg.h'
      character * 100 string
      integer nlf
 1    continue
      read(unit=nlf,fmt='(a)',end=998) string
      if(string.eq.'<event>') then
         backspace nlf
         return
      endif
      read(nlf,'(a)') string
      if(string(2:28).eq.'<extra-info-previous-event>') then
         read(nlf,*) rad_kinreg
c         read(nlf,*) rad_type
         return
      else
         goto 1
      endif
      return
 998  continue
      write(*,*) ' end of event file'
      call exit(1)
      end
