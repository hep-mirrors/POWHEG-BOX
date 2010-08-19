      program filetop_rebin_A
      implicit none      
      integer MAXMOM
      parameter( MAXMOM = 3000)
      real * 8 x1(MAXMOM),y1(MAXMOM),e1(MAXMOM)
      real * 8 x2(MAXMOM),y2(MAXMOM),e2(MAXMOM)
      real * 8 error
      character * 100 line
      integer n1,n2,i,j,k,inbin,ibin
      character * 200 file_in,file_out,file_tmp
      integer iuni,iuno
      integer MAXTAGS
      parameter(MAXTAGS=8)
      real * 8 xx(6,MAXTAGS),tot,toterr
      character * 100 tag(MAXTAGS+1)
      integer length
      external length
      data xx
c     A0           6
     #     /0d0,5d0,10d0,20d0,40d0,100d0,
c     A1           6
     #     0d0,5d0,10d0,20d0,40d0,100d0,
c     A2           6
     #     0d0,5d0,10d0,20d0,40d0,100d0,
c     A3           6
     #     0d0,5d0,10d0,20d0,40d0,100d0,
c     A4           6
     #     0d0,5d0,10d0,20d0,40d0,100d0,
c     A5           6
     #     0d0,5d0,10d0,20d0,40d0,100d0,
c     A6           6
     #     0d0,5d0,10d0,20d0,40d0,100d0,
c     A7           6
     #     0d0,5d0,10d0,20d0,40d0,1000/


      file_in = 'z-PYTHIA-alone-output.top'
      file_out = 'z-PYTHIA-alone-rebinned.top'


      file_tmp= file_in(1:length(file_in))//'.completed'
      call completetop(file_in,file_tmp)
c     

      tag(1) = ' (  A0'
      tag(2) = ' (  A1'
      tag(3) = ' (  A2'
      tag(4) = ' (  A3'
      tag(5) = ' (  A4'
      tag(6) = ' (  A5'
      tag(7) = ' (  A6'
      tag(8) = ' (  A7'


      tag(MAXTAGS+1) = 'XXXX  XXXX   XXXX'
      
      iuni=11
      iuno=12
      open(unit=iuni,file=file_tmp,status='old')
      open(unit=iuno,file=file_out,status='unknown')
 20   continue
      do k=1,MAXTAGS+1
         do i=1,100000
            read(iuni,'(a)',end=33,err=33) line
            call writestring(line,iuno)
            if(line.eq.tag(k)) then
               write(*,*) tag(k)
               read(iuni,'(a)',end=33,err=33) line
               call writestring(line,iuno)
               goto 10
            endif
         enddo
 10      continue
         n1=MAXMOM
         call gettab(iuni,x1(1),y1(1),e1(1),n1)
         tot=0
         toterr=0
         inbin=0
         j=1
         ibin=1
 11      continue
         if(j.le.n1) then
            if(x1(j).lt.xx(1,k)) then
               j=j+1
               goto 11
            endif
            if(x1(j).lt.xx(ibin+1,k)) then
               inbin=inbin+1
               tot=tot+y1(j)
               toterr=toterr+e1(j)**2
               j=j+1
               goto 11
            else
               tot=tot/inbin
               toterr=sqrt(toterr)/inbin
               write(iuno,*) (xx(ibin,k)+xx(ibin+1,k))/2,tot,toterr
               inbin=0
               tot=0
               toterr=0
               ibin=ibin+1
               if((ibin.gt.12.and.k.eq.1) .or.
     1            (ibin.gt.8.and.k.eq.2)) then
                  goto 22
               endif
               goto 11
            endif
         endif
c     if here, we have reached the end of histo
         tot=tot/inbin
         toterr=sqrt(toterr)/inbin
         write(iuno,*) (xx(ibin,k)+xx(ibin+1,k))/2,tot,toterr
 22   enddo

 33   continue
      close(iuni)
      close(iuno)
      end  




      subroutine gettab(iun,xtab,ytab,etab,nent)
      implicit none
      integer iun
      character * 100 string
c      dimension xtab(*),ytab(*),etab(*)
      real * 8 xtab(*),ytab(*),etab(*)
      integer ileft
      common/laststring/string,ileft
      integer nent,j,k,m,l,karr
      real * 8 rarr(10)
      do j=1,nent
         xtab(j)=0
         ytab(j)=0
         etab(j)=0
      enddo
      do k=1,1000000
         do m=1,nent
            read(iun,'(a)',err=888) string
            call reads(string,100,rarr,3,karr)
            if(karr.ne.3) then
               goto 888
            endif
c               l=rarr(1)*100
            l = m
            xtab(l)=rarr(1)
            ytab(l)=rarr(2)
            etab(l)=rarr(3)
         enddo
      enddo
 888  continue
      nent=m-1
      ileft=1
      end

      subroutine zeroleft
      character * 100 string
      integer ileft
      common/laststring/string,ileft
      ileft=0
      end



      subroutine writeout(string,iun)
      character *(*) string
      integer iun
      integer l
      l=len(string)
      do while(string(l:l).eq.' ')
         l=l-1
      enddo
      write(iun,'(a)') string(1:l)
      end


      subroutine writestring(line,iun)
      character * (*) line
      integer iun
      character * 100 string
      integer length
      integer ileft
      common/laststring/string,ileft
      if(ileft.eq.1) then
         ileft=0
         length=len(string)
         do while(string(length:length).eq.' ')
            length=length-1
         enddo
         write(iun,'(a)') string(1:length)
      endif
      length=len(line)
      do while(line(length:length).eq.' ')
         length=length-1
      enddo
      write(iun,'(a)') line(1:length)
      end
      

c Program to read numbers from strings
      subroutine reads(string,nstr,rarr,narr,karr)
      implicit real * 8 (a-h,o-z)
      dimension rarr(narr),isign(2)
      real * 8 num(2)
      character * (*) string
      character * 1 ch
      karr=0
c get token
      istr=1
 1    continue
c skip blanks
      if(istr.le.nstr.and.string(istr:istr).eq.' ') then
         istr=istr+1
         goto 1
      endif
      if(istr.gt.nstr) goto 999
      istart=istr
c find next blank
 2    if(istr.le.nstr.and.string(istr:istr).ne.' ') then
         istr=istr+1
         goto 2
      endif
      iend=istr-1 
      iperiod=0
c value
      num(1)=0
c exponent
      num(2)=0
      k=1
      js=istart
 10   if(string(js:js).eq.'-') then
         isign(k)=-1
         js=js+1
      elseif(string(js:js).eq.'+') then
         isign(k)=1
         js=js+1
      else
         isign(k)=1
      endif
      do j=js,iend
         ch=string(j:j)
         if(ch.le.'9'.and.ch.ge.'0') then
            num(k)=num(k)*10+ichar(ch)-ichar('0')
         elseif(ch.eq.'.') then
            if(iperiod.ne.0.or.k.eq.2)goto 998
            iperiod=j-iend
         elseif(ch.eq.'e'.or.ch.eq.'E'.or.ch.eq.'d'.or.ch.eq.'D')then
            if(j.eq.1.or.k.eq.2) goto 998
            if(iperiod.ne.0) iperiod=iperiod+iend-j+1
            k=2
            js=j+1
            goto 10
         else
            goto 999
         endif
       enddo
       karr=karr+1
       rarr(karr)=isign(1)*num(1)*(10.d0)**(isign(2)*num(2)+iperiod)
       if(karr.eq.narr) goto 999
       if(iend.lt.nstr) goto 1
       goto 999
 998   continue
       stop
 999   end

c Program to read integers from strings
      subroutine ireads(string,nstr,iarr,narr,karr)
      implicit real * 8 (a-h,o-z)
      dimension iarr(narr)
      character * (*) string
      character * 1 ch
      karr=0
c get token
      istr=1
 1    continue
c skip blanks
      if(string(istr:istr).eq.' '.and.istr.le.nstr) then
         istr=istr+1
         goto 1
      endif
      if(istr.gt.nstr) goto 999
      istart=istr
c find next blank
 2    if(string(istr:istr).ne.' '.and.istr.le.nstr) then
         istr=istr+1
         goto 2
      endif
      iend=istr-1 
c value
      num=0
      js=istart
 10   if(string(js:js).eq.'-') then
         isign=-1
         js=js+1
      elseif(string(js:js).eq.'+') then
         isign=1
         js=js+1
      else
         isign=1
      endif
      do j=js,iend
         ch=string(j:j)
         if(ch.le.'9'.and.ch.ge.'0') then
            num=num*10+ichar(ch)-ichar('0')
         else
            goto 999
         endif
      enddo
      karr=karr+1
      iarr(karr)=isign*num
      if(karr.eq.narr) goto 999
      if(iend.lt.nstr) goto 1
      goto 999
 998  continue
      stop
 999  end


c     program to read numbers from input line
      subroutine iread(iun,iarr,nel,nread)
      dimension iarr(40)
      character * 80 str
      
      
      read(iun,'(a)') str
      
      call ireads(str,80,iarr,nel,nread)
      end
      

      function length(string)
      implicit none
      integer length
      character * (*) string
      integer i,len
      do i=len(string),1,-1
         if (string(i:i).ne.' ') goto 10            
      enddo
 10   length=i
      end









