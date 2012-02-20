c Better rebin program;
c rebins histograms (also with variable bins) into new (variable) binning.
c edges must match (of course!)
      implicit none
      character * 100 file,newfile,tag
      character * 200 line
      integer i,j,k,l,ltag
      real * 8 xl,xh,y,ey,xxl,xxh,yyy,eee
      integer nbins,jbin,maxbins
      parameter (maxbins=200)
      real * 8 ptbins(maxbins),norm
      logical touched,lepts,eqpts
c      data ptbins/
c     1      0d0, 10d0, 20d0, 30d0,
c     1     40d0,
c     1     50d0,60d0,70d0,80d0,100d0,
c     1    150d0,200d0,250d0,300d0,400d0/
c      data ptbins/
c     1     0d0, 0.210d0,20d0,30d0,50d0,
c     1     70d0,90d0,110d0,150d0,200d0,
c     1     250d0,300d0,400d0,500d0,600d0,
c     1     700d0,800d0/
      integer lastnonblank
      write(*,*)'enter tag to rebin'

      read(*,'(a)') tag
      tag=trim(tag)
      ltag=lastnonblank(tag)
      write(*,*) 'tag:'//tag(1:ltag)//':'
      write(*,*)'enter bin edges, one per line,'
     1     //' repeat last edge to terminate'
      do j=1,maxbins+1
         read(*,*) ptbins(j)
         if(j.gt.1.and.ptbins(j).eq.ptbins(j-1)) then
            nbins=j-1
            exit
         elseif(j.gt.1.and.ptbins(j).lt.ptbins(j-1)) then
            write(*,*) ' bins should be increasing in value!'
            call exit(-1)
         endif
      enddo
      write(*,*) ' enter filename'
      read(*,'(a)') file
      open(unit=10,file=file,status='old')
      i=index(file,'.top')
      newfile=file(1:i-1)//'_rebinned.top'
      open(unit=11,file=newfile,status='unknown')
      do j=1,1000000
         read(10,'(a)',end=111) line
         if(index(line,tag(1:ltag)).gt.0) then
            write(11,'(a)') line(1:lastnonblank(line))
            jbin=1
            do k=1,1000000
c Use iostat!
               read(10,'(a)',end=100) line
               if(line.eq.' ') then
                  write(11,'(a)')
                  if(jbin.eq.nbins) then
                     exit
                  else
                     write(*,*) ' new binnings extends further!'
                     exit
                  endif
               endif
               if(jbin.ge.nbins) then
                  write(*,*) ' new binning has smaller extension!'
                  do l=1,1000000
c use iostat!
                     read(10,'(a)',end=111) line
                     if(line.eq.' ') then
                        write(11,*)
                        exit
                     endif
                  enddo
               endif
                     
               read(line,*) xl,xh,y,ey
               if(.not.eqpts(ptbins(jbin),xl)) then
                  write(*,*) ' new binning does not match old one'
                  call exit(-1)
               else
                  xxl=xl
                  yyy=0
                  eee=0
                  do l=1,1000000
                     if(lepts(xh,ptbins(jbin+1))) then
                        yyy = yyy + y*(xh-xl)
                        eee = eee + (ey*(xh-xl))**2
                     endif
                     if(eqpts(ptbins(jbin+1),xh)) then
                        xxh = xh
                        norm=xxh-xxl
                        write(11,'(4(1x,d14.8))')
     1                       xxl,xxh,yyy/norm,sqrt(eee/norm**2)
                        jbin=jbin+1
                        exit
                     elseif(xh.gt.ptbins(jbin+1)) then
                        write(*,*)
     1                       ' error: histogram cannot be rebinned'
                        call exit(-1)
                     endif
                     read(10,'(a)',end=100) line
                     if(line.eq.' ') then
                        write(*,*)
     1                       ' data set ended before filling all bins'
                        exit
                     endif
                     read(line,*) xl,xh,y,ey
                  enddo
               endif
            enddo
         else
            write(11,'(a)') line(1:lastnonblank(line))
         endif
      enddo
 111  continue
 100  close(11)
      end

      function eqpts(x1,x2)
      logical eqpts
      real * 8 x1,x2
      if(abs(x1-x2).lt.1d-8) then
         eqpts=.true.
      else
         eqpts=.false.
      endif
      end

      function lepts(x1,x2)
      logical lepts
      real * 8 x1,x2
      if(x1.lt.x2+1d-8) then
         lepts=.true.
      else
         lepts=.false.
      endif
      end

      function lastnonblank(line)
      integer lastnonblank
      character *(*) line
      l=len(line)
      do j=l,1,-1
         if(line(j:j).ne.' ') then
            lastnonblank=j
            return
         endif
      enddo
      lastnonblank=0
      end
