c -*- Fortran -*-

c The user must set nlegborn to the appropriate value for his process.
      integer nlegborn,nlegreal
      
      parameter (nlegborn=5)
c      integer ndiminteg
c      parameter (ndiminteg=(nlegreal-2)*3-4+2-1)
c     if there is a resonance, we need an extra number to pilot
c     the resonance mass
c      parameter (ndiminteg=(nlegreal-2)*3-4+2-1+1)

      integer ndiminteg
      parameter (ndiminteg=(nlegreal-2)*3-4+2-1)
c     if there is a resonance, we need an extra parameter to pilot
c     the resonance mass
c      parameter (ndiminteg=(nlegreal-2)*3-4+2-1+1)
