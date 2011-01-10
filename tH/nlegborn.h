c -*- Fortran -*-

c The user must set nlegborn to the appropriate value for his process.
      integer nlegborn,nlegreal
      
      parameter (nlegborn=4)
      parameter (nlegreal=nlegborn+1)

c     ndiminteg is the dimensionality of the full real integral
c     ndiminteg=(nlegreal-2)*3-4+2-1
c     if there are undecayed resonances, we need extra variables to pilot
c     the resonance's masses

      integer ndiminteg
c      parameter (ndiminteg=(nlegreal-2)*3-4+2-1
C     1    + 1 )  ! one resonance
      parameter (ndiminteg=(nlegreal-2)*3-4+2-1 )

      integer maxprocborn,maxprocreal
      parameter (maxprocborn=200,maxprocreal=500)