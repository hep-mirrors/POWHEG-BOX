c--- edit this file to perform different analyses
      subroutine init_hist
      
      call init_hist_KN
      
      end
      
      
      subroutine analysis(wt)
      implicit none
      real * 8 wt
      
      call analysis_KN(wt)
      
      end
      
      
