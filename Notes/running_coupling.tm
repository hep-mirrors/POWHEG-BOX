<TeXmacs|1.0.7>

<style|generic>

<\body>
  We need an upper bounding function to the running coupling, that has the
  form of a leading log running coupling. We observe that for
  <math|n<rsub|f>\<less\>8> we have <math|b<rsub|1>\<gtr\>0>, where

  <\equation>
    <frac|d \<alpha\><rsub|s>|d log \<mu\><rsup|2>>=-b<rsub|0><rsup|n<rsub|f>>
    \<alpha\><rsub|s><rsup|2>-b<rsub|1><rsup|n<rsub|f>>
    \<alpha\><rsub|s><rsup|3> \<ldots\> .
  </equation>

  Furthermore, both <math|b<rsub|0>> and <math|b<rsub|1>> decrease as
  <math|n<rsub|f>> increases. Let us now consider the two loop running
  coupling, with appropriate threshold matching,
  <math|\<alpha\><rsub|s>(\<mu\>)>. We define <math|\<Lambda\><rsub|L>> such
  that

  <\equation>
    <frac|1|b<rsub|0><rsup|5> log<frac|\<mu\><rsub|0><rsup|2>|\<Lambda\><rsub|L><rsup|2>>>=<with|mode|text|<math|\<alpha\><rsub|s>(\<mu\><rsub|0>)>>,
  </equation>

  We can now convince ourselves that

  <\equation>
    <frac|1|b<rsub|0><rsup|5> log<frac|\<mu\><rsup|2>|\<Lambda\><rsub|L><rsup|2>>>\<gtr\><with|mode|text|<math|\<alpha\><rsub|s>(\<mu\>)>>
    \ \ for \<mu\>\<gtr\>\<mu\><rsub|0>.
  </equation>

  In fact the derivative of the left hand side is always smaller than the
  derivative of the right hand side, both because it uses the largest
  <math|n<rsub|f>> (and <math|b<rsub|0><rsup|n<rsub|f>>,
  b<rsub|1><rsup|n<rsub|f>>> decrease with <math|n<rsub|f>>), and because it
  neglects <math|b<rsub|1>> (that is positive).

  We define <math|\<mu\><rsub|0>=2\<Lambda\><rsub|<wide|MS|\<bar\>>><rsup|5>>
  (<math|\<Lambda\><rsub|<wide|MS|\<bar\>>><rsup|5>> being the 5-flavour, 2
  loop <math|\<Lambda\>>). The quantity

  <\equation>
    <frac|1|b<rsub|0><rsup|5> log<frac|\<mu\><rsup|2>|\<Lambda\><rsub|L><rsup|2>>>/<with|mode|text|<math|\<alpha\><rsub|s>(\<mu\>)>>,<label|eq:alpharatio>
  </equation>

  is plotted in fig. <reference|fig:alpharatio>.

  <big-figure|<postscript|file:///home/nason/Pheno/smc+nlo-vv/POWHEG/FKSGeneral/notes/alpharatio.eps|*3/4|*3/4||||>|<label|fig:alpharatio>
  The quantity of eq. (<reference|eq:alpharatio>)>

  \ 

  \ 
</body>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|eq:alpharatio|<tuple|4|?>>
    <associate|fig:alpharatio|<tuple|1|?>>
  </collection>
</references>