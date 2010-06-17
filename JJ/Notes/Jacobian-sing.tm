<TeXmacs|1.0.7.3>

<style|generic>

<\body>
  <doc-data|<doc-title|Jacobian singularity in the jet cross section>>

  Consider the final state singularity associated with particles 1 and 2,
  particle 3 recoiling against it. In normal FKS variables, the Jacobian is
  proportional to

  <\equation>
    J\<approx\><frac|1-\<xi\>|<left|(>1-<frac|1|2>\<xi\>(1-y)<right|)><rsup|2>>
    ,
  </equation>

  which is singular when both <math|\<xi\>\<rightarrow\>1> and
  <math|y\<rightarrow\>-1>. The singularity is an integrable one. However,
  the normal separation of singularity regions leaves it exposed, as we
  briefly show.

  The <math|S> function to select this region has the form

  <\equation>
    S<rsub|12>=<frac|<frac|1|d<rsub|12>>|<frac|1|d<rsub|12>>+<frac|1|d<rsub|23>>+<frac|1|d<rsub|13>>>,
  </equation>

  where

  <\equation>
    d<rsub|12>=<left|(>k<rsub|1>\<cdot\>k<rsub|2><frac|E<rsub|1>E<rsub|2>|(E<rsub|1>+E<rsub|2>)<rsup|2>><right|)><rsup|p>\<approx\><left|(>2
    (1-\<xi\>)<rsup|2><right|)><rsup|p>, \ \ \ \ \ \ \ \ d<rsub|13>\<approx\>2<rsup|p>,
    \ \ \ \ \ \ d<rsub|23>\<approx\><left|(>(1+y)(1-\<xi\>)<rsup|2><right|)><rsup|p>,
  </equation>

  where we are focussing upon the <math|\<xi\>\<rightarrow\>1>,
  <math|y\<rightarrow\>-1> region (and where we normally take <math|p=1>).
  Thus

  <\equation>
    S<rsub|12>\<approx\><frac|<frac|1|(1-\<xi\>)<rsup|2>>|<frac|1|(1+y)(1-\<xi\>)<rsup|2>>+<frac|1|(1-\<xi\>)<rsup|2>>+2>\<approx\>1+y.
  </equation>

  On the other hand, in this region the amplitude has the singularity
  structure

  <\equation>
    R\<approx\><frac|1|(1+y)(1-\<xi\>)<rsup|2>>.
  </equation>

  Thus, the overall behaviour is

  <\equation>
    R\<times\>S<rsub|12>\<times\>J\<approx\><frac|1|(1+y)(1-\<xi\>)<rsup|2>>(1+y)<frac|1-\<xi\>|<left|(>1-<frac|1|2>\<xi\>(1-y)<right|)><rsup|2>>\<approx\><frac|1|(1-\<xi\>)>
    <frac|1|<left|(>1-<frac|1|2>\<xi\>(1-y)<right|)><rsup|2>> .
  </equation>

  This is not yet the whole story. If particle 2 is a gluon, 1 must also be a
  gluon, and an extra factor <math|E<rsub|2>/(E<rsub|1>+E<rsub|2>)> is
  supplied, which amount to another <math|1-\<xi\>> factor. We are left,
  however, with\ 

  <\equation>
    <frac|1|<left|(>1-<frac|1|2>\<xi\>(1-y)<right|)><rsup|2>>,
  </equation>

  which is not integrable. If we use instead <math|p=2>, we get an extra
  factor of <math|1+y>, and the jacobian singularity becomes again integrable
  with adequate importance sampling. To examine the singularity structure, we
  write for small <math|\<rho\>>

  <\equation>
    \<xi\>=1-\<rho\> cos\<theta\>, \ \ \ \ \ \ \ \ y=-1+2\<rho\>
    sin\<theta\>, \ \ \ \ \ \ \ \ 0\<less\>\<theta\>\<less\>\<pi\>.
  </equation>

  so that

  <\equation>
    <frac|1|<left|(>1-<frac|1|2>\<xi\>(1-y)<right|)><rsup|2>>=<frac|1|\<rho\><rsup|2><left|(>cos\<theta\>+sin\<theta\>)<rsup|2>>
    .
  </equation>

  This seems to suggest that we must use <math|p=2> with square root
  importance sampling for both <math|\<xi\>\<rightarrow\>1> and
  <math|y\<rightarrow\>-1>. Or, we can use higher <math|p> values with no
  need of any importance sampling. Or, we could also allow for
  <math|E<rsub|2><rsup|p>/(E<rsub|1><rsup|p>+E<rsub|2><rsup|2>)> damping with
  <math|p\<gtr\>1> in the two gluon case.

  In case 2 is a quark, it is not clear what kind of singularity the soft
  quark can give\ 
</body>

<\initial>
  <\collection>
    <associate|sfactor|5>
  </collection>
</initial>