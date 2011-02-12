<TeXmacs|1.0.7>

<style|article>

<\body>
  <doc-data|<doc-title|Upper bounding function FSR>>

  The upper bounding function is

  <\equation>
    U=N<frac|\<alpha\><rsub|s>(k<rsub|T><rsup|2>)|\<xi\>(1-y)> ,
  </equation>

  with

  <\equation>
    k<rsub|T><rsup|2>=<frac|s|2>\<xi\><rsup|2> (1-y) ,
  </equation>

  <math|s> being the <math|s> invariant (same for the real and underlying
  Born). The range of <math|\<xi\>>, <math|y> and <math|\<phi\>> is

  <\equation>
    0\<less\>\<xi\>\<less\>\<xi\><rsub|max>=<frac|s-M<rsup|2><rsub|rec>|s>,
    \ \ -1\<less\>y\<less\>1, \ \ \ \ \ 0\<less\>\<phi\>\<less\>2\<pi\>.
  </equation>

  We want to generate <math|p<rsub|T>> uniformly in\ 

  <\equation>
    \<Delta\><rsup|(U)>(p<rsub|T>)=exp<left|[>-<big|int>U
    \<theta\>(k<rsub|T>-p<rsub|T>) d \<xi\> d y d \<phi\><right|]> ,
  </equation>

  We can trade <math|y> for <math|k<rsub|T><rsup|2>>, that yields

  <\eqnarray*>
    <tformat|<table|<row|<cell|-log \<Delta\><rsup|(U)>(p<rsub|T>)>|<cell|=>|<cell|2\<pi\>N<big|int><rsub|0><rsup|\<xi\><rsub|max>><frac|d
    \<xi\>|\<xi\>> <big|int><rsub|p<rsub|T><rsup|2>><rsup|\<xi\><rsup|2>s><frac|d
    k<rsub|T><rsup|2>|k<rsub|T><rsup|2>> \<alpha\><rsub|s>(k<rsub|T><rsup|2>)>>|<row|<cell|>|<cell|=>|<cell|<frac|\<pi\>N|b<rsub|0>>\<theta\>(\<xi\><rsub|max><rsup|2>-p<rsub|T><rsup|2>/s)
    <left|[>log<frac|\<xi\><rsub|max><rsup|2>s|\<Lambda\><rsup|2>>
    log<frac|log<frac|\<xi\><rsub|max><rsup|2>s|\<Lambda\><rsup|2>>|log<frac|p<rsub|T><rsup|2>|\<Lambda\><rsup|2>>>-log<frac|\<xi\><rsub|max><rsup|2>s|p<rsub|T><rsup|2>><right|]>
    .<eq-number>>>>>
  </eqnarray*>

  An alternative upper bounding function is

  <\equation*>
    U=U<rsub|0> \<alpha\><rsub|s>(k<rsub|T><rsup|2>)\<xi\>,
  </equation*>

  <\equation>
    U<rsub|0>=N<frac|1|\<xi\><rsup|2>(1-y)(1-\<xi\>(1-y)/2)<rsup|2>> \ ,
  </equation>

  which accounts for the Jacobian singularity in the phase space mapping of
  final state radiation. We find immediately

  <\equation*>
    -log \<Delta\><rsup|(U<rsub|0>)>(p<rsub|T>)=2\<pi\>N<big|int><rsub|p<rsub|T><rsup|2>><rsup|s\<xi\><rsub|max><rsup|2>><frac|d
    k<rsub|T><rsup|2>|k<rsub|T><rsup|2>><big|int><rsub|<sqrt|k<rsub|T>/s>><rsup|\<xi\><rsub|max>>d
    \<xi\> <frac|1|(\<xi\>-k<rsub|T><rsup|2>/s)<rsup|2>>
  </equation*>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-type|# Simply write the paper name. See papersize(5) for
    possible values>
  </collection>
</initial>