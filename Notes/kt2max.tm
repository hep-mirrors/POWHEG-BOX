<TeXmacs|1.0.7>

<style|generic>

<\body>
  <doc-data|<doc-title|Maximum <math|k<rsub|T>> in initial state radiation>>

  Consider the underlying Born kinematics <math|<wide|x|\<bar\>><rsub|1>>,
  <math|<wide|x|\<bar\>><rsub|2>>, <math|s<rsub|b>>, where <math|s<rsub|b>>
  is the <math|s> invariant of the underlying Born. We work in the underlying
  Born rest frame, where

  <\equation>
    P<rsub|1><wide|x|\<bar\>><rsub|1>=P<rsub|2><wide|x|\<bar\>><rsub|2>,
    \ \ \ s<rsub|b>=(P<rsub|1><wide|x|\<bar\>><rsub|1>+P<rsub|2><wide|x|\<bar\>><rsub|2>)<rsup|2>=4
    P<rsub|1><wide|x|\<bar\>><rsub|1> P<rsub|2><wide|x|\<bar\>><rsub|2> .
  </equation>

  In the ISR configuration, the final state system with squared mass
  <math|s<rsub|b>> acquires a transverse momentum <math|k<rsub|T>>, and a
  massless parton is radiated with transverse momentum
  <math|k<rsub|T><rsub|>>. Energy momentum balance requires that

  <\eqnarray*>
    <tformat|<table|<row|<cell|<sqrt|s<rsub|b>+k<rsub|T><rsup|2>>+<sqrt|k<rsub|L><rsup|2>+k<rsub|T><rsup|2>>>|<cell|=>|<cell|x<rsub|1>P<rsub|1>+x<rsub|2>P<rsub|2>,<eq-number>>>|<row|<cell|k<rsub|L>>|<cell|=>|<cell|x<rsub|1>P<rsub|1>-x<rsub|2>P<rsub|2>
    .<eq-number><label|eq:kldef>>>>>
  </eqnarray*>

  We solve these conditions as follows:

  <\equation>
    <sqrt|k<rsub|L><rsup|2>+k<rsub|T><rsup|2>>=x<rsub|1>P<rsub|1>+x<rsub|2>P<rsub|2>-<sqrt|s<rsub|b>+k<rsub|T><rsup|2>>,
  </equation>

  so we must require

  <\equation>
    x<rsub|1>P<rsub|1>+x<rsub|2>P<rsub|2>-<sqrt|s<rsub|b>+k<rsub|T><rsup|2>>\<gtr\>0,<label|eq:constraint>
  </equation>

  then we can square to get (setting <math|m<rsub|T>=><with|mode|math|<sqrt|s<rsub|b>+k<rsub|T><rsup|2>>>)

  <\equation>
    k<rsub|L><rsup|2>+k<rsub|T><rsup|2>=<left|(>x<rsub|1>P<rsub|1>+x<rsub|2>P<rsub|2><right|)><rsup|2>+s<rsub|b>+k<rsub|T><rsup|2>-2m<rsub|T><left|(>x<rsub|1>P<rsub|1>+x<rsub|2>P<rsub|2><right|)>,
  </equation>

  that, using eq. <reference|eq:kldef> yields

  <\equation>
    m<rsub|T>=<frac|4x<rsub|1>x<rsub|2>P<rsub|1>P<rsub|2>+s<rsub|b>|2(x<rsub|1>P<rsub|1>+x<rsub|2>P<rsub|2>)>
    .
  </equation>

  Defining <math|y<rsub|1>=x<rsub|1>/<wide|x|\<bar\>><rsub|1>>,
  <math|y<rsub|2>=x<rsub|2>/<wide|x|\<bar\>><rsub|2>>, we get

  <\equation>
    <frac|m<rsub|T>|<sqrt|s<rsub|b>>>=<frac|1+y<rsub|1>y<rsub|2>|y<rsub|1>+y<rsub|2>>
    .<with|mode|text|>
  </equation>

  The constraint in these variables yields

  <\equation>
    y<rsub|1>+y<rsub|2>\<geqslant\>2<frac|m<rsub|T>|<sqrt|s<rsub|b>>>=2<frac|1+y<rsub|1>y<rsub|2>|y<rsub|1>+y<rsub|2>>
    ,
  </equation>

  which immediately yields

  <\equation>
    y<rsub|1><rsup|2>+y<rsub|2><rsup|2>\<geqslant\>2 .<label|eq:circle>
  </equation>

  A further constraint on <math|y> arises because
  <math|m<rsub|T>/<sqrt|s<rsub|b>>\<geqslant\>1>. This yields

  <\equation>
    <frac|1+y<rsub|1>y<rsub|2>|y<rsub|1>+y<rsub|2>>\<gtr\>0
    \ \ \<Longrightarrow\> (1-y<rsub|1>)(1-y<rsub|2>)\<geqslant\>0 .
  </equation>

  that together with eq. <reference|eq:circle> implies that both
  <math|y<rsub|1>> and <math|y<rsub|2>> must be larger than 1. Furthermore

  <\equation>
    <frac|\<partial\>|\<partial\>y<rsub|1>><frac|1+y<rsub|1>y<rsub|2>|y<rsub|1>+y<rsub|2>>=<frac|y<rsub|2><rsup|2>-1|(y<rsub|1>+y<rsub|2>)<rsup|2>>,
    \ \ \ \ \ <frac|\<partial\>|\<partial\>y<rsub|2>><frac|1+y<rsub|2>y<rsub|1>|y<rsub|2>+y<rsub|1>>=<frac|y<rsub|1><rsup|2>-1|(y<rsub|1>+y<rsub|2>)<rsup|2>>
    ;
  </equation>

  thus <math|m<rsub|T>> has positive derivatives with respect to
  <math|y<rsub|1>> and <math|y<rsub|2>>, which means that its maximum is
  where both <math|y<rsub|1>> and <math|y<rsub|2>> reach thei maxima,
  respectively <math|1/<wide|x|\<bar\>><rsub|1>> and
  <math|1/<wide|x|\<bar\>><rsub|2>>. Thus

  <\equation>
    <frac|m<rsub|T><rsup|max>|<sqrt|s<rsub|b>>>=<frac|<wide|x|\<bar\>><rsub|1><wide|x<rsub|2>|\<bar\>>+1<rsub|>|<wide|x|\<bar\>><rsub|1>+<wide|x|\<bar\>><rsub|2>>
    \ \ \<Longrightarrow\> k<rsub|T max><rsup|2>
    =s<rsub|b><left|(>1-<left|[><frac|<wide|x|\<bar\>><rsub|1><wide|x<rsub|2>|\<bar\>>+1<rsub|>|<wide|x|\<bar\>><rsub|1>+<wide|x|\<bar\>><rsub|2>><right|]><rsup|2><right|)>=s<rsub|b><frac|(1-x<rsub|1><rsup|2>)(1-x<rsub|2><rsup|2>)|(<wide|x|\<bar\>><rsub|1>+<wide|x|\<bar\>><rsub|2><right|)><rsup|2>>
    .
  </equation>
</body>

<\references>
  <\collection>
    <associate|eq:circle|<tuple|10|?>>
    <associate|eq:constraint|<tuple|5|?>>
    <associate|eq:kldef|<tuple|3|?>>
  </collection>
</references>