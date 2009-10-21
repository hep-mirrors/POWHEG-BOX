<TeXmacs|1.0.7>

<style|article>

<\body>
  <doc-data|<doc-title|Upper bounding function ISR>>

  The upper bounding function is

  <\equation>
    U=N<frac|\<alpha\><rsub|s>(k<rsub|T><rsup|2>)|(1-x)(1-y<rsup|2>)> ,
  </equation>

  with

  <\equation>
    k<rsub|T><rsup|2>=<frac|s<rsub|b>|4x>(1-x)<rsup|2>(1-y<rsup|2>) ,
  </equation>

  <math|s<rsub|b>> being the underlying Born <math|s> invariant. The range of
  <math|U> must cover the range of the radiation variables for the given
  underlying Born configuration. A practical restriction for the range of
  <math|U> is

  <\equation>
    \<rho\>\<leqslant\>x\<leqslant\>1, \ \ \ k<rsub|T><rsup|2>\<leqslant\>k<rsub|T
    max><rsup|2> ,
  </equation>

  where

  <\equation>
    \<rho\>=<frac|s<rsub|b>|S>, \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ k<rsub|T
    max><rsup|2>=s<rsub|b><frac|(1-<wide|x|\<bar\>><rsub|1><rsup|2>)(1-<wide|x|\<bar\>><rsub|2><rsup|2>)|(<wide|x|\<bar\>><rsub|1>+<wide|x|\<bar\>><rsub|2>)<rsup|2>>
    .
  </equation>

  We want to generate <math|p<rsub|T>> uniformly in\ 

  <\equation>
    \<Delta\><rsup|(U)>(p<rsub|T>)=exp<left|[>-<big|int>U
    \<theta\>(k<rsub|T>-p<rsub|T>) d x d y d \<phi\><right|]> ,
  </equation>

  in the given range. We assume <math|0\<leqslant\>\<phi\>\<leqslant\>2
  \<pi\>>. Trading <math|y> for <math|k<rsub|T><rsup|2>> we find

  <\equation>
    \|y\|=<sqrt|1-<frac|4x|(1-x)<rsup|2>><frac|k<rsub|T><rsup|2>|s<rsub|b>>>
    ,
  </equation>

  and

  <\equation>
    <big|int> d x <big|int>d y <big|int><rsub|0><rsup|2\<pi\>>d \<phi\> \ \ U
    \ \ \<theta\>(k<rsub|T>-p<rsub|T>)=2\<pi\> N
    <big|int><rsub|\<rho\>><rsup|x<rsub|->>d x
    <big|int><rsub|p<rsub|T><rsup|2>><rsup|k<rsub|T max><rsup|2>><frac|d
    k<rsub|T><rsup|2>|k<rsub|T><rsup|2>><frac|\<alpha\><rsub|s>(k<rsub|T><rsup|2>)|<sqrt|(x<rsub|+>-x)(x<rsub|->-x)>>
    ,
  </equation>

  where

  <\equation>
    \ \ x<rsub|\<pm\>>=<left|(><sqrt|1+<frac|k<rsub|T><rsup|2>|s<rsub|b>>>\<pm\><frac|k<rsub|T>|s<rsub|b>><right|)><rsup|2>
    .
  </equation>

  <math|s<rsub|b>> being the underlying Born <math|s> invariant. The <math|x>
  integration can be performed to yield

  <\equation>
    <big|int> d x <big|int>d y <big|int><rsub|0><rsup|2\<pi\>>d \<phi\> \ \ U
    \ \ \<theta\>(k<rsub|T>-p<rsub|T>)=<big|int><rsub|p<rsub|T><rsup|2>><rsup|k<rsub|T
    max><rsup|2>><frac|d k<rsub|T><rsup|2>|k<rsub|T><rsup|2>>
    V(k<rsub|T><rsup|2>),
  </equation>

  where

  <\equation>
    V(k<rsub|T><rsup|2>)=2 \<pi\> N \ \<alpha\><rsub|s>(k<rsub|T><rsup|2>)
    log <frac|<sqrt|x<rsub|+>-\<rho\>>+<sqrt|x<rsub|->-\<rho\>>|<sqrt|x<rsub|+>-\<rho\>>-<sqrt|x<rsub|->-\<rho\>>>
    .
  </equation>

  We observe that

  <\equation>
    log <frac|<sqrt|x<rsub|+>-\<rho\>>+<sqrt|x<rsub|->-\<rho\>>|<sqrt|x<rsub|+>-\<rho\>>-<sqrt|x<rsub|->-\<rho\>>>\<leqslant\>log<frac|<sqrt|x<rsub|+>>+<sqrt|x<rsub|->>|<sqrt|x<rsub|+>>-<sqrt|x<rsub|->>>=<frac|1|2>
    log<frac|k<rsub|T><rsup|2>+s<rsub|b>|k<rsub|T><rsup|2>> .
  </equation>

  In the NR2005 paper it is suggested to use the bound

  <\equation>
    <frac|1|2> log<frac|k<rsub|T><rsup|2>+s<rsub|b>|k<rsub|T><rsup|2>>\<leqslant\><frac|1|2>
    log <frac|q<rsup|2>|k<rsub|T><rsup|2>>,
    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ with q<rsup|2>=k<rsub|T
    max><rsup|2>+ s<rsub|b> ,
  </equation>

  and to define

  <\equation>
    <wide|V|~>(k<rsub|T><rsup|2>)=2 \<pi\> N
    \ \<alpha\><rsub|s>(k<rsub|T><rsup|2>) <frac|1|2> log
    <frac|q<rsup|2>|k<rsub|T><rsup|2>>\<geqslant\> V(k<rsub|T><rsup|2>) .
  </equation>

  The <math|d k<rsub|T><rsup|2>> integral of <math|<wide|V|~>> can be
  perfomed analytically, yielding

  <\equation>
    <big|int><rsub|p<rsub|T><rsup|2>><rsup|k<rsub|T max><rsup|2>><frac|d
    k<rsub|T><rsup|2>|k<rsub|T><rsup|2>> <wide|V|~>(k<rsub|T><rsup|2>) =
    <frac|\<pi\> N| b<rsub|0>><left|[>log<frac|q<rsup|2>|\<Lambda\><rsup|2>>
    log<frac|log<frac|k<rsub|T max><rsup|2>|\<Lambda\><rsup|2>>|log<frac|p<rsub|T><rsup|2>|\<Lambda\><rsup|2>>>-log<frac|k<rsub|T
    max><rsup|2>|p<rsub|T><rsup|2>><right|]> .
  </equation>

  One generates <math|p<rsub|T>> uniformly in

  <\equation>
    <wide|\<Delta\>|~>(p<rsub|T>)=exp<left|[>-<big|int><rsub|p<rsub|T><rsup|2>><rsup|k<rsub|T
    max><rsup|2>><frac|d k<rsub|T><rsup|2>|k<rsub|T><rsup|2>>
    <wide|V|~>(k<rsub|T><rsup|2>)<right|]>,
  </equation>

  and then use the veto method to get the <math|p<rsub|T>> distributed
  according to <math|\<Delta\><rsup|(U)>(p<rsub|T>)>.

  The following variant of this procedure has been introduced in FNR2007. We
  have used the bound

  <\equation>
    \ log<frac|k<rsub|T><rsup|2>+s<rsub|b>|k<rsub|T><rsup|2>>\<leqslant\><left|{><tabular|<tformat|<table|<row|<cell|log<frac|2
    s<rsub|b>|k<rsub|T><rsup|2>>>|<cell|for
    k<rsub|T><rsup|2>\<less\>s<rsub|b>>>|<row|<cell|log 2>|<cell|for
    k<rsub|T><rsup|2>\<gtr\> s<rsub|b>>>>>> ,
  </equation>

  so

  <\equation>
    <wide|V|~>(k<rsub|T><rsup|2>)= \<pi\> N
    \ \<alpha\><rsub|s>(k<rsub|T><rsup|2>)
    <left|[>\<theta\>(s<rsub|b>-k<rsub|T><rsup|2>) log<frac|2
    s<rsub|b>|k<rsub|T><rsup|2>>+\<theta\>(k<rsub|T><rsup|2>-s<rsub|b>) log
    2<right|]>.
  </equation>

  So now:

  <\eqnarray*>
    <tformat|<table|<row|<cell|log <wide|\<Delta\>|~>(p<rsub|T>)>|<cell|=>|<cell|\<theta\>(s<rsub|b>-p<rsub|T><rsup|2>)
    <frac|\<pi\> N| b<rsub|0>><left|{>\<theta\>(k<rsub|Tmax><rsup|2>-s<rsub|b>)<left|[>log<frac|2s<rsub|b>|\<Lambda\><rsup|2>>
    log<frac|log<frac|s<rsub|b>|\<Lambda\><rsup|2>>|log<frac|p<rsub|T><rsup|2>|\<Lambda\><rsup|2>>>-log<frac|s<rsub|b>|p<rsub|T><rsup|2>>+log(2)
    log<frac|log<frac|k<rsub|T max><rsup|2>|\<Lambda\><rsup|2>>|log<frac|s<rsub|b>|\<Lambda\><rsup|2>>><right|]>>>|<row|<cell|>|<cell|>|<cell|
    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ +\<theta\>(s<rsub|b>-k<rsub|Tmax><rsup|2>)<left|[>log<frac|2
    s<rsub|b>|\<Lambda\><rsup|2>> log<frac|log<frac|k<rsub|T
    max><rsup|2>|\<Lambda\><rsup|2>>|log<frac|p<rsub|T><rsup|2>|\<Lambda\><rsup|2>>>-log<frac|k<rsub|T
    max><rsup|2>|p<rsub|T><rsup|2>><right|]><right|}>
    >>|<row|<cell|>|<cell|+>|<cell|\<theta\>(p<rsub|T><rsup|2>-s<rsub|b>)<frac|\<pi\>
    N| b<rsub|0>> log(2) log<frac|log<frac|k<rsub|T
    max><rsup|2>|\<Lambda\><rsup|2>>|log<frac|p<rsub|T><rsup|2>|\<Lambda\><rsup|2>>><eq-number>>>>>
  </eqnarray*>

  \;

  \;

  \;

  To improve the behaviour from small <with|mode|math|x> effects it may be
  convenient to use instead

  <\equation*>
    <frac|\<alpha\><rsub|s>(k <rsub|T><rsup|2>)|x(1-x)(1-y<rsup|2>)>
  </equation*>

  as upper bounding function. The algebra of ZZ yields

  <\equation*>
    <big|int>U(v,r) \<theta\>(k<rsub|T>(v,r)-p<rsub|T>)d\<Phi\><rsub|r>=<big|int><rsub|p<rsub|T><rsup|2>><rsup|k<rsup|2><rsub|T
    max>><frac|d k<rsub|T><rsup|2>|k<rsub|T><rsup|2>>V(k<rsub|T><rsup|2>) ,
  </equation*>

  where

  <\equation*>
    V(k<rsub|T><rsup|2>)=\<pi\>\<alpha\><rsub|s>(k<rsub|T><rsup|2>)
    log<frac|<sqrt|x<rsub|+>-\<rho\>>+<sqrt|x<rsub|->-\<rho\>>|<sqrt|x<rsub|+>-\<rho\>>-<sqrt|x<rsub|->-\<rho\>>>
    .
  </equation*>

  With the new upper bounding function one gets instead

  <\equation*>
    V(k<rsub|T><rsup|2>)=\<pi\>\<alpha\><rsub|s>(k<rsub|T><rsup|2>)<left|[>log<frac|2|\<rho\>>+log<frac|<sqrt|(x<rsub|+>-\<rho\>)(x<rsub|->-\<rho\>)>+1-<frac|\<rho\>|2>(x<rsub|+>+x<rsub|->)|x<rsub|+>-x<rsub|->><right|]>
    .
  </equation*>

  Even in this case <with|mode|math|V(k<rsub|T><rsup|2>)> satisfies a simple
  upper bound

  <\equation*>
    V(k<rsub|T><rsup|2>)\<less\> \ \ \<pi\>\<alpha\><rsub|s>(k<rsub|T><rsup|2>)
    \ log<frac|S|k<rsub|T><rsup|2>> ,
  </equation*>

  that can be used for fast generation by vetoing.
</body>

<\initial>
  <\collection>
    <associate|page-type|# Simply write the paper name. See papersize(5) for
    possible values>
  </collection>
</initial>