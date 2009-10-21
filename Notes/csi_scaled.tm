<TeXmacs|1.0.6.14>

<style|generic>

<\body>
  <doc-data|<doc-title|Rescaling of the csi variable>>

  We deal with an integral of the form

  <\equation>
    I=<big|int><rsub|-1><rsup|1>d y<big|int><rsub|0><rsup|X(y)>d \<xi\>
    <frac|1|(1-y)<rsub|+>> <left|(><frac|1|\<xi\>><right|)><rsub|\<xi\><rsub|c>>
    \ f(\<xi\>,y) \ .
  </equation>

  First of all, we demonstrate the identity

  <\equation>
    <\equation*>
      <\equation*>
        <big|int><rsub|0><rsup|X>d \<xi\><left|(><frac|1|\<xi\>><right|)><rsub|\<xi\><rsub|c>><big|.>
        F(\<xi\>)=<big|int><rsub|0<rsup|>><rsup|1>d <wide|\<xi\>|~>
        <left|[><left|(><frac|1|<wide|\<xi\>|~>><right|)><rsub|+> +
        log<frac|X|\<xi\><rsub|c>><big|.>
        \ \<delta\>(<wide|\<xi\>|~>)<right|]> F(\<xi\>)
      </equation*>
    </equation*>
  </equation>

  with <with|mode|math|\<xi\>=X<wide|\<xi\>|~>>. If <with|mode|math|F(0)=0>
  we simply have

  <\equation>
    <big|int><rsub|0><rsup|X>d \<xi\><left|(><frac|1|\<xi\>><right|)><rsub|\<xi\><rsub|c>><big|.>
    F(\<xi\>)=<big|int><rsub|0<rsup|>><rsup|1>d <wide|\<xi\>|~>
    <left|(><frac|1|<wide|\<xi\>|~>><right|)><rsub|+> F(\<xi\>)
  </equation>

  For <with|mode|math|F(\<xi\>)=1> we have

  <\equation>
    <big|int><rsub|0><rsup|X>d \<xi\><big|.>
    <left|(><frac|1|\<xi\>><right|)><rsub|\<xi\><rsub|c>>
    =<big|int><rsub|\<xi\><rsub|c>><rsup|X><frac|d \<xi\>|\<xi\>>
    =<big|.>log<frac|X|\<xi\><rsub|c>> =<big|int><rsub|0<rsup|>><rsup|1>d
    <wide|\<xi\>|~> <left|[><left|(><frac|1|<wide|\<xi\>|~>><right|)><rsub|+>
    + log<frac|X|\<xi\><rsub|c>><big|.> \ \<delta\>(<wide|\<xi\>|~>)<right|]>
    \ ,
  </equation>

  which is obvious.

  We now work out the integral. We follow only the <with|mode|math|1/(1-y)>
  term for simplicity

  <\eqnarray*>
    <tformat|<table|<row|<cell|I=<big|int><rsub|-1<rsup|>><rsup|1>d y
    <big|int><rsub|0><rsup|1>d<wide|\<xi\>|~> <frac|1|(1-y)<rsub|+>>
    <left|[><left|(><frac|1|<wide|\<xi\>|~>><right|)><rsub|+> +
    log<frac|X(y)|\<xi\><rsub|c>><big|.> \ \<delta\>(<wide|\<xi\>|~>)<right|]>
    \ f(\<xi\>,y)>|<cell|>|<cell|>>|<row|<cell|=<big|int><rsub|-1><rsup|1>d
    y<big|int><rsub|0><rsup|1>d<wide|\<xi\>|~> <frac|1|1-y>
    <left|[><frac|f(<wide|\<xi\>|~>X(y),y)-f(0,y)|<wide|\<xi\>|~>>
    -<frac|f(<wide|\<xi\>|~>X(1),1)-f(0,1)|<wide|\<xi\>|~>><left|]>
    >|<cell|>|<cell|>>|<row|<cell|+<big|int><rsub|-1><rsup|1>d y
    <frac|1|1-y><left|[>log<frac|X(y)|\<xi\><rsub|c>>f(0,y)-log<frac|X(1)|\<xi\><rsub|c>>f(0,1)<right|]>
    \ \ \ \ ,>|<cell|>|<cell|<eq-number>>>>>
  </eqnarray*>

  which is our needed formula.

  The real contribution to <math|<wide|B|~>> is obtained as

  <\eqnarray*>
    <tformat|<table|<row|<cell|<wide|B|\<bar\>><rsub|real>>|<cell|=>|<cell|<big|int>d<with|math-font-series|bold|\<Phi\>><rsub|n><big|int><rsub|0><rsup|2
    \<pi\>>d \<phi\><big|int><rsub|-1><rsup|1>d
    y<big|int><rsub|0><rsup|X(y)>d \<xi\> \ \ J<rsub|rad>(\<xi\>,y,\<phi\>)
    R>>|<row|<cell|>|<cell|=>|<cell|<big|int>d<with|math-font-series|bold|\<Phi\>><rsub|n><big|int><rsub|0><rsup|2
    \<pi\>>d \<phi\><big|int><rsub|-1><rsup|1>d
    y<big|int><rsub|0><rsup|X(y)>d \<xi\> \ \ <frac|1|(1-y)<rsub|+>>
    <left|(><frac|1|\<xi\>><right|)><rsub|\<xi\><rsub|c>>
    <frac|J<rsub|rad>(\<xi\>,y,\<phi\>)|\<xi\>>
    (1-y)\<xi\><rsup|2>R>>|<row|<cell|>|<cell|=>|<cell|<big|int>d<with|math-font-series|bold|\<Phi\>><rsub|n><big|int><rsub|0><rsup|2
    \<pi\>>d \<phi\><big|int><rsub|-1><rsup|1>d y
    <frac|1|(1-y)<rsub|+>><left|[><big|int><rsub|0><rsup|1>d <wide|\<xi\>|~>
    <left|(><frac|1|<wide|\<xi\>|~>><right|)><rsub|+>
    <frac|J<rsub|rad>(\<xi\>,y,\<phi\>)|\<xi\>>
    (1-y)\<xi\><rsup|2>R>>|<row|<cell|>|<cell|>|<cell|+log<frac|X(y)|\<xi\><rsub|c>>lim<rsub|\<xi\>\<rightarrow\>0><left|(><frac|J<rsub|rad>(\<xi\>,y,\<phi\>)|\<xi\>>
    (1-y)\<xi\><rsup|2>R<right|)><right|]>,<eq-number>>>>>
  </eqnarray*>

  where we should not forget that <math|\<xi\>=<wide|\<xi\>|~>X(y)> is also a
  function of <math|y>. Defining

  <\equation>
    f(\<xi\>,y)=<math|<frac|J<rsub|rad>(\<xi\>,y,\<phi\>)|\<xi\>>>
    (1-y)\<xi\><rsup|2>R,
  </equation>

  we get

  <\eqnarray*>
    <tformat|<table|<row|<cell|<wide|B|\<bar\>><rsub|real>>|<cell|=>|<cell|<big|int>d<with|math-font-series|bold|\<Phi\>><rsub|n><big|int><rsub|0><rsup|2
    \<pi\>>d \<phi\><big|int><rsub|-1><rsup|1><frac|d
    y|1-y><left|{><big|int><rsub|0><rsup|1>d<wide|\<xi\>|~>
    <left|[><frac|f(<wide|\<xi\>|~>X(y),y)-f(0,y)|<wide|\<xi\>|~>>
    -<frac|f(<wide|\<xi\>|~>X(1),1)-f(0,1)|<wide|\<xi\>|~>><left|]>
    >>|<row|<cell|>|<cell|>|<cell|+ <left|[>log<frac|X(y)|\<xi\><rsub|c>>f(0,y)-log<frac|X(1)|\<xi\><rsub|c>>f(0,1)<right|]><right|}>.<eq-number>>>>>
  </eqnarray*>

  In the FKSgeneral program we will thus return
  <math|J<rsub|rad>(\<xi\>,y,\<phi\>)/\<xi\>> from the routine that computes
  the jacobian, and <math| (1-y)\<xi\><rsup|2>R> from the routine that
  computes <math|R>.

  In the case of initial state singularities, the real routine computes
  <math|(1-y<rsup|2>)\<xi\><rsup|2>R>. In this case formula
  (<reference|eq:brealp>) and the corresponding formula for the
  <math|1/(1+y)<rsub|+>> distribution appear divided by a factor of <math|2>,
  accounting for the fact that

  <\equation*>
    <frac|1|1-y>+<frac|1|1+y>=<frac|2|1-y<rsup|2>> .
  </equation*>
</body>