<TeXmacs|1.0.7>

<style|generic>

<\body>
  <doc-data|<doc-title|Collinear limit of tree amplitudes>>

  In the following we find the collinear factors that relates squared
  amplitudes, summed over spins and colours. We consider first initial state
  radiation.

  <section|The <math|g\<rightarrow\>g> process>

  Consider the <math|g\<rightarrow\>g g> process. An incoming gluon with
  momentum <math|p> and Lorentz index <math|\<mu\>> enters the vertex, a
  gluon with momentum <math|k> and index <math|\<nu\>> leaves the vertex and
  enters the amplitude, a gluon with momentum <math|p-k> leaves the vertex.
  We set

  <\equation>
    k=x p+\<eta\>\<xi\>+k<rsub|T>, \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ p<rsup|2>=0,
    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (p-k)<rsup|2>=k<rsub|T><rsup|2>-2(1-x)\<xi\>
    \ \ \<eta\>\<cdot\>p=0, \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \<xi\>=<frac|k<rsub|T><rsup|2>|2\<eta\>\<cdot\>p(1-x)>
    .
  </equation>

  We also need

  <\equation*>
    (p-k)<rsup|2>=-2p\<cdot\>k+k<rsup|2>=0 \ \ \ \ \<Longrightarrow\>
    \ k<rsup|2>=2p\<cdot\>k=2p\<cdot\>\<eta\>
    \<xi\>=<frac|k<rsub|T><rsup|2>|1-x>
  </equation*>

  The 3-gluon vertex is given by

  <\equation>
    \<Gamma\><rsup|\<mu\>\<nu\>\<rho\>>(p,-k,k-p)=(p+k)<rsup|\<rho\>>g<rsup|\<mu\>\<nu\>>+(p-2k)<rsup|\<mu\>>g<rsup|\<nu\>\<rho\>>+(k-2p)<rsup|\<nu\>>g<rsup|\<mu\>\<rho\>>,
  </equation>

  and using gauge invariance we can replace

  <\eqnarray*>
    <tformat|<table|<row|<cell|(p+k)<rsup|\<rho\>>>|<cell|\<Rightarrow\>>|<cell|(p+k)<rsup|\<rho\>>-<frac|1+x|1-x>(p-k)<rsup|\<rho\>>=<frac|2|1-x>(\<eta\>\<xi\>+k<rsub|T>)<rsup|\<rho\>>>>|<row|<cell|(p-2k)<rsup|\<mu\>>>|<cell|\<Rightarrow\>>|<cell|(p-2k)<rsup|\<mu\>>-(1-2x)p<rsup|\<mu\>>=-2(\<eta\>\<xi\>+k<rsub|T>)<rsup|\<mu\>>>>|<row|<cell|(k-2p)<rsup|\<nu\>>>|<cell|\<Rightarrow\>>|<cell|(k-2p)<rsup|\<nu\>>+<frac|2-x|x>k<rsup|\<nu\>>=<frac|2|x>(\<eta\>\<xi\>+k<rsub|T>)<rsup|\<nu\>><eq-number>>>>>
  </eqnarray*>

  so that the vertex can be replaced with

  <\equation>
    <frac|2|1-x>k<rsub|T><rsup|\<rho\>> \ g<rsup|\<mu\>\<nu\>>-2k<rsub|T><rsup|\<mu\>>
    g<rsup|\<nu\>\<rho\>>+<frac|2|x>k<rsub|T><rsup|\<nu\>>
    g<rsup|\<mu\>\<rho\>>.
  </equation>

  This leads to terms of order <math|k<rsub|T><rsup|2>> when squared. Had we
  kept <math|\<xi\>> terms we would have had non singular terms at the end.
  We now square the vertex, to get

  <\eqnarray*>
    <tformat|<table|<row|<cell|<frac|4|(1-x)<rsup|2>>k<rsub|T><rsup|2>
    g<rsup|\<nu\>\<nu\><rprime|'>><rsub|T>+4k<rsub|T><rsup|2>
    g<rsup|\<nu\>\<nu\><rprime|'>><rsub|T>+<frac|8|x<rsup|2>>k<rsub|T><rsup|\<nu\>>
    k<rsub|T><rsup|\<nu\><rprime|'>>+<left|[>-<frac|1|1-x>-<frac|1|x>+<frac|1|x(1-x)><right|]>4
    k<rsub|T><rsup|\<nu\>> k<rsub|T><rsup|\<nu\><rprime|'>>>|<cell|=>|<cell|>>|<row|<cell|4k<rsub|T><rsup|2><frac|<frac|x|1-x>+<frac|1-x|x>+x(1-x)|x(1-x)>
    g<rsup|\<nu\>\<nu\><rprime|'>>+<frac|8|x<rsup|2>><left|[>k<rsub|T><rsup|\<nu\>>
    k<rsub|T><rsup|\<nu\><rprime|'>>-<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2>k<rsub|T><rsup|2><right|]>>|<cell|>|<cell|<eq-number>>>>>
  </eqnarray*>

  We now divide by <math|k<rsup|4>>, using also
  <math|k<rsub|T><rsup|2>=(1-x)k<rsup|2>>, include a
  <math|g<rsub|S><rsup|2>=4\<pi\>\<alpha\><rsub|S>> factor, and get the AP
  factor

  <\equation>
    8\<pi\>\<alpha\><rsub|S> <frac|1|x> <frac|-1|k<rsup|2>>
    C<rsub|A><left|{>-2 <left|[><frac|x|1-x>+<frac|1-x|x>+x(1-x)<right|]>g<rsup|\<nu\>\<nu\><rprime|'>>+<frac|4(1-x)|x><left|[><wide|k|^><rsub|T><rsup|\<nu\>>
    <wide|k|^><rsub|T><rsup|\<nu\><rprime|'>>+<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2><right|]><right|}>
  </equation>

  where <math|<wide|k|^><rsub|T>=k<rsub|T>/<sqrt|\|k<rsub|T><rsup|2>\|>>.
  <math|C<rsub|A>> originates from the colour sum. When dealing with colour
  and spin averaged amplitude, this formula remains unchanged, since a gluon
  enters the amplitude as well as the subamplitude. When dealing with cross
  section, the <math|1/x> factor disappears, since it turns the flux factor
  of the amplitude into that of the subamplitude.

  Notice that <math|-g<rsup|\<nu\>\<nu\><rprime|'>>A<rsup|(r)><rsub|\<nu\>\<nu\><rprime|'>>>
  is positive, and <math|-1/k<rsup|2>> is also positive.

  <section|The <math|q\<rightarrow\>q> process>

  The amplitude contains the product

  <\equation>
    <frac|1|k<rsup|4>> <neg|k> \ <neg|\<epsilon\>> \ <neg|p>
    \ <neg|\<epsilon\>> <neg|k>\ 
  </equation>

  where <math|\<epsilon\>> is the polarizarion of the emitted gluon. The
  trick is to use the fact that the polarization is orthogonal to <math|p-k>.
  Thus we write

  <\equation>
    k=<frac|x|1-x>(p-k)+\<xi\>\<eta\>+k<rsub|T>+<frac|x|1-x>(\<xi\>\<eta\>+k<rsub|T>)=<frac|x|1-x>(p-k)+<frac|1|1-x>(\<xi\>\<eta\>+k<rsub|T>)
    ,
  </equation>

  and get

  <\eqnarray*>
    <tformat|<table|<row|<cell|<frac|1|k<rsup|4>> <frac|1|(1-x)<rsup|2>>
    [-<neg|\<epsilon\>>x(<neg|p>-<neg|k>)+\<xi\><neg|\<eta\>><neg|\<epsilon\>>+<neg|k><rsub|T><neg|\<epsilon\>>]
    <neg|p> [ -x(<neg|p>-<neg|k>)<neg|\<epsilon\>>+\<xi\><neg|\<epsilon\>><neg|\<eta\>>+<neg|\<epsilon\>><neg|k><rsub|T>]>|<cell|\<approx\>>|<cell|>>|<row|<cell|<frac|1|k<rsup|4>>
    <frac|1|(1-x)<rsup|2>> [<neg|\<epsilon\>>x(\<xi\><neg|\<eta\>>+<neg|k><rsub|T>)+\<xi\><neg|\<eta\>><neg|\<epsilon\>>+<neg|k><rsub|T><neg|\<epsilon\>>]
    <neg|p> [ x(\<xi\><neg|\<eta\>>+<neg|k><rsub|T>)<neg|\<epsilon\>>+\<xi\><neg|\<epsilon\>><neg|\<eta\>>+<neg|\<epsilon\>><neg|k><rsub|T>]>|<cell|\<approx\>>|<cell|>>|<row|<cell|<frac|1|k<rsup|4>>
    <frac|1|(1-x)<rsup|2>> [<neg|\<epsilon\>>x
    <neg|k><rsub|T>+<neg|k><rsub|T><neg|\<epsilon\>>] <neg|p> [ x
    <neg|k><rsub|T><neg|\<epsilon\>>+<neg|\<epsilon\>><neg|k><rsub|T>]>|<cell|\<approx\>>|<cell|>>|<row|<cell|<frac|1|k<rsup|4>>
    <frac|1|(1-x)<rsup|2>> [-(1-x)<neg|\<epsilon\>>
    <neg|k><rsub|T>+2k<rsub|T>\<cdot\>\<epsilon\>] <neg|p> [-(1-x)
    <neg|k><rsub|T> <neg|\<epsilon\>>+2k<rsub|T>\<cdot\>\<epsilon\>]>|<cell|\<approx\>>|<cell|>>|<row|<cell|<frac|1|k<rsup|4>>
    <frac|<neg|p>|(1-x)<rsup|2>> [-(1-x)<rsup|2>k<rsub|T><rsup|2>+4*(k<rsub|T>\<cdot\>\<epsilon\>)<rsup|2>-(2k<rsub|T>\<cdot\>\<epsilon\>)<rsup|2>(1-x)]>|<cell|>|<cell|<eq-number>>>>>
  </eqnarray*>

  Summing over the two polarizations, we get

  <\equation>
    <frac|-k<rsub|T><rsup|2>|k<rsup|4>> <frac|<neg|p>|(1-x)<rsup|2>>
    [2(1-x)<rsup|2>+4*-4(1-x)]=<frac|-1|k<rsup|2>>2<frac|1+x<rsup|2>|1-x><neg|p>=<frac|-1|k<rsup|2>><frac|2|x><frac|1+x<rsup|2>|1-x>(x
    <neg|p>) .
  </equation>

  The <math|x<neg|p>> factor contributes to give the subamplitude in place of
  the <math|<neg|p>> factor in the amplitude. Thus the splitting factor is

  <\equation>
    8\<pi\>\<alpha\><rsub|S> <frac|1|x> <frac|-1|k<rsup|2>>
    C<rsub|F><frac|1+x<rsup|2>|1-x>
  </equation>

  that remains the same when dealing with spin and colour averaged squared
  amplitudes.

  \;

  <section|The <math|q\<rightarrow\>g> process>

  Consider now the case <math|q\<rightarrow\>q g>, with the gluon entering
  the subamplitude. We should compute

  <\equation>
    tr(p,\<nu\>,p-k,\<nu\><rprime|'>)=4(p<rsup|\<nu\>>(p-k)<rsup|\<nu\><rprime|'>>+p<rsup|\<nu\><rprime|'>>(p-k)<rsup|\<nu\>>-p\<cdot\>(p-k)
    g<rsup|\<nu\>\<nu\><rprime|'>>).
  </equation>

  Using gauge invariance we get rid of <math|k<rsup|\<nu\>>> and
  <math|k<rsup|\<nu\><rprime|'>>>, and get

  <\equation>
    4(2p<rsup|\<nu\>>p<rsup|\<nu\><rprime|'>>+p\<cdot\>k
    g<rsup|\<nu\>\<nu\><rprime|'>>).
  </equation>

  Now we substitute

  <\equation>
    p<rsup|\<nu\>>\<rightarrow\> p<rsup|\<nu\>>-<frac|1|x>k \ \ \ \<approx\>
    \ \ -<frac|1|x>k<rsub|T>,
  </equation>

  and get

  <\eqnarray*>
    <tformat|<table|<row|<cell|4<left|(><frac|2|x<rsup|2>>k<rsub|T><rsup|\<nu\>>
    k<rsub|T><rsup|\<nu\><rprime|'>>+p\<cdot\>k
    \ g<rsup|\<nu\>\<nu\><rprime|'>><right|)>=4
    p\<cdot\>k<left|(><frac|4(1-x)|x<rsup|2>> <frac|k<rsub|T><rsup|\<nu\>>
    k<rsub|T><rsup|\<nu\><rprime|'>>|k<rsub|T><rsup|2>>+g<rsup|\<nu\>\<nu\><rprime|'>><right|)>>|<cell|=>|<cell|>>|<row|<cell|4
    p\<cdot\>k<left|(><frac|4(1-x)|x<rsup|2>><left|[><frac|k<rsub|T><rsup|\<nu\>>
    k<rsub|T><rsup|\<nu\><rprime|'>>|k<rsub|T><rsup|2>>-<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2><right|]>+g<rsup|\<nu\>\<nu\><rprime|'>><frac|1+(1-x)<rsup|2>|x<rsup|2>><right|)>,>|<cell|>|<cell|<eq-number>>>>>
  </eqnarray*>

  and dividing by <math|k<rsup|4>> we get

  <\equation>
    8\<pi\>\<alpha\><rsub|S> T<rsub|F><frac|1|x>
    <frac|-1|k<rsup|2>><left|(>-g<rsup|\<nu\>\<nu\><rprime|'>><frac|1+(1-x)<rsup|2>|x>+<frac|4(1-x)|x><left|[><wide|k|^><rsub|T><rsup|\<nu\>>
    <wide|k|^><rsub|T><rsup|\<nu\><rprime|'>>+<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2><right|]><right|)>.
  </equation>

  When considering spin and colour averaged squared amplitudes, again, spin
  average makes no difference. For the color, colour averaging yields
  <math|1/N<rsub|c>>, so <math|T<rsub|F>/N<rsub|c>=C<rsub|F>/D<rsub|A>>,
  where <math|D<rsub|A>> is the dimension of the adjoint representation, that
  provides the colour average for the subamplitude. Thus, the
  <math|T<rsub|F>> colour factor is replaced by <math|C<rsub|F>>.

  <section|Crossing>

  Spin and colour summed invariant amplitudes satisfy crossing
  relations<\footnote>
    If colour or spin average is performed, the crossing relations are
    broken, since initial and final state particles are treated differently.
  </footnote>. The collinear factorization formula

  <\equation>
    A(p)=<frac|1|x k<rsup|2>>P(x,k<rsub|T>)A<rsub|r>(k),
  </equation>

  with <math|x=k\<cdot\>q/p\<cdot\>q> should also hold if the particles with
  momentum <math|p> and <math|k> are crossed to outgoing particles. In this
  case one interprets <math|z=p\<cdot\>q/k\<cdot\>q>, so we should let
  <math|x=1/z>. For the <math|g\<rightarrow\>g> process we get

  <\eqnarray*>
    <tformat|<table|<row|<cell|8\<pi\>\<alpha\><rsub|S> \ z
    <frac|-1|k<rsup|2>> C<rsub|A><left|{>-2
    <left|[><frac|1|z-1>+z-1+<frac|z-1|z<rsup|2>><right|]>g<rsup|\<nu\>\<nu\><rprime|'>>+4(z-1)<left|[><wide|k|^><rsub|T><rsup|\<nu\>>
    <wide|k|^><rsub|T><rsup|\<nu\><rprime|'>>+<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2><right|]><right|}>>|<cell|=>|<cell|>>|<row|<cell|8\<pi\>\<alpha\><rsub|S>
    \ <frac|1|k<rsup|2>> C<rsub|A><left|{>-2<left|[><frac|z|1-z>+z(1-z)+<frac|1-z|z><right|]>g<rsup|\<nu\>\<nu\><rprime|'>>+4z(1-z)<left|[><wide|k|^><rsub|T><rsup|\<nu\>>
    <wide|k|^><rsub|T><rsup|\<nu\><rprime|'>>+<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2><right|]><right|}>>|<cell|>|<cell|<eq-number>>>>>
  </eqnarray*>

  For the <math|g\<rightarrow\>q<wide|q|\<bar\>>> process, remembering to
  include a - sign for crossing a fermion line, we get

  <\eqnarray*>
    <tformat|<table|<row|<cell|8\<pi\>\<alpha\><rsub|S> T<rsub|F> \ z
    \ <frac|1|k<rsup|2>><left|(>-g<rsup|\<nu\>\<nu\><rprime|'>>(z+z(1-1/z)<rsup|2>)+4(z-1)<left|[><wide|k|^><rsub|T><rsup|\<nu\>>
    <wide|k|^><rsub|T><rsup|\<nu\><rprime|'>>+<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2><right|]><right|)>>|<cell|=>|<cell|>>|<row|<cell|8\<pi\>\<alpha\><rsub|S>
    T<rsub|F> <frac|1|k<rsup|2>><left|(>-g<rsup|\<nu\>\<nu\><rprime|'>><left|(>z<rsup|2>+(1-z)<rsup|2><right|)>-4z(1-z)<left|[><wide|k|^><rsub|T><rsup|\<nu\>>
    <wide|k|^><rsub|T><rsup|\<nu\><rprime|'>>+<frac|g<rsup|\<nu\>\<nu\><rprime|'>>|2><right|]><right|)>>|<cell|>|<cell|>>>>
  </eqnarray*>
</body>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-3|<tuple|3|?>>
    <associate|auto-4|<tuple|4|?>>
    <associate|footnote-1|<tuple|1|?>>
    <associate|footnr-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|The
      <with|mode|<quote|math>|g\<rightarrow\>g> process>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|The
      <with|mode|<quote|math>|q\<rightarrow\>q> process>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|The
      <with|mode|<quote|math>|q\<rightarrow\>g> process>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Crossing>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>