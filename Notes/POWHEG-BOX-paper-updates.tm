<TeXmacs|1.0.7.2>

<style|generic>

<\body>
  <section|Multiplicity factor for the underlying Born>

  Each <with|font-family|tt|alr> has a multiplicity factor associated with
  it, stored in the the <with|font-family|tt|flst_mult> array. This factor is
  there since the same region can appear in several equivalent permutations.
  Given a region characterized by two particles of flavours
  <with|font-family|tt|><math|a> and <math|b> becoming unresolved, for a
  given real flavour structure, the corresponding multiplicity corresponds to
  how many ways we can pick the pair <math|a,b>. In case particle <math|a> is
  in the initial state, the multiplicity equals the number of particles of
  flavour <math|b> in the final state. If both <math|a> and <math|b> are in
  the final state, the multiplicity is given by the number of ways we can
  find the unordered pair <math|a,b> among the final state particles (by
  unordered, we mean that the factor of two associated with choosing the pair
  in reverse order should not be included). It is thus the multiplicity for a
  two particle inclusive cross section.

  The following example may clarify what we mean. Suppose we produce four
  identical particles in the final state. The symmetry factor for this
  contribution is <math|1/4!>. However, the symmetry factor for the inclusive
  single particle cross section is <math|1/3!>. This cross section is in fact
  defined by tagging one of the final state particles. This can be done in 4
  ways. Alternatively, we tag the particle, and we find only 3 identical
  particles in the final state (the tagged particle, having been singled out,
  is not counted in the symmetry factor). If we tag an unordered pair of
  particles, the final multiplicity factor is given by the number of way we
  can pick two particles out of four, which is <math|4\<times\>3/2=6> (we
  divide by two because we do not care about the order). The symmetry factor
  becomes <math|6/4!=1/4>, <math|1/2> because of the two identical untagged
  particles, and the other <math|1/2> accounting for the fact that we do not
  care about the order of the tagged pair.

  For final state radiation, when computing the collinear and soft
  approximation to the given <with|font-family|tt|alr>, we should keep in
  mind that also the corresponding underlying Born should be view as an
  inclusive cross section for the given emitter. In fact, the factorization
  theorem for the squared amplitude either applies in the absence of any
  symmetry factor, or to squared amplitudes with an inclusive cross section
  as symmetry factor. The underlying Born symmetry factor should thus be
  computed by singling out the emitter, and considering it different from all
  othe particles of the same flavour. Or, equivalently, we should multilpy
  the full symmetry factor by the mumber of times that the flavour of the
  emitter appears in the final state. In the <with|font-family|tt|BOX>, the
  array <with|font-family|tt|flst_ubmult(alr)> stores precisely this last
  factor.
</body>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Multiplicity
      factor for the underlying Born> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>