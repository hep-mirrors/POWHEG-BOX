<TeXmacs|1.0.7.3>

<style|generic>

<\body>
  <doc-data|<doc-title|Narrow resonances in <with|font-family|tt|POWHEG>>>

  We deal with the problem of NLO corrections to both a production and a
  decay process. The simplest example is Drell-Yan <math|W> production, the
  <math|W> decaying into a <math|q<wide|q|\<bar\>>> pair. The corresponding
  real process must include an emitted gluon. The gluon may be coming from
  the annihilating <math|q<wide|q|\<bar\>>> pair, or from the thecay product
  of the <math|W>. <with|font-family|tt|POWHEG> must be aware of that; in
  other words, the usual separation of regions performed by
  <with|font-family|tt|POWHEG> fails in this case. In fact, if the emitted
  gluon is associated either with the initial or with a final state in a
  ``fuzzy'' way, using the <math|S<rsub|i j>> separation coefficients that
  <with|font-family|tt|POWHEG> usually adopts, it may happen that the gluon
  that is generated by the final state may be associated with the initial
  state. The corresponding underlying Born will then have the mass of the
  final state <math|q<wide|q|\<bar\>>> pair equal to the <math|W> mass,
  rather than the <math|g q<wide|q|\<bar\>>> system having the mass of the
  resonance. Thus, contributions where the emitted particle is associated
  with the resonance decay product must be kept separated in
  <with|font-family|tt|POWHEG>.

  We thus adopt the following modifications to the
  <with|font-family|tt|POWHEG BOX> setup. We require that the list of
  flavours for the real diagrams, <with|font-family|tt|flst_real(nlegreal,
  index)>, besides including the final state particles, must also include the
  resonances. Furthermore, we add an array
  <with|font-family|tt|flst_group_res(nlegreal, index)>. The entry
  <with|font-family|tt|flst_groupres(j,index)> in the array is zero if
  particle <with|font-family|tt|j> does not come from the decay of a
  resonance; if it comes from a resonance,
  \ <with|font-family|tt|flst_real(j,index)> is the index of the resonance in
  the flavour list. At this point, a real contribution is represented by both
  <with|font-family|tt|flst_real> and <with|font-family|tt|flst_group_res>.
  The need to represent the resonance grouping in the flavour structure
  propagates to most structures of the <with|font-family|tt|pwhg_flst> global
  variables. Including also the resonance in the flavour list requires
  modifications in several parts of the code, where it is assumed that only
  final state particles appear. On the other hand, if the resonance is not
  included in the flavour list, we cannot represent resonances chain decays,
  so, in order to maintain full generality, it is better to include it.

  \;
</body>