<TeXmacs|1.0.7>

<style|generic>

<\body>
  <doc-data|<doc-title|Combinatorics>>

  Combinatorics are worked out in the routines
  <with|font-family|tt|genflavreglist>, <with|font-family|tt|find_regions<with|font-family|tt|>>,
  <with|font-family|tt|reorder_regions>, and other minore ancillary routines.
  The driving routine <with|font-family|tt|genflavreglist> is called, after
  the <with|font-family|tt|flst_nreal>, \ <with|font-family|tt|flst_real(nlegreal,flst_nreal)>,
  <with|font-family|tt|flst_nborn>, <with|font-family|tt|flst_born(nlegborn,flst_nborn)>
  are filled with the flavour structure of the real and born processes. The
  format is the following:

  <with|font-family|tt|flst_real(k=1...nlegreal,j)>: flavours of the legs of
  the jth real graph

  <with|font-family|tt|flst_born(k=1,...,nlegborn,j)>: flavours of the legs
  of the jth born graph

  Flavour format: integer number corresponding to the PDG id particle code,
  except for gluons, \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ that
  are assinged 0 instead of 21. Flavours are entering for the incoming
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ particles,
  outgoing for outgoing particles.

  \;

  Call sequence:

  <with|font-family|tt|call genflavreglist>

  Effects:

  <\itemize-dot>
    <item><with|font-family|tt|flst_alr(1...nlegreal,1...flst_nalr)> is
    filled with the flavour structures of all the regions
    <math|\<alpha\><rsub|r>> (the ``alr'' in the name stands for alpha-r) as
    defined in the FNO paper. These are computed in the following way:

    <\itemize-minus>
      <item>To each gluon in the final state, one associates an ISR region.

      <item>To each quark in the final state, if it is equal to a quark in
      the initial state one associates an ISR region, defined by the incoming
      and outgoing quark.

      <item>To each quark-gluon (or antiquark-gluon) pair in the final state
      one associates a FSR region.

      <item>To each quark-antiquark pair of the same flavour in the final
      state we associate a FSR region.
    </itemize-minus>

    In all cases, one checks that the underlying Born corresponding to the
    given region is in fact present (up to a permutation of final state
    particles) in the list of Born processes. If the underlying Born is not
    present, the corresponding region is discarded.

    If a real graph has no region at all (i.e., it is
    <with|font-shape|italic|regular>), the counter
    <with|font-family|tt|flst_nregular> is increased, and the flavour content
    of the real contribution is assigned to the array
    <with|font-family|tt|flst_regular(1...nlegreal,flst_nregular)>. The
    contribution of regular graphs will be generated independently from the
    rest.

    We then reorder the flavours in a region in the following way

    <\itemize-minus>
      <item>In ISR region, the radiated parton is moved to the last position
      in the flavour list (i.e. to the <with|font-family|tt|nlegreal>
      positin)

      <item>In a FSR region, the two particles involved are moved to the endo
      of the list, making sure that the particle that can give soft
      singularities is moved to the last position, and if we have a
      <math|q<wide|q|\<bar\>>> pair, the <math|<wide|q|\<bar\>>> is moved to
      the last position.
    </itemize-minus>

    After the reordering, we assign to the
    <with|font-family|tt|flst_emitter(j=1...flst_nalr)> array the value

    <\itemize-minus>
      <item>0 if the <with|font-family|tt|flst_alr(...,j)> region is an ISR
      region with an emitted gluon

      <item>1(2) if it is an ISR region collinear to 1(2), and the emitted
      parton is not a gluon

      <item>The value <with|font-family|tt|nlegreal-1> for all other (FSR)
      cases\ 
    </itemize-minus>

    This definition correspond to the emitting particles, except in the
    firsts case, where the value 0 is used to mean 1 and 2 at the same time.

    <item>As a consequence of the reordering, some regions may turn out to be
    identical. We thus go through the list of regions and lump together
    identical region. The value of <with|font-family|tt|flst_nalr> is reduce
    to account for this lumping procedure, and the array
    <with|font-family|tt|flst_mult(1...flst_nalr)> is filled with the
    multiplicity of each region. The array <with|font-family|tt|flst_emitter>
    is updated accordingly.

    <item>The underlying Born graph fllavour associated to each region is
    computed, and stored in the array <with|font-family|tt|flst_uborn(1...nlegborn,1...flst_nalr)>.

    <item>At this stage we run through the list, and look for the underlying
    Born flavour sturture in the list of born graphs given in
    <with|font-family|tt|flst_born(1...nlegborn,1...flst_nborn)>. We may find
    the underlying Born structure in this list, in which case we set the
    pointer <with|font-family|tt|flst_alr2born(j)> (where
    <with|font-family|tt|j> is the index of the region we are considering) to
    the index <with|font-family|tt|k> such that
    <with|font-family|tt|flst_born(1...nlegborn,k)> is equal to
    \ <with|font-family|tt|flst_uborn(1...nlegborn,j)>. It may happen,
    however, that we find a flavour structure that is equivalent only up to
    the permutation of some final state coloured partons. Also in this case
    we set the <with|font-family|tt|flst_alr2born(j)> index as before, but we
    also perform a permutation of final state particles upon the arrays
    <with|font-family|tt|flst_alr(1...nlegreal,j)>,
    \ <with|font-family|tt|flst_uborn(1...nlegborn,j)>, and
    <with|font-family|tt|flst_emitter(j)> if the permutation changes the
    position of the <with|font-family|tt|nlegborn> element of the list. At
    the end of this procedure, <with|font-family|tt|flst_emitter> may end up
    being different from <with|font-family|tt|nlegborn>. This fact can take
    place if the Born graphs have more than one coloured massless parton in
    the final state, as, for example, in dijet production.

    <item>At this stage, an array of arrays of pointers

    \ <with|font-family|tt|flst_born2alr(0...flst_nalr,1...flst_nborn)>

    is setup, that associated to each Born flavour structure, a list of the
    regions that share this same underlying born. The integer
    <math|><with|font-family|tt|flst_born2alr(0,k)> stores the number of
    regions that have <with|font-family|tt|flst_born(1...nlegborn,k)> as
    underlying Born, and <with|font-family|tt|flst_born2alr(1...flst_born(1...nlegborn,k),k)>
    are the indices in the region list.

    <item>As a final step, for each region in the list, we also need a list
    of all the singular regions associated to that flavour structures. This
    is needed, because the contribution of each region is given by the real
    graph multiplied by weight factors <math|S<rsub|i>>, <math|S<rsub|i j>>.
    These in turn have the structure described in section 2.4.1 of FNO. In
    order to compute them, we need a list of all singular regions associated
    to the given real graph flavour structure. We thus setup the array

    <with|font-family|tt|flst_allreg(1:2,0:maxregions,1:flst_nalr)>

    the first two elements corresponding to the emitter and emitted parton,
    the integer <with|font-family|tt|flst_allreg(1,0,j)=m> is the number of
    singular regions associated to the flavour structure of the
    <with|font-family|tt|jth> region, and
    <with|font-family|tt|flst_allreg(1:2,1:m,j)> is the list of singular
    regions associated to the <with|font-family|tt|jth> region.
  </itemize-dot>
</body>