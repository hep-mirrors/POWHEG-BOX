<TeXmacs|1.0.6.15>

<style|generic>

<\body>
  Process specific values in common blocks. Look at common block in
  <with|font-family|tt|FlavourStructs.h>.

  \;

  \;

  Process specific subroutines:

  <\itemize-dot>
    <item><with|font-family|tt|init_processes>: Sets up the list of processes
    in the arrays <with|font-family|tt|flst_alr>, of length
    \ <with|font-family|tt|flst_nalr>, for the real processes, and
    <with|font-family|tt|flst_born>, of length
    \ <with|font-family|tt|flst_nborn>, for the born processes.

    <item><with|font-family|tt|setborn(p,bflav,born,bornjk,bmunu)>: computed
    the Born process.

    <\itemize-minus>
      <item><with|font-family|tt|p(0:3,nlegborn)>: momenta

      <item><with|font-family|tt|bflav(1:nlegborn)>: flavour of incoming and
      outgoing particles

      <item><with|font-family|tt|born>: return value for born, spin and
      colour summed and averaged, no flux factor

      <item><with|font-family|tt|bornjk(nlegborn,nlegborn)>: return value, as
      above, for colour ordered amplitudes

      <item><with|font-family|tt|bmunu>(0:3,0:3,?): return value, as above,
      with lorentz index for incoming or outgoing gluons.
    </itemize-minus>

    <item><with|font-family|tt|real_ampsq(p,rflav,amp2)>: as above for real
    cross section

    <\itemize-minus>
      <item><with|font-family|tt|p(0:3,nlegreal)>: momenta

      <item><with|font-family|tt|bflav(1:nlegreal)>: flavour of incoming and
      outgoing particles

      <item><with|font-family|tt|real>: return value for born, spin and
      colour summed and averaged, no flux factor
    </itemize-minus>

    <item><with|font-family|tt|gen_born_phsp(xborn)>: generates Born phase
    space

    <\itemize-minus>
      <item><with|font-family|tt|xborn(ndim)> array of dimension
      <with|font-family|tt|ndim=(nlegborn-2)*3-4+2-1>. The array, in a unit
      hypercube, parametrizes the Born phase space, up to an overall
      azimuthal rotation.
    </itemize-minus>

    side effects:

    <\itemize-minus>
      <item><with|font-family|tt|kn_masses(nlegborn)>: on the first call to
      the subroutine, is se to the masses of the incoming and outgoing
      particles.

      <item><with|font-family|tt|kn_pborn(0:3,nlegborn)>: on each call, is
      filled with the computed Born momenta.

      <item><with|font-family|tt|kn_cmpborn(0:3,nlegborn)>: on each call, is
      filled with the computed partonic centre of mass Born momenta.

      <item><with|font-family|tt|kn_jacborn>: is set to the jacobian.
    </itemize-minus>
  </itemize-dot>

  \;

  \;
</body>