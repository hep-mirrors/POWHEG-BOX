<TeXmacs|1.0.7.3>

<style|generic>

<\body>
  <doc-data|<\doc-title>
    The <with|font-family|tt|POWHEG-BOX-HJJ> manual
  </doc-title>|>

  <section|1 \ Introduction>

  The <with|font-family|tt|POWHEG-BOX-HJ> program generates Higgs plus jet
  production in hadronic collisions, and is described in ref. <cite|noi>.
  Here we document its usage.

  <section|2 Generation of events>

  Do<next-line><with|font-family|tt|$ cd POWHEG-BOX/HJJ><next-line><with|font-family|tt|$
  make pwhg_main><next-line>Then do (for example)<next-line><with|font-family|tt|$
  cd testrun-lhc><next-line><with|font-family|tt|$ ../pwhg_main><next-line>At
  the end of the run, the file <with|font-family|tt|pwgevents.lhe> will
  contain events for <math|H+jet > production in the Les Houches format. In
  order to shower them with <with|font-family|tt|PYTHIA>:<next-line><with|font-family|tt|$
  cd POWHEG-BOX/HJJ><next-line><with|font-family|tt|><with|font-family|tt|$
  make main-PYTHIA-lhef><next-line><with|font-family|tt|$ cd
  test><next-line><with|font-family|tt|$ ../main-PYTHIA-lhef>

  <section|Input parameters>

  Parameters in <with|font-family|tt|powheg.input> that are specific to
  <with|font-family|tt|HJJ>: <next-line><with|font-family|tt|hmass 120
  \ \ \ \ \ \ \ \ \ \ ! Higgs mass in GeV <next-line>hwidth 5.753e-3
  \ \ \ \ ! Higgs width in GeV \ <next-line>runningscales 0 \ \ \ \ !
  (default 0), if 0 use hmass as central<next-line>
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ! factorization and renormalization
  scale;<next-line> \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ! if 1 use the hat
  Ht scale (see eq. (5.1) in<next-line> \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ !
  ref. <cite|noi>)<next-line>bwcutoff \ \ 15 \ \ \ \ \ \ ! Higgs Breit-Wigner
  is probed between hmass +- 15*hwidth<next-line>#ckkwscalup 1 \ \ \ \ \ \ !
  (default 1), compute the scalup scale for subsequent<next-line>
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ! shower using the smallest kt in the
  final state;<next-line> \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ! If 0, use
  the standard POWHEG BOX scalup (see section 5.3<next-line>
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ! of ref <cite|noi> for
  details)<next-line>withnegweights 1 \ \ \ ! Default 0; include negative
  weighted events>

  In this program, at variance with the <with|font-family|tt|HJ> generator,
  there is no option for the generation of unweighted events. One must
  therefore use a Born suppression factor. Generation cuts may be introduced
  by suitably modifying the suppression factor. We will introduce this
  possibility in the future depending upon user's requests.

  The Born suppression factor can be modified by editing the
  <with|font-family|tt|born_suppression><with|font-family|tt|> routine in the
  <with|font-family|tt|Born_phsp.f> file. Its default form is given in
  formula (4.6) of ref. <cite|noi>.

  <\bibliography|bib|JHEP|paper.bib>
    <\bib-list|1>
      <bibitem*|1><label|bib-noi>J.<nbsp>Campbell, R.<nbsp>K. Ellis,
      R.<nbsp>Frederix, P.<nbsp>Nason, C.<nbsp>Oleari, and C.<nbsp>Williams.
    </bib-list>
  </bibliography>
</body>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-3|<tuple|3|?>>
    <associate|auto-4|<tuple|3|?>>
    <associate|bib-Campbell:1999ah|<tuple|3|?>>
    <associate|bib-Campbell:2011bn|<tuple|4|?>>
    <associate|bib-Dixon:1998py|<tuple|2|?>>
    <associate|bib-noi|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|bib>
      noi

      noi
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1
      \ Introduction> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2
      Generation of events> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Input
      parameters> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Bibliography>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>