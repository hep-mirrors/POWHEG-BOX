<TeXmacs|1.0.7.3>

<style|generic>

<\body>
  <doc-data|<doc-title|Dijets in POWHEG>|<doc-author-data|<author-name|S.
  Alioli, K. Hamilton, P. Nason, C. Oleari and E. Re>>>

  <\itemize-dot>
    <item>Enter the <with|font-family|tt|POWHEG-BOX/jj> directory.

    <item>Edit the Makefile to make sure you have the right path for the
    LHAPDF library and for the fastjet library. You can avoid using LHAPDF,
    and use instead the mlmpdf package, which is provided with the
    <with|font-family|tt|BOX>. Some adjustments may be require to link
    fastjet in your platform. However, fastjet is not needed in order to
    generate events. You can replace the <with|font-family|tt|pwhg_analysis.f>
    file with a dummy one <with|font-family|tt|pwhg_analysis-dummy.f>, take
    away the <with|font-family|tt|fastjet*wrap.o> files and the fastjet
    libraries from the dependencies, and the program will link. You can also
    avoid using the LHAPDF, and use instead the mlmpdf package that is
    provided by the BOX itself.

    <item>Do make <with|font-family|tt|pwhg_main>. There are several warning
    because of the -Wall flag in compilation. they can be ignored.

    <item>Go to the <with|font-family|tt|testrun-lhc> (or
    <with|font-family|tt|testrun-tev>) directory. Change the
    <with|font-family|tt|powheg.input> file at your will. Run
    <with|font-family|tt|../pwhg_main>. It will take about 30 hours to
    generate 0.5M events in the <with|font-family|tt|testrun-tev> case, a
    little more in the lhc case.

    <item>In order to split up the work on different nodes, follow the
    instructions in <with|font-family|tt|POWHEG-BOX/Docs/Manyseeds.pdf>.

    <item>At the end of the \ <with|font-family|tt|../pwhg_main> run, a file
    named <with|font-family|tt|pwgevents.lhe> will be created. It contains
    the events in Les Houches format. Shower and analysis is performed by an
    independent program. An example on how to shower and analyze results
    using <with|font-family|tt|PYTHIA> is given in the Makefile target
    <with|font-family|tt|main-PYTHIA-lhef>. This is the part of the program
    that should be used as an example for interfacing the output to a given
    analysis framework. Our example (used for the paper in preparation) uses
    an internal histogramming package, and produces topdrawer files for
    analysis.
  </itemize-dot>
</body>

<\initial>
  <\collection>
    <associate|sfactor|4>
  </collection>
</initial>