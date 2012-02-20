
TARGETNAME=HnJ
BIBFILE=paper
FXFILES=
TOPFILES=


SEPS= 
FDFILES=
GPFILES=HJ_H-pt_fixsc.gp HJ_H-pt_runsc.gp HJ_H-y_fixsc.gp HJ_H-y_runsc.gp HJ_j1-pt_fixsc.gp HJ_j1-pt_runsc.gp HJ_j2-pt_cuts_fixsc.gp HJ_j2-pt_cuts_had_fixsc.gp HJ_j2-pt_cuts_had_runsc.gp HJ_j2-pt_cuts_runsc.gp HJ_j2-pt_fixsc_bornonly.gp HJ_j2-pt_fixsc.gp HJ_j2-pt_runsc_bornonly.gp HJ_j2-pt_runsc.gp HJJ_H-pt_fixsc.gp HJJ_H-pt_runsc.gp HJJ_H-y_fixsc.gp HJJ_H-y_runsc.gp HJJ_j1-pt_fixsc.gp HJJ_j1-pt_runsc.gp HJJ_j2-pt_fixsc.gp HJJ_j2-pt_runsc.gp HJJ_j3-pt_cuts_fixsc.gp HJJ_j3-pt_cuts_had_fixsc.gp HJJ_j3-pt_cuts_had_runsc.gp HJJ_j3-pt_cuts_runsc.gp HJJ_j3-pt_fixsc.gp HJJ_j3-pt_runsc.gp HJJ_mult_fixsc.gp HJJ_mult_runsc.gp HJJ_ptrel1_fixsc.gp HJJ_ptrel1_runsc.gp HJJ_ptrel2_fixsc.gp HJJ_ptrel2_runsc.gp HJ_mult_fixsc.gp HJ_mult_runsc.gp HJ_ptrel1_fixsc_bornonly.gp HJ_ptrel1_fixsc.gp HJ_ptrel1_runsc.gp j2-pt_comp_fixsc.gp j2-pt_comp_runsc.gp H-pt_comp_fixsc_runsc.gp

EXTRAS=
EPS=$(patsubst %.fx,%.eps,$(FXFILES)) $(patsubst %.top,%.eps,$(TOPFILES)) \
    $(patsubst %.seps,%.eps,$(SEPS)) $(patsubst %.C,%.eps,$(FDFILES))\
    $(patsubst %.gp,%.eps,$(GPFILES))
PDF=$(patsubst %.fx,%.pdf,$(FXFILES)) $(patsubst %.top,%.pdf,$(TOPFILES)) \
    $(patsubst %.seps,%.pdf,$(EPS)) $(patsubst %.gp,%.pdf,$(GPFILES)) 

FILES= Makefile $(TEXFILES) $(TOPFILES) $(FXFILES) $(SEPS) $(GPFILES) $(EXTRAS) $(FDFILES)

VPATH= ./:plots/


## user makefile has following lines
#TARGETNAME=bbcorr96
#TEXFILES=files.tex ...
#FXFILES=files.fx ...
#TOPFILES=files.top ...
#EXTRAS=hep-ph.txt hep-ph.9607347 etc.
#EPS=$(patsubst %.fx,%.eps,$(FXFILES)) $(patsubst %.top,%.eps,$(TOPFILES)) 
#FILES= Makefile Makefile.AIX Makefile.Linux Makefile.SunOS \
#       $(TEXFILES) $(TOPFILES) $(FXFILES) $(EXTRAS)
#
#

# Standard tex Makefile, P. Nason, 6-8-96

# Add the eps figures for your collegues

EPSFX=$(patsubst %.fx,%.eps,$(FXFILES))

%.eps: %.fx
	echo $* ps-fonts | feynx

%.eps: %.top
	td_japan -b -deps $*.top;  # ./fixcoloreps.sh $*.eps

%.eps: %.seps
	 cp $*.seps $*.eps

%.eps: %.C
	FD  $* 
	mv $*.ps $*.eps

%.eps: %.gp
	cd plots ; gnuplot $*.gp 



%.pdf: %.eps
	if grep '%%BoundingBox: (atend)' $*.eps ; \
        then export bb=`grep BoundingBox $*.eps| grep -v atend` ; \
             cat $*.eps| sed "s/%%BoundingBox: (atend)/$$bb/" > fixbb.eps ; \
             epstopdf fixbb.eps -o  fixbb.pdf ; \
             mv fixbb.pdf $*.pdf ; \
        else epstopdf $*.eps -o $*.pdf ; \
        fi

dvi: $(TARGETNAME).dvi

$(TARGETNAME).bbl : $(TARGETNAME).tex $(BIBFILE).bib
	latex --interaction nonstopmode $(TARGETNAME).tex ; \
            if bibtex $(TARGETNAME); then : ; else rm $(TARGETNAME).bbl ; \
             touch $(TARGETNAME).bbl ; fi

$(TARGETNAME).dvi : $(EPS) $(TARGETNAME).tex $(TARGETNAME).bbl $(TEXFILES)
	if latex $(TARGETNAME).tex ; then \
           if grep "Rerun to get cross-references right" $(TARGETNAME).log ; \
           then latex $(TARGETNAME).tex ; fi ; \
           if grep "Rerun to get cross-references right" $(TARGETNAME).log ; \
           then latex $(TARGETNAME).tex ; fi ; \
           if grep "Rerun to get cross-references right" $(TARGETNAME).log ; \
           then latex $(TARGETNAME).tex ; fi ; \
           if grep "Rerun to get cross-references right" $(TARGETNAME).log ; \
           then latex $(TARGETNAME).tex ; fi ; \
           else echo rm $(TARGETNAME).dvi ;  rm $(TARGETNAME).dvi ; fi

ps: $(TARGETNAME).ps

$(TARGETNAME).ps:  $(TARGETNAME).dvi $(EPS)
#	dvips -Pcmz -Pamz -o $@ $(TARGETNAME).dvi
	dvips -o $@ $(TARGETNAME).dvi

pdf: $(TARGETNAME).pdf $(TARGETNAME).bbl
$(TARGETNAME).pdf : $(PDF) $(TARGETNAME).tex  $(TARGETNAME).bbl $(TEXFILES) $(PNGFILES)
	cat $(TARGETNAME).tex | \
   sed 's/\.eps/.pdf/; s/\.seps/.pdf/ ; s/^ \\newcommand\\pdfpage/% \\newcommand\\pdfpage/ ; s/%\\newcommand\\pdfpage/\\newcommand\\pdfpage/'  \
         > zzz.tex ; cp $(TARGETNAME).bbl zzz.bbl ; \
	if pdflatex zzz.tex ; then \
          if grep "Rerun to get cross-references right" zzz.log ; \
           then pdflatex zzz.tex ; fi ; \
          if grep "Rerun to get cross-references right" zzz.log ; \
           then pdflatex zzz.tex ; fi ; \
          if grep "Rerun to get cross-references right" zzz.log ; \
           then pdflatex zzz.tex ; fi ; \
          mv zzz.pdf $(TARGETNAME).pdf;\
          else echo failed ; fi

tgz: $(TARGETNAME).tgz

$(TARGETNAME).tgz: $(EPSFX) $(FILES) $(TARGETNAME).tex $(BIBFILE).bib
	excluded=`for i in * ; do echo $(FILES) | $(GREP) -q $$i ; \
          if [ $$? != 0 ] ; then echo $$i ; fi ; done` ; \
          if [ "z$$excluded"!=z ] ; then echo " " ; \
          echo 'Excluded files:' $$excluded ; echo " " ; fi
	tar zcvf $(TARGETNAME).tgz  $(TARGETNAME).tex $(FILES) $(EPSFX) $(BIBFILE).bib


hepph: $(TARGETNAME).tex $(TEXFILES) $(EPS)
	tar zcvf hepph.tar.gz $(TARGETNAME).tex $(TEXFILES) $(EPS)
