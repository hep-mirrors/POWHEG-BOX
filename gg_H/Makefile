#-*- Makefile -*-
ifeq ($(shell uname),Linux)
## -fbounds-check causes a weird error due to non-lazy evaluation
## of boolean in gfortran.
## As of gfortran 4.4.1 optimizing with -O3 yields erroneous results
## Use -O2
F77= gfortran -Wall -fno-automatic -ffpe-trap=invalid,zero,overflow
OPT=-O2
## For g77 comment the above and uncomment below
#F77= g77 -Wall -fno-automatic -ffortran-bounds-check
#OPT=-O3
# For debugging uncomment the following
#DEBUG= -ggdb -pg
#Do not uncomment this with gfortran
#SYSOBJ=trapfpe.o
endif

HERWIG_VERSION=herwig6510
PYTHIA_VERSION=pythia-6.4.21

FASTJET_CONFIG=fastjet-config

CXXFLAGS += $(shell $(FASTJET_CONFIG) --cxxflags)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins )  -L/usr/lib/gcc/i486-linux-gnu/4.3/  -lstdc++
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)

PWD=$(shell pwd)

WDNAME=$(shell basename $(PWD))

VPATH= ./:../:obj/:

FF=$(F77) $(OPT) $(DEBUG)

INCLUDE =$(shell echo ../include/*.h *.h)

#PDFPACK=mlmpdfif.o hvqpdfpho.o
LHAPDF_CONFIG=lhapdf-config
PDFPACK=lhapdfif.o
LIBSLHAPDF=-L$(shell $(LHAPDF_CONFIG) --libdir) -lLHAPDF #-static
LIBS=$(LIBSLHAPDF)
HERWIG_OBJ=$(HERWIG_VERSION).o
PYTHIA_OBJ=$(PYTHIA_VERSION).o

all: pwhg_main

# target to build the modified btilde local version (btilde_ggH)
# in process subdirectory
btilde.o: ../btilde.f $(INCLUDE) 
	ln -sf $(WDNAME)/nlegborn.h ../
	sh make_btilde.sh
	$(FF) -c -o obj/$@ btilde_ggH.f

%.o: %.f $(INCLUDE)
	ln -sf $(WDNAME)/nlegborn.h ../
	$(FF) -c -o obj/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o obj/$@ $^ 

%.o: %.cc
	$(CC) $(DEBUG) -c -o obj/$@ $^ $(FJCXXFLAGS)


USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
        real.o pwhg_analysis.o btilde.o pt2maxreg.o LesHouchesreg.o 

PWHG=pwhg_init.o bbinit.o lhefwrite.o LesHouches.o		       \
	gen_Born_phsp.o find_regions.o test_Sudakov.o		       \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
	Bornzerodamp.o sigremnants.o random.o boostrot.o	       \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o  sigreal.o sigcollremn.o			       \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o	       \
	setstrongcoupl.o integrator.o newunit.o mwarn.o		       \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o		       \
	fastjetsisconewrap.o fastjetktwrap.o			       \
	$(PDFPACK) $(USER) $(SYSOBJ)

# target to generate event file and to analyze LHEF output (also NLO analysis)
pwhg_main:pwhg_main.o  $(PWHG) 
	$(FF) $(patsubst %.o,obj/%.o,$(PWHG) pwhg_main.o)  $(LIBS) $(LIBSFASTJET) -o $@

LHEFANAL=lhef_analysis.o boostrot.o random.o cernroutines.o  opencount.o \
	powheginput.o pwhg_bookhist.o pwhg_analysis.o lhefwrite.o   lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o \
	$(SYSOBJ)

# target to analyze LHEF output
lhef_analysis:$(LHEFANAL)
	$(FF) $(patsubst %,obj/%,$(LHEFANAL)) -o $@ $(LIBS) $(LIBSFASTJET)


# target to read event file, shower them with HERWIG, + generic analysis
HERWIG=main-HERWIG-lhef.o $(HERWIG_OBJ) opencount.o\
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o $(SYSOBJ)
main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,obj/%,$(HERWIG)) -o $@ $(LIBSFASTJET) $(LIBS)

# target to read event file, shower events with PYTHIA + analysis
PYTHIA=main-PYTHIA-lhef.o $(PYTHIA_OBJ) \
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o \
	random.o cernroutines.o opencount.o  $(SYSOBJ)
main-PYTHIA-lhef: $(PYTHIA)
	$(FF) $(patsubst %,obj/%,$(PYTHIA)) -o $@ $(LIBSFASTJET) $(LIBS)
# target to generate events on the fly, shower them with HERWIG + analysis
HERWIG-ONTHEFLY=main-HERWIG.o $(HERWIG_OBJ) $(PWHG)

main-HERWIG: $(HERWIG-ONTHEFLY)
	$(FF) $(patsubst %,obj/%,$(HERWIG-ONTHEFLY)) -o $@ $(LIBSFASTJET) $(LIBS)

# target to generate events on the fly,, shower events with PYTHIA + analysis
PYTHIA-ONTHEFLY=main-PYTHIA.o $(PYTHIA_OBJ) $(PWHG)

main-PYTHIA: $(PYTHIA-ONTHEFLY)
	$(FF) $(patsubst %,obj/%,$(PYTHIA-ONTHEFLY)) -o $@ $(LIBSFASTJET) $(LIBS)

# target to cleanup
clean:
	rm -f *~ obj/*.o pwhg_main lhef_analysis main-HERWIG-lhef main-PYTHIA-lhef \
        main-HERWIG main-PYTHIA

.PHONY: clean 


