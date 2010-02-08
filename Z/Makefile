#-*- Makefile -*-
ifeq ($(shell uname),Linux)
## -fbounds-check causes a weird error due to non-lazy evaluation
## of boolean in gfortran.
## As of gfortran 4.4.1 optimizing with -O3 yields erroneous results
F77= gfortran -Wall -fno-automatic -ffpe-trap=invalid,zero,overflow
## For g77 comment the above and uncomment below
#F77= g77 -Wall -fno-automatic -ffortran-bounds-check
# For debugging uncomment the following
#DEBUG= -ggdb -pg
#Do not uncomment this with gfortran
#OPT=-O3
#SYSOBJ=trapfpe.o
endif

FASTJET_CONFIG=/home/nason/bin/fastjet-config

CXXFLAGS += $(shell $(FASTJET_CONFIG) --cxxflags)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins )  -L/usr/lib/gcc/x86_64-linux-gnu/4.4/  -lstdc++
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)

PWD=$(shell pwd)

WDNAME=$(shell basename $(PWD))

VPATH= ./:../:obj/

FF=$(F77) $(OPT) $(DEBUG)


INCLUDE =$(shell echo ../include/*.h *.h)

PDFPACK=lhapdfif.o
LIBS=-L/home/nason/Pheno/PDFpacks/lha-5.8.1/lib -lLHAPDF -static
# In order to use mlmpdf uncomment the following and comment previous lines
# PDFPACK=mlmpdfif.o hvqpdfpho.o


%.o: %.f $(INCLUDE)
	ln -sf $(WDNAME)/nlegborn.h ../
	$(FF) -c -o obj/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o obj/$@ $^ 

%.o: %.cc
	$(CC) $(DEBUG) -c -o obj/$@ $^ $(FJCXXFLAGS)


USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
		      real_ampsq.o pwhg_analysis.o 

PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o LesHouches.o \
	LesHouchesReg.o gen_Born_phsp.o find_regions.o test_Sudakov.o pt2maxreg.o \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
	Bornzerodamp.o sigremnants.o random.o boostrot.o \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o  sigreal.o sigcollremn.o \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o newunit.o mwarn.o \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	$(PDFPACK) $(USER) $(SYSOBJ)

MADGRAPH=fastjetsisconewrap.o fastjetktwrap.o cbsb_hsbcb.o cbu_hsbd.o \
	bbbb_hbbbb.o bbcb_hbbcb.o bbd_hbbd.o dd_hdd.o ud_hud.o \
	sbd_hsbd.o sd_hsd.o udb_hudb.o cu_hcu.o ucb_hucb.o \
	cbsb_hsbcbg.o cbu_hsbdg.o cs_hscg.o 

# target to generate LHEF output
pwhg_main:$(PWHG)
	$(FF) $(patsubst %,obj/%,$(PWHG)) $(LIBS) $(LIBSFASTJET) -o $@

LHEFANAL=lhef_analysis.o boostrot.o random.o cernroutines.o  opencount.o \
	powheginput.o pwhg_bookhist.o pwhg_analysis.o lhefwrite.o   lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o \
	$(SYSOBJ)

# target to analyze LHEF output
lhef_analysis:$(LHEFANAL)
	$(FF) $(patsubst %,obj/%,$(LHEFANAL)) -o $@ $(LIBS) $(LIBSFASTJET)


# target to read event file, shower them with HERWIG, + generic analysis
HERWIG=main-HERWIG-lhef.o herwig6510.o \
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o $(SYSOBJ)
main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,obj/%,$(HERWIG)) -o $@ $(LIBSFASTJET)

# target to read event file, shower events with PYTHIA + analysis
PYTHIA=main-PYTHIA-lhef.o pythia-6.4.21.o \
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o \
	random.o cernroutines.o opencount.o  $(SYSOBJ)
main-PYTHIA-lhef: $(PYTHIA)
	$(FF) $(patsubst %,obj/%,$(PYTHIA)) -o $@ $(LIBSFASTJET) $(LIBS)

clean:
	rm -f obj/*.o pwhg_main lhef_analysis main-HERWIG-lhef
