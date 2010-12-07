#-*- Makefile -*-
ifeq ($(shell uname),Linux)
## -fbounds-check causes a weird error due to non-lazy evaluation
## of boolean in gfortran.
## As of gfortran 4.4.1 optimizing with -O3 yields erroneous results
## Use -O2
F77= gfortran -Wall -fno-automatic #-ffpe-trap=invalid,zero,overflow,underflow 
#-fbounds-check
OPT=-O2
# For g77 comment the above and uncomment below
#F77= g77 -Wall -fno-automatic  -ffortran-bounds-check
#OPT=-O3
# For debugging uncomment the following
#DEBUG= -ggdb -pg
#Do not uncomment this with gfortran
#SYSOBJ=trapfpe.o
# For ifort
#F77=ifort -r8 -assume bscc -save -extend_source #-fpe0
#CXX=icpc
#CC=icc
#OPT= -O3
#LIBS = -limf
#DEBUG= -check -debug -g
endif

FASTJET_CONFIG=fastjet-config

CXXFLAGS += $(shell $(FASTJET_CONFIG) --cxxflags)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) -lstdc++
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)

PWD=$(shell pwd)

WDNAME=$(shell basename $(PWD))

VPATH= ./:../:obj/: 
#VPATH+=madgraph/:madgraph/real/

FF=$(F77) $(OPT) $(DEBUG)

INCLUDE =$(shell echo ../include/*.h *.h)
#INCLUDE =$(shell echo ../include/*.h *.h madgraph/*.inc)


LHAPDF_CONFIG=lhapdf-config
PDFPACK=lhapdfif.o
LIBSLHAPDF=-L$(shell $(LHAPDF_CONFIG) --libdir) -lLHAPDF 
LIBS=$(LIBSLHAPDF)
# In order to use mlmpdf uncomment the following and comment previous lines
# PDFPACK=mlmpdfif.o hvqpdfpho.o

# Uncomment the following with the appropriate location of libstdc++.a for static link
#STATIC= -L /usr/lib/gcc/x86_64-linux-gnu/4.4/ -lstdc++ -static

LIBSMADGRAPH=madgraph/DHELAS/libdhelas3.2.a
#LIBS += $(LIBSMADGRAPH)

%.o: %.f $(INCLUDE)
	ln -sf $(WDNAME)/nlegborn.h ../
	$(FF) -c -o obj/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o obj/$@ $^ 

%.o: %.cc
	$(CC) $(DEBUG) -c -o obj/$@ $^ $(FJCXXFLAGS)



#MADGRAPH=$(shell echo madgraph/real/*.f | sed -e 's/.f/.o/g;s/madgraph\/real\///g') 
#MADGRAPH+=functions.o setparam.o $(LIBSMADGRAPH) 

USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
		      real.o $(MADGRAPH) pwhg_analysis_CDF.o # pwhg_analysis.o

PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o LesHouches.o \
	LesHouchesreg.o gen_Born_phsp.o find_regions.o test_Sudakov.o pt2maxreg.o \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
	Bornzerodamp.o sigremnants.o random.o boostrot.o \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o  sigreal.o sigcollremn.o \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o newunit.o mwarn.o \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o \
	$(PDFPACK) $(USER) $(SYSOBJ) \
	fastjetCDFJetCluwrap.o


pwhg_main:$(PWHG) 
	$(FF) $(patsubst %.o,obj/%.o,$(PWHG)) $(LIBS) $(LIBSFASTJET) -o $@

LHEFANAL=lhef_analysis.o boostrot.o random.o cernroutines.o  opencount.o \
	powheginput.o pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o  fastjetCDFMidPointwrap.o\
	fastjetD0RunIIConewrap.o pwhg_analysis_driver.o \
	$(SYSOBJ)

# target to analyze LHEF output
lhef_analysis:$(LHEFANAL)
	$(FF) $(patsubst %,obj/%,$(LHEFANAL)) -o $@ $(LIBS) $(LIBSFASTJET)


# target to read event file, shower events with HERWIG + analysis
HERWIG=main-HERWIG-lhef.o herwig6510.o \
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o  fastjetCDFMidPointwrap.o\
	fastjetD0RunIIConewrap.o pwhg_analysis_driver.o \
	random.o cernroutines.o opencount.o  $(SYSOBJ)
main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,obj/%,$(HERWIG)) -o $@ $(LIBSFASTJET)


# target to read event file, shower events with PYTHIA + analysis
PYTHIA=main-PYTHIA-lhef.o pythia-6.4.21.o \
	boostrot.o powheginput.o \
        pwhg_bookhist.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o \
	random.o cernroutines.o opencount.o $(SYSOBJ) \
	pwhg_analysis_CDF.o fastjetCDFJetCluwrap.o

main-PYTHIA-lhef: $(PYTHIA)
	$(FF) $(patsubst %,obj/%,$(PYTHIA)) -o $@ $(LIBSFASTJET) $(LIBS)



clean:
	rm -f obj/*.o madgraph/DHELAS/*.o madgraph/DHELAS/*.a \
	pwhg_main lhef_analysis main-HERWIG-lhef \
	main-PYTHIA-lhef 

###### to compile dhelas3.2
$(LIBSMADGRAPH): force_look
	@echo looking into subdir madgraph/DHELAS
	$(MAKE) FC='$(F77)' --directory=madgraph/DHELAS 
force_look :
	true
######