#-*- Makefile -*-
ifeq ($(shell uname),Linux)
## gfortran compiler
## -fbounds-check causes a weird error due to non-lazy 
## evaluation of bolean in gfortran
## As of gfortran 4.4.1 optimizing with -O3 yields erroneous results
F77= gfortran -Wall -fno-automatic -ffixed-line-length-132 #-ffpe-trap=invalid,zero,overflow
## g77 compiler
#F77= g77 -Wall -fno-automatic -ffixed-line-length-132 -ffortran-bounds-check
endif

SYSOBJ= #trapfpe.o
DEBUG= #-ggdb #-pg
OPT= -O2

HERWIG_VERSION=herwig6510
PYTHIA_VERSION=pythia-6.4.22

FASTJET_CONFIG=fastjet-config

CXXFLAGS += $(shell $(FASTJET_CONFIG) --cxxflags)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins )  -L/usr/lib/gcc/i486-linux-gnu/4.3/  -lstdc++
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)

PWD=$(shell pwd)

WDNAME=$(shell basename $(PWD))

VPATH= ./:../:obj/:madgraph/:madgraph/ME_decayed:QCDLoop-1.9/ql:QCDLoop-1.9/ff

FF=$(F77) $(OPT) $(DEBUG)

INCLUDE =$(shell echo ../include/*.h *.h)

#PDFPACK=mlmpdfif.o hvqpdfpho.o
LHAPDF_CONFIG=lhapdf-config
PDFPACK=lhapdfif.o
LIBSLHAPDF=-L$(shell $(LHAPDF_CONFIG) --prefix)//lib -lLHAPDF -static
LIBS=$(LIBSLHAPDF)
LIBSMADGRAPH=madgraph/dhelas3.2/libdhelas3.2.a
LIBS += $(LIBSMADGRAPH)
LIBSQCDLOOP= QCDLoop-1.9/ql/libqcdloop.a QCDLoop-1.9/ff/libff.a
LIBS += $(LIBSQCDLOOP)


HERWIG_OBJ=$(HERWIG_VERSION).o
PYTHIA_OBJ=$(PYTHIA_VERSION).o

%.o: %.f $(INCLUDE)
	ln -sf $(WDNAME)/nlegborn.h ../
	$(FF) -c -o obj/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o obj/$@ $^ 

%.o: %.cc
	$(CC) $(DEBUG) -c -o obj/$@ $^ $(FJCXXFLAGS)


USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
	real.o pwhg_analysis.o bmunu2.o boost.o $(MADGRAPH)

##########
# to compile madgraph:
# libdhelas3.2a added above to the list of linked libraries
# also VPATH contains directories relevant to link madgraph
MADGRAPH= $(LIBSMADGRAPH) my_setpara.o switchmom.o \
	MAD_born_routines.o MAD_real_routines_DR.o \
	MAD_born_undec_routines.o MAD_real_undec_routines_DR.o \
	MAD_born_dec_routines.o MAD_real_dec_routines.o #printout.o
################

#############
# to link QCDloops object:
# libqcdloop.a and libff.a added above to the list of linked libraries
# also VPATH contains directories relevant to link QCDloop  
#############

PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o LesHouches.o \
	LesHouchesreg.o gen_Born_phsp.o find_regions.o test_Sudakov.o pt2maxreg.o \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
	Bornzerodamp.o sigremnants.o random.o boostrot.o \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o sigreal.o sigcollremn.o \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o newunit.o mwarn.o \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o reshufflemoms.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	$(PDFPACK) $(USER) $(SYSOBJ)

#  target to generate event file and to analyze LHEF output (also NLO analysis)
pwhg_main:$(PWHG)
	$(FF) $(patsubst %.o,obj/%.o,$(PWHG)) $(LIBS) $(LIBSFASTJET) $(LIBSMADGRAPH) $(LIBSQCDLOOP) -o $@

LHEFANAL=lhef_analysis.o boostrot.o opencount.o \
	powheginput.o pwhg_bookhist.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	pwhg_analysis_driver.o pwhg_analysis.o boost.o \
	$(SYSOBJ)

# target to analyze LHEF output
lhef_analysis:$(LHEFANAL)
	$(FF) $(patsubst %,obj/%,$(LHEFANAL)) -o $@ $(LIBS) $(LIBSFASTJET)


# target to read event file, shower them with HERWIG, + generic analysis

HERWIG=main-HERWIG-lhef.o $(HERWIG_OBJ) boostrot.o opencount.o \
	powheginput.o pwhg_bookhist.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	pwhg_analysis_driver.o pwhg_analysis.o boost.o \
	$(SYSOBJ)

# target to read event file, shower them with HERWIG, + generic analysis
main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,obj/%,$(HERWIG)) -o $@ $(LIBSFASTJET) $(LIBS)

PYTHIA=main-PYTHIA-lhef.o $(PYTHIA_OBJ) boostrot.o opencount.o \
	powheginput.o pwhg_bookhist.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	pwhg_analysis_driver.o pwhg_analysis.o boost.o \
	$(SYSOBJ)

# target to read event file, shower events with PYTHIA + analysis
main-PYTHIA-lhef: $(PYTHIA)
	$(FF) $(patsubst %,obj/%,$(PYTHIA)) -o $@ $(LIBSFASTJET) $(LIBS)



###### to compile dhelas3.2
$(LIBSMADGRAPH): force_look
	@echo looking into subdir madgraph/dhelas3.2
	$(MAKE) FC='$(F77)' --directory=madgraph/dhelas3.2
force_look :
	true
######



# target to cleanup
.PHONY: clean
clean:
	rm -f obj/*.o madgraph/dhelas3.2/*.o madgraph/dhelas3.2/*.a \
	pwhg_main lhef_analysis \
	main-HERWIG-lhef main-PYTHIA-lhef \
	merge_t_tb

MERGE_t_tb=merge_t_tb.o newunit.o random.o cernroutines.o

#  target to merge t and tbar samples
merge_t_tb: $(MERGE_t_tb)
	$(FF) $(patsubst %.o,obj/%.o,$(MERGE_t_tb)) -o $@

