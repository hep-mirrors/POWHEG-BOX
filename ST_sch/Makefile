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

VPATH= ./:../:obj/:madgraph/:madgraph/ME_undecayed/:madgraph/ME_decayed

FF=$(F77) $(OPT) $(DEBUG)

INCLUDE =$(shell echo ../include/*.h *.h)

#PDFPACK=mlmpdfif.o hvqpdfpho.o
LHAPDF_CONFIG=lhapdf-config
PDFPACK=lhapdfif.o
LIBSLHAPDF=-L$(shell $(LHAPDF_CONFIG) --prefix)//lib -lLHAPDF -static
LIBS=$(LIBSLHAPDF)
LIBSMADGRAPH=madgraph/dhelas3.2/libdhelas3.2.a
LIBS += $(LIBSMADGRAPH)

HERWIG_OBJ=$(HERWIG_VERSION).o
PYTHIA_OBJ=$(PYTHIA_VERSION).o

%.o: %.f $(INCLUDE)
	ln -sf $(WDNAME)/nlegborn.h ../
	$(FF) -c -o obj/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o obj/$@ $^ 

%.o: %.cc
	$(CC) $(DEBUG) -c -o obj/$@ $^ $(FJCXXFLAGS)


##########
# to compile madgraph:
# libdhelas3.2a added above to the list of linked libraries
# also VPATH contains directories relevant to link madgraph
MADGRAPH= $(LIBSMADGRAPH) my_setpara.o switchmom.o printout.o \
	bdx_tux.o bu_td.o dxb_tux.o dxu_tbx.o ub_td.o udx_tbx.o \
	udx_tbxg.o ub_tdg.o  gb_tuxd_T.o \
	dxu_tbxg.o  dxb_tuxg.o bu_tdg.o bg_tuxd_T.o \
	bdx_tuxg.o \
	gdx_tuxbx_S.o gdx_tuxbx_T.o gu_tdbx_S.o gu_tdbx_T.o \
	dxg_tuxbx_S.o dxg_tuxbx_T.o ug_tdbx_S.o ug_tdbx_T.o \
	bdx_epvebux.o dxb_epvebux.o ub_epvebd.o \
	bu_epvebd.o dxu_epvebbx.o udx_epvebbx.o \
	bdx_epvebuxg.o dxb_epvebuxg.o gb_epvebuxd_T.o udx_epvebbxg.o \
	bg_epvebuxd_T.o dxu_epvebbxg.o      \
	bu_epvebdg.o  ub_epvebdg.o \
	gdx_epvebuxbx_S.o gu_epvebdbx_S.o dxg_epvebuxbx_S.o ug_epvebdbx_S.o \
	gdx_epvebuxbx_T.o gu_epvebdbx_T.o dxg_epvebuxbx_T.o ug_epvebdbx_T.o
################

USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
	real.o pwhg_analysis.o boost.o $(MADGRAPH)

PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o LesHouches.o \
	LesHouchesreg.o gen_Born_phsp.o find_regions.o test_Sudakov.o \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
        pt2maxreg.o Bornzerodamp.o sigremnants.o random.o boostrot.o \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o  sigreal.o sigcollremn.o \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o newunit.o mwarn.o \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	$(PDFPACK) $(USER) $(SYSOBJ)

#  target to generate event file and to analyze LHEF output (also NLO analysis)
pwhg_main: $(PWHG)
	$(FF) $(patsubst %.o,obj/%.o, $(PWHG)) $(LIBS) $(LIBSFASTJET) $(LIBSMADGRAPH) -o $@

LHEFANAL=lhef_analysis.o boostrot.o opencount.o \
	powheginput.o pwhg_bookhist.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	pwhg_analysis_driver.o pwhg_analysis.o boost.o \
	$(SYSOBJ)

# target to analyze LHEF output 
lhef_analysis:$(LHEFANAL)
	$(FF) $(patsubst %,obj/%,$(LHEFANAL)) -o $@ $(LIBS) $(LIBSFASTJET)

HERWIG=main-HERWIG-lhef.o $(HERWIG_OBJ) boostrot.o opencount.o \
	powheginput.o pwhg_bookhist.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	pwhg_analysis_driver.o pwhg_analysis.o boost.o \
	random.o cernroutines.o \
	$(SYSOBJ)

# target to read event file, shower them with HERWIG, + generic analysis
main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,obj/%,$(HERWIG)) -o $@ $(LIBSFASTJET) $(LIBS)

PYTHIA=main-PYTHIA-lhef.o $(PYTHIA_OBJ) boostrot.o opencount.o \
	powheginput.o pwhg_bookhist.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	pwhg_analysis_driver.o pwhg_analysis.o boost.o \
	random.o cernroutines.o \
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
        main-HERWIG main-PYTHIA



