#-*- Makefile -*-

FC=gfortran
#FC=ifort

ifeq ($(FC),ifort)
IFORT=/opt/intel/Compiler/11.1/072/bin/intel64/ifort -save
IFORTLIBS=  -L /opt/intel/Compiler/11.1/072/lib/intel64/ -lifport -lintlc -lifcore -limf -lsvml -lirc
F77= $(IFORT) #  -save -ftrapuv -fp-stack-check -DD -debug all 
OPT=-O2
endif

ifeq ($(FC),gfortran)
F77= gfortran -Wall -fno-automatic # -ffpe-trap=invalid,zero,overflow
OPT=-O2
#DEBUG=-ggdb
endif

FASTJET_CONFIG=fastjet-config

CXXFLAGS += $(shell $(FASTJET_CONFIG) --cxxflags)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins )

FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)

PWD=$(shell pwd)

WDNAME=$(shell basename $(PWD))

VPATH= ./:../:obj-$(FC)/

FF=$(F77) $(OPT) $(DEBUG)  -I F90 -I F90/obj-$(FC)


INCLUDE =$(shell echo ../include/*.h *.h)

PDFPACK=lhapdfif.o
# LHAPDF may be compiled with ifort or gfortran. The user should
# set up the correct path.
LHAPDFCONFIG=~/Pheno/PDFpacks/lhapdf-5.8.4-$(FC)/bin/lhapdf-config
LHAPDFLIB=-L$(shell $(LHAPDFCONFIG) --libdir) -lLHAPDF

# In order to use mlmpdf uncomment the following and comment previous lines
# PDFPACK=mlmpdfif.o hvqpdfpho.o

QCDLOOP=QCDLoop-1.9-$(FC)

LIBS=$(LHAPDFLIB) -LF90/obj-$(FC) -lWpWp2j -L$(QCDLOOP)/ql -L$(QCDLOOP)/ff -lqcdloop -lff


%.o: %.f $(INCLUDE)
	ln -sf $(WDNAME)/nlegborn.h ../
	$(FF) -c -o obj-$(FC)/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o obj-$(FC)/$@ $^ 

%.o: %.cc
	$(CC) $(DEBUG) -c -o obj-$(FC)/$@ $^ $(FJCXXFLAGS)


USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
		      real.o pwhg_analysis.o \
			boost.o phi1_2.o phi3m0.o breit.o 

PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o LesHouches.o \
	LesHouchesreg.o gen_Born_phsp.o find_regions.o test_Sudakov.o \
        pt2maxreg.o mint_upb.o \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
	Bornzerodamp.o sigremnants.o random.o boostrot.o \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o  sigreal.o sigcollremn.o \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o newunit.o mwarn.o \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o \
	fastjetfortran.o \
	$(PDFPACK) $(USER) $(SYSOBJ)


F90/obj-$(FC)/libWpWp2j.a:F90/*.f* F90/$(FC)/*.f*
	(cd F90 ; make FC=$(FC))

$(QCDLOOP)/ql/libqcdloop.a:
	(cd $(QCDLOOP)/ql; make FC=$(FC))

$(QCDLOOP)/ff/libff.a:
	(cd $(QCDLOOP)/ff; make FC=$(FC))
# target to generate LHEF output
# Order of dependencies is important: libWpWp2j.a must be before $(PWHG),
pwhg_main: F90/obj-$(FC)/libWpWp2j.a $(QCDLOOP)/ql/libqcdloop.a $(QCDLOOP)/ff/libff.a  $(PWHG)
	$(FF) $(patsubst %,obj-$(FC)/%,$(PWHG)) $(LIBS) $(LIBSFASTJET) -o $@ $(IFORTLIBS) $(STATIC)


LHEFANAL=lhef_analysis.o boostrot.o random.o cernroutines.o  opencount.o \
	powheginput.o pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetfortran.o pwhg_analysis_driver.o \
	$(SYSOBJ)


# target to analyze LHEF output
lhef_analysis:$(LHEFANAL)
	$(FF) $(patsubst %,obj-$(FC)/%,$(LHEFANAL)) -o $@ $(LIBSFASTJET)


# target to read event file, shower events with HERWIG + analysis
HERWIG=main-HERWIG-lhef.o herwig6510.o \
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetfortran.o pwhg_analysis_driver.o \
	random.o cernroutines.o opencount.o pdfdummies.o $(SYSOBJ)


main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,obj-$(FC)/%,$(HERWIG)) -o $@ $(LIBSFASTJET)

# target to read event file, shower events with PYTHIA + analysis
PYTHIA=main-PYTHIA-lhef.o pythia-6.4.21.o \
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetfortran.o pwhg_analysis_driver.o \
	random.o cernroutines.o opencount.o  pdfdummies.o $(SYSOBJ)


main-PYTHIA-lhef: $(PYTHIA)
	$(FF) $(patsubst %,obj-$(FC)/%,$(PYTHIA)) -o $@ $(LIBSFASTJET)

clean:
	cd QCDLoop-1.9-$(FC); make clean ; cd ql ; make clean ; cd ../ff/ ; make clean
	cd F90 ; make FC=$(FC) clean
	rm -f obj-$(FC)/*.o pwhg_main lhef_analysis main-HERWIG-lhef main-PYTHIA-lhef
