#-*- Makefile -*-
ifeq ($(shell uname),AIX)
F77=xlf -qextname -qflttrap=overflow:zerodivide:invalid:enable -O3 -qstrict
SYSOBJ=
endif
ifeq ($(shell uname),SunOS)
F77= f77 -fnonstd
SYSOBJ=
endif
ifeq ($(shell uname),Linux)
## -fbounds-check causes a weird error due to non-lazy evaluation of bolean in gfortran
#F77= gfortran -Wall -fno-automatic -ffpe-trap=invalid,zero,overflow -fbounds-check
# The following should no longer be needed in gfortran >= 4.3
#SYSOBJ=
F77= g77 -Wall -fno-automatic 
#-ffortran-bounds-check
DEBUG=-ggdb -pg
#DEBUG=-O3
#OPT=-O3
SYSOBJ=trapfpe.o
endif
ifeq ($(shell uname),Darwin)
F77= g77 -dn -Wall -fno-automatic -ffortran-bounds-check
OPT=-O
#SYSOBJ=trapfpe.o
endif
ifeq ($(shell uname),HP-UX)
F77= g77 -Wall
SYSOBJ=
endif

FASTJET_CONFIG=/home/nason/Pheno/fastjet/bin/fastjet-config

CXXFLAGS += $(shell $(FASTJET_CONFIG) --cxxflags)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins )  -L/usr/lib/gcc/i386-redhat-linux/4.3.0/  -lstdc++
#LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins )  -L/usr/lib/gcc/i386-redhat-linux/4.1.2/  -lstdc++
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)

PWD=$(shell pwd)

VPATH= ./:../:obj/

FF=$(F77) $(OPT) $(DEBUG)


INCLUDE =$(shell echo ../include/*.h *.h)

PDFPACK=mlmpdfif.o hvqpdfpho.o
#PDFPACK=lhapdfif.o
LIBS=-L/home/nason/Pheno/PDFpacks/lib -lLHAPDF -static


%.o: %.f $(INCLUDE)
	pwd
	( cd ../ ; ln -sf $(PWD)/nlegborn.h . )
	$(FF) -c -o obj/$@ $<

%.o: %.c
	$(CC) $(DEBUG) -c -o obj/$@ $^ 

%.o: %.cc
	$(CC) $(DEBUG) -c -o obj/$@ $^ $(FJCXXFLAGS)


USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o \
		      real_ampsq.o pwhg_analysis.o 

PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o LesHouches.o \
	gen_Born_phsp.o find_regions.o test_Sudakov.o \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
	Bornzerodamp.o sigremnants.o random.o boostrot.o \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o  sigreal.o sigcollremn.o \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o newunit.o mwarn.o \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	$(PDFPACK) $(USER) $(SYSOBJ)

PWHGNEW=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o LesHouches.o \
	gen_Born_phsp.o find_regions.o test_Sudakov.o \
	sigborn.o gen_real_phsp.o maxrat.o gen_index.o gen_radiation.o \
	Bornzerodamp.o sigremnants-new.o random.o boostrot.o \
	bra_ket_subroutines.o cernroutines.o init_phys.o powheginput.o \
        pdfcalls.o  sigreal-new.o sigcollremn.o \
        pwhg_bookhist.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o newunit.o mwarn.o \
	sigsoftvirt.o sigcollsoft.o sigvirtual.o \
	fastjetsisconewrap.o fastjetktwrap.o \
	$(PDFPACK) $(USER) $(SYSOBJ)

pwhg_main:$(PWHG)
	$(FF) $(patsubst %,obj/%,$(PWHG)) $(LIBS) $(LIBSFASTJET) -o $@
#	$(FF) $^ -o $@ $(LIBS) $(LIBSFASTJET)

pwhg_main_new:$(PWHGNEW)
	$(FF) $(patsubst %,obj/%,$(PWHGNEW)) $(LIBS) $(LIBSFASTJET) -o $@
#	$(FF) $^ -o $@ $(LIBS) $(LIBSFASTJET)


LHEFANAL=lhef_analysis.o boostrot.o \
	powheginput.o pwhg_bookhist.o pwhg_analysis.o  lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o \
	$(SYSOBJ)

lhef_analysis:$(LHEFANAL)
	$(FF) $(patsubst %,obj/%,$(LHEFANAL)) -o $@ $(LIBS) $(LIBSFASTJET)


# target to read event file, shower them with HERWIG, + generic analysis

HERWIG=main-HERWIG-lhef.o herwig6510.o \
	boostrot.o powheginput.o \
        pwhg_bookhist.o pwhg_analysis.o lhefread.o newunit.o \
	fastjetsisconewrap.o fastjetktwrap.o pwhg_analysis_driver.o $(SYSOBJ)
main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,obj/%,$(HERWIG)) -o $@ $(LIBSFASTJET)

clean:
	rm -f obj/*.o pwhg_main lhef_analysis main-HERWIG-lhef
