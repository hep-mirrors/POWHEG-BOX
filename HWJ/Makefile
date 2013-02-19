#-*- Makefile -*-
## Choose compiler: gfortran,ifort (g77 not supported, F90 constructs in use!)
COMPILER=ifort
FC=$(COMPILER)
## Choose PDF: native,lhapdf
## LHAPDF package has to be installed separately
PDF=lhapdf
#Choose Analysis: dummy, HWnJ
## default analysis may require FASTJET package, that has to be installed separately (see below)
ANALYSIS=HWnJ
## For static linking uncomment the following
#STATIC= -static
#



OBJ=obj-$(COMPILER)
OBJDIR:=$(OBJ)

ifeq ("$(COMPILER)","gfortran")	
F77=gfortran -fno-automatic -J$(OBJ) -I$(OBJ)
## -fbounds-check sometimes causes a weird error due to non-lazy evaluation
## of boolean in gfortran.
#FFLAGS= -Wall -Wimplicit-interface -fbounds-check
## For floating point exception trapping  uncomment the following 
#FPE=-ffpe-trap=invalid,zero,overflow,underflow 
## gfortran 4.4.1 optimized with -O3 yields erroneous results
## Use -O2 to be on the safe side
OPT=-O2
## For debugging uncomment the following
#DEBUG= -ggdb 
ifdef DEBUG
OPT=-O0
FPE=-ffpe-trap=invalid,zero,overflow,underflow
endif
endif



ifeq ("$(COMPILER)","ifort")
F77 = ifort -save  -extend_source  -module $(OBJ)
#CXX = g++
#LIBS = -limf
#FFLAGS =  -checkm
## For floating point exception trapping  uncomment the following 
#FPE = -fpe0
OPT = -O3 #-fast
## For debugging uncomment the following
#DEBUG= -debug -g
ifdef DEBUG
OPT=-O0 
FPE = -fpe0
endif
endif



GOSAMLIBS=$(shell cd GoSamlib_new ; echo *.f *.f90 ' ' | sed 's/qlonshellcutoff.f// ; s/qlconstants.f// ; s/.f /.o /g ; s/.f90 /.o /g ; ' | tee GoSamlib.txt)


PWD=$(shell pwd)
WDNAME=$(shell basename $(PWD))
VPATH= ./:../Version-pre2-1/progress:../Version-pre2-1/:../:$(OBJDIR):./GoSamlib_new/

INCLUDE0=$(PWD)
INCLUDE1=$(PWD)/include
INCLUDE1.2=$(shell dirname $(PWD))/Version-pre2-1/include
INCLUDE2=$(shell dirname $(PWD))/include
FF=$(F77) $(FFLAGS) $(FPE) $(OPT) $(DEBUG) -I$(INCLUDE0) -I$(INCLUDE1) -I$(INCLUDE1.2) -I$(INCLUDE2)


INCLUDE =$(wildcard ../include/*.h *.h include/*.h)

ifeq ("$(PDF)","lhapdf")
LHAPDF_CONFIG=lhapdf-config
PDFPACK=lhapdfif.o
LIBSLHAPDF= -Wl,-rpath,$(shell $(LHAPDF_CONFIG) --libdir)  -L$(shell $(LHAPDF_CONFIG) --libdir) -lLHAPDF
ifeq  ("$(STATIC)","-static") 
## If LHAPDF has been compiled with gfortran and you want to link it statically, you have to include
## libgfortran as well. The same holds for libstdc++. 
## One possible solution is to use fastjet, since $(shell $(FASTJET_CONFIG) --libs --plugins ) -lstdc++
## does perform this inclusion. The path has to be set by the user. 
# LIBGFORTRANPATH= #/usr/lib/gcc/x86_64-redhat-linux/4.1.2
# LIBSTDCPP=/lib64
 LIBSLHAPDF+=  -L$(LIBGFORTRANPATH)  -lgfortranbegin -lgfortran -L$(LIBSTDCPP) -lstdc++
endif
LIBS+=$(LIBSLHAPDF)
else
PDFPACK=mlmpdfif.o hvqpdfpho.o
endif


ifeq ("$(ANALYSIS)","HWnJ")
##To include Fastjet configuration uncomment the following lines. 
FASTJET_CONFIG=$(shell which fastjet-config)
LIBSFASTJET += $(shell $(FASTJET_CONFIG) --libs --plugins ) -lstdc++
FJCXXFLAGS+= $(shell $(FASTJET_CONFIG) --cxxflags)
PWHGANAL=pwhg_bookhist-new.o pwhg_analysis-HWnJ.o
## Also add required Fastjet drivers to PWHGANAL (examples are reported)
#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o fastjetCDFMidPointwrap.o fastjetD0RunIIConewrap.o fastjetfortran.o
#PWHGANAL+= fastjetsisconewrap.o fastjetktwrap.o  fastjetfortran.o
PWHGANAL+=  fastjetfortran.o
else
PWHGANAL=pwhg_analysis-dummy.o
endif


%.o: %.f $(INCLUDE) | $(OBJDIR)
	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.f90 $(INCLUDE) | $(OBJDIR)
	$(FF) -c -o $(OBJ)/$@ $<

%.o: %.c | $(OBJDIR)
	$(CC) $(DEBUG) -c -o $(OBJ)/$@ $^ 

%.o: %.cc | $(OBJDIR)
	$(CXX) $(DEBUG) -c -o $(OBJ)/$@ $^ $(FJCXXFLAGS)

USER=init_couplings.o init_processes.o Born_phsp.o Born.o virtual.o	\
     real.o  phi1_2.o phi1_2m_nobw_ktmin.o phi3m0.o phi3m.o boost.o breit.o $(PWHGANAL)


PWHG=pwhg_main.o pwhg_init.o bbinit.o btilde.o lhefwrite.o		\
	LesHouches.o LesHouchesreg.o gen_Born_phsp.o find_regions.o	\
	test_Sudakov.o pt2maxreg.o sigborn.o gen_real_phsp.o maxrat.o	\
	gen_index.o gen_radiation.o Bornzerodamp.o sigremnants.o	\
	random.o boostrot.o bra_ket_subroutines.o cernroutines.o	\
	init_phys.o powheginput.o pdfcalls.o sigreal.o sigcollremn.o	\
	pwhg_analysis_driver.o checkmomzero.o		                \
	setstrongcoupl.o integrator.o newunit.o mwarn.o sigsoftvirt.o	\
	reshufflemoms.o  pwhg_gosam.o                                   \
	sigcollsoft.o sigvirtual.o  ubprojections.o                     \
	pwhgreweight.o setlocalscales.o mint_upb.o opencount.o          \
	validflav.o $(PDFPACK) $(USER) $(FPEOBJ)

LIBDIRMG=.
LINKMGLIBS =  -L$(LIBDIRMG)  -lmadgraph -lmodel -ldhelas3 

MADLIBS=libdhelas3.a libmadgraph.a libmodel.a

# target to generate LHEF output
pwhg_main:gosamlib.a $(PWHG) $(MADLIBS)
	$(FF) $(patsubst %,$(OBJ)/%,$(PWHG)) $(LINKMGLIBS) $(LIBS) $(LIBSFASTJET) $(STATIC)  $(OBJ)/gosamlib.a -o $@

gosamlib.a: $(GOSAMLIBS)
	 echo $(GOSAMLIBS)>GoSamlib.lst ; cd $(OBJ) ; \rm gosamlib.a ; ar cru gosamlib.a $(GOSAMLIBS)


LHEF=lhef_analysis.o boostrot.o random.o cernroutines.o		\
     opencount.o powheginput.o $(PWHGANAL)	\
     lhefread.o newunit.o pwhg_analysis_driver.o $(FPEOBJ)

# target to analyze LHEF output
lhef_analysis:$(LHEF)
	$(FF) $(patsubst %,$(OBJ)/%,$(LHEF)) $(LIBS) $(LIBSFASTJET) $(STATIC)  -o $@ 



# target to read event file, shower events with HERWIG + analysis
HERWIG=main-HERWIG.o setup-HERWIG-lhef.o herwig.o boostrot.o	\
	powheginput.o $(PWHGANAL) lhefread.o	\
	pdfdummies.o opencount.o $(FPEOBJ) 

main-HERWIG-lhef: $(HERWIG)
	$(FF) $(patsubst %,$(OBJ)/%,$(HERWIG))  $(LIBSFASTJET)  $(STATIC) -o $@

# target to read event file, shower events with PYTHIA + analysis
PYTHIA=main-PYTHIA.o setup-PYTHIA-lhef.o pythia.o boostrot.o powheginput.o \
	$(PWHGANAL) lhefread.o newunit.o 	\
	pwhg_analysis_driver.o random.o cernroutines.o opencount.o	\
	$(FPEOBJ)

main-PYTHIA-lhef: $(PYTHIA)
	$(FF) $(patsubst %,$(OBJ)/%,$(PYTHIA))  $(LIBS) $(LIBSFASTJET)  $(STATIC) -o $@

# target to cleanup
.PHONY: clean libdhelas3.a libmadgraph.a libmodel.a

XFFLAGS=$(DEBUG) $(OPT)
libdhelas3.a:
	cd DHELAS ; make FC="$(F77)" F77="$(F77)" XFFLAGS="$(XFFLAGS)"

libmadgraph.a:
	cd Madlib ; make FC="$(F77)" F77="$(F77)" XFFLAGS"=$(XFFLAGS)"

ifeq ("$(COMPILER)","gfortran")
XFFLAGS +=-ffixed-line-length-132
else
XFFLAGS +=-extend-source
endif

libmodel.a:
	cd MODEL ; make FC="$(F77)" F77="$(F77)" XFFLAGS="$(XFFLAGS)"


clean:
	rm -f $(patsubst %,$(OBJ)/%,$(USER) $(PWHG) $(LHEF) $(HERWIG) $(PYTHIA)) pwhg_main lhef_analysis main-HERWIG-lhef	\
	main-PYTHIA-lhef


veryclean:
	rm -f $(OBJ)/*.o $(OBJ)/*.mod $(OBJ)/*.a pwhg_main lhef_analysis main-HERWIG-lhef	\
	main-PYTHIA-lhef *.a DHELAS/*.o Madlib/*.o MODEL/*.o GoSamlib.lst

# target to generate object directory if it does not exist
$(OBJDIR):
	mkdir -p $(OBJDIR)

##########################################################################
array.o : array.f90 
avh_olo_boxc.o : avh_olo_boxc.f90 avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_box.o avh_olo_func.o avh_olo_units.o avh_olo_kinds.o 
avh_olo_box.o : avh_olo_box.f90 avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_units.o avh_olo_kinds.o 
avh_olo_bub.o : avh_olo_bub.f90 avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_units.o avh_olo_kinds.o 
avh_olo_func.o : avh_olo_func.f90 avh_olo_units.o avh_olo_kinds.o 
avh_olo_kinds.o : avh_olo_kinds.f90 avh_olo_xkind.o 
avh_olo_main.o : avh_olo_main.f90 avh_olo_d0_b.h90 avh_olo_d0_a.h90 avh_olo_c0_b.h90 avh_olo_c0_a.h90 avh_olo_b11_b.h90 avh_olo_b11_a.h90 avh_olo_b0_b.h90 avh_olo_b0_a.h90 avh_olo_a0_b.h90 avh_olo_a0_a.h90 avh_olo_box.o avh_olo_tri.o avh_olo_boxc.o avh_olo_bub.o avh_olo_func.o avh_olo_func.o avh_olo_print.o avh_olo_units.o avh_olo_kinds.o 
avh_olo_print.o : avh_olo_print.f90 avh_olo_kinds.o 
avh_olo_tri.o : avh_olo_tri.f90 avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_func.o avh_olo_units.o avh_olo_kinds.o 
avh_olo_units.o : avh_olo_units.f90 
avh_olo_wrp01.o : avh_olo_wrp01.f90 avh_olo_main.o 
avh_olo_xkind.o : avh_olo_xkind.f90 
cache.o : cache.f90 sortie_erreur.o precision_golem.o 
constante.o : constante.f90 precision_golem.o 
constants.o : constants.f90 precision.o 
equal.o : equal.f90 sortie_erreur.o constante.o parametre.o precision_golem.o 
form_factor_1p.o : form_factor_1p.f90 constante.o form_factor_type.o array.o matrice_s.o generic_function_1p.o precision_golem.o 
form_factor_2p.o : form_factor_2p.f90 constante.o array.o generic_function_2p.o form_factor_type.o matrice_s.o precision_golem.o 
form_factor_3p.o : form_factor_3p.f90 constante.o form_factor_type.o array.o matrice_s.o generic_function_3p.o precision_golem.o 
form_factor_4p.o : form_factor_4p.f90 constante.o form_factor_type.o sortie_erreur.o array.o matrice_s.o generic_function_3p.o generic_function_4p.o precision_golem.o 
form_factor_5p.o : form_factor_5p.f90 constante.o form_factor_type.o multiply_div.o sortie_erreur.o matrice_s.o array.o generic_function_3p.o generic_function_4p.o precision_golem.o 
form_factor_6p.o : form_factor_6p.f90 constante.o form_factor_type.o sortie_erreur.o matrice_s.o array.o form_factor_5p.o generic_function_3p.o generic_function_4p.o precision_golem.o 
form_factor_type.o : form_factor_type.f90 constante.o precision_golem.o 
function_2p0m_1mi.o : function_2p0m_1mi.f90 constante.o parametre.o equal.o sortie_erreur.o z_log.o precision_golem.o 
function_2p_m1m2.o : function_2p_m1m2.f90 parametre.o function_2p0m_1mi.o equal.o constante.o sortie_erreur.o z_log.o precision_golem.o 
function_3p0m_1mi.o : function_3p0m_1mi.f90 array.o parametre.o sortie_erreur.o z_log.o precision_golem.o 
function_3p1m_1mi.o : function_3p1m_1mi.f90 array.o parametre.o constante.o sortie_erreur.o mod_he.o zdilog.o z_log.o precision_golem.o 
function_3p1m_2mi.o : function_3p1m_2mi.f90 array.o parametre.o constante.o sortie_erreur.o mod_gn.o precision_golem.o 
function_3p1m.o : function_3p1m.f90 sortie_erreur.o mod_h0.o z_log.o precision_golem.o 
function_3p2m_1mi.o : function_3p2m_1mi.f90 mod_numeric.o array.o parametre.o constante.o sortie_erreur.o mod_hf.o mod_he.o zdilog.o z_log.o precision_golem.o 
function_3p2m.o : function_3p2m.f90 sortie_erreur.o mod_hf.o mod_he.o mod_h0.o z_log.o precision_golem.o 
function_3p3m.o : function_3p3m.f90 translate.o matrice_s.o s_matrix_type.o multiply_div.o generic_function_2p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_3p_finite.o : function_3p_finite.f90 translate.o matrice_s.o s_matrix_type.o multiply_div.o generic_function_2p.o sortie_erreur.o equal.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p1m.o : function_4p1m.f90 translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p2m_3mi_onshell.o : function_4p2m_3mi_onshell.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p2m_adj.o : function_4p2m_adj.f90 translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p2m_opp.o : function_4p2m_opp.f90 translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p3m.o : function_4p3m.f90 translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p4m.o : function_4p4m.f90 translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql10.o : function_4p_ql10.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql11.o : function_4p_ql11.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql12.o : function_4p_ql12.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql13.o : function_4p_ql13.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql14.o : function_4p_ql14.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql15.o : function_4p_ql15.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql16.o : function_4p_ql16.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql6.o : function_4p_ql6.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql7.o : function_4p_ql7.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql8.o : function_4p_ql8.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
function_4p_ql9.o : function_4p_ql9.f90 equal.o translate.o generic_function_3p.o sortie_erreur.o array.o parametre.o constante.o z_log.o zdilog.o mod_numeric.o precision_golem.o 
gb0.o : gb0.f90 parametre.o sortie_erreur.o tool_lt_to_golem.o constante.o form_factor_2p.o form_factor_type.o matrice_s.o precision_golem.o 
gc0.o : gc0.f90 parametre.o array.o sortie_erreur.o tool_lt_to_golem.o constante.o cache.o form_factor_3p.o form_factor_type.o matrice_s.o precision_golem.o 
gd0.o : gd0.f90 parametre.o sortie_erreur.o tool_lt_to_golem.o constante.o cache.o form_factor_4p.o form_factor_type.o matrice_s.o precision_golem.o 
ge0.o : ge0.f90 parametre.o sortie_erreur.o tool_lt_to_golem.o constante.o cache.o form_factor_5p.o form_factor_type.o matrice_s.o precision_golem.o 
generic_function_1p.o : generic_function_1p.f90 equal.o s_matrix_type.o parametre.o sortie_erreur.o constante.o z_log.o array.o precision_golem.o 
generic_function_2p.o : generic_function_2p.f90 function_2p_m1m2.o function_2p0m_1mi.o s_matrix_type.o equal.o parametre.o sortie_erreur.o constante.o tri.o z_log.o array.o precision_golem.o 
generic_function_3p.o : generic_function_3p.f90 sortie_erreur.o equal.o cache.o function_3p_finite.o parametre.o function_3p3m.o function_3p2m_1mi.o function_3p2m.o function_3p1m_2mi.o function_3p1m_1mi.o function_3p1m.o function_3p0m_1mi.o constante.o tri.o array.o s_matrix_type.o matrice_s.o precision_golem.o 
generic_function_4p.o : generic_function_4p.f90 avh_olo_main.o equal.o parametre.o generic_function_3p.o sortie_erreur.o cache.o form_factor_type.o function_4p_ql16.o function_4p_ql15.o function_4p_ql14.o function_4p_ql13.o function_4p_ql12.o function_4p_ql11.o function_4p_ql10.o function_4p_ql9.o function_4p_ql8.o function_4p_ql7.o function_4p_ql6.o function_4p2m_3mi_onshell.o function_4p4m.o function_4p3m.o function_4p2m_adj.o function_4p2m_opp.o function_4p1m.o constante.o tri.o array.o s_matrix_type.o matrice_s.o precision_golem.o 
gf0.o : gf0.f90 parametre.o sortie_erreur.o tool_lt_to_golem.o constante.o cache.o form_factor_6p.o form_factor_type.o matrice_s.o precision_golem.o 
inverse_matrice.o : inverse_matrice.f90 constante.o sortie_erreur.o parametre.o s_matrix_type.o equal.o precision_golem.o 
kinematic.o : kinematic.f90 precision.o 
kronecker.o : kronecker.f90 
ltest.o : ltest.f90 precision.o 
maccu.o : maccu.f90 precision.o 
madds.o : madds.f90 avh_olo_main.o notfirst.o mfunctions.o options.o constants.o avh_olo_kinds.o precision_golem.o precision.o 
matrice_s.o : matrice_s.f90 constante.o s_matrix_type.o parametre.o array.o tri.o inverse_matrice.o cache.o sortie_erreur.o precision_golem.o 
mcgs.o : mcgs.f90 precision.o 
mfunctions.o : mfunctions.f90 constants.o precision.o 
mgetbase.o : mgetbase.f90 kinematic.o mfunctions.o constants.o precision.o 
mgetc1.o : mgetc1.f90 mglobal.o mrestore.o mfunctions.o options.o constants.o precision.o 
mgetc2.o : mgetc2.f90 mglobal.o mrestore.o mfunctions.o options.o constants.o precision.o 
mgetc3.o : mgetc3.f90 mglobal.o mrestore.o mfunctions.o options.o constants.o precision.o 
mgetc4.o : mgetc4.f90 mglobal.o mrestore.o mfunctions.o options.o constants.o precision.o 
mgetc5.o : mgetc5.f90 mfunctions.o options.o constants.o precision.o 
mgetqs.o : mgetqs.f90 options.o mglobal.o mfunctions.o constants.o precision.o 
mglobal.o : mglobal.f90 precision.o 
mod_adapt_gauss.o : mod_adapt_gauss.f90 sortie_erreur.o array.o precision_golem.o 
mod_gn.o : mod_gn.f90 constante.o zdilog.o z_log.o parametre.o sortie_erreur.o mod_numeric.o precision_golem.o 
mod_h0.o : mod_h0.f90 sortie_erreur.o z_log.o parametre.o precision_golem.o 
mod_he.o : mod_he.f90 constante.o z_log.o parametre.o sortie_erreur.o mod_numeric.o precision_golem.o 
mod_hf.o : mod_hf.f90 constante.o z_log.o parametre.o sortie_erreur.o mod_numeric.o precision_golem.o 
mod_numeric.o : mod_numeric.f90 mod_adapt_gauss.o sortie_erreur.o precision_golem.o 
mrestore.o : mrestore.f90 mfunctions.o save.o constants.o precision.o 
msamurai.o : msamurai.f90 maccu.o avh_olo_main.o mtens.o mtests.o mgetc1.o mgetc2.o mgetc3.o mgetc4.o mgetc5.o mrestore.o mgetbase.o mgetqs.o madds.o ltest.o notfirst.o ncuts.o options.o constants.o precision.o 
mtens.o : mtens.f90 mcgs.o mfunctions.o options.o constants.o precision.o 
mtests.o : mtests.f90 mglobal.o ltest.o save.o mrestore.o mfunctions.o options.o constants.o precision.o 
multiply_div.o : multiply_div.f90 precision_golem.o 
ncuts.o : ncuts.f90 
notfirst.o : notfirst.f90 
olp_module.o : olp_module.f90 p26_gu_hepned_matrix.o p24_gdbar_hepneubar_matrix.o p7_ug_hepned_matrix.o p4_udbar_hepneg_matrix.o p2_dbarg_hepneubar_matrix.o p0_dbaru_hepneg_groups.o p30_gc_hepnes_matrix.o p19_sbarg_hepnecbar_matrix.o p17_sbarc_hepneg_matrix.o p14_cg_hepnes_matrix.o p11_csbar_hepneg_groups.o p33_gsbar_hepnecbar_matrix.o p29_gc_hepned_matrix.o p25_gdbar_hepnecbar_matrix.o p13_cg_hepned_matrix.o p10_cdbar_hepneg_matrix.o p3_dbarg_hepnecbar_matrix.o p1_dbarc_hepneg_groups.o p27_gu_hepnes_matrix.o p18_sbarg_hepneubar_matrix.o p16_sbaru_hepneg_matrix.o p8_ug_hepnes_matrix.o p5_usbar_hepneg_groups.o p32_gsbar_hepneubar_matrix.o p31_gc_hepneb_matrix.o p23_bbarg_hepnecbar_matrix.o p21_bbarc_hepneg_matrix.o p15_cg_hepneb_matrix.o p12_cbbar_hepneg_groups.o p35_gbbar_hepnecbar_matrix.o p28_gu_hepneb_matrix.o p22_bbarg_hepneubar_matrix.o p20_bbaru_hepneg_matrix.o p9_ug_hepneb_matrix.o p6_ubbar_hepneg_groups.o p34_gbbar_hepneubar_matrix.o p0_dbaru_hepneg_model.o p11_csbar_hepneg_model.o p1_dbarc_hepneg_model.o p5_usbar_hepneg_model.o p12_cbbar_hepneg_model.o p6_ubbar_hepneg_model.o p0_dbaru_hepneg_config.o p0_dbaru_hepneg_matrix.o p11_csbar_hepneg_config.o p11_csbar_hepneg_matrix.o p1_dbarc_hepneg_config.o p1_dbarc_hepneg_matrix.o p5_usbar_hepneg_config.o p5_usbar_hepneg_matrix.o p12_cbbar_hepneg_config.o p12_cbbar_hepneg_matrix.o p6_ubbar_hepneg_config.o p6_ubbar_hepneg_matrix.o 
options.o : options.f90 precision.o 
p0_dbaru_hepneg_abbrevh0.o : p0_dbaru_hepneg_abbrevh0.f90 p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_globalsh0.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_accu.o : p0_dbaru_hepneg_accu.f90 p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_amplitudeh0.o : p0_dbaru_hepneg_amplitudeh0.f90 options.o form_factor_type.o parametre.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_globalsh0.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_diagramsh0l0.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_golem95h0.o precision_golem.o p0_dbaru_hepneg_samuraih0.o precision.o p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_color.o : p0_dbaru_hepneg_color.f90 p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_config.o : p0_dbaru_hepneg_config.f90 
p0_dbaru_hepneg_d30h0l1d.o : p0_dbaru_hepneg_d30h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d30h0l1.o : p0_dbaru_hepneg_d30h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d34h0l1d.o : p0_dbaru_hepneg_d34h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d34h0l1.o : p0_dbaru_hepneg_d34h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d37h0l1d.o : p0_dbaru_hepneg_d37h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d37h0l1.o : p0_dbaru_hepneg_d37h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d40h0l1d.o : p0_dbaru_hepneg_d40h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d40h0l1.o : p0_dbaru_hepneg_d40h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d59h0l1d.o : p0_dbaru_hepneg_d59h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d59h0l1.o : p0_dbaru_hepneg_d59h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d60h0l1d.o : p0_dbaru_hepneg_d60h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d60h0l1.o : p0_dbaru_hepneg_d60h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d61h0l1d.o : p0_dbaru_hepneg_d61h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d61h0l1.o : p0_dbaru_hepneg_d61h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d70h0l1d.o : p0_dbaru_hepneg_d70h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d70h0l1.o : p0_dbaru_hepneg_d70h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d92h0l1d.o : p0_dbaru_hepneg_d92h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d92h0l1.o : p0_dbaru_hepneg_d92h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d94h0l1d.o : p0_dbaru_hepneg_d94h0l1d.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_d94h0l1.o : p0_dbaru_hepneg_d94h0l1.f90 precision_golem.o p0_dbaru_hepneg_globalsl1.o precision.o p0_dbaru_hepneg_abbrevh0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_util.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_diagramsh0l0.o : p0_dbaru_hepneg_diagramsh0l0.f90 p0_dbaru_hepneg_util.o p0_dbaru_hepneg_accu.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o p0_dbaru_hepneg_color.o 
p0_dbaru_hepneg_dipoles.o : p0_dbaru_hepneg_dipoles.f90 p0_dbaru_hepneg_model.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_globalsh0.o : p0_dbaru_hepneg_globalsh0.f90 p0_dbaru_hepneg_globalsl1.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_globalsl1.o : p0_dbaru_hepneg_globalsl1.f90 p0_dbaru_hepneg_color.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_golem95h0.o : p0_dbaru_hepneg_golem95h0.f90 p0_dbaru_hepneg_d94h0l1d.o p0_dbaru_hepneg_d94h0l1.o p0_dbaru_hepneg_d59h0l1d.o p0_dbaru_hepneg_d59h0l1.o p0_dbaru_hepneg_d37h0l1d.o p0_dbaru_hepneg_d37h0l1.o p0_dbaru_hepneg_d30h0l1d.o p0_dbaru_hepneg_d30h0l1.o p0_dbaru_hepneg_d92h0l1d.o p0_dbaru_hepneg_d92h0l1.o p0_dbaru_hepneg_d60h0l1d.o p0_dbaru_hepneg_d60h0l1.o p0_dbaru_hepneg_d40h0l1d.o p0_dbaru_hepneg_d40h0l1.o p0_dbaru_hepneg_d34h0l1d.o p0_dbaru_hepneg_d34h0l1.o p0_dbaru_hepneg_d61h0l1d.o p0_dbaru_hepneg_d61h0l1.o p0_dbaru_hepneg_d70h0l1d.o p0_dbaru_hepneg_d70h0l1.o p0_dbaru_hepneg_groups.o tens_rec.o p0_dbaru_hepneg_config.o precision_golem.o 
p0_dbaru_hepneg_groups.o : p0_dbaru_hepneg_groups.f90 p0_dbaru_hepneg_globalsl1.o options.o msamurai.o form_factor_4p.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_kinematics.o form_factor_type.o form_factor_3p.o form_factor_2p.o form_factor_1p.o tens_comb.o array.o cache.o parametre.o matrice_s.o p0_dbaru_hepneg_config.o tens_rec.o precision_golem.o madds.o precision.o 
p0_dbaru_hepneg_kinematics.o : p0_dbaru_hepneg_kinematics.f90 p0_dbaru_hepneg_scalar_cache.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_matrix.o : p0_dbaru_hepneg_matrix.f90 p0_dbaru_hepneg_groups.o p0_dbaru_hepneg_dipoles.o p0_dbaru_hepneg_amplitudeh0.o p0_dbaru_hepneg_diagramsh0l0.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_config.o p0_dbaru_hepneg_util.o msamurai.o 
p0_dbaru_hepneg_model.o : p0_dbaru_hepneg_model.f90 p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_rambo.o : p0_dbaru_hepneg_rambo.f90 p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p0_dbaru_hepneg_samuraih0.o : p0_dbaru_hepneg_samuraih0.f90 p0_dbaru_hepneg_globalsl1.o madds.o options.o msamurai.o p0_dbaru_hepneg_d94h0l1.o p0_dbaru_hepneg_d59h0l1.o p0_dbaru_hepneg_d37h0l1.o p0_dbaru_hepneg_d30h0l1.o p0_dbaru_hepneg_d92h0l1.o p0_dbaru_hepneg_d60h0l1.o p0_dbaru_hepneg_d40h0l1.o p0_dbaru_hepneg_d34h0l1.o p0_dbaru_hepneg_d61h0l1.o p0_dbaru_hepneg_d70h0l1.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_scalar_cache.o p0_dbaru_hepneg_config.o precision.o 
p0_dbaru_hepneg_scalar_cache.o : p0_dbaru_hepneg_scalar_cache.f90 madds.o precision.o 
p0_dbaru_hepneg_util.o : p0_dbaru_hepneg_util.f90 precision.o p0_dbaru_hepneg_config.o p0_dbaru_hepneg_color.o 
p0_dbaru_hepneg_version.o : p0_dbaru_hepneg_version.f90 
p10_cdbar_hepneg_matrix.o : p10_cdbar_hepneg_matrix.f90 p1_dbarc_hepneg_matrix.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p11_csbar_hepneg_abbrevh0.o : p11_csbar_hepneg_abbrevh0.f90 p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_color.o p11_csbar_hepneg_model.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_globalsh0.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_accu.o : p11_csbar_hepneg_accu.f90 p11_csbar_hepneg_config.o 
p11_csbar_hepneg_amplitudeh0.o : p11_csbar_hepneg_amplitudeh0.f90 options.o form_factor_type.o parametre.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_globalsh0.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_diagramsh0l0.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_util.o p11_csbar_hepneg_golem95h0.o precision_golem.o p11_csbar_hepneg_samuraih0.o precision.o p11_csbar_hepneg_groups.o p11_csbar_hepneg_color.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_color.o : p11_csbar_hepneg_color.f90 p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_config.o : p11_csbar_hepneg_config.f90 
p11_csbar_hepneg_d30h0l1d.o : p11_csbar_hepneg_d30h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d30h0l1.o : p11_csbar_hepneg_d30h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d34h0l1d.o : p11_csbar_hepneg_d34h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d34h0l1.o : p11_csbar_hepneg_d34h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d37h0l1d.o : p11_csbar_hepneg_d37h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d37h0l1.o : p11_csbar_hepneg_d37h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d40h0l1d.o : p11_csbar_hepneg_d40h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d40h0l1.o : p11_csbar_hepneg_d40h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d59h0l1d.o : p11_csbar_hepneg_d59h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d59h0l1.o : p11_csbar_hepneg_d59h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d60h0l1d.o : p11_csbar_hepneg_d60h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d60h0l1.o : p11_csbar_hepneg_d60h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d61h0l1d.o : p11_csbar_hepneg_d61h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d61h0l1.o : p11_csbar_hepneg_d61h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d70h0l1d.o : p11_csbar_hepneg_d70h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d70h0l1.o : p11_csbar_hepneg_d70h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d92h0l1d.o : p11_csbar_hepneg_d92h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d92h0l1.o : p11_csbar_hepneg_d92h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d94h0l1d.o : p11_csbar_hepneg_d94h0l1d.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_d94h0l1.o : p11_csbar_hepneg_d94h0l1.f90 precision_golem.o p11_csbar_hepneg_globalsl1.o precision.o p11_csbar_hepneg_abbrevh0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_util.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_diagramsh0l0.o : p11_csbar_hepneg_diagramsh0l0.f90 p11_csbar_hepneg_util.o p11_csbar_hepneg_accu.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o p11_csbar_hepneg_color.o 
p11_csbar_hepneg_dipoles.o : p11_csbar_hepneg_dipoles.f90 p11_csbar_hepneg_model.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_color.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_globalsh0.o : p11_csbar_hepneg_globalsh0.f90 p11_csbar_hepneg_globalsl1.o p11_csbar_hepneg_color.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_globalsl1.o : p11_csbar_hepneg_globalsl1.f90 p11_csbar_hepneg_color.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_golem95h0.o : p11_csbar_hepneg_golem95h0.f90 p11_csbar_hepneg_d94h0l1d.o p11_csbar_hepneg_d94h0l1.o p11_csbar_hepneg_d59h0l1d.o p11_csbar_hepneg_d59h0l1.o p11_csbar_hepneg_d37h0l1d.o p11_csbar_hepneg_d37h0l1.o p11_csbar_hepneg_d30h0l1d.o p11_csbar_hepneg_d30h0l1.o p11_csbar_hepneg_d92h0l1d.o p11_csbar_hepneg_d92h0l1.o p11_csbar_hepneg_d60h0l1d.o p11_csbar_hepneg_d60h0l1.o p11_csbar_hepneg_d40h0l1d.o p11_csbar_hepneg_d40h0l1.o p11_csbar_hepneg_d34h0l1d.o p11_csbar_hepneg_d34h0l1.o p11_csbar_hepneg_d61h0l1d.o p11_csbar_hepneg_d61h0l1.o p11_csbar_hepneg_d70h0l1d.o p11_csbar_hepneg_d70h0l1.o p11_csbar_hepneg_groups.o tens_rec.o p11_csbar_hepneg_config.o precision_golem.o 
p11_csbar_hepneg_groups.o : p11_csbar_hepneg_groups.f90 p11_csbar_hepneg_globalsl1.o options.o msamurai.o form_factor_4p.o p11_csbar_hepneg_model.o p11_csbar_hepneg_kinematics.o form_factor_type.o form_factor_3p.o form_factor_2p.o form_factor_1p.o tens_comb.o array.o cache.o parametre.o matrice_s.o p11_csbar_hepneg_config.o tens_rec.o precision_golem.o madds.o precision.o 
p11_csbar_hepneg_kinematics.o : p11_csbar_hepneg_kinematics.f90 p11_csbar_hepneg_scalar_cache.o p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_matrix.o : p11_csbar_hepneg_matrix.f90 p11_csbar_hepneg_groups.o p11_csbar_hepneg_dipoles.o p11_csbar_hepneg_amplitudeh0.o p11_csbar_hepneg_diagramsh0l0.o p11_csbar_hepneg_color.o p11_csbar_hepneg_model.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_config.o p11_csbar_hepneg_util.o msamurai.o 
p11_csbar_hepneg_model.o : p11_csbar_hepneg_model.f90 p11_csbar_hepneg_config.o 
p11_csbar_hepneg_rambo.o : p11_csbar_hepneg_rambo.f90 p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p11_csbar_hepneg_samuraih0.o : p11_csbar_hepneg_samuraih0.f90 p11_csbar_hepneg_globalsl1.o madds.o options.o msamurai.o p11_csbar_hepneg_d94h0l1.o p11_csbar_hepneg_d59h0l1.o p11_csbar_hepneg_d37h0l1.o p11_csbar_hepneg_d30h0l1.o p11_csbar_hepneg_d92h0l1.o p11_csbar_hepneg_d60h0l1.o p11_csbar_hepneg_d40h0l1.o p11_csbar_hepneg_d34h0l1.o p11_csbar_hepneg_d61h0l1.o p11_csbar_hepneg_d70h0l1.o p11_csbar_hepneg_model.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_scalar_cache.o p11_csbar_hepneg_config.o precision.o 
p11_csbar_hepneg_scalar_cache.o : p11_csbar_hepneg_scalar_cache.f90 madds.o precision.o 
p11_csbar_hepneg_util.o : p11_csbar_hepneg_util.f90 precision.o p11_csbar_hepneg_config.o p11_csbar_hepneg_color.o 
p11_csbar_hepneg_version.o : p11_csbar_hepneg_version.f90 
p12_cbbar_hepneg_abbrevh0.o : p12_cbbar_hepneg_abbrevh0.f90 p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_globalsh0.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_accu.o : p12_cbbar_hepneg_accu.f90 p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_amplitudeh0.o : p12_cbbar_hepneg_amplitudeh0.f90 options.o form_factor_type.o parametre.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_globalsh0.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_diagramsh0l0.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_golem95h0.o precision_golem.o p12_cbbar_hepneg_samuraih0.o precision.o p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_color.o : p12_cbbar_hepneg_color.f90 p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_config.o : p12_cbbar_hepneg_config.f90 
p12_cbbar_hepneg_d30h0l1d.o : p12_cbbar_hepneg_d30h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d30h0l1.o : p12_cbbar_hepneg_d30h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d34h0l1d.o : p12_cbbar_hepneg_d34h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d34h0l1.o : p12_cbbar_hepneg_d34h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d37h0l1d.o : p12_cbbar_hepneg_d37h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d37h0l1.o : p12_cbbar_hepneg_d37h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d40h0l1d.o : p12_cbbar_hepneg_d40h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d40h0l1.o : p12_cbbar_hepneg_d40h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d59h0l1d.o : p12_cbbar_hepneg_d59h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d59h0l1.o : p12_cbbar_hepneg_d59h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d60h0l1d.o : p12_cbbar_hepneg_d60h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d60h0l1.o : p12_cbbar_hepneg_d60h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d61h0l1d.o : p12_cbbar_hepneg_d61h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d61h0l1.o : p12_cbbar_hepneg_d61h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d70h0l1d.o : p12_cbbar_hepneg_d70h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d70h0l1.o : p12_cbbar_hepneg_d70h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d92h0l1d.o : p12_cbbar_hepneg_d92h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d92h0l1.o : p12_cbbar_hepneg_d92h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d94h0l1d.o : p12_cbbar_hepneg_d94h0l1d.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_d94h0l1.o : p12_cbbar_hepneg_d94h0l1.f90 precision_golem.o p12_cbbar_hepneg_globalsl1.o precision.o p12_cbbar_hepneg_abbrevh0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_util.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_diagramsh0l0.o : p12_cbbar_hepneg_diagramsh0l0.f90 p12_cbbar_hepneg_util.o p12_cbbar_hepneg_accu.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o p12_cbbar_hepneg_color.o 
p12_cbbar_hepneg_dipoles.o : p12_cbbar_hepneg_dipoles.f90 p12_cbbar_hepneg_model.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_globalsh0.o : p12_cbbar_hepneg_globalsh0.f90 p12_cbbar_hepneg_globalsl1.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_globalsl1.o : p12_cbbar_hepneg_globalsl1.f90 p12_cbbar_hepneg_color.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_golem95h0.o : p12_cbbar_hepneg_golem95h0.f90 p12_cbbar_hepneg_d94h0l1d.o p12_cbbar_hepneg_d94h0l1.o p12_cbbar_hepneg_d59h0l1d.o p12_cbbar_hepneg_d59h0l1.o p12_cbbar_hepneg_d37h0l1d.o p12_cbbar_hepneg_d37h0l1.o p12_cbbar_hepneg_d30h0l1d.o p12_cbbar_hepneg_d30h0l1.o p12_cbbar_hepneg_d92h0l1d.o p12_cbbar_hepneg_d92h0l1.o p12_cbbar_hepneg_d60h0l1d.o p12_cbbar_hepneg_d60h0l1.o p12_cbbar_hepneg_d40h0l1d.o p12_cbbar_hepneg_d40h0l1.o p12_cbbar_hepneg_d34h0l1d.o p12_cbbar_hepneg_d34h0l1.o p12_cbbar_hepneg_d61h0l1d.o p12_cbbar_hepneg_d61h0l1.o p12_cbbar_hepneg_d70h0l1d.o p12_cbbar_hepneg_d70h0l1.o p12_cbbar_hepneg_groups.o tens_rec.o p12_cbbar_hepneg_config.o precision_golem.o 
p12_cbbar_hepneg_groups.o : p12_cbbar_hepneg_groups.f90 p12_cbbar_hepneg_globalsl1.o options.o msamurai.o form_factor_4p.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_kinematics.o form_factor_type.o form_factor_3p.o form_factor_2p.o form_factor_1p.o tens_comb.o array.o cache.o parametre.o matrice_s.o p12_cbbar_hepneg_config.o tens_rec.o precision_golem.o madds.o precision.o 
p12_cbbar_hepneg_kinematics.o : p12_cbbar_hepneg_kinematics.f90 p12_cbbar_hepneg_scalar_cache.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_matrix.o : p12_cbbar_hepneg_matrix.f90 p12_cbbar_hepneg_groups.o p12_cbbar_hepneg_dipoles.o p12_cbbar_hepneg_amplitudeh0.o p12_cbbar_hepneg_diagramsh0l0.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_config.o p12_cbbar_hepneg_util.o msamurai.o 
p12_cbbar_hepneg_model.o : p12_cbbar_hepneg_model.f90 p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_rambo.o : p12_cbbar_hepneg_rambo.f90 p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p12_cbbar_hepneg_samuraih0.o : p12_cbbar_hepneg_samuraih0.f90 p12_cbbar_hepneg_globalsl1.o madds.o options.o msamurai.o p12_cbbar_hepneg_d94h0l1.o p12_cbbar_hepneg_d59h0l1.o p12_cbbar_hepneg_d37h0l1.o p12_cbbar_hepneg_d30h0l1.o p12_cbbar_hepneg_d92h0l1.o p12_cbbar_hepneg_d60h0l1.o p12_cbbar_hepneg_d40h0l1.o p12_cbbar_hepneg_d34h0l1.o p12_cbbar_hepneg_d61h0l1.o p12_cbbar_hepneg_d70h0l1.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_scalar_cache.o p12_cbbar_hepneg_config.o precision.o 
p12_cbbar_hepneg_scalar_cache.o : p12_cbbar_hepneg_scalar_cache.f90 madds.o precision.o 
p12_cbbar_hepneg_util.o : p12_cbbar_hepneg_util.f90 precision.o p12_cbbar_hepneg_config.o p12_cbbar_hepneg_color.o 
p12_cbbar_hepneg_version.o : p12_cbbar_hepneg_version.f90 
p13_cg_hepned_matrix.o : p13_cg_hepned_matrix.f90 p1_dbarc_hepneg_matrix.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p14_cg_hepnes_matrix.o : p14_cg_hepnes_matrix.f90 p11_csbar_hepneg_matrix.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p15_cg_hepneb_matrix.o : p15_cg_hepneb_matrix.f90 p12_cbbar_hepneg_matrix.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p16_sbaru_hepneg_matrix.o : p16_sbaru_hepneg_matrix.f90 p5_usbar_hepneg_matrix.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p17_sbarc_hepneg_matrix.o : p17_sbarc_hepneg_matrix.f90 p11_csbar_hepneg_matrix.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p18_sbarg_hepneubar_matrix.o : p18_sbarg_hepneubar_matrix.f90 p5_usbar_hepneg_matrix.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p19_sbarg_hepnecbar_matrix.o : p19_sbarg_hepnecbar_matrix.f90 p11_csbar_hepneg_matrix.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p1_dbarc_hepneg_abbrevh0.o : p1_dbarc_hepneg_abbrevh0.f90 p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_globalsh0.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_accu.o : p1_dbarc_hepneg_accu.f90 p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_amplitudeh0.o : p1_dbarc_hepneg_amplitudeh0.f90 options.o form_factor_type.o parametre.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_globalsh0.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_diagramsh0l0.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_golem95h0.o precision_golem.o p1_dbarc_hepneg_samuraih0.o precision.o p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_color.o : p1_dbarc_hepneg_color.f90 p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_config.o : p1_dbarc_hepneg_config.f90 
p1_dbarc_hepneg_d30h0l1d.o : p1_dbarc_hepneg_d30h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d30h0l1.o : p1_dbarc_hepneg_d30h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d34h0l1d.o : p1_dbarc_hepneg_d34h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d34h0l1.o : p1_dbarc_hepneg_d34h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d37h0l1d.o : p1_dbarc_hepneg_d37h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d37h0l1.o : p1_dbarc_hepneg_d37h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d40h0l1d.o : p1_dbarc_hepneg_d40h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d40h0l1.o : p1_dbarc_hepneg_d40h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d59h0l1d.o : p1_dbarc_hepneg_d59h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d59h0l1.o : p1_dbarc_hepneg_d59h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d60h0l1d.o : p1_dbarc_hepneg_d60h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d60h0l1.o : p1_dbarc_hepneg_d60h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d61h0l1d.o : p1_dbarc_hepneg_d61h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d61h0l1.o : p1_dbarc_hepneg_d61h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d70h0l1d.o : p1_dbarc_hepneg_d70h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d70h0l1.o : p1_dbarc_hepneg_d70h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d92h0l1d.o : p1_dbarc_hepneg_d92h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d92h0l1.o : p1_dbarc_hepneg_d92h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d94h0l1d.o : p1_dbarc_hepneg_d94h0l1d.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_d94h0l1.o : p1_dbarc_hepneg_d94h0l1.f90 precision_golem.o p1_dbarc_hepneg_globalsl1.o precision.o p1_dbarc_hepneg_abbrevh0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_util.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_diagramsh0l0.o : p1_dbarc_hepneg_diagramsh0l0.f90 p1_dbarc_hepneg_util.o p1_dbarc_hepneg_accu.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o p1_dbarc_hepneg_color.o 
p1_dbarc_hepneg_dipoles.o : p1_dbarc_hepneg_dipoles.f90 p1_dbarc_hepneg_model.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_globalsh0.o : p1_dbarc_hepneg_globalsh0.f90 p1_dbarc_hepneg_globalsl1.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_globalsl1.o : p1_dbarc_hepneg_globalsl1.f90 p1_dbarc_hepneg_color.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_golem95h0.o : p1_dbarc_hepneg_golem95h0.f90 p1_dbarc_hepneg_d94h0l1d.o p1_dbarc_hepneg_d94h0l1.o p1_dbarc_hepneg_d59h0l1d.o p1_dbarc_hepneg_d59h0l1.o p1_dbarc_hepneg_d37h0l1d.o p1_dbarc_hepneg_d37h0l1.o p1_dbarc_hepneg_d30h0l1d.o p1_dbarc_hepneg_d30h0l1.o p1_dbarc_hepneg_d92h0l1d.o p1_dbarc_hepneg_d92h0l1.o p1_dbarc_hepneg_d60h0l1d.o p1_dbarc_hepneg_d60h0l1.o p1_dbarc_hepneg_d40h0l1d.o p1_dbarc_hepneg_d40h0l1.o p1_dbarc_hepneg_d34h0l1d.o p1_dbarc_hepneg_d34h0l1.o p1_dbarc_hepneg_d61h0l1d.o p1_dbarc_hepneg_d61h0l1.o p1_dbarc_hepneg_d70h0l1d.o p1_dbarc_hepneg_d70h0l1.o p1_dbarc_hepneg_groups.o tens_rec.o p1_dbarc_hepneg_config.o precision_golem.o 
p1_dbarc_hepneg_groups.o : p1_dbarc_hepneg_groups.f90 p1_dbarc_hepneg_globalsl1.o options.o msamurai.o form_factor_4p.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_kinematics.o form_factor_type.o form_factor_3p.o form_factor_2p.o form_factor_1p.o tens_comb.o array.o cache.o parametre.o matrice_s.o p1_dbarc_hepneg_config.o tens_rec.o precision_golem.o madds.o precision.o 
p1_dbarc_hepneg_kinematics.o : p1_dbarc_hepneg_kinematics.f90 p1_dbarc_hepneg_scalar_cache.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_matrix.o : p1_dbarc_hepneg_matrix.f90 p1_dbarc_hepneg_groups.o p1_dbarc_hepneg_dipoles.o p1_dbarc_hepneg_amplitudeh0.o p1_dbarc_hepneg_diagramsh0l0.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_config.o p1_dbarc_hepneg_util.o msamurai.o 
p1_dbarc_hepneg_model.o : p1_dbarc_hepneg_model.f90 p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_rambo.o : p1_dbarc_hepneg_rambo.f90 p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p1_dbarc_hepneg_samuraih0.o : p1_dbarc_hepneg_samuraih0.f90 p1_dbarc_hepneg_globalsl1.o madds.o options.o msamurai.o p1_dbarc_hepneg_d94h0l1.o p1_dbarc_hepneg_d59h0l1.o p1_dbarc_hepneg_d37h0l1.o p1_dbarc_hepneg_d30h0l1.o p1_dbarc_hepneg_d92h0l1.o p1_dbarc_hepneg_d60h0l1.o p1_dbarc_hepneg_d40h0l1.o p1_dbarc_hepneg_d34h0l1.o p1_dbarc_hepneg_d61h0l1.o p1_dbarc_hepneg_d70h0l1.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_scalar_cache.o p1_dbarc_hepneg_config.o precision.o 
p1_dbarc_hepneg_scalar_cache.o : p1_dbarc_hepneg_scalar_cache.f90 madds.o precision.o 
p1_dbarc_hepneg_util.o : p1_dbarc_hepneg_util.f90 precision.o p1_dbarc_hepneg_config.o p1_dbarc_hepneg_color.o 
p1_dbarc_hepneg_version.o : p1_dbarc_hepneg_version.f90 
p20_bbaru_hepneg_matrix.o : p20_bbaru_hepneg_matrix.f90 p6_ubbar_hepneg_matrix.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
p21_bbarc_hepneg_matrix.o : p21_bbarc_hepneg_matrix.f90 p12_cbbar_hepneg_matrix.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p22_bbarg_hepneubar_matrix.o : p22_bbarg_hepneubar_matrix.f90 p6_ubbar_hepneg_matrix.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
p23_bbarg_hepnecbar_matrix.o : p23_bbarg_hepnecbar_matrix.f90 p12_cbbar_hepneg_matrix.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p24_gdbar_hepneubar_matrix.o : p24_gdbar_hepneubar_matrix.f90 p0_dbaru_hepneg_matrix.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p25_gdbar_hepnecbar_matrix.o : p25_gdbar_hepnecbar_matrix.f90 p1_dbarc_hepneg_matrix.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p26_gu_hepned_matrix.o : p26_gu_hepned_matrix.f90 p0_dbaru_hepneg_matrix.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p27_gu_hepnes_matrix.o : p27_gu_hepnes_matrix.f90 p5_usbar_hepneg_matrix.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p28_gu_hepneb_matrix.o : p28_gu_hepneb_matrix.f90 p6_ubbar_hepneg_matrix.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
p29_gc_hepned_matrix.o : p29_gc_hepned_matrix.f90 p1_dbarc_hepneg_matrix.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p2_dbarg_hepneubar_matrix.o : p2_dbarg_hepneubar_matrix.f90 p0_dbaru_hepneg_matrix.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p30_gc_hepnes_matrix.o : p30_gc_hepnes_matrix.f90 p11_csbar_hepneg_matrix.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p31_gc_hepneb_matrix.o : p31_gc_hepneb_matrix.f90 p12_cbbar_hepneg_matrix.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p32_gsbar_hepneubar_matrix.o : p32_gsbar_hepneubar_matrix.f90 p5_usbar_hepneg_matrix.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p33_gsbar_hepnecbar_matrix.o : p33_gsbar_hepnecbar_matrix.f90 p11_csbar_hepneg_matrix.o p11_csbar_hepneg_color.o p11_csbar_hepneg_kinematics.o p11_csbar_hepneg_model.o p11_csbar_hepneg_config.o 
p34_gbbar_hepneubar_matrix.o : p34_gbbar_hepneubar_matrix.f90 p6_ubbar_hepneg_matrix.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
p35_gbbar_hepnecbar_matrix.o : p35_gbbar_hepnecbar_matrix.f90 p12_cbbar_hepneg_matrix.o p12_cbbar_hepneg_color.o p12_cbbar_hepneg_kinematics.o p12_cbbar_hepneg_model.o p12_cbbar_hepneg_config.o 
p3_dbarg_hepnecbar_matrix.o : p3_dbarg_hepnecbar_matrix.f90 p1_dbarc_hepneg_matrix.o p1_dbarc_hepneg_color.o p1_dbarc_hepneg_kinematics.o p1_dbarc_hepneg_model.o p1_dbarc_hepneg_config.o 
p4_udbar_hepneg_matrix.o : p4_udbar_hepneg_matrix.f90 p0_dbaru_hepneg_matrix.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p5_usbar_hepneg_abbrevh0.o : p5_usbar_hepneg_abbrevh0.f90 p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_color.o p5_usbar_hepneg_model.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_globalsh0.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_accu.o : p5_usbar_hepneg_accu.f90 p5_usbar_hepneg_config.o 
p5_usbar_hepneg_amplitudeh0.o : p5_usbar_hepneg_amplitudeh0.f90 options.o form_factor_type.o parametre.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_globalsh0.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_diagramsh0l0.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_util.o p5_usbar_hepneg_golem95h0.o precision_golem.o p5_usbar_hepneg_samuraih0.o precision.o p5_usbar_hepneg_groups.o p5_usbar_hepneg_color.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_color.o : p5_usbar_hepneg_color.f90 p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_config.o : p5_usbar_hepneg_config.f90 
p5_usbar_hepneg_d30h0l1d.o : p5_usbar_hepneg_d30h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d30h0l1.o : p5_usbar_hepneg_d30h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d34h0l1d.o : p5_usbar_hepneg_d34h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d34h0l1.o : p5_usbar_hepneg_d34h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d37h0l1d.o : p5_usbar_hepneg_d37h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d37h0l1.o : p5_usbar_hepneg_d37h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d40h0l1d.o : p5_usbar_hepneg_d40h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d40h0l1.o : p5_usbar_hepneg_d40h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d59h0l1d.o : p5_usbar_hepneg_d59h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d59h0l1.o : p5_usbar_hepneg_d59h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d60h0l1d.o : p5_usbar_hepneg_d60h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d60h0l1.o : p5_usbar_hepneg_d60h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d61h0l1d.o : p5_usbar_hepneg_d61h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d61h0l1.o : p5_usbar_hepneg_d61h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d70h0l1d.o : p5_usbar_hepneg_d70h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d70h0l1.o : p5_usbar_hepneg_d70h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d92h0l1d.o : p5_usbar_hepneg_d92h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d92h0l1.o : p5_usbar_hepneg_d92h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d94h0l1d.o : p5_usbar_hepneg_d94h0l1d.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_d94h0l1.o : p5_usbar_hepneg_d94h0l1.f90 precision_golem.o p5_usbar_hepneg_globalsl1.o precision.o p5_usbar_hepneg_abbrevh0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_util.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_diagramsh0l0.o : p5_usbar_hepneg_diagramsh0l0.f90 p5_usbar_hepneg_util.o p5_usbar_hepneg_accu.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o p5_usbar_hepneg_color.o 
p5_usbar_hepneg_dipoles.o : p5_usbar_hepneg_dipoles.f90 p5_usbar_hepneg_model.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_color.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_globalsh0.o : p5_usbar_hepneg_globalsh0.f90 p5_usbar_hepneg_globalsl1.o p5_usbar_hepneg_color.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_globalsl1.o : p5_usbar_hepneg_globalsl1.f90 p5_usbar_hepneg_color.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_golem95h0.o : p5_usbar_hepneg_golem95h0.f90 p5_usbar_hepneg_d94h0l1d.o p5_usbar_hepneg_d94h0l1.o p5_usbar_hepneg_d59h0l1d.o p5_usbar_hepneg_d59h0l1.o p5_usbar_hepneg_d37h0l1d.o p5_usbar_hepneg_d37h0l1.o p5_usbar_hepneg_d30h0l1d.o p5_usbar_hepneg_d30h0l1.o p5_usbar_hepneg_d92h0l1d.o p5_usbar_hepneg_d92h0l1.o p5_usbar_hepneg_d60h0l1d.o p5_usbar_hepneg_d60h0l1.o p5_usbar_hepneg_d40h0l1d.o p5_usbar_hepneg_d40h0l1.o p5_usbar_hepneg_d34h0l1d.o p5_usbar_hepneg_d34h0l1.o p5_usbar_hepneg_d61h0l1d.o p5_usbar_hepneg_d61h0l1.o p5_usbar_hepneg_d70h0l1d.o p5_usbar_hepneg_d70h0l1.o p5_usbar_hepneg_groups.o tens_rec.o p5_usbar_hepneg_config.o precision_golem.o 
p5_usbar_hepneg_groups.o : p5_usbar_hepneg_groups.f90 p5_usbar_hepneg_globalsl1.o options.o msamurai.o form_factor_4p.o p5_usbar_hepneg_model.o p5_usbar_hepneg_kinematics.o form_factor_type.o form_factor_3p.o form_factor_2p.o form_factor_1p.o tens_comb.o array.o cache.o parametre.o matrice_s.o p5_usbar_hepneg_config.o tens_rec.o precision_golem.o madds.o precision.o 
p5_usbar_hepneg_kinematics.o : p5_usbar_hepneg_kinematics.f90 p5_usbar_hepneg_scalar_cache.o p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_matrix.o : p5_usbar_hepneg_matrix.f90 p5_usbar_hepneg_groups.o p5_usbar_hepneg_dipoles.o p5_usbar_hepneg_amplitudeh0.o p5_usbar_hepneg_diagramsh0l0.o p5_usbar_hepneg_color.o p5_usbar_hepneg_model.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_config.o p5_usbar_hepneg_util.o msamurai.o 
p5_usbar_hepneg_model.o : p5_usbar_hepneg_model.f90 p5_usbar_hepneg_config.o 
p5_usbar_hepneg_rambo.o : p5_usbar_hepneg_rambo.f90 p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p5_usbar_hepneg_samuraih0.o : p5_usbar_hepneg_samuraih0.f90 p5_usbar_hepneg_globalsl1.o madds.o options.o msamurai.o p5_usbar_hepneg_d94h0l1.o p5_usbar_hepneg_d59h0l1.o p5_usbar_hepneg_d37h0l1.o p5_usbar_hepneg_d30h0l1.o p5_usbar_hepneg_d92h0l1.o p5_usbar_hepneg_d60h0l1.o p5_usbar_hepneg_d40h0l1.o p5_usbar_hepneg_d34h0l1.o p5_usbar_hepneg_d61h0l1.o p5_usbar_hepneg_d70h0l1.o p5_usbar_hepneg_model.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_scalar_cache.o p5_usbar_hepneg_config.o precision.o 
p5_usbar_hepneg_scalar_cache.o : p5_usbar_hepneg_scalar_cache.f90 madds.o precision.o 
p5_usbar_hepneg_util.o : p5_usbar_hepneg_util.f90 precision.o p5_usbar_hepneg_config.o p5_usbar_hepneg_color.o 
p5_usbar_hepneg_version.o : p5_usbar_hepneg_version.f90 
p6_ubbar_hepneg_abbrevh0.o : p6_ubbar_hepneg_abbrevh0.f90 p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_globalsh0.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_accu.o : p6_ubbar_hepneg_accu.f90 p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_amplitudeh0.o : p6_ubbar_hepneg_amplitudeh0.f90 options.o form_factor_type.o parametre.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_globalsh0.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_diagramsh0l0.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_golem95h0.o precision_golem.o p6_ubbar_hepneg_samuraih0.o precision.o p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_color.o : p6_ubbar_hepneg_color.f90 p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_config.o : p6_ubbar_hepneg_config.f90 
p6_ubbar_hepneg_d30h0l1d.o : p6_ubbar_hepneg_d30h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d30h0l1.o : p6_ubbar_hepneg_d30h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d34h0l1d.o : p6_ubbar_hepneg_d34h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d34h0l1.o : p6_ubbar_hepneg_d34h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d37h0l1d.o : p6_ubbar_hepneg_d37h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d37h0l1.o : p6_ubbar_hepneg_d37h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d40h0l1d.o : p6_ubbar_hepneg_d40h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d40h0l1.o : p6_ubbar_hepneg_d40h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d59h0l1d.o : p6_ubbar_hepneg_d59h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d59h0l1.o : p6_ubbar_hepneg_d59h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d60h0l1d.o : p6_ubbar_hepneg_d60h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d60h0l1.o : p6_ubbar_hepneg_d60h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d61h0l1d.o : p6_ubbar_hepneg_d61h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d61h0l1.o : p6_ubbar_hepneg_d61h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d70h0l1d.o : p6_ubbar_hepneg_d70h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d70h0l1.o : p6_ubbar_hepneg_d70h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d92h0l1d.o : p6_ubbar_hepneg_d92h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d92h0l1.o : p6_ubbar_hepneg_d92h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d94h0l1d.o : p6_ubbar_hepneg_d94h0l1d.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_d94h0l1.o : p6_ubbar_hepneg_d94h0l1.f90 precision_golem.o p6_ubbar_hepneg_globalsl1.o precision.o p6_ubbar_hepneg_abbrevh0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_util.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_diagramsh0l0.o : p6_ubbar_hepneg_diagramsh0l0.f90 p6_ubbar_hepneg_util.o p6_ubbar_hepneg_accu.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o p6_ubbar_hepneg_color.o 
p6_ubbar_hepneg_dipoles.o : p6_ubbar_hepneg_dipoles.f90 p6_ubbar_hepneg_model.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_globalsh0.o : p6_ubbar_hepneg_globalsh0.f90 p6_ubbar_hepneg_globalsl1.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_globalsl1.o : p6_ubbar_hepneg_globalsl1.f90 p6_ubbar_hepneg_color.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_golem95h0.o : p6_ubbar_hepneg_golem95h0.f90 p6_ubbar_hepneg_d94h0l1d.o p6_ubbar_hepneg_d94h0l1.o p6_ubbar_hepneg_d59h0l1d.o p6_ubbar_hepneg_d59h0l1.o p6_ubbar_hepneg_d37h0l1d.o p6_ubbar_hepneg_d37h0l1.o p6_ubbar_hepneg_d30h0l1d.o p6_ubbar_hepneg_d30h0l1.o p6_ubbar_hepneg_d92h0l1d.o p6_ubbar_hepneg_d92h0l1.o p6_ubbar_hepneg_d60h0l1d.o p6_ubbar_hepneg_d60h0l1.o p6_ubbar_hepneg_d40h0l1d.o p6_ubbar_hepneg_d40h0l1.o p6_ubbar_hepneg_d34h0l1d.o p6_ubbar_hepneg_d34h0l1.o p6_ubbar_hepneg_d61h0l1d.o p6_ubbar_hepneg_d61h0l1.o p6_ubbar_hepneg_d70h0l1d.o p6_ubbar_hepneg_d70h0l1.o p6_ubbar_hepneg_groups.o tens_rec.o p6_ubbar_hepneg_config.o precision_golem.o 
p6_ubbar_hepneg_groups.o : p6_ubbar_hepneg_groups.f90 p6_ubbar_hepneg_globalsl1.o options.o msamurai.o form_factor_4p.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_kinematics.o form_factor_type.o form_factor_3p.o form_factor_2p.o form_factor_1p.o tens_comb.o array.o cache.o parametre.o matrice_s.o p6_ubbar_hepneg_config.o tens_rec.o precision_golem.o madds.o precision.o 
p6_ubbar_hepneg_kinematics.o : p6_ubbar_hepneg_kinematics.f90 p6_ubbar_hepneg_scalar_cache.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_matrix.o : p6_ubbar_hepneg_matrix.f90 p6_ubbar_hepneg_groups.o p6_ubbar_hepneg_dipoles.o p6_ubbar_hepneg_amplitudeh0.o p6_ubbar_hepneg_diagramsh0l0.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_config.o p6_ubbar_hepneg_util.o msamurai.o 
p6_ubbar_hepneg_model.o : p6_ubbar_hepneg_model.f90 p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_rambo.o : p6_ubbar_hepneg_rambo.f90 p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
p6_ubbar_hepneg_samuraih0.o : p6_ubbar_hepneg_samuraih0.f90 p6_ubbar_hepneg_globalsl1.o madds.o options.o msamurai.o p6_ubbar_hepneg_d94h0l1.o p6_ubbar_hepneg_d59h0l1.o p6_ubbar_hepneg_d37h0l1.o p6_ubbar_hepneg_d30h0l1.o p6_ubbar_hepneg_d92h0l1.o p6_ubbar_hepneg_d60h0l1.o p6_ubbar_hepneg_d40h0l1.o p6_ubbar_hepneg_d34h0l1.o p6_ubbar_hepneg_d61h0l1.o p6_ubbar_hepneg_d70h0l1.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_scalar_cache.o p6_ubbar_hepneg_config.o precision.o 
p6_ubbar_hepneg_scalar_cache.o : p6_ubbar_hepneg_scalar_cache.f90 madds.o precision.o 
p6_ubbar_hepneg_util.o : p6_ubbar_hepneg_util.f90 precision.o p6_ubbar_hepneg_config.o p6_ubbar_hepneg_color.o 
p6_ubbar_hepneg_version.o : p6_ubbar_hepneg_version.f90 
p7_ug_hepned_matrix.o : p7_ug_hepned_matrix.f90 p0_dbaru_hepneg_matrix.o p0_dbaru_hepneg_color.o p0_dbaru_hepneg_kinematics.o p0_dbaru_hepneg_model.o p0_dbaru_hepneg_config.o 
p8_ug_hepnes_matrix.o : p8_ug_hepnes_matrix.f90 p5_usbar_hepneg_matrix.o p5_usbar_hepneg_color.o p5_usbar_hepneg_kinematics.o p5_usbar_hepneg_model.o p5_usbar_hepneg_config.o 
p9_ug_hepneb_matrix.o : p9_ug_hepneb_matrix.f90 p6_ubbar_hepneg_matrix.o p6_ubbar_hepneg_color.o p6_ubbar_hepneg_kinematics.o p6_ubbar_hepneg_model.o p6_ubbar_hepneg_config.o 
parametre.o : parametre.f90 precision_golem.o 
precision.o : precision.f90 
precision_golem.o : precision_golem.f90 
save.o : save.f90 constants.o precision.o 
s_matrix_type.o : s_matrix_type.f90 constante.o equal.o sortie_erreur.o precision_golem.o 
sortie_erreur.o : sortie_erreur.f90 array.o parametre.o precision_golem.o 
spinor.o : spinor.f90 constante.o precision_golem.o 
tens_comb.o : tens_comb.f90 tens_rec.o form_factor_6p.o form_factor_5p.o form_factor_4p.o form_factor_3p.o form_factor_2p.o form_factor_1p.o form_factor_type.o array.o matrice_s.o precision_golem.o 
tensor_integrals.o : tensor_integrals.f90 array.o spinor.o matrice_s.o cache.o form_factor_6p.o form_factor_5p.o form_factor_4p.o form_factor_3p.o form_factor_2p.o form_factor_1p.o form_factor_type.o precision_golem.o 
tens_rec.o : tens_rec.f90 precision_golem.o 
tool_lt_to_golem.o : tool_lt_to_golem.f90 
translate.o : translate.f90 sortie_erreur.o precision_golem.o 
tri.o : tri.f90 precision_golem.o 
zdilog.o : zdilog.f90 equal.o sortie_erreur.o z_log.o constante.o precision_golem.o 
z_log.o : z_log.f90 equal.o sortie_erreur.o constante.o precision_golem.o 
