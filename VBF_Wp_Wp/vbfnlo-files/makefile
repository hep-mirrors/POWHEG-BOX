FC = gfortran
FFLAGS = -g -fno-automatic -ffixed-line-length-none -O2

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
#
# object files for qq->qq(g)W+W+ to be implemented in POWHEG;
OBJECTS_PH =  ./amp_aux.o \
	  ./braket_vbf.o \
	  ./init_tensors.o \
          ./help.o \
          ./qqwppqq-lo.o \
          ./qqwppqq-vonly.o \
          ./qqwppqqj.o \
          ./dhelas/ixxxxx.o \
          ./dhelas/oxxxxx.o \
          ./dhelas/fvixxx.o \
          ./dhelas/fvoxxx.o \
          ./dhelas/jvvxxx.o \
          ./dhelas/vvvxxx.o \
          ./dhelas/jioxxx.o \
          ./dhelas/iovxxx.o \
          ./dhelas/hvvxxx.o \
          ./dhelas/jvsxxx.o \
          ./dhelas/w3w3xx.o \
          ./dhelas/wwwwxx.o \
          ./dhelas/vvsxxx.o \
          ./dhelas/vxxxxx.o \
          ./dhelas/jwwwxx.o \
          ./toww.o \
          ./towpm.o \
          ./towz.o \
	  ./jvirt-wpp.o \
          ./tensor_redux/tri_box_penta_tens_red.o \
	  ./tensor_redux/bra_ket_subroutines_vbf.o\
	  ./tensor_redux/B0C0D0fin.o \
          ./tensor_redux/tensor_redux_Cij.o \
          ./tensor_redux/tensor_redux_Dij.o \
          ./tensor_redux/tensor_redux_Eij.o \
          ./tensor_redux/boost.o \
	  ./tensor_redux/dilog.o \
          ./tensor_redux/virtual1.o \
	  ./tensor_redux/virtual2.o  

TARGETS_PH = libvbfnlo.a

# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# rules for vbfnlo_lib:
#

 libvbfnlo.a:  $(OBJECTS_PH)
	$(AR) r libvbfnlo.a $(OBJECTS_PH) 

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# for g77 or gfortran: 
.f.o:
	$(FC) -c -o $*.o  $(FFLAGS) $*.f

# ----------------------------------------------------------------------------

clean:
	rm -f *.o core *~ *.x
