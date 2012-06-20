FC = gfortran
FFLAGS = -g -fno-automatic -ffixed-line-length-none -O2

# ----------------------------------------------------------------------------

# object files for qq->qqW+W- with leptonic decays

OBJECTS =  ./amp_aux.o \
	  ./help-z.o \
	  ./help.o \
	  ./braket_vbf.o \
	  ./init_tensors_z.o \
          ./qqzqq-nocut.o \
          ./qqzqqj-nocut.o \
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
          ./dhelas/jw3wxx.o \
          ./toll-hel.o \
	  ./boxline-coeffs-zjj-full.o \
	  ./boxline-coeffs-cross-zjj-full.o \
	  ./tensors/c0-int-mu.o \
	  ./tensors/d0-int-mu.o \
	  ./tensors/scalar-dint-order.o \
	  ./tensors/tensor-c-full.o \
	  ./tensors/tensor-d-quarks.o   

TARGETS = libvbfnlo.a


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# rules for vbfnlo_lib:
#
 libvbfnlo.a:  $(OBJECTS)
	$(AR) r libvbfnlo.a $(OBJECTS) 

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

# for g77 or gfortran: 
.f.o:
	$(FC) -c -o $*.o  $(FFLAGS) $*.f

# ----------------------------------------------------------------------------

clean:
	rm -f *.o core *~ *.x libvbfnlo.a



