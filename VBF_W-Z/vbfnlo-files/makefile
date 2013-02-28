
COMPILER=gfortran
FC=$(COMPILER)
ifeq ("$(FC)","gfortran")
F77 = gfortran
FFLAGS = -fno-automatic -ffixed-line-length-none -O2
endif
ifeq ("$(FC)","g77")
F77= g77 -fno-automatic -ffixed-line-length-none -O2
endif
ifeq ("$(FC)","ifort")
F77= ifort -save -132 -O3
endif


# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
#
# object files for qq->qq(g)W+W+ to be implemented in POWHEG;
OBJECTS_PH = ./loops/ggf_loops/loops_aux.o \
          ./vvjj_amp_aux.o \
          ./anomHiggs.o \
          ./anomV.o \
          ./anomal.o\
          ./brakets.o \
	  ./qqZWjqqi.o \
	  ./qqbqq.o \
	  ./init_tensors.o \
          ./help.o \
          ./kinematics_aux.o \
          ./tbv.o \
          ./jvirt5.o \
          ./jvirt5-isig.o \
          ./HtoWW.o \
          ./dotproducts.o \
          ./parameters.o \
          ./helas/ixxxxx.o \
          ./helas/oxxxxx.o \
          ./helas/fvixxx.o \
          ./helas/fvoxxx.o \
          ./helas/jvvxxx.o \
          ./helas/vvvxxx.o \
          ./helas/jioxxx.o \
          ./helas/iovxxx.o \
          ./helas/hvvxxx.o \
          ./helas/jvsxxx.o \
          ./helas/w3w3xx.o \
          ./helas/wwwwxx.o \
          ./helas/vvsxxx.o \
          ./helas/vxxxxx.o \
          ./helas/jwwwxx.o \
	  ./loops/aux_loops.o \
              ./loops/Diagrams/Boxline/boxlineABEDiv.o \
              ./loops/Diagrams/Boxline/boxlineABE.o \
              ./loops/Diagrams/Boxline/boxlineABETotal.o  \
              ./loops/Diagrams/Boxline/boxline.o  \
              ./loops/Diagrams/Boxline/boxlineNoAbeDiv.o  \
              ./loops/Diagrams/Boxline/boxlineNoAbe.o  \
              ./loops/Diagrams/Boxline/boxlineNoAbeTotal.o \
              ./loops/Diagrams/Penline/penlineABEDiv.o  \
              ./loops/Diagrams/Penline/penlineABETotal.o  \
              ./loops/Diagrams/Penline/penlineNoAbeDiv.o  \
              ./loops/Diagrams/Penline/penlineNoAbeTotal.o \
              ./loops/Diagrams/Penline/virtual1.o \
              ./loops/Diagrams/Penline/penlineABE.o \
              ./loops/Diagrams/Penline/penline.o    \
              ./loops/Diagrams/Penline/penlineNoAbe.o \
              ./loops/Diagrams/Penline/tri_box_penta_tens_red.o  \
              ./loops/Diagrams/Penline/virtual2.o \
              ./loops/Integrals/A0.o \
              ./loops/Integrals/B0C0D01MASG.o \
              ./loops/Integrals/C0functions.o  \
              ./loops/Integrals/D0functionsDiv.o \
              ./loops/Integrals/dilog.o        \
              ./loops/Integrals/F0.o  \
               ./loops/Integrals/E0functions.o  \
              ./loops/Integrals/scalarints.o\
              ./loops/Integrals/Aux_Functions.o  \
              ./loops/Integrals/B0functions.o  \
              ./loops/Integrals/cdilog.o       \
              ./loops/Integrals/D0functions.o \
              ./loops/TenRed/B/tensor_redux_BijLUde1M_Div.o  \
              ./loops/TenRed/B/tensor_redux_BijLUde_Div.o  \
              ./loops/TenRed/B/tensor_redux_BijLUde_G_Div.o\
              ./loops/TenRed/B/tensor_redux_BijLUde1M.o      \
              ./loops/TenRed/B/tensor_redux_BijLUde.o      \
              ./loops/TenRed/B/tensor_redux_BijLUde_G.o\
              ./loops/TenRed/C/tensor_redux_Cij_Div.o     \
              ./loops/TenRed/C/tensor_redux_CijLUde_1MasRank3.o    \
              ./loops/TenRed/C/tensor_redux_CijLUde_Rank3_G.o\
              ./loops/TenRed/C/tensor_redux_Cij.o                    \
              ./loops/TenRed/C/tensor_redux_CijLUdeRank3_Div.o     ./loops/TenRed/C/tensor_redux_Cij_new_Re_Com_1Mas.o \
              ./loops/TenRed/C/tensor_redux_CijLUde_1Mas_Div.o ./loops/TenRed/C/tensor_redux_CijLUdeRank3.o   ./loops/TenRed/C/tensor_redux_Cij_new_Re_Com.o \
              ./loops/TenRed/C/tensor_redux_CijLUde_1MasRank3_Div.o  ./loops/TenRed/C/tensor_redux_CijLUde_Rank3_G_Div.o \
              ./loops/TenRed/D/tensor_redux_Dij.o    ./loops/TenRed/D/tensor_redux_Dij_new_Re_Com.o   \
              ./loops/TenRed/E/tensor_redux_Eij_new_Re_Com_1Mas.o ./loops/TenRed/E/tensor_redux_Eij.o  ./loops/TenRed/E/tensor_redux_Eij_new_Re_Com.o \
              ./loops/Utilities/bra_ket_subroutines.o  ./loops/Utilities/GaugeTest.o  ./loops/Utilities/MatrixElements.o  \
              ./loops/Utilities/pent45_noAbe.o  ./loops/Utilities/Pent_to_boxes.o  ./loops/Utilities/Wardtest.o
# 	          ./helas/anomal6.o\
#           ./helas/anomal8.o\  
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
.o.o:
	$(F77)  -c -o $*.o  $(FFLAGS) $*.o

# ----------------------------------------------------------------------------

clean:
	rm -f *.o */*.o */*/*.o */*/*/*.o core *~ *.x


## 	  ./tensor_redux/B0C0D0fin.o \
