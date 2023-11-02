ALL: eSTOMP1

#---------------------------------------------------------------------------
#  preprocessor definitions. These can be used to turn on different options
#  in the code. Currently recognized definitions:
#    USE_H5HUT parallel IO. If using this, be sure to run in the parallel 
#              file system and set your lustre parameters (lfs setstripe)
#    PETSCDUP  to include redundant petsc headers which pehaps is needed on 
#              some platform?
#    PETSC_3_3 some function names changed in 3.3 See
#              http://www.mcs.anl.gov/petsc/documentation/changes/33.html
#              ***
#              NOTE: Recommend buildling your own petsc.  System builds might
#                    include hdf which will likely conflict with your hdf
#                    for example on hopper
#              ***
#    H5PART_1_6_7 Supports orthogonal meshes
#    MAXLINES 10000  Set the maximum number of lines that can be used in
#                    the buffered input card reader
#---------------------------------------------------------------------------
#DEFINES = -DUSE_H5HUT  -DPETSC_3_3
#DEFINES = -DUSE_H5HUT  

#DEFINES = -DUSE_E4D
#CPE4D = E4D

SYSTEM:=constance
#SYSTEM = hopper

ifeq ($(CPE4D),E4D)
   COMPILER_FLAGS_1 =  $(DEBUG_FLAG) $(OPTIMIZE_FLAG) -r8
   COMPILER_FLAGS_2 =  $(DEBUG_FLAG) $(OPTIMIZE_FLAG)
else
   COMPILER_FLAGS_1 =  $(DEBUG_FLAG) $(OPTIMIZE_FLAG) -r8 #-fPIC 
endif

ifeq ($(findstring constance,$(SYSTEM)),constance)
   PETSC_ARCH  = intel
   PETSC_DIR   = /people/d3a926/lib/petsc-3.1-p8
#   PETSC_DIR = /pic/scratch/hexi877/estomp_versions/petsc-3.3-p2
#   PETSC_DIR   = /pic/projects/geophys/stomp-e4d/petsc-3.3-p7
   GA_DIR      = /people/d3a926/lib/exahdf/intel
   MPI_DIR     = /share/apps/openmpi/1.8.3/intel/15.0.1

   # Optional components if using parallel IO only
   HDF5_DIR    = /people/d3a926/lib/exahdf/intel
   H5HUT_DIR   = /people/d3a926/lib/exahdf/intel

#   HDF5_DIR    = /people/d3a926/lib/exahdf/gcc
#   H5HUT_DIR   = /people/d3a926/lib/exahdf/gcc
   FC = mpif90
#  DEBUG_FLAG = -g 
#  DEBUG_FLAG = -u -g -traceback -fpe:0 -check all 
  OPTIMIZE_FLAG = #-fast -O2
#   OPTIMIZE_FLAG = -O2 -fp-model precise
#   OPTIMIZE_FLAG = -O2
#   OPTIMIZE_FLAG = -O2
   FCFLAGS =  $(DEBUG_FLAG) $(OPTIMIZE_FLAG) -fPIC 
#   FLINKER = mpif90 -fPIC -CB -g 
   FLINKER = mpif90 
   CPP = cpp
   CPPFLAGS = -P -traditional-cpp

   MPI_LIB = -L$(MPI_DIR)/lib -lmpi #-lmpio
   PETSC_LINK = $(PETSC_LIB)
   GA_LIB = -L$(GA_DIR)/lib -lga -libverbs 
endif


ifeq ($(findstring olympus,$(SYSTEM)),olympus)
   PETSC_ARCH = intel
   PETSC_DIR   = /people/d3k870/lib/petsc-3.1-p8
   GA_DIR      = /people/d3k870/lib/exahdf/intel
   MPI_DIR     = /share/apps/openmpi/1.5.4/intel/11.1.069

   # Optional components if using parallel IO only
   HDF5_DIR    = /people/d3k870/lib/exahdf/intel
   H5HUT_DIR   = /people/d3k870/lib/exahdf/intel

   FC = mpif90
#  DEBUG_FLAG = -g 
#  DEBUG_FLAG = -g -traceback -fpe:0 -check all 
#  OPTIMIZE_FLAG = #-fast -O2
   OPTIMIZE_FLAG = -O2 -fp-model precise
   FCFLAGS =  $(DEBUG_FLAG) $(OPTIMIZE_FLAG) -r8 #-fPIC 
   FLINKER = mpif90 
   CPP = cpp
   CPPFLAGS = -P -traditional-cpp


   MPI_LIB = -L$(MPI_DIR)/lib -lmpi #-lmpio
   PETSC_LINK = $(PETSC_LIB)
   GA_LIB = -L$(GA_DIR)/lib -lga -libverbs 
endif

ifeq ($(findstring hopper,$(SYSTEM)),hopper)
   # Required component builds
   PETSC_ARCH = craypgi-xe6_g
   PETSC_DIR = /usr/common/acts/PETSc/3.1
   GA_DIR      = /scratch2/scratchdirs/karen/stompdebug
   MPI_DIR    = ${MPICH_DIR}

   # Optional components if using parallel IO only
   HDF5_DIR    = /scratch/scratchdirs/karen/hdf5-1.8.9/install
   H5HUT_DIR   = /scratch/scratchdirs/karen/H5Part-1.6.6/install

   FC = ftn
   DEBUG_FLAG = -g
   #DEBUG_FLAG = -g -Mbounds
   OPTIMIZE_FLAG = #-fast -O2
   FCFLAGS =  $(DEBUG_FLAG) $(OPTIMIZE_FLAG) -r8 #-fPIC 
   #FLINKER = mpif90 $(FCFLAGS) -fpe0 -traceback
   FLINKER = ftn
   CPP = cpp
   CPPFLAGS = -P -traditional-cpp

#   GA_LIB = -L$(GA_DIR)/lib -lga -lhugetlbfs
   GA_LIB = -L$(GA_DIR)/lib -lga -larmci
   MPI_LIB = -L$(MPI_DIR)/lib -lmpich #-lmpio
   PETSC_LINK = \
               -L$(PETSC_DIR)/$(PETSC_ARCH)/lib -lpetsc \
               -llapack -lblas -lnsl -lrt -ldl
endif

#---------------------------------------------------------------------------
# Hopefully should not need to modify below here
#---------------------------------------------------------------------------

#include ${PETSC_DIR}/conf/base
include ${PETSC_DIR}/${PETSC_ARCH}/conf/petscvariables
include ${PETSC_DIR}/conf/variables
include ${PETSC_DIR}/conf/rules


PETSC_INCLUDES = -I$(PETSC_DIR) \
                 -I$(PETSC_DIR)/include \
                 -I$(PETSC_DIR)/include/finclude \
                 -I$(PETSC_DIR)/$(PETSC_ARCH)/include

GA_INCLUDES    = -I$(GA_DIR)/include
MPI_INCLUDES   = -I$(MPI_DIR)/include

H5HUT_INCLUDES = -I$(H5HUT_DIR)/include -I$(HDF5_DIR)/include

HDF5_LIBRARIES =  -L$(HDF5_DIR)/lib -lhdf5 -lz
H5HUT_LIB = -L$(H5HUT_DIR)/lib -lH5PartF -lH5Part 

INCLUDES = -I. $(GA_INCLUDES) $(H5HUT_INCLUDES) $(PETSC_INCLUDES) $(MPI_INCLUDES)
LIBS = $(GA_LIB) ${PETSC_LINK} $(H5HUT_LIB) $(HDF5_LIBRARIES) #$(MPI_LIB)

EXECF = eSTOMP1.x

SUF     = f90

.PRECIOUS: *.o *.mod

# On PIC, need to comment out Petsc Includes

SOURCEF = grid_mod.F90\
          sio.F90\
          bufferedread.F90\
          rd_mod.F90\
          banner.F90\
          petscapp.F90\
          bcf1.F90\
          difmn.F90\
          bcj1.F90\
          bcp1.F90\
          cap1.F90\
          eos.F90\
          chk1.F90\
          cisc1.F90\
          dpor1.F90\
          drcvl.F90\
          drcvlb.F90\
          eckechem.F90\
          elc1.F90\
          factor.F90\
          fnhgbl.F90\
          fntblx.F90\
          fntbly.F90\
          fsplnx.F90\
          fsplny.F90\
          GA_grid.F90\
          incrm1.F90\
          intlz.F90\
          jcbp.F90\
          jcbwl.F90\
          ksp1.F90\
          lcase.F90\
          ldo1.F90\
          perm_i.F90\
          petsccalls.F90\
          porsty.F90\
          qsat1.F90\
	  e4d_link.F90\
          e4d_vars.F90\
          e4d_mat_inv.F90\
          e4d_setup.F90\
          e4d_run.F90\
          rd_chem.F90\
          rdplant.F90\
          roots.F90\
          rdbc1.F90\
          rdchr.F90\
          rddpr.F90\
          rdgrid.F90\
          rdhydr.F90\
          rdic1.F90\
          rdijk.F90\
          rdinac.F90\
          rdini.F90\
          rdinpl.F90\
          rdinpt1.F90\
          rdint.F90\
          rdlrp.F90\
          rdmech.F90\
          rdou1.F90\
          rdouun.F90\
          rdrock.F90\
          rdrst.F90\
          rdsclf1.F90\
          rdsf1.F90\
          rdsolu.F90\
          rdsp1.F90\
          rdsr1.F90\
          rdsimu.F90\
          rdtf1.F90\
          rdtp1.F90\
          rdunit.F90\
          refnod.F90\
          rkl1.F90\
          rklt1.F90\
          rsdl1.F90\
          sbnd1.F90\
          sfin.F90\
          sjcbl.F90\
          smc1.F90\
          sorc1.F90\
          soric1.F90\
          splinx.F90\
          spliny.F90\
          step.F90\
          stomp1.F90\
          tmstep.F90\
          tortu.F90\
          trpgl1.F90\
          update_nodes.F90\
          updt1.F90\
          utils.F90\
          watlad.F90\
          watlqv.F90\
          watsp.F90\
          wrmsg.F90\
          wrplot.F90\
          wrrst.F90\
	  stomp1_driver.F90\
          watgsd.F90\
          airgsd.F90\
          average_v.F90\
          tport1.F90\
          crntnb.F90\
          sjcbl.F90\
          updtc.F90\
          sort1.F90\
          sorit1.F90\
          cflux1.F90\
          cbnd1.F90\
          sfxlb.F90\
          sfxl.F90

FOBJS =   \
          grid_mod.o\
          sio.o\
          bufferedread.o\
          rd_mod.o\
          petscapp.o\
          banner.o\
          bcf1.o\
          difmn.o\
          bcj1.o\
          bcp1.o\
          cap1.o\
          eos.o\
          chk1.o\
          cisc1.o\
          dpor1.o\
          drcvl.o\
          drcvlb.o\
          eckechem.o\
          elc1.o\
          factor.o\
          fntblx.o\
          fntbly.o\
          fsplnx.o\
          fsplny.o\
          GA_grid.o\
          incrm1.o\
          intlz.o\
          jcbp.o\
          jcbwl.o\
          ksp1.o\
          lcase.o\
          ldo1.o\
          perm_i.o\
          petsccalls.o\
          porsty.o\
          qsat1.o\
          rd_chem.o\
          rdplant.o\
          roots.o\
          rdbc1.o\
          rdchr.o\
          rddpr.o\
          rdgrid.o\
          rdhydr.o\
          rdic1.o\
          rdijk.o\
          rdinac.o\
          rdini.o\
          rdinpl.o\
          rdinpt1.o\
          rdint.o\
          rdlrp.o\
          rdmech.o\
          rdou1.o\
          rdouun.o\
          rdrock.o\
          rdsclf1.o\
          rdsf1.o\
          rdsolu.o\
          rdsp1.o\
          rdsr1.o\
          rdsimu.o\
          rdtf1.o\
          rdtp1.o\
          rdunit.o\
          refnod.o\
          rkl1.o\
          rklt1.o\
          rsdl1.o\
          smc1.o\
          sorc1.o\
          soric1.o\
          splinx.o\
          spliny.o\
          step.o\
          tmpr1.o\
          tmstep.o\
          tortu.o\
          update_nodes.o\
          updt1.o\
          utils.o\
          watlad.o\
          watlqv.o\
          watsp.o\
          wrmsg.o\
          wrplot.o\
          wrrst.o\
          stomp1.o\
          stomp1_driver.o\
          watgsd.o\
          airgsd.o\
          average_v.o\
          tport1.o\
          crntnb.o\
          sjcbl.o\
          sbnd1.o\
          sfin.o\
          updtc.o\
          sprp1.o\
          sort1.o\
          sorit1.o\
          rdrst.o\
          cflux1.o\
          cbnd1.o\
          sfxlb.o\
          sfxl.o\
          ucode1.o\
          well_w.o\
          sort.o
      

ifeq ($(CPE4D),E4D)
CP_OBJS = \
	e4d_mat_inv.o\
          e4d_vars.o\
	e4d_setup.o\
          e4d_run.o\
	e4d_link.o\

OBJS = $(CP_OBJS) $(FOBJS)

$(FOBJS): FCFLAGS := $(COMPILER_FLAGS_1)
$(CP_OBJS): FCFLAGS := $(COMPILER_FLAGS_2)
else
OBJS = $(CP_OBJS) $(FOBJS)
$(FOBJS): FCFLAGS := $(COMPILER_FLAGS_1)
endif

# Vicky Do this instead of explicit list?
#OBJS = $(SOURCEF:.F90=.o)
$(OBJS): %.o: %.F90
	$(FC) -c $(DEFINES) $(FCFLAGS) $(INCLUDES) $< -o $@

#%.o: %.F90
#	$(FC) -c $(DEFINES) $(FCFLAGS) $(INCLUDES) $< -o $*.o

eSTOMP1: $(OBJS)
	$(FC) -o stomp1.x $(OBJS) $(LIBS)


#------------------------------------------------------------------
#  PETCS uses clean:: in its rules so we need to do the same here
#  allclean is for backward compatibility
#------------------------------------------------------------------
allclean:
	rm -f *.o *.if90 *.f90 *.mod *.x

clean::
	rm -f *.o *.if90 *.f90 *.mod *.x
