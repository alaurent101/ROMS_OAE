# svn $Id$
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2021 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Include file for GNU on MSYS/MinGW
# -------------------------------------------------------------------------
#
# ARPACK_LIBDIR  ARPACK libary directory
# FC             Name of the fortran compiler to use
# FFLAGS         Flags to the fortran compiler
# CPP            Name of the C-preprocessor
# CPPFLAGS       Flags to the C-preprocessor
# LIBS           Required libraries during linking
# NETCDF_INCDIR  NetCDF include directory
# NETCDF_LIBDIR  NetCDF libary directory
# LD             Program to load the objects into an executable
# LDFLAGS        Flags to the loader
# RANLIB         Name of ranlib command
# MDEPFLAGS      Flags for sfmakedepend  (-s if you keep .f files)
#
# First the defaults
#

              BIN := $(BIN).exe

               FC := gfortran
           FFLAGS := -frepack-arrays
              CPP := cpp
         CPPFLAGS := -P -traditional
             LIBS := $(SCRATCH_DIR)/libNLM.a         # cyclic dependencies
          LDFLAGS :=
               AR := ar
          ARFLAGS := -r
            MKDIR := mkdir -p
               RM := rm -f
           RANLIB := ranlib
             PERL := perl
             TEST := test

        MDEPFLAGS := --cpp --fext=f90 --file=- --objdir=$(SCRATCH_DIR)

#
# Library locations, can be overridden by environment variables.
#

ifdef USE_NETCDF4
        NF_CONFIG ?= nf-config
    NETCDF_INCDIR ?= $(shell $(NF_CONFIG) --prefix)/include
             LIBS += $(shell $(NF_CONFIG) --flibs)
else
    NETCDF_INCDIR ?= /usr/local/include
    NETCDF_LIBDIR ?= /usr/local/lib
             LIBS += -L$(NETCDF_LIBDIR) -lnetcdf
endif

ifdef USE_ARPACK
    ARPACK_LIBDIR ?= /usr/local/lib
             LIBS += -L$(ARPACK_LIBDIR) -larpack
endif

ifdef USE_OpenMP
         CPPFLAGS += -D_OPENMP
           FFLAGS += -fopenmp
endif

ifdef USE_DEBUG
           FFLAGS += -g -fbounds-check
else
           FFLAGS += -O3 -ffast-math
endif

ifdef USE_MCT
       MCT_INCDIR ?= /usr/local/mct/include
       MCT_LIBDIR ?= /usr/local/mct/lib
           FFLAGS += -I$(MCT_INCDIR)
             LIBS += -L$(MCT_LIBDIR) -lmct -lmpeu
endif

ifdef USE_ESMF
          ESMF_OS ?= $(OS)
      ESMF_SUBDIR := $(ESMF_OS).$(ESMF_COMPILER).$(ESMF_ABI).$(ESMF_COMM).$(ESMF_SITE)
      ESMF_MK_DIR ?= $(ESMF_DIR)/lib/lib$(ESMF_BOPT)/$(ESMF_SUBDIR)
                     include $(ESMF_MK_DIR)/esmf.mk
           FFLAGS += $(ESMF_F90COMPILEPATHS)
             LIBS += $(ESMF_F90LINKPATHS) $(ESMF_F90ESMFLINKLIBS)
endif

#
# Use full path of compiler.
#
               FC := $(shell which ${FC})
               LD := $(FC)

#
# Turn off bounds checking for function def_var, as "dimension(*)"
# declarations confuse Gnu Fortran 95 bounds-checking code.
#

$(SCRATCH_DIR)/def_var.o: FFLAGS += -fno-bounds-check

#
# Allow integer overflow in ran_state.F.  This is not allowed
# during -O3 optimization. This option should be applied only for
# Gfortran versions >= 4.2.
#

FC_TEST := $(findstring $(shell ${FC} --version | head -1 | cut -d " " -f 5 | \
                              cut -d "." -f 1-2),4.0 4.1)

ifeq "${FC_TEST}" ""
$(SCRATCH_DIR)/ran_state.o: FFLAGS += -fno-strict-overflow
endif

#
# Set free form format in source files to allow long string for
# local directory and compilation flags inside the code.
#

$(SCRATCH_DIR)/mod_ncparam.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/mod_strings.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/analytical.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/biology.o: FFLAGS += -ffree-form -ffree-line-length-none
ifdef USE_ADJOINT
$(SCRATCH_DIR)/ad_biology.o: FFLAGS += -ffree-form -ffree-line-length-none
endif
ifdef USE_REPRESENTER
$(SCRATCH_DIR)/rp_biology.o: FFLAGS += -ffree-form -ffree-line-length-none
endif
ifdef USE_TANGENT
$(SCRATCH_DIR)/tl_biology.o: FFLAGS += -ffree-form -ffree-line-length-none
endif

#
# Supress free format in SWAN source files since there are comments
# beyond column 72.
#

ifdef USE_SWAN

$(SCRATCH_DIR)/ocpcre.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/ocpids.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/ocpmix.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom3.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom4.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swancom5.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanmain.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanout1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanout2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanparll.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanpre1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanpre2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swanser.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swmod1.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/swmod2.o: FFLAGS += -ffixed-form
$(SCRATCH_DIR)/m_constants.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/m_fileio.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/mod_xnl4v5.o: FFLAGS += -ffree-form -ffree-line-length-none
$(SCRATCH_DIR)/serv_xnl4v5.o: FFLAGS += -ffree-form -ffree-line-length-none

endif
