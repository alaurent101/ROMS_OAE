# svn $Id$
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2021 The ROMS/TOMS Group                           :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.txt                                                :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Include file for UNICOS F90 compiler on CRAY T3D
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
               FC := f90
           FFLAGS := -e Im
              CPP := /opt/ctl/bin/cpp
         CPPFLAGS := -P -N
             LIBS := $(SCRATCH_DIR)/libNLM.a         # cyclic dependencies
          LDFLAGS :=
               AR := ar
          ARFLAGS := -r
            MKDIR := mkdir -p
               RM := rm -f
           RANLIB := touch
             PERL := perl
             TEST := test

        MDEPFLAGS := --cpp --fext=f90 --file=- --objdir=$(SCRATCH_DIR)

#
# Names of module files for netCDF f90 interface: override the lower-case
# file names specified in makefile
#

   NETCDF_MODFILE := NETCDF.mod
TYPESIZES_MODFILE := TYPESIZES.mod

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
 ifdef USE_MPI
   PARPACK_LIBDIR ?= /usr/local/lib
             LIBS += -L$(PARPACK_LIBDIR) -lparpack
 endif
    ARPACK_LIBDIR ?= /usr/local/lib
             LIBS += -L$(ARPACK_LIBDIR) -larpack
endif

ifdef USE_MPI
         CPPFLAGS += -DMPI
endif

ifdef USE_OpenMP
         CPPFLAGS += -D_OPENMP
endif

ifdef USE_DEBUG
           FFLAGS += -g -O 0 -e in -R abcnp
else
           FFLAGS += -O3
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
# Set free form format in source files to allow long string for
# local directory and compilation flags inside the code.
#

$(SCRATCH_DIR)/mod_ncparam.o: FFLAGS += -f free
$(SCRATCH_DIR)/mod_strings.o: FFLAGS += -f free
$(SCRATCH_DIR)/analytical.o: FFLAGS += -f free
$(SCRATCH_DIR)/biology.o: FFLAGS += -f free
ifdef USE_ADJOINT
$(SCRATCH_DIR)/ad_biology.o: FFLAGS += -f free
endif
ifdef USE_REPRESENTER
$(SCRATCH_DIR)/rp_biology.o: FFLAGS += -f free
endif
ifdef USE_TANGENT
$(SCRATCH_DIR)/tl_biology.o: FFLAGS += -f free
endif

#
# Supress free format in SWAN files since there are comments
# beyond column 72.
#

ifdef USE_SWAN

$(SCRATCH_DIR)/ocpcre.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/ocpids.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/ocpmix.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swancom1.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swancom2.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swancom3.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swancom4.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swancom5.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swanmain.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swanout1.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swanout2.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swanparll.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swanpre1.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swanpre2.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swanser.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swmod1.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/swmod2.o: FFLAGS += -f fixed
$(SCRATCH_DIR)/m_constants.o: FFLAGS += -f free
$(SCRATCH_DIR)/m_fileio.o: FFLAGS += -f free
$(SCRATCH_DIR)/mod_xnl4v5.o: FFLAGS += -f free
$(SCRATCH_DIR)/serv_xnl4v5.o: FFLAGS += -f free

endif
