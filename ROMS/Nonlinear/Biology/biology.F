#include "cppdefs.h"
      MODULE biology_mod
#if defined NONLINEAR && defined BIOLOGY
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the source and sink terms for selected        !
!  biology model.                                                      !
!                                                                      !
/*                                                                    **
** The current design allows the user to have a lot of latitude for   **
** customizing or adding any particular ecosystem model.              **
**                                                                    **
** Each ecosystem model is composed of seven files:                   **
**                                                                    **
**   (1) Model source and sink discretized equations:                 **
**                                                                    **
**         ecosim.h           ECOSIM                                  **
**         fennel.h           BIO_FENNEL                              **
**         hypoxia_srm.h      HYPOXIA_SRM                             **
**         nemuro.h           NEMURO                                  **
**         npzd_Franks.h      NPZD_FRANKS                             **
**         npzd_iron.h        NPZD_IRON                               **
**         npzd_Powell.h      NPZD_POWELL                             **
**         red_tide.h         RED_TIDE                                **
**         reduced_bgc.h      REDUCED_BGC                             **
**                                                                    **
**   (2) Internal model parameters declaration:                       **
**                                                                    **
**         ecosim_mod.h                                               **
**         fennel_mod.h                                               **
**         hypoxia_srm_mod.h                                          **
**         nemuro_mod.h                                               **
**         npzd_Franks_mod.h                                          **
**         npzd_iron_mod.h                                            **
**         npzd_Powell_mod.h                                          **
**         red_tide_mod.h                                             **
**         reduced_bgc_mod.h                                          **
**                                                                    **
**   (3) Model parameters standard input script:                      **
**                                                                    **
**         ecosim.in                                                  **
**         bio_Fennel.in                                              **
**         hypoxia_srm.in                                             **
**         nemuro.in                                                  **
**         npzd_Franks.in                                             **
**         npzd_iron.in                                               **
**         npzd_Powell.in                                             **
**         red_tide.in                                                **
**         reduced_bgc.in                                             **
**                                                                    **
**   (4) Code to read input model parameters:                         **
**                                                                    **
**         ecosim_inp.h                                               **
**         fennel_inp.h                                               **
**         hypoxia_srm_inp.h                                          **
**         nemuro_inp.h                                               **
**         npzd_Franks_inp.h                                          **
**         npzd_iron_inp.h                                            **
**         npzd_Powell_inp.h                                          **
**         red_tide_inp.h                                             **
**         reduced_bgc_inp.h                                          **
**                                                                    **
**   (5) Code to assign indices to model variables during the         **
**       reading of metadata information from "varinfo.dat":          **
**                                                                    **
**         ecosim_var.h                                               **
**         fennel_var.h                                               **
**         hypoxia_srm_var.h                                          **
**         nemuro_var.h                                               **
**         npzd_Franks_var.h                                          **
**         npzd_iron_var.h                                            **
**         npzd_Powell_var.h                                          **
**         red_tide_var.h                                             **
**         reduced_bgc_var.h                                          **
**                                                                    **
**   (6) Code to define input model parameters in all output          **
**       NetCDF files:                                                **
**                                                                    **
**         ecosim_def.h                                               **
**         fennel_def.h                                               **
**         hypoxia_srm_def.h                                          **
**         nemuro_def.h                                               **
**         npzd_Franks_def.h                                          **
**         npzd_iron_def.h                                            **
**         npzd_Powell_def.h                                          **
**         red_tide_def.h                                             **
**         reduced_bgc_def.h                                          **
**                                                                    **
**   (7) Code to write out input model parameters in all output       **
**       NetCDF files:                                                **
**                                                                    **
**         ecosim_wrt.h                                               **
**         fennel_wrt.h                                               **
**         hypoxia_srm_wrt.h                                          **
**         nemuro_wrt.h                                               **
**         npzd_Franks_wrt.h                                          **
**         npzd_iron_wrt.h                                            **
**         npzd_Powell_wrt.h                                          **
**         red_tide_wrt.h                                             **
**         reduced_bgc_wrt.h                                          **
**                                                                    **
**                                                                    **
** Note that all the *.h files are located in ROMS/Nonlinear/Biology  **
** and included within  <...>  to allow the user to customize any of  **
** them in the project directory  while keeping the distributed code  **
** intact (check the build script for details).                       **
*/
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: biology
!
      CONTAINS
!
# if defined BIO_FENNEL
#  include <fennel.h>
# elif defined ECOSIM
#  include <ecosim.h>
# elif defined HYPOXIA_SRM
#  include <hypoxia_srm.h>
# elif defined NEMURO
#  include <nemuro.h>
# elif defined NPZD_FRANKS
#  include <npzd_Franks.h>
# elif defined NPZD_IRON
#  include <npzd_iron.h>
# elif defined NPZD_POWELL
#  include <npzd_Powell.h>
# elif defined RED_TIDE
#  include <red_tide.h>
# elif defined REDUCED_BGC
#  include <reduced_bgc.h>
# endif

#endif
      END MODULE biology_mod
