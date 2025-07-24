/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2021 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Writes the Simple BGC model input parameters into output NetCDF   **
**  files. It is included in routine "wrt_info.F".                    **
**                                                                    **
************************************************************************
*/

!
!  Write out the Simple BGC model parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'BioIter',               &
     &                      BioIter(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'SOC',                   &
     &                      SOC(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

#ifdef TEMP_RATES
      CALL netcdf_put_fvar (ng, model, ncname, 'WOC0',                  &
     &                      WOC0(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3 || defined WOC_H23

      CALL netcdf_put_fvar (ng, model, ncname, 'WOC20',                 &
     &                      WOC20(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'WOC30',                 &
     &                      WOC30(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
# endif
#else
      CALL netcdf_put_fvar (ng, model, ncname, 'WOC',                   &
     &                      WOC(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3 || defined WOC_H23

      CALL netcdf_put_fvar (ng, model, ncname, 'WOC2',                  &
     &                      WOC2(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'WOC3',                  &
     &                      WOC3(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
# endif
#endif

      CALL netcdf_put_fvar (ng, model, ncname, 'PhyNC',                 &
     &                      PhyNC(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'pCO2air',               &
     &                      pCO2air(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

#ifdef TALK_ADDITION
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_alkalinity',        &
     &                      iloc_alkalinity(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_alkalinity',        &
     &                      jloc_alkalinity(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_alkalinity_min',    &
     &                      kloc_alkalinity_min(ng), (/0/), (/0/),       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
     
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_alkalinity_max',    &
     &                      kloc_alkalinity_max(ng), (/0/), (/0/),       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'alkalinity_load',        &
     &                      alkalinity_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'alkalinity_startload',   &
     &                      alkalinity_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'alkalinity_endload',     &
     &                      alkalinity_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'dissTAp',                &
     &                      dissTAp(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wTAp',                   &  
     &                      wTAp(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'P2Dratio',               &  
     &                      P2Dratio(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'sedloss',                &
     &                      sedloss(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#endif
