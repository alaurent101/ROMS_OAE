/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2021 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Writes the dissolution model input parameters into output NetCDF  **
**  files. It is included in routine "wrt_info.F".                    **
**                                                                    **
************************************************************************
*/

!
!  Write out the dissolution model parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'BioIter',               &
     &                      BioIter(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso1',                &
     &                      disso1(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle1',        &
     &                      iloc_particle1(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle1',        &
     &                      jloc_particle1(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle1',        &
     &                      kloc_particle1(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle1_load',        &
     &                      particle1_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle1_startload',   &
     &                      particle1_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle1_endload',     &
     &                      particle1_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar1',                 &  
     &                      wPar1(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

#   ifdef FULL_DISSOLVE
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_dissolve0',        &
     &                      iloc_dissolve0(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_dissolve0',        &
     &                      jloc_dissolve0(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_dissolve0',        &
     &                      kloc_dissolve0(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'dissolve0_load',        &
     &                      dissolve0_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'dissolve0_startload',   &
     &                      dissolve0_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'dissolve0_endload',     &
     &                      dissolve0_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#   endif

#   ifdef MULTI_PARTICLES
      CALL netcdf_put_fvar (ng, model, ncname, 'disso2',                &
     &                      disso2(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle2',        &
     &                      iloc_particle2(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle2',        &
     &                      jloc_particle2(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle2',        &
     &                      kloc_particle2(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle2_load',        &
     &                      particle2_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle2_startload',   &
     &                      particle2_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle2_endload',     &
     &                      particle2_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar2',                 &  
     &                      wPar2(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso3',                &
     &                      disso3(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle3',        &
     &                      iloc_particle3(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle3',        &
     &                      jloc_particle3(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle3',        &
     &                      kloc_particle3(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle3_load',        &
     &                      particle3_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle3_startload',   &
     &                      particle3_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle3_endload',     &
     &                      particle3_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar3',                 &  
     &                      wPar3(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso4',                &
     &                      disso4(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle4',        &
     &                      iloc_particle4(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle4',        &
     &                      jloc_particle4(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle4',        &
     &                      kloc_particle4(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle4_load',        &
     &                      particle4_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle4_startload',   &
     &                      particle4_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle4_endload',     &
     &                      particle4_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar4',                 &  
     &                      wPar4(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso5',                &
     &                      disso5(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle5',        &
     &                      iloc_particle5(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle5',        &
     &                      jloc_particle5(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle5',        &
     &                      kloc_particle5(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle5_load',        &
     &                      particle5_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle5_startload',   &
     &                      particle5_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle5_endload',     &
     &                      particle5_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar5',                 &  
     &                      wPar5(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso6',                &
     &                      disso6(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle6',        &
     &                      iloc_particle6(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle6',        &
     &                      jloc_particle6(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle6',        &
     &                      kloc_particle6(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle6_load',        &
     &                      particle6_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle6_startload',   &
     &                      particle6_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle6_endload',     &
     &                      particle6_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar6',                 &  
     &                      wPar6(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso7',                &
     &                      disso7(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle7',        &
     &                      iloc_particle7(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle7',        &
     &                      jloc_particle7(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle7',        &
     &                      kloc_particle7(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle7_load',        &
     &                      particle7_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle7_startload',   &
     &                      particle7_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle7_endload',     &
     &                      particle7_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar7',                 &  
     &                      wPar7(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso8',                &
     &                      disso8(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle8',        &
     &                      iloc_particle8(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle8',        &
     &                      jloc_particle8(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle8',        &
     &                      kloc_particle8(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle8_load',        &
     &                      particle8_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle8_startload',   &
     &                      particle8_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle8_endload',     &
     &                      particle8_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar8',                 &  
     &                      wPar8(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      CALL netcdf_put_fvar (ng, model, ncname, 'disso9',                &
     &                      disso9(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'iloc_particle9',        &
     &                      iloc_particle9(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'jloc_particle9',        &
     &                      jloc_particle9(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'kloc_particle9',        &
     &                      kloc_particle9(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle9_load',        &
     &                      particle9_load(ng), (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle9_startload',   &
     &                      particle9_startload(ng), (/0/), (/0/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'particle9_endload',     &
     &                      particle9_endload(ng), (/0/), (/0/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      CALL netcdf_put_fvar (ng, model, ncname, 'wPar9',                 &  
     &                      wPar9(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#   endif

