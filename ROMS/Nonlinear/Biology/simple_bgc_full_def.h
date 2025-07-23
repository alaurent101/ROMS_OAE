/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2021 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Defines The Simple BGC model input parameters in output NetCDF    **
**  files. It is included in routine "def_info.F".                    **
**                                                                    **
************************************************************************
*/

!
!  Define Simple BGC model parameters.
!
      Vinfo( 1)='BioIter'
      Vinfo( 2)='number of iterations to achieve convergence'
      status=def_var(ng, model, ncid, varid, nf90_int,                  &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='SOC'
      Vinfo( 2)='sediment oxygen consumption'
      Vinfo( 3)='millimole meter-2 day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

#ifdef TEMP_RATES
      Vinfo( 1)='WOC0'
      Vinfo( 2)='water column respiration rate at T=0C'
      Vinfo( 3)='millimole meter-3 day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3 || defined WOC_H23
      Vinfo( 1)='WOC20'
      Vinfo( 2)='water column respiration rate at T=0C (HRM2)'
      Vinfo( 3)='millimole meter-3 day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='WOC30'
      Vinfo( 2)='water column respiration rate at T=0C (HRM3)'
      Vinfo( 3)='millimole meter-3 day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# endif
#else
      Vinfo( 1)='WOC'
      Vinfo( 2)='water column respiration rate'
      Vinfo( 3)='millimole meter-3 day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3 || defined WOC_H23
      Vinfo( 1)='WOC2'
      Vinfo( 2)='water column respiration rate (HRM2)'
      Vinfo( 3)='millimole meter-3 day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='WOC3'
      Vinfo( 2)='water column respiration rate (HRM3)'
      Vinfo( 3)='millimole meter-3 day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# endif
#endif

      Vinfo( 1)='PhyNC'
      Vinfo( 2)='phytoplankton Nitrogen:Carbon ratio'
      Vinfo( 3)='mole_N mole_C-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='pCO2air'
      Vinfo( 2)='partial pressure of CO2 in the air'
      Vinfo( 3)='parts per million by volume'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

#ifdef TALK_ADDITION
      Vinfo( 1)='iloc_alkalinity'
      Vinfo( 2)='longitude (i) index of the model grid cell where alkalinity is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_alkalinity'
      Vinfo( 2)='latitude (j) index of the model grid cell where alkalinity is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_alkalinity_min'
      Vinfo( 2)='minimum vertical (k) index of the model grid cell where alkalinity is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='kloc_alkalinity_max'
      Vinfo( 2)='maximum vertical (k) index of the model grid cell where alkalinity is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='alkalinity_load'
      Vinfo( 2)='alkalinity added to the grid cell per day'
      Vinfo( 3)='unit of alkalinity/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='alkalinity_startload'
      Vinfo( 2)='starting day of alkalinity load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='alkalinity_endload'
      Vinfo( 2)='ending day of alkalinity load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='disso1'
      Vinfo( 2)='dissolution rate of particle1'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='wPar1'
      Vinfo( 2)='vertical sinking velocity for particle1'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='P2Dratio'
      Vinfo( 2)='ratio of particles in alkalinity load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='sedloss'
      Vinfo( 2)='fraction of particle1 lost to the sediment'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#endif

