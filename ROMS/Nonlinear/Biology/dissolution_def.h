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


      Vinfo( 1)='disso1'
      Vinfo( 2)='dissolution rate of particle1'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle1'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle1 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle1'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle1 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle1'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle1 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle1_load'
      Vinfo( 2)='particle1 added to the grid cell per day'
      Vinfo( 3)='unit of particle1/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle1_startload'
      Vinfo( 2)='starting day of particle1 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle1_endload'
      Vinfo( 2)='ending day of particle1 load'
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

#   ifdef FULL_DISSOLVE
      Vinfo( 1)='iloc_dissolve0'
      Vinfo( 2)='longitude (i) index of the model grid cell where dissolve0 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_dissolve0'
      Vinfo( 2)='latitude (j) index of the model grid cell where dissolve0 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_dissolve0'
      Vinfo( 2)='vertical (k) index of the model grid cell where dissolve0 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='dissolve0_load'
      Vinfo( 2)='dissolve0 added to the grid cell per day'
      Vinfo( 3)='unit of dissolve0/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='dissolve0_startload'
      Vinfo( 2)='starting day of dissolve0 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='dissolve0_endload'
      Vinfo( 2)='ending day of dissolve0 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#   endif


#   ifdef MULTI_PARTICLES
      Vinfo( 1)='disso2'
      Vinfo( 2)='dissolution rate of particle2'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle2'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle2 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle2'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle2 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle2'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle2 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle2_load'
      Vinfo( 2)='particle2 added to the grid cell per day'
      Vinfo( 3)='unit of particle2/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle2_startload'
      Vinfo( 2)='starting day of particle2 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle2_endload'
      Vinfo( 2)='ending day of particle2 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar2'
      Vinfo( 2)='vertical sinking velocity for particle2'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN


      Vinfo( 1)='disso3'
      Vinfo( 2)='dissolution rate of particle3'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle3'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle3 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle3'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle3 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle3'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle3 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle3_load'
      Vinfo( 2)='particle3 added to the grid cell per day'
      Vinfo( 3)='unit of particle3/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle3_startload'
      Vinfo( 2)='starting day of particle3 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle3_endload'
      Vinfo( 2)='ending day of particle3 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar3'
      Vinfo( 2)='vertical sinking velocity for particle3'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN


      Vinfo( 1)='disso4'
      Vinfo( 2)='dissolution rate of particle4'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle4'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle4 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle4'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle4 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle4'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle4 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle4_load'
      Vinfo( 2)='particle4 added to the grid cell per day'
      Vinfo( 3)='unit of particle4/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle4_startload'
      Vinfo( 2)='starting day of particle4 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle4_endload'
      Vinfo( 2)='ending day of particle4 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar4'
      Vinfo( 2)='vertical sinking velocity for particle4'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN


      
      Vinfo( 1)='disso5'
      Vinfo( 2)='dissolution rate of particle5'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle5'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle5 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle5'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle5 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle5'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle5 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle5_load'
      Vinfo( 2)='particle5 added to the grid cell per day'
      Vinfo( 3)='unit of particle5/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle5_startload'
      Vinfo( 2)='starting day of particle5 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle5_endload'
      Vinfo( 2)='ending day of particle5 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar5'
      Vinfo( 2)='vertical sinking velocity for particle5'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN



      Vinfo( 1)='disso6'
      Vinfo( 2)='dissolution rate of particle6'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle6'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle6 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle6'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle6 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle6'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle6 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle6_load'
      Vinfo( 2)='particle6 added to the grid cell per day'
      Vinfo( 3)='unit of particle6/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle6_startload'
      Vinfo( 2)='starting day of particle6 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle6_endload'
      Vinfo( 2)='ending day of particle6 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar6'
      Vinfo( 2)='vertical sinking velocity for particle6'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN



      Vinfo( 1)='disso7'
      Vinfo( 2)='dissolution rate of particle7'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle7'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle7 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle7'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle7 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle7'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle7 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle7_load'
      Vinfo( 2)='particle7 added to the grid cell per day'
      Vinfo( 3)='unit of particle7/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle7_startload'
      Vinfo( 2)='starting day of particle7 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle7_endload'
      Vinfo( 2)='ending day of particle7 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar7'
      Vinfo( 2)='vertical sinking velocity for particle7'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN


      Vinfo( 1)='disso8'
      Vinfo( 2)='dissolution rate of particle8'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle8'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle8 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle8'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle8 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle8'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle8 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle8_load'
      Vinfo( 2)='particle8 added to the grid cell per day'
      Vinfo( 3)='unit of particle8/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle8_startload'
      Vinfo( 2)='starting day of particle8 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle8_endload'
      Vinfo( 2)='ending day of particle8 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar8'
      Vinfo( 2)='vertical sinking velocity for particle8'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN


      Vinfo( 1)='disso9'
      Vinfo( 2)='dissolution rate of particle9'
      Vinfo( 3)='day-1'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='iloc_particle9'
      Vinfo( 2)='longitude (i) index of the model grid cell where particle9 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='jloc_particle9'
      Vinfo( 2)='latitude (j) index of the model grid cell where particle9 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='kloc_particle9'
      Vinfo( 2)='vertical (k) index of the model grid cell where particle9 is added'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      Vinfo( 1)='particle9_load'
      Vinfo( 2)='particle9 added to the grid cell per day'
      Vinfo( 3)='unit of particle9/m2/day' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle9_startload'
      Vinfo( 2)='starting day of particle9 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='particle9_endload'
      Vinfo( 2)='ending day of particle9 load'
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      
      Vinfo( 1)='wPar9'
      Vinfo( 2)='vertical sinking velocity for particle9'
      Vinfo( 3)='meter day-1' 
      status=def_var(ng, model, ncid, varid, NF_TYPE,                   &
     &               1, (/0/), Aval, Vinfo, ncname,                     &
     &               SetParAccess = .FALSE.)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#   endif
