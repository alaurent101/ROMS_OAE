#include "cppdefs.h"
#ifdef FLOATS
      SUBROUTINE wrt_floats (ng)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine writes simulated drifter trajectories into floats   !
!  NetCDF file.                                                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
# ifdef BIOLOGY
      USE mod_biology
# endif
      USE mod_floats
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
      USE mod_stepping
!
      USE strings_mod, ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      integer :: Fcount, itrc, l, status

      real(r8), dimension(Nfloats(ng)) :: Tout
# ifdef FLOAT_PROFILE

      real(r8), dimension(Nfloats(ng),N(ng)) :: Tout3
      real(r8), dimension(Nfloats(ng),N(ng)+1) :: ToutW

#  if defined DIAGNOSTICS_BIO && defined BIO_FENNEL
      integer :: ivar, ifield
#  endif
# endif
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out station data at RHO-points.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Set time record index.
!
      FLT(ng)%Rindex=FLT(ng)%Rindex+1
      Fcount=FLT(ng)%Fcount
      FLT(ng)%Nrec(Fcount)=FLT(ng)%Nrec(Fcount)+1
!
!  Write out model time (s).
!
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      TRIM(Vname(1,idtime)), time(ng:),           &
     &                      (/FLT(ng)%Rindex/), (/1/),                  &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out floats X-grid locations.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(ixgrd,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'Xgrid', Tout,                              &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idXgrd))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out floats Y-grid locations.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(iygrd,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'Ygrid', Tout,                              &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idYgrd))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# ifdef SOLVE3D
!
!  Write out floats Z-grid info.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(izgrd,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'Zgrid', Tout,                              &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idZgrd))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#  ifdef FLOAT_PROFILE
!
!  Write out floats Zr and Zw profiles.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout3(l,:)=DRIFTER(ng)%track3(izgrd,nf(ng),:,l)
        ELSE
          Tout3(l,:)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'Zr', Tout3,                                &
     &                      (/1,1,FLT(ng)%Rindex/),                     &
     &                      (/Nfloats(ng),N(ng),1/),                    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idZrPr))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          ToutW(l,:)=DRIFTER(ng)%trackw(nf(ng),:,l)
        ELSE
          ToutW(l,:)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'Zw', ToutW,                                &
     &                      (/1,1,FLT(ng)%Rindex/),                     &
     &                      (/Nfloats(ng),N(ng)+1,1/),                  &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idZwPr))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#  endif
# endif
!
!  Write out floats (lon,lat) or (x,y) locations.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(iflon,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      IF (spherical) THEN
        CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                   &
     &                        'lon', Tout,                              &
     &                        (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),  &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        varid = FLT(ng)%Vid(idglon))
      ELSE
        CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                   &
     &                        'x', Tout,                                &
     &                        (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),  &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        varid = FLT(ng)%Vid(idglon))
      END IF
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(iflat,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      IF (spherical) THEN
        CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                   &
     &                        'lat', Tout,                              &
     &                        (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),  &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        varid = FLT(ng)%Vid(idglat))
      ELSE
        CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                   &
     &                        'y', Tout,                                &
     &                        (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),  &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        varid = FLT(ng)%Vid(idglat))
      END IF
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# ifdef SOLVE3D
!
!  Write out floats depths.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(idpth,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'depth', Tout,                              &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(iddpth))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out density anomaly.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(ifden,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      TRIM(Vname(1,idDano)), Tout,                &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idDano))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
#  ifdef FLOAT_PROFILE
!  Write out 3D tracer type variables.
!
      DO itrc=1,NT(ng)
        DO l=1,Nfloats(ng)
          IF (DRIFTER(ng)%bounded(l)) THEN
            Tout3(l,:)=DRIFTER(ng)%track3(ifTvar(itrc),nf(ng),:,l)
          ELSE
            Tout3(l,:)=spval
          END IF
        END DO
        CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                   &
     &                        TRIM(Vname(1,idTvar(itrc))), Tout3,       &
     &                        (/1,1,FLT(ng)%Rindex/),                   &
     &                        (/Nfloats(ng),N(ng),1/),                  &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        varid = FLT(ng)%Tid(itrc))
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END DO
#  else
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        DO l=1,Nfloats(ng)
          IF (DRIFTER(ng)%bounded(l)) THEN
            Tout(l)=DRIFTER(ng)%track(ifTvar(itrc),nf(ng),l)
          ELSE
            Tout(l)=spval
          END IF
        END DO
        CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                   &
     &                        TRIM(Vname(1,idTvar(itrc))), Tout,        &
     &                        (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),  &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        varid = FLT(ng)%Tid(itrc))
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END DO
#  endif
!
#  ifdef FLOAT_PROFILE
#   if defined DIAGNOSTICS_BIO && defined BIO_FENNEL
!  Write out 2D biology diagnostic type variables.
!
      DO ivar=1,NDbio2d
        ifield=iDbio2(ivar)
        IF (Dout(ifield,ng)) THEN
          DO l=1,Nfloats(ng)
            IF (DRIFTER(ng)%bounded(l)) THEN
              Tout(l)=DRIFTER(ng)%track(ifD2var(ivar),nf(ng),l)
            ELSE
              Tout(l)=spval
            END IF
          END DO
          CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                 &
     &                        TRIM(Vname(1,ifield)), Tout,              &
     &                        (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),  &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        varid = FLT(ng)%Vid(ifield))
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
      END DO
!  Write out 3D biology diagnostic type variables.
!
      DO ivar=1,NDbio3d
        ifield=iDbio3(ivar)
        IF (Dout(ifield,ng)) THEN
          DO l=1,Nfloats(ng)
            IF (DRIFTER(ng)%bounded(l)) THEN
              Tout3(l,:)=DRIFTER(ng)%track3(ifD3var(ivar),nf(ng),:,l)
            ELSE
              Tout3(l,:)=spval
            END IF
          END DO
          CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                 &
     &                          TRIM(Vname(1,ifield)), Tout3,           &
     &                          (/1,1,FLT(ng)%Rindex/),                 &
     &                          (/Nfloats(ng),N(ng),1/),                &
     &                          ncid = FLT(ng)%ncid,                    &
     &                          varid = FLT(ng)%Vid(ifield))
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
      END DO
#   endif
#  endif
# endif

# ifdef FLOAT_OYSTER
!
!  Write out biological float swimming time.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(iswim,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'swim_time', Tout,                          &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = Flt(ng)%Vid(idswim))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out biological float vertical velocity.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(iwbio,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'w_bio', Tout,                              &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idwbio))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out biological float size (length).
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
                Tout(l)=DRIFTER(ng)%track(isizf,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'bio_size', Tout,                           &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idsize))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out biological float sinking velocity.
!
      DO l=1,Nfloats(ng)
        IF (DRIFTER(ng)%bounded(l)) THEN
          Tout(l)=DRIFTER(ng)%track(iwsin,nf(ng),l)
        ELSE
          Tout(l)=spval
        END IF
      END DO
      CALL netcdf_put_fvar (ng, iNLM, FLT(ng)%name,                     &
     &                      'bio_sink', Tout,                           &
     &                      (/1,FLT(ng)%Rindex/), (/Nfloats(ng),1/),    &
     &                      ncid = FLT(ng)%ncid,                        &
     &                      varid = FLT(ng)%Vid(idwsin))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
# endif
!
!-----------------------------------------------------------------------
!  Synchronize floats NetCDF file to disk.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, iNLM, FLT(ng)%name, FLT(ng)%ncid)

#else
      SUBROUTINE wrt_floats
#endif
      RETURN
      END SUBROUTINE wrt_floats
