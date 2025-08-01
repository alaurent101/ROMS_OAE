#include "cppdefs.h"
#if defined I4DVAR                || \
    (defined WEAK_CONSTRAINT      && \
     (defined POSTERIOR_EOFS      || defined POSTERIOR_ERROR_I ||\
      defined POSTERIOR_ERROR_F)) || defined LCZ_FINAL
      SUBROUTINE wrt_hessian (ng, kout, nout)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine writes Hessian eigenvectors into output NetCDF file.   !
!                                                                      !
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
# ifdef ADJUST_BOUNDARY
      USE mod_boundary
# endif
      USE mod_forces
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
!
      USE nf_fwrite2d_mod,     ONLY : nf_fwrite2d
# ifdef ADJUST_BOUNDARY
      USE nf_fwrite2d_bry_mod, ONLY : nf_fwrite2d_bry
# endif
# ifdef SOLVE3D
      USE nf_fwrite3d_mod,     ONLY : nf_fwrite3d
#  ifdef ADJUST_BOUNDARY
      USE nf_fwrite3d_bry_mod, ONLY : nf_fwrite3d_bry
#  endif
# endif
      USE strings_mod,         ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, kout, nout
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
# ifdef ADJUST_BOUNDARY
      integer :: LBij, UBij
# endif
      integer :: Fcount, i, j, gfactor, gtype, status
# ifdef SOLVE3D
      integer :: itrc, k
# endif
!
      real(dp) :: scale
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
      SourceFile=MyFile
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
# ifdef ADJUST_BOUNDARY
      LBij=BOUNDS(ng)%LBij
      UBij=BOUNDS(ng)%UBij
# endif
!
!-----------------------------------------------------------------------
!  Write out Hessian eigenvectors.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
# if defined WRITE_WATER && defined MASKING
      gfactor=-1
# else
      gfactor=1
# endif
!
!  Set time record index.
!
      HSS(ng)%Rindex=HSS(ng)%Rindex+1
      Fcount=HSS(ng)%Fcount
      HSS(ng)%Nrec(Fcount)=HSS(ng)%Nrec(Fcount)+1
!
!  Write out model time (s).
!
      CALL netcdf_put_fvar (ng, iADM, HSS(ng)%name,                     &
     &                      TRIM(Vname(1,idtime)), time(ng:),           &
     &                      (/HSS(ng)%Rindex/), (/1/),                  &
     &                      ncid = HSS(ng)%ncid,                        &
     &                      varid = HSS(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out free-surface.
!
      scale=1.0_dp
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Vid(idFsur),   &
     &                   HSS(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
     &                   OCEAN(ng)% ad_zeta(:,:,kout))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idFsur)), HSS(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF

# ifdef ADJUST_BOUNDARY
!
!  Write out free-surface open boundaries.
!
      IF (ANY(Lobc(:,isFsur,ng))) THEN
        scale=1.0_dp
        status=nf_fwrite2d_bry (ng, iADM, HSS(ng)%name, HSS(ng)%ncid,   &
     &                          Vname(1,idSbry(isFsur)),                &
     &                          HSS(ng)%Vid(idSbry(isFsur)),            &
     &                          HSS(ng)%Rindex, r2dvar,                 &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % ad_zeta_obc(LBij:,:,:,   &
     &                                                     kout))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isFsur))),            &
     &                        HSS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 2D U-momentum component.
!
      scale=1.0_dp
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Vid(idUbar),   &
     &                   HSS(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % umask_full,                         &
# endif
     &                   OCEAN(ng) % ad_ubar(:,:,kout))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUbar)), HSS(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF

# ifdef ADJUST_BOUNDARY
!
!  Write out 2D U-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isUbar,ng))) THEN
        scale=1.0_dp
        status=nf_fwrite2d_bry (ng, iADM, HSS(ng)%name, HSS(ng)%ncid,   &
     &                          Vname(1,idSbry(isUbar)),                &
     &                          HSS(ng)%Vid(idSbry(isUbar)),            &
     &                          HSS(ng)%Rindex, u2dvar,                 &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % ad_ubar_obc(LBij:,:,:,   &
     &                                                     kout))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isUbar))),            &
     &                        HSS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 2D V-momentum component.
!
      scale=1.0_dp
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Vid(idVbar),   &
     &                   HSS(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % vmask_full,                         &
# endif
     &                   OCEAN(ng) % ad_vbar(:,:,kout))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVbar)), HSS(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF

# ifdef ADJUST_BOUNDARY
!
!  Write out 2D V-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isVbar,ng))) THEN
        scale=1.0_dp
        status=nf_fwrite2d_bry (ng, iADM, HSS(ng)%name, HSS(ng)%ncid,   &
     &                          Vname(1,idSbry(isVbar)),                &
     &                          HSS(ng)%Vid(idSbry(isVbar)),            &
     &                          HSS(ng)%Rindex, v2dvar,                 &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % ad_vbar_obc(LBij:,:,:,   &
     &                                                     kout))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isVbar))),            &
     &                        HSS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif

# ifdef SOLVE3D
!
!  Write out 3D U-momentum component.
!
      scale=1.0_dp
      gtype=gfactor*u3dvar
      status=nf_fwrite3d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Vid(idUvel),   &
     &                   HSS(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, N(ng), scale,           &
#  ifdef MASKING
     &                   GRID(ng) % umask_full,                         &
#  endif
     &                   OCEAN(ng) % ad_u(:,:,:,nout))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUvel)), HSS(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF

#  ifdef ADJUST_BOUNDARY
!
!  Write out 3D U-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isUvel,ng))) THEN
        scale=1.0_dp
        status=nf_fwrite3d_bry (ng, iADM, HSS(ng)%name, HSS(ng)%ncid,   &
     &                          Vname(1,idSbry(isUvel)),                &
     &                          HSS(ng)%Vid(idSbry(isUvel)),            &
     &                          HSS(ng)%Rindex, u3dvar,                 &
     &                          LBij, UBij, 1, N(ng), Nbrec(ng), scale, &
     &                          BOUNDARY(ng) % ad_u_obc(LBij:,:,:,:,    &
     &                                                  nout))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isUvel))),            &
     &                        HSS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out 3D V-momentum component.
!
      scale=1.0_dp
      gtype=gfactor*v3dvar
      status=nf_fwrite3d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Vid(idVvel),   &
     &                   HSS(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, N(ng), scale,           &
#  ifdef MASKING
     &                   GRID(ng) % vmask_full,                         &
#  endif
     &                   OCEAN(ng) % ad_v(:,:,:,nout))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVvel)), HSS(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF

#  ifdef ADJUST_BOUNDARY
!
!  Write out 3D V-momentum component open boundaries.
!
      IF (ANY(Lobc(:,isVvel,ng))) THEN
        scale=1.0_dp
        status=nf_fwrite3d_bry (ng, iADM, HSS(ng)%name, HSS(ng)%ncid,   &
     &                          Vname(1,idSbry(isVvel)),                &
     &                          HSS(ng)%Vid(idSbry(isVvel)),            &
     &                          HSS(ng)%Rindex, v3dvar,                 &
     &                          LBij, UBij, 1, N(ng), Nbrec(ng), scale, &
     &                          BOUNDARY(ng) % ad_v_obc(LBij:,:,:,:,    &
     &                                                  nout))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idSbry(isVvel))),            &
     &                        HSS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Tid(itrc),   &
     &                     HSS(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     OCEAN(ng) % ad_t(:,:,:,nout,itrc))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,10) TRIM(Vname(1,idTvar(itrc))),              &
     &                        HSS(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END DO

#  ifdef ADJUST_BOUNDARY
!
!  Write out tracers open boundaries.
!
      DO itrc=1,NT(ng)
        IF (ANY(Lobc(:,isTvar(itrc),ng))) THEN
          scale=1.0_dp
          status=nf_fwrite3d_bry (ng, iADM, HSS(ng)%name, HSS(ng)%ncid, &
     &                            Vname(1,idSbry(isTvar(itrc))),        &
     &                            HSS(ng)%Vid(idSbry(isTvar(itrc))),    &
     &                            HSS(ng)%Rindex, r3dvar,               &
     &                            LBij, UBij, 1, N(ng), Nbrec(ng),      &
     &                            scale,                                &
     &                            BOUNDARY(ng) % ad_t_obc(LBij:,:,:,:,  &
     &                                                    nout,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idSbry(isTvar(itrc)))),    &
     &                          HSS(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif

#  ifdef ADJUST_STFLUX
!
!  Write out surface net tracers fluxes. Notice that fluxes have their
!  own fixed time-dimension (of size Nfrec) to allow 4DVAR adjustments
!  at other times in addition to initialization time.
!
      DO itrc=1,NT(ng)
        IF (Lstflux(itrc,ng)) THEN
          scale=1.0_dp
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iADM, HSS(ng)%ncid,                    &
     &                       HSS(ng)%Vid(idTsur(itrc)),                 &
     &                       HSS(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, Nfrec(ng), scale,   &
#   ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#   endif
     &                       FORCES(ng) % ad_tflux(:,:,:,kout,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,10) TRIM(Vname(1,idTsur(itrc))),            &
     &                          HSS(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif
# endif
# ifdef ADJUST_WSTRESS
!
!  Write out surface U-momentum stress.  Notice that the stress has its
!  own fixed time-dimension (of size Nfrec) to allow 4DVAR adjustments
!  at other times in addition to initialization time.
!
      scale=1.0_dp
      gtype=gfactor*u3dvar
      status=nf_fwrite3d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Vid(idUsms),   &
     &                   HSS(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, Nfrec(ng), scale,       &
#  ifdef MASKING
     &                   GRID(ng) % umask,                              &
#  endif
     &                   FORCES(ng) % ad_ustr(:,:,:,kout))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idUsms)), HSS(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out surface V-momentum stress.
!
      scale=1.0_dp
      gtype=gfactor*v3dvar
      status=nf_fwrite3d(ng, iADM, HSS(ng)%ncid, HSS(ng)%Vid(idVsms),   &
     &                   HSS(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, Nfrec(ng), scale,       &
#  ifdef MASKING
     &                   GRID(ng) % vmask,                              &
#  endif
     &                   FORCES(ng) % ad_vstr(:,:,:,kout))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,10) TRIM(Vname(1,idVsms)), HSS(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
!
!-----------------------------------------------------------------------
!  Synchronize Hessian eigenvectors NetCDF file to disk to allow other
!  processes to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, iADM, HSS(ng)%name, HSS(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# ifdef SOLVE3D
#  ifdef NESTING
      IF (Master) WRITE (stdout,20) kout, nout, HSS(ng)%Rindex, ng
#  else
      IF (Master) WRITE (stdout,20) kout, nout, HSS(ng)%Rindex
#  endif
# else
#  ifdef NESTING
      IF (Master) WRITE (stdout,20) kout, HSS(ng)%Rindex, ng
#  else
      IF (Master) WRITE (stdout,20) kout, HSS(ng)%Rindex
#  endif
# endif
!
  10  FORMAT (/,' WRT_HESSIAN - error while writing variable: ',a,/,    &
     &        15x,'into Hessian NetCDF file for time record: ',i0)
# ifdef SOLVE3D
  20  FORMAT (6x,'WRT_HESSIAN - wrote Hessian', t39,                    &
#  ifdef NESTING
     &        'fields (Index=',i1,',',i1,') in record = ',i0,t92,i2.2)
#  else
     &        'fields (Index=',i1,',',i1,') in record = ',i0)
#  endif
# else
  20  FORMAT (6x,'WRT_HESSIAN - wrote Hessian', t39,                    &
#  ifdef NESTING
     &        'fields (Index=',i1,')   in record = ',i0,t92,i2.2)
#  else
     &        'fields (Index=',i1,')   in record = ',i0)
#  endif
# endif
      RETURN
      END SUBROUTINE wrt_hessian
#else
      SUBROUTINE wrt_hessian
      RETURN
      END SUBROUTINE wrt_hessian
#endif
