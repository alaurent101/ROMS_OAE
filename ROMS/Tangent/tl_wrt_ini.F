#include "cppdefs.h"
#if (defined TANGENT || defined TL_IOMS) && defined FOUR_DVAR
      SUBROUTINE tl_wrt_ini (ng, Tindex, OutRec)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine writes tangent linear initial conditions into tangent  !
!  linear initial NetCDF file.                                         !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number.                                   !
!     Tindex     State variables time index to write.                  !
!     OutRec     NetCDF file unlimited dimension record to write.      !
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
# if defined ADJUST_WSTRESS || defined ADJUST_STFLUX
      USE mod_forces
# endif
      USE mod_fourdvar
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
# if defined SEDIMENT_NOT_YET || defined BBL_MODEL_NOT_YET
      USE mod_sediment
# endif
      USE mod_stepping
!
# ifdef DISTRIBUTE
      USE distribute_mod,      ONLY : mp_bcasti
# endif
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
      integer, intent(in) :: ng, Tindex, OutRec
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
# ifdef ADJUST_BOUNDARY
      integer :: LBij, UBij
# endif
      integer :: gfactor, gtype, i, itrc, status, varid
!
      real(dp) :: my_time, scale
!
# if defined RPCG
      character (len=35) :: string
# elif defined SP4DVAR
      character (len=31) :: string
# else
      character (len=15) :: string
# endif
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
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
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out tangent linear initial conditions.
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
!  Write out model time (s). Use the "tdays" variable here because of
!  the management of the "time" variable due to nesting.
!
      my_time=tdays(ng)*day2sec

      CALL netcdf_put_fvar (ng, iTLM, ITL(ng)%name,                     &
     &                      TRIM(Vname(1,idtime)), my_time,             &
     &                      (/OutRec/), (/1/),                          &
     &                      ncid = ITL(ng)%ncid,                        &
     &                      varid = ITL(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Write out free-surface (m)
!
      scale=1.0_dp
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Vid(idFsur),   &
     &                   OutRec, gtype,                                 &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
# ifdef WET_DRY
     &                   OCEAN(ng) % tl_zeta(:,:,Tindex),               &
     &                   SetFillVal = .FALSE.)
# else
     &                   OCEAN(ng) % tl_zeta(:,:,Tindex))
# endif
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idFsur)), OutRec
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
        status=nf_fwrite2d_bry (ng, iTLM, ITL(ng)%name, ITL(ng)%ncid,   &
     &                          Vname(1,idSbry(isFsur)),                &
     &                          ITL(ng)%Vid(idSbry(isFsur)),            &
     &                          OutRec, r2dvar,                         &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % tl_zeta_obc(LBij:,:,:,   &
     &                                                     Tindex))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idSbry(isFsur))), OutRec
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 2D U-momentum component (m/s).
!
      scale=1.0_dp
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Vid(idUbar),   &
     &                   OutRec, gtype,                                 &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % umask_full,                         &
# endif
     &                   OCEAN(ng) % tl_ubar(:,:,Tindex))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idUbar)), OutRec
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
        status=nf_fwrite2d_bry (ng, iTLM, ITL(ng)%name, ITL(ng)%ncid,   &
     &                          Vname(1,idSbry(isUbar)),                &
     &                          ITL(ng)%Vid(idSbry(isUbar)),            &
     &                          OutRec, u2dvar,                         &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % tl_ubar_obc(LBij:,:,:,   &
     &                                                     Tindex))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idSbry(isUbar))), OutRec
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      scale=1.0_dp
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Vid(idVbar),   &
     &                   OutRec, gtype,                                 &
     &                   LBi, UBi, LBj, UBj, scale,                     &
# ifdef MASKING
     &                   GRID(ng) % vmask_full,                         &
# endif
     &                   OCEAN(ng) % tl_vbar(:,:,Tindex))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idVbar)), OutRec
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
        status=nf_fwrite2d_bry (ng, iTLM, ITL(ng)%name, ITL(ng)%ncid,   &
     &                          Vname(1,idSbry(isVbar)),                &
     &                          ITL(ng)%Vid(idSbry(isVbar)),            &
     &                          OutRec, v2dvar,                         &
     &                          LBij, UBij, Nbrec(ng), scale,           &
     &                          BOUNDARY(ng) % tl_vbar_obc(LBij:,:,:,   &
     &                                                     Tindex))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idSbry(isVbar))), OutRec
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
# endif
# ifdef ADJUST_WSTRESS
!
!  Write out surface U-momentum stress.  Notice that the stress has its
!  own fixed time-dimension (of size Nfrec) to allow 4DVAR adjustments
!  at other times in addition to initialization time.
!
      scale=1.0_dp                          ! m2/s2
      gtype=gfactor*u3dvar
      status=nf_fwrite3d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Vid(idUsms),   &
     &                   OutRec, gtype,                                 &
     &                   LBi, UBi, LBj, UBj, 1, Nfrec(ng), scale,       &
#  ifdef MASKING
     &                   GRID(ng) % umask,                              &
#  endif
     &                   FORCES(ng) % tl_ustr(:,:,:,Tindex))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idUsms)), OutRec
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out surface V-momentum stress.
!
      scale=1.0_dp                          ! m2/s2
      gtype=gfactor*v3dvar
      status=nf_fwrite3d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Vid(idVsms),   &
     &                   OutRec, gtype,                                 &
     &                   LBi, UBi, LBj, UBj, 1, Nfrec(ng), scale,       &
#  ifdef MASKING
     &                   GRID(ng) % vmask,                              &
#  endif
     &                   FORCES(ng) % tl_vstr(:,:,:,Tindex))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idVsms)), OutRec
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
# endif
# ifdef SOLVE3D
!
!  Write out 3D U-momentum component (m/s).
!
      scale=1.0_dp
      gtype=gfactor*u3dvar
      status=nf_fwrite3d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Vid(idUvel),   &
     &                   OutRec, gtype,                                 &
     &                   LBi, UBi, LBj, UBj, 1, N(ng), scale,           &
#  ifdef MASKING
     &                   GRID(ng) % umask_full,                         &
#  endif
     &                   OCEAN(ng) % tl_u(:,:,:,Tindex))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idUvel)), OutRec
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
        status=nf_fwrite3d_bry (ng, iTLM, ITL(ng)%name, ITL(ng)%ncid,   &
     &                          Vname(1,idSbry(isUvel)),                &
     &                          ITL(ng)%Vid(idSbry(isUvel)),            &
     &                          OutRec, u3dvar,                         &
     &                          LBij, UBij, 1, N(ng), Nbrec(ng), scale, &
     &                          BOUNDARY(ng) % tl_u_obc(LBij:,:,:,:,    &
     &                                                  Tindex))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idSbry(isUvel))), OutRec
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
#  endif
!
!  Write out 3D V-momentum component (m/s).
!
      scale=1.0_dp
      gtype=gfactor*v3dvar
      status=nf_fwrite3d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Vid(idVvel),   &
     &                   OutRec, gtype,                                 &
     &                   LBi, UBi, LBj, UBj, 1, N(ng), scale,           &
#  ifdef MASKING
     &                   GRID(ng) % vmask_full,                         &
#  endif
     &                   OCEAN(ng) % tl_v(:,:,:,Tindex))
      IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idVvel)), OutRec
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
        status=nf_fwrite3d_bry (ng, iTLM, ITL(ng)%name, ITL(ng)%ncid,   &
     &                          Vname(1,idSbry(isVvel)),                &
     &                          ITL(ng)%Vid(idSbry(isVvel)),            &
     &                          OutRec, v3dvar,                         &
     &                          LBij, UBij, 1, N(ng), Nbrec(ng), scale, &
     &                          BOUNDARY(ng) % tl_v_obc(LBij:,:,:,:,    &
     &                                                  Tindex))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idSbry(isVvel))), OutRec
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
        status=nf_fwrite3d(ng, iTLM, ITL(ng)%ncid, ITL(ng)%Tid(itrc),   &
     &                     OutRec, gtype,                               &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     OCEAN(ng) % tl_t(:,:,:,Tindex,itrc))
        IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idTvar(itrc))), OutRec
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END DO

#  ifdef ADJUST_BOUNDARY
!
!  Write out 3D tracers open boundaries.
!
      DO itrc=1,NT(ng)
        IF (ANY(Lobc(:,isTvar(itrc),ng))) THEN
          scale=1.0_dp
          status=nf_fwrite3d_bry (ng, iTLM, ITL(ng)%name, ITL(ng)%ncid, &
     &                            Vname(1,idSbry(isTvar(itrc))),        &
     &                            ITL(ng)%Vid(idSbry(isTvar(itrc))),    &
     &                            OutRec, r3dvar,                       &
     &                            LBij, UBij, 1, N(ng), Nbrec(ng),      &
     &                            scale,                                &
     &                            BOUNDARY(ng) % tl_t_obc(LBij:,:,:,:,  &
     &                                                    Tindex,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idSbry(isTvar(itrc)))),    &
     &                          OutRec
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
          scale=1.0_dp                      ! kinematic flux units
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iTLM, ITL(ng)%ncid,                    &
     &                       ITL(ng)%Vid(idTsur(itrc)),                 &
     &                       OutRec, gtype,                             &
     &                       LBi, UBi, LBj, UBj, 1, Nfrec(ng), scale,   &
#   ifdef MASKING
     &                       GRID(ng) % rmask,                          &
#   endif
     &                       FORCES(ng) % tl_tflux(:,:,:,Tindex,itrc))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idTsur(itrc))),            &
     &                          OutRec
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
#  endif
# endif

# if defined I4DVAR || defined BACKGROUND
!
!-----------------------------------------------------------------------
!  If 4D-Var increment phase, write out cost functions to DAV(ng)%name
!  NetCDF file.
!-----------------------------------------------------------------------

#  if defined I4DVAR
!
!  Write out tangent linear model misfit cost function. Notice that it
!  is written into DAV(ng)%name file instead of ITL(ng)%name file.
!
      IF (LwrtCost(ng)) THEN
        CALL netcdf_put_fvar (ng, iTLM, DAV(ng)%name,                   &
     &                        'TLcost_function',                        &
     &                        FOURDVAR(ng)%ObsCost(0),                  &
     &                        (/Nrun/), (/1/),                          &
     &                        ncid = DAV(ng)%ncid)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#  endif

#  ifdef BACKGROUND
!
!  Write out background misfit cost function, Jb. Notice that it is
!  written into DAV(ng)%name file instead of ITL(ng)%name file.
!
      IF (LwrtCost(ng)) THEN
        CALL netcdf_put_fvar (ng, iTLM, DAV(ng)%name,                   &
     &                        'back_function',                          &
     &                        FOURDVAR(ng)%BackCost(0),                 &
     &                        (/Nrun/), (/1/),                          &
     &                        ncid = DAV(ng)%ncid)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#  endif

#  if defined I4DVAR
!
!  Write out current optimal, normalized cost function minimum.  Notice
!  that it is written into DAV(ng)%name file instead of ITL(ng)%name
!  file.
!
      IF (LwrtCost(ng)) THEN
        CALL netcdf_put_fvar (ng, iTLM, DAV(ng)%name, 'Jmin',           &
     &                        Optimality(ng:), (/Nrun/), (/1/),         &
     &                        ncid = DAV(ng)%ncid)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      END IF
#  endif
# endif
!
!-----------------------------------------------------------------------
!  Synchronize tangent linear initial NetCDF file to disk to allow other
!  processes to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, iTLM, ITL(ng)%name, ITL(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

# if defined I4DVAR || defined BACKGROUND
      CALL netcdf_sync (ng, iTLM, DAV(ng)%name, DAV(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
# endif
!
      IF (Master) THEN
# if defined RPCG
        IF (OutRec.eq.1) THEN
          string='inner-loop initial fields          '
        ELSE IF (OutRec.eq.2) THEN
          string='final outer-loop increments        '
        ELSE IF (OutRec.eq.3) THEN
          string='sum of final outer-loop increments '
        ELSE IF (OutRec.eq.4) THEN
          string='sum of adjoint solutions           '
        ELSE IF (OutRec.eq.5) THEN
          string='augmented correction term          '
        END IF
# elif defined SP4DVAR
        IF (OutRec.eq.1) THEN
          string='TLM initial fields             '
        ELSE IF ((2.le.OutRec).and.(OutRec.le.Nsaddle+1)) THEN
          string='TLM saddle-point starting field'
        ELSE IF ((Nsaddle+2.le.OutRec).and.(OutRec.le.2*Nsaddle+2)) THEN
          string='ADM saddle-point starting field'
        END IF
# else
        IF (OutRec.eq.1) THEN
          string='initial  fields'
        ELSE IF (OutRec.eq.2) THEN
          string='v-increments   '
        ELSE IF (OutRec.eq.3) THEN
          string='v-increments   '
        ELSE IF (OutRec.eq.4) THEN
          string='v-summations   '
        ELSE IF (OutRec.eq.5) THEN
          string='x-increments   '
        END IF
# endif
# ifdef SOLVE3D
        WRITE (stdout,30) string, outer, inner, Tindex, Tindex, OutRec
# else
        WRITE (stdout,30) string, outer, inner, Tindex, OutRec
# endif
      END IF
!
  10  FORMAT (/,' TL_WRT_INI - unable to open initial NetCDF file: ',a)
  20  FORMAT (/,' TL_WRT_INI - error while writing variable: ',a,/,14x, &
     &        'into tangent initial NetCDF file for time record: ',i0)
# ifdef SOLVE3D
  30  FORMAT (3x,'TL_WRT_INI   - wrote ',a,' (Outer=',i2.2,', Inner=',  &
     &        i3.3,', Index=',i0,',',i0,', Rec=',i0,')')
# else
  30  FORMAT (3x,'TL_WRT_INI   - wrote ',a,' (Outer=',i2.2,', Inner=',  &
     &        i3.3,', Index=',i0,', Rec=',i0,')')
# endif

#else
      SUBROUTINE tl_wrt_ini
#endif
      RETURN
      END SUBROUTINE tl_wrt_ini
