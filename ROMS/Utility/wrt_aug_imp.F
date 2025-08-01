#include "cppdefs.h"
#if defined WEAK_CONSTRAINT && defined RPCG
      SUBROUTINE wrt_aug_imp (ng, tile, model, Iinp, Iout, INPncname)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine writes augmented impulse state variables.              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
# ifdef SOLVE3D
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
# endif
      USE strings_mod,     ONLY : FoundError, find_string
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model, Iinp, Iout
!
      character (len=*), intent(in) :: INPncname
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: Irec, MyType, Nrec
      integer :: INPncid, INPvid
      integer :: i, gtype, status, varid
      integer :: ibuffer(2)
!
      real(r8) :: Fmin, Fmax
      real(dp) :: scale
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__

# include "set_bounds.h"
!
      SourceFile=MyFile
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Determine variables to read and their availability.
!-----------------------------------------------------------------------
!
!  Inquire about the dimensions and check for consistency.
!
      CALL netcdf_check_dim (ng, model, INPncname)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      Nrec=rec_size
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, INPncname)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
      scale=1.0_dp
!
!  Process free-surface weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idZtlf), INPvid)) THEN
          gtype=var_flag(INPvid)*r2dvar
          MyType=gtype
          status=nf_fwrite2d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idZtlf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, scale,                 &
# ifdef MASKING
     &                       GRID(ng) % rmask,                          &
# endif
     &                       OCEAN(ng) % tl_zeta(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idZtlf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idZtlf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF

# ifndef SOLVE3D
!
!  Process 2D U-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idUbtf), INPvid)) THEN
          gtype=var_flag(INPvid)*u2dvar
          MyType=gtype
          status=nf_fwrite2d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idUbtf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % umask_full,                     &
#  endif
     &                       OCEAN(ng) % tl_ubar(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idUbtf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,20) TRIM(Vname(1,idUbtf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
!
!  Process 2D V-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idVbtf), INPvid)) THEN
          gtype=var_flag(INPvid)*v2dvar
          MyType=gtype
          status=nf_fwrite2d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idVbtf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, scale,                 &
#  ifdef MASKING
     &                       GRID(ng) % vmask_full,                     &
#  endif
     &                       OCEAN(ng) % tl_vbar(:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idVbtf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idVbtf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
# endif
# ifdef SOLVE3D
!
!  Process 3D U-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idUtlf), INPvid)) THEN
          gtype=var_flag(INPvid)*u3dvar
          MyType=gtype
          status=nf_fwrite3d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idUtlf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % umask_full,                     &
#  endif
     &                       OCEAN(ng) % tl_u(:,:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idUtlf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idUtlf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
!
!  Process 3D V-momentum weak-constraint impulse forcing.
!
        IF (find_string(var_name, n_var, Vname(1,idVtlf), INPvid)) THEN
          gtype=var_flag(INPvid)*v3dvar
          MyType=gtype
          status=nf_fwrite3d(ng, model, TLF(ng)%ncid,                   &
     &                       TLF(ng)%Vid(idVtlf),                       &
     &                       Iout, MyType,                              &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
#  ifdef MASKING
     &                       GRID(ng) % vmask_full,                     &
#  endif
     &                       OCEAN(ng) % tl_v(:,:,:,Iinp))
          IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,30) TRIM(Vname(1,idVtlf)), Irec,            &
     &                          TRIM(TLF(ng)%name)
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        ELSE
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idVtlf)),          &
     &                                  TRIM(INPncname)
          exit_flag=2
          RETURN
        END IF
!
!  Process tracer type variables impulses.
!
        DO i=1,NT(ng)
          IF (find_string(var_name, n_var, Vname(1,idTtlf(i)),          &
     &                    INPvid)) THEN
            gtype=var_flag(INPvid)*r3dvar
            MyType=gtype
            status=nf_fwrite3d(ng, model, TLF(ng)%ncid,                 &
     &                         TLF(ng)%Tid(i),                          &
     &                         Iout, MyType,                            &
     &                         LBi, UBi, LBj, UBj, 1, N(ng), scale,     &
#  ifdef MASKING
     &                         GRID(ng) % rmask_full,                   &
#  endif
     &                         OCEAN(ng) % tl_t(:,:,:,Iinp,i))
            IF (FoundError(status, nf90_noerr, __LINE__, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,30) TRIM(Vname(1,idTtlf(i))), Irec,       &
     &                            TRIM(TLF(ng)%name)
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            END IF
          ELSE
            IF (Master) WRITE (stdout,40) TRIM(Vname(1,idTtlf(i))),     &
     &                                    TRIM(INPncname)
            exit_flag=2
            RETURN
          END IF
        END DO
# endif
!
!-----------------------------------------------------------------------
!  Synchronize impulse NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, model, INPncname, TLF(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      IF (Master)  WRITE (stdout,50) Nrec-1, TRIM(TLF(ng)%name)
!
  10  FORMAT (/,' WRT_AUG_IMP - unable to open input NetCDF file: ',a)
  20  FORMAT (/,' WRT_AUG_IMP - error while reading variable: ',a,2x,   &
     &        'at time record = ',i3,/,17x,'in input NetCDF file: ',a)
  30  FORMAT (/,' WRT_AUG_IMP - error while writing variable: ',a,2x,   &
     &        'at time record = ',i3,/,17x,'into NetCDF file: ',a)
  40  FORMAT (/,' WRT_AUG_IMP - cannot find state variable: ',a,        &
     &        /,12x,'in input NetCDF file: ',a)
  50  FORMAT (4x,'WRT_AUG_IMP - wrote convolved adjoint impulses, ',    &
     &           'records: 1 to ',i0,/,18x,'file: ',a)

      RETURN
      END SUBROUTINE wrt_aug_imp
#else
      SUBROUTINE wrt_aug_imp
      END SUBROUTINE wrt_aug_imp
#endif
