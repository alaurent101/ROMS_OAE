#include "cppdefs.h"
#ifdef ADJOINT
      SUBROUTINE ad_get_idata (ng)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads input data that needs to be obtained only once.  !
!                                                                      !
!  Currently,  this routine is only executed in serial mode by the     !
!  main thread.                                                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_parallel
      USE mod_scalars
      USE mod_sources
      USE mod_stepping
# if defined SSH_TIDES_NOT_YET || defined UV_TIDES_NOT_YET
      USE mod_tides
# endif
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
      logical, dimension(3) :: update =                                 &
     &         (/ .FALSE., .FALSE., .FALSE. /)

      integer :: LBi, UBi, LBj, UBj
      integer :: itrc, is

      real(r8) :: time_save = 0.0_r8
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
      SourceFile=MyFile
!
!  Lower and upper bounds for tiled arrays.
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)

# ifdef PROFILE
!
!-----------------------------------------------------------------------
!  Turn on input data time wall clock.
!-----------------------------------------------------------------------
!
      CALL wclock_on (ng, iADM, 3, __LINE__, MyFile)
# endif

# if defined SSH_TIDES_NOT_YET || defined UV_TIDES_NOT_YET
!
!-----------------------------------------------------------------------
!  Tide period, amplitude, phase, and currents.
!-----------------------------------------------------------------------
!
!  Tidal Period.
!
      IF (LprocessTides(ng)) THEN
        IF (iic(ng).eq.0) THEN
          CALL get_ngfldr (ng, iADM, idTper, ncFRCid(idTper,ng),        &
     &                     nFfiles(ng), FRC(1,ng), update(1),           &
     &                     1, MTC, 1, 1, 1, NTC(ng), 1,                 &
     &                     TIDES(ng) % Tperiod)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
      END IF
# endif

# ifdef SSH_TIDES_NOT_YET
!
!  Tidal elevation amplitude and phase. In order to read data as a
!  function of tidal period, we need to reset the model time variables
!  temporarily.
!
      IF (LprocessTides(ng)) THEN
        IF (iic(ng).eq.0) THEN
          time_save=time(ng)
          time(ng)=8640000.0_r8
          tdays(ng)=time(ng)*sec2day

          CALL get_2dfldr (ng, iADM, idTzam, ncFRCid(idTzam,ng),        &
     &                     nFfiles(ng), FRC(1,ng), update(1),           &
     &                     LBi, UBi, LBj, UBj, MTC, NTC(ng),            &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     TIDES(ng) % SSH_Tamp)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

          CALL get_2dfldr (ng, iADM, idTzph, ncFRCid(idTzph,ng),        &
     &                     nFfiles(ng), FRC(1,ng), update(1),           &
     &                     LBi, UBi, LBj, UBj, MTC, NTC(ng),            &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     TIDES(ng) % SSH_Tphase)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

          time(ng)=time_save
          tdays(ng)=time(ng)*sec2day
        END IF
      END IF
# endif

# ifdef UV_TIDES_NOT_YET
!
!  Tidal currents angle, phase, major and minor ellipse axis.
!
      IF (LprocessTides(ng)) THEN
        IF (iic(ng).eq.0) THEN
          time_save=time(ng)
          time(ng)=8640000.0_r8
          tdays(ng)=time(ng)*sec2day

          CALL get_2dfldr (ng, iADM, idTvan, ncFRCid(idTvan,ng),        &
     &                     nFfiles(ng), FRC(1,ng), update(1),           &
     &                     LBi, UBi, LBj, UBj, MTC, NTC(ng),            &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     TIDES(ng) % UV_Tangle)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

          CALL get_2dfldr (ng, iADM, idTvph, ncFRCid(idTvph,ng),        &
     &                     nFfiles(ng), FRC(1,ng), update(1),           &
     &                     LBi, UBi, LBj, UBj, MTC, NTC(ng),            &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     TIDES(ng) % UV_Tphase)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

          CALL get_2dfldr (ng, iADM, idTvma, ncFRCid(idTvma,ng),        &
     &                     nFfiles(ng), FRC(1,ng), update(1),           &
     &                     LBi, UBi, LBj, UBj, MTC, NTC(ng),            &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     TIDES(ng) % UV_Tmajor)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

          CALL get_2dfldr (ng, iADM, idTvmi, ncFRCid(idTvmi,ng),        &
     &                     nFfiles(ng), FRC(1,ng), update(1),           &
     &                     LBi, UBi, LBj, UBj, MTC, NTC(ng),            &
#  ifdef MASKING
     &                     GRID(ng) % rmask,                            &
#  endif
     &                     TIDES(ng) % UV_Tminor)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

          time(ng)=time_save
          tdays(ng)=time(ng)*sec2day
        END IF
      END IF
# endif

# ifndef ANA_PSOURCE
!
!-----------------------------------------------------------------------
!  Read in point Sources/Sinks position, direction, special flag, and
!  mass transport nondimensional shape profile.  Point sources are at
!  U- and V-points.
!-----------------------------------------------------------------------
!
      IF ((iic(ng).eq.0).and.                                           &
     &    (LuvSrc(ng).or.LwSrc(ng).or.ANY(LtracerSrc(:,ng)))) THEN
        CALL get_ngfldr (ng, iADM, idRxpo, SSF(ng)%ncid,                &
     &                   1, SSF(ng), update(1),                         &
     &                   1, Nsrc(ng), 1, 1, 1, Nsrc(ng), 1,             &
     &                   SOURCES(ng) % Xsrc)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        CALL get_ngfldr (ng, iADM, idRepo, SSF(ng)%ncid,                &
     &                   1, SSF(ng), update(1),                         &
     &                   1, Nsrc(ng), 1, 1, 1, Nsrc(ng), 1,             &
     &                   SOURCES(ng) % Ysrc)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        CALL get_ngfldr (ng, iADM, idRdir, SSF(ng)%ncid,                &
     &                   1, SSF(ng), update(1),                         &
     &                   1, Nsrc(ng), 1, 1, 1, Nsrc(ng), 1,             &
     &                   SOURCES(ng) % Dsrc)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

#  ifdef SOLVE3D
        CALL get_ngfldr (ng, iADM, idRvsh, SSF(ng)%ncid,                &
     &                   1, SSF(ng), update(1),                         &
     &                   1, Nsrc(ng), N(ng), 1, 1, Nsrc(ng), N(ng),     &
     &                   SOURCES(ng) % Qshape)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
#  endif

        DO is=1,Nsrc(ng)
          SOURCES(ng)%Isrc(is)=                                         &
     &                MAX(1,MIN(NINT(SOURCES(ng)%Xsrc(is)),Lm(ng)+1))
          SOURCES(ng)%Jsrc(is)=                                         &
     &                MAX(1,MIN(NINT(SOURCES(ng)%Ysrc(is)),Mm(ng)+1))
        END DO
      END IF
# endif

# ifdef PROFILE
!
!-----------------------------------------------------------------------
!  Turn off input data time wall clock.
!-----------------------------------------------------------------------
!
      CALL wclock_off (ng, iADM, 3, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE ad_get_idata
#else
      SUBROUTINE ad_get_idata
      RETURN
      END SUBROUTINE ad_get_idata
#endif
