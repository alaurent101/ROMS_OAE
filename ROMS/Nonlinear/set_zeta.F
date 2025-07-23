#include "cppdefs.h"
      MODULE set_zeta_mod

#ifdef SOLVE3D
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine sets free-surface to its fast-time averaged value.     !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: set_zeta
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE set_zeta (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_coupling
      USE mod_ocean
! Copied RUNOFF & RUNOFF_SSH from roms-kate_svn version 2021/3/22
# if defined RUNOFF && defined RUNOFF_SSH
      USE mod_forces
      USE mod_grid
# endif

!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 12, __LINE__, MyFile)
# endif
      CALL set_zeta_tile (ng, tile,                                     &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    COUPLING(ng) % Zt_avg1,                       &
! Copied RUNOFF & RUNOFF_SSH from roms-kate_svn version 2021/3/22
# if defined RUNOFF && defined RUNOFF_SSH
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    FORCES(ng) % runoff,                          &
# endif
     &                    OCEAN(ng) % zeta)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 12, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE set_zeta
!
!***********************************************************************
      SUBROUTINE set_zeta_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
! Commented out next line to add RUNOFF & RUNOFF_SSH 2021/3/22
!     &                          Zt_avg1, zeta)
     &                          Zt_avg1,                                &
! Copied RUNOFF & RUNOFF_SSH from roms-kate_svn version 2021/3/22
# if defined RUNOFF && defined RUNOFF_SSH
     &                          pm, pn, runoff,                         & 
# endif
     &                          zeta)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE exchange_2d_mod, ONLY : exchange_r2d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
# ifdef ASSUMED_SHAPE
! Changed Zt_avg1 from intent(in) to intent(inout) 2021/3/22
      real(r8), intent(inout) :: Zt_avg1(LBi:,LBj:)

      real(r8), intent(out) :: zeta(LBi:,LBj:,:)
! Copied RUNOFF & RUONFF_SSH from roms-kate_svn version 2021/3/22
#   if defined RUNOFF && defined RUNOFF_SSH
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: runoff(LBi:,LBj:)
#   endif
# else
! Changed Zt_avg1 from intent(in) to intent(inout) 2021/3/22
      real(r8), intent(inout) :: Zt_avg1(LBi:UBi,LBj:UBj)

      real(r8), intent(out) :: zeta(LBi:UBi,LBj:UBj,3)
! Copied RUNOFF & RUONFF_SSH from roms-kate_svn version 2021/3/22
#   if defined RUNOFF && defined RUNOFF_SSH
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: runoff(LBi:UBi,LBj:UBj)
#   endif
# endif
!
!  Local variable declarations.
!
      integer :: i, j
! Copied cff1 from roms-kate_svn version 2021/3/22
      real(r8) :: cff1

# include "set_bounds.h"

! Copied RUNOFF & RUNOFF_SSH from roms-kate_svn version 2021/3/22
# if defined RUNOFF && defined RUNOFF_SSH
!
! Correct sea level for surface runoff
! Apply to fast-time averaged free-surface height
!
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          cff1 = dt(ng)/rhow
          Zt_avg1(i,j) = Zt_avg1(i,j) + cff1*runoff(i,j)
        END DO
      END DO
# endif

!
!-----------------------------------------------------------------------
!  Prepare to time-step 2D equations:  set initial free-surface
!  to its fast-time averaged values (which corresponds to the time
!  step "n").
!-----------------------------------------------------------------------
!
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          zeta(i,j,1)=Zt_avg1(i,j)
          zeta(i,j,2)=Zt_avg1(i,j)
        END DO
      END DO

      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          zeta(:,:,1))
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          zeta(:,:,2))
      END IF

# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    zeta(:,:,1),                                  &
     &                    zeta(:,:,2))
# endif
!
      RETURN
      END SUBROUTINE set_zeta_tile
#endif
      END MODULE set_zeta_mod
