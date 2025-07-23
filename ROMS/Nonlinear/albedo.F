#include "cppdefs.h"
      MODULE albedo_mod
#ifdef SHORTWAVE_IN
!
!svn $Id$
!=======================================================================
! Adapted from https://github.com/kshedstrom/roms                      !
!                                   !
!=======================================================================
!                                                                      !
! This routine computes the albedo.                                    !
!                                                                      !
! Default: albedo = constant value                                     !
!                                                                      !
! ALBEDO_LY09: albedo is a function of latitude following Large and    !
!              Yaeger (2009).                                          !
!                                                                      !
! Reference:                                                           !
! Large, W. G., & Yeager, S. G. (2009). The global climatology of an   !
! interannually varying air–sea flux data set. Climate Dynamics, 33(2),!
! 341–364. https://doi.org/10.1007/s00382-008-0441-3                   !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: albedo_eval

contains
!
!***********************************************************************
      SUBROUTINE albedo_eval (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid

      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 17, __LINE__, __FILE__)
# endif
      CALL albedo_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
# if defined SHORTWAVE && defined ALBEDO_LY09
     &                  GRID(ng) % latr,                                &
# endif
     &                  FORCES(ng) % albedo                             &
     &                  )
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 17, __LINE__, __FILE__)
# endif
      RETURN
      END SUBROUTINE albedo_eval
!
!***********************************************************************
      SUBROUTINE albedo_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
# if defined SHORTWAVE && defined ALBEDO_LY09
     &                        latr,                                     &
# endif
     &                        albedo                                    &
     &                        )
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
      USE exchange_2d_mod
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
#  if defined SHORTWAVE && defined ALBEDO_LY09
      real(r8), intent(in) :: latr(LBi:,LBj:)
#  endif
      real(r8), intent(out) :: albedo(LBi:,LBj:)
# else
#  if defined SHORTWAVE && defined ALBEDO_LY09
      real(r8), intent(in) :: latr(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(out) :: albedo(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, j, listp
!
      real(r8) :: cff1, cff2
      real(r8) :: cff
!
      real(r8), parameter :: alb_w=0.06_r8

# include "set_bounds.h"
!-------------------------------------------------------------------------------
! PURPOSE:
!   computes albedo over water
!-------------------------------------------------------------------------------

! Note that this loop needs to be cleaned of all global arrays for
! OpenMP.
      DO j=Jstr-1,JendR
        DO i=Istr-1,IendR
!
! Compute ocean albedo
# ifdef ALBEDO_LY09
      albedo(i,j) = (0.069_r8 - 0.011_r8*cos(2*deg2rad*latr(i,j)))
# else
      albedo(i,j)=alb_w
# endif
!
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Exchange boundary data.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          albedo)
      END IF
!
# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    albedo)
# endif
!
      RETURN
      END SUBROUTINE albedo_tile
!
# endif
!
      END module albedo_mod