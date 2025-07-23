#include "cppdefs.h"
      MODULE bulk_flux_wind_mod
#ifdef BULK_FLUX_WIND

! Modified from bulk_flux.F to calculate wind stress using Large & Pond
! (1981) scheme 2021/1/24

!
!svn $Id: bulk_flux.F 1043 2020-11-12 04:56:14Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2020 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: bulk_flux_wind
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE bulk_flux_wind (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
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
      CALL bulk_flux_wind_tile (ng, tile,                               &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     nrhs(ng),                                    &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % vmask,                            &
# endif
# ifdef WET_DRY
     &                     GRID(ng) % rmask_wet,                        &
     &                     GRID(ng) % umask_wet,                        &
     &                     GRID(ng) % vmask_wet,                        &
# endif                                                                 &
# ifdef WIND_MINUS_CURRENT
     &                     OCEAN(ng) % u,                               &
     &                     OCEAN(ng) % v,                               &
# endif
     &                     FORCES(ng) % Uwind,                          &
     &                     FORCES(ng) % Vwind,                          &
     &                     FORCES(ng) % sustr,                          &
     &                     FORCES(ng) % svstr)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 17, __LINE__, __FILE__)
# endif

      RETURN
      END SUBROUTINE bulk_flux_wind
!
!***********************************************************************
      SUBROUTINE bulk_flux_wind_tile (ng, tile,                         &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           nrhs,                                  &
# ifdef MASKING
     &                           rmask, umask, vmask,                   &
# endif
# ifdef WET_DRY
     &                           rmask_wet, umask_wet, vmask_wet,       &
# endif
# ifdef WIND_MINUS_CURRENT
     &                           u, v,                                  &
# endif
     &                           Uwind, Vwind,                          &
     &                           sustr, svstr)
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
      integer, intent(in) :: nrhs
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
#  ifdef WIND_MINUS_CURRENT
      real(r8), intent(in) :: u(LBi:,LBj:,:,:)
      real(r8), intent(in) :: v(LBi:,LBj:,:,:)
#  endif
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
      real(r8), intent(out) :: sustr(LBi:,LBj:)
      real(r8), intent(out) :: svstr(LBi:,LBj:)
# else
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WIND_MINUS_CURRENT
      real(r8), intent(in) :: u(LBi:UBi,LBj:UBj,N(ng),3)
      real(r8), intent(in) :: v(LBi:UBi,LBj:UBj,N(ng),3)
#  endif
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(out) :: svstr(LBi:UBi,LBj:UBj)
# endif
!
!  Local variable declarations.
!
      integer :: i, j
      real(r8) :: Wspeed, rhoAir
      real(r8) :: cff, cff1, cff2
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Taux
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Tauy
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uair
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vair

# include "set_bounds.h"

! Use dry air density
      rhoAir = 1.2_r8

!
!=======================================================================
!  Atmosphere-Ocean bulk fluxes parameterization.
!=======================================================================
#ifdef WIND_MINUS_CURRENT
!
!  Modify near-surface (2m or 10m) effective winds by subtracting the
!  ocean surface current (J Wilkin). See:
!
!  Bye, J.A.T. and J.-O. Wolff, 1999: Atmosphere-ocean momentum exchange
!     in general circulation models. J. Phys. Oceanogr. 29, 671-691.
!
      DO j=Jstr-1,Jend+1
        DO i=Istr-1,MIN(Iend+1,Lm(ng))
          Uair(i,j)=Uwind(i,j)-                                         &
     &              0.5_r8*(u(i,j,N(ng),nrhs)+u(i+1,j,N(ng),nrhs))
        END DO
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          Uair(Iend+1,j)=Uwind(Iend+1,j)-u(Iend,j,N(ng),nrhs)
        END IF
      END DO
      DO i=Istr-1,Iend+1
        DO j=Jstr-1,MIN(Jend+1,Mm(ng))
          Vair(i,j)=Vwind(i,j)-                                         &
     &              0.5_r8*(v(i,j,N(ng),nrhs)+v(i,j+1,N(ng),nrhs))
        END DO
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          Vair(i,Jend+1)=Vwind(i,Jend+1)-v(i,Jend,N(ng),nrhs)
        END IF
      END DO
#else
!
!  Load wind components to local arrays.
!
      DO j=Jstr-1,Jend+1
        DO i=Istr-1,Iend+1
          Uair(i,j)=Uwind(i,j)
          Vair(i,j)=Vwind(i,j)
        END DO
      END DO
#endif
!
!  Compute Atmosphere-ocean fluxes using a bulk flux parameterization.
!
      DO j=Jstr-1,JendR
        DO i=Istr-1,IendR
!
!  Initialize.
!
          Taux(i,j)=0.0_r8
          Tauy(i,j)=0.0_r8
!
!  Assume zero gusts
          Wspeed=SQRT(Uair(i,j)*Uair(i,j)+Vair(i,j)*Vair(i,j))

!  Large and Pond (1981) scheme
	  if (Wspeed.lt.11.0_r8) then
            cff1=1.2e-3_r8
            cff2=0.0_r8
          else
            cff1=0.49e-3_r8
            cff2=0.065e-3_r8
          end if

          Taux(i,j)=rhoAir*Wspeed*(cff1+(cff2*Wspeed))*Uwind(i,j)
# ifdef MASKING
          Taux(i,j)=Taux(i,j)*rmask(i,j)
# endif
# ifdef WET_DRY
          Taux(i,j)=Taux(i,j)*rmask_wet(i,j)
# endif

          Tauy(i,j)=rhoAir*Wspeed*(cff1+(cff2*Wspeed))*Vwind(i,j)
# ifdef MASKING
          Tauy(i,j)=Tauy(i,j)*rmask(i,j)
# endif
# ifdef WET_DRY
          Tauy(i,j)=Tauy(i,j)*rmask_wet(i,j)
# endif
        END DO
      END DO
!
!=======================================================================
!  Compute surface wind stress.
!=======================================================================
!
!  Compute kinematic, surface wind stress (m2/s2).
!
      cff=0.5_r8/rho0
      DO j=JstrR,JendR
        DO i=Istr,IendR
          sustr(i,j)=cff*(Taux(i-1,j)+Taux(i,j))
# ifdef MASKING
          sustr(i,j)=sustr(i,j)*umask(i,j)
# endif
# ifdef WET_DRY
          sustr(i,j)=sustr(i,j)*umask_wet(i,j)
# endif
        END DO
      END DO
      DO j=Jstr,JendR
        DO i=IstrR,IendR
          svstr(i,j)=cff*(Tauy(i,j-1)+Tauy(i,j))
# ifdef MASKING
          svstr(i,j)=svstr(i,j)*vmask(i,j)
# endif
# ifdef WET_DRY
          svstr(i,j)=svstr(i,j)*vmask_wet(i,j)
# endif
        END DO
      END DO
!
!-----------------------------------------------------------------------
!  Exchange boundary data.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          sustr)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          svstr)
      END IF
# ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    sustr, svstr)
# endif

      RETURN
      END SUBROUTINE bulk_flux_wind_tile
#endif
      END MODULE bulk_flux_wind_mod
