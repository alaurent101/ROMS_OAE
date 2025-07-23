#include "cppdefs.h"
      MODULE rho_mean_mod

! Adapted from rho_eos_sprog.F 2017/6/20
! Modified to match format of rho_eos.F in 2020/11 ROMS 2021/3/1
! This version calculates the weighted, smoothed difference between rho_clim and rho
! to be used in the semiprognostic method.

#if defined SOLVE3D && defined SEMIPROG
!
      implicit none
!
      PRIVATE
      PUBLIC  :: rho_mean
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE rho_mean (ng, tile, model)
!***********************************************************************
!
      USE mod_param
      USE mod_coupling
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
!
!  Local variable declarations.
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, model, 14, __LINE__, __FILE__)
# endif
      CALL rho_mean_tile (ng, tile, model,                              &
     &                   LBi, UBi, LBj, UBj,                            &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
     &                   OCEAN(ng) % rho,                               &
     &                   OCEAN(ng) % rho_clim,                          &
     &                   OCEAN(ng) % rho_mean)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 14, __LINE__, __FILE__)
# endif

      RETURN
      END SUBROUTINE rho_mean
!
!***********************************************************************
      SUBROUTINE rho_mean_tile (ng, tile, model,                        &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         IminS, ImaxS, JminS, JmaxS,              &
#  ifdef MASKING
     &                         rmask,                                   &
#  endif
     &                         z_r, z_w, rho,                           &
     &                         rho_clim, rho_mean)
!***********************************************************************
!
      USE mod_param
      USE mod_eoscoef
      USE mod_scalars
!
      USE exchange_2d_mod
      USE exchange_3d_mod
#  ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d, mp_exchange3d
#  endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
!
#  ifdef ASSUMED_SHAPE
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: rho(LBi:,LBj:,:)
      real(r8), intent(in) :: rho_clim(LBi:,LBj:,:)
      real(r8), intent(out) :: rho_mean(LBi:,LBj:,:)
#  else
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in) :: rho(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: rho_clim(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(out) :: rho_mean(LBi:UBi,LBj:UBj,N(ng))
#  endif
!
!  Local variable declarations.
!
      integer :: i, j, k

! Added 2017/6/19
! Changed beta_sprog and int_sprog to run-time parameters 2021/3/1
      integer :: ii, jj , iimin, iimax, jjmin, jjmax
      real(r8) :: rsum, msum
#  include "set_bounds.h"
!
!=======================================================================
      DO j=JstrT,JendT
       DO k=1,N(ng)
        DO i=IstrT,IendT
         rsum=0.0_r8
         msum=0.0_r8
         rho_mean(i,j,k)=rho(i,j,k)
         iimin=MAX(IstrT,i-int_sprog(ng))
         iimax=MIN(i+int_sprog(ng),IendT)
         jjmin=MAX(JstrT,j-int_sprog(ng))
         jjmax=MIN(j+int_sprog(ng),JendT)
         DO jj=jjmin,jjmax
           DO ii=iimin,iimax
           rsum=rsum+((rho_clim(ii,jj,k)-rho(ii,jj,k))*rmask(ii,jj))
           msum=msum+rmask(ii,jj)
          ENDDO
         ENDDO
         IF (msum.GE.1.0_r8) THEN
          rho_mean(i,j,k)=rho(i,j,k)+(beta_sprog(ng)*rsum/msum)
         END IF
        END DO
       END DO
      END DO
!
!-----------------------------------------------------------------------
!  Exchange boundary data.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          rho_mean)
      END IF

#  ifdef DISTRIBUTE
      CALL mp_exchange3d (ng, tile, model, 2,                           &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    rho_mean)
#  endif

      RETURN
      END SUBROUTINE rho_mean_tile

#endif
      END MODULE rho_mean_mod