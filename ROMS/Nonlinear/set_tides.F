#include "cppdefs.h"
      MODULE set_tides_mod
#if defined NONLINEAR && (defined SSH_TIDES || defined UV_TIDES)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group        Robert Hetland   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine adds tidal elevation (m) and tidal currents (m/s) to   !
!  sea surface height and 2D momentum climatologies, respectively.     !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: set_tides
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE set_tides (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_tides
# ifdef NESTING
#  if defined AVERAGES  && defined AVERAGES_DETIDE && \
     (defined SSH_TIDES || defined UV_TIDES)
      USE mod_scalars
#  endif
# endif
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# ifdef NESTING
#  if defined AVERAGES  && defined AVERAGES_DETIDE && \
     (defined SSH_TIDES || defined UV_TIDES)
       integer :: itide, mg
!
#  endif
# endif

      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 11, __LINE__, MyFile)
# endif
      CALL set_tides_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     NTC(ng),                                     &
     &                     GRID(ng) % angler,                           &
# ifdef MASKING
     &                     GRID(ng) % rmask,                            &
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % vmask,                            &
# endif
# ifdef SSH_TIDES
     &                     TIDES(ng) % SSH_Tamp,                        &
     &                     TIDES(ng) % SSH_Tphase,                      &
# endif
# ifdef UV_TIDES
     &                     TIDES(ng) % UV_Tangle,                       &
     &                     TIDES(ng) % UV_Tphase,                       &
     &                     TIDES(ng) % UV_Tmajor,                       &
     &                     TIDES(ng) % UV_Tminor,                       &
# endif
# if defined AVERAGES  && defined AVERAGES_DETIDE && \
    (defined SSH_TIDES || defined UV_TIDES)
     &                     TIDES(ng) % SinOmega,                        &
     &                     TIDES(ng) % CosOmega,                        &
# endif
     &                     TIDES(ng) % Tperiod)

# ifdef NESTING
#  if defined AVERAGES  && defined AVERAGES_DETIDE && \
     (defined SSH_TIDES || defined UV_TIDES)
!
! If nested grids and detiding, load period and harmonics to other
! grids.
!
      IF (LprocessTides(ng)) THEN
        DO mg=1,Ngrids
          IF (ng.ne.mg) THEN
            DO itide=1,NTC(ng)
              TIDES(mg)%Tperiod (1:NTC(ng))=TIDES(ng)%Tperiod (1:NTC(ng))
              TIDES(mg)%SinOmega(1:NTC(ng))=TIDES(ng)%SinOmega(1:NTC(ng))
              TIDES(mg)%CosOmega(1:NTC(ng))=TIDES(ng)%SinOmega(1:NTC(ng))
            END DO
          END IF
        END DO
      END IF
#  endif
# endif
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 11, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE set_tides
!
!***********************************************************************
      SUBROUTINE set_tides_tile (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           NTC,                                   &
     &                           angler,                                &
# ifdef MASKING
     &                           rmask, umask, vmask,                   &
# endif
# ifdef SSH_TIDES
     &                           SSH_Tamp, SSH_Tphase,                  &
# endif
# ifdef UV_TIDES
     &                           UV_Tangle, UV_Tphase,                  &
     &                           UV_Tmajor, UV_Tminor,                  &
# endif
# if defined AVERAGES  && defined AVERAGES_DETIDE && \
    (defined SSH_TIDES || defined UV_TIDES)
     &                           SinOmega, CosOmega,                    &
# endif
     &                           Tperiod)
!***********************************************************************
!
      USE mod_param
      USE mod_boundary
      USE mod_clima
      USE mod_ncparam
      USE mod_scalars
!
# ifdef DISTRIBUTE
      USE distribute_mod, ONLY : mp_boundary
# endif
      USE exchange_2d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
# endif
!
!  Imported variables declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: NTC
!
# ifdef ASSUMED_SHAPE
      real(r8), intent(in) :: angler(LBi:,LBj:)
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: Tperiod(MTC)
#  ifdef SSH_TIDES
      real(r8), intent(in) :: SSH_Tamp(LBi:,LBj:,:)
      real(r8), intent(in) :: SSH_Tphase(LBi:,LBj:,:)
#  endif
#  ifdef UV_TIDES
      real(r8), intent(in) :: UV_Tangle(LBi:,LBj:,:)
      real(r8), intent(in) :: UV_Tmajor(LBi:,LBj:,:)
      real(r8), intent(in) :: UV_Tminor(LBi:,LBj:,:)
      real(r8), intent(in) :: UV_Tphase(LBi:,LBj:,:)
#  endif
#  if defined AVERAGES  && defined AVERAGES_DETIDE && \
     (defined SSH_TIDES || defined UV_TIDES)
      real(r8), intent(inout) :: SinOmega(:)
      real(r8), intent(inout) :: CosOmega(:)
#  endif
# else
      real(r8), intent(in) :: angler(LBi:UBi,LBj:UBj)
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: Tperiod(MTC)
#  ifdef SSH_TIDES
      real(r8), intent(in) :: SSH_Tamp(LBi:UBi,LBj:UBj,MTC)
      real(r8), intent(in) :: SSH_Tphase(LBi:UBi,LBj:UBj,MTC)
#  endif
#  ifdef UV_TIDES
      real(r8), intent(in) :: UV_Tangle(LBi:UBi,LBj:UBj,MTC)
      real(r8), intent(in) :: UV_Tmajor(LBi:UBi,LBj:UBj,MTC)
      real(r8), intent(in) :: UV_Tminor(LBi:UBi,LBj:UBj,MTC)
      real(r8), intent(in) :: UV_Tphase(LBi:UBi,LBj:UBj,MTC)
#  endif
#  if defined AVERAGES  && defined AVERAGES_DETIDE && \
     (defined SSH_TIDES || defined UV_TIDES)
      real(r8), intent(inout) :: SinOmega(MTC)
      real(r8), intent(inout) :: CosOmega(MTC)
#  endif
# endif
!
!  Local variables declarations.
!
      logical :: update

# ifdef DISTRIBUTE
      integer :: ILB, IUB, JLB, JUB
# endif
      integer :: i, itide, j

      real(r8) :: Cangle, Cphase, Sangle, Sphase
      real(r8) :: angle, cff, phase, omega, ramp
      real(r8) :: bry_cor, bry_pgr, bry_str, bry_val

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Etide
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Utide
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vtide
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vwrk

# include "set_bounds.h"

# ifdef DISTRIBUTE
!
!  Lower and upper bounds for nontiled (global values) boundary arrays.
!
      ILB=BOUNDS(ng)%LBi(-1)
      IUB=BOUNDS(ng)%UBi(-1)
      JLB=BOUNDS(ng)%LBj(-1)
      JUB=BOUNDS(ng)%UBj(-1)
# endif
!
!=======================================================================
!  Process tidal forcing used at the lateral boundaries.
!=======================================================================
!
!  In refinement nesting applications, tidal forcing is only necessary
!  in the coarser large scale grid.
!
      NEEDED : IF (LprocessTides(ng)) THEN
!
!  Set time-ramping parameter.
!
# ifdef RAMP_TIDES
        ramp=TANH((tdays(ng)-dstart)/1.0_r8)
# else
        ramp=1.0_r8
# endif
# if defined AVERAGES  && defined AVERAGES_DETIDE && \
    (defined SSH_TIDES || defined UV_TIDES)
!
!-----------------------------------------------------------------------
!  Compute harmonic used to detide output fields.
!-----------------------------------------------------------------------
!
        cff=2.0_r8*pi*(time(ng)-tide_start*day2sec)
        DO itide=1,NTC
          IF (Tperiod(itide).gt.0.0_r8) THEN
            omega=cff/Tperiod(itide)
            SinOmega(itide)=SIN(omega)
            CosOmega(itide)=COS(omega)
          ELSE
            SinOmega(itide)=0.0_r8
            CosOmega(itide)=0.0_r8
          END IF
        END DO
# endif
# ifdef SSH_TIDES
!
!-----------------------------------------------------------------------
!  Add tidal elevation (m) to sea surface height climatology.
!-----------------------------------------------------------------------
!
        Etide(:,:)=0.0_r8
        cff=2.0_r8*pi*(time(ng)-tide_start*day2sec)
        DO itide=1,NTC
          IF (Tperiod(itide).gt.0.0_r8) THEN
            omega=cff/Tperiod(itide)
            DO j=JstrR,JendR
              DO i=IstrR,IendR
                Etide(i,j)=Etide(i,j)+                                  &
     &                     ramp*SSH_Tamp(i,j,itide)*                    &
     &                     COS(omega-SSH_Tphase(i,j,itide))
#  ifdef MASKING
                Etide(i,j)=Etide(i,j)*rmask(i,j)
#  endif
              END DO
            END DO
          END IF
        END DO

#  ifdef ADD_FSOBC
!
!  Add sub-tidal forcing and adjust climatology to include tides.
!
        IF (LsshCLM(ng)) THEN
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              CLIMA(ng)%ssh(i,j)=CLIMA(ng)%ssh(i,j)+Etide(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              CLIMA(ng)%ssh)
          END IF
#   ifdef DISTRIBUTE
          CALL mp_exchange2d (ng, tile, iNLM, 1,                        &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        NghostPoints,                             &
     &                        EWperiodic(ng), NSperiodic(ng),           &
     &                        CLIMA(ng)%ssh)
#   endif
        END IF
#  endif
!
!  If appropriate, load tidal forcing into boundary arrays.  The "zeta"
!  boundary arrays are important for the Flather or reduced physics
!  boundary conditions for 2D momentum. To avoid having two boundary
!  points for these arrays, the values of "zeta_west" and "zeta_east"
!  are averaged at u-points.  Similarly, the values of "zeta_south"
!  and "zeta_north" is averaged at v-points. Noticed that these
!  arrays are also used for the clamped conditions for free-surface.
!  This averaging is less important for that type ob boundary
!  conditions.
!
        IF (LBC(iwest,isFsur,ng)%acquire.or.                            &
     &      LBC(iwest,isUbar,ng)%acquire.or.                            &
     &      LBC(iwest,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO j=JstrR,JendR
#  ifdef ADD_FSOBC
              BOUNDARY(ng)%zeta_west(j)=BOUNDARY(ng)%zeta_west(j)+      &
     &                                  0.5_r8*(Etide(Istr-1,j)+        &
     &                                          Etide(Istr  ,j))
#  else
              BOUNDARY(ng)%zeta_west(j)=0.5_r8*(Etide(Istr-1,j)+        &
     &                                          Etide(Istr  ,j))
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, JstrR, JendR,                     &
     &                      JLB, JUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%zeta_west)
#  endif
        END IF
!
        IF (LBC(ieast,isFsur,ng)%acquire.or.                            &
     &      LBC(ieast,isUbar,ng)%acquire.or.                            &
     &      LBC(ieast,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO j=JstrR,JendR
#  ifdef ADD_FSOBC
              BOUNDARY(ng)%zeta_east(j)=BOUNDARY(ng)%zeta_east(j)+      &
     &                                  0.5_r8*(Etide(Iend  ,j)+        &
     &                                          Etide(Iend+1,j))
#  else
              BOUNDARY(ng)%zeta_east(j)=0.5_r8*(Etide(Iend  ,j)+        &
     &                                          Etide(Iend+1,j))
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, JstrR, JendR,                     &
     &                      JLB, JUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%zeta_east)
#  endif
        END IF
!
        IF (LBC(isouth,isFsur,ng)%acquire.or.                           &
     &      LBC(isouth,isUbar,ng)%acquire.or.                           &
     &      LBC(isouth,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO i=IstrR,IendR
#  ifdef ADD_FSOBC
              BOUNDARY(ng)%zeta_south(i)=BOUNDARY(ng)%zeta_south(i)+    &
     &                                   0.5_r8*(Etide(i,Jstr-1)+       &
     &                                           Etide(i,Jstr  ))
#  else
              BOUNDARY(ng)%zeta_south(i)=0.5_r8*(Etide(i,Jstr-1)+       &
     &                                           Etide(i,Jstr  ))
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, IstrR, IendR,                     &
     &                      ILB, IUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%zeta_south)
#  endif
        END IF
!
        IF (LBC(inorth,isFsur,ng)%acquire.or.                           &
     &      LBC(inorth,isUbar,ng)%acquire.or.                           &
     &      LBC(inorth,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO i=IstrR,IendR
#  ifdef ADD_FSOBC
              BOUNDARY(ng)%zeta_north(i)=BOUNDARY(ng)%zeta_north(i)+    &
     &                                   0.5_r8*(Etide(i,Jend  )+       &
     &                                           Etide(i,Jend+1))
#  else
              BOUNDARY(ng)%zeta_north(i)=0.5_r8*(Etide(i,Jend  )+       &
     &                                           Etide(i,Jend+1))
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, IstrR, IendR,                     &
     &                      ILB, IUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%zeta_north)
#  endif
        END IF
# endif

# if defined UV_TIDES
!
!-----------------------------------------------------------------------
!  Add tidal currents (m/s) to 2D momentum climatologies.
!-----------------------------------------------------------------------
!
        Utide(:,:)=0.0_r8
        Vtide(:,:)=0.0_r8
        cff=2.0_r8*pi*(time(ng)-tide_start*day2sec)
        DO itide=1,NTC
          IF (Tperiod(itide).gt.0.0_r8) THEN
            omega=cff/Tperiod(itide)
            DO j=MIN(JstrR,Jstr-1),JendR
              DO i=MIN(IstrR,Istr-1),IendR
                angle=UV_Tangle(i,j,itide)-angler(i,j)
                Cangle=COS(angle)
                Sangle=SIN(angle)
                phase=omega-UV_Tphase(i,j,itide)
                Cphase=COS(phase)
                Sphase=SIN(phase)
                Uwrk(i,j)=UV_Tmajor(i,j,itide)*Cangle*Cphase-           &
     &                    UV_Tminor(i,j,itide)*Sangle*Sphase
                Vwrk(i,j)=UV_Tmajor(i,j,itide)*Sangle*Cphase+           &
     &                    UV_Tminor(i,j,itide)*Cangle*Sphase
              END DO
            END DO
            DO j=JstrR,JendR
              DO i=Istr,IendR
                Utide(i,j)=Utide(i,j)+                                  &
     &                     ramp*0.5_r8*(Uwrk(i-1,j)+Uwrk(i,j))
#  ifdef MASKING
                Utide(i,j)=Utide(i,j)*umask(i,j)
#  endif
              END DO
            END DO
            DO j=Jstr,JendR
              DO i=IstrR,IendR
                Vtide(i,j)=(Vtide(i,j)+                                 &
     &                      ramp*0.5_r8*(Vwrk(i,j-1)+Vwrk(i,j)))
#  ifdef MASKING
                Vtide(i,j)=Vtide(i,j)*vmask(i,j)
#  endif
              END DO
            END DO
          END IF
        END DO

#  ifdef ADD_M2OBC
!
!  Add sub-tidal forcing and adjust climatology to include tides.
!
        IF (Lm2CLM(ng)) THEN
          DO j=JstrR,JendR
            DO i=Istr,IendR
              CLIMA(ng)%ubarclm(i,j)=CLIMA(ng)%ubarclm(i,j)+Utide(i,j)
            END DO
          END DO
          DO j=Jstr,JendR
            DO i=IstrR,IendR
              CLIMA(ng)%vbarclm(i,j)=CLIMA(ng)%vbarclm(i,j)+Vtide(i,j)
            END DO
          END DO
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_u2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              CLIMA(ng)%ubarclm)
            CALL exchange_v2d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj,                 &
     &                              CLIMA(ng)%vbarclm)
          END IF
#   ifdef DISTRIBUTE
          CALL mp_exchange2d (ng, tile, iNLM, 2,                        &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        NghostPoints,                             &
     &                        EWperiodic(ng), NSperiodic(ng),           &
     &                        CLIMA(ng)%ubarclm,                        &
     &                        CLIMA(ng)%vbarclm)
#   endif
        END IF
#  endif
!
!  If appropriate, load tidal forcing into boundary arrays.
!
        IF (LBC(iwest,isUbar,ng)%acquire.and.                           &
     &      LBC(iwest,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO j=JstrR,JendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%ubar_west(j)=BOUNDARY(ng)%ubar_west(j)+      &
     &                                  Utide(Istr,j)
#  else
              BOUNDARY(ng)%ubar_west(j)=Utide(Istr,j)
#  endif
            END DO
            DO j=Jstr,JendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%vbar_west(j)=BOUNDARY(ng)%vbar_west(j)+      &
     &                                  Vtide(Istr-1,j)
#  else
              BOUNDARY(ng)%vbar_west(j)=Vtide(Istr-1,j)
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, JstrR, JendR,                     &
     &                      JLB, JUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%ubar_west)
          CALL mp_boundary (ng, iNLM, Jstr,  JendR,                     &
     &                      JLB, JUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%vbar_west)
#  endif
        END IF
!
        IF (LBC(ieast,isUbar,ng)%acquire.and.                           &
     &      LBC(ieast,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO j=JstrR,JendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%ubar_east(j)=BOUNDARY(ng)%ubar_east(j)+      &
     &                                  Utide(Iend+1,j)
#  else
              BOUNDARY(ng)%ubar_east(j)=Utide(Iend+1,j)
#  endif
            END DO
            DO j=Jstr,JendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%vbar_east(j)=BOUNDARY(ng)%vbar_east(j)+      &
     &                                  Vtide(Iend+1,j)
#  else
              BOUNDARY(ng)%vbar_east(j)=Vtide(Iend+1,j)
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, JstrR, JendR,                     &
     &                      JLB, JUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%ubar_east)
          CALL mp_boundary (ng, iNLM, Jstr,  JendR,                     &
     &                      JLB, JUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%vbar_east)
#  endif
        END IF
!
        IF (LBC(isouth,isUbar,ng)%acquire.and.                          &
     &      LBC(isouth,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
            DO i=Istr,IendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%ubar_south(i)=BOUNDARY(ng)%ubar_south(i)+    &
     &                                   Utide(i,Jstr-1)
#  else
              BOUNDARY(ng)%ubar_south(i)=Utide(i,Jstr-1)
#  endif
            END DO
            DO i=IstrR,IendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%vbar_south(i)=BOUNDARY(ng)%vbar_south(i)+    &
     &                                   Vtide(i,Jstr)
#  else
              BOUNDARY(ng)%vbar_south(i)=Vtide(i,Jstr)
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, Istr,  IendR,                     &
     &                      ILB, IUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%ubar_south)
          CALL mp_boundary (ng, iNLM, IstrR, IendR,                     &
     &                      ILB, IUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%vbar_south)
#  endif
        END IF
!
        IF (LBC(inorth,isUbar,ng)%acquire.and.                          &
     &      LBC(inorth,isVbar,ng)%acquire) THEN
          update=.FALSE.
          IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
            DO i=Istr,IendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%ubar_north(i)=BOUNDARY(ng)%ubar_north(i)+    &
     &                                   Utide(i,Jend+1)
#  else
              BOUNDARY(ng)%ubar_north(i)=Utide(i,Jend+1)
#  endif
            END DO
            DO i=IstrR,IendR
#  ifdef ADD_M2OBC
              BOUNDARY(ng)%vbar_north(i)=BOUNDARY(ng)%vbar_north(i)+    &
     &                                   Vtide(i,Jend+1)
#  else
              BOUNDARY(ng)%vbar_north(i)=Vtide(i,Jend+1)
#  endif
            END DO
            update=.TRUE.
          END IF
#  ifdef DISTRIBUTE
          CALL mp_boundary (ng, iNLM, Istr,  IendR,                     &
     &                      ILB, IUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%ubar_north)
          CALL mp_boundary (ng, iNLM, IstrR, IendR,                     &
     &                      ILB, IUB, 1, 1, update,                     &
     &                      BOUNDARY(ng)%vbar_north)
#  endif
        END IF
# endif
      END IF NEEDED
!
      RETURN
      END SUBROUTINE set_tides_tile
#endif
      END MODULE set_tides_mod
