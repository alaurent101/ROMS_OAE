      SUBROUTINE biology (ng,tile)
!
!svn $Id$
!***********************************************************************
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!                                                       Katja Fennel   !
!****************************************** Alexander F. Shchepetkin ***
!                                                                      !
!  This routine computes the dissolution of particles for the          !
!  dissolution model. Then, it  adds  those  terms  to  the  global    !
!  biological fields.                                                  !
!                                                                      !
!***********************************************************************
!
      USE mod_param
#ifdef DIAGNOSTICS_BIO
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
      USE mod_ncparam
      USE mod_ocean
      USE mod_stepping
!
      implicit none
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
#include "tile.h"
!
!  Set header file name.
!
#ifdef DISTRIBUTE
      IF (Lbiofile(iNLM)) THEN
#else
      IF (Lbiofile(iNLM).and.(tile.eq.0)) THEN
#endif
        Lbiofile(iNLM)=.FALSE.
        BIONAME(iNLM)=MyFile
      END IF
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 15, __LINE__, MyFile)
#endif
      CALL biology_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp(ng), nnew(ng),                            &
#ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# ifdef WET_DRY
     &                   GRID(ng) % rmask_wet,                          &
#  ifdef DIAGNOSTICS_BIO
     &                   GRID(ng) % rmask_full,                         &
#  endif
# endif
#endif
     &                   GRID(ng) % Hz,                                 &
     &                   GRID(ng) % z_r,                                &
     &                   GRID(ng) % z_w,                                &
     &                   FORCES(ng) % srflx,                            &
#ifdef DIAGNOSTICS_BIO
     &                   DIAGS(ng) % DiaBio2d,                          &
#endif
     &                   OCEAN(ng) % t)

#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 15, __LINE__, MyFile)
#endif
!
      RETURN
      END SUBROUTINE biology
!
!-----------------------------------------------------------------------
      SUBROUTINE biology_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj, UBk, UBt,            &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nstp, nnew,                              &
#ifdef MASKING
     &                         rmask,                                   &
# if defined WET_DRY
     &                         rmask_wet,                               &
#  ifdef DIAGNOSTICS_BIO
     &                         rmask_full,                              &
#  endif
# endif
#endif
     &                         Hz, z_r, z_w, srflx,                     &
#ifdef DIAGNOSTICS_BIO
     &                         DiaBio2d,                                &
#endif
     &                         t)
!-----------------------------------------------------------------------
!
      USE mod_param
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
!
      USE dateclock_mod, ONLY : caldate
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj, UBk, UBt
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nstp, nnew

#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
#   ifdef DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_full(LBi:,LBj:)
#   endif
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: srflx(LBi:,LBj:)
      
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
# endif
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#else
# ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
#   ifdef DIAGNOSTICS_BIO
      real(r8), intent(in) :: rmask_full(LBi:UBi,LBj:UBj)
#   endif
#  endif
# endif
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,UBk)
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:UBk)
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)

# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d)
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif
!
!  Local variable declarations.
!

#ifdef MULTI_PARTICLES
      integer, parameter :: Nsink = 9
# else
      integer, parameter :: Nsink = 1
#endif

      integer :: Iter, i, ibio, isink, itrc, ivar, j, k, ks
      
      integer, dimension(Nsink) :: idsink

      real(r8), parameter :: eps = 1.0e-20_r8

      real(r8) :: dtdays

      real(r8) :: cff, cff1, cff2, cff3, cff4, cff5
      real(r8) :: fac1, fac2, fac3, fac4
      real(r8) :: cffL, cffR, cu, dltL, dltR


#ifdef DIAGNOSTICS_BIO
      real(r8) :: fiter
#endif

      real(r8), dimension(Nsink) :: Wbio
      
      integer, dimension(IminS:ImaxS,N(ng)) :: ksource

      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio
      real(r8), dimension(IminS:ImaxS,N(ng),NT(ng)) :: Bio_old
      
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC

      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv2
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hz_inv3
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: WR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bL
      real(r8), dimension(IminS:ImaxS,N(ng)) :: bR
      real(r8), dimension(IminS:ImaxS,N(ng)) :: qc

#include "set_bounds.h"
#ifdef DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
! If appropriate, initialize time-averaged diagnostic arrays.
!-----------------------------------------------------------------------
!
      IF (((iic(ng).gt.ntsDIA(ng)).and.                                 &
     &     (MOD(iic(ng),nDIA(ng)).eq.1)).or.                            &
     &    ((iic(ng).ge.ntsDIA(ng)).and.(nDIA(ng).eq.1)).or.             &
     &    ((nrrec(ng).gt.0).and.(iic(ng).eq.ntstart(ng)))) THEN
        DO ivar=1,NDbio2d
          DO j=Jstr,Jend
            DO i=Istr,Iend
              DiaBio2d(i,j,ivar)=0.0_r8
            END DO
          END DO
        END DO
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Add biological Source/Sink terms.
!-----------------------------------------------------------------------
!
!  Avoid computing source/sink terms if no biological iterations.
!
      IF (BioIter(ng).le.0) RETURN
!
!  Set time-stepping according to the number of iterations.
!
      dtdays=dt(ng)*sec2day/REAL(BioIter(ng),r8)
#ifdef DIAGNOSTICS_BIO
!
!  A factor to account for the number of iterations in accumulating
!  diagnostic rate variables.
!
      fiter=1.0_r8/REAL(BioIter(ng),r8)
#endif

!
!  Set vertical sinking indentification vector.
!
      idsink(1)=iParticle1
#ifdef MULTI_PARTICLES
      idsink(2)=iParticle2
      idsink(3)=iParticle3
      idsink(4)=iParticle4
      idsink(5)=iParticle5
      idsink(6)=iParticle6
      idsink(7)=iParticle7
      idsink(8)=iParticle8
      idsink(9)=iParticle9
#endif
!
!  Set vertical sinking velocity vector in the same order as the
!  identification vector, IDSINK.
!
      Wbio(1)=wPar1(ng)                ! sinking velocity of particle1
#ifdef MULTI_PARTICLES
      Wbio(2)=wPar2(ng)                ! sinking velocity of particle2
      Wbio(3)=wPar3(ng)                ! sinking velocity of particle3
      Wbio(4)=wPar4(ng)                ! sinking velocity of particle4
      Wbio(5)=wPar5(ng)                ! sinking velocity of particle5
      Wbio(6)=wPar6(ng)                ! sinking velocity of particle6
      Wbio(7)=wPar7(ng)                ! sinking velocity of particle7
      Wbio(8)=wPar8(ng)                ! sinking velocity of particle8
      Wbio(9)=wPar9(ng)                ! sinking velocity of particle9
#endif
!
!  Compute inverse thickness to avoid repeated divisions.
!
      J_LOOP : DO j=Jstr,Jend
        DO k=1,N(ng)
          DO i=Istr,Iend
            Hz_inv(i,k)=1.0_r8/Hz(i,j,k)
          END DO
        END DO
        DO k=1,N(ng)-1
          DO i=Istr,Iend
            Hz_inv2(i,k)=1.0_r8/(Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
        DO k=2,N(ng)-1
          DO i=Istr,Iend
            Hz_inv3(i,k)=1.0_r8/(Hz(i,j,k-1)+Hz(i,j,k)+Hz(i,j,k+1))
          END DO
        END DO
!
!  Extract biological variables from tracer arrays, place them into
!  scratch arrays, and restrict their values to be positive definite.
!  At input, all tracers (index nnew) from predictor step have
!  transport units (m Tunits) since we do not have yet the new
!  values for zeta and Hz. These are known after the 2D barotropic
!  time-stepping.
!
        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio_old(i,k,ibio)=MAX(0.0_r8,t(i,j,k,nstp,ibio))
              Bio(i,k,ibio)=Bio_old(i,k,ibio)
            END DO
          END DO
        END DO

!
!=======================================================================
!  Start internal iterations to achieve convergence of the nonlinear
!  backward-implicit solution.
!=======================================================================
!
!  During the iterative procedure a series of fractional time steps are
!  performed in a chained mode (splitting by different biological
!  conversion processes) in sequence of the main food chain.  In all
!  stages the concentration of the component being consumed is treated
!  in fully implicit manner, so the algorithm guarantees non-negative
!  values, no matter how strong s the concentration of active consuming
!  component (Phytoplankton or Zooplankton).  The overall algorithm,
!  as well as any stage of it, is formulated in conservative form
!  (except explicit sinking) in sense that the sum of concentration of
!  all components is conserved.
!
!
!  In the implicit algorithm, we have for example (N: nitrate,
!                                                  P: phytoplankton),
!
!     N(new) = N(old) - uptake * P(old)     uptake = mu * N / (Kn + N)
!                                                    {Michaelis-Menten}
!  below, we set
!                                           The N in the numerator of
!     cff = mu * P(old) / (Kn + N(old))     uptake is treated implicitly
!                                           as N(new)
!
!  so the time-stepping of the equations becomes:
!
!     N(new) = N(old) / (1 + cff)     (1) when substracting a sink term,
!                                         consuming, divide by (1 + cff)
!  and
!
!     P(new) = P(old) + cff * N(new)  (2) when adding a source term,
!                                         growing, add (cff * source)
!
!  Notice that if you substitute (1) in (2), you will get:
!
!     P(new) = P(old) + cff * N(old) / (1 + cff)    (3)
!
!  If you add (1) and (3), you get
!
!     N(new) + P(new) = N(old) + P(old)
!
!  implying conservation regardless how "cff" is computed. Therefore,
!  this scheme is unconditionally stable regardless of the conversion
!  rate. It does not generate negative values since the constituent
!  to be consumed is always treated implicitly. It is also biased
!  toward damping oscillations.
!
!  The iterative loop below is to iterate toward an universal Backward-
!  Euler treatment of all terms. So if there are oscillations in the
!  system, they are only physical oscillations. These iterations,
!  however, do not improve the accuaracy of the solution.
!
        ITER_LOOP: DO Iter=1,BioIter(ng)
          DO k=1,N(ng)
            DO i=Istr,Iend
!
!-----------------------------------------------------------------------
!  Compute dissolution of particles
!-----------------------------------------------------------------------
!
               Bio(i,k,iParticle1) = Bio(i,k,iParticle1)                &
     &                 -dtdays*disso1(ng)*Bio(i,k,iParticle1)
               Bio(i,k,iDissolve1) = Bio(i,k,iDissolve1)                &
     &                 +dtdays*disso1(ng)*Bio(i,k,iParticle1)
#ifdef MULTI_PARTICLES
               Bio(i,k,iParticle2) = Bio(i,k,iParticle2)                &
     &                 -dtdays*disso2(ng)*Bio(i,k,iParticle2)
               Bio(i,k,iDissolve2) = Bio(i,k,iDissolve2)                &
     &                 +dtdays*disso2(ng)*Bio(i,k,iParticle2)

               Bio(i,k,iParticle3) = Bio(i,k,iParticle3)                &
     &                 -dtdays*disso3(ng)*Bio(i,k,iParticle3)
               Bio(i,k,iDissolve3) = Bio(i,k,iDissolve3)                &
     &                 +dtdays*disso3(ng)*Bio(i,k,iParticle3)

               Bio(i,k,iParticle4) = Bio(i,k,iParticle4)                &
     &                 -dtdays*disso4(ng)*Bio(i,k,iParticle4)
               Bio(i,k,iDissolve4) = Bio(i,k,iDissolve4)                &
     &                 +dtdays*disso4(ng)*Bio(i,k,iParticle4)

               Bio(i,k,iParticle5) = Bio(i,k,iParticle5)                &
     &                 -dtdays*disso5(ng)*Bio(i,k,iParticle5)
               Bio(i,k,iDissolve5) = Bio(i,k,iDissolve5)                &
     &                 +dtdays*disso5(ng)*Bio(i,k,iParticle5)

               Bio(i,k,iParticle6) = Bio(i,k,iParticle6)                &
     &                 -dtdays*disso6(ng)*Bio(i,k,iParticle6)
               Bio(i,k,iDissolve6) = Bio(i,k,iDissolve6)                &
     &                 +dtdays*disso6(ng)*Bio(i,k,iParticle6)

               Bio(i,k,iParticle7) = Bio(i,k,iParticle7)                &
     &                 -dtdays*disso7(ng)*Bio(i,k,iParticle7)
               Bio(i,k,iDissolve7) = Bio(i,k,iDissolve7)                &
     &                 +dtdays*disso7(ng)*Bio(i,k,iParticle7)

               Bio(i,k,iParticle8) = Bio(i,k,iParticle8)                &
     &                 -dtdays*disso8(ng)*Bio(i,k,iParticle8)
               Bio(i,k,iDissolve8) = Bio(i,k,iDissolve8)                &
     &                 +dtdays*disso8(ng)*Bio(i,k,iParticle8)

               Bio(i,k,iParticle9) = Bio(i,k,iParticle9)                &
     &                 -dtdays*disso9(ng)*Bio(i,k,iParticle9)
               Bio(i,k,iDissolve9) = Bio(i,k,iDissolve9)                &
     &                 +dtdays*disso9(ng)*Bio(i,k,iParticle9)
#endif 
!
!-----------------------------------------------------------------------
!  Compute external source of particles (Tracer unit/m2/day)
!-----------------------------------------------------------------------
!
#ifdef FULL_DISSOLVE
                 IF (i.eq.iloc_dissolve0(ng)                            &
     &            .and. j.eq.jloc_dissolve0(ng)                         &
     &            .and. k.eq.kloc_dissolve0(ng)                         &
     &            .and. tdays(ng) .ge. dissolve0_startload(ng)          &
     &            .and. tdays(ng) .le. dissolve0_endload(ng)) THEN  
                    Bio(i,k,idissolve0)=Bio(i,k,idissolve0)             & 
     &                     +dissolve0_load(ng)*dtdays/Hz(i,j,k)
                 END IF
#endif 

         	     IF (i.eq.iloc_particle1(ng)                            &
     &            .and. j.eq.jloc_particle1(ng)                         &
     &            .and. k.eq.kloc_particle1(ng)                         &
     &            .and. tdays(ng) .ge. particle1_startload(ng)          &
     &            .and. tdays(ng) .le. particle1_endload(ng)) THEN  
                    Bio(i,k,iParticle1)=Bio(i,k,iParticle1)             & 
     &                     +particle1_load(ng)*dtdays/Hz(i,j,k)
                 END IF

#ifdef MULTI_PARTICLES
                 IF (i.eq.iloc_particle2(ng)                            &
     &            .and. j.eq.jloc_particle2(ng)                         &
     &            .and. k.eq.kloc_particle2(ng)                         &
     &            .and. tdays(ng) .ge. particle2_startload(ng)          &
     &            .and. tdays(ng) .le. particle2_endload(ng)) THEN  
                    Bio(i,k,iParticle2)=Bio(i,k,iParticle2)             & 
     &                     +particle2_load(ng)*dtdays/Hz(i,j,k)
                 END IF

                 IF (i.eq.iloc_particle3(ng)                            &
     &            .and. j.eq.jloc_particle3(ng)                         &
     &            .and. k.eq.kloc_particle3(ng)                         &
     &            .and. tdays(ng) .ge. particle3_startload(ng)          &
     &            .and. tdays(ng) .le. particle3_endload(ng)) THEN  
                    Bio(i,k,iParticle3)=Bio(i,k,iParticle3)             & 
     &                     +particle3_load(ng)*dtdays/Hz(i,j,k)
                 END IF

                 IF (i.eq.iloc_particle4(ng)                            &
     &            .and. j.eq.jloc_particle4(ng)                         &
     &            .and. k.eq.kloc_particle4(ng)                         &
     &            .and. tdays(ng) .ge. particle4_startload(ng)          &
     &            .and. tdays(ng) .le. particle4_endload(ng)) THEN  
                    Bio(i,k,iParticle4)=Bio(i,k,iParticle4)             & 
     &                     +particle4_load(ng)*dtdays/Hz(i,j,k)
                 END IF

                 IF (i.eq.iloc_particle5(ng)                            &
     &            .and. j.eq.jloc_particle5(ng)                         &
     &            .and. k.eq.kloc_particle5(ng)                         &
     &            .and. tdays(ng) .ge. particle5_startload(ng)          &
     &            .and. tdays(ng) .le. particle5_endload(ng)) THEN  
                    Bio(i,k,iParticle5)=Bio(i,k,iParticle5)             & 
     &                     +particle5_load(ng)*dtdays/Hz(i,j,k)
                 END IF

                 IF (i.eq.iloc_particle6(ng)                            &
     &            .and. j.eq.jloc_particle6(ng)                         &
     &            .and. k.eq.kloc_particle6(ng)                         &
     &            .and. tdays(ng) .ge. particle6_startload(ng)          &
     &            .and. tdays(ng) .le. particle6_endload(ng)) THEN  
                    Bio(i,k,iParticle6)=Bio(i,k,iParticle6)             & 
     &                     +particle6_load(ng)*dtdays/Hz(i,j,k)
                 END IF

                 IF (i.eq.iloc_particle7(ng)                            &
     &            .and. j.eq.jloc_particle7(ng)                         &
     &            .and. k.eq.kloc_particle7(ng)                         &
     &            .and. tdays(ng) .ge. particle7_startload(ng)          &
     &            .and. tdays(ng) .le. particle7_endload(ng)) THEN  
                    Bio(i,k,iParticle7)=Bio(i,k,iParticle7)             & 
     &                     +particle7_load(ng)*dtdays/Hz(i,j,k)
                 END IF

                 IF (i.eq.iloc_particle8(ng)                            &
     &            .and. j.eq.jloc_particle8(ng)                         &
     &            .and. k.eq.kloc_particle8(ng)                         &
     &            .and. tdays(ng) .ge. particle8_startload(ng)          &
     &            .and. tdays(ng) .le. particle8_endload(ng)) THEN  
                    Bio(i,k,iParticle8)=Bio(i,k,iParticle8)             & 
     &                     +particle8_load(ng)*dtdays/Hz(i,j,k)
                 END IF

                 IF (i.eq.iloc_particle9(ng)                            &
     &            .and. j.eq.jloc_particle9(ng)                         &
     &            .and. k.eq.kloc_particle9(ng)                         &
     &            .and. tdays(ng) .ge. particle9_startload(ng)          &
     &            .and. tdays(ng) .le. particle9_endload(ng)) THEN  
                    Bio(i,k,iParticle9)=Bio(i,k,iParticle9)             & 
     &                     +particle9_load(ng)*dtdays/Hz(i,j,k)
                 END IF
#endif 
            END DO
          END DO

!
!-----------------------------------------------------------------------
!  Vertical sinking terms: particles
!-----------------------------------------------------------------------
!
!  Reconstruct vertical profile of selected biological constituents
!  "Bio(:,:,isink)" in terms of a set of parabolic segments within each
!  grid box. Then, compute semi-Lagrangian flux due to sinking.
!
          SINK_LOOP: DO isink=1,Nsink
            ibio=idsink(isink)
!
!  Copy concentration of biological particulates into scratch array
!  "qc" (q-central, restrict it to be positive) which is hereafter
!  interpreted as a set of grid-box averaged values for biogeochemical
!  constituent concentration.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                qc(i,k)=Bio(i,k,ibio)
              END DO
            END DO
!
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
                FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
              END DO
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                dltR=Hz(i,j,k)*FC(i,k)
                dltL=Hz(i,j,k)*FC(i,k-1)
                cff=Hz(i,j,k-1)+2.0_r8*Hz(i,j,k)+Hz(i,j,k+1)
                cffR=cff*FC(i,k)
                cffL=cff*FC(i,k-1)
!
!  Apply PPM monotonicity constraint to prevent oscillations within the
!  grid box.
!
                IF ((dltR*dltL).le.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
!
!  Compute right and left side values (bR,bL) of parabolic segments
!  within grid box Hz(k); (WR,WL) are measures of quadratic variations.
!
!  NOTE: Although each parabolic segment is monotonic within its grid
!        box, monotonicity of the whole profile is not guaranteed,
!        because bL(k+1)-bR(k) may still have different sign than
!        qc(i,k+1)-qc(i,k).  This possibility is excluded,
!        after bL and bR are reconciled using WENO procedure.
!
                cff=(dltR-dltL)*Hz_inv3(i,k)
                dltR=dltR-cff*Hz(i,j,k+1)
                dltL=dltL+cff*Hz(i,j,k-1)
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
                WR(i,k)=(2.0_r8*dltR-dltL)**2
                WL(i,k)=(dltR-2.0_r8*dltL)**2
              END DO
            END DO
            cff=1.0E-14_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
                dltL=MAX(cff,WL(i,k  ))
                dltR=MAX(cff,WR(i,k+1))
                bR(i,k)=(dltR*bR(i,k)+dltL*bL(i,k+1))/(dltR+dltL)
                bL(i,k+1)=bR(i,k)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8            ! NO-flux boundary condition
#if defined LINEAR_CONTINUATION
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=2.0_r8*qc(i,N(ng))-bL(i,N(ng))
#elif defined NEUMANN
              bL(i,N(ng))=bR(i,N(ng)-1)
              bR(i,N(ng))=1.5_r8*qc(i,N(ng))-0.5_r8*bL(i,N(ng))
#else
              bR(i,N(ng))=qc(i,N(ng))       ! default strictly monotonic
              bL(i,N(ng))=qc(i,N(ng))       ! conditions
              bR(i,N(ng)-1)=qc(i,N(ng))
#endif
#if defined LINEAR_CONTINUATION
              bR(i,1)=bL(i,2)
              bL(i,1)=2.0_r8*qc(i,1)-bR(i,1)
#elif defined NEUMANN
              bR(i,1)=bL(i,2)
              bL(i,1)=1.5_r8*qc(i,1)-0.5_r8*bR(i,1)
#else
              bL(i,2)=qc(i,1)               ! bottom grid boxes are
              bR(i,1)=qc(i,1)               ! re-assumed to be
              bL(i,1)=qc(i,1)               ! piecewise constant.
#endif
            END DO
!
!  Apply monotonicity constraint again, since the reconciled interfacial
!  values may cause a non-monotonic behavior of the parabolic segments
!  inside the grid box.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                dltR=bR(i,k)-qc(i,k)
                dltL=qc(i,k)-bL(i,k)
                cffR=2.0_r8*dltR
                cffL=2.0_r8*dltL
                IF ((dltR*dltL).lt.0.0_r8) THEN
                  dltR=0.0_r8
                  dltL=0.0_r8
                ELSE IF (ABS(dltR).gt.ABS(cffL)) THEN
                  dltR=cffL
                ELSE IF (ABS(dltL).gt.ABS(cffR)) THEN
                  dltL=cffR
                END IF
                bR(i,k)=qc(i,k)+dltR
                bL(i,k)=qc(i,k)-dltL
              END DO
            END DO
!
!  After this moment reconstruction is considered complete. The next
!  stage is to compute vertical advective fluxes, FC. It is expected
!  that sinking may occurs relatively fast, the algorithm is designed
!  to be free of CFL criterion, which is achieved by allowing
!  integration bounds for semi-Lagrangian advective flux to use as
!  many grid boxes in upstream direction as necessary.
!
!  In the two code segments below, WL is the z-coordinate of the
!  departure point for grid box interface z_w with the same indices;
!  FC is the finite volume flux; ksource(:,k) is index of vertical
!  grid box which contains the departure point (restricted by N(ng)).
!  During the search: also add in content of whole grid boxes
!  participating in FC.
!
            cff=dtdays*ABS(Wbio(isink))
            DO k=1,N(ng)
              DO i=Istr,Iend
                FC(i,k-1)=0.0_r8
                WL(i,k)=z_w(i,j,k-1)+cff
                WR(i,k)=Hz(i,j,k)*qc(i,k)
                ksource(i,k)=k
              END DO
            END DO
            DO k=1,N(ng)
              DO ks=k,N(ng)-1
                DO i=Istr,Iend
                  IF (WL(i,k).gt.z_w(i,j,ks)) THEN
                    ksource(i,k)=ks+1
                    FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                  END IF
                END DO
              END DO
            END DO
!
!  Finalize computation of flux: add fractional part.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                ks=ksource(i,k)
                cu=MIN(1.0_r8,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
                FC(i,k-1)=FC(i,k-1)+                                    &
     &                    Hz(i,j,ks)*cu*                                &
     &                    (bL(i,ks)+                                    &
     &                     cu*(0.5_r8*(bR(i,ks)-bL(i,ks))-              &
     &                         (1.5_r8-cu)*                             &
     &                         (bR(i,ks)+bL(i,ks)-                      &
     &                          2.0_r8*qc(i,ks))))
              END DO
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                Bio(i,k,ibio)=qc(i,k)+(FC(i,k)-FC(i,k-1))*Hz_inv(i,k)
              END DO
            END DO

!
!  Particles reaching the seafloor stay at the bottom. Without this
!  module, particles falls out of the system. 
!
            IF (ibio.eq.iParticle1) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle1)=Bio(i,1,iParticle1)+cff1
              END DO
            END IF

#ifdef MULTI_PARTICLES
            IF (ibio.eq.iParticle2) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle2)=Bio(i,1,iParticle2)+cff1
              END DO
            END IF

            IF (ibio.eq.iParticle3) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle3)=Bio(i,1,iParticle3)+cff1
              END DO
            END IF

            IF (ibio.eq.iParticle4) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle4)=Bio(i,1,iParticle4)+cff1
              END DO
            END IF

            IF (ibio.eq.iParticle5) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle5)=Bio(i,1,iParticle5)+cff1
              END DO
            END IF

            IF (ibio.eq.iParticle6) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle6)=Bio(i,1,iParticle6)+cff1
              END DO
            END IF

            IF (ibio.eq.iParticle7) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle7)=Bio(i,1,iParticle7)+cff1
              END DO
            END IF

            IF (ibio.eq.iParticle8) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle8)=Bio(i,1,iParticle8)+cff1
              END DO
            END IF

            IF (ibio.eq.iParticle9) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
                Bio(i,1,iParticle9)=Bio(i,1,iParticle9)+cff1
              END DO
            END IF
#endif 

            
          END DO SINK_LOOP
        
        END DO ITER_LOOP
!
!-----------------------------------------------------------------------
!  Update global tracer variables: Add increment due to BGC processes
!  to tracer array in time index "nnew". Index "nnew" is solution after
!  advection and mixing and has transport units (m Tunits) hence the
!  increment is multiplied by Hz.  Notice that we need to subtract
!  original values "Bio_old" at the top of the routine to just account
!  for the concentractions affected by BGC processes. This also takes
!  into account any constraints (non-negative concentrations, carbon
!  concentration range) specified before entering BGC kernel. If "Bio"
!  were unchanged by BGC processes, the increment would be exactly
!  zero. Notice that final tracer values, t(:,:,:,nnew,:) are not
!  bounded >=0 so that we can preserve total inventory of N and
!  C even when advection causes tracer concentration to go negative.
!  (J. Wilkin and H. Arango, Apr 27, 2012)
!-----------------------------------------------------------------------
!
        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
!
!-----------------------------------------------------------------------
!  When we run this dissolution model in a 2-way nesting model,
!  overshooting of tracers happened near the river outlet. To resolve
!  this issue, we impose tracer concentration near the river outlet as 0
!-----------------------------------------------------------------------
!
#ifdef AVOID_OVERSHOOT_HRM23
              IF (ng.eq.1) THEN
                IF ((i.ge.33).and.(i.le.36).and.(j.ge.267)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.34).and.(i.le.37)                             &
     &               .and.(j.ge.251).and.(j.le.257)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.22).and.(i.le.24)                             &
     &               .and.(j.ge.249).and.(j.le.254)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.24).and.(i.le.26)                             &
     &               .and.(j.ge.249).and.(j.le.257)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.26).and.(i.le.28)                             &
     &               .and.(j.ge.282)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.8).and.(i.le.10)                             &
     &               .and.(j.ge.296)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF

              ELSE IF (ng.eq.2) THEN
                IF ((i.ge.92).and.(i.le.103).and.(j.ge.53)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.95).and.(i.le.106)                             &
     &               .and.(j.ge.3).and.(j.le.25)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.59).and.(i.le.67)                             &
     &               .and.(j.ge.1).and.(j.le.16)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.65).and.(i.le.73)                             &
     &               .and.(j.ge.1).and.(j.le.25)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.71).and.(i.le.79)                             &
     &               .and.(j.ge.98)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF
                
                IF ((i.ge.17).and.(i.le.25)                             &
     &               .and.(j.ge.140)) THEN
                  Bio(i,k,ibio) = 0.000001_r8
                END IF

              END IF
#endif 
            
              cff=Bio(i,k,ibio)-Bio_old(i,k,ibio)
#ifdef MASKING
              cff=cff*rmask(i,j)
# ifdef WET_DRY
              cff=cff*rmask_wet(i,j)
# endif
#endif
              t(i,j,k,nnew,ibio)=t(i,j,k,nnew,ibio)+cff*Hz(i,j,k)
              
            END DO
          END DO
        END DO
      END DO J_LOOP
!

      RETURN
      END SUBROUTINE biology_tile

