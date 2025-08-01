#include "cppdefs.h"
      MODULE mod_diags
#ifdef DIAGNOSTICS
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Diagnostics fields for output.                                      !
!                                                                      !
!  DiaBio2d  Diagnostics for 2D biological terms.                      !
!  DiaBio3d  Diagnostics for 3D biological terms.                      !
!  DiaBio4d  Diagnostics for 4D bio-optical terms.                     !
!  DiaTrc    Diagnostics for tracer terms.                             !
!  DiaU2d    Diagnostics for 2D U-momentum terms.                      !
!  DiaV2d    Diagnostics for 2D V-momentum terms.                      !
!  DiaU3d    Diagnostics for 3D U-momentum terms.                      !
!  DiaV3d    Diagnostics for 3D V-momentum terms.                      !
!                                                                      !
!  Diagnostics fields work arrays.                                     !
!                                                                      !
!  DiaTwrk    Diagnostics work array for tracer terms.                 !
!  DiaU2wrk   Diagnostics work array for 2D U-momentum terms.          !
!  DiaV2wrk   Diagnostics work array for 2D V-momentum terms.          !
!  DiaRUbar   Diagnostics RHS array for 2D U-momentum terms.           !
!  DiaRVbar   Diagnostics RHS array for 2D V-momentum terms.           !
!  DiaU2int   Diagnostics array for 2D U-momentum terms                !
!               integrated over short barotropic timesteps.            !
!  DiaV2int   Diagnostics array for 2D U-momentum terms                !
!               integrated over short barotropic timesteps.            !
!  DiaRUfrc   Diagnostics forcing array for 2D U-momentum terms.       !
!  DiaRVfrc   Diagnostics forcing array for 2D V-momentum terms.       !
!  DiaU3wrk   Diagnostics work array for 3D U-momentum terms.          !
!  DiaV3wrk   Diagnostics work array for 3D V-momentum terms.          !
!  DiaRU      Diagnostics RHS array for 3D U-momentum terms.           !
!  DiaRV      Diagnostics RHS array for 3D V-momentum terms.           !
!                                                                      !
!=======================================================================
!
        USE mod_kinds
!
        implicit none
!
        TYPE T_DIAGS

# ifdef DIAGNOSTICS_BIO
#  if defined BIO_FENNEL
          real(r8), pointer :: DiaBio2d(:,:,:)
          real(r8), pointer :: DiaBio3d(:,:,:,:)
#   ifdef FLOAT_PROFILE
          real(r8), pointer :: DiaFlt2d(:,:,:)
          real(r8), pointer :: DiaFlt3d(:,:,:,:)
#   endif
#  elif defined REDUCED_BGC 
          real(r8), pointer :: DiaBio2d(:,:,:)
#  elif defined ECOSIM
          real(r8), pointer :: DiaBio3d(:,:,:,:)
          real(r8), pointer :: DiaBio4d(:,:,:,:,:)
#  endif
# endif

# ifdef DIAGNOSTICS_TS
          real(r8), pointer :: DiaTrc(:,:,:,:,:)
          real(r8), pointer :: DiaTwrk(:,:,:,:,:)
# endif

# ifdef DIAGNOSTICS_UV
          real(r8), pointer :: DiaU2d(:,:,:)
          real(r8), pointer :: DiaV2d(:,:,:)
          real(r8), pointer :: DiaU2wrk(:,:,:)
          real(r8), pointer :: DiaV2wrk(:,:,:)
          real(r8), pointer :: DiaRUbar(:,:,:,:)
          real(r8), pointer :: DiaRVbar(:,:,:,:)

#  ifdef SOLVE3D
          real(r8), pointer :: DiaU2int(:,:,:)
          real(r8), pointer :: DiaV2int(:,:,:)
          real(r8), pointer :: DiaRUfrc(:,:,:,:)
          real(r8), pointer :: DiaRVfrc(:,:,:,:)
          real(r8), pointer :: DiaU3d(:,:,:,:)
          real(r8), pointer :: DiaV3d(:,:,:,:)
          real(r8), pointer :: DiaU3wrk(:,:,:,:)
          real(r8), pointer :: DiaV3wrk(:,:,:,:)
          real(r8), pointer :: DiaRU(:,:,:,:,:)
          real(r8), pointer :: DiaRV(:,:,:,:,:)
#  endif
# endif

        END TYPE T_DIAGS

        TYPE (T_DIAGS), allocatable :: DIAGS(:)
!
      CONTAINS
!
      SUBROUTINE allocate_diags (ng, LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine allocates all variables in the module for all nested   !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
# if defined DIAGNOSTICS_BIO && defined ECOSIM
      USE mod_biology
# endif
!
!  Local variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      real(r8) :: size2d
!
!-----------------------------------------------------------------------
!  Allocate module variables.
!-----------------------------------------------------------------------
!
      IF (ng.eq.1 ) allocate ( DIAGS(Ngrids) )
!
!  Set horizontal array size.
!
      size2d=REAL((UBi-LBi+1)*(UBj-LBj+1),r8)
!
!  Diagnostic arrays.
!
# ifdef DIAGNOSTICS_BIO
#  if defined BIO_FENNEL
      IF (NDbio2d.gt.0) THEN
        allocate ( DIAGS(ng) % DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d) )
        Dmem(ng)=Dmem(ng)+REAL(NDbio2d,r8)*size2d
      END IF
      IF (NDbio3d.gt.0) THEN
        allocate ( DIAGS(ng) % DiaBio3d(LBi:UBi,LBj:UBj,N(ng),NDbio3d) )
        Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDbio3d,r8)*size2d
      END IF
#   ifdef FLOAT_PROFILE
      IF (NDbio2d.gt.0) THEN
        allocate ( DIAGS(ng) % DiaFlt2d(LBi:UBi,LBj:UBj,NDbio2d) )
        Dmem(ng)=Dmem(ng)+REAL(NDbio2d,r8)*size2d
      END IF
      IF (NDbio3d.gt.0) THEN
        allocate ( DIAGS(ng) % DiaFlt3d(LBi:UBi,LBj:UBj,N(ng),NDbio3d) )
        Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDbio3d,r8)*size2d
      END IF
#   endif
#  elif defined REDUCED_BGC 
      IF (NDbio2d.gt.0) THEN
        allocate ( DIAGS(ng) % DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d) )
        Dmem(ng)=Dmem(ng)+REAL(NDbio2d,r8)*size2d
      END IF
#  elif defined ECOSIM
      IF (NDbio3d.gt.0) THEN
        allocate ( DIAGS(ng) % DiaBio3d(LBi:UBi,LBj:UBj,                &
     &                                  NDbands,NDbio3d) )
        Dmem(ng)=Dmem(ng)+REAL(NDbands*NDbio3d,r8)*size2d
      END IF
      IF (NDbio4d.gt.0) THEN
        allocate ( DIAGS(ng) % DiaBio4d(LBi:UBi,LBj:UBj,N(ng),          &
     &                                  NDbands,NDbio4d) )
        Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDbands*NDbio4d,r8)*size2d
      END IF
#  endif
# endif

# ifdef DIAGNOSTICS_TS
      allocate ( DIAGS(ng) % DiaTrc(LBi:UBi,LBj:UBj,N(ng),NT(ng),NDT) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NT(ng)*NDT,r8)*size2d

      allocate ( DIAGS(ng) % DiaTwrk(LBi:UBi,LBj:UBj,N(ng),NT(ng),NDT) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NT(ng)*NDT,r8)*size2d
# endif

# ifdef DIAGNOSTICS_UV
      allocate ( DIAGS(ng) % DiaU2d(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d

      allocate ( DIAGS(ng) % DiaV2d(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d

      allocate ( DIAGS(ng) % DiaU2wrk(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d

      allocate ( DIAGS(ng) % DiaV2wrk(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d

      allocate ( DIAGS(ng) % DiaRUbar(LBi:UBi,LBj:UBj,2,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(NDM2d-1,r8)*size2d

      allocate ( DIAGS(ng) % DiaRVbar(LBi:UBi,LBj:UBj,2,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(NDM2d-1,r8)*size2d

#  ifdef SOLVE3D
      allocate ( DIAGS(ng) % DiaU2int(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d

      allocate ( DIAGS(ng) % DiaV2int(LBi:UBi,LBj:UBj,NDM2d) )
      Dmem(ng)=Dmem(ng)+REAL(NDM2d,r8)*size2d

      allocate ( DIAGS(ng) % DiaRUfrc(LBi:UBi,LBj:UBj,3,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+3.0_r8*REAL(NDM2d-1,r8)*size2d

      allocate ( DIAGS(ng) % DiaRVfrc(LBi:UBi,LBj:UBj,3,NDM2d-1) )
      Dmem(ng)=Dmem(ng)+3.0_r8*REAL(NDM2d-1,r8)*size2d

      allocate ( DIAGS(ng) % DiaU3d(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d

      allocate ( DIAGS(ng) % DiaV3d(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d

      allocate ( DIAGS(ng) % DiaU3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d

      allocate ( DIAGS(ng) % DiaV3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d) )
      Dmem(ng)=Dmem(ng)+REAL(N(ng)*NDM3d,r8)*size2d

      allocate ( DIAGS(ng) % DiaRU(LBi:UBi,LBj:UBj,N(ng),2,NDrhs) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(N(ng)*NDrhs,r8)*size2d

      allocate ( DIAGS(ng) % DiaRV(LBi:UBi,LBj:UBj,N(ng),2,NDrhs) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(N(ng)*NDrhs,r8)*size2d

#  endif

# endif
!
      RETURN
      END SUBROUTINE allocate_diags
!
      SUBROUTINE initialize_diags (ng, tile)
!
!=======================================================================
!                                                                      !
!  This routine initialize all variables in the module using first     !
!  touch distribution policy. In shared-memory configuration, this     !
!  operation actually performs propagation of the  "shared arrays"     !
!  across the cluster, unless another policy is specified to           !
!  override the default.                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
# if defined DIAGNOSTICS_BIO && defined ECOSIM
      USE mod_biology
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, idiag, j
# ifdef SOLVE3D
      integer :: itrc, iband, k
# endif

      real(r8), parameter :: IniVal = 0.0_r8

# include "set_bounds.h"
!
!  Set array initialization range.
!
# ifdef DISTRIBUTE
      Imin=BOUNDS(ng)%LBi(tile)
      Imax=BOUNDS(ng)%UBi(tile)
      Jmin=BOUNDS(ng)%LBj(tile)
      Jmax=BOUNDS(ng)%UBj(tile)
# else
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        Imin=BOUNDS(ng)%LBi(tile)
      ELSE
        Imin=Istr
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        Imax=BOUNDS(ng)%UBi(tile)
      ELSE
        Imax=Iend
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        Jmin=BOUNDS(ng)%LBj(tile)
      ELSE
        Jmin=Jstr
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        Jmax=BOUNDS(ng)%UBj(tile)
      ELSE
        Jmax=Jend
      END IF
# endif
!
!-----------------------------------------------------------------------
!  Initialize module variables.
!-----------------------------------------------------------------------
!
      DO j=Jmin,Jmax
# ifdef DIAGNOSTICS_BIO
#  if defined BIO_FENNEL
        IF (NDbio2d.gt.0) THEN
          DO idiag=1,NDbio2d
            DO i=Imin,Imax
              DIAGS(ng) % DiaBio2d(i,j,idiag) = IniVal
            END DO
          END DO
        END IF
        IF (NDbio3d.gt.0) THEN
          DO idiag=1,NDbio3d
            DO k=1,N(ng)
              DO i=Imin,Imax
                DIAGS(ng) % DiaBio3d(i,j,k,idiag) = IniVal
              END DO
            END DO
          END DO
        END IF
#   ifdef FLOAT_PROFILE
        IF (NDbio2d.gt.0) THEN
          DO idiag=1,NDbio2d
            DO i=Imin,Imax
              DIAGS(ng) % DiaFlt2d(i,j,idiag) = IniVal
            END DO
          END DO
        END IF
        IF (NDbio3d.gt.0) THEN
          DO idiag=1,NDbio3d
            DO k=1,N(ng)
              DO i=Imin,Imax
                DIAGS(ng) % DiaFlt3d(i,j,k,idiag) = IniVal
              END DO
            END DO
          END DO
        END IF
#   endif
#  elif defined REDUCED_BGC
        IF (NDbio2d.gt.0) THEN
          DO idiag=1,NDbio2d
            DO i=Imin,Imax
              DIAGS(ng) % DiaBio2d(i,j,idiag) = IniVal
            END DO
          END DO
        END IF
#  elif defined ECOSIM
        IF (NDbio3d.gt.0) THEN
          DO idiag=1,NDbio3d
            DO k=1,NDbands
              DO i=Imin,Imax
                DIAGS(ng) % DiaBio3d(i,j,k,idiag) = IniVal
              END DO
            END DO
          END DO
        END IF
        IF (NDbio4d.gt.0) THEN
          DO idiag=1,NDbio4d
            DO iband=1,NDbands
              DO k=1,N(ng)
                DO i=Imin,Imax
                  DIAGS(ng) % DiaBio4d(i,j,k,iband,idiag) = IniVal
                END DO
              END DO
            END DO
          END DO
        END IF
#  endif
# endif
# ifdef DIAGNOSTICS_TS
        DO idiag=1,NDT
          DO itrc=1,NT(ng)
            DO k=1,N(ng)
              DO i=Imin,Imax
                DIAGS(ng) % DiaTrc(i,j,k,itrc,idiag) =  IniVal
                DIAGS(ng) % DiaTwrk(i,j,k,itrc,idiag) = IniVal
              END DO
            END DO
          END DO
        END DO
# endif
# ifdef DIAGNOSTICS_UV
        DO idiag=1,NDM2d
          DO i=Imin,Imax
            DIAGS(ng) % DiaU2d(i,j,idiag) = IniVal
            DIAGS(ng) % DiaV2d(i,j,idiag) = IniVal
            DIAGS(ng) % DiaU2wrk(i,j,idiag) = IniVal
            DIAGS(ng) % DiaV2wrk(i,j,idiag) = IniVal
#  ifdef SOLVE3D
            DIAGS(ng) % DiaU2int(i,j,idiag) = IniVal
            DIAGS(ng) % DiaV2int(i,j,idiag) = IniVal
#  endif
          END DO
        END DO
        DO idiag=1,NDM2d-1
          DO i=Imin,Imax
            DIAGS(ng) % DiaRUbar(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRUbar(i,j,2,idiag) = IniVal
            DIAGS(ng) % DiaRVbar(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRVbar(i,j,2,idiag) = IniVal
#  ifdef SOLVE3D
            DIAGS(ng) % DiaRUfrc(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRUfrc(i,j,2,idiag) = IniVal
            DIAGS(ng) % DiaRUfrc(i,j,3,idiag) = IniVal
            DIAGS(ng) % DiaRVfrc(i,j,1,idiag) = IniVal
            DIAGS(ng) % DiaRVfrc(i,j,2,idiag) = IniVal
            DIAGS(ng) % DiaRVfrc(i,j,3,idiag) = IniVal
#  endif
          END DO
        END DO
#  ifdef SOLVE3D
        DO idiag=1,NDM3d
          DO k=1,N(ng)
            DO i=Imin,Imax
              DIAGS(ng) % DiaU3d(i,j,k,idiag) = IniVal
              DIAGS(ng) % DiaV3d(i,j,k,idiag) = IniVal
              DIAGS(ng) % DiaU3wrk(i,j,k,idiag) = IniVal
              DIAGS(ng) % DiaV3wrk(i,j,k,idiag) = IniVal
            END DO
          END DO
        END DO
        DO idiag=1,NDrhs
          DO k=1,N(ng)
            DO i=Imin,Imax
              DIAGS(ng) % DiaRU(i,j,k,1,idiag) = IniVal
              DIAGS(ng) % DiaRU(i,j,k,2,idiag) = IniVal
              DIAGS(ng) % DiaRV(i,j,k,1,idiag) = IniVal
              DIAGS(ng) % DiaRV(i,j,k,2,idiag) = IniVal
            END DO
          END DO
        END DO
#  endif
# endif
      END DO
!
      RETURN
      END SUBROUTINE initialize_diags
#endif
      END MODULE mod_diags
