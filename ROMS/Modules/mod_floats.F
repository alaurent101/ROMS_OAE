#include "cppdefs.h"
      MODULE mod_floats
#ifdef FLOATS
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Findex     Indices of spherical coordinates entries in initial      !
!               location arrays, if any.                               !
!  Flon       Initial longitude locations, if any.                     !
!  Flat       Initial latitude locations, if any.                      !
!  Ftype      Float trajectory type:                                   !
!               Ftype(:) = 1,  neutral density 3D Lagrangian           !
!               Ftype(:) = 2,  isobaric (constant depth) float.        !
!  Tinfo      Float trajectory initial information.                    !
!  bounded    Float bounded status switch.                             !
# if defined SOLVE3D && defined FLOAT_VWALK
!  rwalk      Normally distributed random deviates used in vertical    !
!               random walk.                                           !
# endif
# if defined SOLVE3D && defined FLOAT_STICKY
!  stuck      Reflection switch. Floats that hit the surface are       !
!               reflected and floats that hitthe bottom get stick      !
# endif
!  track      Multivariate float trajectory data at several time       !
!               time levels.                                           !
# ifdef FLOAT_PROFILE
!  FLOAT_PROFILE defined, records vertical profile (AL 2022/03/11)     !
!!  track2     2-dimensional biological diagnostics at float location  !
!  track3     Vertically resolved, multivariate float data             !
!  trackw     Keep track of z_w for float data                         !
# endif
!                                                                      !
!=======================================================================
!
        USE mod_param
!
        implicit none
!
!-----------------------------------------------------------------------
!  Lagrangian drifters (floats) structure.
!-----------------------------------------------------------------------
!
        TYPE T_DRIFTER

          logical, pointer  :: bounded(:)

# if defined SOLVE3D && defined FLOAT_STICKY
          logical, pointer  :: stuck(:)
# endif

          integer, pointer :: Findex(:)
          integer, pointer :: Ftype(:)

          real(r8), pointer :: Flon(:)
          real(r8), pointer :: Flat(:)
          real(r8), pointer :: Fz0(:)
          real(r8), pointer :: Tinfo(:,:)
# if defined SOLVE3D && defined FLOAT_VWALK
          real(r8), pointer :: rwalk(:)
# endif
          real(r8), pointer :: track(:,:,:)

# ifdef FLOAT_PROFILE
          real(r8), pointer :: track3(:,:,:,:)
          real(r8), pointer :: trackw(:,:,:)
# endif
        END TYPE T_DRIFTER

        TYPE (T_DRIFTER), allocatable :: DRIFTER(:)
!
!-----------------------------------------------------------------------
!  Lagrangian drifters parameters.
!-----------------------------------------------------------------------
!
!  Switch to control the printing of floats positions to standard output
!  file.
!
        logical, allocatable :: Fprint(:)
!
!  Identification indices.
!
        integer, parameter :: itstr = 0          ! release time
        integer, parameter :: ixgrd = 1          ! x-grid location
        integer, parameter :: iygrd = 2          ! y-grid location
        integer, parameter :: izgrd = 3          ! z-grid location
        integer, parameter :: iflon = 4          ! longitude location
        integer, parameter :: iflat = 5          ! latitude location
        integer, parameter :: idpth = 6          ! depth
        integer, parameter :: ixrhs = 7          ! x-slope
        integer, parameter :: iyrhs = 8          ! y-slope
        integer, parameter :: izrhs = 9          ! z-slope
        integer, parameter :: ifden = 10         ! density anomaly
# ifdef FLOAT_VWALK
        integer, parameter :: ifakt = 11         ! diffusivity, Akt
        integer, parameter :: ifdak = 12         ! d(Akt)/d(s)
# endif
# ifdef FLOAT_OYSTER
#  ifdef FLOAT_VWALK
        integer, parameter :: i1oHz = 13         ! 1/Hz
        integer, parameter :: isizf = 14         ! larvae size (length)
        integer, parameter :: ibrhs = 15         ! behavior RHS
        integer, parameter :: iswim = 16         ! swimming time
        integer, parameter :: iwbio = 17         ! biological w-velocity
        integer, parameter :: iwsin = 18         ! sinking velocity
#  else
        integer, parameter :: i1oHz = 11         ! 1/Hz
        integer, parameter :: isizf = 12         ! larvae size (length)
        integer, parameter :: ibrhs = 13         ! behavior RHS
        integer, parameter :: iswim = 14         ! swimming time
        integer, parameter :: iwbio = 15         ! biological w-velocity
        integer, parameter :: iwsin = 16         ! sinking velocity
#  endif
# endif
# ifdef SOLVE3D
!
!  Tracer variables indices in the track array.
!
        integer, allocatable :: ifTvar(:)
#  if defined FLOAT_PROFILE && defined BIO_FENNEL
#   ifdef DIAGNOSTICS_BIO
!
!  Diagnostic variables indices in the track array.
!
        integer, allocatable :: ifD2var(:)
        integer, allocatable :: ifD3var(:)
#   endif
#  endif
# endif
!
!  Set float tracjectory types:
!
!    flt_Lagran:  3D Lagrangian floats
!    flt_Isobar:  Isobaric floats, p=g*(z+zeta)=constant
!    flt_Geopot:  Geopotential floats, constant depth
!
        integer, parameter :: flt_Lagran = 1
        integer, parameter :: flt_Isobar = 2
        integer, parameter :: flt_Geopot = 3

# ifdef FLOAT_VWALK
!
!  Vertical random walk, initial seed state.
!
        integer :: flt_iseed
# endif
!
!  Floats restart switch.
!
        integer, allocatable :: frrec(:)
!
      CONTAINS
!
      SUBROUTINE allocate_floats (Ldrifter)
!
!=======================================================================
!                                                                      !
!  This routine eihter  allocates and initialize all variables in      !
!  the DRIFTER structure (Ldrifter=.TRUE.) or other parameters in      !
!  the module that are independent of Nfloats (Ldrifter=.FALSE.).      !
!                                                                      !
!=======================================================================
!
      USE mod_scalars
!
!  Imported variable declarations.
!
      logical, intent(in) :: Ldrifter
!
!  Local variable declarations.
!
      integer :: ng, i, ic, iflt
# ifdef FLOAT_PROFILE
      integer :: k, idv
# endif

      real(r8), parameter :: IniVal = 0.0_r8
!
!-----------------------------------------------------------------------
!  Allocate Langrangian drifters structure.
!-----------------------------------------------------------------------
!
      IF (Ldrifter) THEN

        allocate ( DRIFTER(Ngrids) )
!
!  Allocate variables.
!
        DO ng=1,Ngrids
          allocate ( DRIFTER(ng) % bounded(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)

# if defined SOLVE3D && defined FLOAT_STICKY
          allocate ( DRIFTER(ng) % stuck(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
# endif

          allocate ( DRIFTER(ng) % Findex(0:Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng)+1,r8)

          allocate ( DRIFTER(ng) % Ftype(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)

          allocate ( DRIFTER(ng) % Flon(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)

          allocate ( DRIFTER(ng) % Flat(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)

          allocate ( DRIFTER(ng) % Fz0(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)

          allocate ( DRIFTER(ng) % Tinfo(0:izrhs,Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL((izrhs+1)*Nfloats(ng),r8)

# if defined SOLVE3D && defined FLOAT_VWALK
          allocate ( DRIFTER(ng) % rwalk(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
# endif

# ifdef FLOAT_PROFILE
#  if defined DIAGNOSTICS_BIO && defined BIO_FENNEL
          allocate ( DRIFTER(ng) % track(NFV(ng)+NDbio2d+NDbio3d,0:NFT, &
     &                                   Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL((NFV(ng)+NDbio2d+NDbio3d)*(NFT+1)*     &
     &                            Nfloats(ng),r8)

          allocate ( DRIFTER(ng) % track3(NFV(ng)+NDbio2d+NDbio3d,      &
     &                                    0:NFT,N(ng),Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL((NFV(ng)+NDbio2d+NDbio3d)*(NFT+1)*     &
     &                                    N(ng)*Nfloats(ng),r8)
#  else
          allocate ( DRIFTER(ng) % track(NFV(ng),0:NFT,Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(NFV(ng)*(NFT+1)*Nfloats(ng),r8)

          allocate ( DRIFTER(ng) % track3(NFV(ng),0:NFT,N(ng),          &
     &                                    Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(NFV(ing)*(NFT+1)*N(ng)*Nfloats(ng),r8)
#  endif
          allocate ( DRIFTER(ng) % trackw(0:NFT,N(ng)+1,Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL((NFT+1)*(N(ng)+1)*Nfloats(ng),r8)
# else
          allocate ( DRIFTER(ng) % track(NFV(ng),0:NFT,Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(NFV(ng)*(NFT+1)*Nfloats(ng),r8)
# endif

        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Lagrangian drifters parameters.
!-----------------------------------------------------------------------
!
      IF (.not.Ldrifter) THEN
        allocate ( Fprint(Ngrids) )
        allocate ( frrec(Ngrids) )

# ifdef SOLVE3D
        allocate ( ifTvar(MT) )
#  if defined FLOAT_PROFILE && defined BIO_FENNEL
#   ifdef DIAGNOSTICS_BIO
        allocate ( ifD2var(NDbio2d) )
        allocate ( ifD3var(NDbio3d) )
#   endif
#  endif
# endif
      END IF
!
!-----------------------------------------------------------------------
!  Initialize Langrangian drifters structure.
!-----------------------------------------------------------------------
!
      IF (Ldrifter) THEN
        DO ng=1,Ngrids
          DRIFTER(ng) % Findex(0) = 0
          DO iflt=1,Nfloats(ng)
            DRIFTER(ng) % bounded(iflt) = .FALSE.
# if defined SOLVE3D && defined FLOAT_STICKY
            DRIFTER(ng) % stuck(iflt) = .FALSE.
# endif
            DRIFTER(ng) % Findex(iflt) = 0
            DRIFTER(ng) % Ftype(iflt) = 0
            DRIFTER(ng) % Flon(iflt) = IniVal
            DRIFTER(ng) % Flat(iflt) = IniVal
            DRIFTER(ng) % Fz0(iflt) = 0
# if defined SOLVE3D && defined FLOAT_VWALK
            DRIFTER(ng) % rwalk = IniVal
# endif
            DO i=0,izrhs
              DRIFTER(ng) % Tinfo(i,iflt) = IniVal
            END DO
            DO i=1,NFV(ng)
              DRIFTER(ng) % track(i,0,iflt) = IniVal
              DRIFTER(ng) % track(i,1,iflt) = IniVal
              DRIFTER(ng) % track(i,2,iflt) = IniVal
              DRIFTER(ng) % track(i,3,iflt) = IniVal
              DRIFTER(ng) % track(i,4,iflt) = IniVal
# ifdef FLOAT_PROFILE
              DO k=1,NT(ng)
                DRIFTER(ng) % track3(i,0,k,iflt) = IniVal
                DRIFTER(ng) % track3(i,1,k,iflt) = IniVal
                DRIFTER(ng) % track3(i,2,k,iflt) = IniVal
                DRIFTER(ng) % track3(i,3,k,iflt) = IniVal
                DRIFTER(ng) % track3(i,4,k,iflt) = IniVal
              END DO
#  if defined DIAGNOSTICS_BIO && defined BIO_FENNEL
              IF (i.eq.NFV(ng)) THEN
                DO idv=1,NDbio2d+NDbio3d
                  DRIFTER(ng) % track(i+idv,0,iflt) = IniVal
                  DRIFTER(ng) % track(i+idv,1,iflt) = IniVal
                  DRIFTER(ng) % track(i+idv,2,iflt) = IniVal
                  DRIFTER(ng) % track(i+idv,3,iflt) = IniVal
                  DRIFTER(ng) % track(i+idv,4,iflt) = IniVal
                  DO k=1,NT(ng)
                    DRIFTER(ng) % track3(i+idv,0,k,iflt) = IniVal
                    DRIFTER(ng) % track3(i+idv,1,k,iflt) = IniVal
                    DRIFTER(ng) % track3(i+idv,2,k,iflt) = IniVal
                    DRIFTER(ng) % track3(i+idv,3,k,iflt) = IniVal
                    DRIFTER(ng) % track3(i+idv,4,k,iflt) = IniVal
                  END DO
                END DO
              END IF
#  endif
# endif
            END DO
# ifdef FLOAT_PROFILE
            DO k=1,NT(ng)+1
              DRIFTER(ng) % trackw(0,k,iflt) = IniVal
              DRIFTER(ng) % trackw(1,k,iflt) = IniVal
              DRIFTER(ng) % trackw(2,k,iflt) = IniVal
              DRIFTER(ng) % trackw(3,k,iflt) = IniVal
              DRIFTER(ng) % trackw(4,k,iflt) = IniVal
            END DO
# endif
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Initialize Langrangian drifters parameters.
!-----------------------------------------------------------------------
!
      IF (.not.Ldrifter) THEN
# ifdef FLOAT_VWALK
        flt_iseed=149876
# endif
        DO ng=1,Ngrids
          Fprint(ng)=.TRUE.
        END DO

# ifdef SOLVE3D
!
!  Indices for tracer variables in the floats array track.
!
# ifdef FLOAT_OYSTER
#  ifdef FLOAT_VWALK
        ic=18
#  else
        ic=16
#  endif
# else
#  ifdef FLOAT_VWALK
        ic=12
#  else
        ic=10
#  endif
# endif
        DO i=1,MT
          ic=ic+1
          ifTvar(i)=ic
        END DO
# if defined FLOAT_PROFILE && defined BIO_FENNEL
#  ifdef DIAGNOSTICS_BIO
        k=ic
        DO i=1,NDbio2d
          ic=ic+1
          ifD2var(i)=ic
        END DO
        DO i=1,NDbio3d
          ic=ic+1
          ifD3var(i)=ic
        END DO
#  endif
# endif
#endif
      END IF

      RETURN
      END SUBROUTINE allocate_floats
#endif
      END MODULE mod_floats
