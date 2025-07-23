!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Parameters for the Simple BGC model:                                !
!                                                                      !
!   BioIter  Maximum number of iterations to achieve convergence       !
!              of the nonlinear solution.                              !
#ifdef TEMP_RATES
!   WOC0     Water column respiration rate at T=0C [day-1].            !
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3
!   WOC20    HRM2 water column respiration rate at T=0C [day-1].       !
!   WOC30    HRM3 water column respiration rate at T=0C [day-1].       !
# endif
#else
!   WOC      Water column respiration rate [day-1].                    !
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3
!   WOC2     HRM2 water column respiration rate [day-1].               !
!   WOC3     HRM3 water column respiration rate [day-1].               !
# endif
#endif
!   SOC      Sediment oxygen consumption [day-1].                      !
!   PhyNC    Phytoplankton Nitrogen:Carbon ratio [mol_N/mol_C].        !
!   pCO2air  CO2 partial pressure in the air [ppmv].                   !
#ifdef TALK_ADDITION
!   disso1   dissolution rate of particle1 [day-1].                    !
!   iloc_alkalinity   longitude (i) index of the model grid cell where !
!                     alkalinity is added                              !
!   jloc_alkalinity   latitude (j) index of the model grid cell where  !
!                     alkalinity is added                              !
!   kloc_alkalinity   vertical (k) index of the model grid cell where  !
!                     alkalinity is added                              !
!   alkalinity_load   alkalinity added to the grid cell per day        !
!                     [unit of alkalinity/m2/day].                     !
!   alkalinity_startload   starting day of alkalinity load             !
!   alkalinity_endload     ending day of alkalinity load               !
!   wPar1             vertical sinking velocity for particle1          !
!                     [meter day-1].                                   !
!   P2Dratio   ratio of particles in added alkalinity.                 !
!                                                                      !
#endif
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
!  Set biological tracer identification indices.
!
      integer, allocatable :: idbio(:)  ! Biological tracers
#ifdef CARBON
      integer :: iTIC_                  ! Total inorganic carbon
      integer :: iTAlk                  ! Total alkalinity
#endif
#ifdef OXYGEN
      integer :: iOxyg                  ! Dissolved oxygen concentration
#endif
#ifdef TALK_ADDITION
      integer :: iParticle1              ! particle1
# ifdef TALK_TRACERS
      integer :: iTAin                   ! TA input
      integer :: iTArm                   ! remineralized TA
# endif
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Biological 2D diagnostic variable IDs.
!
      integer, allocatable :: iDbio2(:)       ! 2D biological terms

      integer  :: iCOfx                       ! air-sea CO2 flux
      integer  :: ipCO2                       ! partial pressure of CO2
      integer  :: iO2fx                       ! air-sea O2 flux
#endif
!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)

      real(r8), allocatable :: SOC(:)               ! 1/day
#ifdef TEMP_RATES
      real(r8), allocatable :: WOC0(:)              ! 1/day
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3
      real(r8), allocatable :: WOC20(:)              ! 1/day
      real(r8), allocatable :: WOC30(:)              ! 1/day
# endif
#else
      real(r8), allocatable :: WOC(:)               ! 1/day
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3
      real(r8), allocatable :: WOC2(:)              ! 1/day
      real(r8), allocatable :: WOC3(:)              ! 1/day
# endif
#endif
      real(r8), allocatable :: PhyNC(:)              ! mol_N/mol_C
      real(r8), allocatable :: pCO2air(:)            ! ppmv
#ifdef TALK_ADDITION
      real(r8), allocatable :: iloc_alkalinity(:)
      real(r8), allocatable :: jloc_alkalinity(:)
      real(r8), allocatable :: kloc_alkalinity(:)
      real(r8), allocatable :: alkalinity_load(:)
      real(r8), allocatable :: alkalinity_startload(:)
      real(r8), allocatable :: alkalinity_endload(:)
      real(r8), allocatable :: disso1(:)               ! 1/day               
      real(r8), allocatable :: wPar1(:)
      real(r8), allocatable :: P2Dratio(:)
#endif

      CONTAINS

      SUBROUTINE initialize_biology
!
!=======================================================================
!                                                                      !
!  This routine sets several variables needed by the biology model.    !
!  It allocates and assigns biological tracers indices.                !
!                                                                      !
!=======================================================================
!
!  Local variable declarations
!
      integer :: i, ic
!
!-----------------------------------------------------------------------
!  Determine number of biological tracers.
!-----------------------------------------------------------------------
!
#if defined CARBON && defined OXYGEN
# ifdef TALK_ADDITION
#  ifdef TALK_TRACERS
      NBT=6
#  else
      NBT=4
#  endif
# else
      NBT=3
# endif
#elif defined CARBON && !defined OXYGEN
# ifdef TALK_ADDITION
#  ifdef TALK_TRACERS
      NBT=5
#  else
      NBT=3
#  endif
# else
      NBT=2
# endif
#else
      NBT=1
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
!  Set sources and sinks biology diagnostic parameters.
!-----------------------------------------------------------------------
!
!  Set number of diagnostics terms.
!
      NDbio2d=0
# ifdef CARBON
      NDbio2d=NDbio2d+2
# endif
# ifdef OXYGEN
      NDbio2d=NDbio2d+1
# endif
!
!  Initialize biology diagnostic indices.
!
      ic=0
# ifdef CARBON
      iCOfx=ic+1
      ipCO2=ic+2
      ic=ic+2
# endif
# ifdef OXYGEN
      iO2fx=ic+1
# endif
#endif
!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(BioIter)) THEN
        allocate ( BioIter(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(SOC)) THEN
        allocate ( SOC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
#ifdef TEMP_RATES
      IF (.not.allocated(WOC0)) THEN
        allocate ( WOC0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3
      IF (.not.allocated(WOC20)) THEN
        allocate ( WOC20(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(WOC30)) THEN
        allocate ( WOC30(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# endif
#else
      IF (.not.allocated(WOC)) THEN
        allocate ( WOC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# if defined WOC_HRM23 || defined WOC_H2 || defined WOC_H3
      IF (.not.allocated(WOC2)) THEN
        allocate ( WOC2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      IF (.not.allocated(WOC3)) THEN
        allocate ( WOC3(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
# endif
#endif
      IF (.not.allocated(PhyNC)) THEN
        allocate ( PhyNC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(pCO2air)) THEN
        allocate ( pCO2air(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
#ifdef TALK_ADDITION
      IF (.not.allocated(iloc_alkalinity)) THEN
        allocate ( iloc_alkalinity(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_alkalinity)) THEN
        allocate ( jloc_alkalinity(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_alkalinity)) THEN
        allocate ( kloc_alkalinity(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alkalinity_load)) THEN
        allocate ( alkalinity_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alkalinity_startload)) THEN
        allocate ( alkalinity_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(alkalinity_endload)) THEN
        allocate ( alkalinity_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar1)) THEN
        allocate ( wPar1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso1)) THEN
        allocate ( disso1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(P2Dratio)) THEN
        allocate ( P2Dratio(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#endif
!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
        Dmem(1)=Dmem(1)+REAL(NBT,r8)
      END IF

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Allocate biological diagnostics vectors
!
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
        Dmem(1)=Dmem(1)+REAL(NDbio2d,r8)
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
        idbio(i)=ic+i
      END DO
#ifdef CARBON
      iTIC_=ic+1
      iTAlk=ic+2
      ic=ic+2
# ifdef TALK_ADDITION
      iParticle1=ic+1
      ic=ic+1
#  ifdef TALK_TRACERS
      iTAin=ic+1
      iTArm=ic+2
      ic=ic+2
#  endif
# endif
#endif
#ifdef OXYGEN
      iOxyg=ic+1
      ic=ic+1
#endif

      RETURN
      END SUBROUTINE initialize_biology
