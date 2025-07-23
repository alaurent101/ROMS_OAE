!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Parameters for Fennel et al. (2006) model:                          !
!                                                                      !
!   AttSW    Light attenuation due to sea water [1/m].                 !
!   AttChl   Light attenuation by Chlorophyll [1/(mg_Chl m2)].         !
!   BioIter  Maximum number of iterations to achieve convergence       !
!              of the nonlinear solution.                              !
!   Chl2C_m  Maximum chlorophyll to carbon ratio [mg_Chl/mg_C].        !
!   ChlMin   Chlorophill minimum threshold value [mg_Chl/m3].          !
!   CoagR    Coagulation rate: agregation rate of SDeN + Phyt ==> LDeN !
!              [1/day].                                                !
!   D_p5NH4  Half-saturation radiation for nitrification inhibition    !
!              [Watts/m2].                                             !
!   I_thNH4  Radiation threshold for nitrification inhibition          !
!              [Watts/m2].                                             !
!   K_NH4    Inverse half-saturation for Phytoplankton NH4 uptake      !
!              [m3/(mmol_N)].                                          !
!   K_NO3    Inverse half-saturation for Phytoplankton NO3 uptake      !
!              [m3/(mmol_N)].                                          !
!   K_PO4    Inverse half-saturation for Phytoplankton PO4 uptake      !
!              [m3/(mmol_P)].                                          !
!   K_Phy    Zooplankton half-saturation, squared constant for         !
!              ingestion [mmol_N/m3]^2.                                !
!   LDeRR    Large Detrital re-mineralization rate [1/day].            !
!   NitriR   Nitrification rate: oxidation of NH4 to NO3 [1/day].      !
!   PARfrac  Fraction of shortwave radiation that is available for     !
!              photosyntesis [nondimensional].                         !
!   PhyCN    Phytoplankton Carbon:Nitrogen ratio [mol_C/mol_N].        !
!   R_P2N    Phytoplankton Phosphorus:Nitrogen ratio [mol_P/mol_N].    !
!   PhyIP    Phytoplankton NH4 inhibition parameter [1/(mmol_N)].      !
!   PhyIS    Phytoplankton, initial slope of the P-I curve             !
!              [1/(W m-2 day)].                                        !
!   ZooMin   Phytoplankton minimum threshold value [mmol_N/m3].        !
!   PhyMR    Phytoplankton mortality rate [1/day] to small detritus.   !
!   SDeAR    Small detritus aggregation rate into Large detritus       !
!              [1/day].                                                !
!   SDeBR    Small Detrital breakdown to NH4 rate [1/day].             !
!   SDeRR    Large Detrital re-mineralization rate [1/day].            !
!   RDeRR    River Detrital re-mineralization rate [1/day].            !
!   Vp0      Eppley temperature-limited and light-limited growth       !
!              tuning parameter [nondimensional].                      !
!   wLDet    Vertical sinking velocities for Large Detritus            !
!              fraction [m/day].                                       !
!   wPhy     Vertical sinking velocity for Phytoplankton               !
!              fraction [m/day].                                       !
!   wSDet    Vertical sinking velocities for Small Detritus            !
!              fraction [m/day].                                       !
!   ZooAE_N  Zooplankton nitrogen assimilation efficiency fraction     !
!              [nondimensional].                                       !
!   ZooBM    Zooplankton basal metabolism [1/day].                     !
!   ZooCN    Zooplankton Carbon:Nitrogen ratio [mol_C/mol_N].          !
!   ZooER    Zooplankton specific excretion rate [1/day].              !
!   ZooGR    Zooplankton maximum growth rate [1/day].                  !
!   ZooMin   Zooplankton minimum threshold value [mmol_N/m3].          !
!   ZooMR    Zooplankton mortality to Detritus [1/day].                !
!   pCO2air  CO2 partial pressure in the air [ppmv].                   !
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
      integer :: iNO3_                  ! Nitrate concentration
      integer :: iNH4_                  ! Ammonium concentration
#ifdef PO4
      integer :: iPO4_                  ! Phosphate concentration
#endif
#if defined BIO_2P2Z || defined BIO_2P3Z
      integer :: iSPhy                  ! Small phytoplankton concentration
      integer :: iPhyL                  ! Large phytoplankton concentration
      integer :: iChlS                  ! Chlorophyll concentration
      integer :: iChlL                  ! Chlorophyll concentration
      integer :: iZooS                  ! Small zooplankton concentration
      integer :: iZooL                  ! Zooplankton concentration
#  ifdef BIO_2P3Z
      integer :: iZooP                  ! Zooplankton concentration
#  endif
#else
      integer :: iChlo                  ! Chlorophyll concentration
      integer :: iPhyt                  ! Phytoplankton concentration
      integer :: iZoop                  ! Zooplankton concentration
#endif
      integer :: iLDeN                  ! Large detritus N-concentration
      integer :: iSDeN                  ! Small detritus N-concentration
#ifdef RIVER_DON
      integer :: iRDeN                  ! River detritus N-concentration
#endif
#ifdef CARBON
      integer :: iLDeC                  ! Large detritus C-concentration
      integer :: iSDeC                  ! Small detritus C-concentration
      integer :: iTIC_                  ! Total inorganic carbon
      integer :: iTAlk                  ! Total alkalinity
# ifdef RIVER_DON
      integer :: iRDeC                  ! River detritus C-concentration
# endif
#endif
#ifdef OXYGEN
      integer :: iOxyg                  ! Dissolved oxygen concentration
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Biological 2D diagnostic variable IDs.
!
      integer, allocatable :: iDbio2(:)       ! 2D biological terms

      integer  :: iCOfx                       ! air-sea CO2 flux
      integer  :: iDNIT                       ! denitrification flux
      integer  :: ipCO2                       ! partial pressure of CO2
      integer  :: iO2fx                       ! air-sea O2 flux
!
!  Biological 3D diagnostic variable IDs.
!
      integer, allocatable :: iDbio3(:)       ! 3D biological terms

      integer  :: iPPro = 1                   ! primary productivity
      integer  :: iNO3u = 2                   ! NO3 uptake
      integer  :: iNifx = 3                   ! Nitrification flux
#endif
!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)

      real(r8), allocatable :: AttSW(:)              ! 1/m
      real(r8), allocatable :: AttChl(:)             ! 1/(mg_Chl m2)
      real(r8), allocatable :: ChlMin(:)             ! mg_Chl/m3
      real(r8), allocatable :: CoagR(:)              ! 1/day
      real(r8), allocatable :: D_p5NH4(:)            ! Watts/m2
      real(r8), allocatable :: I_thNH4(:)            ! Watts/m2
      real(r8), allocatable :: LDeRRN(:)             ! 1/day
      real(r8), allocatable :: LDeRRC(:)             ! 1/day
      real(r8), allocatable :: PARfrac(:)            ! nondimensional
      real(r8), allocatable :: PhyCN(:)              ! mol_C/mol_N
      real(r8), allocatable :: R_P2N(:)              ! mol_P/mol_N
      real(r8), allocatable :: PhyIP(:)              ! 1/mmol_N
      real(r8), allocatable :: PhyMin(:)             ! mmol_N/m3
      real(r8), allocatable :: NitriR(:)             ! 1/day
#ifdef TEMP_RATES
#  if defined BIO_2P2Z || defined BIO_2P3Z
      real(r8), allocatable :: PsMR0(:)              ! 1/day
      real(r8), allocatable :: PlMR0(:)              ! 1/day
      
      real(r8), allocatable :: ZooSBM0(:)            ! 1/day
      real(r8), allocatable :: ZooSER0(:)            ! 1/day
      real(r8), allocatable :: ZooSPsGR0(:)          ! 1/day
#    ifdef BIO_2P2Z
      real(r8), allocatable :: ZooSPlGR0(:)          ! 1/day
      real(r8), allocatable :: ZooSDsGR0(:)          ! 1/day
#    endif
      real(r8), allocatable :: ZooSMR0(:)            ! 1/day
      
      real(r8), allocatable :: ZooLBM0(:)            ! 1/day
      real(r8), allocatable :: ZooLER0(:)            ! 1/day
      real(r8), allocatable :: ZooLPsGR0(:)          ! 1/day
      real(r8), allocatable :: ZooLPlGR0(:)          ! 1/day
      real(r8), allocatable :: ZooLZsGR0(:)          ! 1/day
#    ifdef BIO_2P2Z
      real(r8), allocatable :: ZooLDsGR0(:)          ! 1/day
#    endif
      real(r8), allocatable :: ZooLMR0(:)            ! 1/day
#    ifdef BIO_2P3Z
      real(r8), allocatable :: ZooPBM0(:)            ! 1/day
      real(r8), allocatable :: ZooPER0(:)            ! 1/day
      real(r8), allocatable :: ZooPPlGR0(:)          ! 1/day
      real(r8), allocatable :: ZooPZsGR0(:)          ! 1/day
      real(r8), allocatable :: ZooPZlGR0(:)          ! 1/day
      real(r8), allocatable :: ZooPMR0(:)            ! 1/day
#    endif
#  else
      real(r8), allocatable :: PhyMR0(:)             ! 1/day
      real(r8), allocatable :: ZooBM0(:)             ! 1/day
      real(r8), allocatable :: ZooER0(:)             ! 1/day
      real(r8), allocatable :: ZooGR0(:)             ! 1/day
      real(r8), allocatable :: ZooMR0(:)             ! 1/day
#  endif
#else
#  if defined BIO_2P2Z || defined BIO_2P3Z
      real(r8), allocatable :: PsMR(:)               ! 1/day
      real(r8), allocatable :: PlMR(:)               ! 1/day
      real(r8), allocatable :: ZooSBM(:)             ! 1/day
      real(r8), allocatable :: ZooSER(:)             ! 1/day
      real(r8), allocatable :: ZooSPsGR(:)           ! 1/day
#    ifdef BIO_2P2Z
      real(r8), allocatable :: ZooSPlGR(:)          ! 1/day
      real(r8), allocatable :: ZooSDsGR(:)          ! 1/day
#    endif
      real(r8), allocatable :: ZooSMR(:)             ! 1/day

      real(r8), allocatable :: ZooLBM(:)            ! 1/day
      real(r8), allocatable :: ZooLER(:)            ! 1/day
      real(r8), allocatable :: ZooLPsGR(:)          ! 1/day
      real(r8), allocatable :: ZooLPlGR(:)          ! 1/day
      real(r8), allocatable :: ZooLZsGR(:)          ! 1/day
#    ifdef BIO_2P2Z
      real(r8), allocatable :: ZooLDsGR(:)          ! 1/day
#    endif
      real(r8), allocatable :: ZooLMR(:)            ! 1/day
#    ifdef BIO_2P3Z
      real(r8), allocatable :: ZooPBM(:)            ! 1/day
      real(r8), allocatable :: ZooPER(:)            ! 1/day
      real(r8), allocatable :: ZooPPlGR(:)          ! 1/day
      real(r8), allocatable :: ZooPZsGR(:)          ! 1/day
      real(r8), allocatable :: ZooPZlGR(:)          ! 1/day
      real(r8), allocatable :: ZooPMR(:)            ! 1/day
#    endif
#  else
      real(r8), allocatable :: PhyMR(:)              ! 1/day
      real(r8), allocatable :: ZooBM(:)              ! 1/day
      real(r8), allocatable :: ZooER(:)              ! 1/day
      real(r8), allocatable :: ZooGR(:)              ! 1/day
      real(r8), allocatable :: ZooMR(:)              ! 1/day
#  endif
#endif
      real(r8), allocatable :: SDeAR(:)              ! 1/day
      real(r8), allocatable :: SDeBR(:)              ! 1/day
      real(r8), allocatable :: SDeRRN(:)             ! 1/day
      real(r8), allocatable :: SDeRRC(:)             ! 1/day
      real(r8), allocatable :: RDeRRN(:)             ! 1/day
      real(r8), allocatable :: RDeRRC(:)             ! 1/day
#if defined BIO_2P2Z || defined BIO_2P3Z
      real(r8), allocatable :: PsVp0(:)              ! nondimensional
      real(r8), allocatable :: PlVp0(:)              ! nondimensional
      real(r8), allocatable :: K_NO3_Ps(:)           ! m3/mmol_N
      real(r8), allocatable :: K_NH4_Ps(:)           ! m3/mmol_N
      real(r8), allocatable :: K_PO4_Ps(:)           ! m3/mmol_P
      real(r8), allocatable :: K_NO3_Pl(:)           ! m3/mmol_N
      real(r8), allocatable :: K_NH4_Pl(:)           ! m3/mmol_N
      real(r8), allocatable :: K_PO4_Pl(:)           ! m3/mmol_P
      real(r8), allocatable :: PhIS_Ps(:)            ! 1/(Watts m-2 day)
      real(r8), allocatable :: PhIS_Pl(:)            ! 1/(Watts m-2 day)
      real(r8), allocatable :: Chl2CPs_m(:)          ! mg_Chl/mg_C
      real(r8), allocatable :: Chl2CPl_m(:)          ! mg_Chl/mg_C
      real(r8), allocatable :: K_ZsPs(:)              ! (mmol_N/m3)^2
#  ifdef BIO_2P2Z
      real(r8), allocatable :: K_ZsPl(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: K_ZsDs(:)              ! (mmol_N/m3)^2
#  endif
      real(r8), allocatable :: ZooSAE_N(:)            ! nondimensional
      real(r8), allocatable :: K_ZlPs(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: K_ZlPl(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: K_ZlZs(:)              ! (mmol_N/m3)^2
#  ifdef BIO_2P2Z
      real(r8), allocatable :: K_ZlDs(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: ZooS_GrInPl(:)         ! nondimensional
      real(r8), allocatable :: ZooS_GrInDs(:)         ! nondimensional
      real(r8), allocatable :: ZooL_GrInPs(:)         ! nondimensional
      real(r8), allocatable :: ZooL_GrInDs(:)         ! nondimensional
#  endif
      real(r8), allocatable :: ZooLAE_N(:)            ! nondimensional
#  ifdef BIO_2P3Z
      real(r8), allocatable :: K_ZpPl(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: K_ZpZs(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: K_ZpZl(:)              ! (mmol_N/m3)^2      
      real(r8), allocatable :: ZooPAE_N(:)            ! nondimensional
      real(r8), allocatable :: ZooP_GrInPl(:)         ! nondimensional
      real(r8), allocatable :: ZooP_GrInZs(:)         ! nondimensional
#  endif
#else
      real(r8), allocatable :: Vp0(:)                ! nondimensional
      real(r8), allocatable :: K_NO3(:)              ! m3/mmol_N
      real(r8), allocatable :: K_NH4(:)              ! m3/mmol_N
      real(r8), allocatable :: K_PO4(:)              ! m3/mmol_P
      real(r8), allocatable :: PhyIS(:)              ! 1/(Watts m-2 day)
      real(r8), allocatable :: Chl2C_m(:)            ! mg_Chl/mg_C
      real(r8), allocatable :: K_Phy(:)              ! (mmol_N/m3)^2
      real(r8), allocatable :: ZooAE_N(:)            ! nondimensional
#endif
      real(r8), allocatable :: wLDet(:)              ! m/day
#if defined BIO_2P3Z || defined BIO_2P2Z      
      real(r8), allocatable :: wPhyS(:)              ! m/day
      real(r8), allocatable :: wPhyL(:)              ! m/day
#else      
      real(r8), allocatable :: wPhy(:)               ! m/day
#endif      
      real(r8), allocatable :: wSDet(:)              ! m/day
      real(r8), allocatable :: ZooCN(:)              ! mol_C/mol_N
      real(r8), allocatable :: ZooMin(:)             ! mmol_N/m3
      real(r8), allocatable :: pCO2air(:)            ! ppmv
#ifdef OAE
      real(r8), allocatable :: OAE_Flux(:)           ! (mol_C/m3) m/day
      real(r8), allocatable :: OAE_Tmin(:)           ! days
      real(r8), allocatable :: OAE_Tmax(:)           ! days
      real(r8), allocatable :: OAE_iloc(:)           ! i-location
      real(r8), allocatable :: OAE_jloc(:)           ! j-location
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
#if defined BIO_2P3Z
# ifdef CARBON
#  ifdef OXYGEN
#   if defined PO4 && defined RIVER_DON
      NBT=19
#   elif defined RIVER_DON && !defined PO4
      NBT=18
#   elif defined PO4 && !defined RIVER_DON
      NBT=17
#   else
      NBT=16
#   endif
#  else
#   if defined PO4 && defined RIVER_DON
      NBT=18
#   elif defined RIVER_DON && !defined PO4
      NBT=17
#   elif defined PO4 && !defined RIVER_DON
      NBT=16
#   else
      NBT=15
#   endif
#  endif
# else
#  ifdef OXYGEN
#   if defined PO4 && defined RIVER_DON
      NBT=14
#   elif defined PO4 || defined RIVER_DON
      NBT=13
#   else
      NBT=12
#   endif
#  else
#   if defined PO4 && defined RIVER_DON
      NBT=13
#   elif defined PO4 || defined RIVER_DON
      NBT=12
#   else
      NBT=11
#   endif
#  endif
# endif
#elif defined BIO_2P2Z
# ifdef CARBON
#  ifdef OXYGEN
#   if defined PO4 && defined RIVER_DON
      NBT=18
#   elif defined RIVER_DON && !defined PO4
      NBT=17
#   elif defined PO4 && !defined RIVER_DON
      NBT=16
#   else
      NBT=15
#   endif
#  else
#   if defined PO4 && defined RIVER_DON
      NBT=17
#   elif defined RIVER_DON && !defined PO4
      NBT=16
#   elif defined PO4 && !defined RIVER_DON
      NBT=15
#   else
      NBT=14
#   endif
#  endif
# else
#  ifdef OXYGEN
#   if defined PO4 && defined RIVER_DON
      NBT=13
#   elif defined PO4 || defined RIVER_DON
      NBT=12
#   else
      NBT=11
#   endif
#  else
#   if defined PO4 && defined RIVER_DON
      NBT=12
#   elif defined PO4 || defined RIVER_DON
      NBT=11
#   else
      NBT=10
#   endif
#  endif
# endif
#else
# ifdef CARBON
#  ifdef OXYGEN
#   if defined PO4 && defined RIVER_DON
      NBT=15
#   elif defined RIVER_DON && !defined PO4
      NBT=14
#   elif defined PO4 && !defined RIVER_DON
      NBT=13
#   else
      NBT=12
#   endif
#  else
#   if defined PO4 && defined RIVER_DON
      NBT=14
#   elif defined RIVER_DON && !defined PO4
      NBT=13
#   elif defined PO4 && !defined RIVER_DON
      NBT=12
#   else
      NBT=11
#   endif
#  endif
# else
#  ifdef OXYGEN
#   if defined PO4 && defined RIVER_DON
      NBT=10
#   elif defined PO4 || defined RIVER_DON
      NBT=9
#   else
      NBT=8
#   endif
#  else
#   if defined PO4 && defined RIVER_DON
      NBT=9
#   elif defined PO4 || defined RIVER_DON
      NBT=8
#   else
      NBT=7
#   endif
#  endif
# endif
#endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
!  Set sources and sinks biology diagnostic parameters.
!-----------------------------------------------------------------------
!
!  Set number of diagnostics terms.
!
      NDbio3d=3
      NDbio2d=0
# ifdef DENITRIFICATION
      NDbio2d=NDbio2d+1
# endif
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
# ifdef DENITRIFICATION
      iDNIT=ic+1
      ic=ic+1
# endif
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

      IF (.not.allocated(AttSW)) THEN
        allocate ( AttSW(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(AttChl)) THEN
        allocate ( AttChl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ChlMin)) THEN
        allocate ( ChlMin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(CoagR)) THEN
        allocate ( CoagR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(D_p5NH4)) THEN
        allocate ( D_p5NH4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(I_thNH4)) THEN
        allocate ( I_thNH4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(LDeRRN)) THEN
        allocate ( LDeRRN(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(LDeRRC)) THEN
        allocate ( LDeRRC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PARfrac)) THEN
        allocate ( PARfrac(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PhyCN)) THEN
        allocate ( PhyCN(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(R_P2N)) THEN
        allocate ( R_P2N(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PhyIP)) THEN
        allocate ( PhyIP(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PhyMin)) THEN
        allocate ( PhyMin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(SDeAR)) THEN
        allocate ( SDeAR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(SDeBR)) THEN
        allocate ( SDeBR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(SDeRRN)) THEN
        allocate ( SDeRRN(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(SDeRRC)) THEN
        allocate ( SDeRRC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(RDeRRN)) THEN
        allocate ( RDeRRN(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(RDeRRC)) THEN
        allocate ( RDeRRC(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(NitriR)) THEN
        allocate ( NitriR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
#if defined BIO_2P2Z || defined BIO_2P3Z

      IF (.not.allocated(PsVp0)) THEN
        allocate ( PsVp0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PlVp0)) THEN
        allocate ( PlVp0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_NO3_Ps)) THEN
        allocate ( K_NO3_Ps(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_NH4_Ps)) THEN
        allocate ( K_NH4_Ps(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_PO4_Ps)) THEN
        allocate ( K_PO4_Ps(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_NO3_Pl)) THEN
        allocate ( K_NO3_Pl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_NH4_Pl)) THEN
        allocate ( K_NH4_Pl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_PO4_Pl)) THEN
        allocate ( K_PO4_Pl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PhIS_Ps)) THEN
        allocate ( PhIS_Ps(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PhIS_Pl)) THEN
        allocate ( PhIS_Pl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(Chl2CPs_m)) THEN
        allocate ( Chl2CPs_m(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(Chl2CPl_m)) THEN
        allocate ( Chl2CPl_m(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_ZsPs)) THEN
        allocate ( K_ZsPs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#  ifdef BIO_2P2Z
      IF (.not.allocated(K_ZsPl)) THEN
        allocate ( K_ZsPl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_ZsDs)) THEN
        allocate ( K_ZsDs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#  endif
      IF (.not.allocated(ZooSAE_N)) THEN
        allocate ( ZooSAE_N(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_ZlPs)) THEN
        allocate ( K_ZlPs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_ZlPl)) THEN
        allocate ( K_ZlPl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_ZlZs)) THEN
        allocate ( K_ZlZs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#  ifdef BIO_2P2Z
      IF (.not.allocated(K_ZlDs)) THEN
        allocate ( K_ZlDs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooS_GrInPl)) THEN
        allocate ( ZooS_GrInPl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooS_GrInDs)) THEN
        allocate ( ZooS_GrInDs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooL_GrInPs)) THEN
        allocate ( ZooL_GrInPs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooL_GrInDs)) THEN
        allocate ( ZooL_GrInDs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#  endif
      IF (.not.allocated(ZooLAE_N)) THEN
        allocate ( ZooLAE_N(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
#  ifdef BIO_2P3Z
      IF (.not.allocated(K_ZpPl)) THEN
        allocate ( K_ZpPl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_ZpZs)) THEN
        allocate ( K_ZpZs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_ZpZl)) THEN
        allocate ( K_ZpZl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPAE_N)) THEN
        allocate ( ZooPAE_N(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooP_GrInPl)) THEN
        allocate ( ZooP_GrInPl(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooP_GrInZs)) THEN
        allocate ( ZooP_GrInZs(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#  endif
#else
      IF (.not.allocated(Vp0)) THEN
        allocate ( Vp0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_NO3)) THEN
        allocate ( K_NO3(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_NH4)) THEN
        allocate ( K_NH4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_PO4)) THEN
        allocate ( K_PO4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PhyIS)) THEN
        allocate ( PhyIS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(Chl2C_m)) THEN
        allocate ( Chl2C_m(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(K_Phy)) THEN
        allocate ( K_Phy(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooAE_N)) THEN
        allocate ( ZooAE_N(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#endif
#ifdef TEMP_RATES
#   if defined BIO_2P2Z || defined BIO_2P3Z
      IF (.not.allocated(PsMR0)) THEN
        allocate ( PsMR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PlMR0)) THEN
        allocate ( PlMR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSBM0)) THEN
        allocate ( ZooSBM0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSER0)) THEN
        allocate ( ZooSER0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSPsGR0)) THEN
        allocate ( ZooSPsGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     ifdef BIO_2P2Z
      IF (.not.allocated(ZooSPlGR0)) THEN
        allocate ( ZooSPlGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSDsGR0)) THEN
        allocate ( ZooSDsGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     endif
      IF (.not.allocated(ZooSMR0)) THEN
        allocate ( ZooSMR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLBM0)) THEN
        allocate ( ZooLBM0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLER0)) THEN
        allocate ( ZooLER0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLPsGR0)) THEN
        allocate ( ZooLPsGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLPlGR0)) THEN
        allocate ( ZooLPlGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLZsGR0)) THEN
        allocate ( ZooLZsGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     ifdef BIO_2P2Z
      IF (.not.allocated(ZooLDsGR0)) THEN
        allocate ( ZooLDsGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     endif
      IF (.not.allocated(ZooLMR0)) THEN
        allocate ( ZooLMR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
#     ifdef BIO_2P3Z
      IF (.not.allocated(ZooPBM0)) THEN
        allocate ( ZooPBM0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPER0)) THEN
        allocate ( ZooPER0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPPlGR0)) THEN
        allocate ( ZooPPlGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPZsGR0)) THEN
        allocate ( ZooPZsGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPZlGR0)) THEN
        allocate ( ZooPZlGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPMR0)) THEN
        allocate ( ZooPMR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     endif
#   else
      IF (.not.allocated(PhyMR0)) THEN
        allocate ( PhyMR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooBM0)) THEN
        allocate ( ZooBM0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooER0)) THEN
        allocate ( ZooER0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooGR0)) THEN
        allocate ( ZooGR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooMR0)) THEN
        allocate ( ZooMR0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#   endif
#else
#   if defined BIO_2P2Z || defined BIO_2P3Z
      IF (.not.allocated(PsMR)) THEN
        allocate ( PsMR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(PlMR)) THEN
        allocate ( PlMR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSBM)) THEN
        allocate ( ZooSBM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSER)) THEN
        allocate ( ZooSER(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSPsGR)) THEN
        allocate ( ZooSPsGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     ifdef BIO_2P2Z
      IF (.not.allocated(ZooSPlGR)) THEN
        allocate ( ZooSPlGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooSDsGR)) THEN
        allocate ( ZooSDsGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     endif
      IF (.not.allocated(ZooSMR)) THEN
        allocate ( ZooSMR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLBM)) THEN
        allocate ( ZooLBM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLER)) THEN
        allocate ( ZooLER(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLPsGR)) THEN
        allocate ( ZooLPsGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLPlGR)) THEN
        allocate ( ZooLPlGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooLZsGR)) THEN
        allocate ( ZooLZsGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     ifdef BIO_2P2Z
      IF (.not.allocated(ZooLDsGR)) THEN
        allocate ( ZooLDsGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     endif
      IF (.not.allocated(ZooLMR)) THEN
        allocate ( ZooLMR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     ifdef BIO_2P3Z
      IF (.not.allocated(ZooPBM)) THEN
        allocate ( ZooPBM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPER)) THEN
        allocate ( ZooPER(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPPlGR)) THEN
        allocate ( ZooPPlGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPZsGR)) THEN
        allocate ( ZooPZsGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPZlGR)) THEN
        allocate ( ZooPZlGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooPMR)) THEN
        allocate ( ZooPMR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#     endif
#   else
      IF (.not.allocated(PhyMR)) THEN
        allocate ( PhyMR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooBM)) THEN
        allocate ( ZooBM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooER)) THEN
        allocate ( ZooER(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooGR)) THEN
        allocate ( ZooGR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooMR)) THEN
        allocate ( ZooMR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

#   endif
#endif
      IF (.not.allocated(wLDet)) THEN
        allocate ( wLDet(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# if defined BIO_2P2Z || defined BIO_2P3Z
      IF (.not.allocated(wPhyS)) THEN
        allocate ( wPhyS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPhyL)) THEN
        allocate ( wPhyL(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# else
      IF (.not.allocated(wPhy)) THEN
        allocate ( wPhy(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# endif
      IF (.not.allocated(wSDet)) THEN
        allocate ( wSDet(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooCN)) THEN
        allocate ( ZooCN(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(ZooMin)) THEN
        allocate ( ZooMin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(pCO2air)) THEN
        allocate ( pCO2air(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
#ifdef OAE

      IF (.not.allocated(OAE_Flux)) THEN
        allocate ( OAE_Flux(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(OAE_Tmin)) THEN
        allocate ( OAE_Tmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(OAE_Tmax)) THEN
        allocate ( OAE_Tmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(OAE_iloc)) THEN
        allocate ( OAE_iloc(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF

      IF (.not.allocated(OAE_jloc)) THEN
        allocate ( OAE_jloc(Ngrids) )
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

      IF (.not.allocated(iDbio3)) THEN
        allocate ( iDbio3(NDbio3d) )
        Dmem(1)=Dmem(1)+REAL(NDbio3d,r8)
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
      iNO3_=ic+1
      iNH4_=ic+2
#if defined BIO_2P3Z
      iChlS=ic+3
      iChlL=ic+4
      iSPhy=ic+5
      iPhyL=ic+6
      iZooS=ic+7
      iZooL=ic+8
      iZooP=ic+9
      iLDeN=ic+10
      iSDeN=ic+11
      ic=ic+11
#elif defined BIO_2P2Z
      iChlS=ic+3
      iChlL=ic+4
      iSPhy=ic+5
      iPhyL=ic+6
      iZooS=ic+7
      iZooL=ic+8
      iLDeN=ic+9
      iSDeN=ic+10
      ic=ic+10
#else
      iChlo=ic+3
      iPhyt=ic+4
      iZoop=ic+5
      iLDeN=ic+6
      iSDeN=ic+7
      ic=ic+7
#endif
#ifdef RIVER_DON
      iRDeN=ic+1
      ic=ic+1
#endif
#ifdef PO4
      iPO4_=ic+1
      ic=ic+1
#endif
#ifdef CARBON
      iLDeC=ic+1
      iSDeC=ic+2
      iTIC_=ic+3
      iTAlk=ic+4
      ic=ic+4
# ifdef RIVER_DON
      iRDeC=ic+1
      ic=ic+1
# endif
#endif
#ifdef OXYGEN
      iOxyg=ic+1
      ic=ic+1
#endif

      RETURN
      END SUBROUTINE initialize_biology
