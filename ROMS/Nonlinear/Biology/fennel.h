      SUBROUTINE biology (ng,tile)
!
!svn $Id$
!***********************************************************************
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.txt                               Katja Fennel   !
!****************************************** Alexander F. Shchepetkin ***
!                                                                      !
!  This routine computes the  biological sources and sinks for the     !
!  Fennel et al. (2006) ecosystem model. Then, it adds those terms     !
!  to the global biological fields.                                    !
!                                                                      !
!  This model is loosely based on the model by Fasham et al. (1990)    !
!  but it differs in many respects.  The detailed equations of the     !
!  nitrogen cycling component  are given in  Fennel et al. (2006).     !
!  Nitrogen is the  fundamental elemental  currency in this model.     !
!  This model was adapted from a code written originally  by  John     !
!  Moisan and Emanule DiLorenzo.                                       !
!                                                                      !
!  It is recommended to activate always the  "BIO_SEDIMENT" option     !
!  to ensure conservation of mass by converting the organic matter     !
!  that is sinking out of the bottom most grid cell into inorganic     !
!  nutrients (i.e.,  instantanaous remineralization  at the water-     !
!  sediment interface). Additionally, the "DENITRIFICATION" option     !
!  can be activated.  Hence, a fraction of the instantenous bottom     !
!  remineralization is  assumed to  occur  through  the  anearobic     !
!  (denitrification)  pathway  and  thus  lost  from the  pool  of     !
!  biologically availalbe fixed nitrogen. See Fennel et al. (2006)     !
!  for details.                                                        !
!                                                                      !
!  Additional  options can be  activated to  enable  simulation of     !
!  inorganic carbon and dissolved oxygen.  Accounting of inorganic     !
!  carbon is activated by the "CARBON" option,  and results in two     !
!  additional  biological  tracer  variables:  DIC and alkalinity.     !
!  See Fennel et al. (2008) for details.                               !
!                                                                      !
!  If the "pCO2_RZ" options is activated, in addition to "CARBON",     !
!  the carbonate system  routines by Zeebe and Wolf-Gladrow (2001)     !
!  are used,  while the  OCMIP  standard routines are the default.     !
!  There are two different ways of treating alkalinity.  It can be     !
!  treated diagnostically (default),  in this case alkalinity acts     !
!  like a passive tracer  that is  not affected  by changes in the     !
!  concentration of  nitrate or ammonium.  However,  if the option     !
!  "TALK_NONCONSERV" is used,  the alkalinity  will be affected by     !
!  sources and sinks in nitrate. See Fennel et al. (2008) for more     !
!  details.                                                            !
!                                                                      !
!  If the "OXYGEN" option is activated,  one additional biological     !
!  tracer variable for dissolved oxygen. "OXYGEN" can be activated     !
!  independently of the  "CARBON"  option. If "OCMIP_OXYGEN_SC" is     !
!  used, in addition to "OXYGEN",  the Schmidt number of oxygen in     !
!  seawater will be  computed  using the  formulation  proposed by     !
!  Keeling et al. (1998, Global Biogeochem. Cycles,  12, 141-163).     !
!  Otherwise,  the Wanninkhof (1992)  formula will be used. See        !
!  Fennel et al. (2013) for more details.                              !
!                                                                      !
!***********************************************************************
!  UPDATE Sept 2020                                                    !
!                                                                      !
!  In this  version  additional  tracers,  additional   alkalinity     !
!  fluxes, and an updated parameterization of air-sea O2  and  CO2     !
!  fluxes are added as described in Laurent et al. (2017).             !
!                                                                      !
!  If "PO4" is activated,   one  additional biological tracer   is     !
!  added  representing phosphate.  With this option  phytoplankton     !
!  growth can be limited by either  nitrogen and phosphorus.  This     !
!  option was introduced in Laurent et al. (2012).                     !
!                                                                      !
!  If "RIVER_DON"  is activated,  an additional  biological tracer     !
!  (or 2 if "CARBON" is defined) is added representing non-sinking     !
!  dissolved organic matter from rivers as  described in Yu et al.     !
!  (2015).                                                             !
!                                                                      !
!  If the "RW14_OXYGEN_SC" and/or  "RW14_CO2_SC" options are used,     !
!  the model will use Wanninkhof (2014) air-sea flux parameteri-       !
!  zation.   With the  "TALK_NONCONSERV"  option,   alkalinity  is     !
!  affected by biological fluxes   as described in  Laurent et al.     !
!  (2017).                                                             !
!                                                                      !
!  With the  "PCO2AIR_DATA"  option,   atmospheric pCO2  uses  the     !
!  climatology of  Laurent et al. (2017).   The  "PCO2AIR_MAUNALOA"    !
!  option provides an alternative time-dependent atmospheric  pCO2     !
!  evolution that matches the observations from the Mauna Loa          !
!  observatory (1958-2020). If none of the 2 options are defined,      !
!  atmospheric pCO2 is constant.                                       !
!                                                                      !
!                                                                      !
!***********************************************************************
!  UPDATE June 2022                                                    !
!                                                                      !
!  Two options were added to use 2-phytoplanktons 2-zooplanktons       !
!  (2P2Z) and 2-phytoplanktons 3-zooplanktons (2P3Z) model types.      !
!  The options to activate are either "BIO_2P2Z" or "BIO_2P3Z".        !            
!  The 2P2Z model is described in Laurent et al. (2021).               !
!                                                                      !
!  An option was also added to have temperature dependent rates.       !
!  Additional option: "Q10_2" with also "MU_T_BISSINGER". Without      !
!  the Q10, additional options are "MU_T_BISSINGER" and                !
!  "MU_T_BISSINGER_43DIATOMS"                                          !
!                                                                      !
!  With the option "DETN2C" N and C detritus have the same remi-       !
!  neralization rate.                                                  !
!                                                                      !
!***********************************************************************

!  References:                                                         !
!                                                                      !
!    Fennel, K., Wilkin, J., Levin, J., Moisan, J., O^Reilly, J.,      !
!      Haidvogel, D., 2006: Nitrogen cycling in the Mid Atlantic       !
!      Bight and implications for the North Atlantic nitrogen          !
!      budget: Results from a three-dimensional model.  Global         !
!      Biogeochemical Cycles 20, GB3007, doi:10.1029/2005GB002456.     !
!                                                                      !
!    Fennel, K., Wilkin, J., Previdi, M., Najjar, R. 2008:             !
!      Denitrification effects on air-sea CO2 flux in the coastal      !
!      ocean: Simulations for the Northwest North Atlantic.            !
!      Geophys. Res. Letters 35, L24608, doi:10.1029/2008GL036147.     !
!                                                                      !
!    Fennel, K., Hu, J., Laurent, A., Marta-Almeida, M., Hetland, R.   !
!      2013: Sensitivity of Hypoxia Predictions for the Northern Gulf  !
!      of Mexico to Sediment Oxygen Consumption and Model Nesting. J.  !
!      Geophys. Res. Ocean 118 (2), 990-1002, doi:10.1002/jgrc.20077.  !
!                                                                      !
!    Laurent, A., Fennel, K., Hu, J., Hetland, R. 2012: Simulating     !
!      the Effects of Phosphorus Limitation in the Mississippi and     !
!      Atchafalaya River Plumes. Biogeosciences, 9 (11), 4707-4723,    !
!      doi:10.5194/bg-9-4707-2012.                                     !
!                                                                      !
!    Laurent, A., Fennel, K., Kuhn, A. 2021: An observation-based      !
!      evaluation and ranking of historical Earth system model         !
!      simulations in the northwest North Atlantic Ocean.              !
!      Biogeosciences, 18 (5), 1803–1822, doi:10.5194/bg-18-1803-2021. ! 
!                                                                      !
!    Yu, L., Fennel, K., Laurent, A., Murrell, M. C., Lehrter, J. C.   !
!      2015: Numerical Analysis of the Primary Processes Controlling   !
!      Oxygen Dynamics on the Louisiana Shelf. Biogeosciences, 12 (7), !
!      2063-2076, doi:10.5194/bg-12-2063-2015.                         !
!                                                                      !
!    Wanninkhof, R. 2014: Relationship between Wind Speed and Gas      !
!      Exchange over the Ocean Revisited. Limnol. Oceanogr. Methods    !
!      12 (6), 351-362, doi:10.4319/lom.2014.12.351.                   !
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
#ifdef HRM_SOD
     &                   GRID(ng) % lonr,                               &
     &                   GRID(ng) % latr,                               &
#endif
     &                   FORCES(ng) % srflx,                            &
#if defined CARBON || defined OXYGEN
# ifdef BULK_FLUXES
     &                   FORCES(ng) % Uwind,                            &
     &                   FORCES(ng) % Vwind,                            &
# else
     &                   FORCES(ng) % sustr,                            &
     &                   FORCES(ng) % svstr,                            &
# endif
#endif
#ifdef CARBON
     &                   OCEAN(ng) % pH,                                &
#endif
#ifdef DIAGNOSTICS_BIO
     &                   DIAGS(ng) % DiaBio2d,                          &
     &                   DIAGS(ng) % DiaBio3d,                          &
# ifdef FLOAT_PROFILE
                         DIAGS(ng) % DiaFlt2d,                          &
                         DIAGS(ng) % DiaFlt3d,                          &
# endif
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
#ifdef HRM_SOD
     &                         lonr, latr,                              &
#endif
#if defined CARBON || defined OXYGEN
# ifdef BULK_FLUXES
     &                         Uwind, Vwind,                            &
# else
     &                         sustr, svstr,                            &
# endif
#endif
#ifdef CARBON
     &                         pH,                                      &
#endif
#ifdef DIAGNOSTICS_BIO
     &                         DiaBio2d, DiaBio3d,                      &
# ifdef FLOAT_PROFILE
                               DiaFlt2d, DiaFlt3d,                      &
# endif
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
#ifdef HRM_SOD
      real(r8), intent(in) :: lonr(LBi:,LBj:)
      real(r8), intent(in) :: latr(LBi:,LBj:)
#endif
# if defined CARBON || defined OXYGEN
#  ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:,LBj:)
      real(r8), intent(in) :: Vwind(LBi:,LBj:)
#  else
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
#  endif
# endif
# ifdef CARBON
      real(r8), intent(inout) :: pH(LBi:,LBj:)
# endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaBio3d(LBi:,LBj:,:,:)
#  ifdef FLOAT_PROFILE
      real(r8), intent(inout) :: DiaFlt2d(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaFlt3d(LBi:,LBj:,:,:)      
#  endif
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
#ifdef HRM_SOD
      real(r8), intent(in) :: lonr(LBi:,LBj:)
      real(r8), intent(in) :: latr(LBi:,LBj:)
#endif
# if defined CARBON || defined OXYGEN
#  ifdef BULK_FLUXES
      real(r8), intent(in) :: Uwind(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Vwind(LBi:UBi,LBj:UBj)
#  else
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
#  endif
# endif
# ifdef CARBON
      real(r8), intent(inout) :: pH(LBi:UBi,LBj:UBj)
# endif
# ifdef DIAGNOSTICS_BIO
      real(r8), intent(inout) :: DiaBio2d(LBi:UBi,LBj:UBj,NDbio2d)
      real(r8), intent(inout) :: DiaBio3d(LBi:UBi,LBj:UBj,UBk,NDbio3d)
#  ifdef FLOAT_PROFILE
      real(r8), intent(inout) :: DiaFlt2d(LBi:UBi,LBj:UBj,NDbio2d)
      real(r8), intent(inout) :: DiaFlt3d(LBi:UBi,LBj:UBj,UBk,NDbio3d)
#  endif
# endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,UBk,3,UBt)
#endif
!
!  Local variable declarations.
!
#if defined BIO_2P2Z || defined BIO_2P3Z
# if defined CARBON && !defined DETN2C
      integer, parameter :: Nsink = 8
# else
      integer, parameter :: Nsink = 6
# endif
#else
# if defined CARBON && !defined DETN2C
      integer, parameter :: Nsink = 6
# else
      integer, parameter :: Nsink = 4
# endif
#endif

      integer :: Iter, i, ibio, isink, itrc, ivar, j, k, ks

      integer, dimension(Nsink) :: idsink

      real(r8), parameter :: eps = 1.0e-20_r8

#if defined CARBON || defined OXYGEN
      real(r8) :: u10squ
#endif
#ifdef OXYGEN
# if defined OCMIP_OXYGEN_SC
!
! Alternative formulation for Schmidt number coefficients (Sc will be
! slightly smaller up to about 35C) using the formulation proposed by
! Keeling et al. (1998, Global Biogeochem. Cycles, 12, 141-163).
!
      real(r8), parameter :: A_O2 = 1638.0_r8
      real(r8), parameter :: B_O2 = 81.83_r8
      real(r8), parameter :: C_O2 = 1.483_r8
      real(r8), parameter :: D_O2 = 0.008004_r8
      real(r8), parameter :: E_O2 = 0.0_r8

# elif defined RW14_OXYGEN_SC
!
! Alternative formulation for Schmidt number coefficients using the
! formulation of Wanninkhof (2014, L and O Methods, 12,351-362).
!
      real(r8), parameter :: A_O2 = 1920.4_r8
      real(r8), parameter :: B_O2 = 135.6_r8
      real(r8), parameter :: C_O2 = 5.2122_r8
      real(r8), parameter :: D_O2 = 0.10939_r8
      real(r8), parameter :: E_O2 = 0.00093777_r8

# else
!
! Schmidt number coefficients using the formulation of
! Wanninkhof (1992).
!
      real(r8), parameter :: A_O2 = 1953.4_r8
      real(r8), parameter :: B_O2 = 128.0_r8
      real(r8), parameter :: C_O2 = 3.9918_r8
      real(r8), parameter :: D_O2 = 0.050091_r8
      real(r8), parameter :: E_O2 = 0.0_r8
#endif
      real(r8), parameter :: OA0 = 2.00907_r8       ! Oxygen
      real(r8), parameter :: OA1 = 3.22014_r8       ! saturation
      real(r8), parameter :: OA2 = 4.05010_r8       ! coefficients
      real(r8), parameter :: OA3 = 4.94457_r8
      real(r8), parameter :: OA4 =-0.256847_r8
      real(r8), parameter :: OA5 = 3.88767_r8
      real(r8), parameter :: OB0 =-0.00624523_r8
      real(r8), parameter :: OB1 =-0.00737614_r8
      real(r8), parameter :: OB2 =-0.0103410_r8
      real(r8), parameter :: OB3 =-0.00817083_r8
      real(r8), parameter :: OC0 =-0.000000488682_r8
      real(r8), parameter :: rOxNO3= 8.625_r8       ! 138/16
      real(r8), parameter :: rOxNH4= 6.625_r8       ! 106/16
      real(r8) :: l2mol = 1000.0_r8/22.3916_r8      ! liter to mol
#endif
#ifdef CARBON
      integer :: year
      integer, parameter :: DoNewton = 0            ! pCO2 solver

# if defined RW14_CO2_SC
      real(r8), parameter :: A_CO2 = 2116.8_r8      ! Schmidt number
      real(r8), parameter :: B_CO2 = 136.25_r8      ! transfer coeff
      real(r8), parameter :: C_CO2 = 4.7353_r8      ! according to
      real(r8), parameter :: D_CO2 = 0.092307_r8    ! Wanninkhof (2014)
      real(r8), parameter :: E_CO2 = 0.0007555_r8
# else
      real(r8), parameter :: A_CO2 = 2073.1_r8      ! Schmidt
      real(r8), parameter :: B_CO2 = 125.62_r8      ! number
      real(r8), parameter :: C_CO2 = 3.6276_r8      ! transfer
      real(r8), parameter :: D_CO2 = 0.043219_r8    ! coefficients
      real(r8), parameter :: E_CO2 = 0.0_r8
# endif

      real(r8), parameter :: A1 = -60.2409_r8       ! surface
      real(r8), parameter :: A2 = 93.4517_r8        ! CO2
      real(r8), parameter :: A3 = 23.3585_r8        ! solubility
      real(r8), parameter :: B1 = 0.023517_r8       ! coefficients
      real(r8), parameter :: B2 = -0.023656_r8
      real(r8), parameter :: B3 = 0.0047036_r8

      real(r8) :: pmonth                         ! months since Jan 1951
      real(r8) :: pCO2air_secular
      real(dp) :: yday

      real(r8), parameter :: pi2 = 6.2831853071796_r8

# if defined PCO2AIR_MAUNALOA
      real(r8) :: fyear                                   ! fractional year
      real(r8), parameter :: D0 = 0.338329060_r8
      real(r8), parameter :: D1 = 108.589607716_r8        ! coefficients
      real(r8), parameter :: D2 = 9.440999563_r8          ! to calculate
      real(r8), parameter :: D3 = 2.838323785_r8          ! secular trend in
      real(r8), parameter :: D4 = 0.922577614_r8          ! atmospheric pCO2
      real(r8), parameter :: D5 = 0.999565480_r8
      real(r8), parameter :: D6 = 0.782616227_r8
      real(r8), parameter :: D7 = -157.853151556_r8
      real(r8), parameter :: D8 = 33.919758714_r8
      real(r8), parameter :: D9 = 0.012976532_r8
      real(r8), parameter :: D10 = -50.053077642_r8
      real(r8), parameter :: D11 = 48569.391919959_r8
      real(r8), parameter :: D11 = 0.1665_r8
# elif defined PCO2AIR_SABLEISLAND
      real(r8) :: fyear
      real(r8), parameter :: D0 = 72134.344240476_r8       ! coefficients to calculate
      real(r8), parameter :: D1 = -73.611517390_r8         ! secular trend with a
      real(r8), parameter :: D2 = 0.018864984_r8           ! realistic exponential increase
      real(r8), parameter :: D3 = 5.477205011_r8           ! in atmospheric pCO2
      real(r8), parameter :: D4 = 0.645274882_r8           ! from Sable Island
      real(r8), parameter :: D5 = 1.128926486_r8           ! atm pCO2 data
# endif
#endif

#ifdef PARICE
      real(r8) :: icefac                                  ! GLORYS ice factor

#endif

      real(r8) :: Att, AttFac, ExpAtt, Itop, PAR
      real(r8) :: L_NH4, L_NO3, LTOT
#ifdef PO4
      real(r8), parameter :: MinVal = 1.0e-6_r8

      real(r8) :: L_PO4, LMIN, mu, cff6
#endif
      real(r8) :: dtdays, t_PPmax, inhNH4

      real(r8) :: cff, cff1, cff2, cff3, cff4, cff5
#ifdef RIVER_DON
      real(r8) :: cff7, cff8
#endif
      real(r8) :: fac1, fac2, fac3, fac4
      real(r8) :: cffL, cffR, cu, dltL, dltR

      real(r8) :: total_N

#if defined BIO_2P2Z || defined BIO_2P3Z
      real(r8) :: PsVp, PlVp, EppPs, EppPl
      real(r8) :: ChlPs2C, ChlPl2C
      real(r8) :: fac5, fac6, fac7
      real(r8) :: fac8, fac9, fac10, ppref
#else
      real(r8) :: Vp, Epp, Chl2C
#endif

#ifdef TEMP_RATES
# if defined BIO_2P2Z || defined BIO_2P3Z
      real(r8) :: PsMR, PlMR
      real(r8) :: ZooSBM, ZooSER, ZooSMR, ZooSPsGR
      real(r8) :: ZooLBM, ZooLER, ZooLMR, ZooLPsGR, ZooLPlGR, ZooLZsGR
      real(r8) :: ZooPBM, ZooPER, ZooPMR, ZooPPlGR, ZooPZsGR, ZooPZlGR
#  ifdef BIO_2P2Z
      real(r8) :: ZooSPlGR, ZooSDsGR, ZooLDsGR
#  endif
# else
      real(r8) :: PhyMR
      real(r8) :: ZooBM, ZooER, ZooMR, ZooGR
# endif
#endif

#ifdef DIAGNOSTICS_BIO
      real(r8) :: fiter
#endif

#ifdef OXYGEN
      real(r8) :: SchmidtN_Ox, O2satu, O2_Flux
      real(r8) :: TS, AA
#endif

#ifdef CARBON
      real(r8) :: C_Flux_RemineL, C_Flux_RemineS, C_Flux_RemineR
      real(r8) :: CO2_Flux, CO2_sol, SchmidtN, TempK
#endif

#if defined BIO_2P2Z || defined BIO_2P3Z
      real(r8) :: N_Flux_NewProd_Ps, N_Flux_RegProd_Ps
      real(r8) :: N_Flux_NewProd_Pl, N_Flux_RegProd_Pl
      real(r8) :: N_Flux_Pmortal_Ps, N_Flux_Pmortal_Pl

      real(r8) :: N_Flux_Assim_ZsPs
      real(r8) :: N_Flux_Assim_ZlPs, N_Flux_Assim_ZlPl, N_Flux_Assim_ZlZs
      real(r8) :: N_Flux_Assim_ZpPl, N_Flux_Assim_ZpZs, N_Flux_Assim_ZpZl
      real(r8) :: N_Flux_Egest_ZsPs
      real(r8) :: N_Flux_Egest_ZlPs, N_Flux_Egest_ZlPl, N_Flux_Egest_ZlZs
      real(r8) :: N_Flux_Egest_ZpPl, N_Flux_Egest_ZpZs, N_Flux_Egest_ZpZl

      real(r8) :: N_Flux_Assim_Zs, N_Flux_Assim_Zl, N_Flux_Assim_Zp
      real(r8) :: N_Flux_Egest_Zs, N_Flux_Egest_Zl, N_Flux_Egest_Zp

# ifdef BIO_2P2Z
      real(r8) :: N_Flux_Assim_ZsPl, N_Flux_Assim_ZlDs, N_Flux_Assim_ZsDs
      real(r8) :: N_Flux_Egest_ZsPl, N_Flux_Egest_ZlDs, N_Flux_Egest_ZsDs
      real(r8) :: switch_ZlDs
# endif
      real(r8) :: N_Flux_Zsexcret, N_Flux_Zlexcret, N_Flux_Zpexcret
      real(r8) :: N_Flux_Zsmetabo, N_Flux_Zlmetabo, N_Flux_Zpmetabo
      real(r8) :: N_Flux_Zsmortal, N_Flux_Zlmortal, N_Flux_Zpmortal
#endif

      real(r8) :: N_Flux_Assim
      real(r8) :: N_Flux_CoagD, N_Flux_CoagP
      real(r8) :: N_Flux_Egest
      real(r8) :: N_Flux_NewProd, N_Flux_RegProd
      real(r8) :: N_Flux_Nitrifi
      real(r8) :: N_Flux_Pmortal, N_Flux_Zmortal
      real(r8) :: N_Flux_RemineL, N_Flux_RemineS, N_Flux_RemineR
      real(r8) :: N_Flux_Zexcret, N_Flux_Zmetabo

#if defined HRM2_BBATN || defined H2_BBATN
      real(r8) :: AttBBChl

#endif
      real(r8), dimension(Nsink) :: Wbio

      integer, dimension(IminS:ImaxS,N(ng)) :: ksource

      real(r8), dimension(IminS:ImaxS) :: PARsur
#ifdef CARBON
      real(r8), dimension(IminS:ImaxS) :: pCO2
#endif

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

!-----------------------------------------------------------------------------------------
! Coefficients for temperature dependent rates [implemented by LB; modified by AK-2015]
! Temperature dependency of the form: parameter(ng)*Tcoef0*(Tcoef1**Bio(i,k,itemp))
!-----------------------------------------------------------------------------------------

#ifdef Q10_2
# ifdef MU_T_BISSINGER
!  Bissinger et al, 2008, L&O, 53(2): 487-493.
       real(r8), parameter :: Tcoef0 = 0.81_r8
# else
!  (Eppley, R.W.,1972, Fishery Bulletin, 70: 1063-1085; here 0.59=ln(2)*0.851).
!  Check value for Vp is 2.9124317 at 19.25 degC.
       real(r8), parameter :: Tcoef0 = 0.59_r8
# endif
!  Q10 = 2
       real(r8), parameter :: Tcoef1=1.0718_r8
#else
# ifdef MU_T_BISSINGER
#  ifdef MU_T_BISSINGER_43DIATOMS
!  Bissinger et al, 2008, L&O, 53(2): 487-493 for temp, using the formulation for the
!  subset with 43% Diatoms, as in Eppley 1972
       real(r8), parameter :: Tcoef1 = 1.053_r8
#  else
       real(r8), parameter :: Tcoef1 = 1.065_r8
#  endif
# else
!  (Eppley, R.W.,1972, Fishery Bulletin, 70: 1063-1085; growth rate
!  provided in doublings per day. Here 0.59=ln(2)*0.851, i.e., growth rate
!  per day is equal to ln(2)xdoubling per day).

       real(r8), parameter :: Tcoef0 = 0.59_r8
       real(r8), parameter :: Tcoef1 = 1.066_r8
# endif
#endif

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
# ifdef FLOAT_PROFILE
              DiaFlt2d(i,j,ivar)=0.0_r8
# endif
            END DO
          END DO
        END DO
        DO ivar=1,NDbio3d
          DO k=1,N(ng)
            DO j=Jstr,Jend
              DO i=Istr,Iend
                DiaBio3d(i,j,k,ivar)=0.0_r8
# ifdef FLOAT_PROFILE
                DiaFlt3d(i,j,k,ivar)=0.0_r8
# endif
              END DO
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
#if defined BIO_2P2Z || defined BIO_2P3Z
      idsink(1)=iSPhy
      idsink(2)=iPhyL
      idsink(3)=iChlS
      idsink(4)=iChlL
      idsink(5)=iSDeN
      idsink(6)=iLDeN
# ifdef CARBON
      idsink(7)=iSDeC
      idsink(8)=iLDeC
# endif
#else
      idsink(1)=iPhyt
      idsink(2)=iChlo
      idsink(3)=iSDeN
      idsink(4)=iLDeN
# ifdef CARBON
      idsink(5)=iSDeC
      idsink(6)=iLDeC
# endif
#endif
!
!  Set vertical sinking velocity vector in the same order as the
!  identification vector, IDSINK.
!
#if defined BIO_2P2Z || defined BIO_2P3Z
      Wbio(1)=wPhyS(ng)                ! small phytoplankton
      Wbio(2)=wPhyL(ng)                ! large phytoplankton
      Wbio(3)=wPhyS(ng)                ! small P chlorophyll
      Wbio(4)=wPhyL(ng)                ! large P chlorophyll
      Wbio(5)=wSDet(ng)               ! small Nitrogen-detritus
      Wbio(6)=wLDet(ng)               ! large Nitrogen-detritus
# if defined CARBON && !defined DETN2C
      Wbio(7)=wSDet(ng)               ! small Carbon-detritus
      Wbio(8)=wLDet(ng)               ! large Carbon-detritus
# endif
#else
      Wbio(1)=wPhy(ng)                ! phytoplankton
      Wbio(2)=wPhy(ng)                ! chlorophyll
      Wbio(3)=wSDet(ng)               ! small Nitrogen-detritus
      Wbio(4)=wLDet(ng)               ! large Nitrogen-detritus
# if defined CARBON && !defined DETN2C
      Wbio(5)=wSDet(ng)               ! small Carbon-detritus
      Wbio(6)=wLDet(ng)               ! large Carbon-detritus
# endif
#endif
#ifdef BIO_2P2Z
!
! Set switch for ZooL grazing on Ds
!
      IF (ZooL_GrInDs(ng).le.0.0_r8) THEN
       switch_ZlDs=0.0_r8
      ELSE
       switch_ZlDs=1.0_r8
      END IF
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
#ifdef CARBON
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio_old(i,k,iTIC_)=MIN(Bio_old(i,k,iTIC_),3000.0_r8)
            Bio_old(i,k,iTIC_)=MAX(Bio_old(i,k,iTIC_),400.0_r8)
            Bio(i,k,iTIC_)=Bio_old(i,k,iTIC_)
          END DO
        END DO
#endif
!
!  Extract potential temperature and salinity.
!
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio(i,k,itemp)=MIN(t(i,j,k,nstp,itemp),35.0_r8)
            Bio(i,k,isalt)=MAX(t(i,j,k,nstp,isalt), 0.0_r8)
          END DO
        END DO
!
!  Calculate surface Photosynthetically Available Radiation (PAR).  The
!  net shortwave radiation is scaled back to Watts/m2 and multiplied by
!  the fraction that is photosynthetically available, PARfrac.
!
        DO i=Istr,Iend
          PARsur(i)=PARfrac(ng)*srflx(i,j)*rho0*Cp
#ifdef PARICE
          icefac=0.00083921_r8*EXP(-4.19478017_r8*Bio(i,N(ng),itemp))
          PARsur(i)=(1.0_r8-MAX(MIN(icefac,1.0_r8),0.0_r8))*PARsur(i)
#endif
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
!
!-----------------------------------------------------------------------
!  Light-limited computations.
!-----------------------------------------------------------------------
!
!  Compute attenuation coefficient based on the concentration of
!  chlorophyll-a within each grid box.  Then, attenuate surface
!  photosynthetically available radiation (PARsur) down inot the
!  water column.  Thus, PAR at certain depth depends on the whole
!  distribution of chlorophyll-a above.
!  To compute rate of maximum primary productivity (t_PPmax), one needs
!  PAR somewhat in the middle of the gridbox, so that attenuation "Att"
!  corresponds to half of the grid box height, while PAR is multiplied
!  by it twice: once to get it in the middle of grid-box and once the
!  compute on the lower grid-box interface.
!
          DO i=Istr,Iend
            PAR=PARsur(i)
#if defined HRM2_BBATN
            IF ((i.le.68).and.(j.ge.248)) THEN
              AttFac=0.344_r8-AttSW(ng) ! Line below was AttBBChl = 0.0349_r8
              AttBBChl = AttChl(ng)     ! This is a placeholder for now
            ELSE
              AttFac=0.0_r8
              AttBBChl = AttChl(ng)
            END IF
#elif defined H2_BBATN
            IF ((i.ge.112).and.(i.le.164).and.(j.ge.297)) THEN
              AttFac=0.344_r8-AttSW(ng) ! Line below was AttBBChl = 0.0349_r8
              AttBBChl = AttChl(ng)     ! This is a placeholder for now
            ELSE
              AttFac=0.0_r8
              AttBBChl = AttChl(ng)
            END IF
#else
            AttFac=0.0_r8
#endif
            IF (PARsur(i).gt.0.0_r8) THEN
              DO k=N(ng),1,-1
!
!  Compute average light attenuation for each grid cell. To include
!  other attenuation contributions like suspended sediment or CDOM
!  modify AttFac.
!
                Att=(AttSW(ng)+                                         &
#if defined BIO_2P2Z || defined BIO_2P3Z
# if defined HRM2_BBATN || defined H2_BBATN
     &               (AttBBChl*(Bio(i,k,iChlS)+Bio(i,k,iChlL)))+        &
# else
     &               (AttChl(ng)*(Bio(i,k,iChlS)+Bio(i,k,iChlL)))+      &
# endif
#else
     &               AttChl(ng)*Bio(i,k,iChlo)+                         &
#endif
     &               AttFac)*                                           &
     &               (z_w(i,j,k)-z_w(i,j,k-1))
                ExpAtt=EXP(-Att)
                Itop=PAR
                PAR=Itop*(1.0_r8-ExpAtt)/Att    ! average at cell center
!
!  Compute Chlorophyll-a phytoplankton ratio, [mg Chla / (mg C)].
!
                cff=PhyCN(ng)*12.0_r8
#if defined BIO_2P2Z || defined BIO_2P3Z
                ChlPs2C=MIN(Bio(i,k,iChlS)/                            &
     &                     (Bio(i,k,iSPhy)*cff+eps),                   &
     &                     Chl2CPs_m(ng))
                ChlPl2C=MIN(Bio(i,k,iChlL)/                            &
     &                     (Bio(i,k,iPhyL)*cff+eps),                   &
     &                     Chl2CPl_m(ng))
#else
                Chl2C=MIN(Bio(i,k,iChlo)/(Bio(i,k,iPhyt)*cff+eps),     &
     &                    Chl2C_m(ng))
#endif
!
! Temperature and light dependent growth
!
#if defined BIO_2P2Z || defined BIO_2P3Z
! Small phytoplankton:
                fac1=PAR*PhIS_Ps(ng)
                PsVp=PsVp0(ng)*Tcoef0*(Tcoef1**Bio(i,k,itemp))
                EppPs=PsVp/SQRT(PsVp*PsVp+fac1*fac1)
                t_PPmax=EppPs*fac1
!
!  Nutrient-limitation terms (Parker 1993 Ecol Mod., 66, 113-120).
!
                cff1=Bio(i,k,iNH4_)*K_NH4_Ps(ng)
                cff2=Bio(i,k,iNO3_)*K_NO3_Ps(ng)
                inhNH4=1.0_r8/(1.0_r8+cff1)
                L_NH4=cff1/(1.0_r8+cff1)
                L_NO3=cff2*inhNH4/(1.0_r8+cff2)
                LTOT=L_NO3+L_NH4
# ifdef PO4
                cff3=Bio(i,k,iPO4_)*K_PO4_Ps(ng)
                L_PO4=cff3/(1.0_r8+cff3)
                LMIN=MIN(LTOT,L_PO4)
# endif
!
!  Nitrate and ammonium uptake by small Phytoplankton.
!
# ifdef PO4
                mu=dtdays*t_PPmax*LMIN
                cff4=mu*Bio(i,k,iSPhy)*L_NO3/                           &
     &               MAX(MinVal,LTOT)/MAX(MinVal,Bio(i,k,iNO3_))
                cff5=mu*Bio(i,k,iSPhy)*L_NH4/                           &
     &               MAX(MinVal,LTOT)/MAX(MinVal,Bio(i,k,iNH4_))
                cff6=R_P2N(ng)*mu*Bio(i,k,iSPhy)/                       &
     &               MAX(MinVal,Bio(i,k,iPO4_))
# else
                fac1=dtdays*t_PPmax
                cff4=fac1*K_NO3_Ps(ng)*inhNH4/(1.0_r8+cff2)*Bio(i,k,iPhyt)
                cff5=fac1*K_NH4_Ps(ng)/(1.0_r8+cff1)*Bio(i,k,iPhyt)
# endif
                Bio(i,k,iNO3_)=Bio(i,k,iNO3_)/(1.0_r8+cff4)
                Bio(i,k,iNH4_)=Bio(i,k,iNH4_)/(1.0_r8+cff5)
# ifdef PO4
                Bio(i,k,iPO4_)=Bio(i,k,iPO4_)/(1.0_r8+cff6)
# endif
                N_Flux_NewProd_Ps=Bio(i,k,iNO3_)*cff4
                N_Flux_RegProd_Ps=Bio(i,k,iNH4_)*cff5


                Bio(i,k,iSPhy)=Bio(i,k,iSPhy)+                           &
     &                         N_Flux_NewProd_Ps+N_Flux_RegProd_Ps
!
                Bio(i,k,iChlS)=Bio(i,k,iChlS)+                          &
# ifdef PO4
     &                         (dtdays*t_PPmax*t_PPmax*LMIN*LMIN*       &
# else
     &                         (dtdays*t_PPmax*t_PPmax*LTOT*LTOT*       &
# endif
     &                          Chl2CPs_m(ng)*Bio(i,k,iChlS))/          &
     &                         (PhIS_Ps(ng)*MAX(ChlPs2C,eps)*PAR+eps)
!
!
! Large phytoplankton:
                fac1=PAR*PhIS_Pl(ng)
                PlVp=PlVp0(ng)*Tcoef0*(Tcoef1**Bio(i,k,itemp))
                EppPl=PlVp/SQRT(PlVp*PlVp+fac1*fac1)
                t_PPmax=EppPl*fac1
!
!  Nutrient-limitation terms (Parker 1993 Ecol Mod., 66, 113-120).
!
                cff1=Bio(i,k,iNH4_)*K_NH4_Pl(ng)
                cff2=Bio(i,k,iNO3_)*K_NO3_Pl(ng)
                inhNH4=1.0_r8/(1.0_r8+cff1)
                L_NH4=cff1/(1.0_r8+cff1)
                L_NO3=cff2*inhNH4/(1.0_r8+cff2)
                LTOT=L_NO3+L_NH4
# ifdef PO4
                cff3=Bio(i,k,iPO4_)*K_PO4_Pl(ng)
                L_PO4=cff3/(1.0_r8+cff3)
                LMIN=MIN(LTOT,L_PO4)
# endif
!
!  Nitrate and ammonium uptake by large Phytoplankton.
!
# ifdef PO4
                mu=dtdays*t_PPmax*LMIN
                cff4=mu*Bio(i,k,iPhyL)*L_NO3/                           &
     &               MAX(MinVal,LTOT)/MAX(MinVal,Bio(i,k,iNO3_))
                cff5=mu*Bio(i,k,iPhyL)*L_NH4/                           &
     &               MAX(MinVal,LTOT)/MAX(MinVal,Bio(i,k,iNH4_))
                cff6=R_P2N(ng)*mu*Bio(i,k,iPhyL)/                       &
     &               MAX(MinVal,Bio(i,k,iPO4_))
# else
                fac1=dtdays*t_PPmax
                cff4=fac1*K_NO3_Pl(ng)*inhNH4/(1.0_r8+cff2)*Bio(i,k,iPhyL)
                cff5=fac1*K_NH4_Pl(ng)/(1.0_r8+cff1)*Bio(i,k,iPhyL)
# endif
                Bio(i,k,iNO3_)=Bio(i,k,iNO3_)/(1.0_r8+cff4)
                Bio(i,k,iNH4_)=Bio(i,k,iNH4_)/(1.0_r8+cff5)
# ifdef PO4
                Bio(i,k,iPO4_)=Bio(i,k,iPO4_)/(1.0_r8+cff6)
# endif
                N_Flux_NewProd_Pl=Bio(i,k,iNO3_)*cff4
                N_Flux_RegProd_Pl=Bio(i,k,iNH4_)*cff5
                Bio(i,k,iPhyL)=Bio(i,k,iPhyL)+                          &
     &                         N_Flux_NewProd_Pl+N_Flux_RegProd_Pl
!
                Bio(i,k,iChlL)=Bio(i,k,iChlL)+                          &
# ifdef PO4
     &                         (dtdays*t_PPmax*t_PPmax*LMIN*LMIN*       &
# else
     &                         (dtdays*t_PPmax*t_PPmax*LTOT*LTOT*       &
# endif
     &                          Chl2CPl_m(ng)*Bio(i,k,iChlL))/          &
     &                         (PhIS_Pl(ng)*MAX(ChlPl2C,eps)*PAR+eps)

                N_Flux_NewProd=N_Flux_NewProd_Ps+N_Flux_NewProd_Pl
                N_Flux_RegProd=N_Flux_RegProd_Ps+N_Flux_RegProd_Pl
#else
                fac1=PAR*PhyIS(ng)
                Vp=Vp0(ng)*Tcoef0*(Tcoef1**Bio(i,k,itemp))
                Epp=Vp/SQRT(Vp*Vp+fac1*fac1)
                t_PPmax=Epp*fac1
!
!  Nutrient-limitation terms (Parker 1993 Ecol Mod., 66, 113-120).
!
                cff1=Bio(i,k,iNH4_)*K_NH4(ng)
                cff2=Bio(i,k,iNO3_)*K_NO3(ng)
                inhNH4=1.0_r8/(1.0_r8+cff1)
                L_NH4=cff1/(1.0_r8+cff1)
                L_NO3=cff2*inhNH4/(1.0_r8+cff2)
                LTOT=L_NO3+L_NH4
# ifdef PO4
                cff3=Bio(i,k,iPO4_)*K_PO4(ng)
                L_PO4=cff3/(1.0_r8+cff3)
                LMIN=MIN(LTOT,L_PO4)
# endif
!
!  Nitrate and ammonium uptake by Phytoplankton.
!
# ifdef PO4
                mu=dtdays*t_PPmax*LMIN
                cff4=mu*Bio(i,k,iPhyt)*L_NO3/                           &
     &               MAX(MinVal,LTOT)/MAX(MinVal,Bio(i,k,iNO3_))
                cff5=mu*Bio(i,k,iPhyt)*L_NH4/                           &
     &               MAX(MinVal,LTOT)/MAX(MinVal,Bio(i,k,iNH4_))
                cff6=R_P2N(ng)*mu*Bio(i,k,iPhyt)/                       &
     &               MAX(MinVal,Bio(i,k,iPO4_))
# else
                fac1=dtdays*t_PPmax
                cff4=fac1*K_NO3(ng)*inhNH4/(1.0_r8+cff2)*Bio(i,k,iPhyt)
                cff5=fac1*K_NH4(ng)/(1.0_r8+cff1)*Bio(i,k,iPhyt)
# endif
                Bio(i,k,iNO3_)=Bio(i,k,iNO3_)/(1.0_r8+cff4)
                Bio(i,k,iNH4_)=Bio(i,k,iNH4_)/(1.0_r8+cff5)
# ifdef PO4
                Bio(i,k,iPO4_)=Bio(i,k,iPO4_)/(1.0_r8+cff6)
# endif
                N_Flux_NewProd=Bio(i,k,iNO3_)*cff4
                N_Flux_RegProd=Bio(i,k,iNH4_)*cff5
                Bio(i,k,iPhyt)=Bio(i,k,iPhyt)+                          &
     &                         N_Flux_NewProd+N_Flux_RegProd
!
                Bio(i,k,iChlo)=Bio(i,k,iChlo)+                          &
# ifdef PO4
     &                         (dtdays*t_PPmax*t_PPmax*LMIN*LMIN*       &
# else
     &                         (dtdays*t_PPmax*t_PPmax*LTOT*LTOT*       &
# endif
     &                          Chl2C_m(ng)*Bio(i,k,iChlo))/            &
     &                         (PhyIS(ng)*MAX(Chl2C,eps)*PAR+eps)
#endif
#ifdef DIAGNOSTICS_BIO
                DiaBio3d(i,j,k,iPPro)=DiaBio3d(i,j,k,iPPro)+            &
# ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
# endif
     &                                (N_Flux_NewProd+N_Flux_RegProd)*  &
     &                                fiter
                DiaBio3d(i,j,k,iNO3u)=DiaBio3d(i,j,k,iNO3u)+            &
# ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
# endif
     &                                N_Flux_NewProd*fiter
# ifdef FLOAT_PROFILE
                DiaFlt3d(i,j,k,iPPro)=(1/dtdays)*                       &
#  ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
#  endif
     &                                (N_Flux_NewProd+N_Flux_RegProd)
                DiaFlt3d(i,j,k,iNO3u)=(1/dtdays)*                       &
#  ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
#  endif
     &                                N_Flux_NewProd
# endif
#endif
#ifdef OXYGEN
                Bio(i,k,iOxyg)=Bio(i,k,iOxyg)+                          &
     &                         N_Flux_NewProd*rOxNO3+                   &
     &                         N_Flux_RegProd*rOxNH4
#endif
#ifdef CARBON
!
!  Total inorganic carbon (CO2) uptake during phytoplankton growth.
!
                cff1=PhyCN(ng)*(N_Flux_NewProd+N_Flux_RegProd)
                Bio(i,k,iTIC_)=Bio(i,k,iTIC_)-cff1
# ifdef TALK_NONCONSERV
!
!  Account for the uptake of NO3 on total alkalinity.
!
                Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+N_Flux_NewProd-           &
     &                         N_Flux_RegProd
# endif
#endif
!
! The Nitrification of NH4 ==> NO3 is thought to occur only in dark and
! only in aerobic water (see Olson, R. J., 1981, JMR: (39), 227-238.).
!
!         NH4+ + 3/2 O2  ==> NO2- + H2O;  via Nitrosomonas bacteria
!         NO2-  + 1/2 O2 ==> NO3-      ;  via Nitrobacter  bacteria
!
! Note that the entire process has a total loss of two moles of O2 per
! mole of NH4. If we were to resolve NO2 profiles, this is where we
! would change the code to split out the differential effects of the
! two different bacteria types. If OXYGEN is defined, nitrification is
! inhibited at low oxygen concentrations using a Michaelis-Menten term.
!
#ifdef OXYGEN
                fac2=MAX(Bio(i,k,iOxyg),0.0_r8)     ! O2 max
                fac3=MAX(fac2/(3.0_r8+fac2),0.0_r8) ! MM for O2 dependence
                fac1=dtdays*NitriR(ng)*fac3
#else
                fac1=dtdays*NitriR(ng)
#endif
                cff1=(PAR-I_thNH4(ng))/                                 &
     &               (D_p5NH4(ng)+PAR-2.0_r8*I_thNH4(ng))
                cff2=1.0_r8-MAX(0.0_r8,cff1)
                cff3=fac1*cff2
                Bio(i,k,iNH4_)=Bio(i,k,iNH4_)/(1.0_r8+cff3)
                N_Flux_Nitrifi=Bio(i,k,iNH4_)*cff3
                Bio(i,k,iNO3_)=Bio(i,k,iNO3_)+N_Flux_Nitrifi
#ifdef DIAGNOSTICS_BIO
                DiaBio3d(i,j,k,iNifx)=DiaBio3d(i,j,k,iNifx)+            &
# ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
# endif
     &                                N_Flux_Nitrifi*fiter
# ifdef FLOAT_PROFILE
                DiaFlt3d(i,j,k,iNifx)=(1/dtdays)*                       &
#  ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
#  endif
     &                                N_Flux_Nitrifi
# endif
#endif
#ifdef OXYGEN
                Bio(i,k,iOxyg)=Bio(i,k,iOxyg)-2.0_r8*N_Flux_Nitrifi
#endif
#if defined CARBON && defined TALK_NONCONSERV
                Bio(i,k,iTAlk)=Bio(i,k,iTAlk)-2.0_r8*N_Flux_Nitrifi
#endif
!
!  Light attenuation at the bottom of the grid cell. It is the starting
!  PAR value for the next (deeper) vertical grid cell.
!
                PAR=Itop*ExpAtt
              END DO
!
!  If PARsur=0, nitrification occurs at the maximum rate (NitriR).
!
            ELSE
              cff3=dtdays*NitriR(ng)
              DO k=N(ng),1,-1
                Bio(i,k,iNH4_)=Bio(i,k,iNH4_)/(1.0_r8+cff3)
                N_Flux_Nitrifi=Bio(i,k,iNH4_)*cff3
                Bio(i,k,iNO3_)=Bio(i,k,iNO3_)+N_Flux_Nitrifi
#ifdef DIAGNOSTICS_BIO
                DiaBio3d(i,j,k,iNifx)=DiaBio3d(i,j,k,iNifx)+            &
# ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
# endif
     &                                N_Flux_Nitrifi*fiter
# ifdef FLOAT_PROFILE
                DiaFlt3d(i,j,k,iNifx)=(1/dtdays)*                       &
#  ifdef WET_DRY
     &                                rmask_full(i,j)*                  &
#  endif
     &                                N_Flux_Nitrifi
# endif
#endif
#ifdef OXYGEN
                Bio(i,k,iOxyg)=Bio(i,k,iOxyg)-2.0_r8*N_Flux_Nitrifi
#endif
#if defined CARBON && defined TALK_NONCONSERV
                Bio(i,k,iTAlk)=Bio(i,k,iTAlk)-2.0_r8*N_Flux_Nitrifi
#endif
              END DO
            END IF
          END DO
!
!-----------------------------------------------------------------------
!  Phytoplankton grazing by zooplankton (rate: ZooGR), phytoplankton
!  assimilated to zooplankton (fraction: ZooAE_N) and egested to small
!  detritus, and phytoplankton mortality (rate: PhyMR) to small
!  detritus. [Landry 1993 L and O 38:468-472]
!-----------------------------------------------------------------------
!
#ifdef TEMP_RATES
#  if defined BIO_2P3Z
          ZooSPsGR=ZooSPsGR0(ng)*dtdays
          ZooLPsGR=ZooLPsGR0(ng)*dtdays
          ZooLPlGR=ZooLPlGR0(ng)*dtdays
          ZooLZsGR=ZooLZsGR0(ng)*dtdays
          ZooPPlGR=ZooPPlGR0(ng)*dtdays
          ZooPZsGR=ZooPZsGR0(ng)*dtdays
          ZooPZlGR=ZooPZlGR0(ng)*dtdays
#  elif defined BIO_2P2Z
          ZooSPsGR=ZooSPsGR0(ng)*dtdays
          ZooSPlGR=ZooSPlGR0(ng)*dtdays
          ZooLPsGR=ZooLPsGR0(ng)*dtdays
          ZooLPlGR=ZooLPlGR0(ng)*dtdays
          ZooLZsGR=ZooLZsGR0(ng)*dtdays
          ZooSDsGR=ZooSDsGR0(ng)*dtdays
          ZooLDsGR=ZooLDsGR0(ng)*dtdays
#  else
          ZooGR=ZooGR0(ng)*dtdays
#  endif
#else
#  if defined BIO_2P3Z
          fac1=dtdays*ZooSPsGR(ng)
          fac2=dtdays*ZooLPsGR(ng)
          fac3=dtdays*ZooLPlGR(ng)
          fac4=dtdays*ZooLZsGR(ng)
          fac5=dtdays*ZooPPlGR(ng)
          fac6=dtdays*ZooPZsGR(ng)
          fac7=dtdays*ZooPZlGR(ng)
#  elif defined BIO_2P2Z
          fac1=dtdays*ZooSPsGR(ng)
          fac2=dtdays*ZooSPlGR(ng)
          fac3=dtdays*ZooLPsGR(ng)
          fac4=dtdays*ZooLPlGR(ng)
          fac5=dtdays*ZooLZsGR(ng)
          fac6=dtdays*ZooSDsGR(ng)
          fac7=dtdays*ZooLDsGR(ng)
#  else
          fac1=dtdays*ZooGR(ng)
#  endif
#endif
          DO k=1,N(ng)
            DO i=Istr,Iend
#ifdef TEMP_RATES
#  if defined BIO_2P3Z
          fac1=ZooSPsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac2=ZooLPsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac3=ZooLPlGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac4=ZooLZsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac5=ZooPPlGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac6=ZooPZsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac7=ZooPZlGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
#  elif defined BIO_2P2Z
          fac1=ZooSPsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac2=ZooSPlGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac3=ZooLPsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac4=ZooLPlGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac5=ZooLZsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac6=ZooSDsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
          fac7=ZooLDsGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
#  else
          fac1=ZooGR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
#  endif
#endif
#if defined BIO_2P3Z
!---------------------------------------------------------------------------
! small phytoplankton grazing by small zooplankton.
!----------------------------------------------------------------------------
              cff1=fac1*Bio(i,k,iZooS)*Bio(i,k,iSPhy)/                  &
     &             (K_ZsPs(ng)+Bio(i,k,iSPhy)*Bio(i,k,iSPhy))
              cff3=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iSPhy)=cff3*Bio(i,k,iSPhy)
              Bio(i,k,iChlS)=cff3*Bio(i,k,iChlS)
              N_Flux_Assim_ZsPs=cff1*Bio(i,k,iSPhy)*ZooSAE_N(ng)
              N_Flux_Egest_ZsPs=Bio(i,k,iSPhy)*cff1*(1.0_r8-ZooSAE_N(ng))
!------------------------------------------------------------------------------
! small phytoplankton grazing by large zooplankton.
!-----------------------------------------------------------------------------
              cff1=fac2*Bio(i,k,iZooL)*Bio(i,k,iSPhy)/                  &
     &             (K_ZlPs(ng)+Bio(i,k,iSPhy)*Bio(i,k,iSPhy))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iSPhy)=cff4*Bio(i,k,iSPhy)
              Bio(i,k,iChlS)=cff4*Bio(i,k,iChlS)
              N_Flux_Assim_ZlPs=cff1*Bio(i,k,iSPhy)*ZooLAE_N(ng)
              N_Flux_Egest_ZlPs=Bio(i,k,iSPhy)*cff1*(1.0_r8-ZooLAE_N(ng))
!------------------------------------------------------------------------------
! large phytoplankton grazing by large zooplankton.
!-----------------------------------------------------------------------------
              cff1=fac3*Bio(i,k,iZooL)*Bio(i,k,iPhyL)/                  &
     &             (K_ZlPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iPhyL)=cff4*Bio(i,k,iPhyL)
              Bio(i,k,iChlL)=cff4*Bio(i,k,iChlL)
              N_Flux_Assim_ZlPl=cff1*Bio(i,k,iPhyL)*ZooLAE_N(ng)
              N_Flux_Egest_ZlPl=Bio(i,k,iPhyL)*cff1*(1.0_r8-ZooLAE_N(ng))
!------------------------------------------------------------------------------
! small zooplankton grazing by large zooplankton.
!-----------------------------------------------------------------------------
              cff1=fac4*Bio(i,k,iZooL)*Bio(i,k,iZooS)/                  &
     &             (K_ZlZs(ng)+Bio(i,k,iZooS)*Bio(i,k,iZooS))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iZooS)=cff4*Bio(i,k,iZooS)
              N_Flux_Assim_ZlZs=cff1*Bio(i,k,iZooS)*ZooLAE_N(ng)
              N_Flux_Egest_ZlZs=Bio(i,k,iZooS)*cff1*(1.0_r8-ZooLAE_N(ng))
!------------------------------------------------------------------------------
! large phytoplankton grazing by predatory zooplankton.
!-----------------------------------------------------------------------------
              ppref = EXP(-ZooP_GrInPl(ng)*                              &
     &                   (Bio(i,k,iZooS)+Bio(i,k,iZooL)))
              cff1=ppref*fac5*Bio(i,k,iZooP)*Bio(i,k,iPhyL)/             &
     &             (K_ZpPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iPhyL)=cff4*Bio(i,k,iPhyL)
              Bio(i,k,iChlL)=cff4*Bio(i,k,iChlL)
              N_Flux_Assim_ZpPl=cff1*Bio(i,k,iPhyL)*ZooPAE_N(ng)
              N_Flux_Egest_ZpPl=Bio(i,k,iPhyL)*cff1*(1.0_r8-ZooPAE_N(ng))
!------------------------------------------------------------------------------
! small zooplankton grazing by predatory zooplankton.
!-----------------------------------------------------------------------------
              ppref = EXP(-ZooP_GrInZs(ng)*                     &
     &                    Bio(i,k,iZooL))
              cff1=ppref*fac6*Bio(i,k,iZooP)*Bio(i,k,iZooS)/             &
     &             (K_ZpZs(ng)+Bio(i,k,iZooS)*Bio(i,k,iZooS))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iZooS)=cff4*Bio(i,k,iZooS)
              N_Flux_Assim_ZpZs=cff1*Bio(i,k,iZooS)*ZooPAE_N(ng)
              N_Flux_Egest_ZpZs=Bio(i,k,iZooS)*cff1*(1.0_r8-ZooPAE_N(ng))
!------------------------------------------------------------------------------
! large zooplankton grazing by predatory zooplankton.
!-----------------------------------------------------------------------------
              cff1=fac7*Bio(i,k,iZooP)*Bio(i,k,iZooL)/             &
     &             (K_ZpZl(ng)+Bio(i,k,iZooL)*Bio(i,k,iZooL))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iZooL)=cff4*Bio(i,k,iZooL)
              N_Flux_Assim_ZpZl=cff1*Bio(i,k,iZooL)*ZooPAE_N(ng)
              N_Flux_Egest_ZpZl=Bio(i,k,iZooL)*cff1*(1.0_r8-ZooPAE_N(ng))
#elif defined BIO_2P2Z
!---------------------------------------------------------------------------
! small phytoplankton grazing by small zooplankton.
!----------------------------------------------------------------------------
              cff1=fac1*Bio(i,k,iZooS)*Bio(i,k,iSPhy)/                  &
     &             (K_ZsPs(ng)+Bio(i,k,iSPhy)*Bio(i,k,iSPhy))
              cff3=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iSPhy)=cff3*Bio(i,k,iSPhy)
              Bio(i,k,iChlS)=cff3*Bio(i,k,iChlS)
              N_Flux_Assim_ZsPs=cff1*Bio(i,k,iSPhy)*ZooSAE_N(ng)
              N_Flux_Egest_ZsPs=Bio(i,k,iSPhy)*cff1*(1.0_r8-ZooSAE_N(ng))
!------------------------------------------------------------------------------
! small phytoplankton grazing by large zooplankton.
!-----------------------------------------------------------------------------
              ppref = EXP(-ZooL_GrInPs(ng)*                              &
     &        (Bio(i,k,iPhyL)+Bio(i,k,iZooS)+Bio(i,k,iSDeN)*switch_ZlDs))
              cff1=ppref*fac3*Bio(i,k,iZooL)*Bio(i,k,iSPhy)/             &
     &             (K_ZlPs(ng)+Bio(i,k,iSPhy)*Bio(i,k,iSPhy))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iSPhy)=cff4*Bio(i,k,iSPhy)
              Bio(i,k,iChlS)=cff4*Bio(i,k,iChlS)
              N_Flux_Assim_ZlPs=cff1*Bio(i,k,iSPhy)*ZooLAE_N(ng)
              N_Flux_Egest_ZlPs=Bio(i,k,iSPhy)*cff1*(1.0_r8-ZooLAE_N(ng))
!------------------------------------------------------------------------------
! large phytoplankton grazing by small zooplankton.
!-----------------------------------------------------------------------------
              ppref = EXP(-ZooS_GrInPl(ng)*(Bio(i,k,iSPhy)))
              cff1=ppref*fac2*Bio(i,k,iZooS)*Bio(i,k,iPhyL)/             &
     &             (K_ZsPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iPhyL)=cff4*Bio(i,k,iPhyL)
              Bio(i,k,iChlL)=cff4*Bio(i,k,iChlL)
              N_Flux_Assim_ZsPl=cff1*Bio(i,k,iPhyL)*ZooSAE_N(ng)
              N_Flux_Egest_ZsPl=Bio(i,k,iPhyL)*cff1*(1.0_r8-ZooSAE_N(ng))
!------------------------------------------------------------------------------
! large phytoplankton grazing by large zooplankton.
!-----------------------------------------------------------------------------
              cff1=fac4*Bio(i,k,iZooL)*Bio(i,k,iPhyL)/                  &
     &             (K_ZlPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iPhyL)=cff4*Bio(i,k,iPhyL)
              Bio(i,k,iChlL)=cff4*Bio(i,k,iChlL)
              N_Flux_Assim_ZlPl=cff1*Bio(i,k,iPhyL)*ZooLAE_N(ng)
              N_Flux_Egest_ZlPl=Bio(i,k,iPhyL)*cff1*(1.0_r8-ZooLAE_N(ng))
!------------------------------------------------------------------------------
! small zooplankton grazing by large zooplankton.
!-----------------------------------------------------------------------------
              cff1=fac5*Bio(i,k,iZooL)*Bio(i,k,iZooS)/                  &
     &             (K_ZlZs(ng)+Bio(i,k,iZooS)*Bio(i,k,iZooS))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iZooS)=cff4*Bio(i,k,iZooS)
              N_Flux_Assim_ZlZs=cff1*Bio(i,k,iZooS)*ZooLAE_N(ng)
              N_Flux_Egest_ZlZs=Bio(i,k,iZooS)*cff1*(1.0_r8-ZooLAE_N(ng))
!------------------------------------------------------------------------------
! small detritus grazing by small zooplankton.
!-----------------------------------------------------------------------------
              ppref = EXP(-ZooS_GrInDs(ng)*                              &
     &                   (Bio(i,k,iSPhy)+Bio(i,k,iPhyL)))
              cff1=ppref*fac6*Bio(i,k,iZooS)*Bio(i,k,iSDeN)/                  &
     &             (K_ZsDs(ng)+Bio(i,k,iSDeN)*Bio(i,k,iSDeN))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iSDeN)=cff4*Bio(i,k,iSDeN)
              N_Flux_Assim_ZsDs=cff1*Bio(i,k,iSDeN)*ZooSAE_N(ng)
              N_Flux_Egest_ZsDs=Bio(i,k,iSDeN)*cff1*(1.0_r8-ZooSAE_N(ng))
!------------------------------------------------------------------------------
! small detritus grazing by large zooplankton.
!-----------------------------------------------------------------------------
              ppref = EXP(-ZooL_GrInDs(ng)*                              &
     &                   (Bio(i,k,iSPhy)+Bio(i,k,iPhyL)+Bio(i,k,iZooS)))
              cff1=ppref*fac7*Bio(i,k,iZooL)*Bio(i,k,iSDeN)/                  &
     &             (K_ZlDs(ng)+Bio(i,k,iSDeN)*Bio(i,k,iSDeN))
              cff4=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iSDeN)=cff4*Bio(i,k,iSDeN)
              N_Flux_Assim_ZlDs=cff1*Bio(i,k,iSDeN)*ZooLAE_N(ng)
              N_Flux_Egest_ZlDs=Bio(i,k,iSDeN)*cff1*(1.0_r8-ZooLAE_N(ng))
#else
!
! Phytoplankton grazing by zooplankton.
!
              cff1=fac1*Bio(i,k,iZoop)*Bio(i,k,iPhyt)/                  &
     &             (K_Phy(ng)+Bio(i,k,iPhyt)*Bio(i,k,iPhyt))
              cff3=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iPhyt)=cff3*Bio(i,k,iPhyt)
              Bio(i,k,iChlo)=cff3*Bio(i,k,iChlo)
#endif
!
! Phytoplankton assimilated to zooplankton and egested to small
! detritus.
!
#if defined BIO_2P3Z
              N_Flux_Assim_Zs=N_Flux_Assim_ZsPs
              N_Flux_Egest_Zs=N_Flux_Egest_ZsPs
              Bio(i,k,iZooS)=Bio(i,k,iZooS)+                            &
     &                       N_Flux_Assim_Zs

              N_Flux_Assim_Zl=N_Flux_Assim_ZlPs+N_Flux_Assim_ZlPl+      &
     &                       N_Flux_Assim_ZlZs
              N_Flux_Egest_Zl=N_Flux_Egest_ZlPs+N_Flux_Egest_ZlPl+      &
     &                       N_Flux_Egest_ZlZs
              Bio(i,k,iZooL)=Bio(i,k,iZooL)+                            &
     &                       N_Flux_Assim_Zl

              N_Flux_Assim_Zp=N_Flux_Assim_ZpPl+N_Flux_Assim_ZpZs+      &
     &                       N_Flux_Assim_ZpZl
              N_Flux_Egest_Zp=N_Flux_Egest_ZpPl+N_Flux_Egest_ZpZs+      &
     &                       N_Flux_Egest_ZpZl
              Bio(i,k,iZooP)=Bio(i,k,iZooP)+                            &
     &                       N_Flux_Assim_Zp

              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)+                            &
     &                       N_Flux_Egest_Zs+N_Flux_Egest_Zl
              Bio(i,k,iLDeN)=Bio(i,k,iLDeN)+                            &
     &                       N_Flux_Egest_Zp

              N_Flux_Assim = N_Flux_Assim_Zp+N_Flux_Assim_Zl+           &
     &                       N_Flux_Assim_Zs

              N_Flux_Egest = N_Flux_Egest_Zp+N_Flux_Egest_Zl+           &
     &                       N_Flux_Egest_Zs
#elif defined BIO_2P2Z
              N_Flux_Assim_Zs=N_Flux_Assim_ZsPs+N_Flux_Assim_ZsPl+      &
     &                       N_Flux_Assim_ZsDs
              N_Flux_Egest_Zs=N_Flux_Egest_ZsPs+N_Flux_Egest_ZsPl+      &
     &                       N_Flux_Egest_ZsDs
              Bio(i,k,iZooS)=Bio(i,k,iZooS)+                            &
     &                       N_Flux_Assim_Zs

              N_Flux_Assim_Zl=N_Flux_Assim_ZlPs+N_Flux_Assim_ZlPl+      &
     &                       N_Flux_Assim_ZlZs+N_Flux_Assim_ZlDs
              N_Flux_Egest_Zl=N_Flux_Egest_ZlPs+N_Flux_Egest_ZlPl+      &
     &                       N_Flux_Egest_ZlZs+N_Flux_Egest_ZlDs
              Bio(i,k,iZooL)=Bio(i,k,iZooL)+                            &
     &                       N_Flux_Assim_Zl

              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)+                            &
     &                       N_Flux_Egest_Zs+N_Flux_Egest_Zl

              N_Flux_Assim = N_Flux_Assim_Zs+N_Flux_Assim_Zl

              N_Flux_Egest = N_Flux_Egest_Zs+N_Flux_Egest_Zl
#else
              N_Flux_Assim=cff1*Bio(i,k,iPhyt)*ZooAE_N(ng)
              N_Flux_Egest=Bio(i,k,iPhyt)*cff1*(1.0_r8-ZooAE_N(ng))
              Bio(i,k,iZoop)=Bio(i,k,iZoop)+                            &
     &                       N_Flux_Assim
              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)+                            &
     &                       N_Flux_Egest
#endif
            END DO
          END DO

#ifdef TEMP_RATES
#  if defined BIO_2P2Z || defined BIO_2P3Z
              PsMR = PsMR0(ng)*dtdays
              PlMR = PlMR0(ng)*dtdays
#  else
              PhyMR = PhyMR0(ng)*dtdays
#  endif
#else
#  if defined BIO_2P2Z || defined BIO_2P3Z
              fac1=dtdays*PsMR(ng)
              fac2=dtdays*PlMR(ng)
#  else
              cff2=dtdays*PhyMR(ng)
#  endif
#endif

          DO k=1,N(ng)
            DO i=Istr,Iend

#ifdef TEMP_RATES
#  if defined BIO_2P2Z || defined BIO_2P3Z
            fac1=PsMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
            fac2=PlMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
#  else
            cff2=PhyMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))
#  endif
#endif
#if defined BIO_2P2Z || defined BIO_2P3Z
!-------------------------------------------------------------------------
! Small phytoplankton mortality (limited by a phytoplankton minimum).
!-------------------------------------------------------------------------
              N_Flux_Pmortal_Ps=fac1*MAX(Bio(i,k,iSPhy)-PhyMin(ng),0.0_r8)
              Bio(i,k,iSPhy)=Bio(i,k,iSPhy)-N_Flux_Pmortal_Ps
              Bio(i,k,iChlS)=Bio(i,k,iChlS)-                            &
     &                       fac1*MAX(Bio(i,k,iChlS)-ChlMin(ng),0.0_r8)
!-------------------------------------------------------------------------
! Large phytoplankton mortality (limited by a phytoplankton minimum).
!-------------------------------------------------------------------------
              N_Flux_Pmortal_Pl=fac2*MAX(Bio(i,k,iPhyL)-PhyMin(ng),0.0_r8)
              Bio(i,k,iPhyL)=Bio(i,k,iPhyL)-N_Flux_Pmortal_Pl
              Bio(i,k,iChlL)=Bio(i,k,iChlL)-                            &
     &                       fac2*MAX(Bio(i,k,iChlL)-ChlMin(ng),0.0_r8)

              N_Flux_Pmortal=N_Flux_Pmortal_Ps+N_Flux_Pmortal_Pl
#else
!
! Phytoplankton mortality (limited by a phytoplankton minimum).
!
              N_Flux_Pmortal=cff2*MAX(Bio(i,k,iPhyt)-PhyMin(ng),0.0_r8)
              Bio(i,k,iPhyt)=Bio(i,k,iPhyt)-N_Flux_Pmortal
              Bio(i,k,iChlo)=Bio(i,k,iChlo)-                            &
     &                       cff2*MAX(Bio(i,k,iChlo)-ChlMin(ng),0.0_r8)
#endif

              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)+                            &
     &                       N_Flux_Pmortal
#ifdef CARBON
              Bio(i,k,iSDeC)=Bio(i,k,iSDeC)+                            &
     &                       PhyCN(ng)*(N_Flux_Egest+N_Flux_Pmortal)+   &
     &                       (PhyCN(ng)-ZooCN(ng))*N_Flux_Assim
#endif
            END DO
          END DO
!
!-----------------------------------------------------------------------
!  Zooplankton basal metabolism to NH4  (rate: ZooBM), zooplankton
!  mortality to small detritus (rate: ZooMR), zooplankton ingestion
!  related excretion (rate: ZooER).
!-----------------------------------------------------------------------
!
#ifdef TEMP_RATES
#   if defined BIO_2P3Z
          ZooSBM = ZooSBM0(ng)*dtdays
          ZooSER = ZooSER0(ng)*dtdays
          ZooSMR = ZooSMR0(ng)*dtdays

          ZooLBM = ZooLBM0(ng)*dtdays
          ZooLER = ZooLER0(ng)*dtdays
          ZooLMR = ZooLMR0(ng)*dtdays

          ZooPBM = ZooPBM0(ng)*dtdays
          ZooPER = ZooPER0(ng)*dtdays
          ZooPMR = ZooPMR0(ng)*dtdays
#   elif defined BIO_2P2Z
          ZooSBM = ZooSBM0(ng)*dtdays
          ZooSER = ZooSER0(ng)*dtdays
          ZooSMR = ZooSMR0(ng)*dtdays

          ZooLBM = ZooLBM0(ng)*dtdays
          ZooLER = ZooLER0(ng)*dtdays
          ZooLMR = ZooLMR0(ng)*dtdays
#  else
          ZooBM = ZooBM0(ng)*dtdays
          ZooER = ZooER0(ng)*dtdays
          ZooMR = ZooMR0(ng)*dtdays
#  endif
#else
#  if defined BIO_2P3Z
          fac2=dtdays*ZooSBM(ng) !metabolism
          fac3=dtdays*ZooSMR(ng) !mortality
          fac4=dtdays*ZooSER(ng) !excretion

          fac5=dtdays*ZooLBM(ng) !metabolism
          fac6=dtdays*ZooLMR(ng) !mortality
          fac7=dtdays*ZooLER(ng) !excretion

          fac8=dtdays*ZooPBM(ng) !metabolism
          fac9=dtdays*ZooPMR(ng) !mortality
          fac10=dtdays*ZooPER(ng) !excretion
#  elif defined BIO_2P2Z
          fac2=dtdays*ZooSBM(ng) !metabolism
          fac3=dtdays*ZooSMR(ng) !mortality
          fac4=dtdays*ZooSER(ng) !excretion

          fac5=dtdays*ZooLBM(ng) !metabolism
          fac6=dtdays*ZooLMR(ng) !mortality
          fac7=dtdays*ZooLER(ng) !excretion
#  else
          fac2=dtdays*ZooBM(ng) !metabolism
          fac3=dtdays*ZooMR(ng) !mortality
          fac4=dtdays*ZooER(ng) !excretion
#  endif
#endif
          DO k=1,N(ng)
            DO i=Istr,Iend
#ifdef TEMP_RATES
#   if defined BIO_2P3Z
          fac2=ZooSBM*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !metabolism
          fac3=ZooSMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !mortality
          fac4=ZooSER*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !excretion

          fac5=ZooLBM*Tcoef0*(Tcoef1**Bio(i,k,itemp))  !metabolism
          fac6=ZooLMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))  !mortality
          fac7=ZooLER*Tcoef0*(Tcoef1**Bio(i,k,itemp))  !excretion

          fac8=ZooPBM*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !metabolism
          fac9=ZooPMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !mortality
          fac10=ZooPER*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !excretion
#   elif defined BIO_2P2Z
          fac2=ZooSBM*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !metabolism
          fac3=ZooSMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !mortality
          fac4=ZooSER*Tcoef0*(Tcoef1**Bio(i,k,itemp))   !excretion

          fac5=ZooLBM*Tcoef0*(Tcoef1**Bio(i,k,itemp))  !metabolism
          fac6=ZooLMR*Tcoef0*(Tcoef1**Bio(i,k,itemp))  !mortality
          fac7=ZooLER*Tcoef0*(Tcoef1**Bio(i,k,itemp))  !excretion
#   else
          fac2=ZooBM*Tcoef0*(Tcoef1**Bio(i,k,itemp)) !metabolism
          fac3=ZooMR*Tcoef0*(Tcoef1**Bio(i,k,itemp)) !mortality
          fac4=ZooER*Tcoef0*(Tcoef1**Bio(i,k,itemp)) !excretion
#   endif
#endif
#if defined BIO_2P3Z
!-----------------------------------------------------------------------
! Small zooplankton (fac2, fac3, fac4)
!-----------------------------------------------------------------------
!
! Small zooplankton mortality
!
              cff2=fac3*Bio(i,k,iZooS)
              N_Flux_Zsmortal=cff2*Bio(i,k,iZooS)
              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)+N_Flux_Zsmortal
              Bio(i,k,iZooS)=Bio(i,k,iZooS)/(1.0_r8+cff2)
!
! Small zooplankton excretion of small phytoplankton
!
              fac1=fac4*Bio(i,k,iSPhy)*Bio(i,k,iSPhy)/                  &
     &             (K_ZsPs(ng)+(Bio(i,k,iSPhy)*Bio(i,k,iSPhy)))
              cff3=fac1*ZooSAE_N(ng)
              Bio(i,k,iZooS)=Bio(i,k,iZooS)/(1.0_r8+cff3)
              N_Flux_Zsexcret=cff3*Bio(i,k,iZooS)

!  Small zooplankton basal metabolism (limited by a zooplankton minimum).
!
              N_Flux_Zsmetabo=fac2*MAX(Bio(i,k,iZooS)-ZooMin(ng),0.0_r8)
              Bio(i,k,iZooS)=Bio(i,k,iZooS)-N_Flux_Zsmetabo
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zsmetabo
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zsmetabo
# endif
!
!-----------------------------------------------------------------------
! Large zooplankton (fac5, fac6, fac7)
!-----------------------------------------------------------------------
!
! Large zooplankton mortality
!
              cff2=fac6*Bio(i,k,iZooL)
              N_Flux_Zlmortal=cff2*Bio(i,k,iZooL)
              Bio(i,k,iLDeN)=Bio(i,k,iLDeN)+N_Flux_Zlmortal
              Bio(i,k,iZooL)=Bio(i,k,iZooL)/(1.0_r8+cff2)
!
! Large zooplankton excretion of small and large phytoplankton and small
! zooplankton
!
              fac1=fac7*Bio(i,k,iSPhy)*Bio(i,k,iSPhy)/                  &
     &             (K_ZlPs(ng)+Bio(i,k,iSPhy)*Bio(i,k,iSPhy))
              fac2=fac7*Bio(i,k,iPhyL)*Bio(i,k,iPhyL)/                  &
     &             (K_ZlPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              fac3=fac7*Bio(i,k,iZooS)*Bio(i,k,iZooS)/                  &
     &             (K_ZlZs(ng)+Bio(i,k,iZooS)*Bio(i,k,iZooS))
              cff3=(fac1+fac2+fac3)*ZooLAE_N(ng)
              Bio(i,k,iZooL)=Bio(i,k,iZooL)/(1.0_r8+cff3)

              N_Flux_Zlexcret=cff3*Bio(i,k,iZooL)

!  Large zooplankton basal metabolism (limited by a zooplankton minimum).
!
              N_Flux_Zlmetabo=fac5*MAX(Bio(i,k,iZooL)-ZooMin(ng),0.0_r8)
              Bio(i,k,iZooL)=Bio(i,k,iZooL)-N_Flux_Zlmetabo
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zlmetabo
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zlmetabo
# endif
!
!-----------------------------------------------------------------------
! Predatory zooplankton (fac8, fac9, fac10)
!-----------------------------------------------------------------------
!
! Predatory zooplankton mortality
!
              cff2=fac9*Bio(i,k,iZooP)
              N_Flux_Zpmortal=cff2*Bio(i,k,iZooP)
              Bio(i,k,iLDeN)=Bio(i,k,iLDeN)+N_Flux_Zpmortal
              Bio(i,k,iZooP)=Bio(i,k,iZooP)/(1.0_r8+cff2)
!
! Predatory zooplankton excretion of small and large phytoplankton and small
! zooplankton
!
              ppref=EXP(-ZooP_GrInPl(ng)*(Bio(i,k,iZooL)+Bio(i,k,iZooS)))
              fac1=ppref*fac10*Bio(i,k,iPhyL)*Bio(i,k,iPhyL)/             &
     &             (K_ZpPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              ppref=EXP(-ZooP_GrInZs(ng)*(Bio(i,k,iZooL)))
              fac2=ppref*fac10*Bio(i,k,iZooS)*Bio(i,k,iZooS)/             &
     &             (K_ZpZs(ng)+Bio(i,k,iZooS)*Bio(i,k,iZooS))
              fac3=fac10*Bio(i,k,iZooL)*Bio(i,k,iZooL)/                   &
     &             (K_ZpZl(ng)+Bio(i,k,iZooL)*Bio(i,k,iZooL))
              cff3=(fac1+fac2+fac3)*ZooPAE_N(ng)
              Bio(i,k,iZooP)=Bio(i,k,iZooP)/(1.0_r8+cff3)


              N_Flux_Zpexcret=cff3*Bio(i,k,iZooP)

! Predatory zooplankton basal metabolism (limited by a zooplankton minimum).
!
              N_Flux_Zpmetabo=fac8*MAX(Bio(i,k,iZooP)-ZooMin(ng),0.0_r8)
              Bio(i,k,iZooP)=Bio(i,k,iZooP)-N_Flux_Zpmetabo
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zpmetabo
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zpmetabo
# endif
! Total fluxes
              N_Flux_Zmortal= N_Flux_Zsmortal+N_Flux_Zlmortal+         &
     &                        N_Flux_Zpmortal
              N_Flux_Zmetabo= N_Flux_Zsmetabo+N_Flux_Zlmetabo+         &
     &                        N_Flux_Zpmetabo
              N_Flux_Zexcret= N_Flux_Zsexcret+N_Flux_Zlexcret+         &
     &                        N_Flux_Zpexcret
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zexcret
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zexcret
# endif
#elif defined BIO_2P2Z
!-----------------------------------------------------------------------
! Small zooplankton (fac2, fac3, fac4)
!-----------------------------------------------------------------------
!
! Small zooplankton mortality
!
              cff2=fac3*Bio(i,k,iZooS)
              N_Flux_Zsmortal=cff2*Bio(i,k,iZooS)
              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)+N_Flux_Zsmortal
              Bio(i,k,iZooS)=Bio(i,k,iZooS)/(1.0_r8+cff2)
!
! Small zooplankton excretion of phytoplankton and detritus
!
              fac1=fac4*Bio(i,k,iSPhy)*Bio(i,k,iSPhy)/                  &
     &             (K_ZsPs(ng)+(Bio(i,k,iSPhy)*Bio(i,k,iSPhy)))
              ppref = EXP(-ZooS_GrInPl(ng)*(Bio(i,k,iSPhy)))
              fac3=ppref*fac4*Bio(i,k,iPhyL)*Bio(i,k,iPhyL)/            &
     &             (K_ZsPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              ppref = EXP(-ZooS_GrInDs(ng)*(Bio(i,k,iSPhy)+             &
     &                    Bio(i,k,iPhyL)))
              fac8=ppref*fac4*Bio(i,k,iSDeN)*Bio(i,k,iSDeN)/            &
     &             (K_ZsDs(ng)+Bio(i,k,iSDeN)*Bio(i,k,iSDeN))
              cff3=(fac1+fac3+fac8)*ZooSAE_N(ng)
              Bio(i,k,iZooS)=Bio(i,k,iZooS)/(1.0_r8+cff3)
              N_Flux_Zsexcret=cff3*Bio(i,k,iZooS)

!  Small zooplankton basal metabolism (limited by a zooplankton minimum).
!
              N_Flux_Zsmetabo=fac2*MAX(Bio(i,k,iZooS)-ZooMin(ng),0.0_r8)
              Bio(i,k,iZooS)=Bio(i,k,iZooS)-N_Flux_Zsmetabo
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zsmetabo
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zsmetabo
# endif
!
!-----------------------------------------------------------------------
! Large zooplankton (fac5, fac6, fac7)
!-----------------------------------------------------------------------
!
! Large zooplankton mortality
!
              cff2=fac6*Bio(i,k,iZooL)
              N_Flux_Zlmortal=cff2*Bio(i,k,iZooL)
              Bio(i,k,iLDeN)=Bio(i,k,iLDeN)+N_Flux_Zlmortal
              Bio(i,k,iZooL)=Bio(i,k,iZooL)/(1.0_r8+cff2)
!
! Large zooplankton excretion of small and large phytoplankton and small
! zooplankton and small detritus
!
              ppref = EXP(-ZooL_GrInPs(ng)*                              &
     &        (Bio(i,k,iPhyL)+Bio(i,k,iZooS)+Bio(i,k,iSDeN)*switch_ZlDs))
              fac1=ppref*fac7*Bio(i,k,iSPhy)*Bio(i,k,iSPhy)/             &
     &             (K_ZlPs(ng)+Bio(i,k,iSPhy)*Bio(i,k,iSPhy))
              fac2=fac7*Bio(i,k,iPhyL)*Bio(i,k,iPhyL)/                   &
     &             (K_ZlPl(ng)+Bio(i,k,iPhyL)*Bio(i,k,iPhyL))
              fac3=fac7*Bio(i,k,iZooS)*Bio(i,k,iZooS)/                   &
     &             (K_ZlZs(ng)+Bio(i,k,iZooS)*Bio(i,k,iZooS))
              ppref = EXP(-ZooL_GrInDs(ng)*                              &
     &                   (Bio(i,k,iSPhy)+Bio(i,k,iPhyL)+Bio(i,k,iZooS)))
              fac4=fac7*Bio(i,k,iSDeN)*Bio(i,k,iSDeN)/                   &
     &             (K_ZlDs(ng)+Bio(i,k,iSDeN)*Bio(i,k,iSDeN))
              cff3=(fac1+fac2+fac3+fac4)*ZooLAE_N(ng)
              Bio(i,k,iZooL)=Bio(i,k,iZooL)/(1.0_r8+cff3)
              N_Flux_Zlexcret=cff3*Bio(i,k,iZooL)

!  Large zooplankton basal metabolism (limited by a zooplankton minimum).
!
              N_Flux_Zlmetabo=fac5*MAX(Bio(i,k,iZooL)-ZooMin(ng),0.0_r8)
              Bio(i,k,iZooL)=Bio(i,k,iZooL)-N_Flux_Zlmetabo
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zlmetabo
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zlmetabo
# endif

! Total fluxes
              N_Flux_Zmortal= N_Flux_Zsmortal+N_Flux_Zlmortal
              N_Flux_Zmetabo= N_Flux_Zsmetabo+N_Flux_Zlmetabo
              N_Flux_Zexcret= N_Flux_Zsexcret+N_Flux_Zlexcret
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zexcret
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zexcret
# endif
#else
!
!  Zooplankton basal metabolism (limited by a zooplankton minimum).
!
              N_Flux_Zmetabo=fac2*MAX(Bio(i,k,iZoop)-ZooMin(ng),0.0_r8)
              Bio(i,k,iZoop)=Bio(i,k,iZoop)-N_Flux_Zmetabo
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zmetabo
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zmetabo
# endif
!
! Zooplankton mortality
!
              fac1=fac4*Bio(i,k,iPhyt)*Bio(i,k,iPhyt)/                  &
     &             (K_Phy(ng)+Bio(i,k,iPhyt)*Bio(i,k,iPhyt))
              cff2=fac3*Bio(i,k,iZoop)
              cff3=fac1*ZooAE_N(ng)
              Bio(i,k,iZoop)=Bio(i,k,iZoop)/                            &
     &                       (1.0_r8+cff2+cff3)
              N_Flux_Zmortal=cff2*Bio(i,k,iZoop)
              N_Flux_Zexcret=cff3*Bio(i,k,iZoop)
              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)+N_Flux_Zmortal
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_Zexcret
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)*N_Flux_Zexcret
# endif
#endif
#ifdef OXYGEN
              Bio(i,k,iOxyg)=Bio(i,k,iOxyg)-                            &
     &                       rOxNH4*(N_Flux_Zmetabo+N_Flux_Zexcret)
#endif
#ifdef CARBON
# ifndef DETN2C
              Bio(i,k,iSDeC)=Bio(i,k,iSDeC)+                            &
     &                       ZooCN(ng)*N_Flux_Zmortal
# endif
              Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+                            &
     &                       ZooCN(ng)*(N_Flux_Zmetabo+N_Flux_Zexcret)
#ifdef TALK_NONCONSERV
              Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+N_Flux_Zmetabo+             &
     &                       N_Flux_Zexcret
#endif
#endif
            END DO
          END DO
!
!-----------------------------------------------------------------------
!  Coagulation of phytoplankton and small detritus to large detritus.
!-----------------------------------------------------------------------
!
          fac1=dtdays*CoagR(ng)
          DO k=1,N(ng)
            DO i=Istr,Iend
#if defined BIO_2P2Z || defined BIO_2P3Z
              cff1=fac1*(Bio(i,k,iSDeN)+Bio(i,k,iPhyL))
              cff2=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iPhyL)=Bio(i,k,iPhyL)*cff2
              Bio(i,k,iChlL)=Bio(i,k,iChlL)*cff2
              N_Flux_CoagP=Bio(i,k,iPhyL)*cff1
#else
              cff1=fac1*(Bio(i,k,iSDeN)+Bio(i,k,iPhyt))
              cff2=1.0_r8/(1.0_r8+cff1)
              Bio(i,k,iPhyt)=Bio(i,k,iPhyt)*cff2
              Bio(i,k,iChlo)=Bio(i,k,iChlo)*cff2
              N_Flux_CoagP=Bio(i,k,iPhyt)*cff1
#endif
              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)*cff2
              N_Flux_CoagD=Bio(i,k,iSDeN)*cff1
              Bio(i,k,iLDeN)=Bio(i,k,iLDeN)+                            &
     &                       N_Flux_CoagP+N_Flux_CoagD
#if defined CARBON && !defined DETN2C
              Bio(i,k,iSDeC)=Bio(i,k,iSDeC)-PhyCN(ng)*N_Flux_CoagD
              Bio(i,k,iLDeC)=Bio(i,k,iLDeC)+                            &
     &                       PhyCN(ng)*(N_Flux_CoagP+N_Flux_CoagD)
#endif
            END DO
          END DO
!
!-----------------------------------------------------------------------
!  Detritus recycling to NH4, remineralization.
!-----------------------------------------------------------------------
!
#ifdef OXYGEN
          DO k=1,N(ng)
            DO i=Istr,Iend
              fac1=MAX(Bio(i,k,iOxyg)-6.0_r8,0.0_r8) ! O2 off max
              fac2=MAX(fac1/(3.0_r8+fac1),0.0_r8) ! MM for O2 dependence
              cff1=dtdays*SDeRRN(ng)*fac2
              cff2=1.0_r8/(1.0_r8+cff1)
              cff3=dtdays*LDeRRN(ng)*fac2
              cff4=1.0_r8/(1.0_r8+cff3)
              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)*cff2
              Bio(i,k,iLDeN)=Bio(i,k,iLDeN)*cff4
              N_Flux_RemineS=Bio(i,k,iSDeN)*cff1
              N_Flux_RemineL=Bio(i,k,iLDeN)*cff3
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+                            &
     &                       N_Flux_RemineS+N_Flux_RemineL
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)                   &
     &                      *(N_Flux_RemineS+N_Flux_RemineL)
# endif
              Bio(i,k,iOxyg)=Bio(i,k,iOxyg)-                            &
     &                       (N_Flux_RemineS+N_Flux_RemineL)*rOxNH4
# ifdef CARBON
#  ifdef DETN2C
              Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+                            &
     &                       (N_Flux_RemineS+N_Flux_RemineL)*PhyCN(ng)
#  endif
#  ifdef TALK_NONCONSERV
              Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+N_Flux_RemineS+             &
     &                       N_Flux_RemineL
#  endif
# endif
# ifdef RIVER_DON
              cff7=dtdays*RDeRRN(ng)*fac2
              cff8=1.0_r8/(1.0_r8+cff7)
              Bio(i,k,iRDeN)=Bio(i,k,iRDeN)*cff8
              N_Flux_RemineR=Bio(i,k,iRDeN)*cff7
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+                            &
     &                       N_Flux_RemineR
#  ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)                   &
     &                      *N_Flux_RemineR
#  endif
              Bio(i,k,iOxyg)=Bio(i,k,iOxyg)-N_Flux_RemineR*rOxNH4
#  ifdef CARBON
#   ifdef DETN2C
              Bio(i,k,iTIC)=Bio(i,k,iTIC)+N_Flux_RemineR*PhyCN(ng)
#   endif
#   ifdef TALK_NONCONSERV
              Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+N_Flux_RemineR
#   endif
#  endif
# endif
            END DO
          END DO
#else
          cff1=dtdays*SDeRRN(ng)
          cff2=1.0_r8/(1.0_r8+cff1)
          cff3=dtdays*LDeRRN(ng)
          cff4=1.0_r8/(1.0_r8+cff3)
# ifdef RIVER_DON
          cff7=dtdays*RDeRRN(ng)
          cff8=1.0_r8/(1.0_r8+cff7)
# endif
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio(i,k,iSDeN)=Bio(i,k,iSDeN)*cff2
              Bio(i,k,iLDeN)=Bio(i,k,iLDeN)*cff4
              N_Flux_RemineS=Bio(i,k,iSDeN)*cff1
              N_Flux_RemineL=Bio(i,k,iLDeN)*cff3
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+                            &
     &                       N_Flux_RemineS+N_Flux_RemineL
# ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)                   &
     &                      *(N_Flux_RemineS+N_Flux_RemineL)
# endif
# ifdef CARBON
#  ifdef DETN2C
              Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+                            &
     &                       (N_Flux_ReminS+N_Flux_RemineL)*PhyCN(ng)
#  endif
#  ifdef TALK_NONCONSERV
              Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+N_Flux_RemineS+             &
     &                       N_Flux_RemineL
#  endif
# endif
# ifdef RIVER_DON
              Bio(i,k,iRDeN)=Bio(i,k,iRDeN)*cff8
              N_Flux_RemineR=Bio(i,k,iRDeN)*cff7
              Bio(i,k,iNH4_)=Bio(i,k,iNH4_)+N_Flux_RemineR
#  ifdef PO4
              Bio(i,k,iPO4_)=Bio(i,k,iPO4_)+R_P2N(ng)                   &
     &                      *N_Flux_RemineR
#  endif
#  ifdef CARBON
#   ifdef DETN2C
              Bio(i,k,iTIC)=Bio(i,k,iTIC)+N_Flux_RemineR*PhyCN(ng)
#   endif
#   ifdef TALK_NONCONSERV
              Bio(i,k,iTAlk)=Bio(i,k,iTAlk)+N_Flux_RemineR
#   endif
#  endif
# endif
            END DO
          END DO
#endif
#ifdef OXYGEN
!
!-----------------------------------------------------------------------
!  Surface O2 gas exchange.
!-----------------------------------------------------------------------
!
!  Compute surface O2 gas exchange.
!
          cff1=rho0*550.0_r8
# if defined RW14_OXYGEN_SC
          cff2=dtdays*0.251_r8*24.0_r8/100.0_r8
# else
          cff2=dtdays*0.31_r8*24.0_r8/100.0_r8
# endif
          k=N(ng)
          DO i=Istr,Iend
!
!  Compute O2 transfer velocity : u10squared (u10 in m/s)
!
# ifdef BULK_FLUXES
            u10squ=Uwind(i,j)*Uwind(i,j)+Vwind(i,j)*Vwind(i,j)
# else
            u10squ=cff1*SQRT((0.5_r8*(sustr(i,j)+sustr(i+1,j)))**2+     &
     &                       (0.5_r8*(svstr(i,j)+svstr(i,j+1)))**2)
# endif
            SchmidtN_Ox=A_O2-Bio(i,k,itemp)*(B_O2-Bio(i,k,itemp)*(C_O2- &
     &                                            Bio(i,k,itemp)*(D_O2- &
     &                                            Bio(i,k,itemp)*E_O2)))
            cff3=cff2*u10squ*SQRT(660.0_r8/SchmidtN_Ox)
!
!  Calculate O2 saturation concentration using Garcia and Gordon
!  L and O (1992) formula, (EXP(AA) is in ml/l).
!
            TS=LOG((298.15_r8-Bio(i,k,itemp))/                          &
     &             (273.15_r8+Bio(i,k,itemp)))
            AA=OA0+TS*(OA1+TS*(OA2+TS*(OA3+TS*(OA4+TS*OA5))))+          &
     &             Bio(i,k,isalt)*(OB0+TS*(OB1+TS*(OB2+TS*OB3)))+       &
     &             OC0*Bio(i,k,isalt)*Bio(i,k,isalt)
!
!  Convert from ml/l to mmol/m3.
!
            O2satu=l2mol*EXP(AA)
!
!  Add in O2 gas exchange.
!
            O2_Flux=cff3*(O2satu-Bio(i,k,iOxyg))
            Bio(i,k,iOxyg)=Bio(i,k,iOxyg)+                              &
     &                     O2_Flux*Hz_inv(i,k)
# ifdef DIAGNOSTICS_BIO
            DiaBio2d(i,j,iO2fx)=DiaBio2d(i,j,iO2fx)+                    &
#  ifdef WET_DRY
     &                          rmask_full(i,j)*                        &
#  endif
     &                          O2_Flux*fiter
#  ifdef FLOAT_PROFILE
            DiaFlt2d(i,j,iO2fx)=(1/dtdays)*                             &
#   ifdef WET_DRY
     &                          rmask_full(i,j)*                        &
#   endif
     &                          O2_Flux
#  endif
# endif

          END DO
#endif

#ifdef CARBON
# ifndef DETN2C
!
!-----------------------------------------------------------------------
!  Allow different remineralization rates for detrital C and detrital N.
!-----------------------------------------------------------------------
!
          cff1=dtdays*SDeRRC(ng)
          cff2=1.0_r8/(1.0_r8+cff1)
          cff3=dtdays*LDeRRC(ng)
          cff4=1.0_r8/(1.0_r8+cff3)
#  ifdef RIVER_DON
          cff7=dtdays*RDeRRC(ng)
          cff8=1.0_r8/(1.0_r8+cff7)
#  endif
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio(i,k,iSDeC)=Bio(i,k,iSDeC)*cff2
              Bio(i,k,iLDeC)=Bio(i,k,iLDeC)*cff4
              C_Flux_RemineS=Bio(i,k,iSDeC)*cff1
              C_Flux_RemineL=Bio(i,k,iLDeC)*cff3
              Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+                            &
     &                       C_Flux_RemineS+C_Flux_RemineL
#  ifdef RIVER_DON
              Bio(i,k,iRDeC)=Bio(i,k,iRDeC)*cff8
              C_Flux_RemineR=Bio(i,k,iRDeC)*cff7
              Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+C_Flux_RemineR
#  endif
            END DO
          END DO
# endif
# ifndef TALK_NONCONSERV
!
!  Alkalinity is treated as a diagnostic variable. TAlk = f(S[PSU])
!  following Brewer et al. (1986).
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              Bio(i,k,iTAlk)=587.05_r8+50.56_r8*Bio(i,k,isalt)
            END DO
          END DO
# endif
!
!-----------------------------------------------------------------------
!  Surface CO2 gas exchange.
!-----------------------------------------------------------------------
!
!  Compute equilibrium partial pressure inorganic carbon (ppmv) at the
!  surface.
!
          k=N(ng)
# ifdef pCO2_RZ
          CALL pCO2_water_RZ (Istr, Iend, LBi, UBi, LBj, UBj,           &
     &                        IminS, ImaxS, j, DoNewton,                &
#  ifdef MASKING
     &                        rmask,                                    &
#  endif
     &                        Bio(IminS:,k,itemp), Bio(IminS:,k,isalt), &
     &                        Bio(IminS:,k,iTIC_), Bio(IminS:,k,iTAlk), &
     &                        pH, pCO2)
# else
          CALL pCO2_water (Istr, Iend, LBi, UBi, LBj, UBj,              &
     &                     IminS, ImaxS, j, DoNewton,                   &
#  ifdef MASKING
     &                     rmask,                                       &
#  endif
     &                     Bio(IminS:,k,itemp), Bio(IminS:,k,isalt),    &
     &                     Bio(IminS:,k,iTIC_), Bio(IminS:,k,iTAlk),    &
     &                     0.0_r8, 0.0_r8, pH, pCO2)
# endif
!
!  Compute surface CO2 gas exchange.
!
          cff1=rho0*550.0_r8
# if defined RW14_CO2_SC
          cff2=dtdays*0.251_r8*24.0_r8/100.0_r8
# else
          cff2=dtdays*0.31_r8*24.0_r8/100.0_r8
# endif
          DO i=Istr,Iend
!
!  Compute CO2 transfer velocity : u10squared (u10 in m/s)
!
# ifdef BULK_FLUXES
            u10squ=Uwind(i,j)**2+Vwind(i,j)**2
# else
            u10squ=cff1*SQRT((0.5_r8*(sustr(i,j)+sustr(i+1,j)))**2+     &
     &                       (0.5_r8*(svstr(i,j)+svstr(i,j+1)))**2)
# endif
            SchmidtN=A_CO2-Bio(i,k,itemp)*(B_CO2-Bio(i,k,itemp)*(C_CO2- &
     &                                           Bio(i,k,itemp)*(D_CO2- &
     &                                           Bio(i,k,itemp)*E_CO2)))
            cff3=cff2*u10squ*SQRT(660.0_r8/SchmidtN)
!
!  Calculate CO2 solubility [mol/(kg.atm)] using Weiss (1974) formula.
!
            TempK=0.01_r8*(Bio(i,k,itemp)+273.15_r8)
            CO2_sol=EXP(A1+                                             &
     &                  A2/TempK+                                       &
     &                  A3*LOG(TempK)+                                  &
     &                  Bio(i,k,isalt)*(B1+TempK*(B2+B3*TempK)))
!
!  Add in CO2 gas exchange.
!
            CALL caldate (tdays(ng), yy_i=year, yd_dp=yday)
            pmonth=year-1951.0_r8+yday/365.0_r8
# if defined PCO2AIR_DATA
            pCO2air_secular=380.464_r8+9.321_r8*SIN(pi2*yday/365.25_r8+ &
     &                      1.068_r8)
            CO2_Flux=cff3*CO2_sol*(pCO2air_secular-pCO2(i))
# elif defined PCO2AIR_MAUNALOA
            fyear=year+MIN((yday-1.0_r8)/365.0_r8,1.0_r8)
            pCO2air_secular=D0*SIN(pi2*(fyear-D1)/D2)+                  &
     &                      D3*SIN(pi2*(fyear-D4)/D5)+                  &
     &                      D6*SIN(pi2*(fyear-D7)/D8)+                  &
     &                      D9*fyear**2+D10*fyear+D11
            CO2_Flux=cff3*CO2_sol*(pCO2air_secular-pCO2(i))
# elif defined PCO2AIR_SABLEISLAND
            fyear=year+MIN((yday-1.0_r8)/365.0_r8,1.0_r8)
            pCO2air_secular=D0+D1*fyear+D2*fyear**2+                    &
     &                      D3*1.25_r8*LOG(SIN(pi2*fyear+D4)+D5)
            CO2_Flux=cff3*CO2_sol*(pCO2air_secular-pCO2(i))
# else
            CO2_Flux=cff3*CO2_sol*(pCO2air(ng)-pCO2(i))
# endif
            Bio(i,k,iTIC_)=Bio(i,k,iTIC_)+                              &
     &                     CO2_Flux*Hz_inv(i,k)
# ifdef DIAGNOSTICS_BIO
            DiaBio2d(i,j,iCOfx)=DiaBio2d(i,j,iCOfx)+                    &
#  ifdef WET_DRY
     &                          rmask_full(i,j)*                        &
#  endif
     &                          CO2_Flux*fiter
            DiaBio2d(i,j,ipCO2)=pCO2(i)
#  ifdef WET_DRY
            DiaBio2d(i,j,ipCO2)=DiaBio2d(i,j,ipCO2)*rmask_full(i,j)
#  endif
#  ifdef FLOAT_PROFILE
            DiaFlt2d(i,j,iCOfx)=(1/dtdays)*                             &
#   ifdef WET_DRY
     &                          rmask_full(i,j)*                        &
#   endif
     &                          CO2_Flux
            DiaFlt2d(i,j,ipCO2)=pCO2(i)
#   ifdef WET_DRY
            DiaFlt2d(i,j,ipCO2)=DiaFlt2d(i,j,ipCO2)*rmask_full(i,j)
#   endif
#  endif
# endif
          END DO
#endif
!
!-----------------------------------------------------------------------
!  Vertical sinking terms.
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
#ifdef BIO_SEDIMENT
!
!  Particulate flux reaching the seafloor is remineralized and returned
!  to the dissolved nitrate pool. Without this conversion, particulate
!  material falls out of the system. This is a temporary fix to restore
!  total nitrogen conservation. It will be replaced later by a
!  parameterization that includes the time delay of remineralization
!  and dissolved oxygen.
!
            cff2=4.0_r8/16.0_r8
# ifdef OXYGEN
            cff3=115.0_r8/16.0_r8
            cff4=106.0_r8/16.0_r8
# endif
# if defined BIO_2P2Z || defined BIO_2P3Z
            IF ((ibio.eq.iSPhy).or.                                     &
     &          (ibio.eq.iPhyL).or.                                     &
# else
            IF ((ibio.eq.iPhyt).or.                                     &
# endif
     &          (ibio.eq.iSDeN).or.                                     &
     &          (ibio.eq.iLDeN)) THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
# if defined HRM_SOD
                IF ((lonr(i,j).le.296.55_r8).and.                       &
     &              (latr(i,j).ge.44.65_r8)) THEN
                  cff1=0.0_r8
                END IF
# elif defined HRM2_SOD
                IF ((i.le.68).and.(j.ge.248)) THEN
                  cff1=0.0_r8
                END IF
# elif defined HRM3_SOD
                  cff1=0.0_r8
# endif
# ifdef DENITRIFICATION
                Bio(i,1,iNH4_)=Bio(i,1,iNH4_)+cff1*cff2
#   if defined CARBON && defined TALK_NONCONSERV
                Bio(i,1,iTAlk)=Bio(i,1,iTAlk)+cff1*cff2
#   endif
#  ifdef DIAGNOSTICS_BIO
                DiaBio2d(i,j,iDNIT)=DiaBio2d(i,j,iDNIT)+                &
#   ifdef WET_DRY
     &                              rmask_full(i,j)*                    &
#   endif
     &                              (1.0_r8-cff2)*cff1*Hz(i,j,1)*fiter
#   ifdef FLOAT_PROFILE
                DiaFlt2d(i,j,iDNIT)=(1/dtdays)*                         &
#    ifdef WET_DRY
     &                              rmask_full(i,j)*                    &
#    endif
     &                              (1.0_r8-cff2)*cff1*Hz(i,j,1)
#   endif
#  endif
#  ifdef PO4
                Bio(i,1,iPO4_)=Bio(i,1,iPO4_)+cff1*R_P2N(ng)
#  endif
#  ifdef OXYGEN
                Bio(i,1,iOxyg)=Bio(i,1,iOxyg)-cff1*cff3
#  endif
# else
                Bio(i,1,iNH4_)=Bio(i,1,iNH4_)+cff1
#   ifdef PO4
                Bio(i,1,iPO4_)=Bio(i,1,iPO4_)+cff1*R_P2N(ng)
#   endif
#  ifdef OXYGEN
                Bio(i,1,iOxyg)=Bio(i,1,iOxyg)-cff1*cff4
#  endif
#  if defined CARBON && defined TALK_NONCONSERV
                Bio(i,1,iTAlk)=Bio(i,1,iTAlk)+cff1
#  endif
# endif
              END DO
            END IF
# ifdef CARBON
#  ifdef DENITRIFICATION
            cff3=12.0_r8
            cff4=0.74_r8
#  endif
            IF ((ibio.eq.iSDeC).or.                                     &
     &          (ibio.eq.iLDeC))THEN
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
#  if defined HRM_SOD
                IF ((lonr(i,j).le.296.55_r8).and.                       &
     &              (latr(i,j).ge.44.65_r8)) THEN
                  cff1=0.0_r8
                END IF
#  elif defined HRM2_SOD
                IF ((i.le.68).and.(j.ge.248)) THEN
                  cff1=0.0_r8
                END IF
#  elif defined HRM3_SOD
                  cff1=0.0_r8
#  endif
                Bio(i,1,iTIC_)=Bio(i,1,iTIC_)+cff1
              END DO
            END IF
#if defined BIO_2P2Z || defined BIO_2P3Z
            IF ((ibio.eq.iSPhy).or.                                     &
     &          (ibio.eq.iPhyL)) THEN
#else
            IF (ibio.eq.iPhyt) THEN
#endif
              DO i=Istr,Iend
                cff1=FC(i,0)*Hz_inv(i,1)
#  if defined HRM_SOD
                IF ((lonr(i,j).le.296.55_r8).and.                       &
     &              (latr(i,j).ge.44.65_r8)) THEN
                  cff1=0.0_r8
                END IF
#  elif defined HRM2_SOD
                IF ((i.le.68).and.(j.ge.248)) THEN
                  cff1=0.0_r8
                END IF
#  elif defined HRM3_SOD
                  cff1=0.0_r8
#  endif
                Bio(i,1,iTIC_)=Bio(i,1,iTIC_)+cff1*PhyCN(ng)
              END DO
            END IF
# endif
#endif
          END DO SINK_LOOP
#if defined HRM_SOD || defined HRM2_SOD || defined HRM3_SOD
!
! Constant sediment respiration from Shubadeep Rakshit:
! We obtained the sediment respiration values from a combination of sediment oxygen 
! microsensor profiling and modeling, spanned over different seasons in the year 2019.
! We obtained that average sediment respiration was 16.1 mmol m-2 d-1. This is 
! imposed for the Harbour/Bedford Basin area.
! If BIO_SEDIMENT is defined then other areas use instant remineralization. 
!
          DO i=Istr,Iend
            cff1=0.0_r8
# if defined HRM_SOD
             IF ((lonr(i,j).le.296.55_r8).and.                          &
     &           (latr(i,j).ge.44.65_r8)) THEN
               cff1=16.1_r8*Hz_inv(i,1)*dtdays
             END IF
# elif defined HRM2_SOD
             IF ((i.le.68).and.(j.ge.248)) THEN
               cff1=16.1_r8*Hz_inv(i,1)*dtdays 
             END IF
# elif defined HRM3_SOD
               cff1=16.1_r8*Hz_inv(i,1)*dtdays
# endif
            Bio(i,1,iOxyg)=Bio(i,1,iOxyg)-cff1
            Bio(i,1,iNH4_)=Bio(i,1,iNH4_)+cff1*0.036_r8
# ifdef CARBON
              Bio(i,1,iTIC_)=Bio(i,1,iTIC_)+cff1*0.9475_r8
#  ifdef TALK_NONCONSERV
              Bio(i,1,iTAlk)=Bio(i,1,iTAlk)+cff1*0.036_r8
#  endif
# endif
# ifdef PO4
! Assumption: PO4 is produced in Redfield ratio with TIC
!
              Bio(i,1,iPO4_)=Bio(i,1,iPO4_)+cff1*0.9475_r8/106.0_r8
# endif
          END DO
#endif
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
#ifdef CARBON
        DO k=1,N(ng)
          DO i=Istr,Iend
            Bio(i,k,iTIC_)=MIN(Bio(i,k,iTIC_),3000.0_r8)
            Bio(i,k,iTIC_)=MAX(Bio(i,k,iTIC_),400.0_r8)
          END DO
        END DO
#endif
        DO itrc=1,NBT
          ibio=idbio(itrc)
          DO k=1,N(ng)
            DO i=Istr,Iend
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

#ifdef CARBON
# ifdef pCO2_RZ
      SUBROUTINE pCO2_water_RZ (Istr, Iend,                             &
     &                          LBi, UBi, LBj, UBj, IminS, ImaxS,       &
     &                          j, DoNewton,                            &
#  ifdef MASKING
     &                          rmask,                                  &
#  endif
     &                          T, S, TIC, TAlk, pH, pCO2)
!
!***********************************************************************
!                                                                      !
!  This routine computes equilibrium partial pressure of CO2 (pCO2)    !
!  in the surface seawater.                                            !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     Istr       Starting tile index in the I-direction.               !
!     Iend       Ending   tile index in the I-direction.               !
!     LBi        I-dimension lower bound.                              !
!     UBi        I-dimension upper bound.                              !
!     LBj        J-dimension lower bound.                              !
!     UBj        J-dimension upper bound.                              !
!     IminS      I-dimension lower bound for private arrays.           !
!     ImaxS      I-dimension upper bound for private arrays.           !
!     j          j-pipelined index.                                    !
!     DoNewton   Iteration solver:                                     !
!                  [0] Bracket and bisection.                          !
!                  [1] Newton-Raphson method.                          !
!     rmask      Land/Sea masking.                                     !
!     T          Surface temperature (Celsius).                        !
!     S          Surface salinity (PSS).                               !
!     TIC        Total inorganic carbon (millimol/m3).                 !
!     TAlk       Total alkalinity (milli-equivalents/m3).              !
!     pH         Best pH guess.                                        !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     pCO2       partial pressure of CO2 (ppmv).                       !
!                                                                      !
!  Check Value:  (T=24, S=36.6, TIC=2040, TAlk=2390, PO4b=0,           !
!                 SiO3=0, pH=8)                                        !
!                                                                      !
!                pcO2= ppmv  (DoNewton=0)                              !
!                pCO2= ppmv  (DoNewton=1)                              !
!                                                                      !
!  This subroutine was adapted by Katja Fennel (Nov 2005) from         !
!  Zeebe and Wolf-Gladrow (2001).                                      !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!    Zeebe, R.E. and D. Wolf-Gladrow,  2005:  CO2 in Seawater:         !
!      Equilibrium, kinetics, isotopes, Elsevier Oceanographic         !
!      Series, 65, pp 346.                                             !
!                                                                      !
!***********************************************************************
!
      USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      integer,  intent(in) :: LBi, UBi, LBj, UBj, IminS, ImaxS
      integer,  intent(in) :: Istr, Iend, j, DoNewton
!
#  ifdef ASSUMED_SHAPE
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: T(IminS:)
      real(r8), intent(in) :: S(IminS:)
      real(r8), intent(in) :: TIC(IminS:)
      real(r8), intent(in) :: TAlk(IminS:)
      real(r8), intent(inout) :: pH(LBi:,LBj:)
#  else
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: T(IminS:ImaxS)
      real(r8), intent(in) :: S(IminS:ImaxS)
      real(r8), intent(in) :: TIC(IminS:ImaxS)
      real(r8), intent(in) :: TAlk(IminS:ImaxS)
      real(r8), intent(inout) :: pH(LBi:UBi,LBj:UBj)
#  endif

      real(r8), intent(out) :: pCO2(IminS:ImaxS)
!
!  Local variable declarations.
!
      integer, parameter :: InewtonMax = 10
      integer, parameter :: IbrackMax = 30

      integer :: Hstep, Ibrack, Inewton, i

      real(r8) :: Tk, centiTk, invTk, logTk
      real(r8) :: scl, sqrtS
      real(r8) :: borate, alk, dic
      real(r8) :: ff, K1, K2, K12, Kb, Kw
      real(r8) :: p5, p4, p3, p2, p1, p0
      real(r8) :: df, fn, fni(3), ftest
      real(r8) :: deltaX, invX, invX2, X, X2, X3
      real(r8) :: pH_guess, pH_hi, pH_lo
      real(r8) :: X_guess, X_hi, X_lo, X_mid
      real(r8) :: CO2star, Htotal, Htotal2
!
!=======================================================================
!  Determine coefficients for surface carbon chemisty.  If land/sea
!  masking, compute only on water points.
!=======================================================================
!
      I_LOOP: DO i=Istr,Iend
#  ifdef MASKING
        IF (rmask(i,j).gt.0.0_r8) THEN
#  endif
        Tk=T(i)+273.15_r8
        centiTk=0.01_r8*Tk
        invTk=1.0_r8/Tk
        logTk=LOG(Tk)
        sqrtS=SQRT(S(i))
        scl=S(i)/1.80655_r8

        alk= TAlk(i)*0.000001_r8
        dic = TIC(i)*0.000001_r8
!
!-----------------------------------------------------------------------
!  Correction term for non-ideality, ff=k0*(1-pH2O). Equation 13 with
!  table 6 values from Weiss and Price (1980, Mar. Chem., 8, 347-359).
!-----------------------------------------------------------------------
!
        ff=EXP(-162.8301_r8+                                            &
     &         218.2968_r8/centiTk+                                     &
     &         LOG(centiTk)*90.9241_r8-                                 &
     &         centiTk*centiTk*1.47696_r8+                              &
     &         S(i)*(0.025695_r8-                                       &
     &               centiTk*(0.025225_r8-                              &
     &                        centiTk*0.0049867_r8)))
!
!-----------------------------------------------------------------------
!  Compute first (K1) and second (K2) dissociation constant of carboinic
!  acid:
!
!           K1 = [H][HCO3]/[H2CO3]
!           K2 = [H][CO3]/[HCO3]
!
!  From Millero (1995; page 664) using Mehrbach et al. (1973) data on
!  seawater scale.
!-----------------------------------------------------------------------
!
        K1=10.0_r8**(62.008_r8-                                         &
     &               invTk*3670.7_r8-                                   &
     &               logTk*9.7944_r8+                                   &
     &               S(i)*(0.0118_r8-                                   &
     &                     S(i)*0.000116_r8))
        K2=10.0_r8**(-4.777_r8-                                         &
     &               invTk*1394.7_r8+                                   &
     &               S(i)*(0.0184_r8-                                   &
     &                     S(i)*0.000118_r8))
!
!-----------------------------------------------------------------------
!  Compute dissociation constant of boric acid, Kb=[H][BO2]/[HBO2].
!  From Millero (1995; page 669) using data from Dickson (1990).
!-----------------------------------------------------------------------
!
        Kb=EXP(-invTk*(8966.90_r8+                                      &
     &                 sqrtS*(2890.53_r8+                               &
     &                        sqrtS*(77.942_r8-                         &
     &                               sqrtS*(1.728_r8-                   &
     &                                      sqrtS*0.0996_r8))))-        &
     &         logTk*(24.4344_r8+                                       &
     &                sqrtS*(25.085_r8+                                 &
     &                       sqrtS*0.2474_r8))+                         &
     &         Tk*(sqrtS*0.053105_r8)+                                  &
     &         148.0248_r8+                                             &
     &         sqrtS*(137.1942_r8+                                      &
     &                sqrtS*1.62142_r8))
!
!-----------------------------------------------------------------------
!  Compute ion product of whater, Kw = [H][OH].
!  From Millero (1995; page 670) using composite data.
!-----------------------------------------------------------------------
!
        Kw=EXP(148.9652_r8-                                             &
     &         invTk*13847.26_r8-                                       &
     &         logTk*23.6521_r8-                                        &
     &         sqrtS*(5.977_r8-                                         &
     &                invTk*118.67_r8-                                  &
     &                logTk*1.0495_r8)-                                 &
     &         S(i)*0.01615_r8)
!
!-----------------------------------------------------------------------
! Calculate concentrations for borate (Uppstrom, 1974).
!-----------------------------------------------------------------------
!
        borate=0.000232_r8*scl/10.811_r8
!
!=======================================================================
!  Iteratively solver for computing hydrogen ions [H+] using either:
!
!    (1) Newton-Raphson method with fixed number of iterations,
!        use previous [H+] as first guess, or
!    (2) bracket and bisection
!=======================================================================
!
!  Solve for h in fifth-order polynomial. First calculate
!  polynomial coefficients.
!
        K12 = K1*K2

        p5 = -1.0_r8;
        p4 = -alk-Kb-K1;
        p3 = dic*K1-alk*(Kb+K1)+Kb*borate+Kw-Kb*K1-K12
        p2 = dic*(Kb*K1+2*K12)-alk*(Kb*K1+K12)+Kb*borate*K1             &
     &       +(Kw*Kb+Kw*K1-Kb*K12)
        p1 = 2.0_r8*dic*Kb*K12-alk*Kb*K12+Kb*borate*K12                 &
     &       +Kw*Kb*K1+Kw*K12
        p0 = Kw*Kb*K12;
!
!  Set first guess and brackets for [H+] solvers.
!
        pH_guess=pH(i,j)         ! Newton-Raphson
        pH_hi=10.0_r8            ! high bracket/bisection
        pH_lo=5.0_r8             ! low bracket/bisection
!
!  Convert to [H+].
!
        X_guess=10.0_r8**(-pH_guess)
        X_lo=10.0_r8**(-pH_hi)
        X_hi=10.0_r8**(-pH_lo)
        X_mid=0.5_r8*(X_lo+X_hi)
!
!-----------------------------------------------------------------------
!  Newton-Raphson method.
!-----------------------------------------------------------------------
!
        IF (DoNewton.eq.1) THEN
          X=X_guess
!
          DO Inewton=1,InewtonMax
!
!  Evaluate f([H+]) = p5*x^5+...+p1*x+p0
!
            fn=((((p5*X+p4)*X+p3)*X+p2)*X+p1)*X+p0
!
!  Evaluate derivative, df([H+])/dx:
!
!     df= d(fn)/d(X)
!
            df=(((5*p5*X+4*p4)*X+3*p3)*X+2*p2)*X+p1
!
!  Evaluate increment in [H+].
!
            deltaX=-fn/df
!
!  Update estimate of [H+].
!
            X=X+deltaX
          END DO
!
!-----------------------------------------------------------------------
!  Bracket and bisection method.
!-----------------------------------------------------------------------
!
        ELSE
!
!  If first step, use Bracket and Bisection method with fixed, large
!  number of iterations
!
          BRACK_IT: DO Ibrack=1,IbrackMax
            DO Hstep=1,3
              IF (Hstep.eq.1) X=X_hi
              IF (Hstep.eq.2) X=X_lo
              IF (Hstep.eq.3) X=X_mid
!
!  Evaluate f([H+]) for bracketing and mid-value cases.
!
              fni(Hstep)=((((p5*X+p4)*X+p3)*X+p2)*X+p1)*X+p0
            END DO
!
!  Now, bracket solution within two of three.
!
            IF (fni(3).eq.0) THEN
               EXIT BRACK_IT
            ELSE
               ftest=fni(1)/fni(3)
               IF (ftest.gt.0) THEN
                 X_hi=X_mid
               ELSE
                 X_lo=X_mid
               END IF
               X_mid=0.5_r8*(X_lo+X_hi)
            END IF
          END DO BRACK_IT
!
! Last iteration gives value.
!
          X=X_mid
        END IF
!
!-----------------------------------------------------------------------
!  Determine pCO2.
!-----------------------------------------------------------------------
!
!  Total Hydrogen ion concentration, Htotal = [H+].
!
        Htotal=X
        Htotal2=Htotal*Htotal
!
!  Calculate [CO2*] (mole/m3) as defined in DOE Methods Handbook 1994
!  Version 2, ORNL/CDIAC-74, Dickson and Goyet, Eds. (Chapter 2,
!  page 10, Eq A.49).
!
        CO2star=dic*Htotal2/(Htotal2+K1*Htotal+K1*K2)
!
!  Save pH is used again outside this routine.
!
        pH(i,j)=-LOG10(Htotal)
!
!  Add two output arguments for storing pCO2surf.
!
        pCO2(i)=CO2star*1000000.0_r8/ff

#  ifdef MASKING
      ELSE
        pH(i,j)=0.0_r8
        pCO2(i)=0.0_r8
      END IF
#  endif

      END DO I_LOOP
!
      RETURN
      END SUBROUTINE pCO2_water_RZ
# else
      SUBROUTINE pCO2_water (Istr, Iend,                                &
     &                       LBi, UBi, LBj, UBj,                        &
     &                       IminS, ImaxS, j, DoNewton,                 &
#  ifdef MASKING
     &                       rmask,                                     &
#  endif
     &                       T, S, TIC, TAlk, PO4b, SiO3, pH, pCO2)
!
!***********************************************************************
!                                                                      !
!  This routine computes equilibrium partial pressure of CO2 (pCO2)    !
!  in the surface seawater.                                            !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     Istr       Starting tile index in the I-direction.               !
!     Iend       Ending   tile index in the I-direction.               !
!     LBi        I-dimension lower bound.                              !
!     UBi        I-dimension upper bound.                              !
!     LBj        J-dimension lower bound.                              !
!     UBj        J-dimension upper bound.                              !
!     IminS      I-dimension lower bound for private arrays.           !
!     ImaxS      I-dimension upper bound for private arrays.           !
!     j          j-pipelined index.                                    !
!     DoNewton   Iteration solver:                                     !
!                  [0] Bracket and bisection.                          !
!                  [1] Newton-Raphson method.                          !
!     rmask      Land/Sea masking.                                     !
!     T          Surface temperature (Celsius).                        !
!     S          Surface salinity (PSS).                               !
!     TIC        Total inorganic carbon (millimol/m3).                 !
!     TAlk       Total alkalinity (milli-equivalents/m3).              !
!     PO4b        Inorganic phosphate (millimol/m3).                   !
!     SiO3       Inorganic silicate (millimol/m3).                     !
!     pH         Best pH guess.                                        !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     pCO2       partial pressure of CO2 (ppmv).                       !
!                                                                      !
!  Check Value:  (T=24, S=36.6, TIC=2040, TAlk=2390, PO4b=0,           !
!                 SiO3=0, pH=8)                                        !
!                                                                      !
!                pcO2=0.35074945E+03 ppmv  (DoNewton=0)                !
!                pCO2=0.35073560E+03 ppmv  (DoNewton=1)                !
!                                                                      !
!  This subroutine was adapted by Mick Follows (Oct 1999) from OCMIP2  !
!  code CO2CALC. Modified for ROMS by Hernan Arango (Nov 2003).        !
!                                                                      !
!***********************************************************************
!
      USE mod_kinds
!
      implicit none
!
!  Imported variable declarations.
!
      integer,  intent(in) :: LBi, UBi, LBj, UBj, IminS, ImaxS
      integer,  intent(in) :: Istr, Iend, j, DoNewton
!
#  ifdef ASSUMED_SHAPE
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: T(IminS:)
      real(r8), intent(in) :: S(IminS:)
      real(r8), intent(in) :: TIC(IminS:)
      real(r8), intent(in) :: TAlk(IminS:)
      real(r8), intent(inout) :: pH(LBi:,LBj:)
#  else
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: T(IminS:ImaxS)
      real(r8), intent(in) :: S(IminS:ImaxS)
      real(r8), intent(in) :: TIC(IminS:ImaxS)
      real(r8), intent(in) :: TAlk(IminS:ImaxS)
      real(r8), intent(inout) :: pH(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: PO4b
      real(r8), intent(in) :: SiO3

      real(r8), intent(out) :: pCO2(IminS:ImaxS)
!
!  Local variable declarations.
!
      integer, parameter :: InewtonMax = 10
      integer, parameter :: IbrackMax = 30

      integer :: Hstep, Ibrack, Inewton, i

      real(r8) :: Tk, centiTk, invTk, logTk
      real(r8) :: SO4, scl, sqrtS, sqrtSO4
      real(r8) :: alk, dic, phos, sili
      real(r8) :: borate, sulfate, fluoride
      real(r8) :: ff, K1, K2, K1p, K2p, K3p, Kb, Kf, Ks, Ksi, Kw
      real(r8) :: K12, K12p, K123p, invKb, invKs, invKsi
      real(r8) :: A, A2, B, B2, C, dA, dB
      real(r8) :: df, fn, fni(3), ftest
      real(r8) :: deltaX, invX, invX2, X, X2, X3
      real(r8) :: pH_guess, pH_hi, pH_lo
      real(r8) :: X_guess, X_hi, X_lo, X_mid
      real(r8) :: CO2star, Htotal, Htotal2
!
!=======================================================================
!  Determine coefficients for surface carbon chemisty.  If land/sea
!  masking, compute only on water points.
!=======================================================================
!
      I_LOOP: DO i=Istr,Iend
#  ifdef MASKING
        IF (rmask(i,j).gt.0.0_r8) THEN
#  endif
        Tk=T(i)+273.15_r8
        centiTk=0.01_r8*Tk
        invTk=1.0_r8/Tk
        logTk=LOG(Tk)
        sqrtS=SQRT(S(i))
        SO4=19.924_r8*S(i)/(1000.0_r8-1.005_r8*S(i))
        sqrtSO4=SQRT(SO4)
        scl=S(i)/1.80655_r8

        alk=TAlk(i)*0.000001_r8
        dic=TIC(i)*0.000001_r8
        phos=PO4b*0.000001_r8
        sili=SiO3*0.000001_r8
!
!-----------------------------------------------------------------------
!  Correction term for non-ideality, ff=k0*(1-pH2O). Equation 13 with
!  table 6 values from Weiss and Price (1980, Mar. Chem., 8, 347-359).
!-----------------------------------------------------------------------
!
        ff=EXP(-162.8301_r8+                                            &
     &         218.2968_r8/centiTk+                                     &
     &         LOG(centiTk)*90.9241_r8-                                 &
     &         centiTk*centiTk*1.47696_r8+                              &
     &         S(i)*(0.025695_r8-                                       &
     &               centiTk*(0.025225_r8-                              &
     &                        centiTk*0.0049867_r8)))
!
!-----------------------------------------------------------------------
!  Compute first (K1) and second (K2) dissociation constant of carboinic
!  acid:
!
!           K1 = [H][HCO3]/[H2CO3]
!           K2 = [H][CO3]/[HCO3]
!
!  From Millero (1995; page 664) using Mehrbach et al. (1973) data on
!  seawater scale.
!-----------------------------------------------------------------------
!
        K1=10.0_r8**(62.008_r8-                                         &
     &               invTk*3670.7_r8-                                   &
     &               logTk*9.7944_r8+                                   &
     &               S(i)*(0.0118_r8-                                   &
     &                     S(i)*0.000116_r8))
        K2=10.0_r8**(-4.777_r8-                                         &
     &               invTk*1394.7_r8+                                   &
     &               S(i)*(0.0184_r8-                                   &
     &                     S(i)*0.000118_r8))
!
!-----------------------------------------------------------------------
!  Compute dissociation constant of boric acid, Kb=[H][BO2]/[HBO2].
!  From Millero (1995; page 669) using data from Dickson (1990).
!-----------------------------------------------------------------------
!
        Kb=EXP(-invTk*(8966.90_r8+                                      &
     &                 sqrtS*(2890.53_r8+                               &
     &                        sqrtS*(77.942_r8-                         &
     &                               sqrtS*(1.728_r8-                   &
     &                                      sqrtS*0.0996_r8))))-        &
     &         logTk*(24.4344_r8+                                       &
     &                sqrtS*(25.085_r8+                                 &
     &                       sqrtS*0.2474_r8))+                         &
     &         Tk*(sqrtS*0.053105_r8)+                                  &
     &         148.0248_r8+                                             &
     &         sqrtS*(137.1942_r8+                                      &
     &                sqrtS*1.62142_r8))
!
!-----------------------------------------------------------------------
!  Compute first (K1p), second (K2p), and third (K3p) dissociation
!  constant of phosphoric acid:
!
!           K1p = [H][H2PO4]/[H3PO4]
!           K2p = [H][HPO4]/[H2PO4]
!           K3p = [H][PO4]/[HPO4]
!
!  From DOE (1994) equations 7.2.20, 7.2.23, and 7.2.26, respectively.
!  With footnote using data from Millero (1974).
!-----------------------------------------------------------------------
!
        K1p=EXP(115.525_r8-                                             &
     &          invTk*4576.752_r8-                                      &
     &          logTk*18.453_r8+                                        &
     &          sqrtS*(0.69171_r8-invTk*106.736_r8)-                    &
     &          S(i)*(0.01844_r8+invTk*0.65643_r8))
        K2p=EXP(172.0883_r8-                                            &
     &          invTk*8814.715_r8-                                      &
     &          logTk*27.927_r8+                                        &
     &          sqrtS*(1.3566_r8-invTk*160.340_r8)-                     &
     &          S(i)*(0.05778_r8-invTk*0.37335_r8))
        K3p=EXP(-18.141_r8-                                             &
     &          invTk*3070.75_r8+                                       &
     &          sqrtS*(2.81197_r8+invTk*17.27039_r8)-                   &
     &          S(i)*(0.09984_r8+invTk*44.99486_r8))
!
!-----------------------------------------------------------------------
!  Compute dissociation constant of silica, Ksi=[H][SiO(OH)3]/[Si(OH)4].
!  From Millero (1995; page 671) using data from Yao and Millero (1995).
!-----------------------------------------------------------------------
!
        Ksi=EXP(117.385_r8-                                             &
     &          invTk*8904.2_r8-                                        &
     &          logTk*19.334_r8+                                        &
     &          sqrtSO4*(3.5913_r8-invTk*458.79_r8)-                    &
     &          SO4*(1.5998_r8-invTk*188.74_r8-                         &
     &               SO4*(0.07871_r8-invTk*12.1652_r8))+                &
     &          LOG(1.0_r8-0.001005_r8*S(i)))
!
!-----------------------------------------------------------------------
!  Compute ion product of whater, Kw = [H][OH].
!  From Millero (1995; page 670) using composite data.
!-----------------------------------------------------------------------
!
        Kw=EXP(148.9652_r8-                                             &
     &         invTk*13847.26_r8-                                       &
     &         logTk*23.6521_r8-                                        &
     &         sqrtS*(5.977_r8-                                         &
     &                invTk*118.67_r8-                                  &
     &                logTk*1.0495_r8)-                                 &
     &         S(i)*0.01615_r8)
!
!------------------------------------------------------------------------
!  Compute salinity constant of hydrogen sulfate, Ks = [H][SO4]/[HSO4].
!  From Dickson (1990, J. chem. Thermodynamics 22, 113)
!------------------------------------------------------------------------
!
        Ks=EXP(141.328_r8-                                              &
     &         invTk*4276.1_r8-                                         &
     &         logTk*23.093_r8+                                         &
     &         sqrtSO4*(324.57_r8-invTk*13856.0_r8-logTk*47.986_r8-     &
     &                  SO4*invTk*2698.0_r8)-                           &
     &         SO4*(771.54_r8-invTk*35474.0_r8-logTk*114.723_r8-        &
     &              SO4*invTk*1776.0_r8)+                               &
     &         LOG(1.0_r8-0.001005_r8*S(i)))
!
!-----------------------------------------------------------------------
!  Compute stability constant of hydrogen fluorid, Kf = [H][F]/[HF].
!  From Dickson and Riley (1979) -- change pH scale to total.
!-----------------------------------------------------------------------
!
        Kf=EXP(-12.641_r8+                                              &
     &         invTk*1590.2_r8+                                         &
     &         sqrtSO4*1.525_r8+                                        &
     &         LOG(1.0_r8-0.001005_r8*S(i))+                            &
     &         LOG(1.0_r8+0.1400_r8*scl/(96.062_r8*Ks)))
!
!-----------------------------------------------------------------------
! Calculate concentrations for borate (Uppstrom, 1974), sulfate (Morris
! and Riley, 1966), and fluoride (Riley, 1965).
!-----------------------------------------------------------------------
!
        borate=0.000232_r8*scl/10.811_r8
        sulfate=0.14_r8*scl/96.062_r8
        fluoride=0.000067_r8*scl/18.9984_r8
!
!=======================================================================
!  Iteratively solver for computing hydrogen ions [H+] using either:
!
!    (1) Newton-Raphson method with fixed number of iterations,
!        use previous [H+] as first guess, or
!    (2) bracket and bisection
!=======================================================================
!
!  Set first guess and brackets for [H+] solvers.
!
        pH_guess=pH(i,j)         ! Newton-Raphson
        pH_hi=10.0_r8            ! high bracket/bisection
        pH_lo=5.0_r8             ! low bracket/bisection
!
!  Convert to [H+].
!
        X_guess=10.0_r8**(-pH_guess)
        X_lo=10.0_r8**(-pH_hi)
        X_hi=10.0_r8**(-pH_lo)
        X_mid=0.5_r8*(X_lo+X_hi)
!
!-----------------------------------------------------------------------
!  Newton-Raphson method.
!-----------------------------------------------------------------------
!
        IF (DoNewton.eq.1) THEN
          X=X_guess
          K12=K1*K2
          K12p=K1p*K2p
          K123p=K12p*K3p
          invKb=1.0_r8/Kb
          invKs=1.0_r8/Ks
          invKsi=1.0_r8/Ksi
!
          DO Inewton=1,InewtonMax
!
!  Set some common combinations of parameters used in the iterative [H+]
!  solver.
!
            X2=X*X
            X3=X2*X
            invX=1.0_r8/X
            invX2=1.0_r8/X2

            A=X*(K12p+X*(K1p+X))
            B=X*(K1+X)+K12
            C=1.0_r8/(1.0_r8+sulfate*invKs)

            A2=A*A
            B2=B*B
            dA=X*(2.0_r8*K1p+3.0_r8*X)+K12p
            dB=2.0_r8*X+K1
!
!  Evaluate f([H+]):
!
!     fn=HCO3+CO3+borate+OH+HPO4+2*PO4+H3PO4+silicate+Hfree+HSO4+HF-TALK
!
            fn=dic*K1*(X+2.0_r8*K2)/B+                                  &
     &         borate/(1.0_r8+X*invKb)+                                 &
     &         Kw*invX+                                                 &
     &         phos*(K12p*X+2.0_r8*K123p-X3)/A+                         &
     &         sili/(1.0_r8+X*invKsi)-                                  &
     &         X*C-                                                     &
     &         sulfate/(1.0_r8+Ks*invX*C)-                              &
     &         fluoride/(1.0_r8+Kf*invX)-                               &
     &         alk
!
!  Evaluate derivative, f(prime)([H+]):
!
!     df= d(fn)/d(X)
!
            df=dic*K1*(B-dB*(X+2.0_r8*K2))/B2-                          &
     &         borate/(invKb*(1.0+X*invKb)**2)-                         &
     &         Kw*invX2+                                                &
     &         phos*(A*(K12p-3.0_r8*X2)-dA*(K12p*X+2.0_r8*K123p-X3))/A2-&
     &         sili/(invKsi*(1.0_r8+X*invKsi)**2)+                      &
     &         C+                                                       &
     &         sulfate*Ks*C*invX2/((1.0_r8+Ks*invX*C)**2)+              &
     &         fluoride*Kf*invX2/((1.0_r8+Kf*invX)**2)
!
!  Evaluate increment in [H+].
!
            deltaX=-fn/df
!
!  Update estimate of [H+].
!
            X=X+deltaX
          END DO
!
!-----------------------------------------------------------------------
!  Bracket and bisection method.
!-----------------------------------------------------------------------
!
        ELSE
!
!  If first step, use Bracket and Bisection method with fixed, large
!  number of iterations
!
          K12=K1*K2
          K12p=K1p*K2p
          K123p=K12p*K3p
          invKb=1.0_r8/Kb
          invKs=1.0_r8/Ks
          invKsi=1.0_r8/Ksi
!
          BRACK_IT: DO Ibrack=1,IbrackMax
            DO Hstep=1,3
              IF (Hstep.eq.1) X=X_hi
              IF (Hstep.eq.2) X=X_lo
              IF (Hstep.eq.3) X=X_mid
!
!  Set some common combinations of parameters used in the iterative [H+]
!  solver.
!
              X2=X*X
              X3=X2*X
              invX=1.0_r8/X

              A=X*(K12p+X*(K1p+X))+K123p
              B=X*(K1+X)+K12
              C=1.0_r8/(1.0_r8+sulfate*invKs)

              A2=A*A
              B2=B*B
              dA=X*(K1p*2.0_r8+3.0_r8*X2)+K12p
              dB=2.0_r8*X+K1
!
!  Evaluate f([H+]) for bracketing and mid-value cases.
!
              fni(Hstep)=dic*(K1*X+2.0_r8*K12)/B+                       &
     &                   borate/(1.0_r8+X*invKb)+                       &
     &                   Kw*invX+                                       &
     &                   phos*(K12p*X+2.0_r8*K123p-X3)/A+               &
     &                   sili/(1.0_r8+X*invKsi)-                        &
     &                   X*C-                                           &
     &                   sulfate/(1.0_r8+Ks*invX*C)-                    &
     &                   fluoride/(1.0_r8+Kf*invX)-                     &
     &                   alk
            END DO
!
!  Now, bracket solution within two of three.
!
            IF (fni(3).eq.0.0_r8) THEN
              EXIT BRACK_IT
            ELSE
              ftest=fni(1)/fni(3)
              IF (ftest.gt.0.0) THEN
                X_hi=X_mid
              ELSE
                X_lo=X_mid
              END IF
              X_mid=0.5_r8*(X_lo+X_hi)
            END IF
          END DO BRACK_IT
!
! Last iteration gives value.
!
          X=X_mid
        END IF
!
!-----------------------------------------------------------------------
!  Determine pCO2.
!-----------------------------------------------------------------------
!
!  Total Hydrogen ion concentration, Htotal = [H+].
!
        Htotal=X
        Htotal2=Htotal*Htotal
!
!  Calculate [CO2*] (mole/m3) as defined in DOE Methods Handbook 1994
!  Version 2, ORNL/CDIAC-74, Dickson and Goyet, Eds. (Chapter 2,
!  page 10, Eq A.49).
!
        CO2star=dic*Htotal2/(Htotal2+K1*Htotal+K1*K2)
!
!  Save pH is used again outside this routine.
!
        pH(i,j)=-LOG10(Htotal)
!
!  Add two output arguments for storing pCO2surf.
!
        pCO2(i)=CO2star*1000000.0_r8/ff

#  ifdef MASKING
      ELSE
        pH(i,j)=0.0_r8
        pCO2(i)=0.0_r8
      END IF
#  endif

      END DO I_LOOP
!
      RETURN
      END SUBROUTINE pCO2_water
# endif
#endif
