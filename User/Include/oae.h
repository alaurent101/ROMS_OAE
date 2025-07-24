/*
** svn $Id$
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for prognositc run
** Copied from nwa12.h and added notes 2021/9/28
**
** Application flag:   CNS1
** Input script:       roms_cns1_***.in
*/

/* Option for NCDF4 output */
#define HDF5

/* Options associated with momentum equations */
#define UV_ADV         /* activate advection terms */
#define UV_COR         /* activate Coriolis term */
#define UV_VIS2        /* harmonic horizontal mixing */
#define UV_LOGDRAG     /* activate logarithmic bottom friction */
#define SPLINES_VVISC  /* splines reconstruction of vertical viscosity */

/* Options associated with tracers equations */
#define TS_DIF2        /* turn ON harmonic horizontal mixing */
#define NONLIN_EOS     /* use nonlinear equation of state */
#define SALINITY       /* have salinity */
#define SOLAR_SOURCE   /* solar radiation source term */
#define SPLINES_VDIFF  /* splines reconstruction of vertical diffusion */

/* Option to suppress further surface cooling */
#undef LIMIT_STFLX_COOLING /* suppress SST cooling below freezing point */

/* Options for pressure gradient algorithm */
#define DJ_GRADPS /* splines density Jacobian (Shchepetkin, 2000) */
#define ATM_PRESS /* impose atmospheric pressure onto sea surface */

/* Options for atmospheric boundary layer surface fluxes */
#define BULK_FLUXES       /* bulk fluxes computation */
#define COOL_SKIN         /* cool skin correction */
#define LONGWAVE_OUT      /* compute outgoing longwave radiation */
#define EMINUSP           /* compute E-P */
#define SPECIFIC_HUMIDITY /* use specific humidity */

/* Options to adjust surface heat flux */
#undef QCORRECTION
#ifdef QCORRECTION
#  define ANA_DQDSST
#endif

/* #define CLOUDS >>> possibly not needed because LONGWAVE_OUT is defined */
#define SHORTWAVE_IN
#ifdef SHORTWAVE_IN
#  define ALBEDO_LY09
#endif

/* Options for model configuration */
#define SOLVE3D        /* solving 3D primitive equations */
#define MASKING        /* land/sea masking */
#define AVERAGES       /* write out time-averaged data */
#undef DIAGNOSTICS_UV /* write out momentum diagnostics */
#undef DIAGNOSTICS_TS /* write out tracer diagnostics */

/* Options for analytical fields configuration */
#define ANA_BTFLUX /* bottom temperature flux */
#define ANA_BSFLUX /* bottom salinity flux */

/* Options for horizontal mixing of momentum */
#define MIX_GEO_UV /* mixing along geopotential (constant Z) surfaces */

/* Options for horizontal mixing of tracers */
#define MIX_GEO_TS /* mixing along geopotential (constant depth) surfaces */

/* Options for vertical mixing of momentum and tracers */
#undef MY25_MIXING /* Mellor/Yamada Level-2.5 closure */
#undef GLS_MIXING /* GLS turbulent closure */
#define LMD_MIXING

/* Options for the Mellor/Yamada level 2.5 closure */
#if defined GLS_MIXING || defined MY25_MIXING
#  define KANTHA_CLAYSON /* Kantha and Clayson stability function */
#  define N2S2_HORAVG    /* horizontal smoothing of buoyancy/shear */
#  define RI_SPLINES     /* splines reconstruction for vertical sheer */
#endif

/* Options for the LMD_MIXING closure */
#ifdef LMD_MIXING
# define LMD_RIMIX                             /* Add diffusivity due to shear instability */
# define LMD_CONVEC                            /* Add convective mixing due to shear instability */
# define LMD_DDMIX                             /* Add double-diffusive mixing */
# define LMD_SKPP                              /* KPP surface boundary layer mixing */
# define LMD_NONLOCAL                          /* LMD convective nonlocal transport */
# define LMD_SHAPIRO                           /* Shapiro filtering boundary layer depth */
# define LMD_BKPP                              /* KPP bottom boundary layer mixing */
# define RI_SPLINES                            /* Splines reconstruction for Richardson Number */
#endif

/* Options for lateral boundary conditions */
#define RADIATION_2D /* tangential phase speed in radiation conditions */
/* Nudging toward reanalysis data in nudging ("sponge") layers */
/* Set the following in *.in to T to activate nudging: LnudgeTREA for tracers,
   LnudgeM2REA for 2D momentum, LnudgeM3REA for 3D momentum */
#undef NUDGE_BDRY

#undef USE_TIDES
/* Options for tidal forcing at open boundaries */
#if defined USE_TIDES
#  define SSH_TIDES  /* impose tidal elevation */
#  define UV_TIDES   /* impose tidal currents */
/* Warning: Option below is needed for new solution but should be avoided for restarts*/
#  define RAMP_TIDES /* ramping (over one day) tidal forcing */
#  define ADD_FSOBC  /* add tidal elevation to processed open boundary conditions data */
#  define ADD_M2OBC  /* add tidal currents to processed open boundary conditions data */
#endif

/* Added code to keep track of nudging effect */
#ifdef DIAGNOSTICS_TS
# define DIAG_NUDG
#endif

/* Options for the reduced BGC model */
#define REDUCED_BGC
#ifdef REDUCED_BGC
# define TEMP_RATES
# define OXYGEN
# define TALK_BGC
# define P_PRODUCTION
# define RW14_OXYGEN_SC
# define CARBON
# ifdef CARBON
#  define RW14_CO2_SC
#  undef PCO2AIR_MAUNALOA
#  define PCO2AIR_SABLEISLAND
#  define TALK_ADDITION
#  define TALK_FILE
#  define TALK_TRACERS
#  define TALK_NONCONSERV
# endif
# define DIAGNOSTICS_BIO
# define ANA_SPFLUX
# define ANA_BPFLUX
/* Options specific to Halifax Harbour case */
# define PP_SS
# undef PP_HRM23
# define PP_H2
# undef PP_H3
# define WOC_H2
# define SOC_H2
# undef WOC_H3
# undef SOC_H3
# undef WOC_HRM23     /* WOC varies in HRM2 and HRM3 */
# undef SOC_HRM23     /* SOC varies in HRM2 and HRM3 */
# undef WOC_H23     /* WOC varies in H2 and H3 */
# undef SOC_H23     /* SOC varies in H2 and H3 */
# define SOC_ZVAR     /* SOC varies with depth */
# define SOC_OXYDEP   /* O2 dependency on SOC */
#endif

/* Options for nearshore stresses and shallow water configurations */
#define WET_DRY /* activate wetting and drying */

/* Options for NetCDF input and output */
#define PERFECT_RESTART

#ifdef PERFECT_RESTART
#  define OUT_DOUBLE
#endif

/* Turn on nesting*/
#undef NESTING
#undef ONE_WAY

/* MPI communication options */
#define COLLECT_ALLREDUCE
#define REDUCE_ALLGATHER

/* Use spatially varying water type */
#define WTYPE_GRID
