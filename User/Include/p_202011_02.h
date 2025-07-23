/*
** svn $Id: upwelling.h 778 2015-08-22 01:20:04Z arango $
*******************************************************************************
** Copyright (c) 2002-2015 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for prognostic, no-river run
**
** Application flag:   P_202011_01
** Input script:       roms_p_202011_01.in
*/

#define UV_ADV
#define UV_COR
/* Changed bottom drag from linear 2016/10/21 */
#define UV_LOGDRAG
/* #define UV_LDRAG */
#define UV_VIS2
/* Changed mixing 2016/10/24 */
#define  MIX_GEO_UV
/* #define MIX_S_UV */
#define SPLINES_VDIFF
#define SPLINES_VVISC
#define DJ_GRADPS
#define TS_DIF2
/* Changed mixing 2017/5/15 */
#define  MIX_GEO_TS
/* #define MIX_S_TS */

/* Added nonlinear equation of state 2016/10/21 */
#define NONLIN_EOS
#define SALINITY
#define SOLVE3D
#define AVERAGES
#define DIAGNOSTICS_TS
#define DIAGNOSTICS_UV

#define ANA_BTFLUX
#define ANA_BSFLUX

/* Added 2016/8/10 */
# define MY25_MIXING

#if defined GLS_MIXING || defined MY25_MIXING
# define KANTHA_CLAYSON
# define N2S2_HORAVG
# define RI_SPLINES
#else
# define ANA_VMIX
#endif

#if defined BIO_FENNEL  || defined ECOSIM || \
    defined NPZD_POWELL || defined NEMURO
# define ANA_BIOLOGY
# define ANA_SPFLUX
# define ANA_BPFLUX
# define ANA_SRFLUX
#endif

#if defined NEMURO
# define HOLLING_GRAZING
# undef  IVLEV_EXPLICIT
#endif

#ifdef BIO_FENNEL
# define CARBON
# define DENITRIFICATION
# define BIO_SEDIMENT
# define DIAGNOSTICS_BIO
#endif

/* PERFECT_RESTART added 2017/5/1 */
# undef PERFECT_RESTART

#ifdef PERFECT_RESTART
/* Commented out next line 2017/5 1 */
/* # undef  AVERAGES */
# undef  DIAGNOSTICS_BIO
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# define OUT_DOUBLE
#endif

/* Added 2016/5/20 */
# define MASKING

/* Added 2016/8/9 */
/* (copied from benchmark.h and modified) */
# define BULK_FLUXES
# ifdef BULK_FLUXES
/* SNOWFALL moved here 2019/11/20 */
#  define SNOWFALL
#  undef LONGWAVE
#  define LONGWAVE_OUT
#  define SOLAR_SOURCE /* solar radiation source term */
#  define EMINUSP /* turn ON internal calculation of E-P */
/* Added next 2 lines 2017/2/24 */
#  undef ALBEDO_CURVE /* for water */ 
#  define ALBEDO_CLOUD
# endif
# define ATM_PRESS /* impose atmospheric sea-level pressure onto sea surface */
# define SHORTWAVE /* shortwave radiation */
# define CLOUDS /* define cloud fraction */
/* Added 2020/3/6 */
# define COOL_SKIN /* cool skin correction */

/* Added 2016/9/16 */
# define SSH_TIDES /* impose tidal elevation */
# define UV_TIDES /* impose tidal currents */
# define RAMP_TIDES /* ramp tidal forcing from zero over one day */
# define ADD_FSOBC /* add tidal elevation to processed OBC data */
# define ADD_M2OBC /* add tidal currents to processed OBC data */ 
# define RADIATION_2D /* tangential phase speed in radiation conditions */

/* Added 2016/10/25 */
# define WET_DRY

/* Added 2017/2/9, undefined 2020/6/8 */
# undef STATIONS

/* Added 2017/2/24 */
/* Undefined ICE_MODEL 2017/5/15 */
# ifdef SOLVE3D
#  undef ICE_MODEL
#  ifdef ICE_MODEL
#   define SNOWFALL
#   define ANA_ICE
#   define ICE_THERMO
#   define ICE_MK
#   define ICE_MOMENTUM
#   define ICE_MOM_BULK
#   define ICE_EVP
#   define ICE_I_O
#   define ICE_SHOREFAST
#   define ICE_ADVECT
#   define ICE_SMOLAR
#   define ICE_UPWIND
#   define ICE_BULK_FLUXES
#   define ICE_CONVSNOW
#   define ICE_BASIN
#   define ICE_STRENGTH_QUAD
/* Using OUTFLOW_MASK causes a segmentation fault */
#   undef OUTFLOW_MASK
#   define INI_GLORYS_ICE
#  endif
# endif

/* Semiprognostic method added 2016/3/27, undefined 2020/6/8 */
# undef SEMIPROG

/* Spectral nudging added 2018/3/29, undefined 2020/6/8 */
# undef SPCNDG

/* CICE added 2018/4/3, undefined 2020/6/8 */
# undef CICE_MODEL
# ifdef CICE_MODEL
/* Option for turning ice bc on/off added 2019/3/11 */
#  define ICE_BC
# endif

/* SPECIFIC_HUMIDITY added 2019/6/4 */
# define SPECIFIC_HUMIDITY

/* FLOATS added 2019/6/6, undefined 2020/6/8 */
# undef FLOATS

/* NUDGE_BDRY added 2020/11/23 */
/* LnudgeM3 should be changed from F to T in the input file */
# define NUDGE_BDRY
