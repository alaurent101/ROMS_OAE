/*
** svn $Id: upwelling.h 1001 2020-01-10 22:41:16Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for test of continental runoff
** Copied from upwelling.h
**
** Application flag:   P_202103_03
** Input script:       roms_p_202103_03.in
*/

#define UV_ADV
#define UV_COR
/* Changed bottom drag from linear to log 2021/1/18 */
#define UV_LOGDRAG
#define UV_VIS2
/* Changed mixing from s-layer to geopotential 2021/1/18 */
#define MIX_GEO_UV
#undef  MIX_S_UV
#define SPLINES_VDIFF
#define SPLINES_VVISC
#define DJ_GRADPS
#define TS_DIF2
#undef  TS_DIF4
/* Changed mixing from s-layer to geopotential 2021/1/18 */
#define MIX_GEO_TS
#undef  MIX_S_TS

/* Added nonlinear equation of state 2021/1/18 */
#define NONLIN_EOS
#define SALINITY
#define SOLVE3D
#define AVERAGES
#define DIAGNOSTICS_TS
#define DIAGNOSTICS_UV

/* Undefined analytical grid 2021/1/18 */
#undef  ANA_GRID
/* Undefined analystical initial conditions 2021/2/22 */
#undef ANA_INITIAL
/* Undefined analytical surface momentum flux 2021/1/25 */
#undef  ANA_SMFLUX
/* Undefined analytical surface T and S fluxes 2021/2/22 */
#undef ANA_STFLUX
#undef ANA_SSFLUX
#define ANA_BTFLUX
#define ANA_BSFLUX

/* Added Mellor-Yamada vertical mixing 2021/1/18 */
# define MY25_MIXING

#if defined GLS_MIXING || defined MY25_MIXING
# define KANTHA_CLAYSON
# define N2S2_HORAVG
# define RI_SPLINES
#else
# define ANA_VMIX
#endif

/* Added masking 2021/1/18 */
# define MASKING

/* Added wetting and drying 2021/1/18 */
# define WET_DRY

/* Added tides 2021/1/18 */
# define SSH_TIDES
# define UV_TIDES
# define RAMP_TIDES
# define RADIATION_2D

/* Added addition of tides and boundary input 2021/2/22 */
# define ADD_FSOBC
# define ADD_M2OBC

/* Added bulk fluxes and related flags 2021/2/22 */
/* SHORTWAVE will be defined in globaldefs.h because BULK_FLUXES is defined */
/* CLOUDS will be defined in globaldefs.h because LONGWAVE is defined */
# define BULK_FLUXES
# define COOL_SKIN
# define LONGWAVE
# define EMINUSP
# define WIND_MINUS_CURRENT
# define SOLAR_SOURCE
# define SPECIFIC_HUMIDITY

/* Added atmospheric pressure & compensation 2021/2/22 */
# define ATM_PRESS
# define PRESS_COMPENSATE

/* Added suppression of cooling below freezing point 2021/2/22 */
# define LIMIT_STFLX_COOLING

/* NUDGE_BDRY added 2020/11/23 */
/* LnudgeM3 in the input file should be changed to T */
# undef NUDGE_BDRY

/* Added Ji et al. (2012) river scheme 2021/2/15 */
/* LuvSrc and LtracerSrc in the input file should be changed to T */
# define RIVER_JI

/* Added semi-prognostic method 2021/3/2 */
/* BETA_SPROG & INT_SPROG in the input file should be modified as needed */
/* LtracerCLM for T & S in the input file should be changed to T */
# undef SEMIPROG

/* Added spectral nudging 2021/3/8 */
/* NSPCNDG & DKAPPA in the input file should be modified as needed */
/* LtracerCLM & LnudgeTCLM for T & S in the input file should be changed to T */
# undef SPCNDG

/* Added continental runoff (from Kate Hedstrom's version) 2021/3/23 */
# define RUNOFF
# define RUNOFF_SSH

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

#ifdef PERFECT_RESTART
# undef  AVERAGES
# undef  DIAGNOSTICS_BIO
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# define OUT_DOUBLE
#endif