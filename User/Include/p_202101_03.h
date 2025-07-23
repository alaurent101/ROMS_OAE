/*
** svn $Id: upwelling.h 1001 2020-01-10 22:41:16Z arango $
*******************************************************************************
** Copyright (c) 2002-2020 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for run using homogeneous ocean, forced by tides, winds, SLP
** Copied from upwelling.h
**
** Application flag:   P_202101_03
** Input script:       roms_p_202101_03.in
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
#define ANA_INITIAL
/* Undefined analytical surface momentum flux 2021/1/25 */
#undef  ANA_SMFLUX
#define ANA_STFLUX
#define ANA_SSFLUX
#define ANA_BTFLUX
#define ANA_BSFLUX
/* Added analytical shortwave radiation flux 2021/1/18 */
#define ANA_SRFLUX
/* Added analytical open boundary conditions 2021/1/18 */
#define ANA_FSOBC
#define ANA_M2OBC
#define ANA_M3OBC
#define ANA_TOBC

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

/* Added winds & SLP 2021/1/25 */
# define BULK_FLUX_WIND

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
