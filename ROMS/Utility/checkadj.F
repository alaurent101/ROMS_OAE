#include "cppdefs.h"
      SUBROUTINE checkadj
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine checks activated C-preprocessing options for        !
!  consistency with all available algorithms.                          !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
      USE mod_strings
!
      USE strings_mod, ONLY : uppercase
!
      implicit none
!
!  Local variable declarations.
!
      integer :: ic = 0
      integer :: ifound

      character (len=40) :: string
!
!-----------------------------------------------------------------------
!  Report issues with various C-preprocessing options.
!-----------------------------------------------------------------------
!
#ifdef BIO_FASHAM
      ic=ic+1
      string=uppercase('bio_fasham')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &            'CPP option renamed to '//uppercase('bio_fennel')
#endif

      string=uppercase('ts_smagorinsky')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        IF (Master) WRITE(stdout,10) TRIM(string),                      &
     &                               'stability problems, WARNING'
      END IF

      string=uppercase('split_u3')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        IF (Master) WRITE(stdout,10) TRIM(string),                      &
     &                               'stability problems, WARNING'
      END IF

      string=uppercase('uv_smagorinsky')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        IF (Master) WRITE(stdout,10) TRIM(string),                      &
     &                               'stability problems, WARNING'
      END IF

      string=uppercase('uv_u3adv_split')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        IF (Master) WRITE(stdout,10) TRIM(string),                      &
     &                               'stability problems, WARNING'
      END IF

#ifdef IS4DVAR
      string=uppercase('is4dvar')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &                             'deprecated option, use ' //         &
     &                             uppercase('i4dvar') //               &
     &                             ' instead, WARNING'
#endif

#ifdef IS4DVAR_SENSITIVITY
      string=uppercase('is4dvar_sensitivity')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &                             'deprecated option, use ' //         &
     &                         uppercase('i4dvar_ana_sensitivity') //   &
     &                             ' instead, WARNING'
#endif

#ifdef W4DPSAS
      string=uppercase('w4dpsas')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &                             'deprecated option, use ' //         &
     &                             uppercase('rbl4dvar') //             &
     &                             ' instead, WARNING'
#endif

#ifdef W4DPSAS_SENSITIVITY
      string=uppercase('w4dpsas_sensitivity')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &                             'deprecated option, use ' //         &
     &                         uppercase('rbl4dvar_ana_sensitivity') // &
     &                             ' instead, WARNING'
#endif

#ifdef W4DPSAS_FCT_SENSITIVITY
      string=uppercase('w4dpsas_fct_sensitivity')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &                             'deprecated option, use ' //         &
     &                         uppercase('rbl4dvar_fct_sensitivity') // &
     &                             ' instead, WARNING'
#endif

#ifdef W4DVAR
      string=uppercase('w4dvar')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &                             'deprecated option, use ' //         &
     &                             uppercase('r4dvar') //               &
     &                             ' instead, WARNING'
#endif

#ifdef W4DVAR_SENSITIVITY
      string=uppercase('w4dvar_sensitivity')
      IF (Master) WRITE(stdout,10) TRIM(string),                        &
     &                             'deprecated option, use ' //         &
     &                         uppercase('r4dvar_ana_sensitivity') //   &
     &                             ' instead, WARNING'
#endif

# ifdef NL_BULK_FLUXES
      string=uppercase('nl_bulk_fluxes')
      ic=ic+1
      IF (Master) WRITE(stdout,20) TRIM(string),                        &
     &                             'deprecated option, used ' //        &
     &                         uppercase('forward_fluxes')//' and ' //  &
     &                         uppercase('prior_bulk_fluxes') //        &
     &                             ' instead, FATAL ERROR'
# endif

#if defined TANGENT || defined TL_IOMS || defined ADJOINT
!
!-----------------------------------------------------------------------
!  Stop if unsupported C-preprocessing options are activated for the
!  adjoint-based algorithms.
!-----------------------------------------------------------------------
!
      string=uppercase('bedload_mpm')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('bedload_soulsby')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('bio_fennel')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF

# ifndef FORWARD_MIXING
!
      string=uppercase('bvf_mixing')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
# endif
!
      string=uppercase('clima_ts_mix')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        IF (Master) WRITE(stdout,10) TRIM(string),                      &
     &                               'not tested, WARNING'
      END IF
!
      IF (ANY(ad_LBC(ieast,isFsur,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(ieast,isFsur)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(iwest,isFsur,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(iwest,isFsur)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(isouth,isFsur,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(isouth,isFsur)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(inorth,isFsur,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(inorth,isFsur)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(ieast,isUbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(ieast,isUbar)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(iwest,isUbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(iwest,isUbar)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(isouth,isUbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(isouth,isUbar)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(inorth,isUbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(inorth,isUbar)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(ieast,isVbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(ieast,isVbar)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(iwest,isVbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(iwest,isVbar)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(isouth,isVbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(isouth,isVbar)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(inorth,isVbar,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(inorth,isVbar)',           &
     &                               'not finished, FATAL ERROR'
      END IF

# ifdef SOLVE3D
!
      IF (ANY(ad_LBC(ieast,isUvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(ieast,isUvel)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(iwest,isUvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(iwest,isUvel)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(isouth,isUvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(isouth,isUvel)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(inorth,isUvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(inorth,isUvel)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(ieast,isVvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(ieast,isVvel)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(iwest,isVvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(iwest,isVvel)',            &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(isouth,isVvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(isouth,isVvel)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(inorth,isVvel,:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(inorth,isVvel)',           &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(ieast,isTvar(:),:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(ieast,isTvar(:))',         &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(iwest,isTvar(:),:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(iwest,isTvar(:))',         &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(isouth,isTvar(:),:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(isouth,isTvar(:))',        &
     &                               'not finished, FATAL ERROR'
      END IF
!
      IF (ANY(ad_LBC(inorth,isTvar(:),:)%radiation)) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) 'ad_LBC(inorth,isTvar(:))',        &
     &                               'not finished, FATAL ERROR'
      END IF
# endif
!
      string=uppercase('ecosim')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('geopotential_hconv')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,10) TRIM(string),                      &
     &                               'experimental, AVOID USAGE'
      END IF

# ifndef FORWARD_MIXING
!
      string=uppercase('gls_mixing')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
!!      ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not differentiable, WARNING'
      END IF
# endif
!
      string=uppercase('hypoxia_srm')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('limit_bstress')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF

# ifndef FORWARD_MIXING
!
      string=uppercase('lmd_mixing')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
!!      ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not differentiable, WARNING'
      END IF
# endif
!
      string=uppercase('mb_bbl')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF

# ifndef FORWARD_MIXING
!
      string=uppercase('my25_mixing')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
!!      ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not differentiable, WARNING'
      END IF
# endif
!
      string=uppercase('nearshore_mellor')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('nemuro')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('nesting')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('npzd_franks')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
!!      ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not working, FATAL ERROR'
      END IF
!
      string=uppercase('pj_gradpq2')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('pj_gradpq4')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('red_tide')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('refdif_coupling')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not allowed, FATAL ERROR'
      END IF
!
      string=uppercase('sediment')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('sed_dens')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not tested, FATAL ERROR'
      END IF
!
      string=uppercase('sed_morph')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('sg_bbl')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('simple_bgc')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
      
      string=uppercase('dissolution')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('ssw_bbl')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('suspload')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('swan_coupling')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not allowed, FATAL ERROR'
      END IF
!
# if defined TANGENT || defined TL_IOMS
      IF (ANY(tl_Hadvection(:,:)%MPDATA).or.                            &
     &    ANY(tl_Vadvection(:,:)%MPDATA)) THEN
        string=uppercase('mpdata')
        ifound=INDEX(TRIM(Coptions), TRIM(string))
        IF (ifound.ne.0) THEN
          ic=ic+1
          IF (Master) WRITE(stdout,20) TRIM(string),                    &
     &                                 'not coded, FATAL ERROR'
        END IF
      END IF
!
      IF (ANY(tl_Hadvection(:,:)%HSIMT).or.                             &
     &    ANY(tl_Vadvection(:,:)%HSIMT)) THEN
        string=uppercase('hsimt')
        ifound=INDEX(TRIM(Coptions), TRIM(string))
        IF (ifound.ne.0) THEN
          ic=ic+1
          IF (Master) WRITE(stdout,20) TRIM(string),                    &
     &                                 'not coded, FATAL ERROR'
        END IF
      END IF
!
      IF (ANY(tl_Hadvection(:,:)%SPLIT_U3).or.                          &
     &    ANY(tl_Vadvection(:,:)%SPLIT_U3)) THEN
        string=uppercase('split_u3')
        ifound=INDEX(TRIM(Coptions), TRIM(string))
        IF (ifound.ne.0) THEN
          ic=ic+1
          IF (Master) WRITE(stdout,20) TRIM(string),                    &
     &                                 'not coded, FATAL ERROR'
        END IF
      END IF
# endif
!
      string=uppercase('ts_smagorinsky')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('uv_smagorinsky')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('uv_u3adv_split')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('wet_dry')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20)  TRIM(string),                     &
     &                                'not coded, FATAL ERROR'
      END IF
!
      string=uppercase('wrf_coupling')
      ifound=INDEX(TRIM(Coptions), TRIM(string))
      IF (ifound.ne.0) THEN
        ic=ic+1
        IF (Master) WRITE(stdout,20) TRIM(string),                      &
     &                               'not coded, FATAL ERROR'
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Set execution error flag to stop execution.
!-----------------------------------------------------------------------
!
      IF (ic.gt.0) THEN
        exit_flag=5
      END IF
!
 10   FORMAT (/,' CHECKADJ - use caution when activating: ', a,/,12x,   &
     &        'REASON: ',a,'.')
#if defined TANGENT || defined TL_IOMS || defined ADJOINT
 20   FORMAT (/,' CHECKADJ - unsupported option in adjoint-based',      &
     &        ' algorithms: ',a,/,12x,'REASON: ',a,'.')
#endif
!
      RETURN
      END SUBROUTINE checkadj
