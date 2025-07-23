/*
** svn $Id$
*************************************************** Hernan G. Arango ***
** Copyright (c) 2002-2021 The ROMS/TOMS Group                        **
**   Licensed under a MIT/X style license                             **
**   See License_ROMS.txt                                             **
************************************************************************
**                                                                    **
**  Assigns metadata indices for the iSimple BGC model                **
**  variables that are used in input and output NetCDF files.         **
**  The metadata information is read from "varinfo.dat".              **
**                                                                    **
**  This file is included in file "mod_ncparam.F", routine            **
**  "initialize_ncparm".                                              **
**                                                                    **
************************************************************************
*/

/*
**  Model state biological tracers.
*/

#ifdef CARBON
            CASE ('idTvar(iTIC_)')
              idTvar(iTIC_)=varid
            CASE ('idTvar(iTAlk)')
              idTvar(iTAlk)=varid
# ifdef TALK_ADDITION
            CASE ('idTvar(iParticle1)')
              idTvar(iParticle1)=varid
            CASE ('idTvar(idTIC)')
              idTvar(idTIC)=varid
            CASE ('idTvar(idTA)')
              idTvar(idTA)=varid
#  ifdef TALK_TRACERS
            CASE ('idTvar(iTAin)')
              idTvar(iTAin)=varid
            CASE ('idTvar(iTArm)')
              idTvar(iTArm)=varid
#  endif
# endif
#endif
#ifdef OXYGEN
            CASE ('idTvar(iOxyg)')
              idTvar(iOxyg)=varid
#endif

/*
**  Adjoint sensitivity state biological tracers.
*/

#if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
    defined OPT_OBSERVATIONS || defined SENSITIVITY_4DVAR      || \
    defined SO_SEMI
# ifdef CARBON
            CASE ('idTads(iTIC_)')
              idTads(iTIC_)=varid
            CASE ('idTads(iTAlk)')
              idTads(iTAlk)=varid
#  ifdef TALK_ADDITION
            CASE ('idTads(iParticle1)')
              idTads(iParticle1)=varid
            CASE ('idTads(idTIC)')
              idTads(idTIC)=varid
            CASE ('idTads(idTA')
              idTads(idTA)=varid
#   ifdef TALK_TRACERS
            CASE ('idTads(iTAin)')
              idTads(iTAin)=varid
            CASE ('idTads(iTArm)')
              idTads(iTArm)=varid
#   endif
#  endif
# endif
# ifdef OXYGEN
            CASE ('idTads(iOxyg)')
              idTads(iOxyg)=varid
# endif
#endif

/*
**  Biological tracers open boundary conditions.
*/

#ifdef CARBON
            CASE ('idTbry(iwest,iTIC_)')
              idTbry(iwest,iTIC_)=varid
            CASE ('idTbry(ieast,iTIC_)')
              idTbry(ieast,iTIC_)=varid
            CASE ('idTbry(isouth,iTIC_)')
              idTbry(isouth,iTIC_)=varid
            CASE ('idTbry(inorth,iTIC_)')
              idTbry(inorth,iTIC_)=varid

            CASE ('idTbry(iwest,iTAlk)')
              idTbry(iwest,iTAlk)=varid
            CASE ('idTbry(ieast,iTAlk)')
              idTbry(ieast,iTAlk)=varid
            CASE ('idTbry(isouth,iTAlk)')
              idTbry(isouth,iTAlk)=varid
            CASE ('idTbry(inorth,iTAlk)')
              idTbry(inorth,iTAlk)=varid

# ifdef TALK_ADDITION
            CASE ('idTbry(iwest,iParticle1)')
              idTbry(iwest,iParticle1)=varid
            CASE ('idTbry(ieast,iParticle1)')
              idTbry(ieast,iParticle1)=varid
            CASE ('idTbry(isouth,iParticle1)')
              idTbry(isouth,iParticle1)=varid
            CASE ('idTbry(inorth,iParticle1)')
              idTbry(inorth,iParticle1)=varid

            CASE ('idTbry(iwest,idTIC)')
              idTbry(iwest,idTIC)=varid
            CASE ('idTbry(ieast,idTIC)')
              idTbry(ieast,idTIC)=varid
            CASE ('idTbry(isouth,idTIC)')
              idTbry(isouth,idTIC)=varid
            CASE ('idTbry(inorth,idTIC)')
              idTbry(inorth,idTIC)=varid

            CASE ('idTbry(iwest,idTA)')
              idTbry(iwest,idTA)=varid
            CASE ('idTbry(ieast,idTA)')
              idTbry(ieast,idTA)=varid
            CASE ('idTbry(isouth,idTA)')
              idTbry(isouth,idTA)=varid
            CASE ('idTbry(inorth,idTA)')
              idTbry(inorth,idTA)=varid
#  ifdef TALK_TRACERS

            CASE ('idTbry(iwest,iTAin)')
              idTbry(iwest,iTAin)=varid
            CASE ('idTbry(ieast,iTAin)')
              idTbry(ieast,iTAin)=varid
            CASE ('idTbry(isouth,iTAin)')
              idTbry(isouth,iTAin)=varid
            CASE ('idTbry(inorth,iTAin)')
              idTbry(inorth,iTAin)=varid

            CASE ('idTbry(iwest,iTArm)')
              idTbry(iwest,iTArm)=varid
            CASE ('idTbry(ieast,iTArm)')
              idTbry(ieast,iTArm)=varid
            CASE ('idTbry(isouth,iTArm)')
              idTbry(isouth,iTArm)=varid
            CASE ('idTbry(inorth,iTArm)')
              idTbry(inorth,iTArm)=varid
#  endif
# endif

#endif
#ifdef OXYGEN
            CASE ('idTbry(iwest,iOxyg)')
              idTbry(iwest,iOxyg)=varid
            CASE ('idTbry(ieast,iOxyg)')
              idTbry(ieast,iOxyg)=varid
            CASE ('idTbry(isouth,iOxyg)')
              idTbry(isouth,iOxyg)=varid
            CASE ('idTbry(inorth,iOxyg)')
              idTbry(inorth,iOxyg)=varid
#endif

/*
**  Biological tracers point Source/Sinks (river runoff).
*/

#ifdef CARBON
            CASE ('idRtrc(iTIC_)')
              idRtrc(iTIC_)=varid
            CASE ('idRtrc(iTAlk)')
              idRtrc(iTAlk)=varid
# ifdef TALK_ADDITION
            CASE ('idRtrc(iParticle1)')
              idRtrc(iParticle1)=varid
            CASE ('idRtrc(idTIC)')
              idRtrc(idTIC)=varid
            CASE ('idRtrc(idTA)')
              idRtrc(idTA)=varid
#  ifdef TALK_TRACERS
            CASE ('idRtrc(iTAin)')
              idRtrc(iTAin)=varid
            CASE ('idRtrc(iTArm)')
              idRtrc(iTArm)=varid
#  endif
# endif
#endif
#ifdef OXYGEN
            CASE ('idRtrc(iOxyg)')
              idRtrc(iOxyg)=varid
#endif

#ifdef DIAGNOSTICS_BIO

/*
**  Biological tracers term diagnostics.
*/

# ifdef CARBON
            CASE ('iDbio2(iCfxc)')
              iDbio2(iCfxc)=varid
            CASE ('iDbio2(ipCO2c)')
              iDbio2(ipCO2c)=varid
#  ifdef TALK_ADDITION
            CASE ('iDbio2(iCfxa)')
              iDbio2(iCfxa)=varid
            CASE ('iDbio2(ipCO2a)')
              iDbio2(ipCO2a)=varid
!#  else
!            CASE ('iDbio2(iCOfx)')
!              iDbio2(iCOfx)=varid
!            CASE ('iDbio2(ipCO2)')
!              iDbio2(ipCO2)=varid
#  endif
# endif
# ifdef OXYGEN
            CASE ('iDbio2(iO2fx)')
              iDbio2(iO2fx)=varid
# endif
#endif
