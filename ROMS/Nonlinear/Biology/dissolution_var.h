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

            CASE ('idTvar(iParticle1)')
              idTvar(iParticle1)=varid
            CASE ('idTvar(iDissolve1)')
              idTvar(iDissolve1)=varid
             
#   ifdef FULL_DISSOLVE
            CASE ('idTvar(iDissolve0)')
              idTvar(iDissolve0)=varid
#   endif
#   ifdef MULTI_PARTICLES
            CASE ('idTvar(iParticle2)')
              idTvar(iParticle2)=varid
            CASE ('idTvar(iDissolve2)')
              idTvar(iDissolve2)=varid

            CASE ('idTvar(iParticle3)')
              idTvar(iParticle3)=varid
            CASE ('idTvar(iDissolve3)')
              idTvar(iDissolve3)=varid

            CASE ('idTvar(iParticle4)')
              idTvar(iParticle4)=varid
            CASE ('idTvar(iDissolve4)')
              idTvar(iDissolve4)=varid

            CASE ('idTvar(iParticle5)')
              idTvar(iParticle5)=varid
            CASE ('idTvar(iDissolve5)')
              idTvar(iDissolve5)=varid
            
            CASE ('idTvar(iParticle6)')
              idTvar(iParticle6)=varid
            CASE ('idTvar(iDissolve6)')
              idTvar(iDissolve6)=varid

            CASE ('idTvar(iParticle7)')
              idTvar(iParticle7)=varid
            CASE ('idTvar(iDissolve7)')
              idTvar(iDissolve7)=varid

            CASE ('idTvar(iParticle8)')
              idTvar(iParticle8)=varid
            CASE ('idTvar(iDissolve8)')
              idTvar(iDissolve8)=varid

            CASE ('idTvar(iParticle9)')
              idTvar(iParticle9)=varid
            CASE ('idTvar(iDissolve9)')
              idTvar(iDissolve9)=varid
#   endif
/*
**  Adjoint sensitivity state biological tracers.
*/

#if defined AD_SENSITIVITY   || defined I4DVAR_ANA_SENSITIVITY || \
    defined OPT_OBSERVATIONS || defined SENSITIVITY_4DVAR      || \
    defined SO_SEMI
            CASE ('idTads(iParticle1)')
              idTads(iParticle1)=varid
            CASE ('idTads(iDissolve1)')
              idTads(iDissolve1)=varid

#   ifdef FULL_DISSOLVE
            CASE ('idTads(iDissolve0)')
              idTads(iDissolve0)=varid
#   endif
#   ifdef MULTI_PARTICLES
            CASE ('idTads(iParticle2)')
              idTads(iParticle2)=varid
            CASE ('idTads(iDissolve2)')
              idTads(iDissolve2)=varid
            
            CASE ('idTads(iParticle3)')
              idTads(iParticle3)=varid
            CASE ('idTads(iDissolve3)')
              idTads(iDissolve3)=varid
            
            CASE ('idTads(iParticle4)')
              idTads(iParticle4)=varid
            CASE ('idTads(iDissolve4)')
              idTads(iDissolve4)=varid

            CASE ('idTads(iParticle5)')
              idTads(iParticle5)=varid
            CASE ('idTads(iDissolve5)')
              idTads(iDissolve5)=varid

            CASE ('idTads(iParticle6)')
              idTads(iParticle6)=varid
            CASE ('idTads(iDissolve6)')
              idTads(iDissolve6)=varid
            
            CASE ('idTads(iParticle7)')
              idTads(iParticle7)=varid
            CASE ('idTads(iDissolve7)')
              idTads(iDissolve7)=varid
            
            CASE ('idTads(iParticle8)')
              idTads(iParticle8)=varid
            CASE ('idTads(iDissolve8)')
              idTads(iDissolve8)=varid

            CASE ('idTads(iParticle9)')
              idTads(iParticle9)=varid
            CASE ('idTads(iDissolve9)')
              idTads(iDissolve9)=varid
#   endif
#endif

/*
**  Biological tracers open boundary conditions.
*/

            CASE ('idTbry(iwest,iParticle1)')
              idTbry(iwest,iParticle1)=varid
            CASE ('idTbry(ieast,iParticle1)')
              idTbry(ieast,iParticle1)=varid
            CASE ('idTbry(isouth,iParticle1)')
              idTbry(isouth,iParticle1)=varid
            CASE ('idTbry(inorth,iParticle1)')
              idTbry(inorth,iParticle1)=varid

            CASE ('idTbry(iwest,iDissolve1)')
              idTbry(iwest,iDissolve1)=varid
            CASE ('idTbry(ieast,iDissolve1)')
              idTbry(ieast,iDissolve1)=varid
            CASE ('idTbry(isouth,iDissolve1)')
              idTbry(isouth,iDissolve1)=varid
            CASE ('idTbry(inorth,iDissolve1)')
              idTbry(inorth,iDissolve1)=varid

#   ifdef FULL_DISSOLVE
            CASE ('idTbry(iwest,iDissolve0)')
              idTbry(iwest,iDissolve0)=varid
            CASE ('idTbry(ieast,iDissolve0)')
              idTbry(ieast,iDissolve0)=varid
            CASE ('idTbry(isouth,iDissolve0)')
              idTbry(isouth,iDissolve0)=varid
            CASE ('idTbry(inorth,iDissolve0)')
              idTbry(inorth,iDissolve0)=varid
#   endif
#   ifdef MULTI_PARTICLES
            CASE ('idTbry(iwest,iParticle2)')
              idTbry(iwest,iParticle2)=varid
            CASE ('idTbry(ieast,iParticle2)')
              idTbry(ieast,iParticle2)=varid
            CASE ('idTbry(isouth,iParticle2)')
              idTbry(isouth,iParticle2)=varid
            CASE ('idTbry(inorth,iParticle2)')
              idTbry(inorth,iParticle2)=varid

            CASE ('idTbry(iwest,iDissolve2)')
              idTbry(iwest,iDissolve2)=varid
            CASE ('idTbry(ieast,iDissolve2)')
              idTbry(ieast,iDissolve2)=varid
            CASE ('idTbry(isouth,iDissolve2)')
              idTbry(isouth,iDissolve2)=varid
            CASE ('idTbry(inorth,iDissolve2)')
              idTbry(inorth,iDissolve2)=varid
            
            CASE ('idTbry(iwest,iParticle3)')
              idTbry(iwest,iParticle3)=varid
            CASE ('idTbry(ieast,iParticle3)')
              idTbry(ieast,iParticle3)=varid
            CASE ('idTbry(isouth,iParticle3)')
              idTbry(isouth,iParticle3)=varid
            CASE ('idTbry(inorth,iParticle3)')
              idTbry(inorth,iParticle3)=varid

            CASE ('idTbry(iwest,iDissolve3)')
              idTbry(iwest,iDissolve3)=varid
            CASE ('idTbry(ieast,iDissolve3)')
              idTbry(ieast,iDissolve3)=varid
            CASE ('idTbry(isouth,iDissolve3)')
              idTbry(isouth,iDissolve3)=varid
            CASE ('idTbry(inorth,iDissolve3)')
              idTbry(inorth,iDissolve3)=varid

            CASE ('idTbry(iwest,iParticle4)')
              idTbry(iwest,iParticle4)=varid
            CASE ('idTbry(ieast,iParticle4)')
              idTbry(ieast,iParticle4)=varid
            CASE ('idTbry(isouth,iParticle4)')
              idTbry(isouth,iParticle4)=varid
            CASE ('idTbry(inorth,iParticle4)')
              idTbry(inorth,iParticle4)=varid

            CASE ('idTbry(iwest,iDissolve4)')
              idTbry(iwest,iDissolve4)=varid
            CASE ('idTbry(ieast,iDissolve4)')
              idTbry(ieast,iDissolve4)=varid
            CASE ('idTbry(isouth,iDissolve4)')
              idTbry(isouth,iDissolve4)=varid
            CASE ('idTbry(inorth,iDissolve4)')
              idTbry(inorth,iDissolve4)=varid

            CASE ('idTbry(iwest,iParticle5)')
              idTbry(iwest,iParticle5)=varid
            CASE ('idTbry(ieast,iParticle5)')
              idTbry(ieast,iParticle5)=varid
            CASE ('idTbry(isouth,iParticle5)')
              idTbry(isouth,iParticle5)=varid
            CASE ('idTbry(inorth,iParticle5)')
              idTbry(inorth,iParticle5)=varid

            CASE ('idTbry(iwest,iDissolve5)')
              idTbry(iwest,iDissolve5)=varid
            CASE ('idTbry(ieast,iDissolve5)')
              idTbry(ieast,iDissolve5)=varid
            CASE ('idTbry(isouth,iDissolve5)')
              idTbry(isouth,iDissolve5)=varid
            CASE ('idTbry(inorth,iDissolve5)')
              idTbry(inorth,iDissolve5)=varid

            CASE ('idTbry(iwest,iParticle6)')
              idTbry(iwest,iParticle6)=varid
            CASE ('idTbry(ieast,iParticle6)')
              idTbry(ieast,iParticle6)=varid
            CASE ('idTbry(isouth,iParticle6)')
              idTbry(isouth,iParticle6)=varid
            CASE ('idTbry(inorth,iParticle6)')
              idTbry(inorth,iParticle6)=varid

            CASE ('idTbry(iwest,iDissolve6)')
              idTbry(iwest,iDissolve6)=varid
            CASE ('idTbry(ieast,iDissolve6)')
              idTbry(ieast,iDissolve6)=varid
            CASE ('idTbry(isouth,iDissolve6)')
              idTbry(isouth,iDissolve6)=varid
            CASE ('idTbry(inorth,iDissolve6)')
              idTbry(inorth,iDissolve6)=varid

            CASE ('idTbry(iwest,iParticle7)')
              idTbry(iwest,iParticle7)=varid
            CASE ('idTbry(ieast,iParticle7)')
              idTbry(ieast,iParticle7)=varid
            CASE ('idTbry(isouth,iParticle7)')
              idTbry(isouth,iParticle7)=varid
            CASE ('idTbry(inorth,iParticle7)')
              idTbry(inorth,iParticle7)=varid

            CASE ('idTbry(iwest,iDissolve7)')
              idTbry(iwest,iDissolve7)=varid
            CASE ('idTbry(ieast,iDissolve7)')
              idTbry(ieast,iDissolve7)=varid
            CASE ('idTbry(isouth,iDissolve7)')
              idTbry(isouth,iDissolve7)=varid
            CASE ('idTbry(inorth,iDissolve7)')
              idTbry(inorth,iDissolve7)=varid

            CASE ('idTbry(iwest,iParticle8)')
              idTbry(iwest,iParticle8)=varid
            CASE ('idTbry(ieast,iParticle8)')
              idTbry(ieast,iParticle8)=varid
            CASE ('idTbry(isouth,iParticle8)')
              idTbry(isouth,iParticle8)=varid
            CASE ('idTbry(inorth,iParticle8)')
              idTbry(inorth,iParticle8)=varid

            CASE ('idTbry(iwest,iDissolve8)')
              idTbry(iwest,iDissolve8)=varid
            CASE ('idTbry(ieast,iDissolve8)')
              idTbry(ieast,iDissolve8)=varid
            CASE ('idTbry(isouth,iDissolve8)')
              idTbry(isouth,iDissolve8)=varid
            CASE ('idTbry(inorth,iDissolve8)')
              idTbry(inorth,iDissolve8)=varid

            CASE ('idTbry(iwest,iParticle9)')
              idTbry(iwest,iParticle9)=varid
            CASE ('idTbry(ieast,iParticle9)')
              idTbry(ieast,iParticle9)=varid
            CASE ('idTbry(isouth,iParticle9)')
              idTbry(isouth,iParticle9)=varid
            CASE ('idTbry(inorth,iParticle9)')
              idTbry(inorth,iParticle9)=varid

            CASE ('idTbry(iwest,iDissolve9)')
              idTbry(iwest,iDissolve9)=varid
            CASE ('idTbry(ieast,iDissolve9)')
              idTbry(ieast,iDissolve9)=varid
            CASE ('idTbry(isouth,iDissolve9)')
              idTbry(isouth,iDissolve9)=varid
            CASE ('idTbry(inorth,iDissolve9)')
              idTbry(inorth,iDissolve9)=varid
#   endif

/*
**  Biological tracers point Source/Sinks (river runoff).
*/

            CASE ('idRtrc(iParticle1)')
              idRtrc(iParticle1)=varid
            CASE ('idRtrc(iDissolve1)')
              idRtrc(iDissolve1)=varid

#   ifdef FULL_DISSOLVE
            CASE ('idRtrc(iDissolve0)')
              idRtrc(iDissolve0)=varid
#   endif
#   ifdef MULTI_PARTICLES
            CASE ('idRtrc(iParticle2)')
              idRtrc(iParticle2)=varid
            CASE ('idRtrc(iDissolve2)')
              idRtrc(iDissolve2)=varid

            CASE ('idRtrc(iParticle3)')
              idRtrc(iParticle3)=varid
            CASE ('idRtrc(iDissolve3)')
              idRtrc(iDissolve3)=varid

            CASE ('idRtrc(iParticle4)')
              idRtrc(iParticle4)=varid
            CASE ('idRtrc(iDissolve4)')
              idRtrc(iDissolve4)=varid

            CASE ('idRtrc(iParticle5)')
              idRtrc(iParticle5)=varid
            CASE ('idRtrc(iDissolve5)')
              idRtrc(iDissolve5)=varid

            CASE ('idRtrc(iParticle6)')
              idRtrc(iParticle6)=varid
            CASE ('idRtrc(iDissolve6)')
              idRtrc(iDissolve6)=varid

            CASE ('idRtrc(iParticle7)')
              idRtrc(iParticle7)=varid
            CASE ('idRtrc(iDissolve7)')
              idRtrc(iDissolve7)=varid

            CASE ('idRtrc(iParticle8)')
              idRtrc(iParticle8)=varid
            CASE ('idRtrc(iDissolve8)')
              idRtrc(iDissolve8)=varid

            CASE ('idRtrc(iParticle9)')
              idRtrc(iParticle9)=varid
            CASE ('idRtrc(iDissolve9)')
              idRtrc(iDissolve9)=varid
#   endif


#ifdef DIAGNOSTICS_BIO

/*
**  Biological tracers term diagnostics.
*/

#endif
