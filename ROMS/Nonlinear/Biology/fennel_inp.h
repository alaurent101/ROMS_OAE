      SUBROUTINE read_BioPar (model, inp, out, Lwrite)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in Fennel et al. (2006) ecosystem model input    !
!  parameters. They are specified in input script "fennel.in".         !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_ncparam
      USE mod_scalars
!
      USE inp_decode_mod
!
      implicit none
!
!  Imported variable declarations
!
      logical, intent(in) :: Lwrite
      integer, intent(in) :: model, inp, out
!
!  Local variable declarations.
!
      integer :: Npts, Nval
      integer :: iTrcStr, iTrcEnd
      integer :: i, ifield, igrid, itracer, itrc, ng, nline, status

      logical, dimension(Ngrids) :: Lbio
      logical, dimension(NBT,Ngrids) :: Ltrc

      real(r8), dimension(NBT,Ngrids) :: Rbio

      real(dp), dimension(nRval) :: Rval

      character (len=40 ) :: KeyWord
      character (len=256) :: line
      character (len=256), dimension(nCval) :: Cval
!
!-----------------------------------------------------------------------
!  Initialize.
!-----------------------------------------------------------------------
!
      igrid=1                            ! nested grid counter
      itracer=0                          ! LBC tracer counter
      iTrcStr=1                          ! first LBC tracer to process
      iTrcEnd=NBT                        ! last  LBC tracer to process
      nline=0                            ! LBC multi-line counter
!
!-----------------------------------------------------------------------
!  Read in Fennel et al. (2006) biological model parameters.
!-----------------------------------------------------------------------
!
      DO WHILE (.TRUE.)
        READ (inp,'(a)',ERR=10,END=20) line
        status=decode_line(line, KeyWord, Nval, Cval, Rval)
        IF (status.gt.0) THEN
          SELECT CASE (TRIM(KeyWord))
            CASE ('Lbiology')
              Npts=load_l(Nval, Cval, Ngrids, Lbiology)
            CASE ('BioIter')
              Npts=load_i(Nval, Rval, Ngrids, BioIter)
            CASE ('AttSW')
              Npts=load_r(Nval, Rval, Ngrids, AttSW)
            CASE ('AttChl')
              Npts=load_r(Nval, Rval, Ngrids, AttChl)
            CASE ('PARfrac')
              Npts=load_r(Nval, Rval, Ngrids, PARfrac)
            CASE ('I_thNH4')
              Npts=load_r(Nval, Rval, Ngrids, I_thNH4)
            CASE ('D_p5NH4')
              Npts=load_r(Nval, Rval, Ngrids, D_p5NH4)
            CASE ('ChlMin')
              Npts=load_r(Nval, Rval, Ngrids, ChlMin)
            CASE ('PhyCN')
              Npts=load_r(Nval, Rval, Ngrids, PhyCN)
            CASE ('R_P2N')
              Npts=load_r(Nval, Rval, Ngrids, R_P2N)
            CASE ('PhyIP')
              Npts=load_r(Nval, Rval, Ngrids, PhyIP)
            CASE ('PhyMin')
              Npts=load_r(Nval, Rval, Ngrids, PhyMin)
            CASE ('NitriR')
              Npts=load_r(Nval, Rval, Ngrids, NitriR)
#if defined BIO_2P2Z || defined BIO_2P3Z
            CASE ('PsVp0')
              Npts=load_r(Nval, Rval, Ngrids, PsVp0)
            CASE ('PlVp0')
              Npts=load_r(Nval, Rval, Ngrids, PlVp0)
            CASE ('K_NO3_Ps')
              Npts=load_r(Nval, Rval, Ngrids, K_NO3_Ps)
            CASE ('K_NH4_Ps')
              Npts=load_r(Nval, Rval, Ngrids, K_NH4_Ps)
            CASE ('K_PO4_Ps')
              Npts=load_r(Nval, Rval, Ngrids, K_PO4_Ps)
            CASE ('K_NO3_Pl')
              Npts=load_r(Nval, Rval, Ngrids, K_NO3_Pl)
            CASE ('K_NH4_Pl')
              Npts=load_r(Nval, Rval, Ngrids, K_NH4_Pl)
            CASE ('K_PO4_Pl')
              Npts=load_r(Nval, Rval, Ngrids, K_PO4_Pl)
            CASE ('PhIS_Ps')
              Npts=load_r(Nval, Rval, Ngrids, PhIS_Ps)
            CASE ('PhIS_Pl')
              Npts=load_r(Nval, Rval, Ngrids, PhIS_Pl)  
            CASE ('Chl2CPs_m')
              Npts=load_r(Nval, Rval, Ngrids, Chl2CPs_m) 
            CASE ('Chl2CPl_m')
              Npts=load_r(Nval, Rval, Ngrids, Chl2CPl_m)
            CASE ('K_ZsPs')
              Npts=load_r(Nval, Rval, Ngrids, K_ZsPs)
#   ifdef BIO_2P2Z
            CASE ('K_ZsPl')
              Npts=load_r(Nval, Rval, Ngrids, K_ZsPl)
            CASE ('K_ZsDs')
              Npts=load_r(Nval, Rval, Ngrids, K_ZsDs)
#   endif
            CASE ('ZooSAE_N')
              Npts=load_r(Nval, Rval, Ngrids, ZooSAE_N) 
            CASE ('K_ZlPs')
              Npts=load_r(Nval, Rval, Ngrids, K_ZlPs)
            CASE ('K_ZlPl')
              Npts=load_r(Nval, Rval, Ngrids, K_ZlPl)
            CASE ('K_ZlZs')
              Npts=load_r(Nval, Rval, Ngrids, K_ZlZs)
#   ifdef BIO_2P2Z
            CASE ('K_ZlDs')
              Npts=load_r(Nval, Rval, Ngrids, K_ZlDs)
            CASE ('ZooS_GrInPl')
              Npts=load_r(Nval, Rval, Ngrids, ZooS_GrInPl) 
            CASE ('ZooS_GrInDs')
              Npts=load_r(Nval, Rval, Ngrids, ZooS_GrInDs)
            CASE ('ZooL_GrInPs')
              Npts=load_r(Nval, Rval, Ngrids, ZooL_GrInPs)
            CASE ('ZooL_GrInDs')
              Npts=load_r(Nval, Rval, Ngrids, ZooL_GrInDs)
#   endif
            CASE ('ZooLAE_N')
            Npts=load_r(Nval, Rval, Ngrids, ZooLAE_N)
#   ifdef BIO_2P3Z
            CASE ('K_ZpPl')
              Npts=load_r(Nval, Rval, Ngrids, K_ZpPl)
            CASE ('K_ZpZs')
              Npts=load_r(Nval, Rval, Ngrids, K_ZpZs)   
            CASE ('K_ZpZl')
              Npts=load_r(Nval, Rval, Ngrids, K_ZpZl)  
            CASE ('ZooPAE_N')
              Npts=load_r(Nval, Rval, Ngrids, ZooPAE_N)  
            CASE ('ZooP_GrInPl')
              Npts=load_r(Nval, Rval, Ngrids, ZooP_GrInPl)   
            CASE ('ZooP_GrInZs')
              Npts=load_r(Nval, Rval, Ngrids, ZooP_GrInZs)
#   endif
#else
            CASE ('Vp0')
              Npts=load_r(Nval, Rval, Ngrids, Vp0)
            CASE ('K_NO3')
              Npts=load_r(Nval, Rval, Ngrids, K_NO3)
            CASE ('K_NH4')
              Npts=load_r(Nval, Rval, Ngrids, K_NH4)
            CASE ('K_PO4')
              Npts=load_r(Nval, Rval, Ngrids, K_PO4)
            CASE ('PhyIS')
              Npts=load_r(Nval, Rval, Ngrids, PhyIS)
            CASE ('Chl2C_m')
              Npts=load_r(Nval, Rval, Ngrids, Chl2C_m)
            CASE ('K_Phy')
              Npts=load_r(Nval, Rval, Ngrids, K_Phy)
            CASE ('ZooAE_N')
              Npts=load_r(Nval, Rval, Ngrids, ZooAE_N)
#endif

#ifdef TEMP_RATES
#  if defined BIO_2P2Z || defined BIO_2P3Z
            CASE ('PsMR0')
              Npts=load_r(Nval, Rval, Ngrids, PsMR0)
            CASE ('PlMR0')
              Npts=load_r(Nval, Rval, Ngrids, PlMR0)
            CASE ('ZooSBM0')
              Npts=load_r(Nval, Rval, Ngrids, ZooSBM0)
            CASE ('ZooSMR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooSMR0)
            CASE ('ZooSER0')
              Npts=load_r(Nval, Rval, Ngrids, ZooSER0)
            CASE ('ZooSPsGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooSPsGR0)
#    ifdef BIO_2P2Z
            CASE ('ZooSPlGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooSPlGR0)
            CASE ('ZooSDsGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooSDsGR0)
#    endif
            CASE ('ZooLBM0')
              Npts=load_r(Nval, Rval, Ngrids, ZooLBM0)
            CASE ('ZooLMR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooLMR0)
            CASE ('ZooLER0')
              Npts=load_r(Nval, Rval, Ngrids, ZooLER0)
            CASE ('ZooLPsGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooLPsGR0)
            CASE ('ZooLPlGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooLPlGR0)
            CASE ('ZooLZsGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooLZsGR0)
#    ifdef BIO_2P2Z
            CASE ('ZooLDsGR0')
            Npts=load_r(Nval, Rval, Ngrids, ZooLDsGR0)
#    endif
#    ifdef BIO_2P3Z
            CASE ('ZooPBM0')
              Npts=load_r(Nval, Rval, Ngrids, ZooPBM0)
            CASE ('ZooPMR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooPMR0)
            CASE ('ZooPER0')
              Npts=load_r(Nval, Rval, Ngrids, ZooPER0)
            CASE ('ZooPPlGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooPPlGR0)
            CASE ('ZooPZsGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooPZsGR0)
            CASE ('ZooPZlGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooPZlGR0)
#    endif
#  else
            CASE ('PhyMR0')
              Npts=load_r(Nval, Rval, Ngrids, PhyMR0)
            CASE ('ZooBM0')
              Npts=load_r(Nval, Rval, Ngrids, ZooBM0)
            CASE ('ZooMR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooMR0)
            CASE ('ZooER0')
              Npts=load_r(Nval, Rval, Ngrids, ZooER0)
            CASE ('ZooGR0')
              Npts=load_r(Nval, Rval, Ngrids, ZooGR0)
#  endif
#else
#  if defined BIO_2P2Z || defined BIO_2P3Z
            CASE ('PsMR')
              Npts=load_r(Nval, Rval, Ngrids, PsMR)
            CASE ('PlMR')
              Npts=load_r(Nval, Rval, Ngrids, PlMR)
            CASE ('ZooSBM')
              Npts=load_r(Nval, Rval, Ngrids, ZooSBM)
            CASE ('ZooSMR')
              Npts=load_r(Nval, Rval, Ngrids, ZooSMR)
            CASE ('ZooSER')
              Npts=load_r(Nval, Rval, Ngrids, ZooSER)
            CASE ('ZooSPsGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooSPsGR)
#    ifdef BIO_2P2Z
            CASE ('ZooSPlGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooSPlGR)
            CASE ('ZooSDsGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooSDsGR)
#    endif
            CASE ('ZooLBM')
              Npts=load_r(Nval, Rval, Ngrids, ZooLBM)
            CASE ('ZooLMR')
              Npts=load_r(Nval, Rval, Ngrids, ZooLMR)
            CASE ('ZooLER')
              Npts=load_r(Nval, Rval, Ngrids, ZooLER)
            CASE ('ZooLPsGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooLPsGR)
            CASE ('ZooLPlGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooLPlGR)
            CASE ('ZooLZsGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooLZsGR)
#    ifdef BIO_2P2Z
            CASE ('ZooLDsGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooLDsGR)
#    endif
#    ifdef BIO_2P3Z
            CASE ('ZooPBM')
              Npts=load_r(Nval, Rval, Ngrids, ZooPBM)
            CASE ('ZooPMR')
              Npts=load_r(Nval, Rval, Ngrids, ZooPMR)
            CASE ('ZooPER')
              Npts=load_r(Nval, Rval, Ngrids, ZooPER)
            CASE ('ZooPPlGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooPPlGR)
            CASE ('ZooPZsGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooPZsGR)
            CASE ('ZooPZlGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooPZlGR)
#    endif
#  else
            CASE ('PhyMR')
              Npts=load_r(Nval, Rval, Ngrids, PhyMR)
            CASE ('ZooBM')
              Npts=load_r(Nval, Rval, Ngrids, ZooBM)
            CASE ('ZooMR')
              Npts=load_r(Nval, Rval, Ngrids, ZooMR)
            CASE ('ZooER')
              Npts=load_r(Nval, Rval, Ngrids, ZooER)
            CASE ('ZooGR')
              Npts=load_r(Nval, Rval, Ngrids, ZooGR)
#  endif
#endif
            CASE ('ZooMin')
              Npts=load_r(Nval, Rval, Ngrids, ZooMin)
            CASE ('ZooCN')
              Npts=load_r(Nval, Rval, Ngrids, ZooCN)
            CASE ('LDeRRN')
              Npts=load_r(Nval, Rval, Ngrids, LDeRRN)
            CASE ('LDeRRC')
              Npts=load_r(Nval, Rval, Ngrids, LDeRRC)
            CASE ('CoagR')
              Npts=load_r(Nval, Rval, Ngrids, CoagR)
            CASE ('SDeRRN')
              Npts=load_r(Nval, Rval, Ngrids, SDeRRN)
            CASE ('SDeRRC')
              Npts=load_r(Nval, Rval, Ngrids, SDeRRC)
            CASE ('RDeRRN')
              Npts=load_r(Nval, Rval, Ngrids, RDeRRN)
            CASE ('RDeRRC')
              Npts=load_r(Nval, Rval, Ngrids, RDeRRC)
# if defined BIO_2P2Z || defined BIO_2P3Z
            CASE ('wPhyS')
              Npts=load_r(Nval, Rval, Ngrids, wPhyS)
            CASE ('wPhyL')
              Npts=load_r(Nval, Rval, Ngrids, wPhyL)
# else
            CASE ('wPhy')
              Npts=load_r(Nval, Rval, Ngrids, wPhy)
# endif
            CASE ('wLDet')
              Npts=load_r(Nval, Rval, Ngrids, wLDet)
            CASE ('wSDet')
              Npts=load_r(Nval, Rval, Ngrids, wSDet)
            CASE ('pCO2air')
              Npts=load_r(Nval, Rval, Ngrids, pCO2air)
# ifdef OAE
            CASE ('OAE_Flux')
              Npts=load_r(Nval, Rval, Ngrids, OAE_Flux)
            CASE ('OAE_Tmin')
              Npts=load_r(Nval, Rval, Ngrids, OAE_Tmin)
            CASE ('OAE_Tmax')
              Npts=load_r(Nval, Rval, Ngrids, OAE_Tmax)
            CASE ('OAE_iloc')
              Npts=load_r(Nval, Rval, Ngrids, OAE_iloc)
            CASE ('OAE_jloc')
              Npts=load_r(Nval, Rval, Ngrids, OAE_jloc)
# endif
            CASE ('TNU2')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNU4')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  nl_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU2')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu2(i,ng)=Rbio(itrc,ng)
                  tl_tnu2(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_TNU4')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_tnu4(i,ng)=Rbio(itrc,ng)
                  tl_tnu4(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('LtracerSponge')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSponge(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('AKT_BAK')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Akt_bak(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('ad_AKT_fac')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  ad_Akt_fac(i,ng)=Rbio(itrc,ng)
                  tl_Akt_fac(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('TNUDG')
              Npts=load_r(Nval, Rval, NBT, Ngrids, Rbio)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  Tnudg(i,ng)=Rbio(itrc,ng)
                END DO
              END DO
            CASE ('Hadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       Hadvection)
            CASE ('Vadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       Vadvection)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_Hadvection')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       ad_Hadvection)
            CASE ('Vadvection')
              IF (itracer.lt.(NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              itrc=idbio(itracer)
              Npts=load_tadv(Nval, Cval, line, nline, itrc, igrid,      &
     &                       itracer, idbio(iTrcStr), idbio(iTrcEnd),   &
     &                       Vname(1,idTvar(itrc)),                     &
     &                       ad_Vadvection)
#endif
            CASE ('LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), LBC)
#if defined ADJOINT || defined TANGENT || defined TL_IOMS
            CASE ('ad_LBC(isTvar)')
              IF (itracer.lt.NBT) THEN
                itracer=itracer+1
              ELSE
                itracer=1                      ! next nested grid
              END IF
              ifield=isTvar(idbio(itracer))
              Npts=load_lbc(Nval, Cval, line, nline, ifield, igrid,     &
     &                      idbio(iTrcStr), idbio(iTrcEnd),             &
     &                      Vname(1,idTvar(idbio(itracer))), ad_LBC)
#endif
            CASE ('LtracerSrc')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerSrc(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LtracerCLM')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LtracerCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LnudgeTCLM')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LnudgeTCLM(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('LnudgeTREA')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idbio(itrc)
                  LnudgeTREA(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTvar(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Hout(idTsur)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idTsur(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Hout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idsurT)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idsurT(idbio(itrc))
                  IF (i.eq.0) THEN
                    IF (Master) WRITE (out,30)                          &
     &                                'idsurT(idbio(', itrc, '))'
                    exit_flag=5
                    RETURN
                  END IF
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Qout(idTsur)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTsur(idbio(itrc))
                  Qout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            CASE ('Aout(idTvar)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTvar(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idTTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idTTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idUTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(idVTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=idVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHUTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHUTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
            CASE ('Aout(iHVTav)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO itrc=1,NBT
                  i=iHVTav(idbio(itrc))
                  Aout(i,ng)=Ltrc(itrc,ng)
                END DO
              END DO
#endif
#ifdef DIAGNOSTICS_TS
            CASE ('Dout(iTrate)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTrate),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iThadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTyadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTyadv),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTvadv)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvadv),ng)=Ltrc(i,ng)
                END DO
              END DO
# if defined TS_DIF2 || defined TS_DIF4
            CASE ('Dout(iThdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iThdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTxdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTxdif),ng)=Ltrc(i,ng)
                END DO
              END DO
            CASE ('Dout(iTydif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTydif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
            CASE ('Dout(iTsdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTsdif),ng)=Ltrc(i,ng)
                END DO
              END DO
#  endif
# endif
            CASE ('Dout(iTvdif)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTvdif),ng)=Ltrc(i,ng)
                END DO
              END DO
# if defined DIAG_NUDG
            CASE ('Dout(iTnudg)')
              Npts=load_l(Nval, Cval, NBT, Ngrids, Ltrc)
              DO ng=1,Ngrids
                DO i=1,NBT
                  itrc=idbio(i)
                  Dout(idDtrc(itrc,iTnudg),ng)=Ltrc(i,ng)
                END DO
              END DO
# endif
#endif
#ifdef DIAGNOSTICS_BIO
# ifdef CARBON
            CASE ('Dout(iCOfx)')
              IF (iDbio2(iCOfx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iCOfx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iCOfx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif
# ifdef DENITRIFICATION
            CASE ('Dout(iDNIT)')
              IF (iDbio2(iDNIT).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iDNIT)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iDNIT)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif
# ifdef CARBON
            CASE ('Dout(ipCO2)')
              IF (iDbio2(ipCO2).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(ipCO2)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(ipCO2)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif
# ifdef OXYGEN
            CASE ('Dout(iO2fx)')
              IF (iDbio2(iO2fx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio2(iO2fx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio2(iO2fx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
# endif
            CASE ('Dout(iPPro)')
              IF (iDbio3(iPPro).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iPPro)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iPPro)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iNO3u)')
              IF (iDbio3(iNO3u).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iNO3u)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iNO3u)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
            CASE ('Dout(iNifx)')
              IF (iDbio3(iNifx).eq.0) THEN
                IF (Master) WRITE (out,40) 'iDbio3(iNifx)'
                exit_flag=5
                RETURN
              END IF
              Npts=load_l(Nval, Cval, Ngrids, Lbio)
              i=iDbio3(iNifx)
              DO ng=1,Ngrids
                Dout(i,ng)=Lbio(ng)
              END DO
#endif
          END SELECT
        END IF
      END DO
  10  IF (Master) WRITE (out,50) line
      exit_flag=4
      RETURN
  20  CONTINUE
!
!-----------------------------------------------------------------------
!  Report input parameters.
!-----------------------------------------------------------------------
!
      IF (Master.and.Lwrite) THEN
        DO ng=1,Ngrids
          IF (Lbiology(ng)) THEN
            WRITE (out,60) ng
            WRITE (out,70) BioIter(ng), 'BioIter',                      &
     &            'Number of iterations for nonlinear convergence.'
            WRITE (out,80) AttSW(ng), 'AttSW',                          &
     &            'Light attenuation of seawater (m-1).'
            WRITE (out,80) AttChl(ng), 'AttChl',                        &
     &            'Light attenuation by chlorophyll (1/(mg_Chl m-2)).'
            WRITE (out,90) PARfrac(ng), 'PARfrac',                      &
     &            'Fraction of shortwave radiation that is',            &
     &            'photosynthetically active (nondimensional).'
#if defined BIO_2P2Z || defined BIO_2P3Z
            WRITE (out,90) PsVp0(ng), 'PsVp0',                          &
     &            'Temperature-limited growth parameter for small P',   &
     &            '(nondimensional).'
            WRITE (out,90) PlVp0(ng), 'PlVp0',                          &
     &            'Temperature-limited growth parameter for large P',   &
     &            '(nondimensional).'
            WRITE (out,90) K_NO3_Ps(ng), 'K_NO3_Ps',                    &
     &            'Inverse half-saturation for small phyto NO3',        &
     &            'uptake (1/(mmol_N m-3)).'
            WRITE (out,90) K_NH4_Ps(ng), 'K_NH4_Ps',                    &
     &            'Inverse half-saturation for small phyto NH4',        &
     &            'uptake (1/(mmol_N m-3)).'
            WRITE (out,90) K_PO4_Ps(ng), 'K_PO4_Ps',                    &
     &            'Inverse half-saturation for small phyto PO4',        &
     &            'uptake (1/(mmol_P m-3)).'
            WRITE (out,90) K_NO3_Pl(ng), 'K_NO3_Pl',                    &
     &            'Inverse half-saturation for large phyto NO3',        &
     &            'uptake (1/(mmol_N m-3)).'
            WRITE (out,90) K_NH4_Pl(ng), 'K_NH4_Pl',                    &
     &            'Inverse half-saturation for large phyto NH4',        &
     &            'uptake (1/(mmol_N m-3)).'
            WRITE (out,90) K_PO4_Pl(ng), 'K_PO4_Pl',                    &
     &            'Inverse half-saturation for large phyto PO4',        &
     &            'uptake (1/(mmol_P m-3)).'
            WRITE (out,90) PhIS_Ps(ng), 'PhIS_Ps',                      &
     &            'Initial slope of P-I curve, small phyto',            &
     &            '(mg_C/(mg_Chl Watts m-2 day)).'
            WRITE (out,90) PhIS_Pl(ng), 'PhIS_Pl',                      &
     &            'Initial slope of P-I curve, large phyto',            &
     &            '(mg_C/(mg_Chl Watts m-2 day)).'
            WRITE (out,80) Chl2CPs_m(ng), 'Chl2CPs_m',                  &
     &            'Maximum Chl:C ratio for small P (mg_Chl/mg_C).'
            WRITE (out,80) Chl2CPl_m(ng), 'Chl2CPl_m',                  &
     &            'Maximum Chl:C ratio for large P (mg_Chl/mg_C).'
            WRITE (out,90) ZooSAE_N(ng), 'ZooSAE_N',                    &
     &            'Zooplankton nitrogen assimilation efficiency',       &
     &            '(nondimensional).'
            WRITE (out,90) K_ZsPs(ng), 'K_ZsPs',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
#  if defined BIO_2P2Z
            WRITE (out,90) K_ZsPl(ng), 'K_ZsPl',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
            WRITE (out,90) K_ZsDs(ng), 'K_ZsDs',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
#  endif
            WRITE (out,90) ZooLAE_N(ng), 'ZooLAE_N',                    &
     &            'Zooplankton nitrogen assimilation efficiency',       &
     &            '(nondimensional).'
            WRITE (out,90) K_ZlPs(ng), 'K_ZlPs',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
            WRITE (out,90) K_ZlPl(ng), 'K_ZlPl',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
            WRITE (out,90) K_ZlZs(ng), 'K_ZlZs',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
#  if defined BIO_2P2Z
            WRITE (out,90) K_ZlDs(ng), 'K_ZlDs',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
            WRITE (out,90) ZooS_GrInPl(ng), 'ZooS_GrInPl',              &
     &            'Inhibition coefficient for Zs feeding on large P' 
            WRITE (out,90) ZooS_GrInDs(ng), 'ZooS_GrInDs',              &
     &            'Inhibition coefficient for Zs feeding on small D'     
            WRITE (out,90) ZooL_GrInPs(ng), 'ZooL_GrInPs',              &
     &            'Inhibition coefficient for Zl feeding on small P'
            WRITE (out,90) ZooL_GrInDs(ng), 'ZooL_GrInDs',              &
     &            'Inhibition coefficient for Zl feeding on small D'
#  endif
#  if defined BIO_2P3Z
            WRITE (out,90) ZooPAE_N(ng), 'ZooPAE_N',                    &
     &            'Predatory zoo nitrogen assimilation efficiency',     &
     &            '(nondimensional).'
            WRITE (out,90) K_ZpPl(ng), 'K_ZpPl',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
            WRITE (out,90) K_ZpZs(ng), 'K_ZpZs',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
            WRITE (out,90) K_ZpZl(ng), 'K_ZpZl',                        &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
            WRITE (out,90) ZooP_GrInPl(ng), 'ZooP_GrInPl',              &
     &            'Inhibition coefficient for feeding on large P'     
            WRITE (out,90) ZooP_GrInZs(ng), 'ZooP_GrInZs',              &
     &            'Inhibition coefficient for feeding on large P'
#  endif
#else
            WRITE (out,90) Vp0(ng), 'Vp0',                              &
     &            'Eppley temperature-limited growth parameter',        &
     &            '(nondimensional).'
            WRITE (out,90) K_NO3(ng), 'K_NO3',                          &
     &            'Inverse half-saturation for phytoplankton NO3',      &
     &            'uptake (1/(mmol_N m-3)).'
            WRITE (out,90) K_NH4(ng), 'K_NH4',                          &
     &            'Inverse half-saturation for phytoplankton NH4',      &
     &            'uptake (1/(mmol_N m-3)).'
            WRITE (out,90) K_PO4(ng), 'K_PO4',                          &
     &            'Inverse half-saturation for phytoplankton PO4',      &
     &            'uptake (1/(mmol_P m-3)).'
            WRITE (out,90) PhyIS(ng), 'PhyIS',                          &
     &            'Phytoplankton growth, initial slope of P-I curve',   &
     &            '(mg_C/(mg_Chl Watts m-2 day)).'
            WRITE (out,80) Chl2C_m(ng), 'Chl2C_m',                      &
     &            'Maximum chlorophyll to carbon ratio (mg_Chl/mg_C).'
            WRITE (out,90) ZooAE_N(ng), 'ZooAE_N',                      &
     &            'Zooplankton nitrogen assimilation efficiency',       &
     &            '(nondimensional).'
            WRITE (out,90) K_Phy(ng), 'K_Phy',                          &
     &            'Zooplankton half-saturation constant for ingestion', &
     &            '(mmol_N m-3)^2.'
#endif
            WRITE (out,80) I_thNH4(ng), 'I_thNH4',                      &
     &            'Radiation threshold for nitrification (W/m2).'
            WRITE (out,80) D_p5NH4(ng), 'D_p5NH4',                      &
     &            'Half-saturation radiation for nitrification (W/m2).'
            WRITE (out,80) ChlMin(ng), 'ChlMin',                        &
     &            'Chlorophyll minimum threshold (mg_Chl/m3).'
            WRITE (out,80) PhyCN(ng), 'PhyCN',                          &
     &            'Phytoplankton Carbon:Nitrogen ratio (mol_C/mol_N).'
            WRITE (out,80) R_P2N(ng), 'R_P2N',                          &
     &            'Phytoplankton P:N ratio (mol_P/mol_N).'
            WRITE (out,80) PhyIP(ng), 'PhyIP',                          &
     &            'Phytoplankton NH4 inhibition parameter (1/mmol_N).'
            WRITE (out,80) PhyMin(ng), 'PhyMin',                        &
     &            'Phytoplankton minimum threshold (mmol_N/m3).'
            WRITE (out,80) NitriR(ng), 'NitriR',                        &
     &            'Nitrification rate (day-1).'
#ifdef TEMP_RATES
#  if defined BIO_2P2Z || defined BIO_2P3Z
            WRITE (out,80) PsMR0(ng), 'PsMR0',                          &
     &            'small phytoplankton mortality rate at T=0C(day-1).'
            WRITE (out,80) PlMR0(ng), 'PlMR0',                          &
     &            'large phytoplankton mortality rate at T=0C(day-1).'
            WRITE (out,80) ZooSBM0(ng), 'ZooSBM0',                      &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooSER0(ng), 'ZooSER0',                      &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooSPsGR0(ng), 'ZooSPsGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
#    if defined BIO_2P2Z
            WRITE (out,80) ZooSPlGR0(ng), 'ZooSPlGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooSDsGR0(ng), 'ZooSDsGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
#    endif
            WRITE (out,80) ZooSMR0(ng), 'ZooSMR0',                      &
     &            'Zooplankton mortality rate (day-1).'
            WRITE (out,80) ZooLBM0(ng), 'ZooLBM0',                      &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooLER0(ng), 'ZooLER0',                      &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooLPsGR0(ng), 'ZooLPsGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooLPlGR0(ng), 'ZooLPlGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooLZsGR0(ng), 'ZooLZsGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
#    if defined BIO_2P2Z
            WRITE (out,80) ZooLDsGR0(ng), 'ZooLDsGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
#    endif
            WRITE (out,80) ZooLMR0(ng), 'ZooLMR0',                      &
     &            'Zooplankton mortality rate (day-1).'
#    if defined BIO_2P3Z
            WRITE (out,80) ZooPBM0(ng), 'ZooPBM0',                      &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooPER0(ng), 'ZooPER0',                      &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooPPlGR0(ng), 'ZooPPlGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooPZsGR0(ng), 'ZooPZsGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooPZlGR0(ng), 'ZooPZlGR0',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooPMR0(ng), 'ZooPMR0',                      &
     &            'Zooplankton mortality rate (day-1).'
#    endif
#  else
            WRITE (out,80) PhyMR0(ng), 'PhyMR0',                          &
     &            'Phytoplankton mortality rate at T=0C(day-1).'
            WRITE (out,80) ZooBM0(ng), 'ZooBM0',                          &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooER0(ng), 'ZooER0',                          &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooGR0(ng), 'ZooGR0',                          &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooMR0(ng), 'ZooMR0',                          &
     &            'Zooplankton mortality rate (day-1).'
#  endif
#else
#  if defined BIO_2P2Z || defined BIO_2P3Z
            WRITE (out,80) PsMR(ng), 'PsMR',                          &
     &            'small phytoplankton mortality rate (day-1).'
            WRITE (out,80) PlMR(ng), 'PlMR',                          &
     &            'large phytoplankton mortality rate (day-1).'
            WRITE (out,80) ZooSBM(ng), 'ZooSBM',                          &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooSER(ng), 'ZooSER',                          &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooSPsGR(ng), 'ZooSPsGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
#    if defined BIO_2P2Z
            WRITE (out,80) ZooSPlGR(ng), 'ZooSPlGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooSDsGR(ng), 'ZooSDsGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
#    endif
            WRITE (out,80) ZooSMR(ng), 'ZooSMR',                          &
     &            'Zooplankton mortality rate (day-1).'
            WRITE (out,80) ZooLBM(ng), 'ZooLBM',                          &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooLER(ng), 'ZooLER',                          &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooLPsGR(ng), 'ZooLPsGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooLPlGR(ng), 'ZooLPlGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooLZsGR(ng), 'ZooLZsGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
#    if defined BIO_2P2Z
            WRITE (out,80) ZooLDsGR(ng), 'ZooLDsGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
#    endif
            WRITE (out,80) ZooLMR(ng), 'ZooLMR',                          &
     &            'Zooplankton mortality rate (day-1).'
#    if defined BIO_2P3Z
            WRITE (out,80) ZooPBM(ng), 'ZooPBM',                      &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooPER(ng), 'ZooPER',                      &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooPPlGR(ng), 'ZooPPlGR',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooPZsGR(ng), 'ZooPZsGR',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooPZlGR(ng), 'ZooPZlGR',                  &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooPMR(ng), 'ZooPMR',                      &
     &            'Zooplankton mortality rate (day-1).'
#    endif
#  else
            WRITE (out,80) PhyMR(ng), 'PhyMR',                          &
     &            'Phytoplankton mortality rate (day-1).'
            WRITE (out,80) ZooBM(ng), 'ZooBM',                          &
     &            'Rate for zooplankton basal metabolism (1/day).'
            WRITE (out,80) ZooER(ng), 'ZooER',                          &
     &            'Zooplankton specific excretion rate (day-1).'
            WRITE (out,80) ZooGR(ng), 'ZooGR',                          &
     &            'Zooplankton maximum growth rate (day-1).'
            WRITE (out,80) ZooMR(ng), 'ZooMR',                          &
     &            'Zooplankton mortality rate (day-1).'
#  endif
#endif
            WRITE (out,80) ZooMin(ng), 'ZooMin',                        &
     &            'Zooplankton minimum threshold (mmol_N/m3).'
            WRITE (out,80) LDeRRN(ng), 'LDeRRN',                        &
     &            'Large detritus N re-mineralization rate (day-1).'
            WRITE (out,80) LDeRRC(ng), 'LDeRRC',                        &
     &            'Large detritus C re-mineralization rate (day-1).'
            WRITE (out,80) CoagR(ng), 'CoagR',                          &
     &            'Coagulation rate (day-1).'
            WRITE (out,80) SDeRRN(ng), 'SDeRRN',                        &
     &            'Remineralization rate for small detritus N (day-1).'
            WRITE (out,80) SDeRRC(ng), 'SDeRRC',                        &
     &            'Remineralization rate for small detritus C (day-1).'
            WRITE (out,80) RDeRRN(ng), 'RDeRRN',                        &
     &            'Remineralization rate for river detritus N (day-1).'
            WRITE (out,80) RDeRRC(ng), 'RDeRRC',                        &
     &            'Remineralization rate for river detritus C (day-1).'
# if defined BIO_2P2Z || defined BIO_2P3Z
            WRITE (out,80) wPhyS(ng), 'wPhyS',				 &
     &            'Small Phytoplankton sinking velocity (m/day).'
            WRITE (out,80) wPhyL(ng), 'wPhyL',				 &
     &            'Large Phytoplankton sinking velocity (m/day).'
# else
            WRITE (out,80) wPhy(ng), 'wPhy',                            &
     &            'Phytoplankton sinking velocity (m/day).'
# endif
            WRITE (out,80) wLDet(ng), 'wLDet',                          &
     &            'Large detritus sinking velocity (m/day).'
            WRITE (out,80) wSDet(ng), 'wSDet',                          &
     &            'Small detritus sinking velocity (m/day).'
            WRITE (out,80) pCO2air(ng), 'pCO2air',                      &
     &            'CO2 partial pressure in air (ppm by volume).'
# ifdef OAE
            WRITE (out,80) OAE_Flux(ng), 'OAE_Flux',                    &
     &            'Alkalinity input for OAE ((mmol/m3) m/s).'
            WRITE (out,80) OAE_Tmin(ng), 'OAE_Tmin',                    &
     &            'Alkalinity input start time.'
            WRITE (out,80) OAE_Tmax(ng), 'OAE_Tmax',                    &
     &            'Alkalinity input stop time.'
            WRITE (out,80) OAE_iloc(ng), 'OAE_iloc',                    &
     &            'Location of OAE alkalinity input (i-direction).'
            WRITE (out,80) OAE_jloc(ng), 'OAE_jloc',                    &
     &            'Location of OAE alkalinity input (j-direction).'
# endif
#ifdef TS_DIF2
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu2(i,ng), 'nl_tnu2', i,              &
     &              'NLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu2(i,ng), 'ad_tnu2', i,              &
     &              'ADM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu2(i,ng), 'tl_tnu2', i,              &
     &              'TLM Horizontal, harmonic mixing coefficient',      &
     &              '(m2/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
#ifdef TS_DIF4
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) nl_tnu4(i,ng), 'nl_tnu4', i,              &
     &              'NLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# ifdef ADJOINT
              WRITE (out,100) ad_tnu4(i,ng), 'ad_tnu4', i,              &
     &              'ADM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_tnu4(i,ng), 'tl_tnu4', i,              &
     &              'TLM Horizontal, biharmonic mixing coefficient',    &
     &              '(m4/s) for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerSponge(i,ng)) THEN
                WRITE (out,110) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning ON  sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LtracerSponge(i,ng), 'LtracerSponge',   &
     &              i, 'Turning OFF sponge on tracer ', i,              &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE(out,100) Akt_bak(i,ng), 'Akt_bak', i,               &
     &             'Background vertical mixing coefficient (m2/s)',     &
     &             'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
#ifdef FORWARD_MIXING
            DO itrc=1,NBT
              i=idbio(itrc)
# ifdef ADJOINT
              WRITE (out,100) ad_Akt_fac(i,ng), 'ad_Akt_fac', i,        &
     &              'ADM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
# if defined TANGENT || defined TL_IOMS
              WRITE (out,100) tl_Akt_fac(i,ng), 'tl_Akt_fac', i,        &
     &              'TLM basic state vertical mixing scale factor',     &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
# endif
            END DO
#endif
            DO itrc=1,NBT
              i=idbio(itrc)
              WRITE (out,100) Tnudg(i,ng), 'Tnudg', i,                  &
     &              'Nudging/relaxation time scale (days)',             &
     &              'for tracer ', i, TRIM(Vname(1,idTvar(i)))
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerSrc(i,ng)) THEN
                WRITE (out,110) LtracerSrc(i,ng), 'LtracerSrc',         &
     &              i, 'Turning ON  point sources/Sink on tracer ', i,  &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LtracerSrc(i,ng), 'LtracerSrc',         &
     &              i, 'Turning OFF point sources/Sink on tracer ', i,  &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LtracerCLM(i,ng)) THEN
                WRITE (out,110) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning ON  processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LtracerCLM(i,ng), 'LtracerCLM', i,      &
     &              'Turning OFF processing of climatology tracer ', i, &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            DO itrc=1,NBT
              i=idbio(itrc)
              IF (LnudgeTCLM(i,ng)) THEN
                WRITE (out,110) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning ON  nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LnudgeTCLM(i,ng), 'LnudgeTCLM', i,      &
     &              'Turning OFF nudging of climatology tracer ', i,    &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
              IF (LnudgeTREA(i,ng)) THEN
                WRITE (out,110) LnudgeTREA(i,ng), 'LnudgeTREA', i,      &
     &              'Turning ON  nudging of reanalysis tracer ', i,     &
     &              TRIM(Vname(1,idTvar(i)))
              ELSE
                WRITE (out,110) LnudgeTREA(i,ng), 'LnudgeTREA', i,      &
     &              'Turning OFF nudging of reanalysis tracer ', i,     &
     &              TRIM(Vname(1,idTvar(i)))
              END IF
            END DO
            IF ((nHIS(ng).gt.0).and.ANY(Hout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Hout(idTvar(i),ng)) WRITE (out,120)                 &
     &              Hout(idTvar(i),ng), 'Hout(idTvar)',                 &
     &              'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Hout(idTsur(i),ng)) WRITE (out,120)                 &
     &              Hout(idTsur(i),ng), 'Hout(idTsur)',                 &
     &              'Write out tracer flux ', i,                        &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
            END IF
            IF ((nQCK(ng).gt.0).and.ANY(Qout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idTvar(i),ng)) WRITE (out,120)                 &
     &              Qout(idTvar(i),ng), 'Qout(idTvar)',                 &
     &              'Write out tracer ', i, TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idsurT(i),ng)) WRITE (out,120)                 &
     &              Qout(idsurT(i),ng), 'Qout(idsurT)',                 &
     &              'Write out surface tracer ', i,                     &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Qout(idTsur(i),ng)) WRITE (out,120)                 &
     &              Qout(idTsur(i),ng), 'Qout(idTsur)',                 &
     &              'Write out tracer flux ', i,                        &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
            END IF
#if defined AVERAGES    || \
   (defined AD_AVERAGES && defined ADJOINT) || \
   (defined RP_AVERAGES && defined TL_IOMS) || \
   (defined TL_AVERAGES && defined TANGENT)
            IF ((nAVG(ng).gt.0).and.ANY(Aout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idTvar(i),ng)) WRITE (out,120)                 &
     &              Aout(idTvar(i),ng), 'Aout(idTvar)',                 &
     &              'Write out averaged tracer ', i,                    &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idTTav(i),ng)) WRITE (out,120)                 &
     &              Aout(idTTav(i),ng), 'Aout(idTTav)',                 &
     &              'Write out averaged <t*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idUTav(i),ng)) WRITE (out,120)                 &
     &              Aout(idUTav(i),ng), 'Aout(idUTav)',                 &
     &              'Write out averaged <u*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(idVTav(i),ng)) WRITE (out,120)                 &
     &              Aout(idVTav(i),ng), 'Aout(idVTav)',                 &
     &              'Write out averaged <v*t> for tracer ', i,          &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(iHUTav(i),ng)) WRITE (out,120)                 &
     &              Aout(iHUTav(i),ng), 'Aout(iHUTav)',                 &
     &              'Write out averaged <Huon*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
              DO itrc=1,NBT
                i=idbio(itrc)
                IF (Aout(iHVTav(i),ng)) WRITE (out,120)                 &
     &              Aout(iHVTav(i),ng), 'Aout(iHVTav)',                 &
     &              'Write out averaged <Hvom*t> for tracer ', i,       &
     &              TRIM(Vname(1,idTvar(i)))
              END DO
            END IF
#endif
#ifdef DIAGNOSTICS_TS
            IF ((nDIA(ng).gt.0).and.ANY(Dout(:,ng))) THEN
              WRITE (out,'(1x)')
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTrate),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTrate)',               &
     &                'Write out rate of change of tracer ', itrc,      &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iThadv)',               &
     &                'Write out horizontal advection, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTxadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTxadv)',               &
     &               'Write out horizontal X-advection, tracer ', itrc, &
     &               TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTyadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTyadv)',               &
     &               'Write out horizontal Y-advection, tracer ', itrc, &
     &               TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvadv),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTvadv)',               &
     &                'Write out vertical advection, tracer ', itrc,    &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
# if defined TS_DIF2 || defined TS_DIF4
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iThdif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iThdif)',               &
     &                'Write out horizontal diffusion, tracer ', itrc,  &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(i,iTxdif),ng))                          &
     &            WRITE (out,120) .TRUE., 'Dout(iTxdif)',               &
     &               'Write out horizontal X-diffusion, tracer ', itrc, &
     &               TRIM(Vname(1,idTvar(itrc)))
              END DO
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTydif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTydif)',               &
     &               'Write out horizontal Y-diffusion, tracer ', itrc, &
     &               TRIM(Vname(1,idTvar(itrc)))
              END DO
#  if defined MIX_GEO_TS || defined MIX_ISO_TS
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTsdif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTsdif)',               &
     &               'Write out horizontal S-diffusion, tracer ', itrc, &
     &               TRIM(Vname(1,idTvar(itrc)))
              END DO
#  endif
# endif
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTvdif),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTvdif)',               &
     &                'Write out vertical diffusion, tracer ', itrc,    &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
# if defined DIAG_NUDG
              DO i=1,NBT
                itrc=idbio(i)
                IF (Dout(idDtrc(itrc,iTnudg),ng))                       &
     &            WRITE (out,120) .TRUE., 'Dout(iTnudg)',               &
     &                'Write out nudging effect, tracer ', itrc,    &
     &                TRIM(Vname(1,idTvar(itrc)))
              END DO
# endif
            END IF
#endif
#ifdef DIAGNOSTICS_BIO
            IF (nDIA(ng).gt.0) THEN
              IF (NDbio2d.gt.0) THEN
                DO itrc=1,NDbio2d
                  i=iDbio2(itrc)
                  IF (Dout(i,ng)) WRITE (out,130)                       &
     &                Dout(i,ng), 'Dout(iDbio2)',                       &
     &                'Write out diagnostics for', TRIM(Vname(1,i))
                END DO
              END IF
              DO itrc=1,NDbio3d
                i=iDbio3(itrc)
                IF (Dout(i,ng)) WRITE (out,130)                         &
     &              Dout(i,ng), 'Dout(iDbio3)',                         &
     &              'Write out diagnostics for', TRIM(Vname(1,i))
              END DO
            END IF
#endif
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Rescale biological tracer parameters.
!-----------------------------------------------------------------------
!
!  Take the square root of the biharmonic coefficients so it can
!  be applied to each harmonic operator.
!
      DO ng=1,Ngrids
        DO itrc=1,NBT
          i=idbio(itrc)
          nl_tnu4(i,ng)=SQRT(ABS(nl_tnu4(i,ng)))
#ifdef ADJOINT
          ad_tnu4(i,ng)=SQRT(ABS(ad_tnu4(i,ng)))
#endif
#if defined TANGENT || defined TL_IOMS
          tl_tnu4(i,ng)=SQRT(ABS(tl_tnu4(i,ng)))
#endif
!
!  Compute inverse nudging coefficients (1/s) used in various tasks.
!
          IF (Tnudg(i,ng).gt.0.0_r8) THEN
            Tnudg(i,ng)=1.0_r8/(Tnudg(i,ng)*86400.0_r8)
          ELSE
            Tnudg(i,ng)=0.0_r8
          END IF
        END DO
      END DO

  30  FORMAT (/,' read_BioPar - variable info not yet loaded, ',        &
     &        a,i2.2,a)
  40  FORMAT (/,' read_BioPar - variable info not yet loaded, ',a)
  50  FORMAT (/,' read_BioPar - Error while processing line: ',/,a)
  60  FORMAT (/,/,' Fennel Model Parameters, Grid: ',i2.2,              &
     &        /,  ' =================================',/)
  70  FORMAT (1x,i10,2x,a,t32,a)
  80  FORMAT (1p,e11.4,2x,a,t32,a)
  90  FORMAT (1p,e11.4,2x,a,t32,a,/,t34,a)
 100  FORMAT (1p,e11.4,2x,a,'(',i2.2,')',t32,a,/,t34,a,i2.2,':',1x,a)
 110  FORMAT (10x,l1,2x,a,'(',i2.2,')',t32,a,i2.2,':',1x,a)
 120  FORMAT (10x,l1,2x,a,t32,a,i2.2,':',1x,a)
 130  FORMAT (10x,l1,2x,a,t32,a,1x,a)

      RETURN
      END SUBROUTINE read_BioPar
