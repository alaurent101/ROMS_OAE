      SUBROUTINE read_BioPar (model, inp, out, Lwrite)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads in the Simple BGC model input parameters.        !
!  They are specified in input script "simple.in".                     !
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
!  Read in the Simple BGC model parameters.
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
            CASE ('disso1')
              Npts=load_r(Nval, Rval, Ngrids, disso1)
            CASE ('iloc_particle1')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle1)
            CASE ('jloc_particle1')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle1)
            CASE ('kloc_particle1')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle1)
            CASE ('particle1_load')
              Npts=load_r(Nval, Rval, Ngrids, particle1_load)
            CASE ('particle1_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle1_startload)
            CASE ('particle1_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle1_endload)
            CASE ('wPar1')
              Npts=load_r(Nval, Rval, Ngrids, wPar1)
#   ifdef FULL_DISSOLVE
            CASE ('iloc_dissolve0')
              Npts=load_r(Nval, Rval, Ngrids, iloc_dissolve0)
            CASE ('jloc_dissolve0')
              Npts=load_r(Nval, Rval, Ngrids, jloc_dissolve0)
            CASE ('kloc_dissolve0')
              Npts=load_r(Nval, Rval, Ngrids, kloc_dissolve0)
            CASE ('dissolve0_load')
              Npts=load_r(Nval, Rval, Ngrids, dissolve0_load)
            CASE ('dissolve0_startload')
              Npts=load_r(Nval, Rval, Ngrids, dissolve0_startload)
            CASE ('dissolve0_endload')
              Npts=load_r(Nval, Rval, Ngrids, dissolve0_endload)
#   endif
#   ifdef MULTI_PARTICLES
            CASE ('disso2')
              Npts=load_r(Nval, Rval, Ngrids, disso2)
            CASE ('iloc_particle2')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle2)
            CASE ('jloc_particle2')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle2)
            CASE ('kloc_particle2')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle2)
            CASE ('particle2_load')
              Npts=load_r(Nval, Rval, Ngrids, particle2_load)
            CASE ('particle2_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle2_startload)
            CASE ('particle2_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle2_endload)
            CASE ('wPar2')
              Npts=load_r(Nval, Rval, Ngrids, wPar2)
            CASE ('disso3')
              Npts=load_r(Nval, Rval, Ngrids, disso3)
            CASE ('iloc_particle3')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle3)
            CASE ('jloc_particle3')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle3)
            CASE ('kloc_particle3')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle3)
            CASE ('particle3_load')
              Npts=load_r(Nval, Rval, Ngrids, particle3_load)
            CASE ('particle3_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle3_startload)
            CASE ('particle3_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle3_endload)
            CASE ('wPar3')
              Npts=load_r(Nval, Rval, Ngrids, wPar3)
            CASE ('disso4')
              Npts=load_r(Nval, Rval, Ngrids, disso4)
            CASE ('iloc_particle4')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle4)
            CASE ('jloc_particle4')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle4)
            CASE ('kloc_particle4')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle4)
            CASE ('particle4_load')
              Npts=load_r(Nval, Rval, Ngrids, particle4_load)
            CASE ('particle4_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle4_startload)
            CASE ('particle4_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle4_endload)
            CASE ('wPar4')
              Npts=load_r(Nval, Rval, Ngrids, wPar4)
            CASE ('disso5')
              Npts=load_r(Nval, Rval, Ngrids, disso5)
            CASE ('iloc_particle5')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle5)
            CASE ('jloc_particle5')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle5)
            CASE ('kloc_particle5')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle5)
            CASE ('particle5_load')
              Npts=load_r(Nval, Rval, Ngrids, particle5_load)
            CASE ('particle5_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle5_startload)
            CASE ('particle5_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle5_endload)
            CASE ('wPar5')
              Npts=load_r(Nval, Rval, Ngrids, wPar5)
            CASE ('disso6')
              Npts=load_r(Nval, Rval, Ngrids, disso6)
            CASE ('iloc_particle6')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle6)
            CASE ('jloc_particle6')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle6)
            CASE ('kloc_particle6')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle6)
            CASE ('particle6_load')
              Npts=load_r(Nval, Rval, Ngrids, particle6_load)
            CASE ('particle6_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle6_startload)
            CASE ('particle6_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle6_endload)
            CASE ('wPar6')
              Npts=load_r(Nval, Rval, Ngrids, wPar6)
            CASE ('disso7')
              Npts=load_r(Nval, Rval, Ngrids, disso7)
            CASE ('iloc_particle7')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle7)
            CASE ('jloc_particle7')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle7)
            CASE ('kloc_particle7')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle7)
            CASE ('particle7_load')
              Npts=load_r(Nval, Rval, Ngrids, particle7_load)
            CASE ('particle7_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle7_startload)
            CASE ('particle7_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle7_endload)
            CASE ('wPar7')
              Npts=load_r(Nval, Rval, Ngrids, wPar7)
            CASE ('disso8')
              Npts=load_r(Nval, Rval, Ngrids, disso8)
            CASE ('iloc_particle8')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle8)
            CASE ('jloc_particle8')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle8)
            CASE ('kloc_particle8')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle8)
            CASE ('particle8_load')
              Npts=load_r(Nval, Rval, Ngrids, particle8_load)
            CASE ('particle8_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle8_startload)
            CASE ('particle8_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle8_endload)
            CASE ('wPar8')
              Npts=load_r(Nval, Rval, Ngrids, wPar8)
            CASE ('disso9')
              Npts=load_r(Nval, Rval, Ngrids, disso9)
            CASE ('iloc_particle9')
              Npts=load_r(Nval, Rval, Ngrids, iloc_particle9)
            CASE ('jloc_particle9')
              Npts=load_r(Nval, Rval, Ngrids, jloc_particle9)
            CASE ('kloc_particle9')
              Npts=load_r(Nval, Rval, Ngrids, kloc_particle9)
            CASE ('particle9_load')
              Npts=load_r(Nval, Rval, Ngrids, particle9_load)
            CASE ('particle9_startload')
              Npts=load_r(Nval, Rval, Ngrids, particle9_startload)
            CASE ('particle9_endload')
              Npts=load_r(Nval, Rval, Ngrids, particle9_endload)
            CASE ('wPar9')
              Npts=load_r(Nval, Rval, Ngrids, wPar9)
#   endif
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
            WRITE (out,80) disso1(ng), 'disso1',                        &
     &            'dissolution rate for particle1 (day-1).'
            WRITE (out,80) iloc_particle1(ng), 'iloc_particle1',        &
     &            'longitude (i) index of the model grid cell where particle1 is added.'
            WRITE (out,80) jloc_particle1(ng), 'jloc_particle1',        &
     &            'latitude (j) index of the model grid cell where particle1 is added.'
            WRITE (out,80) kloc_particle1(ng), 'kloc_particle1',        &
     &            'vertical (k) index of the model grid cell where particle1 is added.'
            WRITE (out,80) particle1_load(ng), 'particle1_load',        &
     &            'particle1 added to the grid cell per day (unit of particle1/m2/day).'
            WRITE (out,80) particle1_startload(ng),                     &
     &            'particle1_startload', 'starting day of particle1 load.'
            WRITE (out,80) particle1_endload(ng), 'particle1_endload',  &
     &            'ending day of particle1 load.'
            WRITE (out,80) wPar1(ng), 'wPar1',                          &
     &            'vertical sinking velocity for particle1 (m day-1).'
#ifdef FULL_DISSOLVE
            WRITE (out,80) iloc_dissolve0(ng), 'iloc_dissolve0',        &
     &            'longitude (i) index of the model grid cell where dissolve0 is added.'
            WRITE (out,80) jloc_dissolve0(ng), 'jloc_dissolve0',        &
     &            'latitude (j) index of the model grid cell where dissolve0 is added.'
            WRITE (out,80) kloc_dissolve0(ng), 'kloc_dissolve0',        &
     &            'vertical (k) index of the model grid cell where dissolve0 is added.'
            WRITE (out,80) dissolve0_load(ng), 'dissolve0_load',        &
     &            'dissolve0 added to the grid cell per day (unit of dissolve0/m2/day).'
            WRITE (out,80) dissolve0_startload(ng),                     &
     &            'dissolve0_startload', 'starting day of dissolve0 load.'
            WRITE (out,80) dissolve0_endload(ng), 'dissolve0_endload',  &
     &            'ending day of dissolve0 load.'
#endif 
#ifdef MULTI_PARTICLES
            WRITE (out,80) disso2(ng), 'disso2',                        &
     &            'dissolution rate for particle2 (day-1).'
            WRITE (out,80) iloc_particle2(ng), 'iloc_particle2',        &
     &            'longitude (i) index of the model grid cell where particle2 is added.'
            WRITE (out,80) jloc_particle2(ng), 'jloc_particle2',        &
     &            'latitude (j) index of the model grid cell where particle2 is added.'
            WRITE (out,80) kloc_particle2(ng), 'kloc_particle2',        &
     &            'vertical (k) index of the model grid cell where particle2 is added.'
            WRITE (out,80) particle2_load(ng), 'particle2_load',        &
     &            'particle2 added to the grid cell per day (unit of particle2/m2/day).'
            WRITE (out,80) particle2_startload(ng),                     &
     &            'particle2_startload', 'starting day of particle2 load.'
            WRITE (out,80) particle2_endload(ng), 'particle2_endload',  &
     &            'ending day of particle2 load.'
            WRITE (out,80) wPar2(ng), 'wPar2',                          &
     &            'vertical sinking velocity for particle2 (m day-1).'
            WRITE (out,80) disso3(ng), 'disso3',                        &
     &            'dissolution rate for particle3 (day-1).'
            WRITE (out,80) iloc_particle3(ng), 'iloc_particle3',        &
     &            'longitude (i) index of the model grid cell where particle3 is added.'
            WRITE (out,80) jloc_particle3(ng), 'jloc_particle3',        &
     &            'latitude (j) index of the model grid cell where particle3 is added.'
            WRITE (out,80) kloc_particle3(ng), 'kloc_particle3',        &
     &            'vertical (k) index of the model grid cell where particle3 is added.'
            WRITE (out,80) particle3_load(ng), 'particle3_load',        &
     &            'particle3 added to the grid cell per day (unit of particle3/m2/day).'
            WRITE (out,80) particle3_startload(ng),                     &
     &            'particle3_startload', 'starting day of particle3 load.'
            WRITE (out,80) particle3_endload(ng), 'particle3_endload',  &
     &            'ending day of particle3 load.'
            WRITE (out,80) wPar3(ng), 'wPar3',                          &
     &            'vertical sinking velocity for particle3 (m day-1).'
            WRITE (out,80) disso4(ng), 'disso4',                        &
     &            'dissolution rate for particle4 (day-1).'
            WRITE (out,80) iloc_particle4(ng), 'iloc_particle4',        &
     &            'longitude (i) index of the model grid cell where particle4 is added.'
            WRITE (out,80) jloc_particle4(ng), 'jloc_particle4',        &
     &            'latitude (j) index of the model grid cell where particle4 is added.'
            WRITE (out,80) kloc_particle4(ng), 'kloc_particle4',        &
     &            'vertical (k) index of the model grid cell where particle4 is added.'
            WRITE (out,80) particle4_load(ng), 'particle4_load',        &
     &            'particle4 added to the grid cell per day (unit of particle4/m2/day).'
            WRITE (out,80) particle4_startload(ng),                     &
     &            'particle4_startload', 'starting day of particle4 load.'
            WRITE (out,80) particle4_endload(ng), 'particle4_endload',  &
     &            'ending day of particle4 load.'
            WRITE (out,80) wPar4(ng), 'wPar4',                          &
     &            'vertical sinking velocity for particle4 (m day-1).'
            WRITE (out,80) disso5(ng), 'disso5',                        &
     &            'dissolution rate for particle5 (day-1).'
            WRITE (out,80) iloc_particle5(ng), 'iloc_particle5',        &
     &            'longitude (i) index of the model grid cell where particle5 is added.'
            WRITE (out,80) jloc_particle5(ng), 'jloc_particle5',        &
     &            'latitude (j) index of the model grid cell where particle5 is added.'
            WRITE (out,80) kloc_particle5(ng), 'kloc_particle5',        &
     &            'vertical (k) index of the model grid cell where particle5 is added.'
            WRITE (out,80) particle5_load(ng), 'particle5_load',        &
     &            'particle5 added to the grid cell per day (unit of particle5/m2/day).'
            WRITE (out,80) particle5_startload(ng),                     &
     &            'particle5_startload', 'starting day of particle5 load.'
            WRITE (out,80) particle5_endload(ng), 'particle5_endload',  &
     &            'ending day of particle5 load.'
            WRITE (out,80) wPar5(ng), 'wPar5',                          &
     &            'vertical sinking velocity for particle5 (m day-1).'
            WRITE (out,80) disso6(ng), 'disso6',                        &
     &            'dissolution rate for particle6 (day-1).'
            WRITE (out,80) iloc_particle6(ng), 'iloc_particle6',        &
     &            'longitude (i) index of the model grid cell where particle6 is added.'
            WRITE (out,80) jloc_particle6(ng), 'jloc_particle6',        &
     &            'latitude (j) index of the model grid cell where particle6 is added.'
            WRITE (out,80) kloc_particle6(ng), 'kloc_particle6',        &
     &            'vertical (k) index of the model grid cell where particle6 is added.'
            WRITE (out,80) particle6_load(ng), 'particle6_load',        &
     &            'particle6 added to the grid cell per day (unit of particle6/m2/day).'
            WRITE (out,80) particle6_startload(ng),                     &
     &            'particle6_startload', 'starting day of particle6 load.'
            WRITE (out,80) particle6_endload(ng), 'particle6_endload',  &
     &            'ending day of particle6 load.'
            WRITE (out,80) wPar6(ng), 'wPar6',                          &
     &            'vertical sinking velocity for particle6 (m day-1).'
            WRITE (out,80) disso7(ng), 'disso7',                        &
     &            'dissolution rate for particle7 (day-1).'
            WRITE (out,80) iloc_particle7(ng), 'iloc_particle7',        &
     &            'longitude (i) index of the model grid cell where particle7 is added.'
            WRITE (out,80) jloc_particle7(ng), 'jloc_particle7',        &
     &            'latitude (j) index of the model grid cell where particle7 is added.'
            WRITE (out,80) kloc_particle7(ng), 'kloc_particle7',        &
     &            'vertical (k) index of the model grid cell where particle7 is added.'
            WRITE (out,80) particle7_load(ng), 'particle7_load',        &
     &            'particle7 added to the grid cell per day (unit of particle7/m2/day).'
            WRITE (out,80) particle7_startload(ng),                     &
     &            'particle7_startload', 'starting day of particle7 load.'
            WRITE (out,80) particle7_endload(ng), 'particle7_endload',  &
     &            'ending day of particle7 load.'
            WRITE (out,80) wPar7(ng), 'wPar7',                          &
     &            'vertical sinking velocity for particle7 (m day-1).'
            WRITE (out,80) disso8(ng), 'disso8',                        &
     &            'dissolution rate for particle8 (day-1).'
            WRITE (out,80) iloc_particle8(ng), 'iloc_particle8',        &
     &            'longitude (i) index of the model grid cell where particle8 is added.'
            WRITE (out,80) jloc_particle8(ng), 'jloc_particle8',        &
     &            'latitude (j) index of the model grid cell where particle8 is added.'
            WRITE (out,80) kloc_particle8(ng), 'kloc_particle8',        &
     &            'vertical (k) index of the model grid cell where particle8 is added.'
            WRITE (out,80) particle8_load(ng), 'particle8_load',        &
     &            'particle8 added to the grid cell per day (unit of particle8/m2/day).'
            WRITE (out,80) particle8_startload(ng),                     &
     &            'particle8_startload', 'starting day of particle8 load.'
            WRITE (out,80) particle8_endload(ng), 'particle8_endload',  &
     &            'ending day of particle8 load.'
            WRITE (out,80) wPar8(ng), 'wPar8',                          &
     &            'vertical sinking velocity for particle8 (m day-1).'
            WRITE (out,80) disso9(ng), 'disso9',                        &
     &            'dissolution rate for particle9 (day-1).'
            WRITE (out,80) iloc_particle9(ng), 'iloc_particle9',        &
     &            'longitude (i) index of the model grid cell where particle9 is added.'
            WRITE (out,80) jloc_particle9(ng), 'jloc_particle9',        &
     &            'latitude (j) index of the model grid cell where particle9 is added.'
            WRITE (out,80) kloc_particle9(ng), 'kloc_particle9',        &
     &            'vertical (k) index of the model grid cell where particle9 is added.'
            WRITE (out,80) particle9_load(ng), 'particle9_load',        &
     &            'particle9 added to the grid cell per day (unit of particle9/m2/day).'
            WRITE (out,80) particle9_startload(ng),                     &
     &            'particle9_startload', 'starting day of particle9 load.'
            WRITE (out,80) particle9_endload(ng), 'particle9_endload',  &
     &            'ending day of particle9 load.'
            WRITE (out,80) wPar9(ng), 'wPar9',                          &
     &            'vertical sinking velocity for particle9 (m day-1).'
#endif 
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
  60  FORMAT (/,/,' Dissolution Model Parameters, Grid: ',i2.2,              &
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
