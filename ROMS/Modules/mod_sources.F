#include "cppdefs.h"
      MODULE mod_sources
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Msrc       Maximum number of analytical point Sources/Sinks.        !
!  Nsrc       Number of point Sources/Sinks.                           !
!  Dsrc       Direction of point Sources/Sinks:                        !
!               Dsrc(:) = 0,  Along XI-direction.                      !
!               Dsrc(:) > 0,  Along ETA-direction.                     !
!  Fsrc       Point Source/Sinks identification flag:                  !
!               Fsrc(:) = 0,  All Tracer source/sink are off.          !
!               Fsrc(:) = 1,  Only temperature is on.                  !
!               Fsrc(:) = 2,  Only salinity is on.                     !
!               Fsrc(:) = 3,  Both temperature and salinity are on.    !
!               Fsrc(:) = 4,  Both nitrate and salinity are on.        !
!               Fsrc(:) = ... And other combinations.                  !
!                             (We need a more robust logic here)       !
!  Isrc       I-grid location of point Sources/Sinks,                  !
!               1 =< Isrc =< Lm(ng).                                   !
!  Jsrc       J-grid location of point Sources/Sinks,                  !
!               1 =< Jsrc =< Mm(ng).                                   !
!  Added Imsrc & Jmsrc 2021/2/15                                     !
!  Imsrc       I-grid location of river mouths,                        !
!               1 =< Imsrc =< Lm(ng).                                  !
!  Jmsrc       J-grid location of river mouths,                        !
!               1 =< Jmsrc =< Mm(ng).                                  !
!  Qbar       Vertically integrated mass transport (m3/s) of point     !
!               Sources/Sinks at U- or V-points:                       !
!               Qbar -> positive, if the mass transport is in the      !
!                       positive U- or V-direction.                    !
!               Qbar -> negative, if the mass transport is in the      !
!                       negative U- or V-direction.                    !
!  QbarG      Latest two-time snapshots of vertically integrated       !
!               mass transport (m3/s) of point Sources/Sinks.          !
!  Qshape     Nondimensional shape function to distribute mass         !
!               mass point Sources/Sinks vertically.                   !
!  Qsrc       Mass transport profile (m3/s) of point Sources/Sinks.    !
!  Tsrc       Tracer (tracer units) point Sources/Sinks.               !
!  TsrcG      Latest two-time snapshots of tracer (tracer units)       !
!               point Sources/Sinks.                                   !
!  Added Tmpm 2021/6/24                                                !
!  Tmpm       SST near river mouth, used at river heads                !
!                                                                      !
!=======================================================================
!
        USE mod_kinds
        USE mod_param
!
        implicit none
!
        integer, allocatable :: Msrc(:)
        integer, allocatable :: Nsrc(:)
!
        TYPE T_SOURCES

          integer, pointer :: Isrc(:)
          integer, pointer :: Jsrc(:)
!  Added Imsrc & Jmsrc 2021/2/15
#ifdef RIVER_JI
          integer, pointer :: Imsrc(:)
          integer, pointer :: Jmsrc(:)
#endif
          real(r8), pointer :: Dsrc(:)
          real(r8), pointer :: Fsrc(:)
          real(r8), pointer :: Qbar(:)
          real(r8), pointer :: Qshape(:,:)
          real(r8), pointer :: Qsrc(:,:)
          real(r8), pointer :: Tsrc(:,:,:)
          real(r8), pointer :: Xsrc(:)
          real(r8), pointer :: Ysrc(:)
!  Added Xmsrc & Ymsrc 2021/2/15
#ifdef RIVER_JI
          real(r8), pointer :: Xmsrc(:)
          real(r8), pointer :: Ymsrc(:)
#endif
#ifndef ANA_PSOURCE
          real(r8), pointer :: QbarG(:,:)
          real(r8), pointer :: TsrcG(:,:,:,:)
#endif

! Added Tmpm 2021/6/24
#ifdef RIVER_JI
          real(r8), pointer :: Tmpm(:)
#endif

#ifdef ADJOINT
          real(r8), pointer :: ad_Qbar(:)
          real(r8), pointer :: ad_Qsrc(:,:)
          real(r8), pointer :: ad_Tsrc(:,:,:)
#endif

#ifdef TANGENT
          real(r8), pointer :: tl_Qbar(:)
          real(r8), pointer :: tl_Qsrc(:,:)
          real(r8), pointer :: tl_Tsrc(:,:,:)
#endif

        END TYPE T_SOURCES

        TYPE (T_SOURCES), allocatable :: SOURCES(:)
!
      CONTAINS
!
      SUBROUTINE allocate_sources (ng)
!
!=======================================================================
!                                                                      !
!  This routine allocates and initializes all variables in the module  !
!  for all nested grids.                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
#ifndef ANA_PSOURCE
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
#endif
!
      USE strings_mod, ONLY : FoundError
!
!  Imported variable declarations.
!
      integer :: ng
!
!  Local variable declarations.
!
#ifndef ANA_PSOURCE
      logical :: foundit
!
      integer :: Vid, ifile, nvatt, nvdim
#endif
      integer :: is, itrc, k, mg

      real(r8), parameter :: IniVal = 0.0_r8
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__//", allocate_sources"
!
!-----------------------------------------------------------------------
!  Allocate module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(Msrc)) THEN
        allocate ( Msrc(Ngrids) )
      END IF

      IF (.not.allocated(Nsrc)) THEN
        allocate ( Nsrc(Ngrids) )
      END IF

#ifndef ANA_PSOURCE
!
!  Inquire about the number of point sources.
!
      IF (ng.eq.1) THEN
        DO mg=1,Ngrids
          foundit=.FALSE.
          IF (LuvSrc(mg).or.LwSrc(mg).or.ANY(LtracerSrc(:,mg))) THEN
            CALL netcdf_inq_var (ng, iNLM, SSF(mg)%name,                &
     &                           MyVarName = TRIM(Vname(1,idRxpo)),     &
     &                           SearchVar = foundit,                   &
     &                           VarID = Vid,                           &
     &                           nVardim = nvdim,                       &
     &                           nVarAtt = nvatt)
            IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

            IF (foundit) THEN
              Nsrc(mg)=var_Dsize(1)         ! first dimension
              Msrc(mg)=Nsrc(mg)
            END IF
          END IF
        END DO
      END IF
#else
!
!  Set number of point sources to maximum number of analytical sources.
!  Notice that a maximum of 200 analytical sources are set-up here.
!
      Msrc(ng)=200
      Nsrc(ng)=Msrc(ng)
#endif
!
!  Allocate structure.
!
      IF (ng.eq.1) allocate ( SOURCES(Ngrids) )
!
!  Allocate point Sources/Sinks variables.
!
      allocate ( SOURCES(ng) % Isrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % Jsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

!  Added Imsrc & Jmsrc 2021/2/15
#ifdef RIVER_JI
      allocate ( SOURCES(ng) % Imsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % Jmsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)
#endif

      allocate ( SOURCES(ng) % Dsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % Fsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % Qbar(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % Qshape(Nsrc(ng),N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng)*N(ng),r8)

      allocate ( SOURCES(ng) % Qsrc(Nsrc(ng),N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng)*N(ng),r8)

      allocate ( SOURCES(ng) % Tsrc(Nsrc(ng),N(ng),NT(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng)*N(ng)*NT(ng),r8)

      allocate ( SOURCES(ng) % Xsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % Ysrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

!  Added Xmsrc & Ymsrc 2021/2/15
#ifdef RIVER_JI
      allocate ( SOURCES(ng) % Xmsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % Ymsrc(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)
#endif

#ifndef ANA_PSOURCE
      allocate ( SOURCES(ng) % QbarG(Nsrc(ng),2) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % TsrcG(Nsrc(ng),N(ng),2,NT(ng)) )
      Dmem(ng)=Dmem(ng)+2.0_r8*REAL(Nsrc(ng)*N(ng)*NT(ng),r8)
#endif

!  Added Tmpm 2021/6/24
#ifdef RIVER_JI
      allocate ( SOURCES(ng) % Tmpm(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)
#endif

#ifdef ADJOINT
      allocate ( SOURCES(ng) % ad_Qbar(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % ad_Qsrc(Nsrc(ng),N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng)*N(ng),r8)

      allocate ( SOURCES(ng) % ad_Tsrc(Nsrc(ng),N(ng),NT(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng)*N(ng)*NT(ng),r8)
#endif

#ifdef TANGENT
      allocate ( SOURCES(ng) % tl_Qbar(Nsrc(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng),r8)

      allocate ( SOURCES(ng) % tl_Qsrc(Nsrc(ng),N(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng)*N(ng),r8)

      allocate ( SOURCES(ng) % tl_Tsrc(Nsrc(ng),N(ng),NT(ng)) )
      Dmem(ng)=Dmem(ng)+REAL(Nsrc(ng)*N(ng)*NT(ng),r8)
#endif

!
!-----------------------------------------------------------------------
!  Initialize module variables.
!-----------------------------------------------------------------------
!
      DO is=1,Nsrc(ng)
        SOURCES(ng) % Isrc(is) = 0
        SOURCES(ng) % Jsrc(is) = 0
!  Added Imsrc & Jmsrc 2021/2/15
#ifdef RIVER_JI
        SOURCES(ng) % Imsrc(is) = 0
        SOURCES(ng) % Jmsrc(is) = 0
#endif
        SOURCES(ng) % Dsrc(is) = IniVal
        SOURCES(ng) % Fsrc(is) = IniVal
        SOURCES(ng) % Xsrc(is) = IniVal
        SOURCES(ng) % Ysrc(is) = IniVal
!  Added Xmsrc & Ymsrc 2021/2/15
#ifdef RIVER_JI
        SOURCES(ng) % Xmsrc(is) = IniVal
        SOURCES(ng) % Ymsrc(is) = IniVal
#endif
        SOURCES(ng) % Qbar(is) = IniVal
#ifndef ANA_PSOURCE
        SOURCES(ng) % QbarG(is,1) = IniVal
        SOURCES(ng) % QbarG(is,2) = IniVal
#endif
#ifdef ADJOINT
        SOURCES(ng) % ad_Qbar(is) = IniVal
#endif
#ifdef TANGENT
        SOURCES(ng) % tl_Qbar(is) = IniVal
#endif
! Added Tmmpm 2021/6/24
#ifdef RIVER_JI
        SOURCES(ng) % Tmpm(is) = IniVal
#endif
      END DO
      DO k=1,N(ng)
        DO is=1,Nsrc(ng)
          SOURCES(ng) % Qshape(is,k) = IniVal
          SOURCES(ng) % Qsrc(is,k) = IniVal
#ifdef ADJOINT
          SOURCES(ng) % ad_Qsrc(is,k) = IniVal
#endif
#ifdef TANGENT
          SOURCES(ng) % tl_Qsrc(is,k) = IniVal
#endif
        END DO
      END DO
      DO itrc=1,NT(ng)
        DO k=1,N(ng)
          DO is=1,Nsrc(ng)
            SOURCES(ng) % Tsrc(is,k,itrc) = IniVal
#ifdef ADJOINT
            SOURCES(ng) % ad_Tsrc(is,k,itrc) = IniVal
#endif
#ifdef TANGENT
            SOURCES(ng) % tl_Tsrc(is,k,itrc) = IniVal
#endif
#ifndef ANA_PSOURCE
            SOURCES(ng) % TsrcG(is,k,1,itrc) = IniVal
            SOURCES(ng) % TsrcG(is,k,2,itrc) = IniVal
#endif
          END DO
        END DO
      END DO
!
      RETURN
      END SUBROUTINE allocate_sources
      END MODULE mod_sources
