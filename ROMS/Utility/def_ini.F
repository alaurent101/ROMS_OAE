#include "cppdefs.h"
#ifdef FOUR_DVAR
      SUBROUTINE def_ini (ng)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine open existing nonlinear model initial conditions file  !
!  and either define new new variables or inquire about variable IDs.  !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE def_var_mod, ONLY : def_var
      USE strings_mod, ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
# ifdef ADJUST_BOUNDARY
      logical :: got_IorJ, got_boundary, got_obc_adjust
# endif
# if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
      logical :: got_frc_adjust
# endif
      logical :: got_var(NV)
      logical :: Ldefine = .FALSE.
!
      integer, parameter :: Natt = 25

      integer :: i, j, ifield, itrc, status
      integer :: Fcount
# ifdef ADJUST_BOUNDARY
      integer :: IorJdim, brecdim, brecsize
# endif
# if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
      integer :: frecdim, frecsize
# endif
      integer :: DimIDs(nDimID)
# ifdef ADJUST_BOUNDARY
      integer :: t2dobc(4)
#  ifdef SOLVE3D
      integer :: t3dobc(5)
#  endif
# endif
# if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
      integer :: t4dfrc(4), u4dfrc(4), v4dfrc(4)
# endif
      integer :: def_dim
!
      real(r8) :: Aval(6)
!
      character (len=120) :: Vinfo(Natt)
      character (len=256) :: ncname

      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
      SourceFile=MyFile

# if !defined CORRELATION     && \
     (defined ADJUST_BOUNDARY || defined ADJUST_STFLUX || \
      defined ADJUST_WSTRESS)
!
!=======================================================================
!  Open existing nonlinear model initial conditions and define new
!  variables.
!=======================================================================
!
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
      ncname=INI(ng)%name
!
!  Open initialization file for read/write.
!
      CALL netcdf_open (ng, iNLM, ncname, 1, INI(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) THEN
        IF (Master) WRITE (stdout,10) TRIM(ncname)
        RETURN
      END IF
!
!  Inquire about the dimensions and check for consistency.
!
      CALL netcdf_check_dim (ng, iNLM, ncname,                          &
     &                       ncid = INI(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, iNLM, ncname,                            &
     &                     ncid = INI(ng)%ncid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Check if surface forcing variables have been already defined.
!
      DO i=1,NV
        got_var(i)=.FALSE.
      END DO
!
      DO i=1,n_var
#  ifdef ADJUST_BOUNDARY
        IF (TRIM(var_name(i)).eq.                                       &
     &      TRIM(Vname(1,idSbry(isFsur)))) THEN
          got_var(idSbry(isFsur))=.TRUE.
          INI(ng)%Vid(idSbry(isFsur))=var_id(i)
        ELSE IF (TRIM(var_name(i)).eq.                                  &
     &           TRIM(Vname(1,idSbry(isUbar)))) THEN
          got_var(idSbry(isUbar))=.TRUE.
          INI(ng)%Vid(idSbry(isUbar))=var_id(i)
        ELSE IF (TRIM(var_name(i)).eq.                                  &
     &           TRIM(Vname(1,idSbry(isVbar)))) THEN
          got_var(idSbry(isVbar))=.TRUE.
          INI(ng)%Vid(idSbry(isVbar))=var_id(i)
#   ifdef SOLVE3D
        ELSE IF (TRIM(var_name(i)).eq.                                  &
     &           TRIM(Vname(1,idSbry(isUvel)))) THEN
          got_var(idSbry(isUvel))=.TRUE.
          INI(ng)%Vid(idSbry(isUvel))=var_id(i)
        ELSE IF (TRIM(var_name(i)).eq.                                  &
     &           TRIM(Vname(1,idSbry(isVvel)))) THEN
          got_var(idSbry(isVvel))=.TRUE.
          INI(ng)%Vid(idSbry(isVvel))=var_id(i)
#   endif
        END IF
#   ifdef SOLVE3D
        DO itrc=1,NT(ng)
          IF (TRIM(var_name(i)).eq.                                     &
     &        TRIM(Vname(1,idSbry(isTvar(itrc))))) THEN
            got_var(idSbry(isTvar(itrc)))=.TRUE.
            INI(ng)%Vid(idSbry(isTvar(itrc)))=var_id(i)
          END IF
        END DO
#   endif
#  endif
#  ifdef ADJUST_WSTRESS
        IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUsms))) THEN
          got_var(idUsms)=.TRUE.
          INI(ng)%Vid(idUsms)=var_id(i)
          IF (var_ndim(i).ne.4) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idUsms)),                  &
     &                          var_ndim(i), 'frc_adjust',              &
     &                          TRIM(ncname)
            END IF
            exit_flag=2
            RETURN
          END IF
        ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVsms))) THEN
          got_var(idVsms)=.TRUE.
          INI(ng)%Vid(idVsms)=var_id(i)
          IF (var_ndim(i).ne.4) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idUsms)),                  &
     &                          var_ndim(i), 'frc_adjust',              &
     &                          TRIM(ncname)
            END IF
            exit_flag=2
            RETURN
          END IF
        END IF
#  endif
#  if defined ADJUST_STFLUX && defined SOLVE3D
        DO itrc=1,NT(ng)
          IF (Lstflux(itrc,ng)) THEN
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTsur(itrc)))) THEN
              got_var(idTsur(itrc))=.TRUE.
              INI(ng)%Vid(idTsur(itrc))=var_id(i)
              IF (var_ndim(i).ne.4) THEN
                IF (Master) THEN
                  WRITE (stdout,20) TRIM(Vname(1,idTsur(itrc))),        &
     &                              var_ndim(i), 'frc_adjust',          &
     &                              TRIM(ncname)
                 END IF
                 exit_flag=2
                 RETURN
               END IF
            END IF
          END IF
        END DO
#  endif
      END DO

#  ifdef ADJUST_BOUNDARY
      IF (.not.got_var(idSbry(isFsur))) Ldefine=.TRUE.
      IF (.not.got_var(idSbry(isUbar))) Ldefine=.TRUE.
      IF (.not.got_var(idSbry(isVbar))) Ldefine=.TRUE.
#   ifdef SOLVE3D
      IF (.not.got_var(idSbry(isUvel))) Ldefine=.TRUE.
      IF (.not.got_var(idSbry(isVvel))) Ldefine=.TRUE.
      DO itrc=1,NT(ng)
        IF (.not.got_var(idSbry(isTvar(itrc))).and.                     &
     &      ANY(Lobc(:,isTvar(itrc),ng))) Ldefine=.TRUE.
      END DO
#   endif
#  endif
#  ifdef ADJUST_WSTRESS
      IF (.not.got_var(idUsms)) Ldefine=.TRUE.
      IF (.not.got_var(idVsms)) Ldefine=.TRUE.
#  endif
#  if defined ADJUST_STFLUX && defined SOLVE3D
      DO itrc=1,NT(ng)
        IF (Lstflux(itrc,ng)) THEN
          IF (.not.got_var(idTsur(itrc))) Ldefine=.TRUE.
        END IF
      END DO
#  endif
!
!  Put existing file into define mode so new variables can be added.
!
      IF (Ldefine) THEN
        CALL netcdf_redef (ng, iNLM, ncname, INI(ng)%ncid)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) THEN
          IF (Master) WRITE (stdout,30) TRIM(ncname)
          RETURN
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Define the dimensions of staggered fields.
!-----------------------------------------------------------------------
!
      DEFINE: IF (Ldefine) THEN

#  ifdef ADJUST_BOUNDARY
        got_IorJ=.FALSE.
        got_boundary=.FALSE.
        got_obc_adjust=.FALSE.
#  endif
#  if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
        got_frc_adjust=.FALSE.
#  endif
        DO i=1,n_dim
          SELECT CASE (TRIM(ADJUSTL(dim_name(i))))
            CASE ('xi_rho')
              DimIDs( 1)=dim_id(i)
            CASE ('xi_u')
              DimIDs( 2)=dim_id(i)
            CASE ('xi_v')
              DimIDs( 3)=dim_id(i)
            CASE ('eta_rho')
              DimIDs( 5)=dim_id(i)
            CASE ('eta_u')
              DimIDs( 6)=dim_id(i)
            CASE ('eta_v')
              DimIDs( 7)=dim_id(i)
#  ifdef SOLVE3D
            CASE ('s_rho')
              DimIDs( 9)=dim_id(i)
            CASE ('s_w')
              DimIDs(10)=dim_id(i)
#  endif
#  ifdef ADJUST_BOUNDARY
            CASE ('boundary')
              DimIDs(14)=dim_id(i)
              got_boundary=.TRUE.
            CASE ('IorJ')
              IorJdim=dim_id(i)
              got_IorJ=.TRUE.
#  endif
#  if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
            CASE ('frc_adjust')
              frecdim=dim_id(i)
              frecsize=dim_size(i)
              got_frc_adjust=.TRUE.
#  endif
#  ifdef ADJUST_BOUNDARY
            CASE ('obc_adjust')
              brecdim=dim_id(i)
              brecsize=dim_size(i)
              got_obc_adjust=.TRUE.
#  endif
          END SELECT
        END DO

        DimIDs(12)=rec_id
#  ifdef ADJUST_BOUNDARY
        IF (.not.got_boundary) THEN
          status=def_dim(ng, iNLM, INI(ng)%ncid, ncname, 'boundary',    &
     &                   4, DimIDs(14))
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
        IF (.not.got_IorJ) THEN
          status=def_dim(ng, iNLM, INI(ng)%ncid, ncname, 'IorJ',        &
     &                   IOBOUNDS(ng)%IorJ, IorJdim)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
#  if defined ADJUST_STFLUX || defined ADJUST_WSTRESS
        IF (.not.got_frc_adjust) THEN
          status=def_dim(ng, iNLM, INI(ng)%ncid, ncname, 'frc_adjust',  &
     &                   Nfrec(ng), frecdim)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
#  ifdef ADJUST_BOUNDARY
        IF (.not.got_obc_adjust) THEN
          status=def_dim(ng, iNLM, INI(ng)%ncid, ncname, 'obc_adjust',  &
     &                   Nbrec(ng), brecdim)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
!
!  Define dimension vectors for staggered tracer type variables.
!
#  ifdef ADJUST_BOUNDARY
        t2dobc(1)=IorJdim
        t2dobc(2)=DimIDs(14)
        t2dobc(3)=brecdim
        t2dobc(4)=DimIDs(12)
#   ifdef SOLVE3D
        t3dobc(1)=IorJdim
        t3dobc(2)=DimIDs( 9)
        t3dobc(3)=DimIDs(14)
        t3dobc(4)=brecdim
        t3dobc(5)=DimIDs(12)
#   endif
#  endif
#  ifdef ADJUST_STFLUX
        t4dfrc(1)=DimIDs( 1)
        t4dfrc(2)=DimIDs( 5)
        t4dfrc(3)=frecdim
        t4dfrc(4)=DimIDs(12)
#  endif
#  ifdef ADJUST_WSTRESS
!
!  Define dimension vectors for staggered u-momentum type variables.
!
        u4dfrc(1)=DimIDs( 2)
        u4dfrc(2)=DimIDs( 6)
        u4dfrc(3)=frecdim
        u4dfrc(4)=DimIDs(12)
#  endif
#  ifdef ADJUST_WSTRESS
!
!  Define dimension vectors for staggered v-momentum type variables.
!
        v4dfrc(1)=DimIDs( 3)
        v4dfrc(2)=DimIDs( 7)
        v4dfrc(3)=frecdim
        v4dfrc(4)=DimIDs(12)
#  endif
!
!  Initialize local information variable arrays.
!
        DO i=1,Natt
          DO j=1,LEN(Vinfo(1))
            Vinfo(i)(j:j)=' '
          END DO
        END DO
        DO i=1,6
          Aval(i)=0.0_r8
        END DO
!
!-----------------------------------------------------------------------
!  Define additional variables. Notice that these variables have their
!  own fixed time-dimension to allow 4DVAR adjustments at other times
!  in addition to initialization time.
!-----------------------------------------------------------------------

#  if defined ADJUST_BOUNDARY && !defined RBL4DVAR_FCT_SENSITIVITY
!
!  Define free-surface open boundaries.
!
        IF (.not.got_var(idSbry(isFsur)).and.                           &
     &      ANY(Lobc(:,isFsur,ng))) THEN
          ifield=idSbry(isFsur)
          Vinfo( 1)=Vname(1,ifield)
          Vinfo( 2)=Vname(2,ifield)
          Vinfo( 3)=Vname(3,ifield)
          Vinfo(14)=Vname(4,ifield)
          Vinfo(16)=Vname(1,idtime)
          Aval(5)=REAL(Iinfo(1,ifield,ng),r8)
          status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(ifield),   &
     &                   NF_FOUT, 4, t2dobc, Aval, Vinfo, ncname,       &
     &                   SetFillVal = .FALSE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D U-momentum component open boundaries.
!
        IF (.not.got_var(idSbry(isUbar)).and.                           &
     &      ANY(Lobc(:,isUbar,ng))) THEN
          ifield=idSbry(isUbar)
          Vinfo( 1)=Vname(1,ifield)
          Vinfo( 2)=Vname(2,ifield)
          Vinfo( 3)=Vname(3,ifield)
          Vinfo(14)=Vname(4,ifield)
          Vinfo(16)=Vname(1,idtime)
          Aval(5)=REAL(Iinfo(1,ifield,ng),r8)
          status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(ifield),   &
     &                   NF_FOUT, 4, t2dobc, Aval, Vinfo, ncname,       &
     &                   SetFillVal = .FALSE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 2D V-momentum component open boundaries.
!
        IF (.not.got_var(idSbry(isVbar)).and.                           &
     &      ANY(Lobc(:,isVbar,ng))) THEN
          ifield=idSbry(isVbar)
          Vinfo( 1)=Vname(1,ifield)
          Vinfo( 2)=Vname(2,ifield)
          Vinfo( 3)=Vname(3,ifield)
          Vinfo(14)=Vname(4,ifield)
          Vinfo(16)=Vname(1,idtime)
          Aval(5)=REAL(Iinfo(1,ifield,ng),r8)
          status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(ifield),   &
     &                   NF_FOUT, 4, t2dobc, Aval, Vinfo, ncname,       &
     &                   SetFillVal = .FALSE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF

#   ifdef SOLVE3D
!
!  Define 3D U-momentum component open boundaries.
!
        IF (.not.got_var(idSbry(isUvel)).and.                           &
     &      ANY(Lobc(:,isUvel,ng))) THEN
          ifield=idSbry(isUvel)
          Vinfo( 1)=Vname(1,ifield)
          Vinfo( 2)=Vname(2,ifield)
          Vinfo( 3)=Vname(3,ifield)
          Vinfo(14)=Vname(4,ifield)
          Vinfo(16)=Vname(1,idtime)
          Aval(5)=REAL(Iinfo(1,ifield,ng),r8)
          status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(ifield),   &
     &                   NF_FOUT, 5, t3dobc, Aval, Vinfo, ncname,       &
     &                   SetFillVal = .FALSE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define 3D V-momentum component open boundaries.
!
        IF (.not.got_var(idSbry(isVvel)).and.                           &
     &      ANY(Lobc(:,isVvel,ng))) THEN
          ifield=idSbry(isVvel)
          Vinfo( 1)=Vname(1,ifield)
          Vinfo( 2)=Vname(2,ifield)
          Vinfo( 3)=Vname(3,ifield)
          Vinfo(14)=Vname(4,ifield)
          Vinfo(16)=Vname(1,idtime)
          Aval(5)=REAL(Iinfo(1,ifield,ng),r8)
          status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(ifield),   &
     &                   NF_FOUT, 5, t3dobc, Aval, Vinfo, ncname,       &
     &                   SetFillVal = .FALSE.)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define tracer type variables open boundaries.
!
        DO itrc=1,NT(ng)
          IF (.not.got_var(idSbry(isTvar(itrc))).and.                   &
     &        ANY(Lobc(:,isTvar(itrc),ng))) THEN
            ifield=idSbry(isTvar(itrc))
            Vinfo( 1)=Vname(1,ifield)
            Vinfo( 2)=Vname(2,ifield)
            Vinfo( 3)=Vname(3,ifield)
            Vinfo(14)=Vname(4,ifield)
            Vinfo(16)=Vname(1,idtime)
            Aval(5)=REAL(Iinfo(1,ifield,ng),r8)
            status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(ifield), &
     &                     NF_FOUT, 5, t3dobc, Aval, Vinfo, ncname,     &
     &                     SetFillVal = .FALSE.)
            IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
          END IF
        END DO
#   endif
#  endif
#  ifdef ADJUST_WSTRESS
!
!  Define surface U-momentum stress.
!
        IF (.not.got_var(idUsms)) THEN
          Vinfo( 1)=Vname(1,idUsms)
          Vinfo( 2)=Vname(2,idUsms)
          Vinfo( 3)=Vname(3,idUsms)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_u'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(u2dvar,r8)
          status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(idUsms),   &
     &                   NF_FOUT, 4, u4dfrc, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
!
!  Define surface V-momentum stress.
!
        IF (.not.got_var(idVsms)) THEN
          Vinfo( 1)=Vname(1,idVsms)
          Vinfo( 2)=Vname(2,idVsms)
          Vinfo( 3)=Vname(3,idVsms)
#   if defined WRITE_WATER && defined MASKING
          Vinfo(20)='mask_v'
#   endif
          Vinfo(22)='coordinates'
          Aval(5)=REAL(v2dvar,r8)
          status=def_var(ng, iNLM, INI(ng)%ncid, INI(ng)%Vid(idVsms),   &
     &                   NF_FOUT, 4, v4dfrc, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
        END IF
#  endif
#  if defined ADJUST_STFLUX && defined SOLVE3D
!
!  Define surface tracer flux.
!
        DO itrc=1,NT(ng)
          IF (.not.got_var(idTsur(itrc)).and.Lstflux(itrc,ng)) THEN
            Vinfo( 1)=Vname(1,idTsur(itrc))
            Vinfo( 2)=Vname(2,idTsur(itrc))
            Vinfo( 3)=Vname(3,idTsur(itrc))
            IF (itrc.eq.itemp) THEN
              Vinfo(11)='upward flux, cooling'
              Vinfo(12)='downward flux, heating'
            ELSE IF (itrc.eq.isalt) THEN
              Vinfo(11)='upward flux, freshening (net precipitation)'
              Vinfo(12)='downward flux, salting (net evaporation)'
            END IF
#   if defined WRITE_WATER && defined MASKING
            Vinfo(20)='mask_rho'
#   endif
            Vinfo(22)='coordinates'
            Aval(5)=REAL(r2dvar,r8)
            status=def_var(ng, iNLM, INI(ng)%ncid,                      &
     &                     INI(ng)%Vid(idTsur(itrc)), NF_FOUT,          &
     &                     4, t4dfrc, Aval, Vinfo, ncname)
            IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
          END IF
        END DO
#  endif
!
!-----------------------------------------------------------------------
!  Leave definition mode.
!-----------------------------------------------------------------------
!
        CALL netcdf_enddef (ng, iNLM, ncname, INI(ng)%ncid)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

      END IF DEFINE
# endif
!
!=======================================================================
!  Open an existing initialization file, check its contents, and
!  prepare for appending data.
!=======================================================================
!
      IF (.not.LdefINI(ng)) THEN
        ncname=INI(ng)%name
!
!  Open initialization file for read/write.
!
        IF (INI(ng)%ncid.eq.-1) THEN
          CALL netcdf_open (ng, iNLM, ncname, 1, INI(ng)%ncid)
          IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) THEN
            WRITE (stdout,10) TRIM(ncname)
            RETURN
          END IF
        END IF
!
!  Inquire about the dimensions and check for consistency.
!
        CALL netcdf_check_dim (ng, iNLM, ncname,                        &
     &                         ncid = INI(ng)%ncid)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Inquire about the variables.
!
        CALL netcdf_inq_var (ng, iNLM, ncname,                          &
     &                       ncid = INI(ng)%ncid)
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Initialize logical switches.
!
        DO i=1,NV
          got_var(i)=.FALSE.
        END DO
!
!  Scan variable list from input NetCDF and activate switches for
!  initialization variables. Get variable IDs.
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
            INI(ng)%Vid(idtime)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idFsur))) THEN
            got_var(idFsur)=.TRUE.
            INI(ng)%Vid(idFsur)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUbar))) THEN
            got_var(idUbar)=.TRUE.
            INI(ng)%Vid(idUbar)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVbar))) THEN
            got_var(idVbar)=.TRUE.
            INI(ng)%Vid(idVbar)=var_id(i)
# ifdef ADJUST_BOUNDARY
          ELSE IF (TRIM(var_name(i)).eq.                                &
     &             TRIM(Vname(1,idSbry(isFsur)))) THEN
            got_var(idSbry(isFsur))=.TRUE.
            INI(ng)%Vid(idSbry(isFsur))=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.                                &
     &             TRIM(Vname(1,idSbry(isUbar)))) THEN
            got_var(idSbry(isUbar))=.TRUE.
            INI(ng)%Vid(idSbry(isUbar))=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.                                &
     &             TRIM(Vname(1,idSbry(isVbar)))) THEN
            got_var(idSbry(isVbar))=.TRUE.
            INI(ng)%Vid(idSbry(isVbar))=var_id(i)
# endif
# ifdef ADJUST_WSTRESS
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUsms))) THEN
            got_var(idUsms)=.TRUE.
            INI(ng)%Vid(idUsms)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVsms))) THEN
            got_var(idVsms)=.TRUE.
            INI(ng)%Vid(idVsms)=var_id(i)
# endif
# ifdef SOLVE3D
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUvel))) THEN
            got_var(idUvel)=.TRUE.
            INI(ng)%Vid(idUvel)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVvel))) THEN
            got_var(idVvel)=.TRUE.
            INI(ng)%Vid(idVvel)=var_id(i)
#  ifdef ADJUST_BOUNDARY
          ELSE IF (TRIM(var_name(i)).eq.                                &
     &             TRIM(Vname(1,idSbry(isUvel)))) THEN
            got_var(idSbry(isUvel))=.TRUE.
            INI(ng)%Vid(idSbry(isUvel))=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.                                &
     &             TRIM(Vname(1,idSbry(isVvel)))) THEN
            got_var(idSbry(isVvel))=.TRUE.
            INI(ng)%Vid(idSbry(isVvel))=var_id(i)
#  endif
#  if defined BVF_MIXING || defined LMD_MIXING || \
      defined GLS_MIXING || defined MY25_MIXING
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVvis))) THEN
            got_var(idVvis)=.TRUE.
            INI(ng)%Vid(idVvis)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTdif))) THEN
            got_var(idTdif)=.TRUE.
            INI(ng)%Vid(idTdif)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idSdif))) THEN
            got_var(idSdif)=.TRUE.
            INI(ng)%Vid(idSdif)=var_id(i)
#  endif
# endif
          END IF
# ifdef SOLVE3D
          DO itrc=1,NT(ng)
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTvar(itrc)))) THEN
              got_var(idTvar(itrc))=.TRUE.
              INI(ng)%Tid(itrc)=var_id(i)
#  ifdef ADJUST_BOUNDARY
            ELSE IF (TRIM(var_name(i)).eq.                              &
     &               TRIM(Vname(1,idSbry(isTvar(itrc))))) THEN
              got_var(idSbry(isTvar(itrc)))=.TRUE.
              INI(ng)%Vid(idSbry(isTvar(itrc)))=var_id(i)
#  endif
#  ifdef ADJUST_STFLUX
            ELSE IF (TRIM(var_name(i)).eq.                              &
     &               TRIM(Vname(1,idTsur(itrc)))) THEN
              got_var(idTsur(itrc))=.TRUE.
              INI(ng)%Vid(idTsur(itrc))=var_id(i)
#  endif
            END IF
          END DO
# endif
        END DO
!
!  Check if initialization variables are available in input NetCDF
!  file.
!
        IF (.not.got_var(idtime)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idtime)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idFsur)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idFsur)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idUbar)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idUbar)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVbar)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idVbar)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# if defined ADJUST_BOUNDARY && !defined RBL4DVAR_FCT_SENSITIVITY
        IF (.not.got_var(idSbry(isFsur)).and.                           &
     &      ANY(Lobc(:,isFsur,ng))) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idSbry(isFsur))),  &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idSbry(isUbar)).and.                           &
     &      ANY(Lobc(:,isUbar,ng))) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idSbry(isUbar))),  &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idSbry(isVbar)).and.                           &
     &      ANY(Lobc(:,isVbar,ng))) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idSbry(isVbar))),  &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# endif
# ifdef ADJUST_WSTRESS
        IF (.not.got_var(idUsms)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idUsms)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVsms)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idVsms)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
# endif
# ifdef SOLVE3D
        IF (.not.got_var(idUvel)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idUvel)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVvel)) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idVvel)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#  if defined ADJUST_BOUNDARY && !defined RBL4DVAR_FCT_SENSITIVITY
        IF (.not.got_var(idSbry(isUvel)).and.                           &
     &      ANY(Lobc(:,isUvel,ng))) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idSbry(isUvel))),  &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idSbry(isVvel)).and.                           &
     &      ANY(Lobc(:,isVvel,ng))) THEN
          IF (Master) WRITE (stdout,40) TRIM(Vname(1,idSbry(isVvel))),  &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
#  endif
        DO itrc=1,NT(ng)
          IF (.not.got_var(idTvar(itrc))) THEN
            IF (Master) WRITE (stdout,40) TRIM(Vname(1,idTvar(itrc))),  &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
#  if defined ADJUST_BOUNDARY && !defined RBL4DVAR_FCT_SENSITIVITY
          IF (.not.got_var(idSbry(isTvar(itrc))).and.                   &
     &        ANY(Lobc(:,isTvar(itrc),ng))) THEN
            IF (Master) WRITE (stdout,40)                               &
     &                        TRIM(Vname(1,idSbry(isTvar(itrc)))),      &
     &                        TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
#  endif
#  ifdef ADJUST_STFLUX
          IF (.not.got_var(idTsur(itrc)).and.Lstflux(itrc,ng)) THEN
            IF (Master) WRITE (stdout,40) TRIM(Vname(1,idTsur(itrc))),  &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
#  endif
        END DO
# endif
!
!  Set unlimited time record dimension to the appropriate value.
!
        INI(ng)%Rindex=rec_size
        Fcount=INI(ng)%Fcount
        INI(ng)%Nrec(Fcount)=rec_size
      END IF
!
  10  FORMAT (/,' DEF_INI - unable to open initial NetCDF file: ',a)
  20  FORMAT (/,' DEF_INI - illegal dimensions for variable: ',a,       &
     &        /,11x,'Nvardims = ',i0,', missing dimension: ''',a,'''',  &
     &        /,11x,'in file: ',a,                                      &
     &        /,11x,'Remove such variable from input file.')
  30  FORMAT (/,' DEF_INI - unable to put in define mode initial',      &
     &        ' NetCDF file: ',a)
  40  FORMAT (/,' DEF_INI - unable to find variable: ',a,2x,            &
     &        ' in file: ',a)
!
      RETURN
      END SUBROUTINE def_ini
#else
      SUBROUTINE def_ini
      RETURN
      END SUBROUTINE def_ini
#endif
