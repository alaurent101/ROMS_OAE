#include "cppdefs.h"
      SUBROUTINE get_varcoords (ng, model, ncname, ncid,                &
     &                          ncvname, ncvarid, Nx, Ny,               &
     &                          Xmin, Xmax, X, Ymin, Ymax, Y,           &
     &                          rectangular)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine reads the spatial positions of any NetCDF variable     !
!  having the attribute  "coordinates",  as specified by CF rules.     !
!  For example, in CDL syntax:                                         !
!                                                                      !
!       float my_var(time, lat, lon) ;                                 !
!               my_var:long_name = "my variable long name" ;           !
!               my_var:units = "my variable units" ;                   !
!               my_var:coordinates = "lon lat time" ;                  !
!               my_var:time = "time" ;                                 !
!                                                                      !
!  The following "coordinates" attribute is also allowed:              !
!                                                                      !
!               my_var:coordinates = "lon lat" ;                       !
!                                                                      !
!  That is, the time variable "time" is missing in the "coordinates"   !
!  attribute.                                                          !
!                                                                      !
!  Notice that the associated coordinate names "lon" and "lat" are     !
!  separated by a single blank space.  Both "lon" and "lat" can be     !
!  1D or 2D arrays. If 1D array, the positions are rectangular and     !
!  and full 2D arrays are filled with the same values.                 !
!                                                                      !
!  It also determines the rectangular switch  which indicates that     !
!  the spatial positions have a plaid distribution.                    !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_netcdf
      USE mod_scalars
!
      USE strings_mod, ONLY : FoundError
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, ncid, ncvarid
      integer, intent(in) :: Nx, Ny

      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: ncvname

      logical, intent(out) :: rectangular

      real(r8), intent(out) :: Xmin, Xmax, Ymin, Ymax

      real(r8), intent(out) :: X(Nx,Ny)
      real(r8), intent(out) :: Y(Nx,Ny)
!
!  Local variable declarations
!
      logical :: foundit
!
      integer :: i, ic, j, jc
      integer :: alen, blank1, blank2, varid
!
      real(r8), dimension(Nx) :: Xwrk
      real(r8), dimension(Ny) :: Ywrk
!
      character (len=20) :: Xname, Yname

      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Get coarse variable coordinates.
!-----------------------------------------------------------------------
!
!  Inquire the variable attributes.
!
      CALL netcdf_inq_var (ng, model, ncname,                           &
     &                     ncid = ncid,                                 &
     &                     myVarName = ncvname)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!  Get names of variable coordinates.
!
      foundit=.FALSE.
      rectangular=.FALSE.

      DO i=1,n_vatt
        IF (TRIM(var_Aname(i)).eq.'coordinates') THEN
          alen=LEN_TRIM(var_Achar(i))
          blank1=INDEX(var_Achar(i)(1:alen),' ')
          blank2=INDEX(var_Achar(i)(blank1+1:alen),' ')
          Xname=var_Achar(i)(1:blank1-1)
          IF (blank2.gt.0) THEN
            Yname=var_Achar(i)(blank1+1:blank1+blank2-1)
          ELSE
            Yname=var_Achar(i)(blank1+1:alen)
          END IF
          foundit=.TRUE.
          EXIT
        END IF
      END DO
      IF (.not.foundit) THEN
        IF (Master) WRITE (stdout,10) TRIM(ncvname), TRIM(ncname)
        exit_flag=2
        ioerror=0
      END IF
!
!  Read in X-coordinates.
!
      CALL netcdf_inq_var (ng, model, ncname,                           &
     &                     ncid = ncid,                                 &
     &                     myVarName = Xname,                           &
     &                     VarID= varid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
      IF (n_vdim.eq.1) THEN
        CALL netcdf_get_fvar (ng, model, ncname, Xname,                 &
     &                        Xwrk,                                     &
     &                        ncid = ncid,                              &
     &                        start = (/1/),                            &
     &                        total = (/Nx/))
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        rectangular=.TRUE.
        jc=0
        DO j=1,Ny
          DO i=1,Nx
            X(i,j)=Xwrk(i)
          END DO
        END DO
      ELSE
        CALL netcdf_get_fvar (ng, model, ncname, Xname,                 &
     &                        X,                                        &
     &                        ncid = ncid,                              &
     &                        start = (/1,1/),                          &
     &                        total = (/Nx,Ny/))
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        jc=1
        DO j=2,Ny
          IF (X(1,j).eq.X(1,1)) THEN
            jc=jc+1
          END IF
        END DO
      END IF
!
!  Read in Y-coordinates.
!
      CALL netcdf_inq_var (ng, model, ncname,                           &
     &                     ncid = ncid,                                 &
     &                     myVarName = Yname,                           &
     &                     VarID= varid)
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
      IF (n_vdim.eq.1) THEN
        CALL netcdf_get_fvar (ng, model, ncname, Yname,                 &
     &                        Ywrk,                                     &
     &                        ncid = ncid,                              &
     &                        start = (/1/),                            &
     &                        total = (/Ny/))
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        rectangular=.TRUE.
        ic=0
        DO j=1,Ny
          DO i=1,Nx
            Y(i,j)=Ywrk(j)
          END DO
        END DO
      ELSE
        CALL netcdf_get_fvar (ng, model, ncname, Yname,                 &
     &                        Y,                                        &
     &                        ncid = ncid,                              &
     &                        start = (/1,1/),                          &
     &                        total = (/Nx,Ny/))
        IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN

        ic=1
        DO i=2,Nx
          IF (Y(i,1).eq.Y(1,1)) THEN
            ic=ic+1
          END IF
        END DO
      END IF
!
!  Determine "rectangular" switch.
!
      IF (((ic.ne.0).and.(ic.eq.Nx)).and.                               &
     &    ((jc.ne.0).and.(jc.eq.Ny))) THEN
        rectangular=.TRUE.
      END IF
!
!  Determine minimum and maximum positions.
!
      Xmin= spval
      Xmax=-spval
      Ymin= spval
      Ymax=-spval
      DO j=1,Ny
        DO i=1,Nx
          Xmin=MIN(Xmin,X(i,j))
          Xmax=MAX(Xmax,X(i,j))
          Ymin=MIN(Ymin,Y(i,j))
          Ymax=MAX(Ymax,Y(i,j))
        END DO
      END DO
!
 10   FORMAT (/,' GET_VARCOORDS - Cannot find "coordinates" attribute', &
     &        ' for variable:',2x,a,/,17x,'in file:',2x,a,/,            &
     &     /,17x,'This attribute is needed to interpolate input data',  &
     &     /,17x,'to model grid. Following CF compliance, we need:',/,  &
     &     /,17x,'float my_var(time, lat, lon) ;',                      &
     &     /,17x,'      my_var:long_name = "my variable long name" ;',  &
     &     /,17x,'      my_var:units = "my variable units" ;',          &
     &     /,17x,'      my_var:coordinates = "lon lat" ;',              &
     &     /,17x,'      my_var:time = "my_var_time" ;',/)
!
      RETURN
      END SUBROUTINE get_varcoords
