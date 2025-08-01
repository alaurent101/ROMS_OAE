#include "cppdefs.h"
      MODULE ROMS_export_mod

#if defined MODEL_COUPLING && defined MCT_LIB
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This module contains several routines to prepare ROMS fields to     !
!  export to other models.  It assumed that outside models  fields     !
!  (like observations) are located at RHO-points.                      !
!                                                                      !
!=======================================================================
!
      USE mod_kinds

      implicit none

      PUBLIC :: ROMS_export2d

      CONTAINS
!
!***********************************************************************
      SUBROUTINE ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          InpField,                               &
     &                          OutFmin, OutFmax,                       &
# if defined MCT_LIB
     &                          Npts, OutField,                         &
# endif
     &                          status)
!***********************************************************************
!
      USE mod_param
      USE mod_ncparam
!
# ifdef DISTRIBUTE
      USE distribute_mod,  ONLY : mp_reduce
# endif
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, id, gtype
      integer, intent(in) :: LBi, UBi, LBj, UBj
# ifdef MCT_LIB
      integer, intent(in) :: Npts
# endif
      integer, intent(out) :: status

      real(r8), intent(in) :: scale, add_offset
      real(r8), intent(out) :: OutFmin, OutFmax

# ifdef ASSUMED_SHAPE
      real(r8), intent(in)  :: InpField(LBi:,LBj:)
#  if defined MCT_LIB
      real(r8), intent(out) :: OutField(:)
#  endif
# else
      real(r8), intent(in)  :: InpField(LBi:UBi,LBj:UBj)
#  if defined MCT_LIB
      real(r8), intent(out) :: OutField(Npts)
#  endif
# endif
!
!  Local variable declarations.
!
      integer :: i, ij, j

      real(r8), parameter :: Large = 1.0E+20_r8

      real(r8), dimension(2) :: range

# ifdef MCT_LIB
      real(r8), dimension(LBi:UBi,LBj:UBj) :: Awork
# endif
# ifdef DISTRIBUTE
      character (len=3), dimension(2) :: op_handle
# endif


# ifdef CICE_COUPLING

      integer :: Istr, Iend, Jstr, Jend
      integer :: IstrR, IendR, JstrR, JendR

      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)

      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
         IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
         IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
         IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
         IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
         JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
         JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
         JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
         JendR=BOUNDS(ng)%Jend(tile)
      END IF

# else
#  include "set_bounds.h"
# endif!
!-----------------------------------------------------------------------
!  Compute export fields to RHO-points.
!-----------------------------------------------------------------------
!
      status=0
      range(1)= Large
      range(2)=-Large

# if defined MCT_LIB
!
!  RHO-type variables.
!
      IF (gtype.eq.r2dvar) THEN
        ij=0
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            ij=ij+1
            OutField(ij)=InpField(i,j)
            range(1)=MIN(range(1),OutField(ij))
            range(2)=MAX(range(2),OutField(ij))
          END DO
        END DO
!
!  U-type variables.
!
      ELSE IF (gtype.eq.u2dvar) THEN
        DO j=JstrR,JendR
          DO i=Istr,Iend
            Awork(i,j)=0.5_r8*(InpField(i  ,j)+                         &
     &                         InpField(i+1,j))
          END DO
        END DO
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
            Awork(Istr-1,j)=Awork(Istr,j)
          END DO
        END IF
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
            Awork(Iend+1,j)=Awork(Iend,j)
          END DO
        END IF
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          Awork(Istr-1,Jstr-1)=0.5_r8*(Awork(Istr  ,Jstr-1)+            &
     &                                 Awork(Istr-1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          Awork(Iend+1,Jstr-1)=0.5_r8*(Awork(Iend  ,Jstr-1)+            &
     &                                 Awork(Iend+1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          Awork(Istr-1,Jend+1)=0.5_r8*(Awork(Istr-1,Jend  )+            &
     &                                 Awork(Istr  ,Jend+1))
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          Awork(Iend+1,Jend+1)=0.5_r8*(Awork(Iend+1,Jend  )+            &
     &                                 Awork(Iend  ,Jend+1))
        END IF
!
!  Pack into export vector.
!
        ij=0
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            ij=ij+1
            OutField(ij)=Awork(i,j)
            range(1)=MIN(range(1),OutField(ij))
            range(2)=MAX(range(2),OutField(ij))
          END DO
        END DO
!
!  V-type variables.
!
      ELSE IF (gtype.eq.v2dvar) THEN
        DO j=Jstr,Jend
          DO i=IstrR,IendR
            Awork(i,j)=0.5_r8*(InpField(i,j  )+                         &
     &                         InpField(i,j+1))
          END DO
        END DO
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr,Iend
            Awork(i,Jend+1)=Awork(i,Jend)
          END DO
        END IF
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr,Iend
            Awork(i,Jstr-1)=Awork(i,Jstr)
          END DO
        END IF
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          Awork(Istr-1,Jstr-1)=0.5_r8*(Awork(Istr  ,Jstr-1)+            &
     &                                 Awork(Istr-1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          Awork(Iend+1,Jstr-1)=0.5_r8*(Awork(Iend  ,Jstr-1)+            &
     &                                 Awork(Iend+1,Jstr  ))
        END IF
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          Awork(Istr-1,Jend+1)=0.5_r8*(Awork(Istr-1,Jend  )+            &
     &                                 Awork(Istr  ,Jend+1))
        END IF
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          Awork(Iend+1,Jend+1)=0.5_r8*(Awork(Iend+1,Jend  )+            &
     &                                 Awork(Iend  ,Jend+1))
        END IF
!
!  Pack into export vector.
!
        ij=0
        DO j=JstrR,JendR
          DO i=IstrR,IendR
            ij=ij+1
            OutField(ij)=Awork(i,j)
            range(1)=MIN(range(1),OutField(ij))
            range(2)=MAX(range(2),OutField(ij))
          END DO
        END DO
      END IF
# endif
# ifdef DISTRIBUTE
!
!-----------------------------------------------------------------------
!  Global reduction for imported field range values.
!-----------------------------------------------------------------------
!
      op_handle(1)='MIN'
      op_handle(2)='MAX'
      CALL mp_reduce (ng, iNLM, 2, range, op_handle)
      OutFmin=range(1)
      OutFmax=range(2)
# endif

      END SUBROUTINE ROMS_export2d
#endif
      END MODULE roms_export_mod
