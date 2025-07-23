#include "cppdefs.h"

      MODULE kupdate_mod
#if defined SPCNDG
# if !defined TS_FIXED && (defined NONLINEAR && defined SOLVE3D)

! Subroutine to calculate THAT used in spectral nudging.
! Kyoko Ohashi
! 2018/2/27

      implicit none
!
      PRIVATE
      PUBLIC  :: kupdate
!
      CONTAINS

!***********************************************************************
      SUBROUTINE kupdate (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_grid
      USE mod_ocean
      USE mod_clima
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
# include "tile.h"
!
! Added __LINE__, __FILE__ 2021/3/8
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 14, __LINE__, __FILE__)
# endif
      CALL kupdate_tile (ng, tile,                                      &
     &                   LBi, UBi, LBj, UBj,                            &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nnew(ng),                                      &
# ifdef MASKING
     &                   GRID(ng) % rmask,                              &
# endif
     &                   OCEAN(ng) % t,                                 &
     &                   OCEAN(ng) % Tmob,                              &
     &                   OCEAN(ng) % Smob,                              &
     &                   CLIMA(ng) % tclm,                              &
     &                   OCEAN(ng) % That)
! Added __LINE__, __FILE__ 2021/3/8
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 14, __LINE__, __FILE__)
# endif

      RETURN
      END SUBROUTINE kupdate
!
!***********************************************************************
      SUBROUTINE kupdate_tile (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         nnew,                                    &
#  ifdef MASKING
     &                         rmask,                                   &
#  endif
     &                         t,Tmob,Smob,tclm,That)                   
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
#  ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d, mp_exchange3d
#  endif

!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nnew
!
#  ifdef ASSUMED_SHAPE
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
#   endif
#  ifdef SUN
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#  else
      real(r8), intent(in) :: t(LBi:,LBj:,:,:,:)
#  endif
      real(r8), intent(inout) :: Tmob(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: Smob(LBi:,LBj:,:,:)
      real(r8), intent(in) :: tclm(LBi:,LBj:,:,:)
      real(r8), intent(out) :: That(LBi:,LBj:,:,:)
#  else
#   ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
      real(r8), intent(in) :: Tmob(LBi:UBi,LBj:UBj,N(ng),5)
      real(r8), intent(in) :: Smob(LBi:UBi,LBj:UBj,N(ng),5)
      real(r8), intent(in) :: tclm(LBi:UBi,LBj:UBj,N(ng),NT(ng))
      real(r8), intent(out) :: That(LBi:UBi,LBj:UBj,N(ng),NT(ng))
#  endif
!
!  Local variable declarations.
!
      integer :: i, j, k, m, itrc
      integer, parameter :: nn = 5
      real(r8) :: dtidays, om, dkappa1
      real(r8), dimension(5,5) :: cD
      real(r8), dimension(5,1) :: cK
      real(r8), dimension(1,5) :: cH
      real(r8), dimension(5,1) :: state
      real(r8), dimension(5,1) :: temp1
      real(r8), dimension(1,1) :: temp2
      real(r8), dimension(5,1) :: temp3
      real(r8), dimension(1,1) :: temp4
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,N(ng),NT(ng)) :: ff

#  include "set_bounds.h"

      dtidays=dt(ng)/(24.0_r8*3600.0_r8)
      om=2.0_r8*3.14159265359_r8*dtidays/365.0_r8
      dkappa1=dtidays/dkappa(ng)

      DO i=1,nn
        cH(1,i)=0.0_r8
        cK(i,1)=0.0_r8
        DO j=1,nn
          cD(i,j)=0.0_r8
        END DO
      END DO

      cH(1,1)=1.0_r8
      cH(1,2)=1.0_r8
      cH(1,4)=1.0_r8

      cK(1,1)=dkappa1
      cK(2,1)=2.0_r8*dkappa1
      cK(4,1)=2.0_r8*dkappa1

      cD(1,1)=1.0_r8
      cD(2,2)=COS(om)
      cD(2,3)=-SIN(om)
      cD(3,2)=SIN(om)
      cD(3,3)=COS(om)
      cD(4,4)=COS(2.0_r8*om)
      cD(4,5)=-SIN(2.0_r8*om)
      cD(5,4)=SIN(2.0_r8*om)
      cD(5,5)=COS(2.0_r8*om)

      DO itrc=1,NT(ng)
        DO j=JstrT,JendT
          DO k=1,N(ng)
            DO i=IstrT,IendT
              ff(i,j,k,itrc)=tclm(i,j,k,itrc)-t(i,j,k,nnew,itrc)
              DO m=1,nn
                IF (itrc.eq.itemp) THEN
                  state(m,1)=Tmob(i,j,k,m)
                ELSE IF (itrc.eq.isalt) THEN
                  state(m,1)=Smob(i,j,k,m)
                END IF
              END DO
              CALL matprod(temp1,cD,state,nn,nn,1)
              CALL matprod(temp2,cH,temp1,1,nn,1)
              CALL matprod(temp3,cK,temp2,nn,1,1)
              DO m=1,nn
                state(m,1)=temp1(m,1)-temp3(m,1)+cK(m,1)*ff(i,j,k,itrc)
                IF (itrc.eq.itemp) THEN
                  Tmob(i,j,k,m)=state(m,1)
                ELSE IF (itrc.eq.isalt) THEN
                  Smob(i,j,k,m)=state(m,1)
                END IF
              END DO
              CALL matprod(temp4,cH,state,1,nn,1)
              That(i,j,k,itrc)=temp4(1,1)
            END DO
          END DO
        END DO
      END DO

      RETURN
      END SUBROUTINE kupdate_tile

!
!***********************************************************************
      SUBROUTINE matprod(AB,A,B,m,n1,l)
!***********************************************************************
!
      USE mod_param

!  Imported variable declarations.
!

      integer, intent(in) :: m, n1, l
      real(r8), intent(in) :: A(m,n1), B(n1,l)
      real(r8), intent(out) :: AB(m,l)
!
!  Local variable declarations.
!
      integer :: i, j, k

      DO j=1,m
        DO k=1,l
          AB(j,k)=0.0_r8
          DO i=1,n1 
            AB(j,k)=AB(j,k)+A(j,i)*B(i,k)
          END DO
        END DO
      END DO 

      RETURN
      END SUBROUTINE matprod
# endif
#endif
      END MODULE kupdate_mod
