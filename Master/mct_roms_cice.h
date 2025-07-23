/*
** svn $Id$
************************************************************************
** Most of the code has been copied from ocean_coupler.F of the       **
** metroms code                                                       **
************************************************************************
**                                                                    **
** These routines are use couple ROMS/TOMS to CICE ice model using    **
** the Model Coupling Toolkit (MCT).                                  **
**                                                                    **
************************************************************************
*/

      SUBROUTINE initialize_ocn2cice_coupling (ng, tile)
!
!=======================================================================
!                                                                      !
!  Initialize ocean and wave models coupling stream.  This is the      !
!  training phase used to constuct MCT parallel interpolators and      !
!  and stablish communication patterns.                                !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_forces
      USE mod_kinds
      USE mod_scalars
      USE mod_iounits

!
!  Imported variable definitions.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Istr, Iend, Jstr, Jend
      integer :: IstrR, IendR, JstrR, JendR, IstrU, JstrV
      integer :: Asize, Jsize, MyError
      integer :: j, jc, nprocs

      integer, allocatable :: length(:)
      integer, allocatable :: start(:)
!
!-----------------------------------------------------------------------
!  Compute lower and upper bounds over a particular domain partition or
!  tile for RHO-, U-, and V-variables. Notice that "set_bounds.h" is
!  not used here because of implementation of periodicity in other
!  models.
!-----------------------------------------------------------------------
!
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
!
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
!
!-----------------------------------------------------------------------
!  Begin initialization phase.
!-----------------------------------------------------------------------
!
!  Get communicator local rank and size.
!
      CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)
      CALL mpi_comm_size (OCN_COMM_WORLD, nprocs, MyError)
!
!  Initialize MCT coupled model registry.
!
      CALL MCTWorld_init (Nmodels, MPI_COMM_WORLD, OCN_COMM_WORLD,      &
     &                    OCNid)
!
!  Determine start and lengths for domain decomposition.
!
      Jsize=JendR-JstrR+1
      IF (.not.allocated(start)) THEN
        allocate ( start(Jsize) )
      END IF
      IF (.not.allocated(length)) THEN
        allocate ( length(Jsize) )
      END IF
      jc=0
      DO j=JstrR,JendR
        jc=jc+1
        start (jc)=j*(Lm(ng)+2)+IstrR+1
        length(jc)=(IendR-IstrR+1)
      END DO
      CALL GlobalSegMap_init (GSMapROMS, start, length, 0,              &
     &                        OCN_COMM_WORLD, OCNid)
!
!  Initialize attribute vector holding the export data code strings of
!  the sea ice model. The Asize is the number of grid point on this
!  processor.
!
      Asize=GlobalSegMap_lsize(GSMapROMS, OCN_COMM_WORLD)

      CALL AttrVect_init (cice2ocn_AV, rList=TRIM(ExportList(Icice)),   &
     &                    lsize=Asize)
!
!  Initialize attribute vector holding the export data code string of
!  the ocean model.
!
      CALL AttrVect_init (ocn2cice_AV, rList=TRIM(ExportList(Iocean)),   &
     &                    lsize=Asize)
      CALL AttrVect_zero (ocn2cice_AV)
!
!  Initialize a router to the sea ice model component.
!
      CALL Router_init (CICEid, GSMapROMS, OCN_COMM_WORLD, ROMStoCICE)
!
!  Deallocate working arrays.
!
      IF (allocated(start)) THEN
        deallocate (start)
      END IF
      IF (allocated(length)) THEN
        deallocate (length)
      END IF

      RETURN
      END SUBROUTINE initialize_ocn2cice_coupling

      SUBROUTINE ocn2cice_coupling (ng, tile, ncouple)
      USE mod_param
      USE mod_parallel
      USE mod_coupler
      USE mod_forces
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
      USE mod_iounits, only: stdout
      USE mod_grid
!      USE distribute_mod, ONLY : mp_reduce
      USE ROMS_import_mod, ONLY : ROMS_import2d
      USE ROMS_export_mod, ONLY : ROMS_export2d
#ifdef LMD_SKPP
      USE mod_mixing
#endif
      USE bc_2d_mod
      USE exchange_2d_mod
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
      USE mod_ice
!jd
      USE frazil_ice_prod_mod, only : t_freeze
!jd

      implicit none

!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, ncouple
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
!  Local variable declarations.
!
      integer :: Istr, Iend, Jstr, Jend
      integer :: IstrR, IendR, JstrR, JendR, IstrU, JstrV
      integer :: Asize, Iimport, Iexport, MyError
      integer :: gtype, i, id, ifield, ij, j, status

      real(r8) :: add_offset, scale

      real(r8), pointer :: A(:)

      ! intermediates for ice stress interpolation
      real(r8) :: aice_psip, aice_psi, sap, sa 

      character (len=3 ), dimension(2) :: op_handle
      character (len=40) :: code


      integer :: nbotu, nbotv, k
      integer, dimension(:,:), allocatable :: nbot
      real(r8), dimension(:,:), allocatable :: uw
      real(r8), dimension(:,:), allocatable :: vw
      real(r8), dimension(:,:), allocatable :: uwater
      real(r8), dimension(:,:), allocatable :: vwater
      real(r8), dimension(:,:), allocatable :: Awrk

      real(r8) :: mlio
      real(r8) :: dml
      real(r8) :: totml
      real(r8) :: cff

      real(r8) :: Sold, t_fr, dz

#include "tile.h"

#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 36, __LINE__, MyFile)
#endif


      allocate(nbot(IminS:ImaxS,JminS:JmaxS))
      allocate(uw(IminS:ImaxS,JminS:JmaxS))
      allocate(vw(IminS:ImaxS,JminS:JmaxS))
      allocate(uwater(LBi:UBi,LBj:UBj))
      allocate(vwater(LBi:UBi,LBj:UBj))
      allocate(Awrk(LBi:UBi,LBj:UBj))

      uw=0.0_r8
      vw=0.0_r8
      uwater=0.0_r8
      vwater=0.0_r8


      IF (Master) THEN
          write(stdout,*) ' '
          write(stdout,*) ' ***************************************** '
          write(stdout,*) &
     &         '    Ocean - CICE: coupling routine called from ROMS'
          write(stdout,*) &
          '    Time : ', time_code, time
      END IF

!
!-----------------------------------------------------------------------
!  Compute lower and upper bounds over a particular domain partition or
!  tile for RHO-, U-, and V-variables. Notice that "set_bounds.h" is
!  not used here because of implementation of periodicity in other
!  models.
!-----------------------------------------------------------------------
!
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
      IstrU=BOUNDS(ng)%IstrU(tile)
      JstrV=BOUNDS(ng)%JstrV(tile)

!
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
!
!-----------------------------------------------------------------------
!  Allocate communications array.
!-----------------------------------------------------------------------
!
      Asize=GlobalSegMap_lsize(GSMapROMS, OCN_COMM_WORLD)
      allocate(A(Asize))
      A=0.0_r8

!
!  Schedule receiving fields from cice model.
!
      CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)

      CALL MCT_Recv (cice2ocn_AV, ROMStoCICE)
!
!  Receive fields from cice model.
!
      Iimport=0
      DO ifield=1,Nimport(Iocean)
        id=ImportID(Iocean)%val(ifield)
        code=ADJUSTL(Fields(id)%code)
        gtype=Fields(id)%GridType
        scale=Fields(id)%scale
        add_offset=Fields(id)%AddOffset

        SELECT CASE (TRIM(code))

        CASE ('AICE')           ! Sea ice concentration

           CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code),A,Asize)
           Iimport=Iimport+1
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) 'ROMS rank ', MyRank,                        &
     &          ' received ',trim(code),' field (max/min): ',           &
     &           maxval(A),' ',minval(A),' gtype : ', gtype
#endif
           scale=1.0_r8
           add_offset=0.0_r8
           CALL ROMS_import2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         Asize, A,                                &
     &                         IstrR, IendR, JstrR, JendR,              &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Fields(id)%ImpMin, Fields(id)%ImpMax,    &
     &                         ICE(ng)%aice,                            &
     &                         status)
       
!jd Transfere to u- and v- points for weigthing with stress

           DO j=Jstr,Jend
              DO i=IstrU,Iend
                 ICE(ng)%aice_u(i,j)=                                   &
     &                0.5_r8*(ICE(ng)%aice(i-1,j)+ICE(ng)%aice(i,j))
!# ifdef MASKING
!                 ICE(ng)%aice_u(i,j)=ICE(ng)%aice_u(i,j)*umask(i,j)
!# endif
              END DO
           END DO
           DO j=JstrV,Jend
              DO i=Istr,Iend
                 ICE(ng)%aice_v(i,j)=                                   &
     &                0.5_r8*(ICE(ng)%aice(i,j-1)+ICE(ng)%aice(i,j))
!# ifdef MASKING
!                 ICE(ng)%aice_v(i,j)=ICE(ng)%aice_v(i,j)*vmask(i,j)
!# endif
              END DO
           END DO
!
!  Apply boundary conditions.
!
      CALL bc_u2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  ICE(ng)%aice_u)
      CALL bc_v2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  ICE(ng)%aice_v)

#ifdef DISTRIBUTE
           CALL mp_exchange2d (ng, tile, iNLM, 2,                       &
     &          LBi, UBi, LBj, UBj,                                     &
     &          NghostPoints,                                           &
     &          EWperiodic(ng), NSperiodic(ng),                         &
     &          ICE(ng)%aice_u,ICE(ng)%aice_v )
#endif


        CASE ('freshAI')        ! Fresh water flux

           CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
           Iimport=Iimport+1
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) 'ROMS rank ', MyRank,                        &
     &          ' received ',trim(code),' field (max/min): ',           &
     &          maxval(A),' ',minval(A),' gtype : ', gtype
#endif
           scale=1.0_r8
           add_offset=0.0_r8
           CALL ROMS_import2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         Asize, A,                                &
     &                         IstrR, IendR, JstrR, JendR,              &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Fields(id)%ImpMin, Fields(id)%ImpMax,    &
     &                         ICE(ng)%freshAI,                         &
     &                         status)

        CASE ('fsaltAI')        ! Salt flux

           CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
           Iimport=Iimport+1
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) 'ROMS rank ', MyRank,                        &
     &          ' received ',trim(code),' field (max/min): ',           &
     &          maxval(A),' ',minval(A),' gtype : ', gtype
#endif
           scale=1.0_r8
           add_offset=0.0_r8
           CALL ROMS_import2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         Asize, A,                                &
     &                         IstrR, IendR, JstrR, JendR,              &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Fields(id)%ImpMin, Fields(id)%ImpMax,    &
     &                         ICE(ng)%fsaltAI,                         &
     &                         status)

        CASE ('fhocnAI')        ! Nonradiative heat flux

           CALL AttrVect_exportRAttr(cice2ocn_AV, TRIM(code), A, Asize)
           Iimport=Iimport+1
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) 'ROMS rank ', MyRank,                        &
     &          ' received ',trim(code),' field (max/min): ',           &
     &          maxval(A),' ',minval(A),' gtype : ', gtype
#endif
           scale=1.0_r8
           add_offset=0.0_r8
           CALL ROMS_import2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         Asize, A,                                &
     &                         IstrR, IendR, JstrR, JendR,              &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Fields(id)%ImpMin, Fields(id)%ImpMax,    &
     &                         ICE(ng)%fhocnAI,                         &
     &                         status)

        CASE ('fswthruAI')      ! Radiative heat flux through ice

           CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
           Iimport=Iimport+1
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) 'ROMS rank ', MyRank,                        &
     &          ' received ',trim(code),' field (max/min): ',           &
     &          maxval(A),' ',minval(A),' gtype : ', gtype
#endif
           scale=1.0_r8
           add_offset=0.0_r8
           CALL ROMS_import2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         Asize, A,                                &
     &                         IstrR, IendR, JstrR, JendR,              &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Fields(id)%ImpMin, Fields(id)%ImpMax,    &
     &                         ICE(ng)%fswthruAI,                       &
     &                         status)

        CASE ('strocnx')        ! ice-ocean stress x-dir

           CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code),A,Asize)
           Iimport=Iimport+1
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) 'ROMS rank ', MyRank,                        &
     &          ' received strocnx field (max/min): ',                  &
     &          maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
           scale=1.0_r8
           add_offset=0.0_r8

           CALL ROMS_import2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         Asize, A,                                &
     &                         IstrR, IendR, JstrR, JendR,              &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Fields(id)%ImpMin, Fields(id)%ImpMax,    &
     &                         ICE(ng)%strx,                            &
     &                         status)
           

           do j=Jstr,Jend
              do i=IstrU,Iend
                 ICE(ng)%stru(i,j) = 0.5_r8*                            &
     &                (ICE(ng)%strx(i-1,j) + ICE(ng)%strx(i,j))

!# ifdef MASKING
!                 ICE(ng)%stru(i,j)=ICE(ng)%stru(i,j)*umask(i,j)
!# endif
              END DO
           END DO
!
!  Apply boundary conditions.
!
      CALL bc_u2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  ICE(ng)%stru)

#ifdef DISTRIBUTE
           CALL mp_exchange2d (ng, tile, iNLM, 1,                       &
     &          LBi, UBi, LBj, UBj,                                     &
     &          NghostPoints,                                           &
     &          EWperiodic(ng), NSperiodic(ng),                         &
     &          ICE(ng)%stru )
#endif


        CASE ('strocny')        ! ice-ocean stress y-dir 

           CALL AttrVect_exportRAttr (cice2ocn_AV, TRIM(code), A, Asize)
           Iimport=Iimport+1
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) 'ROMS rank ', MyRank,                        &
     &          ' received strocny field (max/min): ',                  &
     &          maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
           scale=1.0_r8
           add_offset=0.0_r8
           CALL ROMS_import2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         Asize, A,                                &
     &                         IstrR, IendR, JstrR, JendR,              &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Fields(id)%ImpMin, Fields(id)%ImpMax,    &
     &                         ICE(ng)%stry,                            &
     &                         status)

           do j=JstrV,Jend
              do i=Istr,Iend
                 ICE(ng)%strv(i,j)=0.5_r8*                              &
     &                (ICE(ng)%stry(i,j-1)+ICE(ng)%stry(i,j))
!# ifdef MASKING
!                 ICE(ng)%strv(i,j)=ICE(ng)%strv(i,j)*vmask(i,j)
!# endif
              END DO
           END DO
!
!  Apply boundary conditions.
!
      CALL bc_v2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  ICE(ng)%strv)

#ifdef DISTRIBUTE
           CALL mp_exchange2d (ng, tile, iNLM, 1,                       &
     &          LBi, UBi, LBj, UBj,                                     &
     &          NghostPoints,                                           &
     &          EWperiodic(ng), NSperiodic(ng),                         &
     &          ICE(ng)%strv )
#endif
           
        END SELECT
#ifdef REPORT_COUPLING_ALL
        IF (Master) write(stdout,*)                                     &
     &       'i2o: ',trim(code),' min/max', Fields(id)%ImpMin,        &
     &          Fields(id)%ImpMax
#endif

      END DO
!
!-----------------------------------------------------------------------
!  Export fields from ocean (ROMS) to sea ice (CICE) model.
!-----------------------------------------------------------------------
!

!  Prepare for depth-averaging (find k index just below mixed layer)

#define I_RANGE MAX(Istr - 2, 0), MIN(Iend + 2, Lm(ng) + 1)
#define J_RANGE MAX(Jstr - 2, 0), MIN(Jend + 2, Mm(ng) + 1)
      do j=J_RANGE
         do i=I_RANGE

#ifdef LMD_SKPP
            mlio = min(MIXING(ng)%hsbl(i,j),-10._r8)
#else
            mlio = -10._r8
#endif
            nbot(i,j) = 1
            do k=N(ng),1,-1
               if(GRID(ng)%z_r(i,j,k).lt.mlio) then
                  nbot(i,j) = min(k,N(ng))
                  nbot(i,j) = max(nbot(i,j),1)
                  goto 1111
               endif
            enddo
 1111       continue
         enddo
      enddo
#undef I_RANGE
#undef J_RANGE

!  Schedule sending fields to the cice model.
!
      Iexport=0
      DO ifield=1,Nexport(Iocean)
         id=ExportID(Iocean)%val(ifield)
         code=ADJUSTL(Fields(id)%code)
         gtype=Fields(id)%GridType
         scale=Fields(id)%scale
         add_offset=Fields(id)%AddOffset

         SELECT CASE (TRIM(code))

         CASE ('SST')

            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OCEAN(ng)%t(:,:,N(ng),NOUT,itemp),      &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)

#ifdef REPORT_COUPLING_ALL
            write(stdout,*)                                             &
     &           'ROMS rank ', MyRank, ' sending sst field (max/min): ',&
     &           maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1

         CASE ('SSS')

            CALL ROMS_export2d (ng, tile,                               &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OCEAN(ng)%t(:,:,N(ng),NOUT,isalt),      &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
            write(stdout,*) &
     &           'ROMS rank ', MyRank, ' sending sss field (max/min): ',&
     &           maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
            CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
            Iexport=Iexport+1

         CASE ('FRZMLT')
      
           Awrk = 0.0_r8
           do j=JstrR,JendR
            do i=IstrR,IendR
# ifdef WET_DRY
             IF (GRID(ng) % rmask_wet(i,j) .ne. 0.0_r8) THEN
# endif
             if (ICE(ng)%qfraz_accum(i,j).gt.0.0_r8) then
              ! ice was formed
              Awrk(i,j) = ICE(ng)%qfraz_accum(i,j)/ncouple
             else
              ! No ice was formed; recompute melt potential in surface
              ! Accumulate over max. 5 meter
              do k=1,N(ng)
               if (GRID(ng) % z_r(i,j,k) < -5.0_r8) cycle
               !
               Sold = max(0.0_r8, OCEAN(ng)%t(i,j,k,NOUT,isalt))
               t_fr = t_freeze(Sold, GRID(ng)%z_r(i,j,k))
               dz = GRID(ng)%Hz(i,j,k)
               Awrk(i,j) = Awrk(i,j) + min(0.0_r8,                      &
     &                   t_fr - OCEAN(ng)%t(i,j,N(ng),NOUT,itemp))*     &
     &                   dz*rho0*Cp/(ncouple*dt(ng))
              end do
             end if
# ifdef WET_DRY
            END IF
# endif
            end do
           end do

           CALL ROMS_export2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Awrk,                &
     &                         Fields(id)%ExpMin, Fields(id)%ExpMax,    &
     &                         Asize, A,                                &
     &                         status)
#ifdef REPORT_COUPLING_ALL
           write(stdout,*) &
     &          'ROMS rank ',MyRank,' sending frzmlt field (max/min):', &
     &          maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
           CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
           Iexport=Iexport+1

!jd Reset accumulation array
           ICE(ng)%qfraz_accum(:,:) = 0


        CASE ('u')
#define I_RANGE MAX(Istr - 1, 1), Iend + 1             
           do j=Jstr,Jend
              do i=I_RANGE
                 nbotu = NINT(0.5_r8*(nbot(i-1,j)+nbot(i,j)))
                 nbotu = max(min(nbotu,N(ng)),1)
                 uw(i,j) = 0._r8
                 totml = 0._r8
                 do k=N(ng),nbotu,-1
                    dml = 0.5_r8*(GRID(ng)%z_w(i-1,j,k)                 &
     &                   -GRID(ng)%z_w(i-1,j,k-1)                       &
     &                   + GRID(ng)%z_w(i,j,k)-GRID(ng)%z_w(i,j,k-1))
                    uw(i,j) = uw(i,j) + OCEAN(ng)%u(i,j,k,NOUT)*dml
                    totml = totml + dml
                 enddo
                 uw(i,j) = uw(i,j)/totml
              enddo
           enddo
#undef I_RANGE
           do j=Jstr,Jend
              do i=IstrU,Iend
                 uwater(i,j) = uw(i,j)
              enddo
           enddo
!  Apply boundary conditions.
           CALL bc_u2d_tile (ng, tile,                                  &
     &          LBi, UBi, LBj, UBj,                                     &
     &          uwater)
#ifdef DISTRIBUTE
           CALL mp_exchange2d (ng, tile, iNLM, 1,                       &
     &          LBi, UBi, LBj, UBj,                                     &
     &          NghostPoints, EWperiodic(ng), NSperiodic(ng),           &
     &          uwater)
#endif
!            write(stdout,*) TRIM(code),id,gtype
           CALL ROMS_export2d (ng, tile,                                &
     &                         id, gtype, scale, add_offset,            &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         uwater,                                  &
!     &                         OCEAN(ng)%u(:,:,N(ng),NOUT),            &
     &                         Fields(id)%ExpMin, Fields(id)%ExpMax,    &
     &                         Asize, A,                                &
     &                         status)

#ifdef REPORT_COUPLING_ALL
           write(stdout,*)                                              &
     &          'ROMS rank ',MyRank,' sending u field (max/min):',      &
     &          maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
           CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code), A, Asize)
           Iexport=Iexport+1

        CASE ('v')
#define J_RANGE MAX(Jstr - 1, 1), Jend + 1 
           do j=J_RANGE
              do i=Istr,Iend
                 nbotv = NINT(0.5_r8*(nbot(i,j-1)+nbot(i,j)))
                 nbotv = max(min(nbotv,N(ng)),1)
                 vw(i,j) = 0._r8
                 totml = 0._r8
                 do k=N(ng),nbotv,-1
                    dml = 0.5_r8*(GRID(ng)%z_w(i,j-1,k)                 &
     &                    -GRID(ng)%z_w(i,j-1,k-1)                      &
     &                    + GRID(ng)%z_w(i,j,k)-GRID(ng)%z_w(i,j,k-1))
                    vw(i,j) = vw(i,j) + OCEAN(ng)%v(i,j,k,NOUT)*dml
                    totml = totml + dml
                 enddo
                 vw(i,j) = vw(i,j)/totml
              enddo
           enddo
#undef J_RANGE
           do j=JstrV,Jend
              do i=Istr,Iend
                 vwater(i,j) = vw(i,j)
              enddo
             enddo
!  Apply boundary conditions.
             CALL bc_v2d_tile (ng, tile,                                &
     &            LBi, UBi, LBj, UBj,                                   &
     &            vwater)
#ifdef DISTRIBUTE
             CALL mp_exchange2d (ng, tile, iNLM, 1,                     &
     &            LBi, UBi, LBj, UBj,                                   &
     &            NghostPoints, EWperiodic(ng), NSperiodic(ng),         &
     &            vwater)
#endif
             CALL ROMS_export2d (ng, tile,                              &
     &                           id, gtype, scale, add_offset,          &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           vwater,                                &
!     &                          OCEAN(ng)%v(:,:,N(ng),NOUT),            &
     &                           Fields(id)%ExpMin, Fields(id)%ExpMax,  &
     &                           Asize, A,                              &
     &                           status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &            'ROMS rank ',MyRank,' sending v field (max/min):',    &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV, TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('SSH')

             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          OCEAN(ng)%zeta(:,:,KOUT),               &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending SSH field (max/min):',   &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('Tair')                    ! air temperature
            
            ! CICE needs Tair in Kelvin
            Awrk = 0.0_r8
            DO j=JstrR,JendR
              DO i=IstrR,IendR
                Awrk(i,j)=FORCES(ng)%Tair(i,j) + 273.16
              END DO
            END DO

             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Awrk,                                   &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending Tair field (max/min):',  &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('Qair')                    ! humidity
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Hair,                        &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending Qair field (max/min):',  &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('rain')                    ! rain fall rate [kg m-2 s-1]

            ! the variable 'rain' contains the total precipitation rate,
            ! i.e., rain and snow. Here we subtract the snowfall rate
            ! because CICE distiguished between the two forms of preci-
            ! pitation.
            Awrk = 0.0_r8
            DO j=JstrR,JendR
              DO i=IstrR,IendR
                Awrk(i,j)=FORCES(ng)%rain(i,j) - FORCES(ng)%snow(i,j)
                Awrk(i,j)=max(Awrk(i,j), 0.0_r8)
              END DO
            END DO


             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Awrk,                        &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending rain field (max/min):',  &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('snow')                    ! snowfall rate [kg m-2 s-1]
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%snow,                        &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending snow field (max/min):',  &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('SWrad')                    ! solar shortwave radiation
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%SW_down,                     &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending srflx field (max/min):', &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('LWrad')                    ! longwave radiation [W m-2]
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%LW_down,                     &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending lrflx field (max/min):', &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('Uwind')                    ! wind x-component [m s-1]
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Uwind,                       &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending Uwind field (max/min):', &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('Vwind')                    ! wind y-component [m s-1]
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Vwind,                       &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending Vwind field (max/min):', &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('Pair')                    ! surface air pressure
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Pair,                        &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending Pair field (max/min):',  &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('Aice')                    ! sea ice concentration
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Aice,                        &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending Aice field (max/min):',  &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

          CASE ('Hice')                    ! sea ice thickness
            
             CALL ROMS_export2d (ng, tile,                              &
     &                          id, gtype, scale, add_offset,           &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Hice,                        &
     &                          Fields(id)%ExpMin, Fields(id)%ExpMax,   &
     &                          Asize, A,                               &
     &                          status)
#ifdef REPORT_COUPLING_ALL
             write(stdout,*)                                            &
     &           'ROMS rank ',MyRank,' sending Hice field (max/min):',  &
     &            maxval(A), ' ', minval(A),' gtype : ', gtype
#endif
             CALL AttrVect_importRAttr(ocn2cice_AV,TRIM(code),A,Asize)
             Iexport=Iexport+1

        END SELECT

#ifdef REPORT_COUPLING_ALL
        IF (Master) write(stdout,*)                                     &
     &       'o2i: ',trim(code),' min/max', Fields(id)%ExpMin,        &
     &          Fields(id)%ExpMax
#endif

      END DO

!
!  Send ocean fields
!
      IF (Iexport.gt.0) THEN

#ifdef REPORT_COUPLING_ALL
         if (master)                                                    &
     &        write(stdout,*)'ROMS sends', iexport,' fields to CICE'
#endif
         CALL MCT_Send(ocn2cice_AV, ROMStoCICE)
      END IF

!
!  Deallocate communication arrays.
!
      deallocate (A,Awrk,nbot,uw,vw,uwater,vwater)

#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 36, __LINE__, MyFile)
#endif

      RETURN
      END SUBROUTINE ocn2cice_coupling

      SUBROUTINE finalize_ocn2cice_coupling
!
!========================================================================
!                                                                       !
!  This routine finalizes ocean and ice models coupling data streams.   !
!                                                                       !
!========================================================================
!
!  Local variable declarations.
!
      integer :: MyError
!
!-----------------------------------------------------------------------
!  Deallocate MCT environment.
!-----------------------------------------------------------------------
!
      CALL Router_clean (ROMStoCICE, MyError)
      CALL AttrVect_clean (ocn2cice_AV, MyError)
      CALL GlobalSegMap_clean (GSMapROMS, MyError)

      RETURN

      END SUBROUTINE finalize_ocn2cice_coupling
