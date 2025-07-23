!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2021 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Parameters for the dissolution model:                               !
!                                                                      !
!   BioIter  Maximum number of iterations to achieve convergence       !
!              of the nonlinear solution.                              !
!   disso1   dissolution rate of particle1 [day-1].                    !
!   iloc_particle1    longitude (i) index of the model grid cell where !
!                     particle1 is added                               !
!   jloc_particle1    latitude (j) index of the model grid cell where  !
!                     particle1 is added                               !
!   kloc_particle1    vertical (k) index of the model grid cell where  !
!                     particle1 is added                               !
!   particle1_load    particle1 added to the grid cell per day         !
!                     [unit of particle1/m2/day].                      !
!   particle1_startload    starting day of particle1 load              !
!   particle1_endload      ending day of particle1 load                !
!                                                                      !
!   wPar1             vertical sinking velocity for particle1          !
!                     [meter day-1].                                   !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
!  Set biological tracer identification indices.
!
      integer, allocatable :: idbio(:)  ! Biological tracers

#   ifdef FULL_DISSOLVE
      integer :: iDissolve0              ! dissolve0
#   endif

      integer :: iParticle1              ! particle1
      integer :: iDissolve1              ! dissolve1

#   ifdef MULTI_PARTICLES
      integer :: iParticle2              ! particle2
      integer :: iDissolve2              ! dissolve2
      
      integer :: iParticle3              ! particle3
      integer :: iDissolve3              ! dissolve3

      integer :: iParticle4              ! particle4
      integer :: iDissolve4              ! dissolve4

      integer :: iParticle5              ! particle5
      integer :: iDissolve5              ! dissolve5

      integer :: iParticle6              ! particle6
      integer :: iDissolve6              ! dissolve6

      integer :: iParticle7              ! particle7
      integer :: iDissolve7              ! dissolve7

      integer :: iParticle8              ! particle8
      integer :: iDissolve8              ! dissolve8

      integer :: iParticle9              ! particle9
      integer :: iDissolve9              ! dissolve9
#   endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Biological 2D diagnostic variable IDs.
!

#endif
!
!  Biological parameters.
!
      integer, allocatable :: BioIter(:)

      real(r8), allocatable :: disso1(:)               ! 1/day
      real(r8), allocatable :: iloc_particle1(:)
      real(r8), allocatable :: jloc_particle1(:)
      real(r8), allocatable :: kloc_particle1(:)
      real(r8), allocatable :: particle1_load(:)
      real(r8), allocatable :: particle1_startload(:)
      real(r8), allocatable :: particle1_endload(:)
      real(r8), allocatable :: wPar1(:)   

#   ifdef FULL_DISSOLVE
      real(r8), allocatable :: iloc_dissolve0(:)
      real(r8), allocatable :: jloc_dissolve0(:)
      real(r8), allocatable :: kloc_dissolve0(:)
      real(r8), allocatable :: dissolve0_load(:)
      real(r8), allocatable :: dissolve0_startload(:)
      real(r8), allocatable :: dissolve0_endload(:)
#   endif

#   ifdef MULTI_PARTICLES
      real(r8), allocatable :: disso2(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle2(:)
      real(r8), allocatable :: jloc_particle2(:)
      real(r8), allocatable :: kloc_particle2(:)
      real(r8), allocatable :: particle2_load(:)
      real(r8), allocatable :: particle2_startload(:)
      real(r8), allocatable :: particle2_endload(:)
      real(r8), allocatable :: wPar2(:)   
      
      real(r8), allocatable :: disso3(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle3(:)
      real(r8), allocatable :: jloc_particle3(:)
      real(r8), allocatable :: kloc_particle3(:)
      real(r8), allocatable :: particle3_load(:)
      real(r8), allocatable :: particle3_startload(:)
      real(r8), allocatable :: particle3_endload(:)
      real(r8), allocatable :: wPar3(:) 

      real(r8), allocatable :: disso4(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle4(:)
      real(r8), allocatable :: jloc_particle4(:)
      real(r8), allocatable :: kloc_particle4(:)
      real(r8), allocatable :: particle4_load(:)
      real(r8), allocatable :: particle4_startload(:)
      real(r8), allocatable :: particle4_endload(:)
      real(r8), allocatable :: wPar4(:) 

      real(r8), allocatable :: disso5(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle5(:)
      real(r8), allocatable :: jloc_particle5(:)
      real(r8), allocatable :: kloc_particle5(:)
      real(r8), allocatable :: particle5_load(:)
      real(r8), allocatable :: particle5_startload(:)
      real(r8), allocatable :: particle5_endload(:)
      real(r8), allocatable :: wPar5(:) 

      real(r8), allocatable :: disso6(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle6(:)
      real(r8), allocatable :: jloc_particle6(:)
      real(r8), allocatable :: kloc_particle6(:)
      real(r8), allocatable :: particle6_load(:)
      real(r8), allocatable :: particle6_startload(:)
      real(r8), allocatable :: particle6_endload(:)
      real(r8), allocatable :: wPar6(:) 
      
      real(r8), allocatable :: disso7(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle7(:)
      real(r8), allocatable :: jloc_particle7(:)
      real(r8), allocatable :: kloc_particle7(:)
      real(r8), allocatable :: particle7_load(:)
      real(r8), allocatable :: particle7_startload(:)
      real(r8), allocatable :: particle7_endload(:)
      real(r8), allocatable :: wPar7(:) 
      
      real(r8), allocatable :: disso8(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle8(:)
      real(r8), allocatable :: jloc_particle8(:)
      real(r8), allocatable :: kloc_particle8(:)
      real(r8), allocatable :: particle8_load(:)
      real(r8), allocatable :: particle8_startload(:)
      real(r8), allocatable :: particle8_endload(:)
      real(r8), allocatable :: wPar8(:) 
          
      real(r8), allocatable :: disso9(:)               ! 1/day               
      real(r8), allocatable :: iloc_particle9(:)
      real(r8), allocatable :: jloc_particle9(:)
      real(r8), allocatable :: kloc_particle9(:)
      real(r8), allocatable :: particle9_load(:)
      real(r8), allocatable :: particle9_startload(:)
      real(r8), allocatable :: particle9_endload(:)
      real(r8), allocatable :: wPar9(:) 
#   endif

      CONTAINS

      SUBROUTINE initialize_biology
!
!=======================================================================
!                                                                      !
!  This routine sets several variables needed by the biology model.    !
!  It allocates and assigns biological tracers indices.                !
!                                                                      !
!=======================================================================
!
!  Local variable declarations
!
      integer :: i, ic
!
!-----------------------------------------------------------------------
!  Determine number of biological tracers.
!-----------------------------------------------------------------------
!

#   if defined MULTI_PARTICLES && defined FULL_DISSOLVE
      NBT=19
#   elif defined MULTI_PARTICLES && !defined FULL_DISSOLVE
      NBT=18
#   elif defined FULL_DISSOLVE && !defined MULTI_PARTICLES
      NBT=3
#   else
      NBT=2
#   endif

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!-----------------------------------------------------------------------
!  Set sources and sinks biology diagnostic parameters.
!-----------------------------------------------------------------------
!
!  Set number of diagnostics terms.
!
      NDbio2d=0
!
!  Initialize biology diagnostic indices.
!
      ic=0
#endif
!
!-----------------------------------------------------------------------
!  Allocate various module variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(BioIter)) THEN
        allocate ( BioIter(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso1)) THEN
        allocate ( disso1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle1)) THEN
        allocate ( iloc_particle1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle1)) THEN
        allocate ( jloc_particle1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle1)) THEN
        allocate ( kloc_particle1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle1_load)) THEN
        allocate ( particle1_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle1_startload)) THEN
        allocate ( particle1_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle1_endload)) THEN
        allocate ( particle1_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar1)) THEN
        allocate ( wPar1(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# ifdef FULL_DISSOLVE
      IF (.not.allocated(iloc_dissolve0)) THEN
        allocate ( iloc_dissolve0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_dissolve0)) THEN
        allocate ( jloc_dissolve0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_dissolve0)) THEN
        allocate ( kloc_dissolve0(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(dissolve0_load)) THEN
        allocate ( dissolve0_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(dissolve0_startload)) THEN
        allocate ( dissolve0_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(dissolve0_endload)) THEN
        allocate ( dissolve0_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# endif
# ifdef MULTI_PARTICLES
      IF (.not.allocated(disso2)) THEN
        allocate ( disso2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle2)) THEN
        allocate ( iloc_particle2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle2)) THEN
        allocate ( jloc_particle2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle2)) THEN
        allocate ( kloc_particle2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle2_load)) THEN
        allocate ( particle2_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle2_startload)) THEN
        allocate ( particle2_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle2_endload)) THEN
        allocate ( particle2_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar2)) THEN
        allocate ( wPar2(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso3)) THEN
        allocate ( disso3(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle3)) THEN
        allocate ( iloc_particle3(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle3)) THEN
        allocate ( jloc_particle3(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle3)) THEN
        allocate ( kloc_particle3(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle3_load)) THEN
        allocate ( particle3_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle3_startload)) THEN
        allocate ( particle3_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle3_endload)) THEN
        allocate ( particle3_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar3)) THEN
        allocate ( wPar3(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso4)) THEN
        allocate ( disso4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle4)) THEN
        allocate ( iloc_particle4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle4)) THEN
        allocate ( jloc_particle4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle4)) THEN
        allocate ( kloc_particle4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle4_load)) THEN
        allocate ( particle4_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle4_startload)) THEN
        allocate ( particle4_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle4_endload)) THEN
        allocate ( particle4_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar4)) THEN
        allocate ( wPar4(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso5)) THEN
        allocate ( disso5(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle5)) THEN
        allocate ( iloc_particle5(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle5)) THEN
        allocate ( jloc_particle5(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle5)) THEN
        allocate ( kloc_particle5(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle5_load)) THEN
        allocate ( particle5_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle5_startload)) THEN
        allocate ( particle5_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle5_endload)) THEN
        allocate ( particle5_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar5)) THEN
        allocate ( wPar5(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso6)) THEN
        allocate ( disso6(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle6)) THEN
        allocate ( iloc_particle6(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle6)) THEN
        allocate ( jloc_particle6(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle6)) THEN
        allocate ( kloc_particle6(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle6_load)) THEN
        allocate ( particle6_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle6_startload)) THEN
        allocate ( particle6_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle6_endload)) THEN
        allocate ( particle6_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar6)) THEN
        allocate ( wPar6(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso7)) THEN
        allocate ( disso7(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle7)) THEN
        allocate ( iloc_particle7(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle7)) THEN
        allocate ( jloc_particle7(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle7)) THEN
        allocate ( kloc_particle7(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle7_load)) THEN
        allocate ( particle7_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle7_startload)) THEN
        allocate ( particle7_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle7_endload)) THEN
        allocate ( particle7_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar7)) THEN
        allocate ( wPar7(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso8)) THEN
        allocate ( disso8(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle8)) THEN
        allocate ( iloc_particle8(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle8)) THEN
        allocate ( jloc_particle8(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle8)) THEN
        allocate ( kloc_particle8(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle8_load)) THEN
        allocate ( particle8_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle8_startload)) THEN
        allocate ( particle8_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle8_endload)) THEN
        allocate ( particle8_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar8)) THEN
        allocate ( wPar8(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(disso9)) THEN
        allocate ( disso9(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(iloc_particle9)) THEN
        allocate ( iloc_particle9(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(jloc_particle9)) THEN
        allocate ( jloc_particle9(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(kloc_particle9)) THEN
        allocate ( kloc_particle9(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle9_load)) THEN
        allocate ( particle9_load(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle9_startload)) THEN
        allocate ( particle9_startload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(particle9_endload)) THEN
        allocate ( particle9_endload(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(wPar9)) THEN
        allocate ( wPar9(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
# endif

!
!  Allocate biological tracer vector.
!
      IF (.not.allocated(idbio)) THEN
        allocate ( idbio(NBT) )
        Dmem(1)=Dmem(1)+REAL(NBT,r8)
      END IF

#if defined DIAGNOSTICS && defined DIAGNOSTICS_BIO
!
!  Allocate biological diagnostics vectors
!
      IF (.not.allocated(iDbio2)) THEN
        allocate ( iDbio2(NDbio2d) )
        Dmem(1)=Dmem(1)+REAL(NDbio2d,r8)
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Initialize tracer identification indices.
!-----------------------------------------------------------------------
!
      ic=NAT+NPT+NCS+NNS
      DO i=1,NBT
        idbio(i)=ic+i
      END DO
      iParticle1 = ic+1
      iDissolve1 = ic+2
      ic = ic+2
# ifdef FULL_DISSOLVE
      iDissolve0 = ic+1
      ic = ic+1
# endif
# ifdef MULTI_PARTICLES
      iParticle2 = ic+1
      iDissolve2 = ic+2
      iParticle3 = ic+3
      iDissolve3 = ic+4
      iParticle4 = ic+5
      iDissolve4 = ic+6
      iParticle5 = ic+7
      iDissolve5 = ic+8
      iParticle6 = ic+9
      iDissolve6 = ic+10
      iParticle7 = ic+11
      iDissolve7 = ic+12
      iParticle8 = ic+13
      iDissolve8 = ic+14
      iParticle9 = ic+15
      iDissolve9 = ic+16
      ic = ic+16
# endif

      RETURN
      END SUBROUTINE initialize_biology
