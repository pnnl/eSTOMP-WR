

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SBND1( NSL,petsc_A )
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their employees, makes any
!     warranty, express or implied, or assumes any legal liability or
!     responsibility for the accuracy, completeness, or usefulness
!     of any information, apparatus, product, software or process
!     disclosed, or represents that its use would not infringe
!     privately owned rights.
!
!----------------------Acknowledgement---------------------------------!
!
!     This software and its documentation were produced with Government
!     support under Contract Number DE-AC06-76RLO-1830 awarded by the
!     United Department of Energy. The Government retains a paid-up
!     non-exclusive, irrevocable worldwide license to reproduce,
!     prepare derivative works, perform publicly and display publicly
!     by or for the Government, including the right to distribute to
!     other Government contractors.
!
!---------------------Copyright Notices--------------------------------!
!
!            Copyright Battelle Memorial Institute, 1996
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Modify the Jacobian matrix for the solute transport equation
!     to incorporate boundary conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!




      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      USE BCVP
      USE BCV
      use grid_mod
      use petscapp
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!--- Petsc includes
!
#include "petscwrapper.h"
!
!----------------------Parameter Statements----------------------------!
!

      PetscInt :: ic(2),ir(2),nr,nc 
      PetscScalar :: values_(4)
      PetscErrorCode :: ierr
!
      REAL*8 BCX(LSPBC+1)
      double precision :: s_fx_up(3),s_area_x(3)
      LOGICAL :: ISJCB
      LOGICAL :: use_ga
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/SBND1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
       '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over number of specified boundary conditions  ---
!
      NBCT = MIN( NSL+LUK,NSOLU+LUK+1 )
      AP = 0.D+0
      WCZ = 0.D+0
      FLB = 0.D+0
      FCL = 0.D+0
      FCLP = 0.D+0
      WCZ = 0.D+0
      DLZ = 0.D+0
      ISJCB = .TRUE.
      DO 200 NB = 1,num_bcnx
        ICYCLE = 0
        CALL CBND1( NB,NBCT,NSL,ICYCLE,AP,DLZ,FLB,FCL,FCLP,WCZ,ISJCB )
        IF( ICYCLE.EQ.1 )CYCLE
        nr = 1
        nc = 1
        n = ibcn(nb)
!        irow = loc_map(n)-1
! 2 matrix -BH
        irow = gloc_map(n)-1
!
        ir(1) = irow
        ic(1) = irow
        values_(1) = ap
!       print *,'sol_bc',me,n,ap,q_flux_b(1,nb)
        call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SBND1 group  ---
!

      RETURN
      END
