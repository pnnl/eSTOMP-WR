

!----------------------Subroutine--------------------------------------!
!
!      SUBROUTINE TPORT1( NSL )
      SUBROUTINE TPORT1( NSL,petsc_ksp,petsc_a,petsc_b,petsc_x,petsc_pc)
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
!     Solute/Reactive Species Transport Shell.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 13 September 2005.
!     Last Modified by MD White, PNNL, 13 September 2005.
!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR
      USE SOLTN
      USE JACOB
      use grid_mod
      use petscapp
      use trnspt
      use files
      use fdvp
      USE COUP_WELL
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      LOGICAL :: use_ga
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!--- PETSc
#include "petscwrapper.h"
!--------------------Parameter ----
!
    Mat :: mat
    PetscViewer :: viewer
    PetscErrorCode :: ierr
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/TPORT1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      maxiter = 4000
!
      residual(:,:) = 0.d0
      call MatZeroEntries(petsc_A,ierr)
!
!---  Compute solute sources ---
!
      IF( L_CW.EQ.1 ) THEN
          CALL SOLUT_COUP_WELL(NSL,petsc_A)
      ENDIF
      CALL SORT1( NSL, petsc_A )
!
!---  Zero solute transport fluxes  ---
!
!      CALL SFXZ( NSL )
!
!---  Load Jacobian matrix ---
!
      CALL SJCBL( NSL, petsc_A )
!
!---  Modify Jacobian matrix for boundary conditions ---
!
      CALL SBND1( NSL, petsc_A )
      residual=-1.d0*residual
      icnv = 3
      iter = -1
!      call petsc_solver_solve(icnv,iter,nstep)
      call petsc_solver_solve(1,icnv,iter,nstep,&
                petsc_ksp,petsc_a,petsc_b,petsc_x,petsc_pc)
      IF( icnv == 1 .and. me.eq.0) THEN
        WRITE(ISC,'(4X,A)') &
          '( Solution for Transport Equation is not convergent )'
        WRITE(IWR,'(4X,A)') &
          '( Solution for Transport Equation is not convergent )'
        ICNV = 1 
      ENDIF
!
!---  Update solute concentrations ---
!
      call update_nodes('blu',2,0,1)
      CALL UPDTC( NSL )
!
!---  Compute solute aqueous-phase fluxes (interior nodes)  ---
!
      CALL SFXL( NSL )
!
!---  Compute solute aqueous-phase fluxes (boundary surfaces)  ---
!
      CALL SFXLB( NSL )
!
!---  Integrate solute sources  ---
!
     IF( L_CW.EQ.1 ) THEN
                CALL SOLUIT_COUP_WELL(NSL)
     ENDIF
     CALL SORIT1( NSL )
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of TPORT1 group
!

!if(nsl==1) stop
      RETURN
      END
