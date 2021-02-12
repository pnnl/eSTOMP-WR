SUBROUTINE PETSC_SOLVER_INIT( petsc_option,RTOL,ATOL,MAXITER,NUK,L_SIZE,LL_size,G_SIZE,num_nodes,num_loc_nodes,nnz_d,nnz_o,ixp,imxp,id_l2g )
!
!---  Notice  ---
!
!  This software, which is provided in confidence, was prepared
!  by employees of Pacific Northwest National Laboratory operated 
!  by Battelle Memorial Institute, under Contract DE-AC06-76RLO1830 
!  with the U.S. Department of Energy (DOE).  Battelle has certain 
!  unperfected rights in the software which should not be copied or 
!  otherwise disseminated outside your organization without the express 
!  written authorization from Battelle or DOE. All rights in the software 
!  are reserved by DOE and Battelle. Neither the Government nor Battelle 
!  makes any warranty, express or implied, or assumes any liability or 
!  responsibility for the use of this software.
!
!---  Description  ---
!
!  STOMP90:  Subsurface Transport Over Multiple Phases FORTRAN 90
!  Operational Mode:  Water-Oil
!
!  This subroutine initializes the PETSc solver.
!  
!---  Authors  ---
!
!  Written by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on 24 February 2003.
!  Last Modified by MD White, PNNL on 16 May 2003.
!
!---  Specification Statements  ---
!
  USE PETSCAPP
!
!---  Implicit Statements  ---
!
  IMPLICIT NONE
!
!---  Arguments  ---
!
  REAL(8) :: RTOL,ATOL
  INTEGER :: MAXITER,NUK,NLOF,G_SIZE,l_size,ll_size
  integer, dimension(num_loc_nodes) :: nnz_d,nnz_o
  integer, dimension(num_nodes) :: ixp,imxp,id_l2g
  integer :: num_nodes, nrx,n,m, num_loc_nodes
  integer :: petsc_option
!
!---  PETSc Fortran Includes  ---
!
#include "petscwrapper.h"
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!---  Locals  ---
!
  INTEGER :: NPES
  PetscInt :: l2gmap(ll_size),output(12),imxpx(12)
  PetscInt :: nr,num_nodesx
  PetscErrorCode :: ierr
  integer :: me
!
!---  Executable Statements  ---
!
  me = ga_nodeid()
  LOCAL_SIZE = L_SIZE
  GLOBAL_SIZE = G_SIZE
!
!--- E4D PATCH
!    Split MPI_COMM_WORLD
!  CALL MPI_COMM_SIZE( MPI_COMM_WORLD,NPES,IERR )
   CALL MPI_COMM_SIZE( PETSC_COMM_WORLD,NPES,IERR)
!--- END E4D PATCH

!--- Create mapping b/w local and global ordering
  nrx = 0
  do n=1,num_nodes
    if(ixp(n) <= 0) cycle
    do m=1,nuk
      nrx = nrx+1
      l2gmap(nrx) = (imxp(n)-1)*nuk+m-1
!print *,'imxp-=-------imxp',me,n,imxp(n),nrx,l2gmap(nrx)
!,nrx,l2gmap(nrx),(imxp(n)-1)*nuk+m-1,nuk
    enddo
  enddo
  nr = nrx
!
!---  Deallocate matrix pointer array memory  ---
!
  ALLOCATE( D_NNZ(L_SIZE) )
  ALLOCATE( O_NNZ(L_SIZE) )
  ALLOCATE( N_COL(L_SIZE) )
  ALLOCATE( N_ROW(L_SIZE) )
  ALLOCATE( VALUES(L_SIZE) )
!
!---  Initialize matrix pointers  ---
!

  CALL INIT_SPARSE( ixp,nnz_d,nnz_o,id_l2g,num_nodes,num_loc_nodes,l_size,nuk,d_nnz,o_nnz )
!
!---  Create sequential or parallel PETSc solution vector  ---
!
  IF( NPES == 1 ) THEN
    CALL VecCreateSeq( PETSC_COMM_SELF,G_SIZE,petsc_X,IERR )
  ELSE
    CALL VecCreateMPI( PETSC_COMM_WORLD,L_SIZE,G_SIZE,petsc_X,IERR )
  END IF
!
!---  Set PETSc vector options from command line for the solution
!     vector  ---
!
  CALL VecSetFromOptions( petsc_X,IERR )
!
!---  Create the PETSc problem vector  ---
!
  CALL VecDuplicate( petsc_X,petsc_B,IERR )
!
!---  Create sequential or parallel PETSc matrix and
!     PETSc scalable linear equation solver (KSP)  ---
!
  if(npes == 1) then
!#ifdef PETSC_3_3
!   call ISLocalToGlobalMappingCreate(PETSC_COMM_SELF, nrx, l2gmap, PETSC_COPY_VALUES,mapping,ierr)
!#else
   call ISLocalToGlobalMappingCreate(PETSC_COMM_SELF, nrx, l2gmap, mapping,ierr)
!#endif
  else
!#ifdef PETSC_3_3
!   call ISLocalToGlobalMappingCreate(PETSC_COMM_WORLD, nrx, l2gmap,PETSC_COPY_VALUES, mapping,ierr)
!#else
   call ISLocalToGlobalMappingCreate(PETSC_COMM_SELF, nrx, l2gmap, mapping,ierr)
!#endif
  endif
!num_nodesx = num_nodes
!do n=1,num_nodes
!imxpx(n)=n-1
!enddo
!do n=13,24
!imxpx(n)=n-1
!enddo
!call ISLocalToGlobalMappingApply(mapping,num_nodesx,imxpx,output,ierr)
!call ISGlobalToLocalMappingApply(mapping,IS_GTOLM_MASK,num_nodesx,imxpx,PETSC_NULL,output,ierr)
!  call PetscIntView(num_nodesx,output,PETSC_VIEWER_STDOUT_SELF,ierr)
!print *,'me',me,'output---------',output
!vlf
  IF( NPES == 1 ) THEN
    CALL MatCreateSeqAIJ( PETSC_COMM_SELF,G_SIZE,G_SIZE,0,D_NNZ,petsc_A,IERR )
    CALL KSPCreate( PETSC_COMM_SELF,petsc_ksp,IERR ) 
  ELSE
!#ifdef PETSC_3_3
!    CALL MatCreateAIJ( PETSC_COMM_WORLD,L_SIZE,L_SIZE, &
!       G_SIZE,G_SIZE,0,D_NNZ,0,O_NNZ,petsc_A,IERR )
!#else
    CALL MatCreateMPIAIJ( PETSC_COMM_WORLD,L_SIZE,L_SIZE, &
       G_SIZE,G_SIZE,0,D_NNZ,0,O_NNZ,petsc_A,IERR )
!#endif
    CALL KSPCreate( PETSC_COMM_WORLD,petsc_ksp,IERR ) 
  END IF
!#ifdef PETSC_3_3
!  call MatSetLocalToGlobalMapping(petsc_A,mapping,mapping,ierr)
!#else
  call MatSetLocalToGlobalMapping(petsc_A,mapping,ierr)
!#endif
!
!---  Set PETSc matrix associated with the PETSc linear equation solver
!     and set PETSc matrix associated with the PETSc preconditioner   ---
!
  CALL KSPSetOperators( petsc_ksp,petsc_A,petsc_A,SAME_NONZERO_PATTERN,IERR )
!
!---  Get the PETSc preconditioner (PC) associated with PETSc 
!     linear equation solver (KSP)   ---
!
!vlf iterative solve
  CALL KSPGetPC( petsc_ksp,petsc_pc,IERR )
!vlf iterative solve
!vlf direct solve
!  call PCSetType(petsc_pc,PCLU,ierr)
!vlf
!  call PCSetType(petsc_pc,PCASM,ierr)
!  call PCSetType(petsc_pc,PCCOMPOSITE,ierr)
!  call PCCompositeAddPC(petsc_pc,PCILU,ierr )
!  call PCCompositeAddPC(petsc_pc,PCASM,ierr)
!
!---  Build the PETSc Krylov subspace method (KSP) type associated with 
!     PETSc linear equation solver (KSP)   ---
!vlf iterative solve
   CALL KSPSetType( petsc_ksp,KSPBCGS,IERR )
!vlf iterative solve
!vlf direct solve
!  CALL KSPSetType( petsc_ksp,KSPPREONLY,IERR )
!vlf
!
!---  Set linear equation solver (KSP), preconditioner (PC), and
!     Krylov subspace method (KSP) parameters from the command line  ---
!
  CALL KSPSetFromOptions( petsc_ksp,IERR )
  if(petsc_option == 1) &
  call kspsettolerances(petsc_ksp,rtol,atol,petsc_default_double_precision,petsc_default_integer,ierr)
!
!---  Return to calling routine  ---
!
  RETURN
!
!---  End Subroutine  ---
!
END SUBROUTINE PETSC_SOLVER_INIT


SUBROUTINE PETSC_SOLVER_DESTROY()
!
!---  Notice  ---
!
!  This software, which is provided in confidence, was prepared
!  by employees of Pacific Northwest National Laboratory operated 
!  by Battelle Memorial Institute, under Contract DE-AC06-76RLO1830 
!  with the U.S. Department of Energy (DOE).  Battelle has certain 
!  unperfected rights in the software which should not be copied or 
!  otherwise disseminated outside your organization without the express 
!  written authorization from Battelle or DOE. All rights in the software 
!  are reserved by DOE and Battelle. Neither the Government nor Battelle 
!  makes any warranty, express or implied, or assumes any liability or 
!  responsibility for the use of this software.
!
!---  Description  ---
!
!  STOMP90:  Subsurface Transport Over Multiple Phases FORTRAN 90
!  Operational Mode:  Water-Oil
!
!  This subroutine destroys the PETSc solver.
!  
!---  Authors  ---
!
!  Written by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on 24 February 2003.
!  Last Modified by MD White, PNNL on 16 May 2003.
!
!---  Specification Statements  ---
!
  USE PETSCAPP
!
!---  Implicit Statements  ---
!
  IMPLICIT NONE
!
!---  Locals  ---
!
  INTEGER :: IERR
!
!---  Destroy PETSc solution vector  ---
!
  CALL VecDestroy( petsc_X,IERR )
!
!---  Destroy PETSc problem vector  ---
!
  CALL VecDestroy( petsc_B,IERR )
!
!---  Destroy PETSc matrix  ---
!
  CALL MatDestroy( petsc_A,IERR )
  call ISLocalToGlobalMappingDestroy(mapping,ierr)
!
!---  Destroy PETSc linear equation solver (KSP)  ---
!
  CALL KSPDestroy( petsc_ksp,IERR )
!
!---  Deallocate matrix pointer array memory  ---
!
  DEALLOCATE( D_NNZ )
  DEALLOCATE( O_NNZ )
  DEALLOCATE( N_COL )
  DEALLOCATE( N_ROW )
  DEALLOCATE( VALUES )
!
!---  Return to calling routine  ---
!
  RETURN
!
!---  End Subroutine  ---
!
END SUBROUTINE PETSC_SOLVER_DESTROY


SUBROUTINE PETSC_SOLVER_FINAL
!
!---  Notice  ---
!
!  This software, which is provided in confidence, was prepared
!  by employees of Pacific Northwest National Laboratory operated 
!  by Battelle Memorial Institute, under Contract DE-AC06-76RLO1830 
!  with the U.S. Department of Energy (DOE).  Battelle has certain 
!  unperfected rights in the software which should not be copied or 
!  otherwise disseminated outside your organization without the express 
!  written authorization from Battelle or DOE. All rights in the software 
!  are reserved by DOE and Battelle. Neither the Government nor Battelle 
!  makes any warranty, express or implied, or assumes any liability or 
!  responsibility for the use of this software.
!
!---  Description  ---
!
!  STOMP90:  Subsurface Transport Over Multiple Phases FORTRAN 90
!  Operational Mode:  Water-Oil
!
!  This subroutine finalizes the PETSc solver.
!  
!---  Authors  ---
!
!  Written by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on 24 February 2003.
!  Last Modified by MD White, PNNL on 16 May 2003.
!
!---  Specification Statements  ---
!
  USE PETSCAPP
!
!---  Implicit Statements  ---
!
  IMPLICIT NONE
!
!---  PETSc Fortran Includes  ---
!
!#include "include/finclude/petsc.h"
!
!---  Locals  ---
!
  INTEGER :: IERR
!
!---  Finalize PETSc  ---
!
  CALL PetscFinalize( IERR )
!
!---  Return to calling routine  ---
!
  RETURN
!
!---  End Subroutine  ---
!
END SUBROUTINE PETSC_SOLVER_FINAL

SUBROUTINE PETSC_SOLVER_SOLVE( ICNV,ITER,nstep )
!
!---  Notice  ---
!
!  This software, which is provided in confidence, was prepared
!  by employees of Pacific Northwest National Laboratory operated 
!  by Battelle Memorial Institute, under Contract DE-AC06-76RLO1830 
!  with the U.S. Department of Energy (DOE).  Battelle has certain 
!  unperfected rights in the software which should not be copied or 
!  otherwise disseminated outside your organization without the express 
!  written authorization from Battelle or DOE. All rights in the software 
!  are reserved by DOE and Battelle. Neither the Government nor Battelle 
!  makes any warranty, express or implied, or assumes any liability or 
!  responsibility for the use of this software.
!
!---  Description  ---
!
!  STOMP90:  Subsurface Transport Over Multiple Phases FORTRAN 90
!  Operational Mode:  Water-Oil
!
!  This subroutine the initial thermodynamic and hydrologic states.
!  
!---  Authors  ---
!
!  Written by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on 24 February 2003.
!  Last Modified by MD White, PNNL on 16 May 2003.
!
!---  Specification Statements  ---
!
  USE PETSCAPP
  use grid_mod
!
!---  Implicit Statements  ---
!
  IMPLICIT NONE
!
!---  Arguments  ---
!
  INTEGER :: ICNV,ITER
!
!---  PETSc Fortran Includes  ---
!
#include "petscwrapper.h"
#include "mafdecls.fh"
#include "global.fh"
!
!
!---  Locals  ---
!
!  INTEGER :: IERR,NPES,NSTEP,NITER,IVL,IVH
  integer, save :: npes
  INTEGER :: NSTEP,NITER,IVL,IVH
  INTEGER :: IERR_L,IERR_G,me,ireason
  LOGICAL IFLAG,use_ga
  PetscScalar      X_ARRAY(1)
  PetscOffset      I_X
  PetscViewer      PV_VIEWER,M_VIEWER,SV_VIEWER,KSP_VIEWER 
  PetscErrorCode   ierr
  KSPConvergedReason reason
  double precision :: t_b,t_e
!
!---  Viewing option flag ---
!
  t_b = MPI_Wtime()
      me = ga_nodeid()
      use_ga = .true.
  IFLAG = ( ITER < 0 )
!
!--- E4D PATCH 
!    Split MPI_COMM_WORLD
!  IF( IFLAG ) CALL MPI_COMM_SIZE( MPI_COMM_WORLD,NPES,IERR )
   IF( IFLAG ) CALL MPI_COMM_SIZE( PETSC_COMM_WORLD,NPES,IERR )
!--- END E4D PATCH
!
!---  Load and assemble problem vector ---
!
  CALL LOAD_PETSC_VECTOR( N_COL,VALUES )
  CALL VecSetValues( petsc_B,LOCAL_SIZE,N_COL,VALUES,INSERT_VALUES,IERR )
!  CALL MPI_BARRIER( MPI_COMM_WORLD,IERR)
  CALL VecAssemblyBegin( petsc_B,IERR )
  CALL VecAssemblyEnd( petsc_B,IERR )
!
!---  View sequential or parallel problem vector  ---
!
!  IF ( IFLAG ) THEN
!if(nstep.eq.9)then
    IF( NPES == 1 ) THEN
!      CALL PetscViewerASCIIOpen( PETSC_COMM_SELF,"prob_vec.dat",PV_VIEWER,IERR )
    ELSE
!     CALL PetscViewerASCIIOpen( PETSC_COMM_WORLD,"prob_vec.dat",PV_VIEWER,IERR )
    END IF    
!    CALL VecView( petsc_B,PV_VIEWER,IERR )
!    CALL PetscViewerDestroy( PV_VIEWER,IERR )
!  END IF
!
!---  Load and assemble Jacobian matrix ---
!
!  CALL LOAD_PETSC_MATRIX()
  CALL MatAssemblyBegin( petsc_A,MAT_FINAL_ASSEMBLY,IERR )
  CALL MatAssemblyEnd( petsc_A,MAT_FINAL_ASSEMBLY,IERR )
!
!---  View sequential or parallel Jacobian matrix  ---
!
!  IF ( IFLAG ) THEN
!print *,'npes============npes',npes
!if(nstep.eq.9)then
    IF( NPES == 1 ) THEN
!      CALL PetscViewerASCIIOpen( PETSC_COMM_SELF,"matrix.dat",M_VIEWER,IERR )
    ELSE
!      CALL PetscViewerASCIIOpen( PETSC_COMM_WORLD,"matrix.dat",M_VIEWER,IERR )
    END IF    
!    CALL MatView( petsc_A,M_VIEWER,IERR )
!    CALL PetscViewerDestroy( M_VIEWER,IERR )
!  END IF
!
!---  View linear equation solver (KSP)  ---
!
!  IF ( IFLAG ) THEN
!     CALL KSPView( ksp,KSP_VIEWER,IERR )
!  END IF
!
!---  Solve linear equation (KSP)  ---
!
  CALL KSPSetOperators( petsc_ksp,petsc_A,petsc_A,SAME_NONZERO_PATTERN,IERR )
  CALL KSPSolve( petsc_ksp,petsc_B,petsc_X,IERR )
  call KSPGetIterationNumber(petsc_ksp,iter,ierr)
       petsc_iter = petsc_iter + iter
  call KSPGetConvergedReason(petsc_ksp,reason,ierr)
!print *,'ksp iter',iter,ierr,petsc_B,petsc_X
!
!---  Check for errors  ----
!
  IERR_L = IERR
  IERR_G = IERR
  ireason = reason
!fp! emit({#exec_list# CALL MPI_ALLREDUCE( IERR_L,IERR_G,1,MPI_INTEGER,MPI_MAX,MPI_COMM_WORLD,IERR); #});
  call ga_igop(1,ierr_g,1,'max')
  call ga_igop(1,ireason,1,'max')
  IF( IERR_G /= 0 .or. ireason < 0) ICNV = 1
!
!---  View sequential or parallel solution vector  ---
!
!  IF ( IFLAG ) THEN
!print *,'petsc_comm',petsc_comm_self,sv_viewer,ierr
!if(nstep.eq.9)then
    IF( NPES == 1 ) THEN
!     CALL PetscViewerASCIIOpen(PETSC_COMM_SELF,"solu_vec.dat",SV_VIEWER,IERR )
    ELSE
!     CALL PetscViewerASCIIOpen( PETSC_COMM_WORLD,"solu_vec.dat",SV_VIEWER,IERR )
    END IF    
!     CALL VecView( petsc_X,SV_VIEWER,IERR )
!     CALL PetscViewerDestroy( SV_VIEWER,IERR )
!  END IF
!stop
!
!---  Get PETSc solution vector  ---
!
  CALL VecGetArray( petsc_X,X_ARRAY,I_X,IERR ) 
!
!---  Copy PETSc solution vector into STOMP solution vector  ---
!
  CALL VecGetOwnershipRange(petsc_X,IVL,IVH,IERR) 
  CALL DUMP_PETSC_VECTOR( X_ARRAY(I_X+1),IVL )
  CALL VecRestoreArray( petsc_X,X_ARRAY,I_X,IERR ) 
  t_e = MPI_Wtime();
  petsc_time = petsc_time + t_e-t_b
!
END SUBROUTINE PETSC_SOLVER_SOLVE

SUBROUTINE SET_PETSC_VALUES ( NR,IR,NC,IC,VALUE )
!
!---  Notice  ---
!
!  This software, which is provided in confidence, was prepared
!  by employees of Pacific Northwest National Laboratory operated 
!  by Battelle Memorial Institute, under Contract DE-AC06-76RLO1830 
!  with the U.S. Department of Energy (DOE).  Battelle has certain 
!  unperfected rights in the software which should not be copied or 
!  otherwise disseminated outside your organization without the express 
!  written authorization from Battelle or DOE. All rights in the software 
!  are reserved by DOE and Battelle. Neither the Government nor Battelle 
!  makes any warranty, express or implied, or assumes any liability or 
!  responsibility for the use of this software.
!
!---  Description  ---
!
!  STOMP90:  Subsurface Transport Over Multiple Phases FORTRAN 90
!  Operational Mode:  Water-Oil
!
!  This subroutine sets multiple PETSc elements.
!  
!---  Authors  ---
!
!  Written by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on 24 February 2003.
!  Last Modified by MD White, PNNL on 16 May 2003.
!
!---  Specification Statements  ---
!
  USE PETSCAPP
!
!---  Implicit Statements  ---
!
  IMPLICIT NONE
!
!---  Arguments  ---
!
  INTEGER, INTENT(IN) :: NR,NC
  REAL(8), DIMENSION(NR,NC) :: VALUE
  INTEGER, DIMENSION(NR) :: IR
  INTEGER, DIMENSION(NC) :: IC
!
!---  PETSc Fortran Includes  ---
!
#include "petscwrapper.h"
!
!---  Locals  ---
!
  INTEGER :: IERR
!
!---  Executable Statements  ---
!
!  CALL MatSetValues( A,NR,IR,NC,IC,VALUE,INSERT_VALUES,IERR )
!
!---  Return to calling routine  ---
!
  RETURN
!
!---  End Subroutine  ---
!
END SUBROUTINE SET_PETSC_VALUES

SUBROUTINE SET_PETSC_VALUE( IR,IC,VALUE )
!
!---  Notice  ---
!
!  This software, which is provided in confidence, was prepared
!  by employees of Pacific Northwest National Laboratory operated 
!  by Battelle Memorial Institute, under Contract DE-AC06-76RLO1830 
!  with the U.S. Department of Energy (DOE).  Battelle has certain 
!  unperfected rights in the software which should not be copied or 
!  otherwise disseminated outside your organization without the express 
!  written authorization from Battelle or DOE. All rights in the software 
!  are reserved by DOE and Battelle. Neither the Government nor Battelle 
!  makes any warranty, express or implied, or assumes any liability or 
!  responsibility for the use of this software.
!
!---  Description  ---
!
!  STOMP90:  Subsurface Transport Over Multiple Phases FORTRAN 90
!  Operational Mode:  Water-Oil
!
!
!  This subroutine sets a single PETSc element.
!  
!---  Authors  ---
!
!  Written by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on August 10, 1999.
!  Last Modified by MD White, PNNL on 24 February 2003.
!  Last Modified by MD White, PNNL on 16 May 2003.
!
!---  Specification Statements  ---
!
  USE PETSCAPP
!
!---  Implicit Statements  ---
!
  IMPLICIT NONE
!
!---  Arguments  ---
!
  INTEGER :: IR,IC
  REAL(8) :: VALUE
!
!---  PETSc Fortran Includes  ---
!
#include "petscwrapper.h"
!
!---  Locals  ---
!
  INTEGER :: IERR
!
!---  Executable Statements  ---
!
!  CALL MatSetValue( A,IR,IC,VALUE,INSERT_VALUES,IERR )
!
!---  Return to calling routine  ---
!
  RETURN
!
!---  End Subroutine  ---
!
END SUBROUTINE SET_PETSC_VALUE

SUBROUTINE init_sparse(ixp,nnz_d,nnz_o,id_l2g,num_nodes,num_loc_nodes,lsize,nuk,d_nnz,o_nnz)
!
  implicit none
!
  integer :: n, nr, num_nodes, nuk, m, lsize, nx,num_loc_nodes
  integer, dimension(lsize) :: d_nnz, o_nnz 
  integer, dimension(num_nodes) :: ixp
  integer, dimension(num_loc_nodes) :: nnz_d, nnz_o,id_l2g
!
  nr = 0
  d_nnz = 0
  o_nnz = 0
  do n = 1,num_loc_nodes
    nx = id_l2g(n)
!    nx = n
    if(ixp(nx) <= 0) cycle
    do m=1,nuk
      nr = nr+1  
      d_nnz(nr) = nnz_d(n)
      o_nnz(nr) = nnz_o(n)
    enddo
  enddo
!
END SUBROUTINE init_sparse

subroutine load_petsc_vector(ir,value)
 use grid
 use grid_mod
 use jacob
use soltn
 implicit none
 integer, dimension(lsize) :: ir
 double precision, dimension(lsize) :: value
 integer :: n, nx, nr, nuk,m
 nuk = isvc
 nr = 0
 do n=1,num_loc_nodes
   nx = id_l2g(n)
   if( ixp(nx) <= 0 ) cycle
   do m=1,nuk
     nr = nr + 1
     ir(nr) = (imxp(nx)-1)*nuk+m-1
     value(nr) = -1.d0*residual(m,nx)
!if(nstep.eq.26)print *,'nx-------------------load',value(nr),loc2nat(nx),niter
   enddo
 enddo
end subroutine load_petsc_vector

subroutine dump_petsc_vector(vec,ivl)
 use grid
 use grid_mod
 use jacob
 implicit none
 double precision, dimension(lsize) :: vec
 integer :: n, nx, nr, nuk,m, ivl
 nuk = isvc
 nr = 0
 do n=1,num_loc_nodes
   nx = id_l2g(n)
   if( ixp(nx) <= 0 ) cycle
   do m=1,nuk
     nr = nr + 1
     blu(m,nx) = vec(nr)
   enddo
 enddo
end subroutine dump_petsc_vector
