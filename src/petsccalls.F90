!SUBROUTINE PETSC_SOLVER_INIT( petsc_option,RTOL,ATOL,MAXITER,NUK,L_SIZE,LL_size,G_SIZE,num_nodes,num_loc_nodes,nnz_d,nnz_o,ixp,imxp,id_l2g )
SUBROUTINE PETSC_SOLVER_INIT(petsc_option,RTOL,ATOL,MAXITER,NUK,num_nodes,num_loc_nodes,ixp,imxp,imxp_ncw,id_l2g, &
                        petsc_A,petsc_B,petsc_X,petsc_ksp,mapping,petsc_pc)
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
  USE COUP_WELL
  USE GLB_PAR
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
  integer, dimension(num_nodes) :: imxp_ncw
  integer :: isvcx
  integer :: b_size
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
!  PetscInt :: l2gmap(ll_size),output(12),imxpx(12)
  PetscInt :: output(12),imxpx(12)
  PetscInt, dimension(:), allocatable :: l2gmap
  PetscInt :: nr,num_nodesx
  PetscErrorCode :: ierr
  Vec     :: petsc_x, Petsc_b
  Mat     :: petsc_A
  KSP     :: petsc_ksp
  KSP     :: petsc_pc
  ISLocalToGlobalMapping :: mapping
  integer :: me
  double precision :: dtol
  integer:: kmit
  integer :: iwx,lwx
  integer :: ndifx,nx
  integer :: lw_size,n_cwx,iwnx,gidx
  integer :: lsize,llsize,gsize
!
!---  Executable Statements  ---
!
  me = ga_nodeid()
!  LOCAL_SIZE = L_SIZE
!  GLOBAL_SIZE = G_SIZE
  isvcx = nuk   ! 2 matrix -Bryan
!
!--- E4D PATCH
!    Split MPI_COMM_WORLD
!  CALL MPI_COMM_SIZE( MPI_COMM_WORLD,NPES,IERR )
   CALL MPI_COMM_SIZE( PETSC_COMM_WORLD,NPES,IERR)
!--- END E4D PATCH

!--- Create mapping b/w local and global ordering
!  nrx = 0
!  do n=1,num_nodes
!    if(ixp(n) <= 0) cycle
!    do m=1,nuk
!      nrx = nrx+1
!      l2gmap(nrx) = (imxp(n)-1)*nuk+m-1
!!print *,'imxp-=-------imxp',me,n,imxp(n),nrx,l2gmap(nrx)
!!,nrx,l2gmap(nrx),(imxp(n)-1)*nuk+m-1,nuk
!    enddo
!  enddo
!  nr = nrx

!--- Create mapping b/w local and global ordering
!************Coupled well - Bryan**************************
!**********************************************************
!**************for 2 matrix - Bryan
  l_size = 0
  DO n=1,num_loc_nodes
      nx = id_l2g(n)
      IF( ixp(nx) <= 0 ) CYCLE
      l_size = l_size + isvcx
  END DO

  if(l_cw > 0) then
    lw_size = 0
    do n_cwx = 1,n_l_cw
      iwnx = id_cw(3,n_cwx)
      gidx = iwi_cw(iwnx)
! injection node, withdrawal later...
      if(gidx == 1) then
        lw_size = lw_size+1
      endif
    enddo
  endif

  if(n_cw /= 0) l_size = l_size + lw_size
  g_size = l_size
  call ga_igop(1,g_size,1,'+')

  ll_size = 0

  DO n=1,num_nodes
    IF( ixp(n) <= 0 ) CYCLE
    ll_size = ll_size + isvcx
  END DO
  if(n_cw /= 0) ll_size = ll_size+lw_size
!  write(*,*) 'll_size:',ll_size
  allocate(l2gmap(ll_size))
  lsize = l_size
  gsize = g_size
  llsize = ll_size
  LOCAL_SIZE = L_SIZE
!  GLOBAL_SIZE = G_SIZE
  CALL MPI_COMM_SIZE( MPI_COMM_WORLD,NPES,IERR )

!*******************************************************
!  if(.not.allocated(l2gmap)) allocate(l2gmap(ll_size))
  nrx = 0
  if(n_cw <= 0) then
   do n=1,num_nodes
    if(ixp(n) <= 0) cycle
    do m=1,nuk
      nrx = nrx+1
      l2gmap(nrx) = (imxp_ncw(n)-1)*nuk+m-1
    enddo
   enddo
  else
   allocate(mmap_cw(n_cw))
   mmap_cw = 0
   do n=1,num_nodes
    if(ixp(n) <= 0) cycle
    if(iwt_cw(n) > 0) then
      iwx = iwt_cw(n)
      nrx = nrx+1
!      if(nrx.eq.1) then
!       l2gmap(nrx) = (imxp(n)-iwx-lwstart-1)*nuk+lwstart-1
       l2gmap(nrx) = imxp(n)-nuk
!      else
       l2gmap(nrx) = imxp(n)-nuk-1
!      endif
      lwx = w_loc(iwx)
      mmap_cw(id_cw(7,lwx)) = l2gmap(nrx)
    endif
    do m=1,nuk
      nrx = nrx+1
      l2gmap(nrx) = imxp(n)-nuk+m-1
    enddo
   enddo
   call ga_igop(1,mmap_cw,n_cw,'max')
  endif
  nr = nrx
!  write(*,*) 'mmap_cw',mmap_cw
!***************************************************************
!
!---  Deallocate matrix pointer array memory  ---
!
  ALLOCATE( D_NNZ(L_SIZE) )
  ALLOCATE( O_NNZ(L_SIZE) )
!  ALLOCATE( N_COL(L_SIZE) )
!  ALLOCATE( N_ROW(L_SIZE) )
!  ALLOCATE( VALUES(L_SIZE) )
!
!---  Initialize matrix pointers  ---
!
  CALL INIT_SPARSE(l_size,nuk,d_nnz,o_nnz )
!  CALL INIT_SPARSE( ixp,nnz_d,nnz_o,id_l2g,num_nodes,num_loc_nodes,l_size,nuk,d_nnz,o_nnz )
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
   call ISLocalToGlobalMappingCreate(PETSC_COMM_world, nrx, l2gmap,mapping,ierr)
!   call ISLocalToGlobalMappingCreate(PETSC_COMM_SELF, nrx, l2gmap, mapping,ierr)
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
  deallocate(l2gmap)
  DEALLOCATE( D_NNZ )
  DEALLOCATE( O_NNZ )
!
!---  Return to calling routine  ---
!
  RETURN
!
!---  End Subroutine  ---
!
END SUBROUTINE PETSC_SOLVER_INIT


SUBROUTINE PETSC_SOLVER_DESTROY(petsc_ksp,petsc_a,petsc_b,petsc_x,mapping)
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
  Vec     :: petsc_x, Petsc_b
  Mat     :: petsc_A
  KSP     :: petsc_ksp
  ISLocalToGlobalMapping :: mapping
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
!  DEALLOCATE( D_NNZ )
!  DEALLOCATE( O_NNZ )
!  DEALLOCATE( N_COL )
!  DEALLOCATE( N_ROW )
!  DEALLOCATE( VALUES )
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

SUBROUTINE PETSC_SOLVER_SOLVE(nuk,ICNV,ITER,nstep,&
                 petsc_ksp,petsc_a,petsc_b,petsc_x,petsc_pc  )
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
  Vec     :: petsc_x, Petsc_b
  Mat     :: petsc_A
  KSP     :: petsc_ksp
  PC      :: petsc_pc
  integer :: nuk
!
!---  Viewing option flag ---
!
  t_b = MPI_Wtime()
      me = ga_nodeid()
      use_ga = .true.
  
  call VecGetLocalSize(petsc_B,local_size,ierr)
  ALLOCATE( N_COL(LOCAL_SIZE) )
  ALLOCATE( VALUES(LOCAL_SIZE) )

  IFLAG = ( ITER < 0 )
!
!--- E4D PATCH 
!    Split MPI_COMM_WORLD
  IF( IFLAG ) CALL MPI_COMM_SIZE( MPI_COMM_WORLD,NPES,IERR )
!   IF( IFLAG ) CALL MPI_COMM_SIZE( PETSC_COMM_WORLD,NPES,IERR )
!--- END E4D PATCH
!
!---  Load and assemble problem vector ---
!
  CALL LOAD_PETSC_VECTOR( N_COL,VALUES,NUK )
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
  CALL DUMP_PETSC_VECTOR( X_ARRAY(I_X+1),IVL,NUK )
  CALL VecRestoreArray( petsc_X,X_ARRAY,I_X,IERR )
  DEALLOCATE( N_COL )
  DEALLOCATE( VALUES ) 
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

!SUBROUTINE init_sparse(ixp,nnz_d,nnz_o,id_l2g,num_nodes,num_loc_nodes,lsize,nuk,d_nnz,o_nnz)
SUBROUTINE init_sparse(lsize,nuk,d_nnz,o_nnz)
  USE COUP_WELL
  use grid
  use grid_mod
  use soltn
!
  implicit none
!
!  integer :: n, nr, num_nodes, nuk, m, lsize, nx,num_loc_nodes
!  integer, dimension(lsize) :: d_nnz, o_nnz 
!  integer, dimension(num_nodes) :: ixp
!  integer, dimension(num_loc_nodes) :: nnz_d, nnz_o,id_l2g
  integer :: isvc,n, nr,  nuk, m, lsize, nx
  integer, dimension(lsize) :: d_nnz, o_nnz
  integer, dimension(num_loc_nodes) :: nnz_d, nnz_o
  integer :: nlwx,ngwx
  integer :: n_cwx,iwnx,g_n_cw,iwt_cwnx,iwfx,gtiwx,ltiwx,nfx
  integer :: id_dng,id_upg,id_dn,id_up
!
  nr = 0
  d_nnz = 0
  o_nnz = 0
!  do n = 1,num_loc_nodes
!    nx = id_l2g(n)
!!    nx = n
!    if(ixp(nx) <= 0) cycle
!    do m=1,nuk
!      nr = nr+1  
!      d_nnz(nr) = nnz_d(n)
!      o_nnz(nr) = nnz_o(n)
!    enddo
!  enddo
  ! ***********Coupled well - Bryan **************
  isvc = nuk
!    if(nuk == 6 ) isvc = 1
    nnz_d = isvc
    nnz_o = 0
    DO n=1,num_cnx
      id_dng = conn_dn(n)
      id_upg = conn_up(n)
      id_dn = id_g2l(id_dng)
      id_up = id_g2l(id_upg)
      if( id_dn.lt.0 .and. ixp(id_dng).gt.0 ) then
        nnz_o(id_up) = nnz_o(id_up) + isvc
      elseif( id_up.lt.0 .and. ixp(id_upg).gt.0 ) then
        nnz_o(id_dn) = nnz_o(id_dn) + isvc
      else
        if(ixp(id_upg).gt.0) nnz_d(id_up) = nnz_d(id_up) + isvc
        if(ixp(id_dng).gt.0) nnz_d(id_dn) = nnz_d(id_dn) + isvc
      endif
    END DO
    if(n_cw > 0 ) then
     do n_cwx = 1,n_l_cw
         iwnx = id_cw(3,n_cwx)
         nx = iwn_cw(iwnx)
         n = id_g2l(nx)
         g_n_cw = id_cw(7,n_cwx)
         iwt_cwnx = iwt_cw(nx)
         if(iwt_cwnx > 0) then
!           nnz_o(n) = nnz_o(n)+1
!         else
!           nnz_d(n) = nnz_d(n)+1
           nnz_d_cw(g_n_cw) = nnz_d_cw(g_n_cw) + 1
!         else
!           nnz_o(n) = nnz_o(n)+1
        endif
         do iwfx = id_cw(5,n_cwx),id_cw(6,n_cwx)
           nnz_d_cw(g_n_cw) = nnz_d_cw(g_n_cw) + isvc
           nfx = id_g2l(iwf_cw(iwfx))
           if(iwt_cwnx > 0) then
             nnz_d(nfx) = nnz_d(nfx) + 1
           else
             nnz_o(nfx) = nnz_o(nfx) + 1
           endif
         enddo
         gtiwx = g_iwf_cw(g_n_cw)
         ltiwx = id_cw(6,n_cwx)-id_cw(5,n_cwx)+1
         if(gtiwx > ltiwx) then
           nnz_o_cw(g_n_cw) = nnz_o_cw(g_n_cw) + (gtiwx-ltiwx)*isvc
         endif
       enddo
    endif

      if(n_cw <= 0) then
   do n = 1,num_loc_nodes
    nx = id_l2g(n)
    if(ixp(nx) <= 0) cycle
    do m=1,isvc
      nr = nr+1
      d_nnz(nr) = nnz_d(n)
      o_nnz(nr) = nnz_o(n)
    enddo
   enddo
  else
   do n = 1,num_loc_nodes
    nx = id_l2g(n)
    if(ixp(nx) <= 0) cycle
    nlwx = iwt_cw(nx)
    if(nlwx > 0) then
      nlwx = w_loc(nlwx)
      nr=nr+1
      ngwx = id_cw(7,nlwx)
      d_nnz(nr) = nnz_d_cw(ngwx)
      o_nnz(nr) = nnz_o_cw(ngwx)
    endif
    do m=1,isvc
      nr = nr+1
      d_nnz(nr) = nnz_d(n)
      o_nnz(nr) = nnz_o(n)
    enddo
   enddo
  endif
!***************************************************
!
END SUBROUTINE init_sparse

subroutine load_petsc_vector(ir,value,isvcx)
 use grid
 use grid_mod
 use jacob
use soltn
USE COUP_WELL
 implicit none
 integer, dimension(lsize) :: ir
 double precision, dimension(lsize) :: value
! integer :: n, nx, nr, nuk,m
 integer :: n, nx, nr, nuk,m,isvcx
 integer :: ilwx,nwx,gnwx,n_locx,incx
 nuk = isvcx
 nr = 0
! do n=1,num_loc_nodes
!   nx = id_l2g(n)
!   if( ixp(nx) <= 0 ) cycle
!   do m=1,nuk
!     nr = nr + 1
!     ir(nr) = (imxp(nx)-1)*nuk+m-1
!     value(nr) = -1.d0*residual(m,nx)
!!if(nstep.eq.26)print *,'nx-------------------load',value(nr),loc2nat(nx),niter
!   enddo
! enddo
!***************Coupled well - Bryan*********************
if(n_cw <= 0) then
  do n=1,num_loc_nodes
   nx = id_l2g(n)
   if( ixp(nx) <= 0 ) cycle
   do m=1,nuk
     nr = nr + 1
     ir(nr) = (imxp_ncw(nx)-1)*nuk+m-1
     value(nr) = -1.d0*residual(m,nx)
   enddo
  enddo
 else
  do n=1,num_loc_nodes
   nx = id_l2g(n)
   if( ixp(nx) <= 0 ) cycle
    ilwx = iwt_cw(nx)
    if(ilwx > 0) then
      nwx = w_loc(ilwx)
      nr = nr+1
      ir(nr) = mmap_cw(id_cw(7,nwx))
      value(nr) = rs_cw(1,nwx)
    endif
    incx = (imxp(nx)-lstart)-n
    do m=1,nuk
      nr = nr+1
      ir(nr) = imxp(nx)-nuk+m-1
      value(nr) = -1.d0*residual(m,nx)
    enddo
  enddo
 endif
!***********************************************************
end subroutine load_petsc_vector

subroutine dump_petsc_vector(vec,ivl,isvcx)
 use grid
 use grid_mod
 use jacob
 USE COUP_WELL
 implicit none
 double precision, dimension(lsize) :: vec
 integer :: n, nx, nr, nuk,m, ivl
 integer :: ilwx,nwx,gnwx,isvcx
 nuk = isvcx
 nr = 0
! do n=1,num_loc_nodes
!   nx = id_l2g(n)
!   if( ixp(nx) <= 0 ) cycle
!   do m=1,nuk
!     nr = nr + 1
!     blu(m,nx) = vec(nr)
!   enddo
! enddo

!*************Coupled well - Bryan*************
 if(n_cw <= 0) then
  do n=1,num_loc_nodes
   nx = id_l2g(n)
   if( ixp(nx) <= 0 ) cycle
   do m=1,nuk
     nr = nr + 1
     blu(m,nx) = vec(nr)
   enddo
  enddo
 else
  do n=1,num_loc_nodes
   nx = id_l2g(n)
   if( ixp(nx) <= 0 ) cycle
   ilwx = iwt_cw(nx)
   if(ilwx > 0) then
     nwx = w_loc(ilwx)
     nr = nr+1
     blu_cw(nwx) = vec(nr)
   endif
   do m=1,nuk
     nr = nr + 1
     blu(m,nx) = vec(nr)
   enddo
  enddo
 endif
!***********************************************
end subroutine dump_petsc_vector
