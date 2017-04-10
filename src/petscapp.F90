MODULE PETSCAPP
  INTEGER, DIMENSION(:), ALLOCATABLE :: D_NNZ,O_NNZ,N_COL,N_ROW
  INTEGER :: LOCAL_SIZE, GLOBAL_SIZE
!#define PETSC_AVOID_DECLARATIONS
#include "include/finclude/petscdef.h"
#include "include/finclude/petscvecdef.h"
!#include "include/finclude/petscvecdef.h90"
#include "include/finclude/petscmatdef.h"
!#include "include/finclude/petscmatdef.h90"
#include "include/finclude/petscpcdef.h"
#include "include/finclude/petsckspdef.h"
#include "include/finclude/petscsysdef.h"
#include "include/finclude/petscisdef.h"
  PetscScalar, DIMENSION(:), ALLOCATABLE :: values
  Vec     :: petsc_x, Petsc_b
  Mat     :: petsc_A  
  PC      :: petsc_pc
  KSP     :: petsc_ksp
  ISLocalToGlobalMapping :: mapping
!#undef  PETSC_AVOID_DECLARATIONS
END MODULE
