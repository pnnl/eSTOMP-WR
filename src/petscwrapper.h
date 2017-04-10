!< Wrapper for PETSC includes to make it easy to change and 
!! ifdef
!! Exact rules are platform dependent 
!!>
#include "include/finclude/petsc.h"
#ifdef PETSCDUP
#include "include/finclude/petscvec.h"
#include "include/finclude/petscvec.h90"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscmat.h90"
#include "include/finclude/petscpc.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscsys.h"
#include "include/finclude/petscviewer.h"
#endif
