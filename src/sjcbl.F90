!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SJCBL( NSL )
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
!     Loads the matrix elements and solution vector for the
!     aqueous-phase convective-dispersive mass transport equation.
!
!     The Jacobian matrix is initially configured assuming zero-flux
!     boundary conditions.  The matrix is then updated for other
!     user-specified boundary conditions.
!
!     Matrix elements are stored in the array ALU.
!     Elements for the right-hand-side are stored in the array BLU.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.
!     $Id: sjcbl.F90,v 1.3 2009/04/28 20:39:17 d3m045 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      use bcvp
      use grid_mod
      use petscapp
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
      double precision, dimension(3) :: s_fx_up
      double precision, dimension(3) :: s_fx_dn
      double precision, dimension(3) :: s_area_x 
      logical :: isjcb
      LOGICAL :: use_ga
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
!--Petsc parameters
      PetscInt :: ir(2),ic(2),nr,nc
      PetscScalar :: values_(4)
      PetscErrorCode :: ierr
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/SJCBL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(196)(1:1),'$').EQ.0 ) CVS_ID(196) = &
       '$Id: sjcbl.F90,v 1.3 2009/04/28 20:39:17 d3m045 Exp $' 
      ICSN = ICSN+ICSNX
!
!--- Zero matrix elements
!
!  residual(:,:) = 0.d0
!  call MatZeroEntries(petsc_A,ierr)
  do nx = 1,num_loc_nodes
    n = id_l2g(nx) 
    if( ixp(n) <= 0 ) cycle
    sc = vol(n)*dti
    nc = 1
    nr = 1
    icol = loc_map(n)-1
    ic(1) = icol
    ir(1) = icol
    values_(1) = sc
    call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
!       right hand side
    if(nsl.gt.nsolu) then
      residual(1,n) = residual(1,n) + co(nsl,n)*sc
    else
      residual(1,n) = residual(1,n) + co(nsl,n)*sc &
             - 6.931d-1*co(nsl,n)*vol(n)/hlf(nsl)
!      residual(1,n) = residual(1,n) + co(nsl,n)* &
!        (2.D+0**(-dt/hlf(nsl)))*sc
    endif
!
!---        Daughter 1 chain decay  ---
!
    if( nsl.le.nsolu ) then
      do npsl = 1,nsolu
        if( npsl.eq.nsl ) cycle
        residual(1,n) = residual(1,n) + &
             chdf(npsl,nsl)*6.931d-1*co(npsl,n)*vol(n)/hlf(npsl)
      enddo
    endif
!
!---        Leonard-TVD, Roe's Superbee, or 1st-order upwind  ---
!
!       IF( ISLC(1).GE.1 ) residual(1,n) = residual(1,n) + CC
  enddo
!
  isjcb = .true.
  do iconn = 1,num_cnx
    icycle = 0
    id_up = conn_up(iconn)
    id_dn = conn_dn(iconn)
!   c_flux(nsl,iconn) = 0.d0
    if (ixp(id_dn) == 0 .or. ixp(id_up) == 0) cycle
    CALL CFLUX1 (nsl,iconn,id_up,id_dn,icycle, &
      fcl_up,fcl_dn,flb,dlz,ucx,isjcb)
    IF( ICYCLE.EQ.1 )CYCLE
!
!--- TVD solute transport  ---
!
    if( islc(1).ge.1 ) then
      if( id_g2l(id_dn) > 0 ) then
        residual(1,id_dn) = residual(1,id_dn) - ucx
      endif
      if( id_g2l(id_up) > 0 ) then
        residual(1,id_up) = residual(1,id_up) + ucx
      endif
      ap_up = dlz*fcl_up !nondiagonal of id_dn
      ap_dn = dlz*fcl_dn !diagonal of id_dn
      irowdx = loc_map(id_dn)-1
      irowux = loc_map(id_up)-1
      values_(1:4) = 0.d0
      nr = 2 
      nc = 2
      ir(1) = irowdx
      ir(2) = irowux
      ic(1) = irowdx
      ic(2) = irowux
      if( id_g2l(id_dn) < 0 ) then
        values_(1) = 0.d0
        values_(2) = 0.d0
      else
        values_(1) = ap_dn
        values_(2) = -1.d0*ap_up
      endif
      if( id_g2l(id_up) < 0 ) then
        values_(3) = 0.d0
        values_(4) = 0.d0
      else
        values_(3) = -1.d0*ap_dn
        values_(4) = ap_up
      endif
      call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
    else
!
!--- Patankar solute transport  ---
!
      alux = max(-flb,zero) !up as east
      aldx = max(flb,zero) !dn as node
      aldf = dlz*max((one-(tenth*abs(flb)/(dlz+small)))**5,zero)
      ap_up = (aldx+aldf-flb)*fcl_up !diagonal of id_up row
      ap_dn = (alux+aldf+flb)*fcl_dn !diagonal of id_dn row !ae
      ab_dn = (alux+aldf)*fcl_up !non diagonal of id_dn row
      ab_up = (aldx+aldf)*fcl_dn !non diagonal of id_up row
      irowdx = loc_map(id_dn)-1
      irowux = loc_map(id_up)-1
      values_(1:4) = 0.d0
      nr = 2 
      nc = 2
      ir(1) = irowdx
      ir(2) = irowux
      ic(1) = irowdx
      ic(2) = irowux
      if( id_g2l(id_dn) < 0 ) then
        values_(1) = 0.d0
        values_(2) = 0.d0
      else
        values_(1) = ap_dn
        values_(2) = -1.d0*ab_dn
      endif
      if( id_g2l(id_up) < 0 ) then
        values_(3) = 0.d0
        values_(4) = 0.d0
      else
        values_(3) = -1.d0*ab_up
        values_(4) = ap_up
      endif
      call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
    endif
  enddo
!
!---  End of SJCBL group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

