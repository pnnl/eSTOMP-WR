!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE JCBWL(petsc_A)
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
!     Load the Jacobian matrix for the water equation with
!     aqueous-phase contributions
!     (zero flux boundary conditions).
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1993.
!     Last Modified by MD White, PNNL, October 19, 1999.
!     $Id: jcbwl.F,v 1.7 2006/03/30 21:12:21 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOURC
      USE SOLTN
      USE POINTE
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVSO
      USE FDVP
      use grid_mod
      use petscapp
      USE COUP_WELL
      use plt_atm
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      REAL*8 STWX(LUK+1),RWP(LUK),RWA(LUK,6),FW(LSFV,6)
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
!
!----------------------Common Blocks-----------------------------------!
!

      PetscInt :: ic(2),ir(2),nr,nc 
      PetscScalar :: values_(4)
      PetscErrorCode :: ierr
      Mat :: jac
  PetscViewer      PV_VIEWER,M_VIEWER,SV_VIEWER,KSP_VIEWER 


!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/JCBWL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(112)(1:1),'$').EQ.0 ) CVS_ID(112) = &
      '$Id: jcbwl.F,v 1.7 2006/03/30 21:12:21 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over all nodes, skipping inactive nodes  ---
!
!  if (.not.associated(accum_res)) allocate(accum_res(ISVC+1,num_nodes))
!  if (.not.associated(flux_res)) allocate(flux_res(ISVF,num_cnx))
!  if (.not.associated(accum_deriv)) allocate(accum_deriv(isvc+1,num_nodes))
!  if (.not.associated(flux_deriv)) allocate(flux_deriv(2,num_cnx))
!  if (.not.associated(residual)) allocate(residual(num_nodes))
  accum_res(:,:) = 0.d0
  accum_deriv(:,:) = 0.d0
  flux_res(:,:) = 0.d0
  flux_deriv(:,:) = 0.d0
  residual(:,:) = 0.d0
  blu(:,:) = 0.d0
!
  call MatZeroEntries(petsc_A,ierr)
!
!---  Water mass: Storage terms  ---
!
  do nx = 1,num_loc_nodes
        n = id_l2g(nx) 
        if( ixp(n) <= 0 ) cycle
        stw1 = pord(1,n)*xlw(1,n)*rhol(1,n)*sl(1,n)
! m = 1 present time for node itself, 2 present itself with increment
        do m = 1,isvc+1
          mp = m + 1
          stw0 = pord(mp,n)*xlw(mp,n)*rhol(mp,n)*sl(mp,n)
          stwx(m) = (stw0-stw1)*dti*vol(n)
!if(n.eq.1.and.m.eq.2) print *,'stwx',stw0,stw1,pord(1:3,n)
        enddo
        do m = 1,isvc
          accum_res(m,n) = stwx(m+1) - srcw(m+2,n) - stwx(1) + srcw(2,n)
          if(lplant == 1) then
            accum_res(m,n) = accum_res(m,n) - veg_sink(m+2,n) + veg_sink(2,n)
          endif
        END DO
!       right hand side
        residual(ieqw,n) = stwx(1) - srcw(2,n)
        if(lplant == 1) then
          residual(ieqw,n) = residual(ieqw,n) - veg_sink(2,n)
        endif
        nc = isvc
        nr = isvc
!**********************coupled well - Bryan*************************
        if(l_cw == 1) then
          nax = ixp(n)
          n_locx = nax-1
          incx = loc_map(n)-nax
        else
          n_locx = loc_map(n)-1
          incx = 0
        endif
        ir(1) = incx+n_locx*luk+ieqw-1
        do m=1,isvc
          icol = incx+n_locx*luk+m-1
!          icol = (loc_map(n)-1)*luk+m-1
!!          icol = (n-1)*luk+m-1
          ic(m) = icol
!          ir(m) = icol     ! should it comment out?-Since m=ieqw=1, it doesn't
!          matter.
!*******************************************************************        
          accum_deriv(m,n) = accum_res(m,n)/dnr(m,n)
        enddo
        values_(1:isvc) = accum_deriv(1:isvc,n)
        call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
  enddo
  
!
!---  Water mass: Interior Connections  ---
!
  do iconn=1,num_cnx
    id_up = conn_up(iconn)
    id_dn = conn_dn(iconn)
    do m = 1,isvf
      mb = madj(m)
      mp = mnod(m)
!      flwb = xlw(mp,id_up)*rhol(mp,id_up)
!      flwp = xlw(mb,id_dn)*rhol(mb,id_dn)	        
      flwb = xlw(mb,id_up)*rhol(mb,id_up)
      flwp = xlw(mp,id_dn)*rhol(mp,id_dn)	        
      flw = 0.d0
      indx = 2
      flw = difmn(flwp,flwb,dist_dn(iconn),dist_up(iconn),q_flux(1,iconn),indx)
      if (ixp(id_up) /= 0 .and. ixp(id_dn) /= 0) then
        fwx = areac(iconn)*q_flux(m,iconn)*flw
        flux_res(m,iconn) = fwx
        if( m.eq.1 ) then
         if( id_g2l(id_up) > 0 ) &
          residual(ieqw,id_up) = residual(ieqw,id_up)-fwx
         if( id_g2l(id_dn) > 0 ) &
          residual(ieqw,id_dn) = residual(ieqw,id_dn)+fwx
        endif
      endif
    enddo
  enddo

!to load into petsc
  nc = 2*isvc
!  nr = nr
  nr = 2  ! nr  one for field and one for coupled wells
  ic = 0
  ir = 0
  do i=1,num_cnx
    id_up = conn_up(i)
    id_dn = conn_dn(i)
    if(ixp(id_up) <= 0 .or. ixp(id_dn) <= 0) cycle
    ! ******** Couple well *****************
    isvc2 = 2*isvc
    m = ieqw
    if(l_cw == 1) then
      id_dn_ax = ixp(id_dn)
      id_up_ax = ixp(id_up)
      n_locx_dn = id_dn_ax-1
      incx_dn = loc_map(id_dn)-id_dn_ax
      n_locx_up = id_up_ax-1
      incx_up = loc_map(id_up)-id_up_ax
    else
      n_locx_dn = loc_map(id_dn)-1
      incx_dn = 0
      n_locx_up = loc_map(id_up)-1
      incx_up = 0
    endif
    irowdx = incx_dn+n_locx_dn*luk+m-1
    irowux = incx_up+n_locx_up*luk+m-1
    ir(1) = irowdx
    ir(2) = irowux
    !***********************************************
    do n=1,isvc
     irowdx = incx_dn+n_locx_dn*luk+n-1
     irowux = incx_up+n_locx_up*luk+n-1
     n1 = n
     n2 = isvc+n
     ic(n1) = irowdx
     ic(n2) = irowux
     flux_deriv(n1,id_dn) = (flux_res(2*n,i) - flux_res(1,i))/dnr(n,id_dn)
     flux_deriv(n2,id_dn) = (flux_res(2*n+1,i) - flux_res(1,i))/dnr(n,id_up)
     flux_deriv(n1,id_up) = -flux_deriv(n1,id_dn)
     flux_deriv(n2,id_up) = -flux_deriv(n2,id_dn)
!    do m=1,isvc
!     flux_deriv(1,id_dn) = (flux_res(2*m,i) - flux_res(1,i))/dnr(m,id_dn)
!     flux_deriv(2,id_dn) = (flux_res(2*m+1,i) - flux_res(1,i))/dnr(m,id_up)
!     flux_deriv(1,id_up) = -flux_deriv(1,id_dn)
!     flux_deriv(2,id_up) = -flux_deriv(2,id_dn)
!     nr = 2
!     nc = 2
!     irowdx = (loc_map(id_dn)-1)*luk+m-1
!     irowux = (loc_map(id_up)-1)*luk+m-1
!!print *,'n--map me',me,id_dn,loc_map(id_dn),id_up,loc_map(id_up)
!!     irowdx = ((id_dn)-1)*luk+m-1
!!     irowux = ((id_up)-1)*luk+m-1
     ir(1) = irowdx
     ir(2) = irowux
     ic(1) = irowdx
     ic(2) = irowux
     if( id_g2l(id_dn) < 0 ) then
      values_(1) = 0.d0
      values_(2) = 0.d0
     else
      values_(1) = flux_deriv(1,id_dn)
!      values_(2) = flux_deriv(1,id_up)
      values_(2) = flux_deriv(2,id_dn)
     endif
     if( id_g2l(id_up) < 0 ) then
      values_(3) = 0.d0
      values_(4) = 0.d0
     else 
!      values_(3) = flux_deriv(2,id_dn)
      values_(3) = flux_deriv(1,id_up)
      values_(4) = flux_deriv(2,id_up)
     endif
!if(id_up.eq.165166.or.id_dn.eq.165166)then
!  print *,'valuses----',me,id_dn,id_up,values_(1:4),flux_res(1,i)
!endif
    call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )     
    enddo
  enddo
1001 continue
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of JCBWL group
!
      RETURN
      END
