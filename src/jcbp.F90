SUBROUTINE JCBP
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
!     Configure the Jacobian matrix pointer arrays.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.
!     Last Modified by MD White on June 15, 2000.
!     $Id: jcbp.F90,v 1.7 2009/04/28 20:39:17 d3m045 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE JACOB
      USE GRID
      use grid_mod
      USE TRNSPT
      USE COUP_WELL
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
!--- Petsc
!#include "petscwrapper.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscpc.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscsys.h"
#include "include/finclude/petscviewer.h"
!
!----------------------Parameter Statements----------------------------!
!
  integer ix_offset, iy_offset, iz_offset, i_offset, one
  integer i, lo_put(5), hi_put(5), lo_get(5), hi_get(5), ldxx(5)
  integer idx,slen,nlen
  logical t_ok
  character (len=64) :: t_string
  LOGICAL :: use_ga
  integer :: lw_size                       ! coupled well -BH
  integer :: gidx ,gtiwx, g_n_cw,gwidx     ! coupled well -BH
  integer :: lstart_ncw                    ! for 2 matrix -BH
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/JCBP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(95)(1:1),'$').EQ.0 ) CVS_ID(95) = &
      '$Id: jcbp.F90,v 1.7 2009/04/28 20:39:17 d3m045 Exp $' 
      ICSN = ICSN+ICSNX

!**************************coupled well - Bryan ***********************
   inax = 0
  do n=1,num_nodes
    if(ixp(n) <= 0) cycle
    inax = inax+1
    ixp(n) = inax
  enddo
  if(l_cw > 0) then
    allocate(g_iwf_cw(n_cw))
    allocate(nnz_o_cw(n_cw))
    allocate(nnz_d_cw(n_cw))
    g_iwf_cw(:) = 0
    nnz_o_cw(:) = 0
    nnz_d_cw(:) = 0
    lw_size = 0
    do n_cwx = 1,n_l_cw
      iwnx = id_cw(3,n_cwx)
      gidx = iwi_cw(iwnx)
! injection node, withdrawal later...
      if(gidx == 1) then
        lw_size = lw_size+1
      endif
! global well number
      gwidx = id_cw(7,n_cwx)
      itfwx = id_cw(6,n_cwx)-id_cw(5,n_cwx)+1
      g_iwf_cw(gwidx) = g_iwf_cw(gwidx)+itfwx
    enddo
    call ga_igop(1,g_iwf_cw,n_cw,'+')
  endif
!*************************************************************************

!
!---  Determine the number of local and global rows in matrix
!     and the local starting point  ---
!
  lsize = 0
!
  DO n=1,num_loc_nodes
    nx = id_l2g(n)
    IF( ixp(nx) <= 0 ) CYCLE
    lsize = lsize + isvc
  END DO

  ! well node -*************** coupled well-Bryan**********************
  lgsize = lsize
    if(l_cw == 1) then
      lsize = lsize+lw_size
      CALL mpi_scan(lw_size,lwstart,1,MPI_INTEGER,MPI_SUM,PETSC_COMM_WORLD,ierr)
      lwstart = lwstart-lw_size
    endif
  !**********************************************************************
  gsize = lsize
  lstart = lsize
  call ga_igop(1,gsize,1,'+')
!  CALL mpi_scan( lsize,lstart,1,MPI_INTEGER,MPI_SUM,PETSC_COMM_WORLD,ierr )
  CALL mpi_scan( lgsize,lstart,1,MPI_INTEGER,MPI_SUM,PETSC_COMM_WORLD,ierr ) !copied from estomp33-Bryan
!  lstart = (lstart-lsize)/isvc
!  llsize = 0
  lstart_ncw = (lstart-lgsize)/isvc

  !*************************coupled well -Bryan**************************
  if(l_cw == 1) then
      lstart = lstart-lgsize + lwstart
      wlstart = lstart
    else
      lstart = (lstart-lgsize)/isvc
    endif
    llsize = 0
  !**********************************************************************
!
  DO n=1,num_nodes
    IF( ixp(n) <= 0 ) CYCLE
    llsize = llsize + isvc
  END DO
  !*************************coupled well -Bryan**************************
  if(l_cw == 1) then
     llsize = llsize+lw_size
  endif
!  write(*,*) 'llsize,lwsize:',llsize,lw_size
  !**********************************************************************
!
!---  Define matrix pointer array  ---
!
  nc = 0
  t_ok = .false.
!
  t_string = 'imxp'
!  slen = grid_clen(t_string)
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.inode_field.and.(.not.t_ok))
    i = i + 1
!    nlen = grid_clen(i_nd_fld_names(i))
    nlen = len_trim(i_nd_fld_names(i))
    if (t_string(1:slen).eq.i_nd_fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  enddo
!  DO n = 1,num_loc_nodes
!    nx = id_l2g(n)
!    IF( ixp(nx) <= 0 ) CYCLE
!    nc = nc + 1
!    i_nd_fld(idx)%p(nx) = nc + lstart
!  END DO
!

  !*********** Coupled well - Bryan *********************************
  if(l_cw == 1) then
     allocate(w_loc(n_l_cw))
     w_loc(:) = 0
     nwx_ = 0
     DO n = 1,num_loc_nodes
      nx = id_l2g(n)
      IF( ixp(nx) <= 0 ) CYCLE
      nc = nc + luk
      if(iwt_cw(nx) == 1) then
        nc = nc+1
        nwx_ = nwx_+1
        iwt_cw(nx) = nwx_
        loop_well:do n_cwx = 1,n_l_cw
          do nwfx_ = id_cw(5,n_cwx),id_cw(6,n_cwx)
            nwfx = iwf_cw(nwfx_)
            if(nwfx == nx) then
              iwnx = id_cw(3,n_cwx)
              gidx = iwi_cw(iwnx)
              if(gidx == 1) then
                w_loc(nwx_) = n_cwx
                exit loop_well
              endif
            endif
          enddo
        enddo loop_well
      endif
      i_nd_fld(idx)%p(nx) = nc + lstart
     END DO
!     write(*,*)'w_loc:',w_loc(:)
    else
     DO n = 1,num_loc_nodes
      nx = id_l2g(n)
      IF( ixp(nx) <= 0 ) CYCLE
      nc = nc + 1
      i_nd_fld(idx)%p(nx) = nc + lstart
     END DO
    endif
!*********************************************************************

!  find location of first element in local array
!
  if (ixmin.eq.1) then
    ix_offset = 0
  else
    ix_offset = gwidth
  endif
  if (iymin.eq.1) then
    iy_offset = 0
  else
    iy_offset = gwidth
  endif
  if (izmin.eq.1) then
    iz_offset = 0
  else
    iz_offset = gwidth
  endif
  one = 1
  i_offset = ix_offset + iy_offset*ldx + iz_offset*ldx*ldy + 1
!
  lo_put(1) = ixmin
  lo_put(2) = iymin
  lo_put(3) = izmin
  hi_put(1) = ixmax
  hi_put(2) = iymax
  hi_put(3) = izmax
!
  ldxx(1) = ldx
  ldxx(2) = ldy
  ldxx(3) = ldz
  if (ixmin.eq.1) then
    lo_get(1) = ixmin
  else
    lo_get(1) = ixmin - gwidth
  endif
  if (iymin.eq.1) then
    lo_get(2) = iymin
  else
    lo_get(2) = iymin - gwidth
  endif
  if (izmin.eq.1) then
    lo_get(3) = izmin
  else
    lo_get(3) = izmin - gwidth
  endif
!
  if (ixmax.eq.nxdim) then
    hi_get(1) = ixmax
  else
    hi_get(1) = ixmax + gwidth
  endif
  if (iymax.eq.nydim) then
    hi_get(2) = iymax
  else
    hi_get(2) = iymax + gwidth
  endif
  if (izmax.eq.nzdim) then
    hi_get(3) = izmax
  else
    hi_get(3) = izmax + gwidth
  endif
  call nga_put(ga_int,lo_put,hi_put,i_nd_fld(idx)%p(i_offset),ldxx)
  call ga_sync
  call nga_get(ga_int,lo_get,hi_get,i_nd_fld(idx)%p(one),ldxx)
  call ga_sync
  imxp => i_nd_fld(idx)%p

  ! Mapping without coupled well
    call add_node_ifield('imxp_ncw', idx)
    imxp_ncw => i_nd_fld(idx)%p
    imxp_ncw = 0
    nc = 0
    DO n = 1,num_loc_nodes
      nx = id_l2g(n)
      IF( ixp(nx) <= 0 ) CYCLE
      nc = nc + 1
      i_nd_fld(idx)%p(nx) = nc + lstart_ncw
    END DO
    call nga_put(ga_int,lo_put,hi_put,i_nd_fld(idx)%p(i_offset),ldxx)
    call ga_sync
    call nga_get(ga_int,lo_get,hi_get,i_nd_fld(idx)%p(one),ldxx)
    call ga_sync
    imxp_ncw => i_nd_fld(idx)%p
!
!
!---  Determine number of nonzeros in the local DIAGONAL and OFF-DIAGONAL
!     portions of the matrix   ---
!
!  nnz_d = isvc
!  nnz_o = 0
!  DO n=1,num_cnx
!    id_dng = conn_dn(n)
!    id_upg = conn_up(n)
!    id_dn = id_g2l(id_dng)
!    id_up = id_g2l(id_upg)
!    if(ixp(id_upg) <= 0 .or. ixp(id_dng) <= 0) cycle
!    if( id_dn.lt.0) then
!      nnz_o(id_up) = nnz_o(id_up) + isvc
!    elseif( id_up.lt.0 ) then
!      nnz_o(id_dn) = nnz_o(id_dn) + isvc
!    else
!      nnz_d(id_up) = nnz_d(id_up) + isvc
!      nnz_d(id_dn) = nnz_d(id_dn) + isvc
!    endif
!  END DO
!--- local equation mapping
  allocate(loc_map(num_nodes))
  loc_map = 0
  allocate(gloc_map(num_nodes))
  gloc_map = 0
  nrx = 0

!************** Coupled well - Bryan*********************************
   if(l_cw <= 0) then
     do n=1,num_nodes
      if(ixp(n) <= 0) cycle
      nrx = nrx+1
      loc_map(n) = nrx
     enddo
    else
     do n=1,num_nodes
      if(ixp(n) <= 0) cycle
      if(iwt_cw(n) > 0) then
       nrx = nrx+1
      endif
      nrx = nrx+1
      loc_map(n) = nrx
     enddo
    endif

    if(nsolu > 0 .or. ISLC(40).eq.1) then
     nrx = 0
     do n=1,num_nodes
      if(ixp(n) <= 0) cycle
      nrx = nrx+1
      gloc_map(n) = nrx
     enddo
    endif

    lstart_cw = lstart  
!  do n=1,num_nodes
!    if(ixp(n) <= 0) cycle
!    nrx = nrx+1
!    loc_map(n) = nrx
!  enddo
!call ga_sync
!
!---  Reset subroutine character string ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of JCBP group  ---
!
      RETURN
      END
