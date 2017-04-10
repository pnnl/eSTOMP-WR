subroutine update_nodes(t_string,i_dimx,iflg,dflg)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.update_nodes.use)
  use grid_mod
  implicit none

#include "petscwrapper.h"
#include "mafdecls.fh"
#include "global.fh"
  integer ix_offset, iy_offset, iz_offset, i_offset, one
  integer i, lo_put(5), hi_put(5), lo_get(5), hi_get(5), ld(5)
  integer i_dimx,iflg,dflg,idx
  double precision :: t_b,t_e
  character (len=*) :: t_string
  logical :: t_ok
  integer :: me,nproc
  double precision :: t_bs1,t_es1
  logical :: status
!
!  find location of first element in local array
!
      ME = GA_NODEID()
NPROC = GA_NNODES()
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
  ld(1) = ldx
  ld(2) = ldy
  ld(3) = ldz
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
! 
!   Cycle through all fields associated with nodes
!
  t_b = MPI_Wtime()
  t_bs1 = MPI_Wtime()
  call ga_sync
  t_es1 = MPI_Wtime()
  call string2idx(i_dimx,iflg,dflg,t_string,idx,t_ok)
  if(i_dimx.eq.1 .and. dflg.eq.1) then
    i = idx
    call nga_put(ga_dbl,lo_put,hi_put,d_nd_fld(i)%p(i_offset),ld)
#ifdef USE_GHOST_UPDATE
    call ga_sync
    status = ga_update5_ghosts(ga_dbl)
    call ga_sync
    call nga_get_ghost_block(ga_dbl,lo_get,hi_get,d_nd_fld(i)%p(one),ld)
    call ga_sync
#else
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
    sync_iter = sync_iter+1
    call nga_get(ga_dbl,lo_get,hi_get,d_nd_fld(i)%p(one),ld)
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
#endif
    sync_iter = sync_iter+1
   endif
   if(i_dimx.eq.1.and.iflg.eq.1) then
    i = idx
    call nga_put(ga_int,lo_put,hi_put,i_nd_fld(i)%p(i_offset),ld)
    t_bs1 = MPI_Wtime()
#ifdef USE_GHOST_UPDATE
!      call ga_update_ghosts(ga_int)
    call ga_sync
    status = ga_update5_ghosts(ga_int)
    call ga_sync
    call nga_get_ghost_block(ga_int,lo_get,hi_get,i_nd_fld(i)%p(one),ld)
    call ga_sync
#else
    call ga_sync
    t_es1 = MPI_Wtime()
    sync_iter = sync_iter+1
    call nga_get(ga_int,lo_get,hi_get,i_nd_fld(i)%p(one),ld)
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
#endif
    sync_iter = sync_iter+1
   endif
!
!   Cycle through all 2D fields associated with nodes
!
  do i = 3, 1, -1
    lo_put(i+1) = lo_put(i)
    hi_put(i+1) = hi_put(i)
    lo_get(i+1) = lo_get(i)
    hi_get(i+1) = hi_get(i)
    ld(i+1) = ld(i)
  end do
  if(i_dimx.eq.2.and.dflg.eq.1) then
    i = idx
    lo_put(1) = 1
    hi_put(1) = d_nd_2dim1(i)
    lo_get(1) = 1
    hi_get(1) = d_nd_2dim1(i)
    ld(1) = d_nd_2dim1(i)
    call nga_put(ga_dbl2,lo_put,hi_put,d_nd_2fld(i)%p(one,i_offset),ld)
#ifdef USE_GHOST_UPDATE
    call ga_sync
    status = ga_update5_ghosts(ga_dbl2)
    call ga_sync
      call nga_get_ghost_block(ga_dbl2,lo_get,hi_get,d_nd_2fld(i)%p(one,one),ld)
      call ga_sync
#else
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
    sync_iter = sync_iter+1
    call nga_get(ga_dbl2,lo_get,hi_get,d_nd_2fld(i)%p(one,one),ld)
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
#endif
!  sync_time(me+1) = sync_time(me+1)+t_es1-t_bs1
  sync_iter = sync_iter+1
   endif
   if( i_dimx.eq.2.and.iflg.eq.1) then
    i = idx
    lo_put(1) = 1
    hi_put(1) = i_nd_2dim1(i)
    lo_get(1) = 1
    hi_get(1) = i_nd_2dim1(i)
    ld(1) = i_nd_2dim1(i)
    call nga_put(ga_int2,lo_put,hi_put,i_nd_2fld(i)%p(one,i_offset),ld)
#ifdef USE_GHOST_UPDATE
    call ga_sync
    call ga_sync
    call nga_get_ghost_block(ga_int2,lo_get,hi_get,i_nd_2fld(i)%p(one,one),ld)
    call ga_sync
#else
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
    sync_iter = sync_iter+1
    call nga_get(ga_int2,lo_get,hi_get,i_nd_2fld(i)%p(one,one),ld)
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
#endif
    sync_iter = sync_iter+1
   endif
!
!   Cycle through all 3D fields associated with nodes
!
  do i = 4, 2, -1
    lo_put(i+1) = lo_put(i)
    hi_put(i+1) = hi_put(i)
    lo_get(i+1) = lo_get(i)
    hi_get(i+1) = hi_get(i)
    ld(i+1) = ld(i)
  end do
   if(i_dimx.eq.3.and.dflg.eq.1) then
    i = idx
    lo_put(1) = 1
    hi_put(1) = d_nd_3dim1(i)
    lo_get(1) = 1
    hi_get(1) = d_nd_3dim1(i)
    lo_put(2) = 1
    hi_put(2) = d_nd_3dim2(i)
    lo_get(2) = 1
    hi_get(2) = d_nd_3dim2(i)
    ld(1) = d_nd_3dim1(i)
    ld(2) = d_nd_3dim2(i)
    call nga_put(ga_dbl3,lo_put,hi_put,d_nd_3fld(i)%p(one,one,i_offset),ld)
#ifdef USE_GHOST_UPDATE
    call ga_sync
    status = ga_update5_ghosts(ga_dbl3)
    call ga_sync
      call nga_get_ghost_block(ga_dbl3,lo_get,hi_get,d_nd_3fld(i)%p(one,one,one),ld)
      call ga_sync
#else
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
    sync_iter = sync_iter+1
    call nga_get(ga_dbl3,lo_get,hi_get,d_nd_3fld(i)%p(one,one,one),ld)
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
  sync_iter = sync_iter+1
#endif
!  sync_time(me+1) = sync_time(me+1)+t_es1-t_bs1
!print *,'here 3d',dnode_3field
  endif
!  end do
!print *,'done d3node'
!  do i = 1, inode_3field
   if(i_dimx.eq.3.and.iflg.eq.1) then
    i = idx
    lo_put(1) = 1
    hi_put(1) = i_nd_3dim1(i)
    lo_get(1) = 1
    hi_get(1) = i_nd_3dim1(i)
    lo_put(2) = 1
    hi_put(2) = i_nd_3dim2(i)
    lo_get(2) = 1
    hi_get(2) = i_nd_3dim2(i)
    ld(1) = i_nd_3dim1(i)
    ld(2) = i_nd_3dim2(i)
    call nga_put(ga_int3,lo_put,hi_put,i_nd_3fld(i)%p(one,one,i_offset),ld)
#ifdef USE_GHOST_UPDATE
    call ga_sync
      status = ga_update5_ghosts(ga_int3)
    call ga_sync
      call nga_get_ghost_block(ga_int3,lo_get,hi_get,i_nd_3fld(i)%p(one,one,one),ld)
      call ga_sync
#else
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
    sync_iter = sync_iter+1
    call nga_get(ga_int3,lo_get,hi_get,i_nd_3fld(i)%p(one,one,one),ld)
    t_bs1 = MPI_Wtime()
    call ga_sync
    t_es1 = MPI_Wtime()
#endif
  sync_iter = sync_iter+1
   endif
  t_e = MPI_Wtime()
  ga_time = ga_time + t_e-t_b
  return
end subroutine update_nodes
