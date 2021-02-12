subroutine average_v
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
!-- get cell centered velocity
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle's Pacific Northwest Division, 1996.
!     Last Modified by MD White on September 5, 1996.
!     $Id: sjcbl.F,v 1.9 2006/01/09 19:42:40 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE GRID
      USE FLUXP
      USE CONST
      use bcvp
      use grid_mod
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
      double precision, dimension(3) :: s_fx_up
      double precision, dimension(3) :: s_area_x 
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
integer :: dims(4), lo(4), hi(4), ldxx(4)
double precision, dimension(:,:,:,:), allocatable :: buf4
double precision, dimension(:), allocatable :: val_buftf
double precision, dimension(:), allocatable :: val_bufts
integer, dimension(:,:), allocatable :: idx_buf2
integer :: g_buf
logical :: status
double precision :: alpha
integer :: dim1
LOGICAL :: use_ga
!
!----------------------Executable Lines--------------------------------!
!
  me = ga_nodeid()
  use_ga = .true.
  s_fx = 0.d0
  s_area = 0.d0
  do nx = 1,num_loc_nodes
    id_dn = id_l2g(nx)
      call accum_flux(s_fx_up,s_area_x,id_dn)
      s_fx(1:3,id_dn) = s_fx_up(1:3)
      s_area(1:3,id_dn) = s_area_x(1:3) 
  enddo
  icx = 0
  ldi = ldx
  ldij = ldi*ldy
  num_nodes3 = 3*num_nodes
  allocate(val_buftf(num_nodes3))
  allocate(val_bufts(num_nodes3))
  allocate(idx_buf2(4,num_nodes3))
  do izz = 1,ldz
  do iyy = 1,ldy
  do ixx = 1,ldx
   nx = ixx+ldi*(iyy-1)+ldij*(izz-1)
  do ix=1,3
    icx = icx+1
    idx_buf2(1,icx) = ixx+iaxmin-1
    idx_buf2(2,icx) = iyy+iaymin-1
    idx_buf2(3,icx) = izz+iazmin-1
    idx_buf2(4,icx) = ix
    val_buftf(icx) = s_fx(ix,nx) 
    val_bufts(icx) = s_area(ix,nx) 
  enddo
  enddo
  enddo
  enddo
  alpha = 1.d0
  dims(1) = nxdim
  dims(2) = nydim
  dims(3) = nzdim
  dims(4) = 3 
  ndimx = 4
  g_buf= ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(g_buf,GAGRP)
#endif
!
  call ga_set_data(g_buf, ndimx, dims, MT_F_DBL)
  status = ga_allocate(g_buf)
  call ga_zero(g_buf)
  call nga_scatter_acc(g_buf,val_buftf(1),idx_buf2(1,1),icx,alpha)
  call ga_sync
  deallocate(val_buftf)
  ldxx(1) = iaxmax - iaxmin + 1
  ldxx(2) = iaymax - iaymin + 1
  ldxx(3) = iazmax - iazmin + 1
  ldxx(4) = 3
  lo(1) = iaxmin
  lo(2) = iaymin
  lo(3) = iazmin
  lo(4) = 1
  hi(1) = iaxmax
  hi(2) = iaymax
  hi(3) = iazmax
  hi(4) = 3
  allocate(buf4(ldxx(1),ldxx(2),ldxx(3),ldxx(4)))
  buf4 = 0.d0
  call nga_get(g_buf,lo,hi,buf4(1,1,1,1),ldxx)
  do izz=1,ldxx(3)
  do iyy=1,ldxx(2)
  do ixx=1,ldxx(1)
   nx = ixx+ldi*(iyy-1)+ldij*(izz-1)
   s_fx(1:3,nx) = buf4(ixx,iyy,izz,1:3)
  enddo
  enddo
  enddo
  call ga_zero(g_buf)
  call nga_scatter_acc(g_buf,val_bufts(1),idx_buf2(1,1),icx,alpha)
  call ga_sync
  buf4 = 0.d0
  call nga_get(g_buf,lo,hi,buf4(1,1,1,1),ldxx)
  do izz=1,ldxx(3)
  do iyy=1,ldxx(2)
  do ixx=1,ldxx(1)
   nx = ixx+ldi*(iyy-1)+ldij*(izz-1)
   s_area(1:3,nx) = buf4(ixx,iyy,izz,1:3)
   vnc(1:3,nx) = s_fx(1:3,nx)/(s_area(1:3,nx)+1.d-60)
  enddo
  enddo
  enddo
  deallocate(val_bufts)
  deallocate(idx_buf2)
  deallocate(buf4)
  status = ga_destroy(g_buf)
return
end
!
subroutine accum_flux(s_fx_up,s_area_x,id_up)
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE GRID
      USE FLUXP
      USE CONST
      use bcvp
      use grid_mod
!
      double precision, dimension(3) :: s_fx_up
      double precision, dimension(3) :: s_area_x 
!
!---    Hydrodynamic dispersion coefficients at cell faces  ---
!
    s_fx_up = 0.d0
    s_area_x = 0.d0
! unstructured grid use vector projection to get area in each direction
! average of velocity in each direction, weighted by area    
    do ifcx = 1,6
      icnx = nd2cnx(ifcx,id_up)
      if(icnx > 0) then
        areaxx = areac(icnx)
        v_x = abs(unvxc(icnx))
        v_y = abs(unvyc(icnx))
        v_z = abs(unvzc(icnx))
        q_fx = q_flux(1,icnx)
        wvax = v_x*areaxx
        wvay = v_y*areaxx
        wvaz = v_z*areaxx
        s_fx_up(1) = s_fx_up(1) + q_fx*wvax
        s_fx_up(2) = s_fx_up(2) + q_fx*wvay
        s_fx_up(3) = s_fx_up(3) + q_fx*wvaz
        s_area_x(1) = s_area_x(1) + wvax
        s_area_x(2) = s_area_x(2) + wvay
        s_area_x(3) = s_area_x(3) + wvaz
      elseif(icnx < 0) then
        icnx = abs(icnx)
        if(icnx <= num_bcnx) then
          areaxx = areab(icnx)
          v_x = abs(uvxb(icnx))
          v_y = abs(uvyb(icnx))
          v_z = abs(uvzb(icnx))
          q_fx = q_flux_b(1,icnx)
          wvax = v_x*areaxx
          wvay = v_y*areaxx
          wvaz = v_z*areaxx
          s_fx_up(1) = s_fx_up(1) + q_fx*wvax
          s_fx_up(2) = s_fx_up(2) + q_fx*wvay
          s_fx_up(3) = s_fx_up(3) + q_fx*wvaz
          s_area_x(1) = s_area_x(1) + wvax 
          s_area_x(2) = s_area_x(2) + wvay
          s_area_x(3) = s_area_x(3) + wvaz
        else
          icnx = icnx - num_bcnx
          areaxx = areab_zf(icnx)
          v_x = abs(uvxb_zf(icnx))
          v_y = abs(uvyb_zf(icnx))
          v_z = abs(uvzb_zf(icnx))
          q_fx = 0.d0
          wvax = v_x*areaxx
          wvay = v_y*areaxx
          wvaz = v_z*areaxx
          s_fx_up(1) = s_fx_up(1) + q_fx*wvax
          s_fx_up(2) = s_fx_up(2) + q_fx*wvay
          s_fx_up(3) = s_fx_up(3) + q_fx*wvaz
          s_area_x(1) = s_area_x(1) + wvax
          s_area_x(2) = s_area_x(2) + wvay
          s_area_x(3) = s_area_x(3) + wvaz
        endif
      endif
    enddo
     
!  
return
end
!


