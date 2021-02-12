subroutine grid_init
  use grid_mod
  use grid
  use soltn 
  use react
  use trnspt
  use bufferedread
  use const 
  implicit none

#include "mpif.h"
#include "mafdecls.fh"
#include "global.fh"
!
  integer lo(3), hi(3), dims(3), ld(2), blocks(3)
  integer ndim, pnum, icnt, me, inode
  integer ixx, iyy, izz, px, py, pz
  integer i, j, k, imin, jmin, kmin, imax, jmax, kmax
  integer id1, id2, idx, ierr, idim1, iwr
  logical status
  double precision, pointer :: d_vol(:)
  double precision, pointer :: d_area(:),d_fdis(:),d_dist(:)
  double precision, pointer :: d_xsep(:),d_ysep(:),d_zsep(:)
  double precision, pointer :: c_xcen(:),c_ycen(:),c_zcen(:)
  double precision, pointer :: d_dist_up(:), d_dist_dn(:)
  integer, pointer :: i_id(:)
  integer, pointer :: i_id1(:),i_id2(:),i_bid(:)
  integer, pointer :: i_bfid(:),i_bidi(:),i_bidj(:),i_bidk(:)
!  integer, pointer :: id_l2g(:), id_g2l(:)
  double precision, pointer :: d_barea(:),d_bdist(:)
  double precision, pointer :: d_bxsep(:),d_bysep(:),d_bzsep(:)
  double precision, pointer :: d_bxcent(:),d_bycent(:),d_bzcent(:)
  double precision, pointer :: b_grvx(:),b_tltx(:)
  double precision :: xvx(4),yvx(4),zvx(4)
  double precision, dimension(:,:), allocatable :: afx
  double precision, dimension(:,:), allocatable :: dxg_p
  double precision, dimension(:,:), allocatable :: grv_x
  double precision, dimension(:,:), allocatable :: tlt_x

  double precision rx, ry, rz, r, zareaw_
  logical :: fchk
  integer :: nxyz
  integer :: np,nx,itx,jtx,n,nw,nb,ns,indx
  double precision :: pxmin,pxmax,pymin,pymax,pzmin,pzmax
  double precision :: xfw,yfw,zfw
  double precision :: xfe,yfe,zfe
  double precision :: xfb,yfb,zfb
  double precision :: xft,yft,zft
  double precision :: xfs,yfs,zfs
  double precision :: xfn,yfn,zfn
  double precision :: afxx,volx,afyx,afzx,varx
!  double precision :: small,epsl,grav,
  double precision :: tmp_x
  double precision :: dxw,dxe,dyn,dys,dzb,dzt
  double precision :: atltx,atlty,atltz
  integer :: n_node,ii !BH
  CHARACTER*16 FORM2
  DATA FORM2 / '(10(1PE16.9,1X))' /
  gwidth = 2 

  pnum = ga_nnodes()
  me = ga_nodeid()

!  nxdim = 0
!  nydim = 0
!  nzdim = 0
!  dx = 0.0d00
!  dy = 0.0d00
!  dz = 0.0d00
  xdim = 0.0d00
  ydim = 0.0d00
  zdim = 0.0d00

  nxdim = ifld
  nydim = jfld
  nzdim = kfld
  nxyz = (nxdim+1)*(nydim+1)*(nzdim+1)
  if (me.ne.0) then
   if(ics /= 8 .AND. ics /=3) then !BH
    if(.not. allocated(x)) allocate(x(nxdim+1))
    if(.not. allocated(y)) allocate(y(nydim+1))
    if(.not. allocated(z)) allocate(z(nzdim+1))
    x = 0.0d00
    y = 0.0d00
    z = 0.0d00
   else
    if(.not.allocated(xbf)) allocate(xbf(nxdim+1,nydim+1,nzdim+1))
    if(.not.allocated(ybf)) allocate(ybf(nxdim+1,nydim+1,nzdim+1))
    if(.not.allocated(zbf)) allocate(zbf(nxdim+1,nydim+1,nzdim+1))
    xbf = 0.0d00
    ybf = 0.0d00
    zbf = 0.0d00

   endif
  endif
 if(ics /=8 .AND. ics /=3 ) then !BH
  call ga_dgop(4,x(1),nxdim+1,'+')
  call ga_dgop(5,y(1),nydim+1,'+')
  call ga_dgop(6,z(1),nzdim+1,'+')
  xmaxi = maxval(x,DIM=1)
  ymaxi = maxval(y,DIM=1)
  zmaxi = maxval(z,DIM=1)
!
  xmini = minval(x,DIM=1)
  ymini = minval(y,DIM=1)
  zmini = minval(z,DIM=1)
 else
  call ga_dgop(4,xbf(1,1,1),nxyz,'+')
  call ga_dgop(5,ybf(1,1,1),nxyz,'+')
  call ga_dgop(6,zbf(1,1,1),nxyz,'+')

 ! Record the cell node coordinates -BH    
!    allocate(xlnbf(nxdim,nydim,nzdim))
!    allocate(ylnbf(nxdim,nydim,nzdim))
!    allocate(zlnbf(nxdim,nydim,nzdim))
!    xlnbf = 0.0d00
!    ylnbf = 0.0d00
!    zlnbf = 0.0d00

!    if (me == 0) then
!        allocate(xnbf(nxdim,nydim,nzdim))
!        allocate(ynbf(nxdim,nydim,nzdim))
!        allocate(znbf(nxdim,nydim,nzdim))
!        xnbf = 0.0d00
!        ynbf = 0.0d00
!        znbf = 0.0d00
!    endif    

 endif
! arrays for hydraulic gradient
!
  allocate(hgz_table_z(nzdim))
 if(ics /= 8 .AND. ics /=3 ) then !BH
  do i=1,nzdim
    hgz_table_z(i) = (z(i)+z(i+1))/2.d0
  enddo
 else
!BH
  do i=1,nzdim
    hgz_table_z(i) = (zbf(1,1,i)+zbf(1,1,i+1))/2.d0
  enddo
!   allocate(hgz_table_z_bfg(nxdim,nydim,nzdim))  
!   hgz_table_z_bfg(:,:,:) = 0.d0

 endif
  ndim = 3
  dims(1) = nxdim
  dims(2) = nydim
  dims(3) = nzdim
!print *,'0_NXDIM',nxdim,nydim,nzdim
!  dims(1) = 3200
!  dims(2) = 3200
!  dims(3) = 15
  upx = 0
  upy = 0
  upz = 0
  call ga_igop(5,upx,1,'+')
  call ga_igop(6,upy,1,'+')
  call ga_igop(7,upz,1,'+')
  nxyz = (nxdim+1)*(nydim+1)*(nzdim+1)
  if(upx > 0) then
   IF ( PNUM.NE.(UPX*UPY*UPZ) )THEN
    INDX = 4
    CHMSG = 'Total Number of Processors Does Not Match Input File '
    CALL WRMSGS( INDX )
   ENDIF
  endif
!
  if(upx > 0 ) then
! User provied processor topology
    px = upx
    py = upy
    pz = upz
  else
   call grid_factor(pnum,nxdim,nydim,nzdim,px,py,pz)
  endif
!print *,'1_NXDIM',nxdim,nydim,nzdim
  blocks(1) = px
  blocks(2) = py
  blocks(3) = pz
  ga_px = px
  ga_py = py
  ga_pz = pz
!print *,'pxyz',px,py,pz,px+py+pz
  allocate(ga_mapc(px+py+pz))
!
  icnt = 1
  do i = 1, px
    ga_mapc(icnt) = int(dble(nxdim)*dble(i-1)/dble(px))+1
    icnt = icnt + 1
  end do
  do i = 1, py
    ga_mapc(icnt) = int(dble(nydim)*dble(i-1)/dble(py))+1
    icnt = icnt + 1
  end do
  do i = 1, pz
    ga_mapc(icnt) = int(dble(nzdim)*dble(i-1)/dble(pz))+1
    icnt = icnt + 1
  end do
!print *,'3_NXDIM',nxdim,nydim,nzdim
!
  ga_dbl = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch
  IF (GAE4D) CALL GA_SET_PGROUP(ga_dbl,GAGRP)
#endif
  call ga_set_data(ga_dbl, ndim, dims, MT_DBL)
!if(me.eq.0) print *,'ga_set_irreg_distr 1',ndim,dims
  call ga_set_irreg_distr(ga_dbl, ga_mapc, blocks)
!if(me.eq.0) print *,'ga_set_irreg_distr 1e'
  status = ga_allocate(ga_dbl)
  ga_int = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch
  IF (GAE4D) CALL GA_SET_PGROUP(ga_int,GAGRP)
#endif
  call ga_set_data(ga_int, ndim, dims, MT_INT)
!if(me.eq.0) print *,'ga_set_irreg_distr 2'
  call ga_set_irreg_distr(ga_int, ga_mapc, blocks)
!if(me.eq.0) print *,'ga_set_irreg_distr 2e'
  status = ga_allocate(ga_int)
!
  call nga_distribution(ga_dbl, me, lo, hi)
!print *,'lo----hi',me,lo(1:3),hi(1:3)
!  write(*,'(a,7(I3,X))') 'ME, LO, HI', me,lo,hi
  ixmin = lo(1)
  iymin = lo(2)
  izmin = lo(3)
  ixmax = hi(1)
  iymax = hi(2)
  izmax = hi(3)
113 format(i4,' ',a5,': ',i4)
!
!  write(*,'(a,7(I3,X))') 'me, xmin, xmax',me,ixmin,iymin,izmin,ixmax,iymax,izmax
  iaxmin = ixmin
  if (lo(1).gt.1) iaxmin = iaxmin-gwidth
  iaymin = iymin
  if (lo(2).gt.1) iaymin = iaymin-gwidth
  iazmin = izmin
  if (lo(3).gt.1) iazmin = iazmin-gwidth
  iaxmax = ixmax
  if (hi(1).lt.nxdim) iaxmax = iaxmax+gwidth
!print *,'in ga',me,iaxmax,hi(1)
!stop
! print *,'4_NXDIM',nxdim,nydim,nzdim
  iaymax = iymax
  if (hi(2).lt.nydim) iaymax = iaymax+gwidth
  iazmax = izmax
  if (hi(3).lt.nzdim) iazmax = iazmax+gwidth

!  write(*,'(a,7(I3,X))') 'me,axmin, axmax',me,iaxmin,iaymin,iazmin,iaxmax,iaymax,iazmax
  ldx = iaxmax - iaxmin + 1
  ldy = iaymax - iaymin + 1
  ldz = iazmax - iazmin + 1
114 format(i4,' ',a6,': ',i4)
 write(*,'(a,4(I3,X))') 'me,ldx,ldy,ldz',me,ldx,ldy,ldz
!
!  Determine number of locally held nodes, etc.
!
  num_loc_nodes = (ixmax-ixmin+1)*(iymax-iymin+1)*(izmax-izmin+1)
  allocate(local_node_list(num_loc_nodes))
  num_nodes = (iaxmax-iaxmin+1)*(iaymax-iaymin+1)*(iazmax-iazmin+1)
  allocate(grid_mask(num_nodes))
!  write(*,*) 'Me, num_loc_nodes,num_nodes',me,num_loc_nodes,num_nodes
  
!
  ixx = ixmax-ixmin
  if (ixmin.gt.1.and.nxdim.ne.1) ixx = ixx + 1
  if (ixmax.lt.nxdim.and.nxdim.ne.1) ixx = ixx + 1
  iyy = iymax-iymin
  if (iymin.gt.1.and.nydim.ne.1) iyy = iyy + 1
  if (iymax.lt.nydim.and.nydim.ne.1) iyy = iyy + 1
  izz = izmax-izmin
  if (izmin.gt.1.and.nzdim.ne.1) izz = izz + 1
  if (izmax.lt.nzdim.and.nzdim.ne.1) izz = izz + 1

  num_cnx = ixx*(iymax-iymin+1)*(izmax-izmin+1) &
          + iyy*(ixmax-ixmin+1)*(izmax-izmin+1) &
          + izz*(ixmax-ixmin+1)*(iymax-iymin+1)
  ixx = 0
  if (ixmin.eq.1.and.nxdim.ne.1) ixx = ixx + 1
  if (ixmax.eq.nxdim.and.nxdim.ne.1) ixx = ixx + 1
  iyy = 0
  if (iymin.eq.1.and.nydim.ne.1) iyy = iyy + 1
  if (iymax.eq.nydim.and.nydim.ne.1) iyy = iyy + 1
  izz = 0
  if (izmin.eq.1.and.nzdim.ne.1) izz = izz + 1
  if (izmax.eq.nzdim.and.nzdim.ne.1) izz = izz + 1
  num_bcnx = ixx*(iymax-iymin+1)*(izmax-izmin+1) &
           + iyy*(ixmax-ixmin+1)*(izmax-izmin+1) &
           + izz*(ixmax-ixmin+1)*(iymax-iymin+1)
!  write(*,*) 'me,num_bcnx',me,num_bcnx
!  print *,'5_NXDIM',nxdim,nydim,nzdim
!
!  Start building grid. This will need to be replaced with STOMP input
!  routines
!
  d2dim1_max = 1
  i2dim1_max = 1
  d3dim1_max = 1
  i3dim1_max = 1
  d3dim2_max = 1
  i3dim2_max = 1
!
  dnode_field = 0
  inode_field = 0
  dnode_2field = 0
  inode_2field = 0
  dnode_3field = 0
  inode_3field = 0
!
  call add_node_dfield('xcent',idx)
  d_xc => d_nd_fld(idx)%p
  call add_node_dfield('ycent',idx)
  d_yc => d_nd_fld(idx)%p
  call add_node_dfield('zcent',idx)
  d_zc => d_nd_fld(idx)%p
  call add_node_dfield('volume',idx)
  d_vol => d_nd_fld(idx)%p
  call add_node_dfield('dxgf',idx)
  dxgf => d_nd_fld(idx)%p
  call add_node_dfield('dygf',idx)
  dygf => d_nd_fld(idx)%p
  call add_node_dfield('dzgf',idx)
  dzgf => d_nd_fld(idx)%p
  call add_node_dfield('grvpx',idx)
  grvpx => d_nd_fld(idx)%p
  call add_node_dfield('grvpy',idx)
  grvpy => d_nd_fld(idx)%p
  call add_node_dfield('grvpz',idx)
  grvpz => d_nd_fld(idx)%p
!
  call add_node_ifield('natural_id',idx)
  i_id => i_nd_fld(idx)%p
!print *,'me-natu-id',me,idx
!  call add_node_ifield('local2ghostid',idx)
!  id_l2g => i_nd_fld(idx)%p
!  call add_node_ifield('ghost2localid',idx)
!  id_g2l => i_nd_fld(idx)%p

!
! Assign grid cell locations and volumes
!
  inode = 0
  if(ics == 8 .or. ics ==3) then !BH
    allocate(afx(6,num_nodes))
    afx = 0.d0
    allocate(grv_x(6,num_nodes))
    grv_x = 0.d0
    allocate(tlt_x(6,num_nodes))
    tlt_x = 0.d0
    allocate(dxg_p(6,num_nodes))
    dxg_p = 0.d0
  endif
  do i = 1, num_nodes
!  write(*,*) 'num_nodes',num_nodes
!  nxdim = ifld
!  nydim = jfld
!  nzdim = kfld
!        print *,'ifld',ifld,jfld,kfld
!        print *,'6_NXDIM',nxdim,nydim,nzdim 
!        print *,'dims',dims
!
! Evaluate local indices of node
!
    icnt = i-1
    ixx = mod(icnt,ldx)
    icnt = (icnt-ixx)/ldx
    iyy = mod(icnt,ldy)
    izz = (icnt-iyy)/ldy
!
    ixx = ixx + 1
    iyy = iyy + 1
    izz = izz + 1
!
! Correct lower bound for ghost cells
!
    if (ixmin.gt.1) then
      ixx = ixx - gwidth
    endif
    if (iymin.gt.1) then
      iyy = iyy - gwidth
    endif
    if (izmin.gt.1) then
      izz = izz - gwidth
    endif
    ixx = ixx + ixmin - 1
    iyy = iyy + iymin - 1
    izz = izz + izmin - 1
!    write(*,'(a,5(I10,X))')'me, i, ixx,iyy,izz',me, I,ixx,iyy,izz
!
!  Determine if node is local, ghost, or unused
!
    if (ixx.ge.ixmin.and.ixx.le.ixmax.and.  &
        iyy.ge.iymin.and.iyy.le.iymax.and.  &
        izz.ge.izmin.and.izz.le.izmax) then
! local node
!      grid_mask(i) = 1
      inode = inode + 1
! ghosted id to local id
      grid_mask(i) = inode
! local id to ghosted id
      local_node_list(inode) = i
    elseif (ixx.ge.ixmin.and.ixx.le.ixmax.and.  &
            iyy.ge.iymin.and.iyy.le.iymax) then
! ghost node
      grid_mask(i) = -1
    elseif (ixx.ge.ixmin.and.ixx.le.ixmax.and.  &
            izz.ge.izmin.and.izz.le.izmax) then
      grid_mask(i) = -1
    elseif (iyy.ge.iymin.and.iyy.le.iymax.and.  &
            izz.ge.izmin.and.izz.le.izmax) then
      grid_mask(i) = -1
    else
! unused node
      grid_mask(i) = 0
    endif
!        write(*,'(a,6(I10,X))')'02 ixx,iyy,izz,ixmin,iymin,izmin',ixx,iyy,izz,ixmin,iymin,izmin
!        write(*,*) 'i,grid mask:',i,grid_mask(i)
!        write(*,*) 'ics',ics
   if(ics.ne.8 .AND. ics .ne. 3) then !BH
    d_xc(i) = (x(ixx)+x(ixx+1))*0.5d00
    dxgf(i) = (x(ixx+1)-x(ixx))
    d_yc(i) = (y(iyy)+y(iyy+1))*0.5d00
    dygf(i) = (y(iyy+1)-y(iyy))
    d_zc(i) = (z(izz)+z(izz+1))*0.5d00
    dzgf(i) = (z(izz+1)-z(izz))
    if( ics.eq.2 ) then
      zareaw_ = (x(ixx+1)*x(ixx+1)-x(ixx)*x(ixx))*(y(iyy+1)-y(iyy))/2.d0
      d_vol(i) = zareaw_*(z(izz+1)-z(izz))
    else
    d_vol(i) = (x(ixx+1)-x(ixx))*(y(iyy+1)-y(iyy))*(z(izz+1)-z(izz))
    endif
   else
!        write(*,*) 'i,d_vol (should be 0)',i,d_vol(i)
      d_vol(i) = 0.d0
        PXMIN = 1.D+20
        PXMAX = -1.D+20
        PYMIN = 1.D+20
        PYMAX = -1.D+20
        PZMIN = 1.D+20
        PZMAX = -1.D+20
!
!---        West surface centroid  ---
!       write(*,'(a,6(I10,X))')'021 ixx,iyy,izz,ixmin,iymin,izmin',ixx,iyy,izz,ixmin,iymin,izmin
        XVX(1) = XBF(IXX,IYY,IZZ)
        XVX(2) = XBF(IXX,IYY+1,izz)
        XVX(3) = XBF(ixx,iyy+1,izz+1)
        XVX(4) = XBF(ixx,iyy,izz+1)
        YVX(1) = YBF(ixx,iyy,izz)
        YVX(2) = YBF(ixx,iyy+1,izz)
        YVX(3) = YBF(ixx,iyy+1,izz+1)
        YVX(4) = YBF(ixx,iyy,izz+1)
        ZVX(1) = ZBF(ixx,iyy,izz)
        ZVX(2) = ZBF(ixx,iyy+1,izz)
        ZVX(3) = ZBF(ixx,iyy+1,izz+1)
        ZVX(4) = ZBF(ixx,iyy,izz+1)
        NP = 4
        DO 1741 NX = 1,NP
!          write(*,*) 'NX',NX
          PXMIN = MIN( XVX(NX),PXMIN )
          PXMAX = MAX( XVX(NX),PXMAX )
          PYMIN = MIN( YVX(NX),PYMIN )
          PYMAX = MAX( YVX(NX),PYMAX )
          PZMIN = MIN( ZVX(NX),PZMIN )
          PZMAX = MAX( ZVX(NX),PZMAX )
   1741       CONTINUE
        CALL PGCNTRD( NP,XVX,YVX,ZVX,XFW,YFW,ZFW )

!
!---        East surface centroid  ---
!
        XVX(1) = XBF(Ixx+1,iyy,izz)
        XVX(2) = XBF(Ixx+1,iyy+1,izz)
        XVX(3) = XBF(Ixx+1,iyy+1,izz+1)
        XVX(4) = XBF(Ixx+1,iyy,izz+1)
        YVX(1) = YBF(Ixx+1,iyy,izz)
        YVX(2) = YBF(Ixx+1,iyy+1,izz)
        YVX(3) = YBF(Ixx+1,iyy+1,izz+1)
        YVX(4) = YBF(Ixx+1,iyy,izz+1)
        ZVX(1) = ZBF(Ixx+1,iyy,izz)
        ZVX(2) = ZBF(Ixx+1,iyy+1,izz)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz+1)
        ZVX(4) = ZBF(Ixx+1,iyy,izz+1)
        NP = 4
        DO 1742 NX = 1,NP
          PXMIN = MIN( XVX(NX),PXMIN )
          PXMAX = MAX( XVX(NX),PXMAX )
          PYMIN = MIN( YVX(NX),PYMIN )
          PYMAX = MAX( YVX(NX),PYMAX )
          PZMIN = MIN( ZVX(NX),PZMIN )
          PZMAX = MAX( ZVX(NX),PZMAX )
   1742       CONTINUE
        CALL PGCNTRD( NP,XVX,YVX,ZVX,XFE,YFE,ZFE )
!
!---        South surface centroid  ---
!
        XVX(1) = XBF(Ixx,iyy,izz)
        XVX(2) = XBF(Ixx+1,iyy,izz)
        XVX(3) = XBF(Ixx+1,iyy,izz+1)
        XVX(4) = XBF(Ixx,iyy,izz+1)
        YVX(1) = YBF(Ixx,iyy,izz)
        YVX(2) = YBF(Ixx+1,iyy,izz)
        YVX(3) = YBF(Ixx+1,iyy,izz+1)
        YVX(4) = YBF(Ixx,iyy,izz+1)
        ZVX(1) = ZBF(Ixx,iyy,izz)
        ZVX(2) = ZBF(Ixx+1,iyy,izz)
        ZVX(3) = ZBF(Ixx+1,iyy,izz+1)
        ZVX(4) = ZBF(Ixx,iyy,izz+1)
        NP = 4
        DO 1743 NX = 1,NP
          PXMIN = MIN( XVX(NX),PXMIN )
          PXMAX = MAX( XVX(NX),PXMAX )
          PYMIN = MIN( YVX(NX),PYMIN )
          PYMAX = MAX( YVX(NX),PYMAX )
          PZMIN = MIN( ZVX(NX),PZMIN )
          PZMAX = MAX( ZVX(NX),PZMAX )
   1743       CONTINUE
        CALL PGCNTRD( NP,XVX,YVX,ZVX,XFS,YFS,ZFS )
!
!---        North surface centroid  ---
!
        XVX(1) = XBF(Ixx,iyy+1,izz)
        XVX(2) = XBF(Ixx+1,iyy+1,izz)
        XVX(3) = XBF(Ixx+1,iyy+1,izz+1)
        XVX(4) = XBF(Ixx,iyy+1,izz+1)
        YVX(1) = YBF(Ixx,iyy+1,izz)
        YVX(2) = YBF(Ixx+1,iyy+1,izz)
        YVX(3) = YBF(Ixx+1,iyy+1,izz+1)
        YVX(4) = YBF(Ixx,iyy+1,izz+1)
        ZVX(1) = ZBF(Ixx,iyy+1,izz)
        ZVX(2) = ZBF(Ixx+1,iyy+1,izz)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz+1)
        ZVX(4) = ZBF(Ixx,iyy+1,izz+1)
        NP = 4
        DO 1744 NX = 1,NP
          PXMIN = MIN( XVX(NX),PXMIN )
          PXMAX = MAX( XVX(NX),PXMAX )
          PYMIN = MIN( YVX(NX),PYMIN )
          PYMAX = MAX( YVX(NX),PYMAX )
          PZMIN = MIN( ZVX(NX),PZMIN )
          PZMAX = MAX( ZVX(NX),PZMAX )
   1744       CONTINUE
        CALL PGCNTRD( NP,XVX,YVX,ZVX,XFN,YFN,ZFN )
!
!---        Bottom surface centroid  ---
!
        XVX(1) = XBF(Ixx,iyy,izz)
        XVX(2) = XBF(Ixx+1,iyy,izz)
        XVX(3) = XBF(Ixx+1,iyy+1,izz)
        XVX(4) = XBF(Ixx,iyy+1,izz)
        YVX(1) = YBF(Ixx,iyy,izz)
        YVX(2) = YBF(Ixx+1,iyy,izz)
        YVX(3) = YBF(Ixx+1,iyy+1,izz)
        YVX(4) = YBF(Ixx,iyy+1,izz)
        ZVX(1) = ZBF(Ixx,iyy,izz)
        ZVX(2) = ZBF(Ixx+1,iyy,izz)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz)
        ZVX(4) = ZBF(Ixx,iyy+1,izz)
        NP = 4
        DO 1745 NX = 1,NP
          PXMIN = MIN( XVX(NX),PXMIN )
          PXMAX = MAX( XVX(NX),PXMAX )
          PYMIN = MIN( YVX(NX),PYMIN )
          PYMAX = MAX( YVX(NX),PYMAX )
          PZMIN = MIN( ZVX(NX),PZMIN )
          PZMAX = MAX( ZVX(NX),PZMAX )
   1745       CONTINUE
        CALL PGCNTRD( NP,XVX,YVX,ZVX,XFB,YFB,ZFB )
!
!---        Top surface centroid  ---
!
        XVX(1) = XBF(Ixx,iyy,izz+1)
        XVX(2) = XBF(Ixx+1,iyy,izz+1)
        XVX(3) = XBF(Ixx+1,iyy+1,izz+1)
        XVX(4) = XBF(Ixx,iyy+1,izz+1)
        YVX(1) = YBF(Ixx,iyy,izz+1)
        YVX(2) = YBF(Ixx+1,iyy,izz+1)
        YVX(3) = YBF(Ixx+1,iyy+1,izz+1)
        YVX(4) = YBF(Ixx,iyy+1,izz+1)
        ZVX(1) = ZBF(Ixx,iyy,izz+1)
        ZVX(2) = ZBF(Ixx+1,iyy,izz+1)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz+1)
        ZVX(4) = ZBF(Ixx,iyy+1,izz+1)
        NP = 4
        DO 1746 NX = 1,NP
          PXMIN = MIN( XVX(NX),PXMIN )
          PXMAX = MAX( XVX(NX),PXMAX )
          PYMIN = MIN( YVX(NX),PYMIN )
          PYMAX = MAX( YVX(NX),PYMAX )
          PZMIN = MIN( ZVX(NX),PZMIN )
          PZMAX = MAX( ZVX(NX),PZMAX )
   1746       CONTINUE
        CALL PGCNTRD( NP,XVX,YVX,ZVX,XFT,YFT,ZFT )
        d_xc(I) = 5.D-1*(XFW+XFE)
        d_yc(I) = 5.D-1*(YFS+YFN)
        d_zc(I) = 5.D-1*(ZFB+ZFT)

! Assign cell centroid coordinates to a global matrix.
! This global matrix is still a local variable  -BH
!        write(*,'(a,7(I3,X))') 'ME,xmin,xmax,ymin,ymax,zmin,zmax:',me,ixmin,ixmax,iymin,iymax,izmin,izmax 
!        if (ixx .GE. ixmin .AND. ixx .LE. ixmax .AND. &
!              iyy .GE. iymin .AND. iyy .LE. iymax .AND. &
!                izz .GE. izmin .AND. izz .LE. izmax) then
!        
!                xlnbf(ixx,iyy,izz) = d_xc(I)
!                ylnbf(ixx,iyy,izz) = d_yc(I)
!                zlnbf(ixx,iyy,izz) = d_zc(I)
!       endif

        IF( d_xc(I).LT.PXMIN .OR. d_xc(I).GT.PXMAX .OR. &
          d_yc(I).LT.PYMIN .OR. d_yc(I).GT.PYMAX .OR. &
          d_zc(I).LT.PZMIN .OR. d_zc(I).GT.PZMAX ) THEN
          PRINT *,'Node Centroid Outside of Polygon Limits'
!          PRINT *,'N = ',N,' I = ',ID(N),' J = ',JD(N),' K = ',KD(N)
          PRINT *,'XP = ',d_xc(i),' XMIN = ',PXMIN,' XMAX = ',PXMAX
          PRINT *,'YP = ',d_yc(i),' YMIN = ',PYMIN,' YMAX = ',PYMAX
          PRINT *,'ZP = ',d_zc(i),' ZMIN = ',PZMIN,' ZMAX = ',PZMAX
        ENDIF
!
        XVX(1) = XBF(IXX,IYY,IZZ)
        XVX(2) = XBF(IXX,IYY+1,izz)
        XVX(3) = XBF(ixx,iyy+1,izz+1)
        XVX(4) = XBF(ixx,iyy,izz+1)
        YVX(1) = YBF(ixx,iyy,izz)
        YVX(2) = YBF(ixx,iyy+1,izz)
        YVX(3) = YBF(ixx,iyy+1,izz+1)
        YVX(4) = YBF(ixx,iyy,izz+1)
        ZVX(1) = ZBF(ixx,iyy,izz)
        ZVX(2) = ZBF(ixx,iyy+1,izz)
        ZVX(3) = ZBF(Ixx,iyy+1,izz+1)
        ZVX(4) = ZBF(Ixx,iyy,izz+1)
      NP = 4
!
!---      West surface area and volume contributions  ---
!
!
      AFX(1,i) = 0.D+0
      DO 1750 ITX = 1,4
        JTX = ITX+1
        IF( JTX.EQ.5 ) JTX = 1
        CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX), &
          XVX(JTX),YVX(JTX),ZVX(JTX),XFW,YFW,ZFW,AFXX )
        AFX(1,i) = AFX(1,i)+AFXX
        CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX), &
          ZVX(JTX),XFW,YFW,ZFW,d_xc(i),d_yc(i),d_zc(i),VOLX )
        D_VOL(I) = D_VOL(I)+VOLX
   1750     CONTINUE
!
        XVX(1) = XBF(Ixx+1,iyy,izz)
        XVX(2) = XBF(Ixx+1,iyy+1,izz)
        XVX(3) = XBF(Ixx+1,iyy+1,izz+1)
        XVX(4) = XBF(Ixx+1,iyy,izz+1)
        YVX(1) = YBF(Ixx+1,iyy,izz)
        YVX(2) = YBF(Ixx+1,iyy+1,izz)
        YVX(3) = YBF(Ixx+1,iyy+1,izz+1)
        YVX(4) = YBF(Ixx+1,iyy,izz+1)
        ZVX(1) = ZBF(Ixx+1,iyy,izz)
        ZVX(2) = ZBF(Ixx+1,iyy+1,izz)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz+1)
        ZVX(4) = ZBF(Ixx+1,iyy,izz+1)
      NP = 4

!
!---      East surface area and volume contributions  ---
!
      AFX(2,i) = 0.D+0
      DO 1752 ITX = 1,4
        JTX = ITX+1
        IF( JTX.EQ.5 ) JTX = 1
        CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX), &
          XVX(JTX),YVX(JTX),ZVX(JTX),XFE,YFE,ZFE,AFXX )
        AFX(2,i) = AFX(2,i)+AFXX
        CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX), &
          ZVX(JTX),XFE,YFE,ZFE,d_xc(i),d_yc(i),d_zc(i),VOLX )
        D_VOL(I) = D_VOL(I)+VOLX
   1752     CONTINUE

!
!---      West-east differentials  ---
!
      DXW = SQRT((d_xc(i)-XFW)**2 + (d_yc(i)-YFW)**2 + (d_zc(i)-ZFW)**2)
      DXE = SQRT((XFE-d_xc(i))**2 + (YFE-d_yc(i))**2 + (ZFE-d_zc(i))**2)
      n = i
      DXGF(N) = MAX( DXW+DXE,SMALL )
      DXG_P(1,n) = MAX( DXG_P(1,n)+DXW,SMALL )
      DXG_P(2,n) = MAX( DXG_P(2,n)+DXE,SMALL )
!
        XVX(1) = XBF(Ixx,iyy,izz)
        XVX(2) = XBF(Ixx+1,iyy,izz)
        XVX(3) = XBF(Ixx+1,iyy,izz+1)
        XVX(4) = XBF(Ixx,iyy,izz+1)
        YVX(1) = YBF(Ixx,iyy,izz)
        YVX(2) = YBF(Ixx+1,iyy,izz)
        YVX(3) = YBF(Ixx+1,iyy,izz+1)
        YVX(4) = YBF(Ixx,iyy,izz+1)
        ZVX(1) = ZBF(Ixx,iyy,izz)
        ZVX(2) = ZBF(Ixx+1,iyy,izz)
        ZVX(3) = ZBF(Ixx+1,iyy,izz+1)
        ZVX(4) = ZBF(Ixx,iyy,izz+1)
      NP = 4


!
!---      South surface area and volume contributions  ---
!
      AFX(3,i) = 0.D+0
      DO 1754 ITX = 1,4
        JTX = ITX+1
        IF( JTX.EQ.5 ) JTX = 1
        CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX), &
          XVX(JTX),YVX(JTX),ZVX(JTX),XFS,YFS,ZFS,AFYX )
        AFX(3,I) = AFX(3,I)+AFYX
        CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX), &
          ZVX(JTX),XFS,YFS,ZFS,d_xc(N),d_yc(N),d_zc(N),VOLX )
        D_VOL(I) = D_VOL(I)+VOLX
   1754     CONTINUE
!
        XVX(1) = XBF(Ixx,iyy+1,izz)
        XVX(2) = XBF(Ixx+1,iyy+1,izz)
        XVX(3) = XBF(Ixx+1,iyy+1,izz+1)
        XVX(4) = XBF(Ixx,iyy+1,izz+1)
        YVX(1) = YBF(Ixx,iyy+1,izz)
        YVX(2) = YBF(Ixx+1,iyy+1,izz)
        YVX(3) = YBF(Ixx+1,iyy+1,izz+1)
        YVX(4) = YBF(Ixx,iyy+1,izz+1)
        ZVX(1) = ZBF(Ixx,iyy+1,izz)
        ZVX(2) = ZBF(Ixx+1,iyy+1,izz)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz+1)
        ZVX(4) = ZBF(Ixx,iyy+1,izz+1)
      NP = 4
!
!---      North surface area and volume contributions  ---
!
      AFX(4,I) = 0.D+0
      DO 1756 ITX = 1,4
        JTX = ITX+1
        IF( JTX.EQ.5 ) JTX = 1
        CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX), &
          XVX(JTX),YVX(JTX),ZVX(JTX),XFN,YFN,ZFN,AFYX )
        AFX(4,I) = AFX(4,I)+AFYX
        CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX), &
          ZVX(JTX),XFN,YFN,ZFN,d_xc(N),d_yc(N),d_zc(N),VOLX )
        D_VOL(I) = D_VOL(I)+VOLX
   1756     CONTINUE
!
!---      South-north differentials  ---
!
      DYS = SQRT((d_xc(N)-XFS)**2 + (d_yc(N)-YFS)**2 + (d_zc(N)-ZFS)**2)
      DYN = SQRT((XFN-d_xc(N))**2 +(YFN-d_yc(N))**2 +(ZFN-d_zc(N))**2)
      DYGF(N) = MAX( DYS+DYN,SMALL )
      dxg_p(3,n) = MAX( dxg_p(3,n)+DYS,SMALL )
      dxg_p(4,n) = MAX( dxg_p(4,n)+DYN,SMALL )
!
        XVX(1) = XBF(Ixx,iyy,izz)
        XVX(2) = XBF(Ixx+1,iyy,izz)
        XVX(3) = XBF(Ixx+1,iyy+1,izz)
        XVX(4) = XBF(Ixx,iyy+1,izz)
        YVX(1) = YBF(Ixx,iyy,izz)
        YVX(2) = YBF(Ixx+1,iyy,izz)
        YVX(3) = YBF(Ixx+1,iyy+1,izz)
        YVX(4) = YBF(Ixx,iyy+1,izz)
        ZVX(1) = ZBF(Ixx,iyy,izz)
        ZVX(2) = ZBF(Ixx+1,iyy,izz)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz)
        ZVX(4) = ZBF(Ixx,iyy+1,izz)
      NP = 4
!
!---      Bottom surface area and volume contributions  ---
!
      AFX(5,I) = 0.D+0
      DO 1758 ITX = 1,4
        JTX = ITX+1
        IF( JTX.EQ.5 ) JTX = 1
        CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX), &
          XVX(JTX),YVX(JTX),ZVX(JTX),XFB,YFB,ZFB,AFZX )
        AFX(5,I) = AFX(5,I)+AFZX
        CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX), &
          ZVX(JTX),XFB,YFB,ZFB,d_xc(N),d_yc(N),d_zc(N),VOLX )
        D_VOL(I) = D_VOL(I)+VOLX
   1758     CONTINUE
!
        XVX(1) = XBF(Ixx,iyy,izz+1)
        XVX(2) = XBF(Ixx+1,iyy,izz+1)
        XVX(3) = XBF(Ixx+1,iyy+1,izz+1)
        XVX(4) = XBF(Ixx,iyy+1,izz+1)
        YVX(1) = YBF(Ixx,iyy,izz+1)
        YVX(2) = YBF(Ixx+1,iyy,izz+1)
        YVX(3) = YBF(Ixx+1,iyy+1,izz+1)
        YVX(4) = YBF(Ixx,iyy+1,izz+1)
        ZVX(1) = ZBF(Ixx,iyy,izz+1)
        ZVX(2) = ZBF(Ixx+1,iyy,izz+1)
        ZVX(3) = ZBF(Ixx+1,iyy+1,izz+1)
        ZVX(4) = ZBF(Ixx,iyy+1,izz+1)
      NP = 4
!
!---      Top surface area and volume contributions  ---
!
      AFX(6,I) = 0.D+0
      DO 1760 ITX = 1,4
        JTX = ITX+1
        IF( JTX.EQ.5 ) JTX = 1
        CALL TRGAREA( XVX(ITX),YVX(ITX),ZVX(ITX), &
          XVX(JTX),YVX(JTX),ZVX(JTX),XFT,YFT,ZFT,AFZX )
        AFX(6,I) = AFX(6,I)+AFZX
        CALL TETRVOL( XVX(ITX),YVX(ITX),ZVX(ITX),XVX(JTX),YVX(JTX), &
          ZVX(JTX),XFT,YFT,ZFT,d_xc(N),d_yc(N),d_zc(N),VOLX )
        D_VOL(I) = D_VOL(I)+VOLX
   1760     CONTINUE
!
!---      Bottom-top differentials  ---
!
      DZB = SQRT((d_xc(N)-XFB)**2 + (d_yc(N)-YFB)**2 + (d_zc(N)-ZFB)**2)
      DZT = SQRT((XFT-d_xc(N))**2 + (YFT-d_yc(N))**2 + (ZFT-d_zc(N))**2)
      DZGF(N) = MAX( DZB+DZT,SMALL )
      DXG_P(5,N) = MAX( dxg_p(5,n)+DZB,SMALL )
      DXG_P(6,n) = MAX( dxg_p(6,n)+DZT,SMALL )

        !
!---      West-east gravity vectors and surface tilts  ---
!
      IF( IXX.EQ.iaxmin ) THEN
        VARX = SQRT((d_xc(N)-XFW)**2 + (d_yc(N)-YFW)**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_X(1,I) = GRAV
        ELSE
          GRV_X(1,I) = GRAV*SIN(ATAN((d_zc(N)-ZFW)/VARX))
        ENDIF
        VARX = (d_xc(N)-XFW)
        IF( VARX.LT.EPSL ) THEN
          TLT_X(1,i) = 0.D+0
        ELSE
          ATLTX = ATAN(SQRT((d_zc(N)-ZFW)**2 + (d_yc(N)-YFW)**2)/VARX)
          TLT_X(1,i) = SIGN( COS(ATLTX),ATLTX )
        ENDIF
      ENDIF
      IF( IXX.EQ.iaxmax ) THEN
        VARX = SQRT((XFE-d_xc(N))**2 + (YFE-d_yc(N))**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_X(2,I) = GRAV
        ELSE
          GRV_X(2,I) = GRAV*SIN(ATAN((ZFE-d_zc(N))/VARX))
        ENDIF
        VARX = (XFE-d_xc(N))
        IF( VARX.LT.EPSL ) THEN
          TLT_X(2,I) = 0.D+0
        ELSE
          ATLTX = ATAN(SQRT((ZFE-d_zc(N))**2 + (YFE-d_yc(N))**2)/VARX)
          TLT_X(2,I) = SIGN( COS(ATLTX),ATLTX )
        ENDIF
      ENDIF
      IF( IXX.GT.iaxmin .AND. ldx.GT.1 ) THEN
        nw = n-1
        VARX = SQRT((d_xc(N)-d_xc(NW))**2 + (d_yc(N)-d_yc(NW))**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_X(2,i-1) = GRAV
          GRV_X(1,i) = GRAV
        ELSE
          tmp_x = GRAV*SIN(ATAN((d_zc(N)-d_zc(NW))/VARX))
          GRV_X(2,i-1) = tmp_x
          GRV_X(1,i) = tmp_x
        ENDIF
        VARX = (d_xc(N)-d_xc(NW))
        IF( VARX.LT.EPSL ) THEN
          TLT_X(2,i-1) = 0.D+0
          TLT_X(1,i) = 0.D+0
        ELSE
          ATLTX = ATAN(SQRT((d_zc(N)-d_zc(NW))**2 + &
             (d_yc(N)-d_yc(NW))**2)/VARX)
          tmp_x = SIGN( COS(ATLTX),ATLTX )
          TLT_X(2,i-1) = tmp_x
          TLT_X(1,i) = tmp_x
        ENDIF
      ENDIF
!
!---      South-north gravity vectors and surface tilts  ---
!
      IF( IYY.EQ.iaymin ) THEN
        VARX = SQRT((d_xc(N)-XFS)**2 + (d_yc(N)-YFS)**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_X(3,i) = GRAV
        ELSE
          GRV_x(3,i) = GRAV*SIN(ATAN((d_zc(N)-ZFS)/VARX))
        ENDIF
        VARX = (d_yc(N)-YFS)
        IF( VARX.LT.EPSL ) THEN
          TLT_x(3,i) = 0.D+0
        ELSE
          ATLTY = ATAN(SQRT((d_zc(N)-ZFS)**2 + (d_xc(N)-XFS)**2)/VARX)
          TLT_x(3,i) = SIGN( COS(ATLTY),ATLTY )
        ENDIF
      ENDIF
      IF( IYY.EQ. iaymax) THEN
        VARX = SQRT((XFN-d_xc(N))**2 + (YFN-d_yc(N))**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_x(4,i) = GRAV
        ELSE
          GRV_x(4,i) = GRAV*SIN(ATAN((ZFN-d_zc(N))/VARX))
        ENDIF
        VARX = (YFN-d_yc(N))
        IF( VARX.LT.EPSL ) THEN
          TLT_x(4,i) = 0.D+0
        ELSE
          ATLTY = ATAN(SQRT((ZFN-d_zc(N))**2 + (XFN-d_xc(N))**2)/VARX)
          TLT_x(4,i) = SIGN( COS(ATLTY),ATLTY )
        ENDIF
      ENDIF
      IF( IYY.GT.iaymin .AND. ldy.GT.1 ) THEN
        ns = n-ldx
        VARX = SQRT((d_xc(N)-d_xc(NS))**2 + (d_yc(N)-d_yc(NS))**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_x(4,ns) = GRAV
          GRV_x(3,i) = GRAV
        ELSE
          tmp_x = GRAV*SIN(ATAN((d_zc(N)-d_zc(NS))/VARX))
          GRV_x(4,ns) = tmp_x 
          GRV_x(3,i) = tmp_x 
        ENDIF
        VARX = (d_yc(N)-d_yc(NS))
        IF( VARX.LT.EPSL ) THEN
          TLT_x(4,ns) = 0.D+0
          TLT_x(3,i) = 0.D+0
        ELSE
          ATLTY = ATAN(SQRT((d_zc(N)-d_zc(NS))**2 +  &
            (d_xc(N)-d_xc(NS))**2)/VARX)
          tmp_x = SIGN( COS(ATLTY),ATLTY )
          TLT_x(4,ns) = tmp_x
          TLT_x(3,i) = tmp_x
        ENDIF
      ENDIF
!
!---      Bottom-top gravity vectors and surface tilts  ---
!
      IF( IZZ.EQ.iazmin ) THEN
        VARX = SQRT((d_xc(N)-XFB)**2 + (d_yc(N)-YFB)**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_X(5,i) = GRAV
        ELSE
          GRV_x(5,i) = GRAV*SIN(ATAN((d_zc(N)-ZFB)/VARX))
        ENDIF
        VARX = (d_zc(N)-ZFB)
        IF( VARX.LT.EPSL ) THEN
          TLT_X(5,i) = 0.D+0
        ELSE
          ATLTZ = ATAN(SQRT((d_xc(N)-XFB)**2 + (d_yc(N)-YFB)**2)/VARX)
          TLT_X(5,i) = SIGN( COS(ATLTZ),ATLTZ )
        ENDIF
      ENDIF
      IF( IZZ.EQ.iazmax ) THEN
        VARX = SQRT((XFT-d_xc(N))**2 + (YFT-d_yc(N))**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_X(6,i) = GRAV
        ELSE
          tmp_x = GRAV*SIN(ATAN((ZFT-d_zc(N))/VARX))
           grv_x(6,i) = tmp_x
        ENDIF
        VARX = (ZFT-d_zc(N))
        IF( VARX.LT.EPSL ) THEN
          TLT_x(6,i) = 0.D+0
        ELSE
          ATLTZ = ATAN(SQRT((XFT-d_xc(N))**2 + (YFT-d_yc(N))**2)/VARX)
          tmp_x = SIGN( COS(ATLTZ),ATLTZ )
          tlt_x(6,i) = tmp_x
        ENDIF
      ENDIF
      IF( IZZ.GT.iazmin .AND. ldz.GT.1 ) THEN
        nb = n-ldx*ldy
        VARX = SQRT((d_xc(N)-d_xc(NB))**2 + (d_yc(N)-d_yc(NB))**2)
        IF( VARX.LT.EPSL ) THEN
          GRV_x(6,nb) = GRAV
          GRV_x(5,i) = GRAV
        ELSE
          tmp_x = GRAV*SIN(ATAN((d_zc(N)-d_zc(NB))/VARX))
          grv_x(6,nb) = tmp_x
          grv_x(5,i) = tmp_x
        ENDIF
        VARX = (d_zc(N)-d_zc(NB))
        IF( VARX.LT.EPSL ) THEN
          TLT_x(6,nb) = 0.D+0
          TLT_x(5,i) = 0.D+0
        ELSE
          ATLTZ = ATAN(SQRT((d_xc(N)-d_xc(NB))**2 +  &
            (d_yc(N)-d_yc(NB))**2)/VARX)
          tmp_x = SIGN( COS(ATLTZ),ATLTZ )
          tlt_x(6,nb) = tmp_x
          tlt_x(5,i) = tmp_x
        ENDIF
      ENDIF

   endif
!        print *,'i ixx, iyy, izz,nxdim,nydim',i,ixx, iyy, izz,dims
    i_id(i) = (ixx-1) + (iyy-1)*nxdim + (izz-1)*nxdim*nydim + 1
!    i_id(i) = (ixx-1) + (iyy-1)*dims(1) + (izz-1)*dims(2)*nydim + 1
!print *,'i_id-----i_id','me=',me,i,i_id(i)
  end do
  n_node = nxdim*nydim*nzdim

  if (inode.ne.num_loc_nodes) then
    write(6,*) me,' Huge error on local node list'
  endif
!
! set up connections
!
  dcnx_field = 0
  icnx_field = 0
  dcnx_2field = 0
  icnx_2field = 0
  dcnx_3field = 0
  icnx_3field = 0
!
  call add_cnx_dfield('area',idx)
  d_area => d_cnx_fld(idx)%p
  call add_cnx_dfield('distance',idx)
  d_dist => d_cnx_fld(idx)%p
  call add_cnx_dfield('distance_dn',idx)
  d_dist_dn => d_cnx_fld(idx)%p
  call add_cnx_dfield('distance_up',idx)
  d_dist_up => d_cnx_fld(idx)%p
  call add_cnx_dfield('fractional_distance',idx)
  d_fdis => d_cnx_fld(idx)%p
  call add_cnx_dfield('x_separation',idx)
  d_xsep => d_cnx_fld(idx)%p
  call add_cnx_dfield('y_separation',idx)
  d_ysep => d_cnx_fld(idx)%p
  call add_cnx_dfield('z_separation',idx)
  d_zsep => d_cnx_fld(idx)%p
!
  call add_cnx_dfield('cnx_x_cent',idx)
  c_xcen => d_cnx_fld(idx)%p
  call add_cnx_dfield('cnx_y_cent',idx)
  c_ycen => d_cnx_fld(idx)%p
  call add_cnx_dfield('cnx_z_cent',idx)
  c_zcen => d_cnx_fld(idx)%p
!
  call add_cnx_ifield('node_id1',idx)
  i_id1 => i_cnx_fld(idx)%p
  call add_cnx_ifield('node_id2',idx)
  i_id2 => i_cnx_fld(idx)%p
!  idim1 = 6
!  call add_node_i2field('nd2cnx',idim1,idx)
!  nd2cnx => i_nd_2fld(idx)%p
!  nd2cnx = 0
  call add_cnx_dfield('grvx',idx)
  grvx => d_cnx_fld(idx)%p
  grvx = 0.d0
  call add_cnx_dfield('tltx',idx)
  tltx => d_cnx_fld(idx)%p
  tltx = 0.d0
   allocate(nd2cnx(6,num_nodes))
   nd2cnx = 0
!
  icnt = 0
!
  if (ixmin.eq.1) then
    imin = 1
  else
    imin = gwidth
  endif
  if(gwidth == 1) then
    imax = iaxmax - iaxmin
  elseif(gwidth == 2) then
    if(ixmin.eq.1.and.ixmax.eq.nxdim) then
     imax=iaxmax-iaxmin
    elseif(ixmin.eq.1) then
      imax =iaxmax - iaxmin-1
    elseif(ixmax.eq.nxdim) then
      imax = imin+iaxmax - iaxmin-2
    else
      imax = imin + (iaxmax-iaxmin-3)
    endif
  endif
  if (iymin.eq.1) then
    jmin = 1
  else 
    jmin = gwidth+1
  endif
  jmax = jmin + iymax-iymin
  if (izmin.eq.1) then
    kmin = 1
  else 
    kmin = gwidth+1
  endif
  kmax = kmin + izmax-izmin
!  write(*,'(a,7I5)') 'x,me,imin,imax,jmin,jmax,kmin,kmax: ',me,imin,imax,jmin,jmax,kmin,kmax
  do k = kmin, kmax
    do j = jmin, jmax
      do i = imin, imax
        id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
        id2 = i + (j-1)*ldx + (k-1)*ldx*ldy + 1
        call parse_id(i_id(id1),ixx,iyy,izz)
        icnt = icnt + 1
!        if( nd2cnx(1,id1).eq.0 .and. i>imin)then 
!          nd2cnx(1,id1) = icnt
!        else
          nd2cnx(2,id1) = icnt
!        endif
!        if( nd2cnx(1,id2).eq.0 ) then
          nd2cnx(1,id2) = icnt
!        else
!          nd2cnx(2,id2) = icnt
!        endif
      if(ics /= 8 .AND. ics /= 3) then !BH
        if(ics.eq.2) then
          d_area(icnt) = x(ixx+1)*(y(iyy+1)-y(iyy))*(z(izz+1)-z(izz))
        else
        d_area(icnt) = (y(iyy+1)-y(iyy))*(z(izz+1)-z(izz))
        endif
      endif
        i_id1(icnt) = id1
        i_id2(icnt) = id2
       if(ics /= 8 .AND. ics /=3) then !BH
        rx = d_xc(id2) - d_xc(id1)
        ry = d_yc(id2) - d_yc(id1)
        rz = d_zc(id2) - d_zc(id1)
        r = sqrt(rx**2+ry**2+rz**2)
        d_xsep(icnt) = rx/r
        d_ysep(icnt) = ry/r
        d_zsep(icnt) = rz/r
        d_dist(icnt) = r
        rx = d_xc(id1)-x(ixx+1)
        ry = d_yc(id1)-0.5d0*(y(iyy)+y(iyy+1))
        rz = d_zc(id1)-0.5d0*(z(izz)+z(izz+1))
        d_dist_dn(icnt) = sqrt(rx**2+ry**2+rz**2)
        rx = d_xc(id2)-x(ixx+1)
        ry = d_yc(id2)-0.5d0*(y(iyy)+y(iyy+1))
        rz = d_zc(id2)-0.5d0*(z(izz)+z(izz+1))
        d_dist_up(icnt) = sqrt(rx**2+ry**2+rz**2)
        d_fdis(icnt) = 0.5d00
!        c_xcen(icnt) = x(ixx+1)
       elseif(ics==8) then !BH
        d_xsep(icnt) = 1.d0
        d_ysep(icnt) = 0.d0
        d_zsep(icnt) = 0.d0
        d_dist_dn(icnt) = dxg_p(2,id1) 
        d_dist_up(icnt) = dxg_p(1,id2)
        d_area(icnt) = afx(2,id1) 
        grvx(icnt) = grv_x(2,id1)
        tltx(icnt) = tlt_x(2,id1)
        grvpx(id1) = 0.5d0*(grv_x(1,id1)+grv_x(2,id1))
        grvpx(id2) = 0.5d0*(grv_x(1,id2)+grv_x(2,id2))
!BH
       else
        rx = d_xc(id2) - d_xc(id1)
        ry = d_yc(id2) - d_yc(id1)
        rz = d_zc(id2) - d_zc(id1)
        r = sqrt(rx**2+ry**2+rz**2)
        d_xsep(icnt) = rx/r
        d_ysep(icnt) = ry/r
        d_zsep(icnt) = rz/r
        d_dist(icnt) = r
        d_dist_dn(icnt) = dxg_p(2,id1)
        d_dist_up(icnt) = dxg_p(1,id2)
        d_area(icnt) = afx(2,id1)
        grvx(icnt) = grv_x(2,id1)
        tltx(icnt) = tlt_x(2,id1)
        grvpx(id1) = 0.5d0*(grv_x(1,id1)+grv_x(2,id1))
        grvpx(id2) = 0.5d0*(grv_x(1,id2)+grv_x(2,id2))
!BH
       endif
      end do
    end do
  end do
!
  if (ixmin.eq.1) then
    imin = 1
  else
    imin = gwidth+1
  endif
  imax = imin + ixmax-ixmin
  if (iymin.eq.1) then
    jmin = 1
  else 
    jmin = gwidth
  endif
  if(gwidth == 1) then
    jmax = iaymax - iaymin
  elseif(gwidth == 2) then
    if(iymin.eq.1.and.iymax.eq.nydim) then
     jmax=iaymax-iaymin
    elseif(iymin.eq.1) then
      jmax =iaymax - iaymin-1
    elseif(iymax.eq.nydim) then
      jmax = jmin+iaymax - iaymin-2
    else
      jmax = jmin + (iaymax-iaymin-3)
    endif
  endif
!  jmax = jmin + iymax-iymin - 1
!  if (iymax.lt.nydim) jmax = jmax + 1
!  jmax = iaymax-iaymin-gwidth+1
!  if(iymin.gt.1.and.iymax.lt.nydim) jmax = jmax - gwidth + 1  
  if (izmin.eq.1) then
    kmin = 1
  else 
    kmin = gwidth+1
  endif
  kmax = kmin + izmax-izmin
!  write(*,'(a,7I5)') 'y,me,imin,imax,jmin,jmax,kmin,kmax: ',me,imin,imax,jmin,jmax,kmin,kmax
  do k = kmin, kmax
    do j = jmin, jmax
      do i = imin, imax
        id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
        id2 = (i-1) + j*ldx + (k-1)*ldx*ldy + 1
        call parse_id(i_id(id1),ixx,iyy,izz)
        icnt = icnt + 1
!        if( nd2cnx(3,id1).eq.0 .and. j>jmin)then 
!          nd2cnx(3,id1) = icnt
!        else
          nd2cnx(4,id1) = icnt
!        endif
!        if( nd2cnx(3,id2).eq.0 ) then
          nd2cnx(3,id2) = icnt
!        else
!          nd2cnx(4,id2) = icnt
!        endif
        i_id1(icnt) = id1
        i_id2(icnt) = id2
       if(ics /= 8 .AND. ics /= 3) then !BH
        d_area(icnt) = (x(ixx+1)-x(ixx))*(z(izz+1)-z(izz))
        rx = d_xc(id2) - d_xc(id1)
        ry = d_yc(id2) - d_yc(id1)
        rz = d_zc(id2) - d_zc(id1)
        r = sqrt(rx**2+ry**2+rz**2)
        d_xsep(icnt) = rx/r
        d_ysep(icnt) = ry/r
        d_zsep(icnt) = rz/r
        d_dist(icnt) = r
        rx = d_xc(id1)-0.5d0*(x(ixx)+x(ixx+1))
        ry = d_yc(id1)-y(iyy+1)
        rz = d_zc(id1)-0.5d0*(z(izz)+z(izz+1))
        d_dist_dn(icnt) = sqrt(rx**2+ry**2+rz**2)
        rx = d_xc(id2)-0.5d0*(x(ixx)+x(ixx+1))
        ry = d_yc(id2)-y(iyy+1)
        rz = d_zc(id2)-0.5d0*(z(izz)+z(izz+1))
        d_dist_up(icnt) = sqrt(rx**2+ry**2+rz**2)
        d_fdis(icnt) = 0.5d00
       elseif(ics==8) then !BH
        d_xsep(icnt) = 0.d0
        d_ysep(icnt) = 1.d0
        d_zsep(icnt) = 0.d0
        d_dist_dn(icnt) = dxg_p(4,id1) 
        d_dist_up(icnt) = dxg_p(3,id2)
        d_area(icnt) = afx(4,id1) 
        grvx(icnt) = grv_x(4,id1)
        tltx(icnt) = tlt_x(4,id1)
        grvpy(id1) = 0.5d0*(grv_x(3,id1)+grv_x(4,id1))
        grvpy(id2) = 0.5d0*(grv_x(3,id2)+grv_x(4,id2))
!BH
       else
        rx = d_xc(id2) - d_xc(id1)
        ry = d_yc(id2) - d_yc(id1)
        rz = d_zc(id2) - d_zc(id1)
        r = sqrt(rx**2+ry**2+rz**2)
        d_xsep(icnt) = rx/r
        d_ysep(icnt) = ry/r
        d_zsep(icnt) = rz/r
        d_dist(icnt) = r
        d_dist_dn(icnt) = dxg_p(4,id1)
        d_dist_up(icnt) = dxg_p(3,id2)
        d_area(icnt) = afx(4,id1)
        grvx(icnt) = grv_x(4,id1)
        tltx(icnt) = tlt_x(4,id1)
        grvpy(id1) = 0.5d0*(grv_x(3,id1)+grv_x(4,id1))
        grvpy(id2) = 0.5d0*(grv_x(3,id2)+grv_x(4,id2))        
!BH       
       endif
      end do
    end do
  end do
!
  if (ixmin.eq.1) then
    imin = 1
  else
    imin = gwidth+1
  endif
  imax = imin + ixmax-ixmin
  if (iymin.eq.1) then
    jmin = 1
  else 
    jmin = gwidth+1
  endif
  jmax = jmin + iymax-iymin
  if (izmin.eq.1) then
    kmin = 1
  else 
    kmin = gwidth
  endif
  if(gwidth == 1) then
    kmax = iazmax - iazmin
  elseif(gwidth == 2) then
    if(izmin.eq.1.and.izmax.eq.nzdim) then
     kmax=iazmax-iazmin
    elseif(izmin.eq.1) then
      kmax =iazmax - iazmin-1
    elseif(izmax.eq.nzdim) then
      kmax = kmin+iazmax - iazmin-2
    else
      kmax = kmin + (iazmax-iazmin-3)
    endif
  endif
!  write(*,'(a,7I5)') 'z,me,imin,imax,jmin,jmax,kmin,kmax: ',me,imin,imax,jmin,jmax,kmin,kmax
  do k = kmin, kmax
    do j = jmin, jmax
      do i = imin, imax
        id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
        id2 = (i-1) + (j-1)*ldx + k*ldx*ldy + 1
        call parse_id(i_id(id1),ixx,iyy,izz)
        icnt = icnt + 1
!        if( nd2cnx(5,id1).eq.0 .and. k>kmin)then 
!          nd2cnx(5,id1) = icnt
!        else
          nd2cnx(6,id1) = icnt
!        endif
!        if( nd2cnx(5,id2).eq.0 ) then
          nd2cnx(5,id2) = icnt
!        else
!          nd2cnx(6,id2) = icnt
!        endif
        i_id1(icnt) = id1
        i_id2(icnt) = id2
       if(ics /= 8 .AND. ics /= 3) then !BH
        if(ics.eq.2) then
          d_area(icnt) = (x(ixx+1)*x(ixx+1)-x(ixx)*x(ixx))*(y(iyy+1)-y(iyy))/2.d0
        else
          d_area(icnt) = (x(ixx+1)-x(ixx))*(y(iyy+1)-y(iyy))
        endif
       endif

       if(ics /= 8 .AND. ics /= 3) then !BH
        rx = d_xc(id2) - d_xc(id1)
        ry = d_yc(id2) - d_yc(id1)
        rz = d_zc(id2) - d_zc(id1)
        r = sqrt(rx**2+ry**2+rz**2)
        d_xsep(icnt) = rx/r
        d_ysep(icnt) = ry/r
        d_zsep(icnt) = rz/r
        d_dist(icnt) = r
        rx = d_xc(id1)-0.5d0*(x(ixx)+x(ixx+1))
        ry = d_yc(id1)-0.5d0*(y(iyy)+y(iyy+1))
        rz = d_zc(id1)-z(izz+1)
        d_dist_dn(icnt) = sqrt(rx**2+ry**2+rz**2)
        rx = d_xc(id2)-0.5d0*(x(ixx)+x(ixx+1))
        ry = d_yc(id2)-0.5d0*(y(iyy)+y(iyy+1))
        rz = d_zc(id2)-z(izz+1)
        d_dist_up(icnt) = sqrt(rx**2+ry**2+rz**2)
        d_fdis(icnt) = 0.5d00
        grvx(icnt) = 9.81d0
       elseif(ics==8) then !BH
        d_xsep(icnt) = 0.d0
        d_ysep(icnt) = 0.d0
        d_zsep(icnt) = 1.d0
        d_dist_dn(icnt) = dxg_p(6,id1) 
        d_dist_up(icnt) = dxg_p(5,id2)
        d_area(icnt) = afx(6,id1) 
        grvx(icnt) = grv_x(6,id1)
        tltx(icnt) = tlt_x(6,id1)
        grvpz(id1) = 0.5d0*(grv_x(5,id1)+grv_x(6,id1))
        grvpz(id2) = 0.5d0*(grv_x(5,id2)+grv_x(6,id2))
!BH
       else
        rx = d_xc(id2) - d_xc(id1)
        ry = d_yc(id2) - d_yc(id1)
        rz = d_zc(id2) - d_zc(id1)
        r = sqrt(rx**2+ry**2+rz**2)
        d_xsep(icnt) = rx/r
        d_ysep(icnt) = ry/r
        d_zsep(icnt) = rz/r
        d_dist(icnt) = r
        d_dist_dn(icnt) = dxg_p(6,id1)
        d_dist_up(icnt) = dxg_p(5,id2)
        d_area(icnt) = afx(6,id1)
        grvx(icnt) = grv_x(6,id1)
        tltx(icnt) = tlt_x(6,id1)
        grvpz(id1) = 0.5d0*(grv_x(5,id1)+grv_x(6,id1))
        grvpz(id2) = 0.5d0*(grv_x(5,id2)+grv_x(6,id2))
!BH
       endif
      end do
    end do
  end do
!  write(*,*) 'me,num_cnx,icnt',me,num_cnx,icnt
  if (num_cnx.ne.icnt) then
    write(6,*) me,' Huge error calculating connections'
  endif
!
!  Set up boundary connections
!
  dbcnx_field = 0
  ibcnx_field = 0
  dbcnx_2field = 0
  ibcnx_2field = 0
  dbcnx_3field = 0
  ibcnx_3field = 0
!
  icnt = 0
!
  call add_bcnx_dfield('b_area',idx)
  d_barea => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_distance',idx)
  d_bdist => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_x_separation',idx)
  d_bxsep => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_y_separation',idx)
  d_bysep => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_z_separation',idx)
  d_bzsep => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_grvx',idx)
  b_grvx => d_bcnx_fld(idx)%p
  b_grvx = 0.d0
  call add_bcnx_dfield('b_tltx',idx)
  b_tltx => d_bcnx_fld(idx)%p
  b_tltx = 0.d0
!
  call add_bcnx_dfield('b_xcent',idx)
  d_bxcent => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_ycent',idx)
  d_bycent => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_zcent',idx)
  d_bzcent => d_bcnx_fld(idx)%p
!
  call add_bcnx_ifield('bcnx_id',idx)
  i_bid => i_bcnx_fld(idx)%p
!YFang
!boundary face id
  call add_bcnx_ifield('bcnxf_id',idx)
  i_bfid => i_bcnx_fld(idx)%p
!boundary i,j,k
  call add_bcnx_ifield('bcnxi_id',idx)
  i_bidi => i_bcnx_fld(idx)%p
  call add_bcnx_ifield('bcnxj_id',idx)
  i_bidj => i_bcnx_fld(idx)%p
  call add_bcnx_ifield('bcnxk_id',idx)
  i_bidk => i_bcnx_fld(idx)%p
 
!
  if (ixmin.eq.1.and.nxdim.ne.1) then
    imin = 1
    imax = 1
    if (iymin.eq.1) then
      jmin = 1
    else
!ghosted connection already accounted for, local numbering 1...	 
      jmin = gwidth + 1
    endif
    jmax = jmin + iymax-iymin
    if (izmin.eq.1) then
      kmin = 1
    else 
      kmin = gwidth + 1
    endif
    kmax = kmin + izmax-izmin
    do k = kmin, kmax
      do j = jmin, jmax
        do i = imin, imax
          id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
          call parse_id(i_id(id1),ixx,iyy,izz)
          icnt = icnt + 1
          i_bid(icnt) = id1
          i_bfid(icnt) = -1
          i_bidi(icnt) = i
          i_bidj(icnt) = j
          i_bidk(icnt) = k
         if(ics /= 8 .AND. ics /=3 ) then !BH 

          if(ics.eq.2) then
            d_barea(icnt) = x(ixx)*(y(iyy+1)-y(iyy))*(z(izz+1)-z(izz))
          else
            d_barea(icnt) = (y(iyy+1)-y(iyy))*(z(izz+1)-z(izz))
          endif
          d_bxcent(icnt) = x(ixx)
          d_bycent(icnt) = 0.5d0*(y(iyy)+y(iyy+1))
          d_bzcent(icnt) = 0.5d0*(z(izz)+z(izz+1))
           d_bdist(icnt) = d_xc(id1)-x(ixx)
         else
           d_barea(icnt) = afx(1,id1)
           d_bdist(icnt) = dxg_p(1,id1)
           b_grvx(icnt) = grv_x(1,id1)
           b_tltx(icnt) = tlt_x(1,id1)
         endif
          rx = -1.0d00
          ry = 0.0d00
          rz = 0.0d00
          r = sqrt(rx**2+ry**2+rz**2)
          d_bxsep(icnt) = rx/r
          d_bysep(icnt) = ry/r
          d_bzsep(icnt) = rz/r
        end do
      end do
    end do
  endif
  if (ixmax.eq.nxdim.and.nxdim.ne.1) then
    imin = ixmax - iaxmin + 1
    imax = ixmax - iaxmin + 1
    if (iymin.eq.1) then
      jmin = 1
    else 
      jmin = gwidth + 1
    endif
    jmax = jmin + iymax-iymin
    if (izmin.eq.1) then
      kmin = 1
    else 
      kmin = gwidth + 1
    endif
    kmax = kmin + izmax-izmin
    do k = kmin, kmax
      do j = jmin, jmax
        do i = imin, imax
          id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
          call parse_id(i_id(id1),ixx,iyy,izz)
          icnt = icnt + 1
          i_bid(icnt) = id1
          i_bfid(icnt) = 1
          i_bidi(icnt) = i
          i_bidj(icnt) = j
          i_bidk(icnt) = k
          if(ics /= 8 .AND. ics /= 3) then !BH
          if(ics.eq.2) then
            d_barea(icnt) = x(ixx)*(y(iyy+1)-y(iyy))*(z(izz+1)-z(izz))
          else
           d_barea(icnt) = (y(iyy+1)-y(iyy))*(z(izz+1)-z(izz))
          endif
          d_bxcent(icnt) = x(ixx+1)
          d_bycent(icnt) = 0.5d0*(y(iyy)+y(iyy+1))
          d_bzcent(icnt) = 0.5d0*(z(izz)+z(izz+1))
          d_bdist(icnt) = x(ixx+1) - d_xc(id1)
          else
           d_barea(icnt) = afx(2,id1)
           d_bdist(icnt) = dxg_p(2,id1)
           b_grvx(icnt) = grv_x(2,id1)
           b_tltx(icnt) = tlt_x(2,id1)
          endif
          rx = 1.0d00
          ry = 0.0d00
          rz = 0.0d00
          r = sqrt(rx**2+ry**2+rz**2)
          d_bxsep(icnt) = rx/r
          d_bysep(icnt) = ry/r
          d_bzsep(icnt) = rz/r
        end do
      end do
    end do
  endif
!
  if (iymin.eq.1.and.nydim.ne.1) then
    if (ixmin.eq.1) then
      imin = 1
    else 
      imin = gwidth + 1
    endif
    imax = imin + ixmax-ixmin
    jmin = 1
    jmax = 1
    if (izmin.eq.1) then
      kmin = 1
    else 
      kmin = gwidth + 1
    endif
    kmax = kmin + izmax-izmin
    do k = kmin, kmax
      do j = jmin, jmax
        do i = imin, imax
          id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
          call parse_id(i_id(id1),ixx,iyy,izz)
          icnt = icnt + 1
          i_bid(icnt) = id1
          i_bfid(icnt) = -2
          i_bidi(icnt) = i
          i_bidj(icnt) = j
          i_bidk(icnt) = k
          if(ics /= 8 .AND. ics /= 3) then !BH
          d_barea(icnt) = (x(ixx+1)-x(ixx))*(z(izz+1)-z(izz))
          d_bxcent(icnt) = 0.5d0*(x(ixx)+x(ixx+1))
          d_bycent(icnt) = y(iyy)
          d_bzcent(icnt) = 0.5d0*(z(izz)+z(izz+1))
          d_bdist(icnt) = d_yc(id1) - y(iyy)
          else
           d_barea(icnt) = afx(3,id1)
           d_bdist(icnt) = dxg_p(3,id1)
           b_grvx(icnt) = grv_x(3,id1)
           b_tltx(icnt) = tlt_x(3,id1)
          endif
          rx = 0.0d00
          ry = -1.0d00
          rz = 0.0d00
          r = sqrt(rx**2+ry**2+rz**2)
          d_bxsep(icnt) = rx/r
          d_bysep(icnt) = ry/r
          d_bzsep(icnt) = rz/r
        end do
      end do
    end do
  endif
  if (iymax.eq.nydim.and.nydim.ne.1) then
    if (ixmin.eq.1) then
      imin = 1
    else 
      imin = gwidth + 1
    endif
    imax = imin + ixmax-ixmin
    jmin = iymax - iaymin + 1
    jmax = iymax - iaymin + 1
    if (izmin.eq.1) then
      kmin = 1
    else 
      kmin = gwidth + 1
    endif
    kmax = kmin + izmax-izmin
    do k = kmin, kmax
      do j = jmin, jmax
        do i = imin, imax
          id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
          call parse_id(i_id(id1),ixx,iyy,izz)
          icnt = icnt + 1
          i_bid(icnt) = id1
          i_bfid(icnt) = 2
          i_bidi(icnt) = i
          i_bidj(icnt) = j
          i_bidk(icnt) = k
          if(ics /= 8.AND. ics /= 3 ) then !BH
          d_barea(icnt) = (x(ixx+1)-x(ixx))*(z(izz+1)-z(izz))
          d_bxcent(icnt) = 0.5d0*(x(ixx)+x(ixx+1))
          d_bycent(icnt) = y(iyy+1)
          d_bzcent(icnt) = 0.5d0*(z(izz)+z(izz+1))
          d_bdist(icnt) = y(iyy+1) - d_yc(id1)
          else
           d_barea(icnt) = afx(4,id1)
           d_bdist(icnt) = dxg_p(4,id1)
           b_grvx(icnt) = grv_x(4,id1)
           b_tltx(icnt) = tlt_x(4,id1)
          endif
          rx = 0.0d00
          ry = 1.0d00
          rz = 0.0d00
          r = sqrt(rx**2+ry**2+rz**2)
          d_bxsep(icnt) = rx/r
          d_bysep(icnt) = ry/r
          d_bzsep(icnt) = rz/r
        end do
      end do
    end do
  endif
!
  if (izmin.eq.1.and.nzdim.ne.1) then
    if (ixmin.eq.1) then
      imin = 1
    else 
      imin = gwidth + 1
    endif
    imax = imin + ixmax-ixmin
    if (iymin.eq.1) then
      jmin = 1
    else 
      jmin = gwidth + 1
    endif
    jmax = jmin + iymax-iymin
    kmin = 1
    kmax = 1
    do k = kmin, kmax
      do j = jmin, jmax
        do i = imin, imax
          id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
          call parse_id(i_id(id1),ixx,iyy,izz)
          icnt = icnt + 1
          i_bid(icnt) = id1
          i_bfid(icnt) = -3
          i_bidi(icnt) = i
          i_bidj(icnt) = j
          i_bidk(icnt) = k
          if(ics /= 8 .AND. ics /= 3) then !BH
          if(ics.eq.2) then
            d_barea(icnt) = (x(ixx+1)*x(ixx+1)-x(ixx)*x(ixx))*(y(iyy+1)-y(iyy))/2.d0
          else
           d_barea(icnt) = (x(ixx+1)-x(ixx))*(y(iyy+1)-y(iyy))
          endif
          d_bxcent(icnt) = 0.5d0*(x(ixx)+x(ixx+1))
          d_bycent(icnt) = 0.5d0*(y(iyy)+y(iyy+1))
          d_bzcent(icnt) = z(izz)
          d_bdist(icnt) = d_zc(id1) - z(izz)
          b_grvx(icnt) = 9.81d0
          else
           d_barea(icnt) = afx(5,id1)
           d_bdist(icnt) = dxg_p(5,id1)
           b_grvx(icnt) = grv_x(5,id1)
           b_tltx(icnt) = tlt_x(5,id1)
          endif
         if (ics /= 3) then !BH
          rx = 0.0d00
          ry = 0.0d00
          rz = -1.0d00
          r = sqrt(rx**2+ry**2+rz**2)
          d_bxsep(icnt) = rx/r
          d_bysep(icnt) = ry/r
          d_bzsep(icnt) = rz/r
!BH
         else
!---        Bottom surface centroid  ---             
          XVX(1) = XBF(Ixx,iyy,izz)
          XVX(2) = XBF(Ixx+1,iyy,izz)
          XVX(3) = XBF(Ixx+1,iyy+1,izz)
          XVX(4) = XBF(Ixx,iyy+1,izz)
          YVX(1) = YBF(Ixx,iyy,izz)
          YVX(2) = YBF(Ixx+1,iyy,izz)
          YVX(3) = YBF(Ixx+1,iyy+1,izz)
          YVX(4) = YBF(Ixx,iyy+1,izz)
          ZVX(1) = ZBF(Ixx,iyy,izz)
          ZVX(2) = ZBF(Ixx+1,iyy,izz)
          ZVX(3) = ZBF(Ixx+1,iyy+1,izz)
          ZVX(4) = ZBF(Ixx,iyy+1,izz)
          NP = 4
          CALL PGCNTRD( NP,XVX,YVX,ZVX,XFB,YFB,ZFB )
          rx = XFB - d_xc(id1)
          ry = YFB - d_yc(id1)
          rz = ZFB - d_zc(id1)
          r = sqrt(rx**2+ry**2+rz**2)
          d_bxsep(icnt) = rx/r
          d_bysep(icnt) = ry/r
          d_bzsep(icnt) = rz/r
         endif
!BH
        end do
      end do
    end do
  endif
  if (izmax.eq.nzdim.and.nzdim.ne.1) then
    if (ixmin.eq.1) then
      imin = 1
    else 
      imin = gwidth + 1
    endif
    imax = imin + ixmax-ixmin
    if (iymin.eq.1) then
      jmin = 1
    else 
      jmin = gwidth + 1
    endif
    jmax = jmin + iymax-iymin
    kmin = izmax - iazmin + 1
    kmax = izmax - iazmin + 1
    do k = kmin, kmax
      do j = jmin, jmax
        do i = imin, imax
          id1 = (i-1) + (j-1)*ldx + (k-1)*ldx*ldy + 1
          call parse_id(i_id(id1),ixx,iyy,izz)
          icnt = icnt + 1
          i_bid(icnt) = id1
          i_bfid(icnt) = 3
          i_bidi(icnt) = i
          i_bidj(icnt) = j
          i_bidk(icnt) = k
          if(ics /= 8 .AND. ics /= 3 ) then !BH
          if(ics.eq.2) then
            d_barea(icnt) = (x(ixx+1)*x(ixx+1)-x(ixx)*x(ixx))*(y(iyy+1)-y(iyy))/2.d0
          else
            d_barea(icnt) = (x(ixx+1)-x(ixx))*(y(iyy+1)-y(iyy))
          endif
          d_bxcent(icnt) = 0.5d0*(x(ixx)+x(ixx+1))
          d_bycent(icnt) = 0.5d0*(y(iyy)+y(iyy+1))
          d_bzcent(icnt) = z(izz+1)
          d_bdist(icnt) = z(izz+1) - d_zc(id1)
          b_grvx(icnt) = 9.81d0
          else
           d_barea(icnt) = afx(6,id1)
           d_bdist(icnt) = dxg_p(6,id1)
           b_grvx(icnt) = grv_x(6,id1)
           b_tltx(icnt) = tlt_x(6,id1)
          endif
          if (ics /= 3) then
           rx = 0.0d00
           ry = 0.0d00
           rz = 1.0d00
           r = sqrt(rx**2+ry**2+rz**2)
           d_bxsep(icnt) = rx/r
           d_bysep(icnt) = ry/r
           d_bzsep(icnt) = rz/r
!BH
          else
!---        Top surface centroid  ---
           XVX(1) = XBF(Ixx,iyy,izz+1)
           XVX(2) = XBF(Ixx+1,iyy,izz+1)
           XVX(3) = XBF(Ixx+1,iyy+1,izz+1)
           XVX(4) = XBF(Ixx,iyy+1,izz+1)
           YVX(1) = YBF(Ixx,iyy,izz+1)
           YVX(2) = YBF(Ixx+1,iyy,izz+1)
           YVX(3) = YBF(Ixx+1,iyy+1,izz+1)
           YVX(4) = YBF(Ixx,iyy+1,izz+1)
           ZVX(1) = ZBF(Ixx,iyy,izz+1)
           ZVX(2) = ZBF(Ixx+1,iyy,izz+1)
           ZVX(3) = ZBF(Ixx+1,iyy+1,izz+1)
           ZVX(4) = ZBF(Ixx,iyy+1,izz+1)
           NP = 4
           CALL PGCNTRD( NP,XVX,YVX,ZVX,XFT,YFT,ZFT )
           rx = XFT - d_xc(id1)
           ry = YFT - d_yc(id1)
           rz = ZFT - d_zc(id1)
           r = sqrt(rx**2+ry**2+rz**2)
           d_bxsep(icnt) = rx/r
           d_bysep(icnt) = ry/r
           d_bzsep(icnt) = rz/r    
          endif
!BH
        end do
      end do
    end do
  endif
  if (num_bcnx.ne.icnt) then
    write(6,*) me,' Huge error calculating boundary connections'
  endif
!mlr
!#ifdef USE_E4D
!--- E4D PATCH
!  IF (.NOT.GAE4D) THEN
!#endif
    if(ics /= 8 .AND. ics /= 3 ) then !BH
      if(allocated(x)) deallocate(x)
      if(allocated(y)) deallocate(y)
      if(allocated(z)) deallocate(z)
!#ifdef USE_E4D
    else
!BH      if(allocated(xbf)) deallocate(xbf)
!BH      if(allocated(ybf)) deallocate(ybf)
!BH      if(allocated(zbf)) deallocate(zbf)
!#endif
    endif
!        write(*,*) 'END GRID_INIT'
!#ifdef USE_E4D
!  ENDIF
!#endif
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.grid_init)
end subroutine grid_init

subroutine grid_timer_initialize
   use grid_mod
   implicit none

#include "mafdecls.fh"
#include "global.fh"

   integer nproc

   petsc_init_time =0.d0
   jcbp_time = 0.d0
   incrm_time = 0.d0
   stomp_time = 0.d0
   total_time = 0.d0
   smc_time = 0.d0
   tmpr_time = 0.d0
   bcp_time = 0.d0
   bcj_time = 0.d0
   cisc_time = 0.d0
   drcvl_time = 0.d0
   bcf_time = 0.d0
   ldo_time = 0.d0
   average_time = 0.d0
   tmstep_time = 0.d0
   sorc_time = 0.d0
   jcbwl_time = 0.d0
   updt_time = 0.d0
   rsdl_time=0.d0
   setup_time=0.d0
!  initial_condition_time=0.d0
   sync_iter1 = 0
   sync_iter =0

   nproc = GA_NNODES()
   allocate(sync_time(nproc))
   allocate(sync_time1(nproc))
   sync_time1 = 0.d0
   eckechem_time = 0.d0
   sync_time = 0.d0

end subroutine grid_timer_initialize

subroutine grid_timer_terminate
   use grid_mod
   implicit none

#include "mafdecls.fh"
#include "global.fh"

   double precision ga_time_min, ga_time_max
   integer nproc

   nproc = GA_NNODES()
   call ga_dgop(1,sync_time,nproc,'+')
   call ga_dgop(1,sync_time1,nproc,'+')
   ga_time_min = ga_time
   ga_time_max = ga_time 
   call ga_dgop(1,ga_time_min,1,'min')
   call ga_dgop(1,ga_time_max,1,'max')
   if(ga_nodeid() .eq.0) then
      print *,''
      print *,'ga_init_time       : ',ga_init_time
      print *,'jcbp_time          : ',jcbp_time
      print *,'petsc_init_time    : ',petsc_init_time
      print *,'petsc_sinit_time   : ',petsc_sinit_time
      print *,'chk_time           : ',chk_time
      print *,'incrm_time         : ',incrm_time
      print *,'stomp_time         : ',stomp_time
      print *,'smc_time           : ',smc_time
      print *,'tmpr_time          : ',tmpr_time
      print *,'bcp_time           : ',bcp_time
      print *,'bcj_time           : ',bcj_time
      print *,'cisc_time          : ',cisc_time
      print *,'drcvl_time         : ',drcvl_time
      print *,'bcf_time           : ',bcf_time
      print *,'ldo_time           : ',ldo_time
      print *,'average_time       : ',average_time
      print *,'tmstep_time        : ',tmstep_time
      print *,'sorc_time          : ',sorc_time
      print *,'jcbwl_time         : ',jcbwl_time
      print *,'updt_time          : ',updt_time
      print *,'rsdl_time          : ',rsdl_time
      print *,'petsc_iter         : ',petsc_iter
      print *,'stomp_iter         : ',stomp_iter
      print *,'ga_time            : ',ga_time
      print *,'petsc_time         : ',petsc_time
      print *,'ga_time_min        : ',ga_time_min
      print *,'ga_time_max        : ',ga_time_max
      print *,'eckechem_time      : ',eckechem_time
!     print *,'initial_cond_time  : ',initial_condition_time
      print *,'setup_time         : ',setup_time
      print *,'total_time         : ',total_time
      !print *,'sync1              : ',sync_iter1
      !print *,'sync               : ',sync_iter
      !do i=1,nproc
      !print *,'me=',i-1,sync_time1(i),sync_time(i)
      !enddo
      !      print *,'ga_time',ga_time,'petsc_time',petsc_time
      !      print *,'stomp_iter',stomp_iter,'petsc_iter',petsc_iter
   endif
end subroutine grid_timer_terminate

! Method:  get_node_dfield[]
subroutine get_node_dfield(t_string, t_field, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_dfield.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_dfield.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:)
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_dfield)
! Insert-Code-Here {stmpgrid.GAGrid.get_node_dfield} (get_node_dfield method)
! 
  integer i, slen, nlen, grid_clen
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dnode_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_nd_fld_names(i))
    if (t_string(1:slen).eq.d_nd_fld_names(i)(1:nlen)) then
      t_field => d_nd_fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_dfield)
end subroutine get_node_dfield


! 
! Method:  get_node_d2field[]
! 

subroutine get_node_d2field(t_string, t_field,  &
  t_dim1, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_d2field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_d2field.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:,:) ! out
  integer :: t_dim1 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_d2field)
! Insert-Code-Here {stmpgrid.GAGrid.get_node_d2field} (get_node_d2field method)
! 
  integer i, slen, nlen, grid_clen, dim1
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dnode_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_nd_2fld_names(i))
    if (t_string(1:slen).eq.d_nd_2fld_names(i)(1:nlen)) then
      dim1 = d_nd_2dim1(i)
      t_dim1 = dim1
!      call borrow(d_nd_2fld(i)%p(1,1),t_lower,t_upper,t_stride,t_field)
      t_field => d_nd_2fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_d2field)
end subroutine get_node_d2field


! 
! Method:  get_node_d3field[]
! 

subroutine get_node_d3field(t_string, t_field,  &
  t_dim1, t_dim2, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_d3field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_d3field.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:,:,:) ! out
  integer :: t_dim1 ! out
  integer :: t_dim2 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_d3field)
! Insert-Code-Here {stmpgrid.GAGrid.get_node_d3field} (get_node_d3field method)
! 
  integer i, slen, nlen, grid_clen, dim1, dim2
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dnode_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_nd_3fld_names(i))
    if (t_string(1:slen).eq.d_nd_3fld_names(i)(1:nlen)) then
      dim1 = d_nd_3dim1(i)
      dim2 = d_nd_3dim2(i)
!      call borrow(d_nd_3fld(i)%p(1,1,1),t_lower,t_upper,t_stride,t_field)
      t_field => d_nd_3fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_d3field)
end subroutine get_node_d3field


! 
! Method:  get_node_ifield[]
! 

subroutine get_node_ifield(t_string, t_field,  &
  t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_ifield.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_ifield.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:) ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_ifield)
! Insert-Code-Here {stmpgrid.GAGrid.get_node_ifield} (get_node_ifield method)
! 
  integer i, slen, nlen, grid_clen
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.inode_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_nd_fld_names(i))
    if (t_string(1:slen).eq.i_nd_fld_names(i)(1:nlen)) then
!      call borrow(i_nd_fld(i)%p(1),t_lower,t_upper,t_lower,t_field)
      t_field => i_nd_fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_ifield)
end subroutine get_node_ifield


! 
! Method:  get_node_i2field[]
! 

subroutine get_node_i2field(t_string, t_field,  &
  t_dim1, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_i2field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_i2field.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:,:) ! out
  integer :: t_dim1 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_i2field)
! Insert-Code-Here {stmpgrid.GAGrid.get_node_i2field} (get_node_i2field method)
! 
  integer i, slen, nlen, grid_clen, dim1
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.inode_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_nd_2fld_names(i))
    if (t_string(1:slen).eq.i_nd_2fld_names(i)(1:nlen)) then
      dim1 = i_nd_2dim1(i)
      t_dim1 = dim1
!      call borrow(i_nd_2fld(i)%p(1,1),t_lower,t_upper,t_stride,t_field)
      t_field => i_nd_2fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_i2field)
end subroutine get_node_i2field


! 
! Method:  get_node_i3field[]
! 

subroutine get_node_i3field(t_string, t_field,  &
  t_dim1, t_dim2, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_i3field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_i3field.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:,:,:) ! out
  integer :: t_dim1 ! out
  integer :: t_dim2 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_i3field)
! Insert-Code-Here {stmpgrid.GAGrid.get_node_i3field} (get_node_i3field method)
! 
  integer i, slen, nlen, grid_clen, dim1, dim2
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.inode_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_nd_3fld_names(i))
    if (t_string(1:slen).eq.i_nd_3fld_names(i)(1:nlen)) then
      dim1 = i_nd_3dim1(i)
      dim2 = i_nd_3dim2(i)
      t_dim1 = dim1
!      call borrow(i_nd_3fld(i)%p(1,1,1),t_lower,t_upper,t_stride,t_field)
      t_field => i_nd_3fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_i3field)
end subroutine get_node_i3field

! 
! 
! 
! 
! Method:  get_node_mask[]
! Returns a list that signifies whether nodes are local or ghost,
! or whether they are used at all
! @param t_mask: signifies node status: local, ghost, unused
! 

subroutine get_node_mask(t_mask)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_mask.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_mask.use)
  implicit none
  integer, pointer :: t_mask(:) ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_node_mask)
! Insert-Code-Here {stmpgrid.GAGrid.get_node_mask} (get_node_mask method)
! 
!  call borrow(grid_mask(1),t_lower,t_upper,t_lower,t_mask)
  t_mask => grid_mask
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_node_mask)
end subroutine get_node_mask
! 
! Method:  get_cnx_dfield[]
! Find connection data corresponding to an existing field name.
! @param t_string: name of field
! @param t_field: pointer to local field data
! @param t_dim1: size of first dimension
! @param t_dim2: size of second dimension
! @param t_ok: returns true if field exists, false otherwise
! 

subroutine get_cnx_dfield( t_string, t_field,  &
  t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_dfield.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_dfield.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:) ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_dfield)
! Insert-Code-Here {stmpgrid.GAGrid.get_cnx_dfield} (get_cnx_dfield method)
! 
  integer i, slen, nlen, grid_clen
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dcnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_cnx_fld_names(i))
    if (t_string(1:slen).eq.d_cnx_fld_names(i)(1:nlen)) then
!      call borrow(d_cnx_fld(i)%p(1),t_lower,t_upper,t_lower,t_field)
      t_field => d_cnx_fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_dfield)
end subroutine get_cnx_dfield


! 
! Method:  get_cnx_d2field[]
! 

subroutine get_cnx_d2field(t_string, t_field,  &
  t_dim1, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_d2field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_d2field.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:,:) ! out
  integer :: t_dim1 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_d2field)
! Insert-Code-Here {stmpgrid.GAGrid.get_cnx_d2field} (get_cnx_d2field method)
! 
  integer i, slen, nlen, grid_clen, dim1
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dcnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_cnx_2fld_names(i))
    if (t_string(1:slen).eq.d_cnx_2fld_names(i)(1:nlen)) then
      dim1 = d_cnx_2dim1(i)
      t_dim1 = dim1
!      call borrow(d_cnx_2fld(i)%p(1,1),t_lower,t_upper,t_stride,t_field)
      t_field => d_cnx_2fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_d2field)
end subroutine get_cnx_d2field

! 
! Method:  get_cnx_d3field[]
! 

subroutine get_cnx_d3field(t_string, t_field,  &
  t_dim1, t_dim2, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_d3field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_d3field.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:,:,:) ! out
  integer :: t_dim1 ! out
  integer :: t_dim2 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_d3field)
! Insert-Code-Here {stmpgrid.GAGrid.get_cnx_d3field} (get_cnx_d3field method)
! 
  integer i, slen, nlen, grid_clen, dim1, dim2
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dcnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_cnx_3fld_names(i))
    if (t_string(1:slen).eq.d_cnx_3fld_names(i)(1:nlen)) then
      dim1 = d_cnx_3dim1(i)
      dim2 = d_cnx_3dim2(i)
      t_dim1 = dim1
!      call borrow(d_cnx_3fld(i)%p(1,1,1),t_lower,t_upper,t_stride,t_field)
      t_field => d_cnx_3fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_d3field)
end subroutine get_cnx_d3field

! 
! Method:  get_cnx_ifield[]
! 

subroutine get_cnx_ifield(t_string, t_field,  &
  t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_ifield.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_ifield.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:) ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_ifield)
! Insert-Code-Here {stmpgrid.GAGrid.get_cnx_ifield} (get_cnx_ifield method)
! 
  integer i, slen, nlen, grid_clen
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.icnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_cnx_fld_names(i))
    if (t_string(1:slen).eq.i_cnx_fld_names(i)(1:nlen)) then
!      call borrow(i_cnx_fld(i)%p(1),t_lower,t_upper,t_lower,t_field)
      t_field => i_cnx_fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_ifield)
end subroutine get_cnx_ifield


! 
! Method:  get_cnx_i2field[]
! 

subroutine get_cnx_i2field( t_string, t_field,  &
  t_dim1, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_i2field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_i2field.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:,:) ! out
  integer :: t_dim1 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_i2field)
! Insert-Code-Here {stmpgrid.GAGrid.get_cnx_i2field} (get_cnx_i2field method)
! 
  integer i, slen, nlen, grid_clen, dim1
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.icnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_cnx_2fld_names(i))
    if (t_string(1:slen).eq.i_cnx_2fld_names(i)(1:nlen)) then
      dim1 = i_cnx_2dim1(i)
      t_dim1 = dim1
!      call borrow(i_cnx_2fld(i)%p(1,1),t_lower,t_upper,t_stride,t_field)
      t_field => i_cnx_2fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_i2field)
end subroutine get_cnx_i2field


! 
! Method:  get_cnx_i3field[]
! 

subroutine get_cnx_i3field(t_string, t_field,  &
  t_dim1, t_dim2, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_i3field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_i3field.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:,:,:) ! out
  integer :: t_dim1 ! out
  integer :: t_dim2 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_cnx_i3field)
! Insert-Code-Here {stmpgrid.GAGrid.get_cnx_i3field} (get_cnx_i3field method)
! 
  integer i, slen, nlen, grid_clen, dim1, dim2
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.icnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_cnx_3fld_names(i))
    if (t_string(1:slen).eq.i_cnx_3fld_names(i)(1:nlen)) then
      dim1 = i_cnx_3dim1(i)
      dim2 = i_cnx_3dim2(i)
      t_dim1 = dim1
!      call borrow(i_cnx_3fld(i)%p(1,1,1),t_lower,t_upper,t_stride,t_field)
      t_field => i_cnx_3fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_cnx_i3field)
end subroutine get_cnx_i3field


! Method:  get_bcnx_dfield[]
! Find boundary connection data corresponding to an existing field name.
! @param t_string: name of field
! @param t_field: pointer to local field data
! @param t_dim1: size of first dimension
! @param t_dim2: size of second dimension
! @param t_ok: returns true if field exists, false otherwise
subroutine get_bcnx_dfield( t_string, t_field,  &
  t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_dfield.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_dfield.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:) ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_dfield)
! Insert-Code-Here {stmpgrid.GAGrid.get_bcnx_dfield} (get_bcnx_dfield method)
! 
  integer i, slen, nlen, grid_clen
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dbcnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_bcnx_fld_names(i))
    if (t_string(1:slen).eq.d_bcnx_fld_names(i)(1:nlen)) then
!      call borrow(d_bcnx_fld(i)%p(1),t_lower,t_upper,t_lower,t_field)
      t_field => d_bcnx_fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_dfield)
end subroutine get_bcnx_dfield


! 
! Method:  get_bcnx_d2field[]
! 

subroutine get_bcnx_d2field(t_string, t_field,  &
  t_dim1, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_d2field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_d2field.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:,:) ! out
  integer :: t_dim1 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_d2field)
! Insert-Code-Here {stmpgrid.GAGrid.get_bcnx_d2field} (get_bcnx_d2field method)
! 
  integer i, slen, nlen, grid_clen, dim1
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dbcnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_bcnx_2fld_names(i))
    if (t_string(1:slen).eq.d_bcnx_2fld_names(i)(1:nlen)) then
      dim1 = d_bcnx_2dim1(i)
      t_dim1 = dim1
!      call borrow(d_bcnx_2fld(i)%p(1,1),t_lower,t_upper,t_stride,t_field)
      t_field => d_bcnx_2fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_d2field)
end subroutine get_bcnx_d2field


! 
! Method:  get_bcnx_d3field[]
! 

recursive subroutine get_bcnx_d3field(t_string, t_field,  &
  t_dim1, t_dim2, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_d3field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_d3field.use)
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:,:,:) ! out
  integer :: t_dim1 ! out
  integer :: t_dim2 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_d3field)
! Insert-Code-Here {stmpgrid.GAGrid.get_bcnx_d3field} (get_bcnx_d3field method)
! 
  integer i, slen, nlen, grid_clen, dim1, dim2
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dbcnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_bcnx_3fld_names(i))
    if (t_string(1:slen).eq.d_bcnx_3fld_names(i)(1:nlen)) then
      dim1 = d_bcnx_3dim1(i)
      dim2 = d_bcnx_3dim2(i)
      t_dim1 = dim1
!      call borrow(d_bcnx_3fld(i)%p(1,1,1),t_lower,t_upper,t_stride,t_field)
      t_field => d_bcnx_3fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_d3field)
end subroutine get_bcnx_d3field


! 
! Method:  get_bcnx_ifield[]
! 

subroutine get_bcnx_ifield(t_string, t_field,  &
  t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_ifield.use)
  use grid_mod
 ! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_ifield.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:) ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_ifield)
! Insert-Code-Here {stmpgrid.GAGrid.get_bcnx_ifield} (get_bcnx_ifield method)
! 
  integer i, slen, nlen, grid_clen
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
!  do while (i.lt.dbcnx_field.and.(.not.t_ok))
  do while (i.lt.ibcnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_bcnx_fld_names(i))
    if (t_string(1:slen).eq.i_bcnx_fld_names(i)(1:nlen)) then
!      call borrow(i_bcnx_fld(i)%p(1),t_lower,t_upper,t_lower,t_field)
      t_field => i_bcnx_fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_ifield)
end subroutine get_bcnx_ifield


! 
! Method:  get_bcnx_i2field[]
! 

subroutine get_bcnx_i2field(t_string, t_field,  &
  t_dim1, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_i2field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_i2field.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:,:) ! out
  integer :: t_dim1 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_i2field)
! Insert-Code-Here {stmpgrid.GAGrid.get_bcnx_i2field} (get_bcnx_i2field method)
! 
  integer i, slen, nlen, grid_clen, dim1
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.ibcnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_bcnx_2fld_names(i))
    if (t_string(1:slen).eq.i_bcnx_2fld_names(i)(1:nlen)) then
      dim1 = i_bcnx_2dim1(i)
      t_dim1 = dim1
!      call borrow(i_bcnx_2fld(i)%p(1,1),t_lower,t_upper,t_stride,t_field)
      t_field => i_bcnx_2fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_i2field)
end subroutine get_bcnx_i2field


! 
! Method:  get_bcnx_i3field[]
! 

subroutine get_bcnx_i3field(t_string, t_field,  &
  t_dim1, t_dim2, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_i3field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_i3field.use)
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:,:,:) ! out
  integer :: t_dim1 ! out
  integer :: t_dim2 ! out
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_bcnx_i3field)
! Insert-Code-Here {stmpgrid.GAGrid.get_bcnx_i3field} (get_bcnx_i3field method)
! 
  integer i, slen, nlen, grid_clen, dim1, dim2
  t_ok = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.ibcnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_bcnx_3fld_names(i))
    if (t_string(1:slen).eq.i_bcnx_3fld_names(i)(1:nlen)) then
      dim1 = i_bcnx_3dim1(i)
      dim2 = i_bcnx_3dim2(i)
      t_dim1 = dim1
!      call borrow(i_bcnx_3fld(i)%p(1,1,1),t_lower,t_upper,t_stride,t_field)
      t_field => i_bcnx_3fld(i)%p
      t_ok = .true.
    endif
  end do
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_bcnx_i3field)
end subroutine get_bcnx_i3field

! 
! Method:  get_local_node_list[]
! Returns a list containing the indices of the locally held
! nodes.
! @param t_list: list of local node indices
subroutine get_local_node_list(t_list)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_local_node_list.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_local_node_list.use)
  implicit none
  integer, pointer :: t_list(:) ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.get_local_node_list)
! Insert-Code-Here {stmpgrid.GAGrid.get_local_node_list} (get_local_node_list method)
! 
!  call borrow(local_node_list(1),t_lower,t_upper,t_lower,t_list)
  t_list => local_node_list
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.get_local_node_list)
end subroutine get_local_node_list

! 
! Method:  delete_node_field[]
! Remove field from nodes
! @param t_string: name of field
! @param t_ok: verify that field was found (and deleted)
! 

subroutine delete_node_field(t_string, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.delete_node_field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.delete_node_field.use)
  implicit none
  character (len=*) :: t_string ! in
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.delete_node_field)
! Insert-Code-Here {stmpgrid.GAGrid.delete_node_field} (delete_node_field method)
! 
  integer i, idx, slen, nlen, grid_clen, dim1, dim2
  logical t_done
  t_ok = .false.
  t_done = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dnode_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_nd_fld_names(i))
    if (t_string(1:slen).eq.d_nd_fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dnode_field-1
      d_nd_fld(i)%p => d_nd_fld(i+1)%p
      d_nd_fld_names(i) = d_nd_fld_names(i+1)
    end do
    dnode_field = dnode_field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.dnode_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_nd_2fld_names(i))
    if (t_string(1:slen).eq.d_nd_2fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dnode_2field-1
      d_nd_2fld(i)%p => d_nd_2fld(i+1)%p
      d_nd_2fld_names(i) = d_nd_2fld_names(i+1)
      d_nd_2dim1(i) = d_nd_2dim1(i+1)
    end do
    dnode_2field = dnode_2field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.dnode_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_nd_3fld_names(i))
    if (t_string(1:slen).eq.d_nd_3fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dnode_3field-1
      d_nd_3fld(i)%p => d_nd_3fld(i+1)%p
      d_nd_3fld_names(i) = d_nd_3fld_names(i+1)
      d_nd_3dim1(i) = d_nd_3dim1(i+1)
      d_nd_3dim2(i) = d_nd_3dim2(i+1)
    end do
    dnode_3field = dnode_3field - 1
    t_done = .true.
  endif
!
  do while (i.lt.inode_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_nd_fld_names(i))
    if (t_string(1:slen).eq.i_nd_fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, inode_field-1
      i_nd_fld(i)%p => i_nd_fld(i+1)%p
      i_nd_fld_names(i) = i_nd_fld_names(i+1)
    end do
    inode_field = inode_field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.inode_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_nd_2fld_names(i))
    if (t_string(1:slen).eq.i_nd_2fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, inode_2field-1
      i_nd_2fld(i)%p => i_nd_2fld(i+1)%p
      i_nd_2fld_names(i) = i_nd_2fld_names(i+1)
      i_nd_2dim1(i) = i_nd_2dim1(i+1)
    end do
    inode_2field = inode_2field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.inode_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_nd_3fld_names(i))
    if (t_string(1:slen).eq.i_nd_3fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, inode_3field-1
      i_nd_3fld(i)%p => i_nd_3fld(i+1)%p
      i_nd_3fld_names(i) = i_nd_3fld_names(i+1)
      i_nd_3dim1(i) = i_nd_3dim1(i+1)
      i_nd_3dim2(i) = i_nd_3dim2(i+1)
    end do
    inode_3field = inode_3field - 1
    t_done = .true.
  endif
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.delete_node_field)
end subroutine delete_node_field

! 
! Method:  delete_cnx_field[]
! Remove field from connections
! @param t_string: name of field
! @param t_ok: verify that field was found (and deleted)
! 

subroutine delete_cnx_field(t_string, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.delete_cnx_field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.delete_cnx_field.use)
  implicit none
  character (len=*) :: t_string ! in
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.delete_cnx_field)
! Insert-Code-Here {stmpgrid.GAGrid.delete_cnx_field} (delete_cnx_field method)
! 
  integer i, idx, slen, nlen, grid_clen, dim1, dim2
  logical t_done
  t_ok = .false.
  t_done = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dcnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_cnx_fld_names(i))
    if (t_string(1:slen).eq.d_cnx_fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dcnx_field-1
      d_cnx_fld(i)%p => d_cnx_fld(i+1)%p
      d_cnx_fld_names(i) = d_cnx_fld_names(i+1)
    end do
    dcnx_field = dcnx_field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.dcnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_cnx_2fld_names(i))
    if (t_string(1:slen).eq.d_cnx_2fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dcnx_2field-1
      d_cnx_2fld(i)%p => d_cnx_2fld(i+1)%p
      d_cnx_2fld_names(i) = d_cnx_2fld_names(i+1)
      d_cnx_2dim1(i) = d_cnx_2dim1(i+1)
    end do
    dcnx_2field = dcnx_2field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.dcnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_cnx_3fld_names(i))
    if (t_string(1:slen).eq.d_cnx_3fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dcnx_3field-1
      d_cnx_3fld(i)%p => d_cnx_3fld(i+1)%p
      d_cnx_3fld_names(i) = d_cnx_3fld_names(i+1)
      d_cnx_3dim1(i) = d_cnx_3dim1(i+1)
      d_cnx_3dim2(i) = d_cnx_3dim2(i+1)
    end do
    dcnx_3field = dcnx_3field - 1
    t_done = .true.
  endif
!
  do while (i.lt.icnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_cnx_fld_names(i))
    if (t_string(1:slen).eq.i_cnx_fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, icnx_field-1
      i_cnx_fld(i)%p => i_cnx_fld(i+1)%p
      i_cnx_fld_names(i) = i_cnx_fld_names(i+1)
    end do
    icnx_field = icnx_field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.icnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_cnx_2fld_names(i))
    if (t_string(1:slen).eq.i_cnx_2fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, icnx_2field-1
      i_cnx_2fld(i)%p => i_cnx_2fld(i+1)%p
      i_cnx_2fld_names(i) = i_cnx_2fld_names(i+1)
      i_cnx_2dim1(i) = i_cnx_2dim1(i+1)
    end do
    icnx_2field = icnx_2field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.icnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_cnx_3fld_names(i))
    if (t_string(1:slen).eq.i_cnx_3fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, icnx_3field-1
      i_cnx_3fld(i)%p => i_cnx_3fld(i+1)%p
      i_cnx_3fld_names(i) = i_cnx_3fld_names(i+1)
      i_cnx_3dim1(i) = i_cnx_3dim1(i+1)
      i_cnx_3dim2(i) = i_cnx_3dim2(i+1)
    end do
    icnx_3field = icnx_3field - 1
    t_done = .true.
  endif
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.delete_cnx_field)
end subroutine delete_cnx_field


! 
! Method:  delete_bcnx_field[]
! Remove field from boundary connections
! @param t_string: name of field
! @param t_ok: verify that field was found (and deleted)
! 

subroutine delete_bcnx_field( t_string, t_ok)
! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.delete_bcnx_field.use)
  use grid_mod
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.delete_bcnx_field.use)
  implicit none
  character (len=*) :: t_string ! in
  logical :: t_ok ! out

! DO-NOT-DELETE splicer.begin(stmpgrid.GAGrid.delete_bcnx_field)
! Insert-Code-Here {stmpgrid.GAGrid.delete_bcnx_field} (delete_bcnx_field method)
! 
  integer i, idx, slen, nlen, grid_clen, dim1, dim2
  logical t_done
  t_ok = .false.
  t_done = .false.
!
  slen = len_trim(t_string)
  i = 0
  do while (i.lt.dbcnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_bcnx_fld_names(i))
    if (t_string(1:slen).eq.d_bcnx_fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dbcnx_field-1
      d_bcnx_fld(i)%p => d_bcnx_fld(i+1)%p
      d_bcnx_fld_names(i) = d_bcnx_fld_names(i+1)
    end do
    dbcnx_field = dbcnx_field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.dbcnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_bcnx_2fld_names(i))
    if (t_string(1:slen).eq.d_bcnx_2fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dbcnx_2field-1
      d_bcnx_2fld(i)%p => d_bcnx_2fld(i+1)%p
      d_bcnx_2fld_names(i) = d_bcnx_2fld_names(i+1)
      d_bcnx_2dim1(i) = d_bcnx_2dim1(i+1)
    end do
    dbcnx_2field = dbcnx_2field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.dbcnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(d_bcnx_3fld_names(i))
    if (t_string(1:slen).eq.d_bcnx_3fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, dbcnx_3field-1
      d_bcnx_3fld(i)%p => d_bcnx_3fld(i+1)%p
      d_bcnx_3fld_names(i) = d_bcnx_3fld_names(i+1)
      d_bcnx_3dim1(i) = d_bcnx_3dim1(i+1)
      d_bcnx_3dim2(i) = d_bcnx_3dim2(i+1)
    end do
    dbcnx_3field = dbcnx_3field - 1
    t_done = .true.
  endif
!
  i = 0
  do while (i.lt.ibcnx_field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_bcnx_fld_names(i))
    if (t_string(1:slen).eq.i_bcnx_fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, ibcnx_field-1
      i_bcnx_fld(i)%p => i_bcnx_fld(i+1)%p
      i_bcnx_fld_names(i) = i_bcnx_fld_names(i+1)
    end do
    ibcnx_field = ibcnx_field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.ibcnx_2field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_bcnx_2fld_names(i))
    if (t_string(1:slen).eq.i_bcnx_2fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, ibcnx_2field-1
      i_bcnx_2fld(i)%p => i_bcnx_2fld(i+1)%p
      i_bcnx_2fld_names(i) = i_bcnx_2fld_names(i+1)
      i_bcnx_2dim1(i) = i_bcnx_2dim1(i+1)
    end do
    ibcnx_2field = ibcnx_2field - 1
    t_done = .true.
  endif
  i = 0
  do while (i.lt.ibcnx_3field.and.(.not.t_ok))
    i = i + 1
    nlen = len_trim(i_bcnx_3fld_names(i))
    if (t_string(1:slen).eq.i_bcnx_3fld_names(i)(1:nlen)) then
      idx = i
      t_ok = .true.
    endif
  end do
  if (t_ok.and.(.not.t_done)) then
    do i = idx, ibcnx_3field-1
      i_bcnx_3fld(i)%p => i_bcnx_3fld(i+1)%p
      i_bcnx_3fld_names(i) = i_bcnx_3fld_names(i+1)
      i_bcnx_3dim1(i) = i_bcnx_3dim1(i+1)
      i_bcnx_3dim2(i) = i_bcnx_3dim2(i+1)
    end do
    ibcnx_3field = ibcnx_3field - 1
    t_done = .true.
  endif
  return
! DO-NOT-DELETE splicer.end(stmpgrid.GAGrid.delete_bcnx_field)
end subroutine delete_bcnx_field
subroutine grid_factor(p,xdim,ydim,zdim,idx,idy,idz)
  implicit none
  integer MAX_FACTOR
  parameter (MAX_FACTOR = 10000)
  integer i,j,p,idx,idy,idz
  integer ip,ifac,pmax,prime(MAX_FACTOR)
  integer fac(MAX_FACTOR)
  integer xdim,ydim,zdim,ix,iy,iz
!
  i = 1
!
!    factor p completely
!    first, find all prime numbers, besides 1, less than or equal to
!    the square root of p
!
  ip = int(sqrt(dble(p)))+1
  pmax = 0
  do i = 2, ip
    do j = 1, pmax
      if (mod(i,prime(j)).eq.0) go to 100
    end do
    pmax = pmax + 1
    if (pmax.gt.MAX_FACTOR) write(6,*) 'Overflow in grid_factor'
    prime(pmax) = i
    100   continue
  end do
!
!    find all prime factors of p
!
  ip = p
  ifac = 0
  do i = 1, pmax
    200   if (mod(ip,prime(i)).eq.0) then
      ifac = ifac + 1
      fac(ifac) = prime(i)
      ip = ip/prime(i)
      go to 200
    endif
  end do
!
!    determine three factors of p of approximately the
!    same size
!
  idx = 1
  idy = 1
  idz = 1
  do i = ifac, 1, -1
    ix = xdim / idx
    iy = ydim / idy
    iz = zdim / idz
    if (ix.ge.iy.and.ix.ge.iz.and.ix.gt.1) then
      idx = fac(i)*idx
    elseif (iy.ge.ix.and.iy.ge.iz.and.iy.gt.1) then
      idy = fac(i)*idy
    elseif (iz.ge.ix.and.iz.ge.iy.and.iz.gt.1) then
      idz = fac(i)*idz
    else
      write(6,*) 'Too many processors in grid factoring routine'
    endif
  end do
  return
end subroutine grid_factor

!
subroutine add_node_dfield(name, idx)
  use grid_mod
  character(*) name
  integer slen, idx, grid_clen
  dnode_field = dnode_field + 1
  idx = dnode_field
  slen = len_trim(name)
  d_nd_fld_names(dnode_field)(1:) = ""
  d_nd_fld_names(dnode_field)(1:slen) = name(1:slen)
  allocate(d_nd_fld(dnode_field)%p(num_nodes))
end subroutine add_node_dfield
!
subroutine add_node_d2field(name, dim1, idx)
  use grid_mod
  character(*) name
  integer slen, dim1, idx, grid_clen
  dnode_2field = dnode_2field + 1
  idx = dnode_2field
  slen = len_trim(name)
  d_nd_2fld_names(dnode_2field)(1:) = ""
  d_nd_2fld_names(dnode_2field)(1:slen) = name(1:slen)
  d_nd_2dim1(dnode_2field) = dim1
  allocate(d_nd_2fld(dnode_2field)%p(dim1,num_nodes))
  call set_2d_dbls_ga
end subroutine add_node_d2field
!
subroutine add_node_d3field(name, dim1, dim2, idx)
  use grid_mod
  character(*) name
  integer slen, dim1, dim2, idx, grid_clen
  dnode_3field = dnode_3field + 1
  idx = dnode_3field
  slen = len_trim(name)
  d_nd_3fld_names(dnode_3field)(1:) = ""
  d_nd_3fld_names(dnode_3field)(1:slen) = name(1:slen)
  d_nd_3dim1(dnode_3field) = dim1
  d_nd_3dim2(dnode_3field) = dim2
  allocate(d_nd_3fld(dnode_3field)%p(dim1,dim2,num_nodes))
  call set_3d_dbls_ga
end subroutine add_node_d3field
!
subroutine add_node_ifield(name, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, idx, grid_clen
  inode_field = inode_field + 1
  idx = inode_field
  slen = len_trim(name)
  i_nd_fld_names(inode_field)(1:) = ""
  i_nd_fld_names(inode_field)(1:slen) = name(1:slen)
  allocate(i_nd_fld(inode_field)%p(num_nodes))
end subroutine add_node_ifield
!
subroutine add_node_i2field(name, dim1, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, idx, grid_clen
  inode_2field = inode_2field + 1
  idx = inode_2field
  slen = len_trim(name)
  i_nd_2fld_names(inode_2field)(1:) = ""
  i_nd_2fld_names(inode_2field)(1:slen) = name(1:slen)
  i_nd_2dim1(inode_2field) = dim1
  allocate(i_nd_2fld(inode_2field)%p(dim1,num_nodes))
  call set_2d_ints_ga
end subroutine add_node_i2field
!
subroutine add_node_i3field(name, dim1, dim2, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, dim2, idx, grid_clen
  inode_3field = inode_3field + 1
  idx = inode_3field
  slen = len_trim(name)
  i_nd_3fld_names(inode_3field)(1:) = ""
  i_nd_3fld_names(inode_3field)(1:slen) = name(1:slen)
  i_nd_3dim1(inode_3field) = dim1
  i_nd_3dim2(inode_3field) = dim2
  allocate(i_nd_3fld(inode_3field)%p(dim1,dim2,num_nodes))
  call set_3d_ints_ga
end subroutine add_node_i3field
!
subroutine add_cnx_dfield(name, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, idx, grid_clen
  dcnx_field = dcnx_field + 1
  idx = dcnx_field
  slen = len_trim(name)
  d_cnx_fld_names(dcnx_field)(1:) = ""
  d_cnx_fld_names(dcnx_field)(1:slen) = name(1:slen)
  allocate(d_cnx_fld(dcnx_field)%p(num_cnx))
end subroutine add_cnx_dfield
!
subroutine add_cnx_d2field(name, dim1, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, idx, grid_clen
  dcnx_2field = dcnx_2field + 1
  idx = dcnx_2field
  slen = len_trim(name)
  d_cnx_2fld_names(dcnx_2field)(1:) = ""
  d_cnx_2fld_names(dcnx_2field)(1:slen) = name(1:slen)
  d_cnx_2dim1(dcnx_2field) = dim1
  allocate(d_cnx_2fld(dcnx_2field)%p(dim1,num_cnx))
end subroutine add_cnx_d2field
!
subroutine add_cnx_d3field(name, dim1, dim2, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, dim2, idx, grid_clen
  dcnx_3field = dcnx_3field + 1
  idx = dcnx_3field
  slen = len_trim(name)
  d_cnx_3fld_names(dcnx_3field)(1:) = ""
  d_cnx_3fld_names(dcnx_3field)(1:slen) = name(1:slen)
  d_cnx_3dim1(dcnx_3field) = dim1
  d_cnx_3dim2(dcnx_3field) = dim2
  allocate(d_cnx_3fld(dcnx_3field)%p(dim1,dim2,num_cnx))
end subroutine add_cnx_d3field
!
subroutine add_cnx_ifield(name, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, idx, grid_clen
  icnx_field = icnx_field + 1
  idx = icnx_field
  slen = len_trim(name)
  i_cnx_fld_names(icnx_field)(1:) = ""
  i_cnx_fld_names(icnx_field)(1:slen) = name(1:slen)
  allocate(i_cnx_fld(icnx_field)%p(num_cnx))
end subroutine add_cnx_ifield
!
subroutine add_cnx_i2field(name, dim1, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, idx, grid_clen
  icnx_2field = icnx_2field + 1
  idx = icnx_2field
  slen = len_trim(name)
  i_cnx_2fld_names(icnx_2field)(1:) = ""
  i_cnx_2fld_names(icnx_2field)(1:slen) = name(1:slen)
  i_cnx_2dim1(icnx_2field) = dim1
  allocate(i_cnx_2fld(icnx_2field)%p(dim1,num_cnx))
end subroutine add_cnx_i2field
!
subroutine add_cnx_i3field(name, dim1, dim2, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, dim2, idx, grid_clen
  icnx_3field = icnx_3field + 1
  idx = icnx_3field
  slen = len_trim(name)
  i_cnx_3fld_names(icnx_3field)(1:) = ""
  i_cnx_3fld_names(icnx_3field)(1:slen) = name(1:slen)
  i_cnx_3dim1(icnx_3field) = dim1
  i_cnx_3dim2(icnx_3field) = dim2
  allocate(i_cnx_3fld(icnx_3field)%p(dim1,dim2,num_cnx))
end subroutine add_cnx_i3field
!
subroutine add_bcnx_dfield(name, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, idx, grid_clen
  dbcnx_field = dbcnx_field + 1
  idx = dbcnx_field
  slen = len_trim(name)
  d_bcnx_fld_names(dbcnx_field)(1:) = ""
  d_bcnx_fld_names(dbcnx_field)(1:slen) = name(1:slen)
  allocate(d_bcnx_fld(dbcnx_field)%p(num_bcnx))
end subroutine add_bcnx_dfield
!
subroutine add_bcnx_d2field(name, dim1, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, idx, grid_clen
  dbcnx_2field = dbcnx_2field + 1
  idx = dbcnx_2field
  slen = len_trim(name)
  d_bcnx_2fld_names(dbcnx_2field)(1:) = ""
  d_bcnx_2fld_names(dbcnx_2field)(1:slen) = name(1:slen)
  d_bcnx_2dim1(dbcnx_2field) = dim1
  allocate(d_bcnx_2fld(dbcnx_2field)%p(dim1,num_bcnx))
end subroutine add_bcnx_d2field
!
subroutine add_bcnx_d3field(name, dim1, dim2, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, dim2, idx, grid_clen
  dbcnx_3field = dbcnx_3field + 1
  idx = dbcnx_3field
  slen = len_trim(name)
  d_bcnx_3fld_names(dbcnx_3field)(1:) = ""
  d_bcnx_3fld_names(dbcnx_3field)(1:slen) = name(1:slen)
  d_bcnx_3dim1(dbcnx_3field) = dim1
  d_bcnx_3dim2(dbcnx_3field) = dim2
  allocate(d_bcnx_3fld(dbcnx_3field)%p(dim1, dim2, num_bcnx))
end subroutine add_bcnx_d3field
!
subroutine add_bcnx_ifield(name, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, idx, grid_clen
  ibcnx_field = ibcnx_field + 1
  idx = ibcnx_field
  slen = len_trim(name)
  i_bcnx_fld_names(ibcnx_field)(1:) = ""
  i_bcnx_fld_names(ibcnx_field)(1:slen) = name(1:slen)
  allocate(i_bcnx_fld(ibcnx_field)%p(num_bcnx))
end subroutine add_bcnx_ifield
!
subroutine add_bcnx_i2field(name, dim1, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, idx, grid_clen
  ibcnx_2field = ibcnx_2field + 1
  idx = ibcnx_2field
  slen = len_trim(name)
  i_bcnx_2fld_names(ibcnx_2field)(1:) = ""
  i_bcnx_2fld_names(ibcnx_2field)(1:slen) = name(1:slen)
  i_bcnx_2dim1(ibcnx_2field) = dim1
  allocate(i_bcnx_2fld(ibcnx_2field)%p(dim1, num_bcnx))
end subroutine add_bcnx_i2field
!
subroutine add_bcnx_i3field(name, dim1, dim2, idx)
  use grid_mod
  implicit none
  character(*) name
  integer slen, dim1, dim2, idx, grid_clen
  ibcnx_3field = ibcnx_3field + 1
  idx = ibcnx_3field
  slen = len_trim(name)
  i_bcnx_3fld_names(ibcnx_3field)(1:) = ""
  i_bcnx_3fld_names(ibcnx_3field)(1:slen) = name(1:slen)
  i_bcnx_3dim1(ibcnx_3field) = dim1
  i_bcnx_3dim2(ibcnx_3field) = dim2
  allocate(i_bcnx_3fld(ibcnx_3field)%p(dim1, dim2, num_bcnx))
end subroutine add_bcnx_i3field
!
!  Check to see if current Global Arrays are sufficient to update
!  2 and 3 dimensional node fields
!
subroutine set_2d_dbls_ga()
  use grid_mod
  implicit none
#include "mafdecls.fh"
#include "global.fh"
  integer i, imax, dims(4), blocks(4), ndim, me
  integer, allocatable :: mapc(:)
  logical status
  imax = 0
  do i = 1, dnode_2field
    if (d_nd_2dim1(i).gt.imax) imax = d_nd_2dim1(i)
  end do
  if (dnode_2field.eq.0) then
    status = ga_destroy(ga_dbl2)
  else if (dnode_2field.eq.1.or.imax.ne.d2dim1_max) then
    allocate(mapc(ga_px+ga_py+ga_pz+1))
    do i = 1, ga_px+ga_py+ga_pz
      mapc(i+1) = ga_mapc(i)
    end do
    mapc(1) = 1
    blocks(1) = 1
    blocks(2) = ga_px
    blocks(3) = ga_py
    blocks(4) = ga_pz
    dims(1) = imax
    dims(2) = nxdim
    dims(3) = nydim
    dims(4) = nzdim
    ndim = 4
    d2dim1_max = imax
    if (dnode_2field.gt.1) then
      status = ga_destroy(ga_dbl2)
    endif
    ga_dbl2 = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
    IF (GAE4D) CALL GA_SET_PGROUP(ga_dbl2,GAGRP)
#endif
    call ga_set_data(ga_dbl2, ndim, dims, MT_DBL)
!  me = ga_nodeid()
!if(me.eq.0) print *,'ga_set_irreg_distr 3'
    call ga_set_irreg_distr(ga_dbl2, mapc, blocks)
!if(me.eq.0) print *,'ga_set_irreg_distr 3e'
!print *,'ga_2d',ga_dbl3,mapc(1:5),blocks(1:5)
    status = ga_allocate(ga_dbl2)
    deallocate(mapc)
  endif
end subroutine set_2d_dbls_ga
!
subroutine set_2d_ints_ga()
  use grid_mod
  implicit none
#include "mafdecls.fh"
#include "global.fh"
  integer i, imax, dims(4), blocks(4), ndim,me
  integer, allocatable :: mapc(:)
  logical status
  imax = 0
  do i = 1, inode_2field
    if (i_nd_2dim1(i).gt.imax) imax = i_nd_2dim1(i)
  end do
  if (inode_2field.eq.0) then
    status = ga_destroy(ga_int2)
  else if (inode_2field.eq.1.or.imax.ne.i2dim1_max) then
    allocate(mapc(ga_px+ga_py+ga_pz+1))
    do i = 1, ga_px+ga_py+ga_pz
      mapc(i+1) = ga_mapc(i)
    end do
    mapc(1) = 1
    blocks(1) = 1
    blocks(2) = ga_px
    blocks(3) = ga_py
    blocks(4) = ga_pz
    dims(1) = imax
    dims(2) = nxdim
    dims(3) = nydim
    dims(4) = nzdim
    ndim = 4
    i2dim1_max = imax
    if (inode_2field.gt.1) then
      status = ga_destroy(ga_int2)
    endif
    ga_int2 = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch
    IF (GAE4D) CALL GA_SET_PGROUP(ga_int2,GAGRP)
#endif
    call ga_set_data(ga_int2, ndim, dims, MT_INT)
!  me = ga_nodeid()
!if(me.eq.0) print *,'ga_set_irreg_distr 4'
    call ga_set_irreg_distr(ga_int2, mapc, blocks)
!if(me.eq.0) print *,'ga_set_irreg_distr 4e'
    status = ga_allocate(ga_int2)
    deallocate(mapc)
  endif
end subroutine set_2d_ints_ga
!
subroutine set_3d_dbls_ga()
  use grid_mod
  implicit none
#include "mafdecls.fh"
#include "global.fh"
  integer i, imax1, imax2, dims(5), blocks(5), ndim,me
  integer, allocatable :: mapc(:)
  logical status
  imax1 = 1
  imax2 = 1
  do i = 1, dnode_3field
    if (d_nd_3dim1(i).gt.imax1) imax1 = d_nd_3dim1(i)
    if (d_nd_3dim2(i).gt.imax2) imax2 = d_nd_3dim2(i)
  end do
  if (dnode_3field.eq.0) then
    status = ga_destroy(ga_dbl3)
  else if (dnode_3field.eq.1.or.imax1.ne.d3dim1_max.or. &
           imax2.ne.d3dim2_max) then
    allocate(mapc(ga_px+ga_py+ga_pz+2))
!    do i = 2, ga_px+ga_py+ga_pz
    do i = 1, ga_px+ga_py+ga_pz
      mapc(i+2) = ga_mapc(i)
    end do
    mapc(1) = 1
    mapc(2) = 1
    blocks(1) = 1
    blocks(2) = 1
    blocks(3) = ga_px
    blocks(4) = ga_py
    blocks(5) = ga_pz
    dims(1) = imax1
    dims(2) = imax2
    dims(3) = nxdim
    dims(4) = nydim
    dims(5) = nzdim
    ndim = 5
    d3dim1_max = imax1
    d3dim2_max = imax2
    if (dnode_3field.gt.1) then
      status = ga_destroy(ga_dbl3)
    endif
    ga_dbl3 = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch
    IF (GAE4D) CALL GA_SET_PGROUP(ga_dbl3,GAGRP)
#endif
    call ga_set_data(ga_dbl3, ndim, dims, MT_DBL)
!  me = ga_nodeid()
!if(me.eq.0) print *,'ga_set_irreg_distr 5'
    call ga_set_irreg_distr(ga_dbl3, mapc, blocks)
!if(me.eq.0) print *,'ga_set_irreg_distr 5e'
!print *,'ga_3d',ga_dbl3,mapc(1:5),blocks(1:5)
!stop
    status = ga_allocate(ga_dbl3)
    deallocate(mapc)
  endif
end subroutine set_3d_dbls_ga
!
subroutine set_3d_ints_ga()
  use grid_mod
  implicit none
#include "mafdecls.fh"
#include "global.fh"
  integer i, imax1, imax2, dims(5), blocks(5), ndim,me
  integer, allocatable :: mapc(:)
  logical status
  imax1 = 1
  imax2 = 1
  do i = 1, inode_3field
    if (i_nd_3dim1(i).gt.imax1) imax1 = i_nd_3dim1(i)
    if (i_nd_3dim2(i).gt.imax2) imax2 = i_nd_3dim2(i)
  end do
  if (inode_3field.eq.0) then
    status = ga_destroy(ga_int3)
  else if (inode_3field.eq.1.or.imax1.ne.i3dim1_max.or. &
           imax2.ne.i3dim2_max) then
    allocate(mapc(ga_px+ga_py+ga_pz+2))
!    do i = 2, ga_px+ga_py+ga_pz
    do i = 1, ga_px+ga_py+ga_pz
      mapc(i+2) = ga_mapc(i)
    end do
    mapc(1) = 1
    mapc(2) = 1
    blocks(1) = 1
    blocks(2) = 1
    blocks(3) = ga_px
    blocks(4) = ga_py
    blocks(5) = ga_pz
    dims(1) = imax1
    dims(2) = imax2
    dims(3) = nxdim
    dims(4) = nydim
    dims(5) = nzdim
    ndim = 5
    i3dim1_max = imax1
    i3dim2_max = imax2
    if (inode_3field.gt.1) then
      status = ga_destroy(ga_int3)
    endif
    ga_int3 = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
    IF (GAE4D) CALL GA_SET_PGROUP(ga_int3,GAGRP)
#endif
    call ga_set_data(ga_int3, ndim, dims, MT_INT)
!  me = ga_nodeid()
!if(me.eq.0) print *,'ga_set_irreg_distr 6'
    call ga_set_irreg_distr(ga_int3, mapc, blocks)
!if(me.eq.0) print *,'ga_set_irreg_distr 6e'
    status = ga_allocate(ga_int3)
    deallocate(mapc)
  endif
end subroutine set_3d_ints_ga
!
integer function grid_clen(string)
  implicit none
  character(*) string
  integer slen, icnt
  icnt = 1 
  grid_clen = 0
  slen = len_trim(string)
  do while (string(icnt:icnt).ne." ".and.string(icnt:icnt).ne."".and. &
            icnt.le.slen)
    grid_clen = icnt
    icnt = icnt + 1
  end do
  return
end function grid_clen
subroutine parse_id(it,ix,iy,iz)
  use grid_mod
  implicit none
  integer i, it, ix, iy, iz
  i = it - 1
  ix = mod(i,nxdim)
  i = (i-ix)/nxdim
  iy = mod(i,nydim)
  iz = (i-iy)/nydim
  ix = ix + 1
  iy = iy + 1
  iz = iz + 1
end subroutine parse_id
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION COPYNDFLD( G_BUF,VAR_OUT,LDXX,LO,HI,LXP ) RESULT (T_OK)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID_MOD
      USE GRID
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
!
!
!----------------------Type Declarations-------------------------------!
!
       INTEGER, INTENT(IN) :: LDXX(3)
       INTEGER, INTENT(IN) :: LO(3)
       INTEGER, INTENT(IN) :: HI(3)
       INTEGER BUF3D( LDXX(1),LDXX(2),LDXX(3) )
       INTEGER, INTENT(OUT) :: VAR_OUT( LDXX(1)*LDXX(2)*LDXX(3) )
       INTEGER G_BUF
!
!----------------------Executable Lines--------------------------------!
!
       ME = GA_NODEID()
       NPROC = GA_NNODES()
       SUBNMX = '/COPYNDFLD'
       ICSNX = INDEX( SUBNMX,'  ' )-1
       SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
       IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
          '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
       ICSN = ICSN+ICSNX
       T_OK = .FALSE.
!
!---  Copy contents of array back to node field ---
!
       CALL GA_SYNC
       BUF3D = 0
       CALL NGA_GET(G_BUF,LO,HI,BUF3D(1,1,1),LDXX)
       N = 0
       NN = LDXX(1)*LDXX(2)*LDXX(3)
       DO K = 1, LDXX(3)
         DO J = 1, LDXX(2)
           DO I = 1, LDXX(1)
             N = N + 1
             VAR_OUT(N) = BUF3D(I,J,K)
             IF( LXP.GT.0 )THEN
               IF (BUF3D(I,J,K).EQ.0.AND.GRID_MASK(N).GT.0) THEN
                  NXP = NXP + 1
               ENDIF
             ENDIF
           END DO
         END DO
       END DO
       IF( N == NN )T_OK = .TRUE.
!
!---  End of COPYNDFLD
!
       ICSN = ICSN-ICSNX
       SUBNM = SUBNM(1:ICSN)
       RETURN
       END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION COPYIJK1D_INT( IVAR_OUT,IVAR,IROCK ) RESULT (T_OK)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID
      USE GRID_MOD

      IMPLICIT NONE
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER, INTENT(OUT) :: IVAR_OUT( NUM_NODES )
      INTEGER, INTENT(IN) :: IVAR
      INTEGER :: NPROC,ME,ICSNX
      INTEGER :: N,IROCK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/COPYIJK1D_INT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---  Copy contents of array back to node field ---
!
      IF( IROCK < 0 ) THEN
        DO N = 1, NUM_NODES
          IVAR_OUT(N) = IVAR 
        END DO
      ELSE
        DO N = 1, NUM_NODES
          IF( IZ(N) == IROCK )IVAR_OUT(N) = IVAR 
        END DO
      ENDIF
      IF( N == NUM_NODES )T_OK = .TRUE.
!
!---  End of COPYIJK1D_INT
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION COPYIJK2D_INT( VAR_OUT,VAR,LNDX,INDC,IROCK ) RESULT (T_OK)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID
      USE GRID_MOD

      IMPLICIT NONE
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER, INTENT(OUT) :: VAR_OUT( LNDX,NUM_NODES )
      INTEGER, INTENT(IN) :: VAR
      INTEGER :: NPROC,ME,ICSNX
      INTEGER :: N,INDC,LNDX,IROCK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/COPYIJK2D_INT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---  Copy contents of array back to node field ---
!
      IF( IROCK < 0 )THEN
        DO N = 1, NUM_NODES
          VAR_OUT(INDC,N) = VAR 
        END DO
      ELSE
        DO N = 1, NUM_NODES
          IF( IZ(N) == IROCK )VAR_OUT(INDC,N) = VAR 
        END DO
      ENDIF
      IF( N == NUM_NODES )T_OK = .TRUE.
!
!---  End of COPYIJK2D_INT
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION COPYIJK3D_INT( VAR_OUT,VAR,LNDX,INDC,LCX,ICX,IROCK ) &
        RESULT (T_OK)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID
      USE GRID_MOD

      IMPLICIT NONE
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER, INTENT(OUT) :: VAR_OUT( LCX,LNDX,NUM_NODES )
      INTEGER, INTENT(IN) :: VAR
      INTEGER :: NPROC,ME,ICSNX
      INTEGER :: N,INDC,LNDX,LCX,ICX,IROCK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/COPYIJK3D_INT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---  Copy contents of array back to node field ---
!
      IF( IROCK < 0 )THEN
        DO N = 1, NUM_NODES
          VAR_OUT(ICX,INDC,N) = VAR 
        END DO
      ELSE
        DO N = 1, NUM_NODES
          IF( IZ(N) == IROCK )VAR_OUT(ICX,INDC,N) = VAR 
        END DO
      ENDIF
      IF( N == NUM_NODES )T_OK = .TRUE.
!
!---  End of COPYIJK3D_INT
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION COPYIJK1D( VAR_OUT,VAR,IROCK ) RESULT (T_OK)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID
      USE GRID_MOD

      IMPLICIT NONE
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8, INTENT(OUT) :: VAR_OUT( NUM_NODES )
      REAL*8, INTENT(IN) :: VAR
      INTEGER :: NPROC,ME,ICSNX
      INTEGER :: N,IROCK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/COPYIJK1D'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---  Copy contents of array back to node field ---
!
      IF( IROCK < 0 )THEN
        DO N = 1, NUM_NODES
          VAR_OUT(N) = VAR 
        END DO
      ELSE
        DO N = 1, NUM_NODES
          IF( IZ(N) == IROCK )VAR_OUT(N) = VAR 
        END DO
      ENDIF
      IF( N == NUM_NODES )T_OK = .TRUE.
!
!---  End of COPYIJK1D
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION COPYIJK2D( VAR_OUT,VAR,LNDX,INDC,IROCK ) RESULT (T_OK)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID
      USE GRID_MOD

      IMPLICIT NONE
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8, INTENT(OUT) :: VAR_OUT( LNDX,NUM_NODES )
      REAL*8, INTENT(IN) :: VAR
      INTEGER :: NPROC,ME,ICSNX
      INTEGER :: N,INDC,LNDX,IROCK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/COPYIJK2D'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---  Copy contents of array back to node field ---
!
      IF( IROCK < 0 )THEN
        DO N = 1, NUM_NODES
          VAR_OUT(INDC,N) = VAR 
        END DO
      ELSE
        DO N = 1, NUM_NODES
          IF( IZ(N) == IROCK )VAR_OUT(INDC,N) = VAR 
        END DO
      ENDIF
      IF( N == NUM_NODES )T_OK = .TRUE.
!
!---  End of COPYIJK2D
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION COPYIJK3D( VAR_OUT,VAR,LNDX,INDC,LCX,ICX,IROCK ) &
        RESULT (T_OK)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID
      USE GRID_MOD

      IMPLICIT NONE
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!
!----------------------Type Declarations-------------------------------!
!
      REAL*8, INTENT(OUT) :: VAR_OUT( LCX,LNDX,NUM_NODES )
      REAL*8, INTENT(IN) :: VAR
      INTEGER :: NPROC,ME,ICSNX
      INTEGER :: N,INDC,LNDX,LCX,ICX,IROCK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/COPYIJK3D'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
         '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      T_OK = .FALSE.
!
!---  Copy contents of array back to node field ---
!
      IF( IROCK < 0 )THEN
        DO N = 1, NUM_NODES
          VAR_OUT(ICX,INDC,N) = VAR 
        END DO
      ELSE
        DO N = 1, NUM_NODES
          IF( IZ(N) == IROCK )VAR_OUT(ICX,INDC,N) = VAR 
        END DO
      ENDIF
      IF( N == NUM_NODES )T_OK = .TRUE.
!
!---  End of COPYIJK3D
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION CREATE_INTGA( G_BUF,IX,IY,IZ ) RESULT (STATUS)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID_MOD
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
!
!
!----------------------Type Declarations-------------------------------!
!
       INTEGER, INTENT(IN) :: IX
       INTEGER, INTENT(IN) :: IY
       INTEGER, INTENT(IN) :: IZ
       INTEGER, INTENT(OUT) :: G_BUF
       INTEGER DIMS(3),  THREE

!----------------------Executable Lines--------------------------------!
!
       ME = GA_NODEID()
       NPROC = GA_NNODES()
       SUBNMX = '/CREATE_INTGA'
       ICSNX = INDEX( SUBNMX,'  ' )-1
       SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
       IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
          '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
       ICSN = ICSN+ICSNX
!
!---  Create temporary Global Array ---
!
       DIMS(1) = IX
       DIMS(2) = IY
       DIMS(3) = IZ
       THREE = 3
       G_BUF = GA_CREATE_HANDLE()
#ifdef USE_E4D
!-- E4D Patch
       IF (GAE4D) CALL GA_SET_PGROUP(G_BUF,GAGRP)
#endif
       CALL GA_SET_DATA(G_BUF, THREE, DIMS, MT_INT)
       STATUS = GA_ALLOCATE(G_BUF)
!
!---  End of CREATE_INTGA
!
       ICSN = ICSN-ICSNX
       SUBNM = SUBNM(1:ICSN)
       RETURN
       END
!
!
!
!----------------------Function----------------------------------------!
!
      LOGICAL FUNCTION CREATE_DBLGA( G_BUF,IX,IY,IZ ) RESULT (STATUS)
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
!     Copies temporary global array to permanent global array
!
!----------------------Authors-----------------------------------------!
!
!     Written by VL Freedman, PNL, March, 2013.
!
!
!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE CONST
      USE GRID_MOD
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
!
!
!----------------------Type Declarations-------------------------------!
!
       INTEGER, INTENT(IN) :: IX
       INTEGER, INTENT(IN) :: IY
       INTEGER, INTENT(IN) :: IZ
       INTEGER, INTENT(OUT) :: G_BUF
       INTEGER DIMS(3),  THREE

!----------------------Executable Lines--------------------------------!
!
       ME = GA_NODEID()
       NPROC = GA_NNODES()
       SUBNMX = '/CREATE_DBLGA'
       ICSNX = INDEX( SUBNMX,'  ' )-1
       SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
       IF( INDEX(CVS_ID(290)(1:1),'$').EQ.0 ) CVS_ID(290) = &
          '$Id: GA_grid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
       ICSN = ICSN+ICSNX
!
!---  Create temporary Global Array ---
!
       DIMS(1) = IX
       DIMS(2) = IY
       DIMS(3) = IZ
       THREE = 3
       G_BUF = GA_CREATE_HANDLE()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(G_BUF,GAGRP)
#endif
       CALL GA_SET_DATA(G_BUF, THREE, DIMS, MT_DBL)
       STATUS = GA_ALLOCATE(G_BUF)
!
!---  End of CREATE_DBLGA
!
       ICSN = ICSN-ICSNX
       SUBNM = SUBNM(1:ICSN)
       RETURN
       END
!
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE CROSS_3D ( X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3 )
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
!     CROSS_3D computes the cross product of two vectors in 3D.
!
!     Definition:
!
!     The cross product in 3D can be regarded as the determinant of the
!     symbolic matrix:
!
!          |  i  j  k |
!      det | x1 y1 z1 |
!          | x2 y2 z2 |
!
!      = ( y1 * z2 - z1 * y2 ) * i
!      + ( z1 * x2 - x1 * z2 ) * j
!      + ( x1 * y2 - y1 * x2 ) * k
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, X2, Y2, Z2, the coordinates 
!     of the vectors.
!
!     Output, real X3, Y3, Z3, the cross product vector.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 X1,Y1,Z1
  REAL*8 X2,Y2,Z2
  REAL*8 X3,Y3,Z3
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/CROSS_3D'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX

  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $'
  ICSN = ICSN+ICSNX
  X3 = Y1*Z2 - Z1*Y2
  Y3 = Z1*X2 - X1*Z2
  Z3 = X1*Y2 - Y1*X2
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)

!
!---  End of CROSS_3D group  ---
!
  RETURN
  END
  
!------------------------Function--------------------------------------!
!
  FUNCTION ENORM_3D ( X1,Y1,Z1 )
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
!     ENORM_3D computes the Euclidean norm of a vector in 3D.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, the coordinates of the vector.
!
!     Output, real ENORM_3D, the Euclidean norm of the vector.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 ENORM_3D
  REAL*8 X1,Y1,Z1
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/ENORM_3D'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $'
  ICSN = ICSN+ICSNX
  ENORM_3D = SQRT ( X1*X1 + Y1*Y1 + Z1*Z1 )
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)
!
!---  End of ENORM_3D group  ---
!
  RETURN
  END
  
  
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE PGCNTRD ( N,PX,PY,PZ,CX,CY,CZ )
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
!     PGCNTRD computes the centroid of a polygon in 3D.
!
!
!     Method:
!
!     The centroid is the area-weighted sum of the centroids of
!     disjoint triangles that make up the polygon.
!
!     Reference:
!
!     Adrian Bowyer and John Woodwark,
!     A Programmer's Geometry,
!     Butterworths, 1983.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, integer N, the number of vertices of the polygon.
!
!     Input, real X(N), Y(N), Z(N), the coordinates of the vertices.
! 
!     Output, real CX, CY, CZ, the coordinates of the centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 AREA,AREAT
  REAL*8 CX,CY,CZ
  REAL*8 PX(N),PY(N),PZ(N)
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/PGCNTRD'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $'
  ICSN = ICSN+ICSNX
  AREA = 0.D+0
  CX = 0.D+0
  CY = 0.D+0
  CZ = 0.D+0
  DO 100 I = 1,N-2
    CALL TRGAREA ( PX(I),PY(I),PZ(I),PX(I+1), &
      PY(I+1),PZ(I+1),PX(N),PY(N),PZ(N),AREAT )
    AREA = AREA + AREAT
    CX = CX + AREAT*( PX(I)+PX(I+1)+PX(N) )/3.D+0
    CY = CY + AREAT*( PY(I)+PY(I+1)+PY(N) )/3.D+0
    CZ = CZ + AREAT*( PZ(I)+PZ(I+1)+PZ(N) )/3.D+0
    100 CONTINUE
  CX = CX/AREA
  CY = CY/AREA
  CZ = CZ/AREA
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)
!
!---  End of PGCNTRD group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE PHCNTRD ( PX,PY,PZ,CX,CY,CZ,NP,N )
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
!     PHCNTRD computes the centroid of a 
!     polyhedron in 3D.
!
!     Method:
!
!     The centroid is computed as the point having the minimum
!     variation of distances to the vertices.
!
!     Reference:
!
!     Zunic, T. B., and E. Makovicky.  1996.  Determination of the
!     centroid or 'the best centre' of a coordination polyhedron.
!
!     Parameters:
!
!     Input, integer NP, the number of vertices of the polygon.
!
!     Input, real X(N), Y(N), Z(N), the coordinates of the vertices.
! 
!     Output, real CX, CY, CZ, the coordinates of the centroid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 27 March 2006.
!     Last Modified by MD White, PNNL, 27 March 2006.
!     $Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE GRID
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 CX,CY,CZ
  REAL*8 PX(NP),PY(NP),PZ(NP)
  REAL*8 AJM(3,3),BJM(3)
  INTEGER IJM(3)
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/PHCNTRD'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $'
  ICSN = ICSN+ICSNX
  SUMX = 0.D+0
  SUMX2 = 0.D+0
  SUMX3 = 0.D+0
  SUMY = 0.D+0
  SUMY2 = 0.D+0
  SUMY3 = 0.D+0
  SUMZ = 0.D+0
  SUMZ2 = 0.D+0
  SUMZ3 = 0.D+0
  SUMXY = 0.D+0
  SUMYZ = 0.D+0
  SUMZX = 0.D+0
  SUMX2Y = 0.D+0
  SUMX2Z = 0.D+0
  SUMY2Z = 0.D+0
  SUMY2X = 0.D+0
  SUMZ2X = 0.D+0
  SUMZ2Y = 0.D+0
  PXMIN = 1.D+20
  PXMAX = -1.D+20
  PYMIN = 1.D+20
  PYMAX = -1.D+20
  PZMIN = 1.D+20
  PZMAX = -1.D+20
  DO 10 I = 1,NP
    PXMIN = MIN( PX(I),PXMIN )
    PXMAX = MAX( PX(I),PXMAX )
    PYMIN = MIN( PY(I),PYMIN )
    PYMAX = MAX( PY(I),PYMAX )
    PZMIN = MIN( PZ(I),PZMIN )
    PZMAX = MAX( PZ(I),PZMAX )
     10 CONTINUE
  DO 100 I = 1,NP
    SUMX = SUMX + PX(I)
    SUMX2 = SUMX2 + PX(I)**2
    SUMX3 = SUMX3 + PX(I)**3
    SUMY = SUMY + PY(I)
    SUMY2 = SUMY2 + PY(I)**2
    SUMY3 = SUMY3 + PY(I)**3
    SUMZ = SUMZ + PZ(I)
    SUMZ2 = SUMZ2 + PZ(I)**2
    SUMZ3 = SUMZ3 + PZ(I)**3
    SUMXY = SUMXY + PX(I)*PY(I)
    SUMYZ = SUMYZ + PY(I)*PZ(I)
    SUMZX = SUMZX + PZ(I)*PX(I)
    SUMX2Y = SUMX2Y + (PX(I)**2)*PY(I)
    SUMX2Z = SUMX2Z + (PX(I)**2)*PZ(I)
    SUMY2X = SUMY2X + (PY(I)**2)*PX(I)
    SUMY2Z = SUMY2Z + (PY(I)**2)*PZ(I)
    SUMZ2X = SUMZ2X + (PZ(I)**2)*PX(I)
    SUMZ2Y = SUMZ2Y + (PZ(I)**2)*PY(I)
    100 CONTINUE
  NJM = 3
  RNX = REAL(NP)
  AJM(1,1) = 2.D+0*(SUMX2-(SUMX**2)/RNX)
  AJM(1,2) = 2.D+0*(SUMXY-(SUMX*SUMY)/RNX)
  AJM(1,3) = 2.D+0*(SUMZX-(SUMZ*SUMX)/RNX)
  BJM(1) = SUMX3 + SUMY2X + SUMZ2X - (SUMX2*SUMX)/RNX &
    - (SUMY2*SUMX)/RNX - (SUMZ2*SUMX)/RNX
  AJM(2,1) = 2.D+0*(SUMXY-(SUMX*SUMY)/RNX)
  AJM(2,2) = 2.D+0*(SUMY2-(SUMY**2)/RNX)
  AJM(2,3) = 2.D+0*(SUMYZ-(SUMY*SUMZ)/RNX)
  BJM(2) = SUMX2Y + SUMY3 + SUMZ2Y - (SUMX2*SUMY)/RNX &
    - (SUMY2*SUMY)/RNX - (SUMZ2*SUMY)/RNX
  AJM(3,1) = 2.D+0*(SUMZX-(SUMZ*SUMX)/RNX)
  AJM(3,2) = 2.D+0*(SUMYZ-(SUMY*SUMZ)/RNX)
  AJM(3,3) = 2.D+0*(SUMZ2-(SUMZ**2)/RNX)
  BJM(3) = SUMX2Z + SUMY2Z + SUMZ3 - (SUMX2*SUMZ)/RNX &
    - (SUMY2*SUMZ)/RNX - (SUMZ2*SUMZ)/RNX
  CALL LUDCMP( AJM,NJM,NJM,IJM,DJM )
  CALL LUBKSB( AJM,NJM,NJM,IJM,BJM )
  CX = BJM(1)
  CY = BJM(2)
  CZ = BJM(3)
  IF( CX.LT.PXMIN .OR. CX.GT.PXMAX .OR. &
    CY.LT.PYMIN .OR. CY.GT.PYMAX .OR. &
    CZ.LT.PZMIN .OR. CZ.GT.PZMAX ) THEN
    PRINT *,'Node Centroid Outside of Polygon Limits'
    PRINT *,'N = ',N,' I = ',ID(N),' J = ',JD(N),' K = ',KD(N)
    PRINT *,'XP = ',CX,' XMIN = ',PXMIN,' XMAX = ',PXMAX
    PRINT *,'YP = ',CY,' YMIN = ',PYMIN,' YMAX = ',PYMAX
    PRINT *,'ZP = ',CZ,' ZMIN = ',PZMIN,' ZMAX = ',PZMAX
    PRINT *,'X = ',PX
    PRINT *,'Y = ',PY
    PRINT *,'Z = ',PZ
  ENDIF
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)
!
!---  End of PHCNTRD group  ---
!
  RETURN
  END
  
!------------------------Function--------------------------------------!
!
  FUNCTION RM4DET ( AX )
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
!     RM4DET computes the determinant of a 4 by 4 matrix.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real AX(4,4), the matrix whose determinant is desired.
!
!     Output, real RM4DET, the determinant of the matrix.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 RM4DET
  REAL*8 AX(4,4)
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/RM4DET'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $'
  ICSN = ICSN+ICSNX
  RM4DET = &
      AX(1,1) * ( &
      AX(2,2) * ( AX(3,3) * AX(4,4) - AX(3,4) * AX(4,3) ) &
    - AX(2,3) * ( AX(3,2) * AX(4,4) - AX(3,4) * AX(4,2) ) &
    + AX(2,4) * ( AX(3,2) * AX(4,3) - AX(3,3) * AX(4,2) ) ) &
    - AX(1,2) * ( &
      AX(2,1) * ( AX(3,3) * AX(4,4) - AX(3,4) * AX(4,3) ) &
    - AX(2,3) * ( AX(3,1) * AX(4,4) - AX(3,4) * AX(4,1) ) &
    + AX(2,4) * ( AX(3,1) * AX(4,3) - AX(3,3) * AX(4,1) ) ) &
    + AX(1,3) * ( &
      AX(2,1) * ( AX(3,2) * AX(4,4) - AX(3,4) * AX(4,2) ) &
    - AX(2,2) * ( AX(3,1) * AX(4,4) - AX(3,4) * AX(4,1) ) &
    + AX(2,4) * ( AX(3,1) * AX(4,2) - AX(3,2) * AX(4,1) ) ) &
    - AX(1,4) * ( &
      AX(2,1) * ( AX(3,2) * AX(4,3) - AX(3,3) * AX(4,2) ) &
    - AX(2,2) * ( AX(3,1) * AX(4,3) - AX(3,3) * AX(4,1) ) &
    + AX(2,3) * ( AX(3,1) * AX(4,2) - AX(3,2) * AX(4,1) ) )
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)
!
!---  End of RM4DET group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE TETRVOL( X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,VOLUME )
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
!     TETRVOL computes the area of a triangle in 3D.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X4, Y4, Z4, the
!     coordinates of the corners of the tetrahedron.
!
!     Output, real VOLUME, the volume of the tetrahedron.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 RM4DET
  REAL*8 X1,Y1,Z1
  REAL*8 X2,Y2,Z2
  REAL*8 X3,Y3,Z3
  REAL*8 X4,Y4,Z4
  REAL*8 AX(4,4)
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/TETRVOL'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $'
  ICSN = ICSN+ICSNX
  AX(1,1) = X1
  AX(2,1) = X2
  AX(3,1) = X3
  AX(4,1) = X4
  AX(1,2) = Y1
  AX(2,2) = Y2
  AX(3,2) = Y3
  AX(4,2) = Y4
  AX(1,3) = Z1
  AX(2,3) = Z2
  AX(3,3) = Z3
  AX(4,3) = Z4
  AX(1,4) = 1.D+0
  AX(2,4) = 1.D+0
  AX(3,4) = 1.D+0
  AX(4,4) = 1.D+0
  VOLUME = ABS ( RM4DET( AX ) )/6.D+0
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)
!
!---  End of TETRVOL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE TRGAREA( X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,AREA )
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
!     TRGAREA computes the area of a triangle in 3D.
!
!
!     Discussion:
!
!     This routine uses the fact that the norm of the cross product 
!     vector is the area of the parallelogram they form.  The triangle 
!     they form has half that area.
!
!     Reference:
!
!     Adrian Bowyer and John Woodwark,
!     A Programmer's Geometry,
!     Butterworths, 1983.
!
!     Author:
!
!     John Burkardt
!
!     Parameters:
!
!     Input, real X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, 
!     the triangle vertices.
!
!     Output, real AREA, the area of the triangle.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 24 March 2006.
!     Last Modified by MD White, PNNL, 24 March 2006.
!     $Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 AREA,ENORM_3D,NORM
  REAL*8 X1,Y1,Z1
  REAL*8 X2,Y2,Z2
  REAL*8 X3,Y3,Z3
  REAL*8 X4,Y4,Z4
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/TRGAREA'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $'
   ICSN = ICSN+ICSNX
   CALL CROSS_3D ( X2-X1,Y2-Y1,Z2-Z1,X3-X1,Y3-Y1,Z3-Z1,X4,Y4,Z4 )
  NORM = ENORM_3D ( X4,Y4,Z4 )
  AREA = 5.D-1*NORM
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)
!
!---  End of TRGAREA group  ---
!
  RETURN
  END
  
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE LUDCMP( A,N,NP,IX,D )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 1, 2000.
!     Last Modified by Mark White, PNNL, August 1, 2000.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE CONST
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 A(NP,NP),VV(NP)
  INTEGER IX(NP)
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/LUDCMP'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.19 2009/05/15 14:58:57 d3c002 Exp $'
  D = 1.D+0
  DO 12 I = 1,N
    AAMAX = 0.D+0
    DO 11 J = 1,N
      IF( ABS(A(I,J)).GT.AAMAX ) AAMAX = ABS(A(I,J))
     11   CONTINUE
    IF( ABS(AAMAX)/EPSL.LT.EPSL ) THEN
      INDX = 20
      CHMSG = 'Singular Matrix: '
      IMSG = NP
      CALL WRMSGS( INDX )
      ICUTTS = 1
    ENDIF
    VV(I) = 1.D+0/AAMAX
     12 CONTINUE
  DO 19 J = 1,N
    DO 14 I = 1,J-1
      SUM = A(I,J)
      DO 13 K = 1,I-1
        SUM = SUM - A(I,K)*A(K,J)
     13     CONTINUE
      A(I,J) = SUM
     14   CONTINUE
    AAMAX = 0.D+0
    DO 16 I = J,N
      SUM = A(I,J)
      DO 15 K = 1,J-1
        SUM = SUM - A(I,K)*A(K,J)
     15     CONTINUE
      A(I,J) = SUM
      DUM = VV(I)*ABS(SUM)
      IF( DUM.GE.AAMAX ) THEN
        IMAX = I
        AAMAX = DUM
      ENDIF
     16   CONTINUE
    IF( J.NE.IMAX ) THEN
      DO 17 K = 1,N
        DUM = A(IMAX,K)
        A(IMAX,K) = A(J,K)
        A(J,K) = DUM
     17     CONTINUE
      D = -D
      VV(IMAX) = VV(J)
    ENDIF
    IX(J) = IMAX
    IF( ABS(A(J,J))/EPSL.LT.EPSL ) A(J,J) = 1.D-30
    IF( J.NE.N ) THEN
      DUM = 1.D+0/A(J,J)
      DO 18 I = J+1,N
        A(I,J) = A(I,J)*DUM
     18     CONTINUE
    ENDIF
     19 CONTINUE
  SUBNM = SUBNM(1:ICSN)
!
!---  End of LUDCMP group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE LUBKSB( A,N,NP,IX,B )
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
!            Copyright Battelle Memorial Institute, 1996.
!                    All Rights Reserved.
!
!----------------------Description-------------------------------------!
!
!     Numerical Recipes, The Art of Scientific Computing
!     W.H. Press, B.P. Flannery, Saul A. Teukolsky, and W.T. Vetterling
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, August 1, 2000.
!     Last Modified by Mark White, PNNL, August 1, 2000.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE CONST
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 A(NP,NP),B(NP)
  INTEGER IX(NP)
!
!----------------------Executable Lines--------------------------------!
!
  SUBNMX = '/LUBKSB'
  ICSNX = INDEX( SUBNMX,'  ' )-1
  SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
  IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
   '$Id: rdgrid.F,v 1.19 2009/05/15 14:58:57 d3c002 Exp $'
  ICSN = ICSN+ICSNX
  II = 0
  DO 12 I = 1,N
    IL = IX(I)
    SUM = B(IL)
    B(IL) = B(I)
    IF( II.NE.0 ) THEN
      DO 11 J = II,I-1
        SUM = SUM - A(I,J)*B(J)
     11     CONTINUE
    ELSEIF( ABS(SUM)/EPSL.GT.EPSL ) THEN
      II = I
    ENDIF
    B(I) = SUM
     12 CONTINUE
  DO 14 I = N,1,-1
    SUM = B(I)
    IF( I.LT.N ) THEN
      DO 13 J = I+1,N
        SUM = SUM - A(I,J)*B(J)
     13     CONTINUE
    ENDIF
    B(I) = SUM/A(I,I)
     14 CONTINUE
  ICSN = ICSN-ICSNX
  SUBNM = SUBNM(1:ICSN)
!
!---  End of LUBKSB group  ---
!
  RETURN
  END
