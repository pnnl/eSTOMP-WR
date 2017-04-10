module grid_mod
  integer MAX_DND_FLD, MAX_IND_FLD
  integer MAX_DCNX_FLD, MAX_ICNX_FLD
  parameter (MAX_DND_FLD = 500)
  parameter (MAX_IND_FLD = 500)
  parameter (MAX_DCNX_FLD = 500)
  parameter (MAX_ICNX_FLD = 500)
  parameter (MAX_DBCNX_FLD = 500)
  parameter (MAX_IBCNX_FLD = 500)
  type d_ptr
    double precision, pointer :: p(:)
  end type d_ptr
  type i_ptr
    integer, pointer :: p(:)
  end type i_ptr
  type d_2ptr
    double precision, pointer :: p(:,:)
  end type d_2ptr
  type i_2ptr
    integer, pointer :: p(:,:)
  end type i_2ptr
  type d_3ptr
    double precision, pointer :: p(:,:,:)
  end type d_3ptr
  type i_3ptr
    integer, pointer :: p(:,:,:)
  end type i_3ptr
!
!  list of pointers to fields on nodes and connections
!
  type(d_ptr), target :: d_nd_fld(MAX_DND_FLD)
  type(i_ptr), target :: i_nd_fld(MAX_IND_FLD)
  type(d_ptr), target :: d_cnx_fld(MAX_DCNX_FLD)
  type(i_ptr), target :: i_cnx_fld(MAX_ICNX_FLD)
  type(d_ptr), target :: d_bcnx_fld(MAX_DBCNX_FLD)
  type(i_ptr), target :: i_bcnx_fld(MAX_IBCNX_FLD)
  logical, target :: d_updt_flg(MAX_DND_FLD)
  logical, target :: i_updt_flg(MAX_IND_FLD)
!
  type(d_2ptr), target :: d_nd_2fld(MAX_DND_FLD)
  type(i_2ptr), target :: i_nd_2fld(MAX_IND_FLD)
  type(d_2ptr), target :: d_cnx_2fld(MAX_DCNX_FLD)
  type(i_2ptr), target :: i_cnx_2fld(MAX_ICNX_FLD)
  type(d_2ptr), target :: d_bcnx_2fld(MAX_DBCNX_FLD)
  type(i_2ptr), target :: i_bcnx_2fld(MAX_IBCNX_FLD)
  logical, target :: d2_updt_flg(MAX_DND_FLD)
  logical, target :: i2_updt_flg(MAX_IND_FLD)
!
  type(d_3ptr), target :: d_nd_3fld(MAX_DND_FLD)
  type(i_3ptr), target :: i_nd_3fld(MAX_IND_FLD)
  type(d_3ptr), target :: d_cnx_3fld(MAX_DCNX_FLD)
  type(i_3ptr), target :: i_cnx_3fld(MAX_ICNX_FLD)
  type(d_3ptr), target :: d_bcnx_3fld(MAX_DBCNX_FLD)
  type(i_3ptr), target :: i_bcnx_3fld(MAX_IBCNX_FLD)
  logical, target :: d3_updt_flg(MAX_DND_FLD)
  logical, target :: i3_updt_flg(MAX_IND_FLD)
!
  integer d_nd_2dim1(MAX_DND_FLD)
  integer d_nd_3dim1(MAX_DND_FLD)
  integer d_nd_3dim2(MAX_DND_FLD)
  integer i_nd_2dim1(MAX_IND_FLD)
  integer i_nd_3dim1(MAX_IND_FLD)
  integer i_nd_3dim2(MAX_IND_FLD)
!
  integer d_cnx_2dim1(MAX_DCNX_FLD)
  integer d_cnx_3dim1(MAX_DCNX_FLD)
  integer d_cnx_3dim2(MAX_DCNX_FLD)
  integer i_cnx_2dim1(MAX_ICNX_FLD)
  integer i_cnx_3dim1(MAX_ICNX_FLD)
  integer i_cnx_3dim2(MAX_ICNX_FLD)
!
  integer d_bcnx_2dim1(MAX_DBCNX_FLD)
  integer d_bcnx_3dim1(MAX_DBCNX_FLD)
  integer d_bcnx_3dim2(MAX_DBCNX_FLD)
  integer i_bcnx_2dim1(MAX_IBCNX_FLD)
  integer i_bcnx_3dim1(MAX_IBCNX_FLD)
  integer i_bcnx_3dim2(MAX_IBCNX_FLD)
!
  integer d2dim1_max, i2dim1_max
  integer d3dim1_max, i3dim1_max
  integer d3dim2_max, i3dim2_max
!
  integer, allocatable, target :: grid_mask(:)
  integer, allocatable, target :: local_node_list(:)
  integer, allocatable :: ga_mapc(:)
!
  integer dnode_field, inode_field
  integer dcnx_field, icnx_field
  integer dbcnx_field, ibcnx_field
!
  integer dnode_2field, inode_2field
  integer dcnx_2field, icnx_2field
  integer dbcnx_2field, ibcnx_2field
!
  integer dnode_3field, inode_3field
  integer dcnx_3field, icnx_3field
  integer dbcnx_3field, ibcnx_3field
!
!  field names
!
  character*32 d_nd_fld_names(MAX_DND_FLD)
  character*32 i_nd_fld_names(MAX_IND_FLD)
  character*32 d_cnx_fld_names(MAX_DCNX_FLD)
  character*32 i_cnx_fld_names(MAX_ICNX_FLD)
  character*32 d_bcnx_fld_names(MAX_DBCNX_FLD)
  character*32 i_bcnx_fld_names(MAX_IBCNX_FLD)
!
  character*32 d_nd_2fld_names(MAX_DND_FLD)
  character*32 i_nd_2fld_names(MAX_IND_FLD)
  character*32 d_cnx_2fld_names(MAX_DCNX_FLD)
  character*32 i_cnx_2fld_names(MAX_ICNX_FLD)
  character*32 d_bcnx_2fld_names(MAX_DBCNX_FLD)
  character*32 i_bcnx_2fld_names(MAX_IBCNX_FLD)
!
  character*32 d_nd_3fld_names(MAX_DND_FLD)
  character*32 i_nd_3fld_names(MAX_IND_FLD)
  character*32 d_cnx_3fld_names(MAX_DCNX_FLD)
  character*32 i_cnx_3fld_names(MAX_ICNX_FLD)
  character*32 d_bcnx_3fld_names(MAX_DBCNX_FLD)
  character*32 i_bcnx_3fld_names(MAX_IBCNX_FLD)
!
!  Dimensions of logical grid
!
  integer nxdim, nydim, nzdim
!
!  Dimensions of processor grid
!
  integer ga_px, ga_py, ga_pz
!
!  Dimensions of physical grid
!
  double precision xdim, ydim, zdim
  double precision dx, dy, dz
  double precision xmaxi,ymaxi,zmaxi
  double precision xmini,ymini,zmini
!
!  Corner locations of locally owned block
!
  integer ixmin, iymin, izmin, ixmax, iymax, izmax
  integer iaxmin, iaymin, iazmin, iaxmax, iaymax, iazmax
  integer ldx, ldy, ldz
!
!  GA handles of arrays used for updating ghost cell nodes
!
  integer ga_dbl, ga_int, ga_dbl2, ga_int2, ga_dbl3, ga_int3
!
!  Number of nodes, connections, boundary connections
!
  integer num_nodes, num_loc_nodes, num_cnx, num_bcnx
!
! ghost cell width
!
  integer gwidth
!
!  Useful pointers
!
  double precision, pointer :: d_xc(:),d_yc(:),d_zc(:)
  double precision :: ga_time
  double precision :: petsc_time
  integer :: petsc_iter
  integer :: stomp_iter
  integer :: stomp_nstep
  double precision :: ga_init_time
  double precision :: jcbp_time
  double precision :: petsc_init_time
  double precision :: petsc_sinit_time
  double precision :: chk_time
  double precision :: incrm_time
  double precision :: stomp_time
  double precision :: smc_time
  double precision :: tmpr_time
  double precision :: bcp_time
  double precision :: bcj_time
  double precision :: cisc_time
  double precision :: drcvl_time
  double precision :: bcf_time
  double precision :: ldo_time
  double precision :: average_time
  double precision :: tmstep_time
  double precision :: sorc_time
  double precision :: jcbwl_time
  double precision :: updt_time
  double precision :: rsdl_time
  double precision :: setup_time
  double precision :: initial_condition_time
  double precision :: eckechem_time
  double precision :: total_time
  double precision, dimension(:), allocatable :: sync_time
  double precision, dimension(:), allocatable :: sync_time1
  integer :: sync_iter1
  integer :: sync_iter

#ifdef USE_E4D
!-- Exclude E4D Procs from GA
    logical :: gae4d = .false.
    integer :: gagrp
#endif

end module grid_mod
