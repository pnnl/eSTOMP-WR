!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDBC1
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
!     Water Mode
!
!     Read input file for boundary condition information.
!
!     1 - Dirichlet
!     2 - Neumann
!     3 - Zero Flux
!     4 - Saturated
!     5 - Unit Gradient
!     6 - Free Gradient
!     7 - Outflow
!     8 - Aqueous Concentration
!     9 - Gas Concentration
!     10 - NAPL Concentration
!     11 - Hydraulic Gradient
!     12 - Initial Condition
!     13 - Inflow
!     14 - Inflow Aqueous-Phase
!     15 - Inflow Gas-Phase
!     16 - Inflow NAPL
!     17 - Seepage Face
!     18 - Convective
!     19 - Inflow-Outflow Volumetric
!     20 - Falling Head
!     21 - Falling Pond
!     22 - Free Boundary
!     23 - Inflow-Outflow Aqueous
!     24 - Potential Evapotranspiration
!     25 - Fluctuating Water Table
!     26 - Dirichlet-Outflow
!     27 - Diode
!     28 - Convective-Radiative
!     29 - Convective Ground Surface
!     30 - Shuttleworth-Wallace
!     31 - Bare Shuttleworth-Wallace
!     32 - Relative Saturation
!     33 - Inflow Relative Saturation
!     34 - Aqu. Rel. Sat.
!     35 - Inflow Aqu. Rel. Sat.
!     36 - Aqu. Mass Frac.
!     37 - Inflow Aqu. Mass Frac.
!     38 - Vol. Conc.
!     39 - Inflow Vol. Conc.
!     40 - Aqu. Conc.
!     41 - Inflow Aqu. Conc.
!     42 - Dirichlet-Inflow
!     43 - Inflow-Outflow Gas
!     44 - X-Y-Z Hydraulic Gradient
!     45 - X-Y-Z Seepage Face
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!
      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE GRID
      USE FILES
      USE CONST
      USE BCV
      USE GRID_MOD
      USE GRID
      USE BUFFEREDREAD
      USE BCVP 
      USE PLT_ATM
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
#include "utils.h"
!--- PETSc
!#include "include/finclude/petsc.h"
!#include "include/finclude/petscvec.h"
!#include "include/finclude/petscmat.h"
!#include "include/finclude/petscpc.h"
!#include "include/finclude/petscksp.h"
!#include "include/finclude/petscsys.h"
!#include "include/finclude/petscviewer.h"
!
!
!----------------------Parameter Statements----------------------------!
!
interface 
  subroutine get_node_mask(t_mask)
  use grid_mod
  implicit none
  integer, pointer :: t_mask(:) ! out
  end subroutine get_node_mask
end interface

interface
  subroutine get_local_node_list(t_list)
  use grid_mod
  implicit none
  integer, pointer :: t_list(:) ! out
  end subroutine get_local_node_list
end interface

interface
  subroutine get_bcnx_ifield(t_string, t_field,  &
  t_ok)
  use grid_mod
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:) ! out
  logical :: t_ok ! out
  end subroutine get_bcnx_ifield
end interface

interface
  subroutine get_bcnx_dfield( t_string, t_field,  &
  t_ok)
  use grid_mod
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:) ! out
  logical :: t_ok ! out
  end subroutine get_bcnx_dfield
end interface

interface
  subroutine get_node_ifield(t_string, t_field,  &
  t_ok)
  use grid_mod
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:) ! out
  logical :: t_ok ! out
  end subroutine get_node_ifield
end interface

!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM(LUK+LSOLU+1),FDUM,SDUM,EXTLINE
      CHARACTER*128 UNTS,TMP,PNAME
      CHARACTER*24 CHLB(3)
      CHARACTER*32 CHTYP(45)
      CHARACTER*512 CHDUM
      REAL*8 VAR(LBTM,LBCV)
      INTEGER ITYP(LUK+LSOLU+1)
      INTEGER IRDBCF(LBTM,LBCV)
      INTEGER IBCSPX(LSPBC+1)
      integer newnumx,newnumx1
      integer, dimension(:), allocatable :: ibcrdx
      integer, dimension(:), allocatable :: new_bcnx
      type(i_ptr), target :: b_id_
      type(d_ptr), target :: b_dist_
      type(d_ptr), target :: b_dx_
      type(d_ptr), target :: b_dy_
      type(d_ptr), target :: b_dz_
      type(d_ptr), target :: b_area_
      integer, dimension(:), allocatable :: ibcc_
      integer, dimension(:), allocatable :: ibcd_
      integer, dimension(:), allocatable :: ibcsn_
      integer, dimension(:), allocatable :: ibcm_
      integer, dimension(:), allocatable :: ibcbs_
      integer, dimension(:,:), allocatable :: ibct_
      integer, dimension(:), allocatable :: ibcin_
      integer, dimension(:), allocatable :: irefb_
      integer, dimension(:,:), allocatable :: ibcsp_
      double precision, dimension(:,:,:), allocatable :: bc_
!      type(d_ptr), target :: basex_
!      type(d_ptr), target :: basey_
!      type(d_ptr), target :: basez_
 
      integer, pointer :: i_id(:)
      integer, dimension(:,:), allocatable :: idx_buf
      integer, dimension(:), allocatable :: ival_buf
      real*8, dimension(:,:), allocatable :: val_buf
      integer, dimension(:,:), allocatable :: idx_bufw
      integer, dimension(:,:,:), allocatable :: ibuf3
      integer, dimension(:), allocatable :: ival_bufw
      real*8, dimension(:,:), allocatable :: val_bufw
      real*8, dimension(:,:,:), allocatable :: buf3
      real*8, dimension(:), allocatable :: val_buft
      integer, pointer :: i_bidi(:), i_bidj(:), i_bidk(:),i_bid(:),i_bfid(:)
      double precision, pointer :: d_barea(:),d_bdist(:)
      double precision, pointer :: d_bxsep(:),d_bysep(:),d_bzsep(:)
      integer, dimension(:,:), allocatable :: temp_f 
      integer lo(3),hi(3),ldxx(3),dims(3)
      integer,dimension(:), allocatable :: flag_n
      integer,dimension(:), allocatable :: flag_b
      integer,dimension(:), allocatable :: flag_i
!     working array for zero flux boundary
      real(kind=dp), dimension(:), allocatable :: areab_zfw
      real(kind=dp), dimension(:), allocatable :: uvxb_zfw
      real(kind=dp), dimension(:), allocatable :: uvyb_zfw
      real(kind=dp), dimension(:), allocatable :: uvzb_zfw
      real(kind=dp), dimension(:), allocatable :: xpb_zfw
      real(kind=dp), dimension(:), allocatable :: ypb_zfw
      real(kind=dp), dimension(:), allocatable :: zpb_zfw
      real(kind=dp), dimension(:), allocatable :: distb_zfw
      integer, dimension(:), allocatable :: bid_zfw
      integer, dimension(:,:,:), allocatable :: ifdx
      integer, dimension(:,:,:), allocatable :: ifdcx
  logical t_ok,status
  integer :: g_buf,three,idx
  real*8 :: zerox
  integer :: izero
  integer :: t_num ! out
  real*8 :: basep_min
  integer :: rbase,irfbx
  integer :: nhydrx
  integer :: buf3xw
  integer :: ix_1,jx_1,kx_1,lbc_x
  double precision:: t_e,t_b,t_b1,t_e1,t_b2,t_e2,time1x,time2x,time3x,t_b3,t_e3
  integer :: ierr
  integer :: pft
  integer :: dim1
  real*8 :: time, pet
  CHARACTER*64 :: time_unit,pet_unit
  ! Functions
  logical rd_sparse_ijkv
  LOGICAL :: use_ga
  real*8 ::z_up,z_bt
  real*8 :: epsl1=1.0E-12
  real*8, dimension(:,:), allocatable :: beta_tmp
  real(kind=dp), dimension(:), allocatable ::var_tmp
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Data Statements---------------------------------!
!
      SAVE CHLB
      DATA CHLB /'X-Direction Gradient, ','Y-Direction Gradient, ', &
               'Z-Direction Gradient, '/
      SAVE CHTYP
      DATA CHTYP /'Dirichlet','Neumann','Zero Flux','Saturated', &
      'Unit Gradient','Free Gradient','Outflow', &
      'Aqueous Concentration','Gas Concentration', &
      'NAPL Concentration','Hydraulic Gradient', &
      'Initial Condition','Inflow','Inflow Aqueous-Phase', &
      'Inflow Gas-Phase','Inflow NAPL','Seepage Face','Convective', &
      'Inflow-Outflow Volumetric','Falling Head','Falling Pond', &
      'Free Boundary','Inflow-Outflow Aqueous', &
      'Potential Evapotranspiration','Fluctuating Water Table', &
      'Dirichlet-Outflow','Diode', &
      'Convective-Radiative','Convective Ground Surface', &
      'Shuttleworth-Wallace','Bare Shuttleworth-Wallace', &
      'Relative Saturation','Inflow Relative Saturation', &
      'Aqu. Rel. Sat.','Inflow Aqu. Rel. Sat.','Aqu. Mass Frac.', &
      'Inflow Aqu. Mass Frac.','Vol. Conc.','Inflow Vol. Conc.', &
      'Aqu. Conc.','Inflow Aqu. Conc.','Dirichlet-Inflow', &
      'Inflow-Outflow Gas', &
      'X-Y-Z Hydraulic Gradient','X-Y-Z Seepage Face'/
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/RDBC1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
!
!      call add_node_ifield('ghost2localid',idx)
!      call get_node_mask(gridPort,t_ivec,excpt)
!      i_nd_fld(idx)%p => t_ivec%d_data
!      id_g2l => i_nd_fld(idx)%p

!
! Assign grid cell locations and volumes
!

      call get_node_mask(id_g2l)
      call get_local_node_list(id_l2g)
     dims(1) = nxdim
     dims(2) = nydim
     dims(3) = nzdim
     three = 3
     g_buf = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
     IF (GAE4D) CALL GA_SET_PGROUP(g_buf,GAGRP)
#endif
     call ga_set_data(g_buf, three, dims, MT_INT)
     status = ga_allocate(g_buf)
     zerox = 0.d0
     izero = 0
     call ga_fill(g_buf,izero)
     idim1 = LUK+LPH*LSOLU*LC+LR+LL+LG+LN
! array for time step calculation
     allocate(ibcc_t(lbc))
     allocate(ibcm_t(lbc))
     allocate(bc_t(lbtm,lbc))
!
     allocate(bcxyzg(lbtm,3))
     allocate(ibcc_(lbc))
     ibcc_ = 0
     allocate(ibcd_(lbc))
     ibcd_ = 0
     allocate(ibcsn_(lbc))
     ibcsn_ = 0
     allocate(ibcm_(lbc))
     ibcm_ = 0
     allocate(ibcbs_(lbc))
     ibcbs_ = 0
     allocate(ibct_(idim1,lbc))
     ibct_ = 0
     allocate(ibcin_(lbc))
     ibcin_ = 0
     allocate(irefb_(lbc))
     irefb_ = 0
     if( num_bcnx /=0 ) then
       allocate(flag_b(num_bcnx))
     else
       allocate(flag_b(1))
     endif
     allocate(flag_i(num_nodes))
     flag_b = 0     
     flag_i = 0
     IF( ISLC(40).EQ.1 ) THEN
       idim1 = lspbc+1
!       call add_bcnx_i2field('ibcsp',idim1,idx)
!       ibcsp => i_bcnx_2fld(idx)%p
       allocate(ibcsp_(idim1,lbc))
     endif
      ldxx(1) = iaxmax - iaxmin + 1
      ldxx(2) = iaymax - iaymin + 1
      ldxx(3) = iazmax - iazmin + 1
      lo(1) = iaxmin
      lo(2) = iaymin
      lo(3) = iazmin
      hi(1) = iaxmax
      hi(2) = iaymax
      hi(3) = iazmax
!      call nga_distribution(g_buf,me,lo,hi)
!      do i=1,3
!        ldxx(i) = hi(i) - lo(i) + 1
!      enddo
      call get_bcnx_ifield('bcnxi_id',i_bidi,t_ok)
!      i_bidi => t_ivec%d_data
      call get_bcnx_ifield('bcnxj_id',i_bidj,t_ok)
!      i_bidj => t_ivec%d_data
      call get_bcnx_ifield('bcnxk_id',i_bidk,t_ok)
!      i_bidk => t_ivec%d_data
      call get_bcnx_ifield('bcnx_id',i_bid,t_ok)
!      i_bid => t_ivec%d_data
      call get_bcnx_ifield('bcnxf_id',i_bfid,t_ok)
!      i_bfid => t_ivec%d_data
!
      call get_node_ifield('natural_id',i_id,t_ok)
!      i_id => t_ivec%d_data
!
!      call add_bcnx_dfield('b_area_w',idx)
!      d_barea => d_bcnx_fld(idx)%p
!      call add_bcnx_dfield('b_distance_w',idx)
!      d_bdist => d_bcnx_fld(idx)%p
!      call add_bcnx_dfield('b_x_separation_w',idx)
!      d_bxsep => d_bcnx_fld(idx)%p
!      call add_bcnx_dfield('b_y_separation_w',idx)
!      d_bysep => d_bcnx_fld(idx)%p
!      call add_bcnx_dfield('b_z_separation_w',idx)
!      d_bzsep => d_bcnx_fld(idx)%p
!

      call get_bcnx_dfield('b_area',d_barea,t_ok)
      call get_bcnx_dfield('b_distance',d_bdist,t_ok)
      call get_bcnx_dfield('b_x_separation',d_bxsep,t_ok)
      call get_bcnx_dfield('b_y_separation',d_bysep,t_ok)
      call get_bcnx_dfield('b_z_separation',d_bzsep,t_ok)
!
      if(.not.allocated(flag_n))allocate(flag_n(num_nodes))
      newnumx = 0
!
      CARD = 'Boundary Conditions Card'
      ICD = INDEX( CARD,'  ' )-1
      if(me.eq.0) WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      
            ! read roota and rootb for root profile
      if (lplant > 0) then
        allocate(veg_type(ldx*ldy))
        veg_type = 1
      endif
!
      NBC = 0
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Boundary Condition Cards: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      allocate(bc_(lbcv,lbtm,nlin))
      bc_ = 0.d0
!        call add_bcnx_d2field('basex',nlin,idx)
!        basex => d_bcnx_2fld(idx)%p
!        call add_bcnx_d2field('basey',nlin,idx)
!        basey => d_bcnx_2fld(idx)%p
!        call add_bcnx_d2field('basez',nlin,idx)
!        basez => d_bcnx_2fld(idx)%p
!        call add_bcnx_d2field('basep',nlin,idx)
!        basep => d_bcnx_2fld(idx)%p
       allocate(basex(nlin))
       allocate(basey(nlin))
       allocate(basez(nlin))
       allocate(basep(nlin))
       allocate(basen(nlin))
       basex = 0.d0
       basey = 0.d0
       basez = 0.d0
       basep = 0.d0
       basen(1:nlin) = 0
!     call add_bcnx_ifield('irefb',idx)
!     irefb => i_bcnx_fld(idx)%p
!      nbcx = 0
     nhydrx = 0    
      nb_t = nlin 

!      allocate(ifdx(ldxx(1),ldxx(2),ldxx(3)))
!      allocate(ifdcx(ldxx(1),ldxx(2),ldxx(3)))
      allocate(ifdx(iaxmin:iaxmax,iaymin:iaymax,iazmin:iazmax))
      allocate(ifdcx(iaxmin:iaxmax,iaymin:iaymax,iazmin:iazmax))
      DO 400 NB = 1, NLIN
        call ga_fill(g_buf,izero)
        call ga_igop(1,nhydrx,1,'max')
!call ga_sync
!       if (me.eq.0) then
        nbc = 0
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        IF( NB.NE.1 .and. me.eq.0) WRITE(IWR, '(/)')
!
!---    Read boundary orientation  ---
!
        ISTART = 1
        VARB = 'Boundary Condition Orientation: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF(ME.EQ.0) WRITE(IWR,'(/,A,$)') VARB(1:IVR)
        IBCSNX = 0
        IF( INDEX(ADUM(1:),'surface normal').NE.0 )  IBCSNX = 1
        IF( INDEX(ADUM(1:),'west').NE.0 ) THEN
          IBCDX = -1
          if(me.eq.0) WRITE(IWR,'(A)') 'X-Direction: West Surface'
        ELSEIF( INDEX(ADUM(1:),'east').NE.0 ) THEN
          IBCDX = 1
          if(me.eq.0) WRITE(IWR,'(A)') 'X-Direction: East Surface'
        ELSEIF( INDEX(ADUM(1:),'south').NE.0 ) THEN
          IBCDX = -2
          if(me.eq.0) WRITE(IWR,'(A)') 'Y-Direction: South Surface'
        ELSEIF( INDEX(ADUM(1:),'north').NE.0 ) THEN
          IBCDX = 2
          if(me.eq.0) WRITE(IWR,'(A)') 'Y-Direction: North Surface'
        ELSEIF( INDEX(ADUM(1:),'bottom').NE.0 ) THEN
          IBCDX = -3
          if(me.eq.0) WRITE(IWR,'(A)') 'Z-Direction: Bottom Surface'
        ELSEIF( INDEX(ADUM(1:),'top').NE.0 ) THEN
          IBCDX = 3
          if(me.eq.0) WRITE(IWR,'(A)') 'Z-Direction: Top Surface'
        ELSEIF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
          NCH = INDEX(FDUM,'  ')-1
          IF( ME.EQ.0 )THEN
            WRITE(IWR,'(/)')
            WRITE(ISC,'(/)')
            T_OK = OPENFILE( FDUM(1:NCH),IUNIT,0 )
          ENDIF
          ALLOCATE(TEMP_F(LBC,4))
          TEMP_F = 0
          IS = 1
          IE = 1
          JS = 1
          JE = 1
          KS = 1
          KE = 0
          T_OK = RD_SPARSE_IJKV(FDUM(1:NCH),TEMP_F(1,1),LBC,4,KE)
          call ga_igop(22,ke,1,'max')
            
        ENDIF
!
!---    Read boundary types  ---
!
        VARB = 'Boundary Condition Type: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM(1))
        IF( IEQC.GT.0 ) THEN
          DO 15 NSL = 1,NSOLU
!
!---        Allow for returns in input lines  ---
!
            CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            BDUM(NSL+LUK) = 'zero flux'
            IDFLT = 1
            VARB = 'Solute Boundary Condition Type: '
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM(NSL+LUK))
   15     CONTINUE
        ENDIF

!
!---    Reactive species boundary condition type  ---
!
          ibcspx = 0
        IF( ISLC(40).EQ.1 ) THEN
!
!---      Allow for returns in input lines  ---
!
          CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          BDUM(NSOLU+LUK+1) = 'zero flux'
          IDFLT = 1
          VARB = 'Reactive Species Boundary Condition Type: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM(NSOLU+LUK+1))
!
!---      Number of reactive species  ---
!
          ibcspx = 0
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Number of Reactive Species'
          CALL RDINT(ISTART,ICOMMA,CHDUM,IBCSPX(1))
          DO 16 NSPX = 2,IBCSPX(1)+1
            IBCSPX(NSPX) = 0
   16     CONTINUE
!
!---      Loop over number of reactive species  ---
!
          DO 20 NSPX = 1,IBCSPX(1)
!
!---        Allow for returns in input lines  ---
!
            CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            VARB = 'Species Name'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,SDUM)
!
!---        Aqueous species  ---
!
            DO 17 M = 1,NSPL
              IF( SPNML(M).EQ.SDUM ) THEN
                IBCSPX(NSPX+1) = M
                GOTO 18
              ENDIF
   17       CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Aqueous Species Name: ' &
             // SDUM(1:NCH)
            CALL WRMSGS( INDX )
   18       CONTINUE
   20     CONTINUE
        ENDIF

!
!---    Assign aqueous boundary condition type ---
!
        IF( INDEX(BDUM(1)(1:),'dirichlet').NE.0 ) THEN
           ITYP(IEQW) = 1
        ELSEIF( INDEX(BDUM(1)(1:),'neumann').NE.0 ) THEN
           ITYP(IEQW) = 2
        ELSEIF( INDEX(BDUM(1)(1:),'zero flux').NE.0 ) THEN
           ITYP(IEQW) = 3
        ELSEIF( INDEX(BDUM(1)(1:),'saturated').NE.0 ) THEN
           ITYP(IEQW) = 4
        ELSEIF( INDEX(BDUM(1)(1:),'unit gradient').NE.0 ) THEN
           ITYP(IEQW) = 5
        ELSEIF( INDEX(BDUM(1)(1:),'free gradient').NE.0 ) THEN
           ITYP(IEQW) = 6
        ELSEIF( INDEX(BDUM(1)(1:),'outflow').NE.0 ) THEN
           ITYP(IEQW) = 7
        ELSEIF( INDEX(BDUM(1)(1:),'hydraulic gradient').NE.0 ) THEN
           ITYP(IEQW) = 11
           IF( INDEX(BDUM(1)(1:),'x-y-z').NE.0 ) ITYP(IEQW) = 44
        ELSEIF( INDEX(BDUM(1)(1:),'initial cond').NE.0 ) THEN
           ITYP(IEQW) = 12
        ELSEIF( INDEX(BDUM(1)(1:),'seepage face').NE.0 ) THEN
           ITYP(IEQW) = 17
           IF( INDEX(BDUM(1)(1:),'x-y-z').NE.0 ) ITYP(IEQW) = 45
        ELSEIF( INDEX(BDUM(1)(1:),'falling head').NE.0 ) THEN
           ITYP(IEQW) = 20
        ELSEIF( INDEX(BDUM(1)(1:),'falling pond').NE.0 ) THEN
           ITYP(IEQW) = 21
        ELSEIF( INDEX(BDUM(1)(1:),'free boundary').NE.0 ) THEN
           ITYP(IEQW) = 22
        ELSEIF(( INDEX(BDUM(1)(1:),'poten').NE.0 .AND. &
                 INDEX(BDUM(1)(1:),'evap').NE.0 ) .OR. &
                 (INDEX(BDUM(1)(1:),'pet').NE.0)) THEN
           CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,PNAME)
           DO IP = 1, NPLANT
             IF (PNAME==PLANT(IP)) THEN
                IPFTX = IP
             ENDIF
           ENDDO
!           CALL RDINT(ISTART,ICOMMA,CHDUM,IPFTX)
           ITYP(IEQW) = 24*1000 + IPFTX
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Boundary Condition: '//BDUM(1)
          CALL WRMSGS( INDX )
        ENDIF
!
!---    Assign solute boundary condition type ---
!
        IF( IEQC.GT.0 ) THEN
          DO 25 NSL = 1,NSOLU
            IF( INDEX(BDUM(NSL+LUK)(1:), &
            'inflow-outflow vol').NE.0 ) THEN
              ITYP(NSL+LUK) = 19
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:), &
            'inflow-outflow aqu').NE.0 ) THEN
              ITYP(NSL+LUK) = 23
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'outflow').NE.0 ) THEN
              ITYP(NSL+LUK) = 7
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'initial cond').NE.0 ) THEN
              ITYP(NSL+LUK) = 12
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'inflow vol').NE.0 ) THEN
              ITYP(NSL+LUK) = 13
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'inflow aqu').NE.0 ) THEN
              ITYP(NSL+LUK) = 14
            ELSEIF(INDEX(BDUM(NSL+LUK)(1:), &
            'volumetric conc').NE.0 ) THEN
              ITYP(NSL+LUK) = 1
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'aqueous conc').NE.0 )THEN
              ITYP(NSL+LUK) = 8
            ELSEIF( INDEX(BDUM(NSL+LUK)(1:),'zero flux').NE.0 ) THEN
              ITYP(NSL+LUK) = 3
            ELSE
              INDX = 4
              CHMSG = 'Unrecognized Solute Boundary Condition: ' &
              //BDUM(NSL+LUK)
              CALL WRMSGS( INDX )
            ENDIF
   25     CONTINUE
        ENDIF

!
!---    Assign reactive species boundary condition type ---
!
        IF( ISLC(40).EQ.1 ) THEN
          IF( INDEX(BDUM(NSOLU+LUK+1)(1:), &
          'inflow-outflow aqu').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 23
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'outflow').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 7
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'initial co').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 12
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'inflow aqu').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 14
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'aqueous conc').NE.0 )THEN
            ITYP(NSOLU+LUK+1) = 8
          ELSEIF( INDEX(BDUM(NSOLU+LUK+1)(1:),'zero flux').NE.0 ) THEN
            ITYP(NSOLU+LUK+1) = 3
          ELSE
            INDX = 4
            CHMSG = 'Unrecognized Reactive Species Boundary Condition: ' &
            //BDUM(NSOLU+LUK)
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF

!
!---    Write boundary condition type(s) ---
!
        if(me.eq.0) WRITE(IWR,'(A,$)') 'Boundary Condition Type: '
        if (ITYP(IEQW)<24000) then
          if(me.eq.0) WRITE(IWR,'(2X,2A)') CHTYP(ITYP(IEQW)),' Aqueous'
          !print *, "IEQW=", IEQW, "ITYP(IEQW)=",ITYP(IEQW),&
          !  "CHTYP(ITYP(IEQW))=", CHTYP(ITYP(IEQW))
        else
          if(me.eq.0) WRITE(IWR,'(2X,2A)') CHTYP(24),' Aqueous'
          if(me.eq.0) WRITE(IWR,'(A,A)') 'Plant name: ',PLANT(ITYP(IEQW)-24000)
          !print *, "IEQW=", IEQW, "ITYP(IEQW)=",ITYP(IEQW),&
          !  "CHTYP(ITYP(IEQW))=", CHTYP(24)
        endif
!
!---    Write solute boundary condition type(s) ---
!
        IF( IEQC.GT.0 ) THEN
          DO 30 NSL = 1,NSOLU
            IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
            if(me.eq.0) WRITE(IWR,'(2X,2A)') CHTYP(ITYP(NSL+LUK)),SOLUT(NSL)(1:IDB)
   30     CONTINUE
        ENDIF

!
!---    Write species boundary condition type(s) ---
!
        IF( ISLC(40).EQ.1 ) THEN
          if(me.eq.0) WRITE(IWR,'(2X,2A)') CHTYP(ITYP(NSOLU+LUK+1)), &
          ' Reactive Species'
        ENDIF

!
!---  Read and write boundary domain indices  ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        IF( INDEX(ADUM(1:),'file').EQ.0 ) THEN
          VARB = 'Boundary Condition Domain: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,IS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,IE)
          CALL RDINT(ISTART,ICOMMA,CHDUM,JS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,JE)
          CALL RDINT(ISTART,ICOMMA,CHDUM,KS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,KE)
          if(me.eq.0) WRITE(IWR,'(A)') VARB(1:IVR)
          if(me.eq.0) WRITE(IWR, '(2X,A,I6,A,I6)') 'I = ',IS,' to ',IE
          if(me.eq.0) WRITE(IWR, '(2X,A,I6,A,I6)') 'J = ',JS,' to ',JE
          if(me.eq.0) WRITE(IWR, '(2X,A,I6,A,I6)') 'K = ',KS,' to ',KE
!
!---  Check boundary domain  ---
!
          IF( IS.GT.IE .OR. JS.GT.JE .OR. KS.GT.KE ) THEN
            INDX = 4
            CHMSG = 'Nonascending Boundary Condition Domain Indices'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IS.LT.1 .OR. IE.GT.nxdim .OR. JS.LT.1 .OR. &
          JE.GT.nydim .OR. KS.LT.1 .OR. KE.GT.nzdim ) THEN
            INDX = 4
            CHMSG = 'Illegal Boundary Condition Domain'
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---  Read number of boundary times  ---
!
        VARB = 'Number of Boundary Condition Times: '
        CALL RDINT(ISTART,ICOMMA,CHDUM,IBCMX)
        IF( IBCMX.LE.-3 ) THEN
          IBCCX = 1
          IBCMX = -IBCMX
          if(me.eq.0) WRITE(IWR,'(A)') 'Cyclic Boundary Conditions'
        ELSEIF( IBCMX.GE.1 ) THEN
          IBCCX = 0
          if(me.eq.0) WRITE(IWR,'(A)') 'Noncyclic Boundary Conditions'
        ELSEIF( IBCMX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'No Boundary Condition Times'
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          CHMSG = 'Number of Cyclic Boundary Conditions Times < 3'
          CALL WRMSGS( INDX )
        ENDIF
        IF( IBCMX.GT.LBTM ) THEN
          INDX = 5
          CHMSG = 'Number of Boundary Condition Times > LBTM'
          CALL WRMSGS( INDX )
        ENDIF
        BCTMO = -SMALL
        if(me.eq.0) &
        WRITE(IWR,'(A)') 'Boundary Condition Times and Variables:'


        DO 100 NTM = 1,IBCMX
          DO 40 M = 1,LBCV
            VAR(NTM,M) = 0.D+0
   40     CONTINUE
!
!---  Read, write, and convert boundary condition time, variables,
!     and units  ---
!
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,TMP)
          if (INDEX(TMP(1:),'file') /= 0) then
            if (ITYP(IEQW)>24000) then
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              NCH = INDEX(FDUM,'  ')-1
              IF (ME .EQ. 0) THEN
                T_OK = OPENFILE( FDUM(1:NCH),IUNIT,0 )
                ! loop through each line of the external file
                DO ILINE = 1,IBCMX
                  READ(IUNIT,'(4A)') CHDUM !time,time_unit,pet,pet_unit 
                  VARB = 'Boundary Time'
                  ISTART = 1
                  CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(ILINE,1))
                  CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                  if(me.eq.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),',',UNTS(1:NCH), &
                    ': ',VAR(ILINE,1)
                  INDX = 0
                  IUNS = 1
                  CALL RDUNIT(UNTS,VAR(ILINE,1),INDX)
                  if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(ILINE,1),', s)'
                  VARB = 'Potential Evapotranspiration Rate'
                  if(me.eq.0) WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
                  ISX = ISTART
                  ICX = ICOMMA
                  CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                  ISTART = ISX
                  ICOMMA = ICX
                  CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(ILINE,2))
                  CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                  if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(ILINE,2)
                  INDX = 0
                  IUNM = 1
                  IUNS = -1
                  CALL RDUNIT(UNTS,VAR(ILINE,2),INDX)
                  if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(ILINE,2),', m/s)'
                ENDDO
              ENDIF
              allocate(var_tmp(IBCMX))
              do il = 1,2
                var_tmp(1:IBCMX) = 0.0
                if (me==0) var_tmp(1:IBCMX) = VAR(1:IBCMX,il)
                call ga_dgop(10,var_tmp, IBCMX, 'max')
                if (ME .NE. 0) VAR(1:IBCMX,il) = var_tmp
              enddo
              deallocate(var_tmp)
!              CALL MPI_BCAST(VAR,2*IBCMX,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
              GO TO 101
            else
              IF (ME .EQ. 0) THEN
                WRITE(IWR,*) "ERROR: Read external boundary condition time file" 
                WRITE(IWR,*) "       is only available for PET calculation. "
                WRITE(ISC,*) "ERROR: Read external boundary condition time file"
                WRITE(ISC,*) "       is only available for PET calculation. "
              ENDIF
              STOP
            endif
          else
            ISTART = 1
            VARB = 'Boundary Time'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,1))         
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,1)
            NDX = 0
            IUNS = 1
            CALL RDUNIT(UNTS,VAR(NTM,1),INDX)
            if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,1),', s)'
            IF( ITYP(IEQW).EQ.1 ) THEN
              VARB = 'Aqueous Pressure, '
              if(me.eq.0) WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
              VAR(NTM,2) = VAR(NTM,2) - PATM
            ELSEIF( ITYP(IEQW).EQ.2 ) THEN
              VARB = 'Volumetric Aqueous Flux, '
              if(me.eq.0) WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = 1
              IUNS = -1
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', m^3/s)'
            ELSEIF( ITYP(IEQW).EQ.3 ) THEN
              VARB = 'Aqueous Pressure'
              if(me.eq.0) WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
              VAR(NTM,2) = VAR(NTM,2) - PATM
            ELSEIF( ITYP(IEQW).EQ.7 ) THEN
              VARB = 'Aqueous Pressure'
              if(me.eq.0) WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
              VAR(NTM,2) = VAR(NTM,2) - PATM
            ELSEIF( ITYP(IEQW).EQ.11 ) THEN
              VARB = 'Base Aqueous Pressure, '
              if(me.eq.0) WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
              VAR(NTM,2) = VAR(NTM,2) - PATM
            ELSEIF( ITYP(IEQW).EQ.12 ) THEN
              VARB = 'Dummy Variable, '
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            ELSEIF( ITYP(IEQW).EQ.17 ) THEN
              VARB = 'Base Aqueous Pressure, '
              if(me.eq.0) WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
              VAR(NTM,2) = VAR(NTM,2) - PATM
!
!---      Aqueous potential evaporation  ---
!
!          ELSEIF( ITYP(IEQW).EQ.24 ) THEN
            ELSEIF( ITYP(IEQW).EQ.24 .or. ityp(ieqw)>24000) THEN
              if (lplant >0) then
                VARB = 'Potential Evapotranspiration Rate'
                if(me.eq.0) WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
                INDX = 0
                IUNM = 1
                IUNS = -1
                CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', m/s)'
              else
                VARB = 'Maximum Capillary Head'
                if(me.eq.0) WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),', '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,3)
                INDX = 0
                IUNM = 1
                CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', m)'
              endif
! 
!---      X-Y-Z Hydraulic gradient 
!
            ELSEIF( ITYP(IEQW).EQ.44 ) THEN
              VARB = 'Base Aqueous Pressure, '
              if(me.eq.0) WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
              VAR(NTM,2) = VAR(NTM,2) - PATM
              DO 44 I = 1,3
                CALL RDDPR(ISTART,ICOMMA,CHDUM,BCXYZG(NTM,I))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                if(me.eq.0) WRITE(IWR,'(2X,4A,2PE11.4,$)') CHLB(I),', ', &
                UNTS(1:NCH),': ',BCXYZG(NTM,I)
                INDX = 0
                IUNM = -1
                CALL RDUNIT( UNTS,BCXYZG(NTM,I),INDX )
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',BCXYZG(NTM,I),', 1/m)'
   44         CONTINUE
!
!---      X-Y-Z Seepage face
!
            ELSEIF( ITYP(IEQW).EQ.45 ) THEN
              VARB = 'Base Aqueous Pressure, '
              if(me.eq.0) WRITE(IWR,'(2X,A,$)') VARB(1:IVR)
              ISX = ISTART
              ICX = ICOMMA
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
              ISTART = ISX
              ICOMMA = ICX
              CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              if(me.eq.0) &
              WRITE(IWR,'(2A,1PE11.4,$)') UNTS(1:NCH),': ',VAR(NTM,2)
              INDX = 0
              IUNM = -1
              IUNKG = 1
              IUNS = -2
              CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
              if(me.eq.0) &
                WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
              VAR(NTM,2) = VAR(NTM,2) - PATM
              DO 45 I = 1,3
                CALL RDDPR(ISTART,ICOMMA,CHDUM,BCXYZG(NTM,I))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                if(me.eq.0) WRITE(IWR,'(2X,4A,2PE11.4,$)') CHLB(I),', ', &
                UNTS(1:NCH),': ',BCXYZG(NTM,I)
                INDX = 0
                IUNM = -1
                CALL RDUNIT( UNTS,BCXYZG(NTM,I),INDX )
                if(me.eq.0) &
                WRITE(IWR,'(A,1PE11.4,A)') ' (',BCXYZG(NTM,I),', 1/m)'
   45         CONTINUE
            ELSE
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            ENDIF
          endif

          IF( IEQC.GT.0 ) THEN
            DO 50 NSL = 1,NSOLU
              IF( ITYP(NSL+LUK).EQ.1 .OR. ITYP(NSL+LUK).EQ.19 ) THEN
                VARB = 'Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                if(me.eq.0)&
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB), &
                VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
                VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSEIF( ITYP(NSL+LUK).EQ.8 .OR. ITYP(NSL+LUK).EQ.23 ) THEN
                VARB = 'Aqueous-Phase Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                if(me.eq.0) &
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB), &
                VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
                VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSEIF( ITYP(NSL+LUK).EQ.12 ) THEN
                VARB = 'Dummy Variable, '
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ELSEIF( ITYP(NSL+LUK).EQ.13 ) THEN
                VARB = 'Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                if(me.eq.0) &
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB), &
                VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
                VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSEIF( ITYP(NSL+LUK).EQ.14 ) THEN
                VARB = 'Aqueous-Phase Volumetric Concentration, '
                ISX = ISTART
                ICX = ICOMMA
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
                ISTART = ISX
                ICOMMA = ICX
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,NSL+LBCU))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SOLUT(NSL)(1:),'  ') - 1
                if(me.eq.0) &
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SOLUT(NSL)(1:IDB), &
                VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,NSL+LBCU)
                INDX = 0
                IUNM = -3
                CALL RDUNIT(UNTS,VAR(NTM,NSL+LBCU),INDX)
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
                VAR(NTM,NSL+LBCU),', 1/m^3)'
              ELSE
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ENDIF
   50       CONTINUE
          ENDIF

!
!---      Reactive species concentrations  ---
!
!          IF( ISLC(40).EQ.1 ) THEN
!
!---        Loop over reactive species inputs  ---
!
            DO 62 NSPX = 1,IBCSPX(1)
              NSP = IBCSPX(NSPX+1)
              M = NSOLU+LBCU+NSPX
!
!---          Initial input line  ---
!
              IF( NSPX.EQ.1 ) THEN
                T_OK = BUFFEREDREAD_GETLINE(CHDUM)
                CALL LCASE( CHDUM )
                ISTART = 1
              ENDIF
!
!---          Allow for returns in input lines  ---
!
              CALL CHKCHR( ISTART,ICOMMA,CHDUM,INDX )
              IF( INDX.EQ.0 ) THEN
                T_OK = BUFFEREDREAD_GETLINE(CHDUM)
                CALL LCASE( CHDUM )
                ISTART = 1
              ENDIF
              IF( ITYP(NSOLU+LUK+1).EQ.8  &
              .OR. ITYP(NSOLU+LUK+1).EQ.14 &
              .OR. ITYP(NSOLU+LUK+1).EQ.23 ) THEN
                VARB = 'Aqueous-Phase Concentration, '
                CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,M))
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                IDB = INDEX( SPNML(NSP)(1:),'  ') - 1
                if(me.eq.0) &
                WRITE(IWR,'(2X,A,2X,3A,1PE11.4,$)') SPNML(NSP)(1:IDB), &
                VARB(1:IVR),UNTS(1:NCH),': ',VAR(NTM,M)
                INDX = 0
                IUNM = -3
                IUNMOL = 1
                CALL RDUNIT(UNTS,VAR(NTM,M),INDX)
!
!---            Convert aqueous concentration from kmol/m^3 to
!               mol/m^3  ---
!
                VAR(NTM,M) = VAR(NTM,M)*1.D+3
                if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (', &
                VAR(NTM,M),', mol/m^3)'
              ELSEIF( ITYP(NSOLU+LUK+1).EQ.12 ) THEN
                VARB = 'Dummy Variable, '
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ELSE
                VARB = 'Dummy Variable, '
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
                CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
              ENDIF
   62       CONTINUE
!          ENDIF

!
!---  Check for nonascending boundary condition times  ---
!
          IF( VAR(NTM,1).LT.BCTMO ) THEN
            INDX = 4
            CHMSG = 'Boundary Condition Time Sequencing'
            CALL WRMSGS( INDX )
          ENDIF
          BCTMO = VAR(NTM,1)
  100   CONTINUE
  101   CONTINUE
!
!---    Assign values to boundary variables  ---
!
        ibcc_t(nb) = ibccx
        ibcm_t(nb) = ibcmx        
        do ntm = 1,ibcmx
          bc_t(ntm,nb) = var(ntm,1)
          do m=1,lbcu+nsolu+ibcspx(1)
             bc_(m,ntm,nb) = var(ntm,m)
          enddo
!---   Assign x-y-z gradient to boundary variables --
          if(abs(ityp(ieqw)).eq.44 .or. abs(ityp(ieqw)).eq.45) then
           do icnt = 1,3
            jcnt = lbcv - 3 + icnt
            bc_(jcnt,ntm,nb) = bcxyzg(ntm,icnt)
           enddo
          endif
        enddo
!
  108   CONTINUE
        icolx = 6+nsolu+1*islc(40)+lspbc+1*islc(40)
!        if (me.eq.0) then !restore
          if(.not.allocated(idx_buf))allocate(idx_buf(3,lbc))
          idx_buf = 0
          if(.not.allocated(val_buf))allocate(val_buf(icolx,lbc))
          if(.not.allocated(ival_buf))allocate(ival_buf(lbc))
          if(.not.allocated(val_buft)) allocate(val_buft(lbc))
          ival_buf = 0
          val_buf = 0.d0
          val_buft = 0.d0

!
!---  Assign values to boundary variables  ---
!
          NBCL = 0
!        NBCM(NB) = 0
!  t_b = MPI_Wtime()
time1x = 0.d0
time2x = 0.d0
time3x = 0.d0
          ifdx = 0
          ifdcx = 0
          DO 320 K = KS,KE
          DO 310 J = JS,JE
            DO 300 I = IS,IE
              NBCL = NBCL + 1
              IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
                ix = temp_f(k,1)
                jx = temp_f(k,2)
                kx = temp_f(k,3)
                ibcdx = temp_f(k,4)
              ELSE
                IX = I
                JX = J
                KX = K
              ENDIF
              nbc = nbc + 1
!
!--- one element in the global array could carry multiple value of boundary directions
!              if(me.eq.0) then
               idx_buf(1,nbc) = ix
               idx_buf(2,nbc) = jx
               idx_buf(3,nbc) = kx
              ncvalx = 0
              ifound = -1
              if(ibcdx.eq.-1) ibcdxw = 1
              if(ibcdx.eq.1) ibcdxw = 2
              if(ibcdx.eq.-2) ibcdxw = 3
              if(ibcdx.eq.2) ibcdxw = 4
              if(ibcdx.eq.-3) ibcdxw = 5
              if(ibcdx.eq.3) ibcdxw = 6
              if(ix.le.iaxmax.and.ix.ge.iaxmin.and. &
                jx.le.iaymax.and.jx.ge.iaymin.and. &
                kx.le.iazmax.and.kx.ge.iazmin) then
                ifound = me
                nbx = ifdx(ix,jx,kx)
              endif
              call ga_sync
              call ga_igop(2,ifound,1,'max')
              call ga_brdcst(3,ifound,sizeof(ifound),ifound)
              call ga_brdcst(4,nbx,sizeof(nbx),ifound)
              if(nbx.ne.0) then
                if(ifound.eq.me) then
                  ifdcx(ix,jx,kx) = ifdcx(ix,jx,kx)+1
                  ncvalx = ifdcx(ix,jx,kx)
                endif
                call ga_brdcst(5,ncvalx,sizeof(ncvalx),ifound)
                val_buf(1,nbx) = val_buf(1,nbx) + ibcdxw*10**ncvalx
                nbc = nbc -1
              endif
              if(ifound.eq.me) then
                if(ifdx(ix,jx,kx).eq.0)ifdx(ix,jx,kx) = nbc
              endif
              if(ncvalx.eq.0) then
                val_buf(1,nbc) = val_buf(1,nbc)+ibcdxw
                val_buf(2,nbc) = ibccx
                val_buf(3,nbc) = ibcsnx
                val_buf(4,nbc) = ityp(ieqw)
                if(ityp(ieqw).eq.11.and.nbcl.eq.1 ) val_buf(4,nbc) = -val_buf(4,nbc)
                if(ityp(ieqw).eq.17.and.nbcl.eq.1 ) val_buf(4,nbc) = -val_buf(4,nbc)
                if(ityp(ieqw).eq.44.and.nbcl.eq.1 ) val_buf(4,nbc) = -val_buf(4,nbc)
                if(ityp(ieqw).eq.45.and.nbcl.eq.1 ) val_buf(4,nbc) = -val_buf(4,nbc)
                val_buf(5,nbc) = nbcl
                val_buf(6,nbc) = ibcmx
!                if(me.eq.0)print *, 'nbc = ',val_buf(2,nbc),nbc
                ival_buf(nbc) = nbc
                i4x = 6
                do nsl = 1,nsolu
                  val_buf(i4x+nsl,nbc) = ityp(nsl+luk)
                enddo
                IF( ISLC(40).EQ.1 ) THEN
                  val_buf(i4x+nsolu+1,nbc) = ityp(nsolu+luk+1)
                  ifrst1 = i4x+nsolu+1
                  do nsp = 1,lspbc+1
                    val_buf(ifrst1+nsp,nbc) = ibcspx(nsp)
                  enddo
                endif
              endif
!  t_e3 = MPI_Wtime()
!time3x = time3x+t_e3-t_b3
  300       CONTINUE
  310     CONTINUE
  320     CONTINUE
!  t_e = MPI_Wtime()
          IF( INDEX(ADUM(1:),'file').NE.0 ) deallocate(temp_f)
      if(me.eq.0) then
        allocate(idx_bufw(3,lbc))
        idx_bufw = idx_buf
!  t_b = MPI_Wtime()
        call nga_scatter(g_buf,ival_buf,idx_bufw(1,1),nbc)
!  t_e = mpi_wtime()
        deallocate(idx_bufw)
      endif
      call ga_sync
!stop
      if(.not.allocated(new_bcnx)) then
        allocate(new_bcnx(lbc))
        new_bcnx = 0
      endif
      allocate(ibuf3(ldxx(1),ldxx(2),ldxx(3)))
      ibuf3 = 0 
! boundary conditions defined
      call nga_get(g_buf,lo,hi,ibuf3(1,1,1),ldxx)
      newnumx1 = newnumx + 1
      allocate(buf3(ldxx(1),ldxx(2),ldxx(3)))
      buf3 = 0.d0
      val_buft(1:nbc) = val_buf(1,1:nbc)
      do kx_1 = 1,ldxx(3)
      do jx_1 = 1,ldxx(2)
      do ix_1 = 1,ldxx(1)
        lbc_x = ibuf3(ix_1,jx_1,kx_1)
        if(lbc_x /= 0) then
          buf3(ix_1,jx_1,kx_1) = val_buft(lbc_x)
        endif
      enddo
      enddo
      enddo
      flag_n = 0
!
!---  Check domain boundary
!
      do icntx = 1,num_bcnx
        ix = i_bidi(icntx)
        jx = i_bidj(icntx)
        kx = i_bidk(icntx)
        buf3xw = int(buf3(ix,jx,kx))
        buf3x = 0 
        if(buf3xw.eq.6) buf3x = 3
        if(buf3xw.eq.5) buf3x = -3
        if(buf3xw.eq.4) buf3x = 2
        if(buf3xw.eq.3) buf3x = -2
        if(buf3xw.eq.2) buf3x = 1
        if(buf3xw.eq.1) buf3x = -1
        if(i_bfid(icntx).eq.int(buf3x)) then
          n = i_bid(icntx)
          newnumx = newnumx+1
          flag_n(n) = 1
          flag_b(icntx) = 1
!          flag_n(n) = newnumx
          new_bcnx(newnumx) = icntx
          ibcd_(newnumx) = buf3x
          ibcdx = ibcd_(newnumx)
!
!---  Check for boundary values applied to inactive nodes  ---
!
              IF( IXP(N).EQ.0 ) THEN
                INDX = 7
                IMSG = N
                CHMSG = 'Boundary Condition Applied to an Inactive Node'
                CALL WRMSGS( INDX )
              ENDIF
!
!---  Check for boundary values applied to interior surfaces  ---
!
              IERR = 0
              IF( IERR.EQ.1 ) THEN
                INDX = 7
                IMSG = NBC
                CHMSG = 'Boundary Cond. Applied to an Interior Surface' &
                //': Boundary Number'
                CALL WRMSGS( INDX )
              ENDIF
        endif
      enddo
!
!---  beyond inactive node
!
!  t_b1 = MPI_Wtime()
      icntx = num_bcnx
      do inodex = 1,num_loc_nodes
        n = id_l2g(inodex)
        if(flag_n(n).eq.0) then
        icnt = n-1
        ixx = mod(icnt,ldxx(1))
        icnt = (icnt-ixx)/ldxx(1)
        iyy = mod(icnt,ldxx(2))
        izz = (icnt-iyy)/ldxx(2)
        ixx = ixx+1
        iyy = iyy+1
        izz = izz+1
!
        buf3x = buf3(ixx,iyy,izz)
        if(int(buf3x).ne.0) then
          if(int(buf3x)/100000 > 0) then
            ndx = 6
          elseif(int(buf3x)/10000 > 0) then
            ndx = 5 
          elseif(int(buf3x)/1000 > 0) then
            ndx = 4
          elseif(int(buf3x)/100 > 0) then
            ndx = 3
          elseif(int(buf3x)/10 > 0) then 
            ndx = 2
          else
            ndx = 1
          endif
          flag_n(n) = 1 
          flag_i(n) = buf3x
          do idxw = ndx,1,-1
           newnumx = newnumx+1
           icntx = icntx+1
           new_bcnx(newnumx) = -n
           itenx = 10**(idxw-1)
           ibuf3xw = int(buf3x)/itenx
           if(ibuf3xw.eq.6) then
             ibcd_(newnumx) = 3
             buf3x = mod(int(buf3x),itenx)
           elseif(ibuf3xw.eq.5) then
             ibcd_(newnumx) = -3
             buf3x = mod(int(buf3x),itenx)
           elseif(ibuf3xw.eq.4) then
             ibcd_(newnumx) = 2
             buf3x = mod(int(buf3x),itenx)
           elseif(ibuf3xw.eq.3) then
             ibcd_(newnumx) = -2
             buf3x = mod(int(buf3x),itenx)
           elseif(ibuf3xw.eq.2) then
             ibcd_(newnumx) = 1
             buf3x = mod(int(buf3x),itenx)
           elseif(ibuf3xw.eq.1) then
             ibcd_(newnumx) = -1
             buf3x = mod(int(buf3x),itenx)
           endif
!           ibcdx = ibcd(newnumx)
          enddo
!
!---  Check for boundary values applied to inactive nodes  ---
!
              IF( IXP(N).EQ.0 ) THEN
                INDX = 7
                IMSG = N
                CHMSG = 'Boundary Condition Applied to an Inactive Node'
                CALL WRMSGS( INDX )
              ENDIF
!
!---  Check for boundary values applied to interior surfaces  ---
!
!              ix = ixx
!              jx = iyy
!              kx = izz
              IERR = 0
              IF( IERR.EQ.1 ) THEN
                INDX = 7
                IMSG = NBC
                CHMSG = 'Boundary Cond. Applied to an Interior Surface' &
                //': Boundary Number'
                CALL WRMSGS( INDX )
              ENDIF
        endif
        endif        
      enddo
!  t_e1 = MPI_Wtime()
!time2x = time2x+t_e1-t_b1
      itmx = 6
      ivarx = lbcu+nsolu+ibcspx(1) 
      isolux = 6 + nsolu + islc(40)
      ntmx = 0
      nslx = 0        
!      icolxx = 6+ibcmx*ivarx+nsolu+1*islc(40)+lspbc+1*islc(40)
      icolxx = 6+nsolu+1*islc(40)+lspbc+1*islc(40)
!---  reference node for hydraulic gradient condition
      basexx = 0.d0
      baseyx = 0.d0
      basezx = 0.d0
      do ivrx = 2,icolxx
       buf3 = 0.d0
!       call ga_fill(g_buf,zerox)
!       if (me.eq.0) then
!       val_buft(1:nbc) = val_buf(1:nbc,ivrx)
!        call nga_scatter(g_buf,val_buft,idx_buf(1,1),nbc)
!       endif
!       call ga_sync
!       call nga_get(g_buf,lo,hi,buf3(1,1,1),ldxx)
!  t_b = MPI_Wtime()
       val_buft(1:nbc) = val_buf(ivrx,1:nbc)
       lbc_x = 0
       do ix_1 = 1,ldxx(1)
       do jx_1 = 1,ldxx(2)
       do kx_1 = 1,ldxx(3)
        lbc_x = ibuf3(ix_1,jx_1,kx_1)
         if(lbc_x /= 0) then
           buf3(ix_1,jx_1,kx_1) = val_buft(lbc_x)
         endif
       enddo
       enddo
       enddo
!  t_e = MPI_Wtime()
!time1x = time1x+t_e-t_b
       icxr = 0

       if(ivrx.le.6) then
        do icx = newnumx1,newnumx
         irefb_(icx) = nb
         ibcin_(icx) = nb
         icntx = new_bcnx(icx)
         if(icntx.gt.0) then
          ix = i_bidi(icntx)
          jx = i_bidj(icntx)
          kx = i_bidk(icntx)
         else
          icnt = abs(icntx)-1
          ix = mod(icnt,ldxx(1))
          icnt = (icnt-ix)/ldxx(1)
          jx = mod(icnt,ldxx(2))
          kx = (icnt-jx)/ldxx(2)
          ix = ix+1
          jx = jx+1
          kx = kx+1
         endif
         if(ivrx.eq.2) ibcc_(icx) = buf3(ix,jx,kx)
         if(ivrx.eq.3) ibcsn_(icx) = buf3(ix,jx,kx)
         if(ivrx.eq.4) then
            ibct_(ieqw,icx) = int(buf3(ix,jx,kx))
! the first read boundary is set as base
            ibuf3x = val_buf(1,ibuf3(ix,jx,kx))
            if(ibuf3x > 10) then
            do is_ = 6,1,-1
              ibuf3x_ = mod(ibuf3x,10**is_)
              if(ibuf3x_.ne.val_buf(1,ibuf3(ix,jx,kx))) then
                val_buf(1,ibuf3(ix,jx,kx)) = ibuf3x_
                exit
              endif
            enddo
            endif
            if(ibuf3x > 10) ibct_(ieqw,icx) = abs(ibct_(ieqw,icx))
!            if(basen(nb) > 0) ibct_(ieqw,icx) = abs(ibct_(ieqw,icx))
            if(ibct_(ieqw,icx) .lt. 0.and.basen(nb) .eq.0) then
              nhydrx = nhydrx + 1
              basen(nb) = nhydrx
              icxr = icx
                          
             endif
         endif
         if(ivrx.eq.5) ibcbs_(icx) = buf3(ix,jx,kx) 
         if(ivrx.eq.6) ibcm_(icx) = buf3(ix,jx,kx) 
        enddo           
       elseif( ivrx.gt.itmx .and. ivrx.le. isolux) then
           mx = ivrx - itmx + luk
           if(mx.eq.0) mx = nsolu+islc(40)+luk
           do icx = newnumx1,newnumx
            icntx = new_bcnx(icx)
            if(icntx.gt.0) then
             ix = i_bidi(icntx)
             jx = i_bidj(icntx)
             kx = i_bidk(icntx)
             idx = i_bid(icntx)
            else
             icnt = abs(icntx)-1
             ix = mod(icnt,ldxx(1))
             icnt = (icnt-ix)/ldxx(1)
             jx = mod(icnt,ldxx(2))
             kx = (icnt-jx)/ldxx(2)
             ix = ix+1
             jx = jx+1
             kx = kx+1
             idx = icnt + 1
            endif
            ibct_(mx,icx) = buf3(ix,jx,kx)
           enddo
       elseif(ivrx.gt.isolux) then
           mx = ivrx-isolux
           if(mx.eq.0) mx = lspbc+1
           do icx = newnumx1,newnumx
            icntx = new_bcnx(icx)
            if(icntx.gt.0) then
             ix = i_bidi(icntx)
             jx = i_bidj(icntx)
             kx = i_bidk(icntx)
            else
             icnt = abs(icntx)-1
             ix = mod(icnt,ldxx(1))
             icnt = (icnt-ix)/ldxx(1)
             jx = mod(icnt,ldxx(2))
             kx = (icnt-jx)/ldxx(2)
             ix = ix+1
             jx = jx+1
             kx = kx+1
            endif
            ibcsp_(mx,icx) = buf3(ix,jx,kx)
           enddo
       endif
      enddo
       deallocate(idx_buf)
       deallocate(val_buf)
       deallocate(ival_buf)
      deallocate(buf3)
      deallocate(ibuf3)
  400 CONTINUE
!  t_b3 = MPI_Wtime()
      deallocate(ifdx)
      deallocate(ifdcx)
     status = ga_destroy(g_buf)
     allocate(b_id_%p(newnumx))
     allocate(b_dist_%p(newnumx))
     allocate(b_dx_%p(newnumx))
     allocate(b_dy_%p(newnumx))
     allocate(b_dz_%p(newnumx))
     allocate(b_area_%p(newnumx))
!     allocate(basex_%p(newnumx))
!     allocate(basey_%p(newnumx))
!     allocate(basez_%p(newnumx))
     do i=1,newnumx
        icntx = new_bcnx(i)
        if(icntx.gt.0) then
         b_id_%p(i) = i_bid(icntx)
         b_dist_%p(i) = d_bdist(icntx)
         b_dx_%p(i) = d_bxsep(icntx)
         b_dy_%p(i) = d_bysep(icntx)
         b_dz_%p(i) = d_bzsep(icntx)
         b_area_%p(i) = d_barea(icntx)
!         basex_%p(i) = basex(icntx)
!         basey_%p(i) = basey(icntx)
!         basez_%p(i) = basez(icntx)
        else
!         idx = i_id(abs(icntx))
         idx = abs(icntx)
!         print*, "i,icntx,idx:",i,icntx,abs(icntx)
         if(ibcd_(i).eq.-1) then
           jcnx = nd2cnx(1,idx)
           if(jcnx.eq.0) then
             jcnx = nd2cnx(2,idx)
             xdist = dist_dn(nd2cnx(2,idx))
           else
             xdist = dist_up(jcnx)
             nd2cnx(1,idx) = -i
           endif
         elseif(ibcd_(i).eq.1) then
           jcnx = nd2cnx(2,idx)
           if(jcnx.eq.0) then
             jcnx = nd2cnx(1,idx)
             xdist = dist_up(nd2cnx(1,idx))
           else
             xdist = dist_dn(jcnx)
             nd2cnx(2,idx) = -i
           endif
         elseif(ibcd_(i).eq.-2) then
           jcnx = nd2cnx(3,idx)
           if(jcnx.eq.0) then
             jcnx = nd2cnx(4,idx)
             xdist = dist_dn(nd2cnx(4,idx))
           else
             xdist = dist_up(jcnx)
             nd2cnx(3,idx) = -i
           endif
         elseif(ibcd_(i).eq.2) then
           jcnx = nd2cnx(4,idx)
           if(jcnx.eq.0) then
             jcnx = nd2cnx(3,idx)
             xdist = dist_up(nd2cnx(3,idx))
           else
             xdist = dist_dn(jcnx)
             nd2cnx(4,idx) = -i
           endif
         elseif(ibcd_(i).eq.-3) then
           jcnx = nd2cnx(5,idx)
           if(jcnx.eq.0) then
             xdist = dist_dn(nd2cnx(6,idx))
             jcnx = nd2cnx(6,idx)
           else
             xdist = dist_up(jcnx)
             nd2cnx(5,idx) = -i
           endif
         elseif(ibcd_(i).eq.3) then
           jcnx = nd2cnx(6,idx)
           if(jcnx.eq.0) then
             jcnx = nd2cnx(5,idx)
             xdist = dist_up(nd2cnx(5,idx))
           else
             xdist = dist_dn(jcnx)
             nd2cnx(6,idx) = -i
           endif
         endif
         xarea = areac(jcnx)
         b_area_%p(i) = xarea
         b_id_%p(i) = idx
         ibcdx = ibcd_(i)
         if(abs(ibcdx).eq.1) then
           b_dist_%p(i) = xdist
           if(ibcdx.eq.1) then
             b_dx_%p(i) = 1.d0
             b_dy_%p(i) = 0.d0
             b_dz_%p(i) = 0.d0
           else
             b_dx_%p(i) = -1.d0
             b_dy_%p(i) = 0.d0
             b_dz_%p(i) = 0.d0
           endif
         elseif(abs(ibcdx).eq.2) then
           b_dist_%p(i) = xdist
           if(ibcdx.eq.2) then
             b_dx_%p(i) = 0.d0
             b_dy_%p(i) = 1.d0
             b_dz_%p(i) = 0.d0
           else
             b_dx_%p(i) = 0.d0
             b_dy_%p(i) = -1.d0
             b_dz_%p(i) = 0.d0
           endif
         elseif(abs(ibcdx).eq.3) then
           b_dist_%p(i) = xdist
           if(ibcdx.eq.3) then
             b_dx_%p(i) = 0.d0
             b_dy_%p(i) = 0.d0
             b_dz_%p(i) = 1.d0
           else
             b_dx_%p(i) = 0.d0
             b_dy_%p(i) = 0.d0
             b_dz_%p(i) = -1.d0
           endif
         endif
        endif
     enddo
!stop
! generate no flux boundary condition
     nzfw = num_bcnx + 4*nxp    
     allocate(areab_zfw(nzfw))
     allocate(bid_zfw(nzfw))
     allocate(uvxb_zfw(nzfw))
     allocate(uvyb_zfw(nzfw))
     allocate(uvzb_zfw(nzfw))
     allocate(distb_zfw(nzfw))
     num_zf = 0
     do icx = 1,num_bcnx
       nx = i_bid(icx)
       if(ixp(nx) <= 0 .or. flag_b(icx) == 1 ) cycle  
       num_zf = num_zf + 1
       areab_zfw(num_zf) = d_barea(icx) 
       uvxb_zfw(num_zf) = d_bxsep(icx)
       uvyb_zfw(num_zf) = d_bysep(icx)
       uvzb_zfw(num_zf) = d_bzsep(icx)
       bid_zfw(num_zf) = nx
       distb_zfw(num_zf) = d_bdist(icx)
     enddo
     call delete_bcnx_field('b_area',t_ok)
     call delete_bcnx_field('b_distance',t_ok)
     call delete_bcnx_field('b_x_separation',t_ok)
     call delete_bcnx_field('b_y_separation',t_ok)
     call delete_bcnx_field('b_z_separation',t_ok)
     call delete_bcnx_field('bcnx_id',t_ok)
     call delete_bcnx_field('b_xcent',t_ok)
     call delete_bcnx_field('b_ycent',t_ok)
     call delete_bcnx_field('b_zcent',t_ok)
     call delete_bcnx_field('bcnxf_id',t_ok)
     call delete_bcnx_field('bcnxi_id',t_ok)
     call delete_bcnx_field('bcnxj_id',t_ok)
     call delete_bcnx_field('bcnxk_id',t_ok)
!BH
     call delete_bcnx_field('plb',t_ok)
     call delete_bcnx_field('pgb',t_ok)
     call delete_bcnx_field('q_flux_b',t_ok)
!BH
     num_bcnx = newnumx
!---  reallocate variables for boundary conditions
!
    if(num_bcnx.ne.0) then
     call add_bcnx_dfield('b_area',idx)
     d_bcnx_fld(idx)%p => b_area_%p
     call add_bcnx_dfield('b_distance',idx)
     d_bcnx_fld(idx)%p => b_dist_%p
     call add_bcnx_dfield('b_x_separation',idx)
     d_bcnx_fld(idx)%p => b_dx_%p
     call add_bcnx_dfield('b_y_separation',idx)
     d_bcnx_fld(idx)%p => b_dy_%p
     call add_bcnx_dfield('b_z_separation',idx)
     d_bcnx_fld(idx)%p => b_dz_%p
!
     call add_bcnx_ifield('bcnx_id',idx)
     i_bcnx_fld(idx)%p => b_id_%p
     call add_bcnx_ifield('ibcn',idx)
     i_bcnx_fld(idx)%p => b_id_%p
     ibcn => i_bcnx_fld(idx)%p
!BH
     idim1 = LUK+LPH*LSOLU*LC+LR+LL+LG+LN
     call add_bcnx_d2field('plb',idim1,idx)
      plb => d_bcnx_2fld(idx)%p
      plb = 0.d0
     call add_bcnx_d2field('pgb',idim1,idx)
      pgb => d_bcnx_2fld(idx)%p
      pgb = 0.d0
     call add_bcnx_d2field('q_flux_b',idim1,idx)
      q_flux_b => d_bcnx_2fld(idx)%p
      q_flux_b = 0.d0
 
   
!BH


!x     call add_bcnx_d3field('bc',lbcv,lbtm,idx)
!x     bc => d_bcnx_3fld(idx)%p
     allocate(bc(lbcv,lbtm,nlin))
     bc(1:lbcv,1:lbtm,1:nlin) = bc_(1:lbcv,1:lbtm,1:nlin)
     call add_bcnx_ifield('ibcc',idx)
     ibcc => i_bcnx_fld(idx)%p
     ibcc(1:num_bcnx) = ibcc_(1:num_bcnx)
     call add_bcnx_ifield('ibcd',idx)
     ibcd => i_bcnx_fld(idx)%p
     ibcd(1:num_bcnx) = ibcd_(1:num_bcnx)
     call add_bcnx_ifield('ibcsn',idx)
     ibcsn => i_bcnx_fld(idx)%p
     ibcsn(1:num_bcnx) = ibcsn_(1:num_bcnx)
     call add_bcnx_ifield('ibcm',idx)
     ibcm => i_bcnx_fld(idx)%p
     ibcm(1:num_bcnx) = ibcm_(1:num_bcnx)
     call add_bcnx_ifield('ibcbs',idx)
     ibcbs => i_bcnx_fld(idx)%p
     ibcbs(1:num_bcnx) = ibcbs_(1:num_bcnx)
     idim1 = LUK+LPH*LSOLU*LC+LR+LL+LG+LN
     call add_bcnx_i2field('ibct',idim1,idx)
     ibct => i_bcnx_2fld(idx)%p
     ibct(1:idim1,1:num_bcnx) = ibct_(1:idim1,1:num_bcnx)
     call add_bcnx_ifield('ibcin',idx)
     ibcin => i_bcnx_fld(idx)%p
     ibcin(1:num_bcnx) = ibcin_(1:num_bcnx)
     call add_bcnx_ifield('irefb',idx)
     irefb => i_bcnx_fld(idx)%p
     irefb(1:num_bcnx) = irefb_(1:num_bcnx)
     IF( ISLC(40).EQ.1 ) THEN
       idim1 = lspbc+1
       call add_bcnx_i2field('ibcsp',idim1,idx)
       ibcsp => i_bcnx_2fld(idx)%p
       ibcsp(1:idim1,1:num_bcnx) = ibcsp_(1:idim1,1:num_bcnx)
     endif
    endif
     deallocate(ibcc_)
     deallocate(ibcd_)
     deallocate(ibcsn_)
     deallocate(ibcm_)
     deallocate(ibcbs_)
     deallocate(ibct_)
     deallocate(ibcin_)
     deallocate(irefb_)
     if(islc(40).eq.1) deallocate(ibcsp_)
     
     deallocate(bc_)
     if(allocated(new_bcnx)) deallocate(new_bcnx)
      rbase = 0
      do nb = 1, nlin
        call ga_igop(5,basen(nb),1,'max')
        if(basen(nb).gt.0) rbase = rbase+1
      enddo
      allocate(base_node(rbase))
      base_node(1:rbase) = 0
      basex(1:rbase) = 0.d0
      basey(1:rbase) = 0.d0
!      basez(1:rbase) = 0.d0
      basez(1:rbase) = -99999.d0
      irfbx = 0
      do icx=1,num_bcnx
        irfbx = irefb(icx)
        if(basen(irfbx).ne.0) then
          n_bx = basen(irfbx) 
          irefb(icx) = n_bx
          nx = ibcn(icx)
          if(ibct(ieqw,icx) < 0) then
!--- x-y-z BC need basex and basey
            basex(n_bx) = xp(nx)
            basey(n_bx) = yp(nx)
            basez(n_bx) = zp(nx)
            if(ibcd(icx).eq.-1) then
              basex(n_bx) = xp(nx) - b_dist_%p(icx)
            elseif(ibcd(icx).eq.1) then
              basex(n_bx) = xp(nx) + b_dist_%p(icx)
            endif
            if(ibcd(icx).eq.-2) then
              basey(n_bx) = yp(nx) - b_dist_%p(icx)
            elseif(ibcd(icx).eq.2) then
              basey(n_bx) = yp(nx) + b_dist_%p(icx)
            endif
            if(ibcd(icx).eq.-3) then
              basez(n_bx) = zp(nx) - b_dist_%p(icx)
            elseif(ibcd(icx).eq.3) then
              basez(n_bx) = zp(nx) + b_dist_%p(icx)
            endif
          endif       
!          base_node(irfbx) = ibcn(icx)
        else
          irefb(icx) = 0
        endif
      enddo
      do irfbx = 1,rbase
!--- x-y-z
          call ga_dgop(6,basex(irfbx),1,'max')
          call ga_dgop(7,basey(irfbx),1,'max')
          call ga_dgop(8,basez(irfbx),1,'max')
          call locate(hgz_table_z,nzdim,basez(irfbx),ix)
          hgz_table_z(ix) = basez(irfbx)
      enddo
      
!      call ga_sync
      allocate(hgz_table_p(nzdim,rbase))
      hgz_table_p = 0.d0
! set array for node vs boundary connection
     do icx = 1, num_bcnx
       nx = ibcn(icx)
       if(nd2cnx(1,nx).eq.0.and.ibcd(icx).eq.-1) then
         nd2cnx(1,nx) = -icx
       elseif(nd2cnx(2,nx).eq.0.and.ibcd(icx).eq.1) then
         nd2cnx(2,nx) = -icx
       elseif(nd2cnx(3,nx).eq.0.and.ibcd(icx).eq.-2) then
         nd2cnx(3,nx) = -icx
       elseif(nd2cnx(4,nx).eq.0.and.ibcd(icx).eq.2) then
         nd2cnx(4,nx) = -icx
       elseif(nd2cnx(5,nx).eq.0.and.ibcd(icx).eq.-3) then
         nd2cnx(5,nx) = -icx
       elseif(nd2cnx(6,nx).eq.0.and.ibcd(icx).eq.3) then
         nd2cnx(6,nx) = -icx
       endif
     enddo
!find internal zero flux b/c of inactive nodes
     do icx=1,num_cnx
       id_up = conn_up(icx)
       id_dn = conn_dn(icx)
       if(ixp(id_up) <= 0 .and. ixp(id_dn) <= 0) cycle
       if(ixp(id_up) > 0) buf3x = flag_i(id_up)
       if(ixp(id_dn) > 0) buf3x = flag_i(id_dn)          
       if(int(buf3x)/100000 > 0) then
            ndx = 6
       elseif(int(buf3x)/10000 > 0) then
            ndx = 5 
       elseif(int(buf3x)/1000 > 0) then
            ndx = 4
       elseif(int(buf3x)/100 > 0) then
            ndx = 3
       elseif(int(buf3x)/10 > 0) then 
            ndx = 2
       else
            ndx = 1
       endif
       if6x = 0
       if5x = 0
       if4x = 0
       if3x = 0
       if2x = 0
       if1x = 0
       do idxw = ndx,1,-1
           itenx = 10**(idxw-1)
           ibuf3xw = int(buf3x)/itenx

           if(ibuf3xw.eq.6) then
             buf3x = mod(int(buf3x),itenx)
             if6x = ibuf3xw
           elseif(ibuf3xw.eq.5) then
             buf3x = mod(int(buf3x),itenx)
             if5x = ibuf3xw
           elseif(ibuf3xw.eq.4) then
             buf3x = mod(int(buf3x),itenx)
             if4x = ibuf3xw
           elseif(ibuf3xw.eq.3) then
             buf3x = mod(int(buf3x),itenx)
             if3x = ibuf3xw
           elseif(ibuf3xw.eq.2) then
             buf3x = mod(int(buf3x),itenx)
             if2x = ibuf3xw
           elseif(ibuf3xw.eq.1) then
             if1x = ibuf3xw
           endif
       enddo
       x_up = xp(id_up)
       y_up = yp(id_up)
       z_up = zp(id_up)
       x_dn = xp(id_dn)
       y_dn = yp(id_dn)
       z_dn = zp(id_dn)
       if(ixp(id_up) <= 0) then
           if(x_dn /= x_up .and. if2x /= 0) cycle
           if(y_dn /= y_up .and. if4x /= 0) cycle
           if(z_dn /= z_up .and. if6x /= 0) cycle
           num_zf = num_zf + 1
           areab_zfw(num_zf) = areac(icx) 
           uvxb_zfw(num_zf) = unvxc(icx)
           uvyb_zfw(num_zf) = unvyc(icx)
           uvzb_zfw(num_zf) = unvzc(icx)
           bid_zfw(num_zf) = id_dn
           distb_zfw(num_zf) = dist_dn(icx)
       elseif(ixp(id_dn) <= 0) then
           if(x_dn /= x_up .and. if1x /= 0) cycle
           if(y_dn /= y_up .and. if3x /= 0) cycle
           if(z_dn /= z_up .and. if5x /= 0) cycle
           num_zf = num_zf + 1
           areab_zfw(num_zf) = areac(icx) 
           uvxb_zfw(num_zf) = -unvxc(icx)
           uvyb_zfw(num_zf) = -unvyc(icx)
           uvzb_zfw(num_zf) = -unvzc(icx)
           bid_zfw(num_zf) = id_up
           distb_zfw(num_zf) = dist_up(icx)
       endif
     enddo
     if(num_zf > 0) then
       allocate(xpb_zf(num_zf))
       allocate(ypb_zf(num_zf))
       allocate(zpb_zf(num_zf))
       xpb_zf = 0.d0
       ypb_zf = 0.d0
       zpb_zf = 0.d0
       allocate(areab_zf(num_zf))
       allocate(uvxb_zf(num_zf))
       allocate(uvyb_zf(num_zf))
       allocate(uvzb_zf(num_zf))
       allocate(bid_zf(num_zf))
       areab_zf(1:num_zf) = areab_zfw(1:num_zf)
       uvxb_zf(1:num_zf) = uvxb_zfw(1:num_zf)
       uvyb_zf(1:num_zf) = uvyb_zfw(1:num_zf)
       uvzb_zf(1:num_zf) = uvzb_zfw(1:num_zf)
       bid_zf(1:num_zf) = bid_zfw(1:num_zf)
     endif
     do icx=1,num_zf
       idx = bid_zf(icx)
       if(uvxb_zf(icx) == -1) then
         xpb_zf(icx) = xp(idx) - distb_zfw(icx)
         ypb_zf(icx) = yp(idx)
         zpb_zf(icx) = zp(idx)
       elseif(uvxb_zf(icx) == 1) then
         xpb_zf(icx) = xp(idx) + distb_zfw(icx)
         ypb_zf(icx) = yp(idx)
         zpb_zf(icx) = zp(idx)
       endif  
       if(uvyb_zf(icx) == -1) then
         xpb_zf(icx) = xp(idx)
         ypb_zf(icx) = yp(idx) - distb_zfw(icx)
         zpb_zf(icx) = zp(idx)
       elseif(uvyb_zf(icx) == 1) then
         xpb_zf(icx) = xp(idx)
         ypb_zf(icx) = yp(idx) + distb_zfw(icx)
         zpb_zf(icx) = zp(idx)
       endif
       if(uvzb_zf(icx) == -1) then
         xpb_zf(icx) = xp(idx)
         ypb_zf(icx) = yp(idx)
         zpb_zf(icx) = zp(idx) - distb_zfw(icx)
       elseif(uvzb_zf(icx) == 1) then
         xpb_zf(icx) = xp(idx)
         ypb_zf(icx) = yp(idx)
         zpb_zf(icx) = zp(idx) + distb_zfw(icx)
       endif
     enddo 
     deallocate(areab_zfw)
     deallocate(uvxb_zfw)
     deallocate(uvyb_zfw)
     deallocate(uvzb_zfw)
     deallocate(bid_zfw)
     deallocate(distb_zfw)
     do icxx = 1, num_zf
       nx = bid_zf(icxx)
       icx = icxx+num_bcnx
       if(nd2cnx(1,nx).eq.0) then
         nd2cnx(1,nx) = -icx
       elseif(nd2cnx(2,nx).eq.0) then
         nd2cnx(2,nx) = -icx
       elseif(nd2cnx(3,nx).eq.0) then
         nd2cnx(3,nx) = -icx
       elseif(nd2cnx(4,nx).eq.0) then
         nd2cnx(4,nx) = -icx
       elseif(nd2cnx(5,nx).eq.0) then
         nd2cnx(5,nx) = -icx
       elseif(nd2cnx(6,nx).eq.0) then
         nd2cnx(6,nx) = -icx
       endif
     enddo
     if(lplant == 1) then
       call add_bcnx_ifield('ibpft',idx)
       ibpft => i_bcnx_fld(idx)%p
       ibpft = 0
       call add_node_dfield('root_fr', idx)
       root_fr => d_nd_fld(idx)%p
       root_fr = 0.d0
       dim1 = lsv
       call add_node_d2field('wilt_factor', dim1, idx)
       wiltf => d_nd_2fld(idx)%p
       wiltf = 0.d0
       call add_node_d2field('veg_sink', dim1, idx)
       veg_sink => d_nd_2fld(idx)%p
       veg_sink = 0.d0
       call add_node_d2field('evap_trans', dim1, idx)
       evap_trans => d_nd_2fld(idx)%p
       evap_trans = 0.d0
       allocate(veg_varx(4,ldx*ldy))
       veg_varx = 0.d0
       allocate(veg_bc(ldx*ldy))
       veg_bc = 0.d0
       allocate(crf(ldx*ldy))
       crf = 0.d0
!
       do i=1,num_bcnx
         if(ibct(ieqw,i) > 24000) then
           nx  = ibcn(i)
           zsurf = zp(nx) + b_dist_%p(i)
           ibpft(i) = ibct(ieqw,i) - 24000
           ibct(ieqw,i) = 24
           call parse_id_local(nx,ix_x,iy_x,iz_x)
           lndx = ix_x + (iy_x-1)*ldx
           veg_varx(1,lndx) = zsurf
           veg_varx(2,lndx) = ix_x
           veg_varx(3,lndx) = iy_x
           veg_varx(4,lndx) = ibpft(i)
         endif
       enddo
       do lndx = 1,ldx*ldy
         if(veg_varx(2,lndx) > 0.d0) then
           zsurf = veg_varx(1,lndx)
           ix = int(veg_varx(2,lndx))
           iy = int(veg_varx(3,lndx))
           pft = int(veg_varx(4,lndx))
           do izz = 1, ldz
             ndx = ix + (iy-1)*ldx + (izz-1) * ldx * ldy
             if(id_g2l(ndx) > 0 .and. ixp(ndx)>0) then
               if ((rsd_p(1,pft) == 0.d0) .or. &
                  ((rsd_p(1,pft) > 0.d0).and. &
                      ((zsurf-(zp(ndx)-dzgf(ndx)/2.d0))<=rsd_p(1,pft)))) then
                 z_up = zsurf-(zp(ndx) + dzgf(ndx)/2.d0)
                 z_bt = zsurf-(zp(ndx) - dzgf(ndx)/2.d0)
                 if(z_up >= 0.0 .and. z_bt >=0.0) then
                    if (IPLF_P(pft) == 3 .or. IPLF_P(pft) == 4) then
                      call zeng(pft,z_up,z_bt,root_fr(ndx))
                      crf(lndx) = crf(lndx) + root_fr(ndx)
                    endif
                 endif
               endif
             endif
           enddo
         endif
       enddo
       do lndx = 1,ldx*ldy
         if(veg_varx(2,lndx) > 0.d0) then
           zsurf = veg_varx(1,lndx)
           ix = int(veg_varx(2,lndx))
           iy = int(veg_varx(3,lndx))
           pft = int(veg_varx(4,lndx))
           if (IPLF_P(pft) == 4) then
             do izz = 1, ldz
               ndx = ix + (iy-1)*ldx + (izz-1) * ldx * ldy
               if(id_g2l(ndx) > 0 .and. ixp(ndx)>0) then
                 if ((rsd_p(1,pft) == 0.d0) .or. &
                    ((rsd_p(1,pft) > 0.d0).and. &
                      ((zsurf-(zp(ndx)-dzgf(ndx)/2.d0))<=rsd_p(1,pft)))) then
                    z_up = zsurf-(zp(ndx) + dzgf(ndx)/2.d0)
                    z_bt = zsurf-(zp(ndx) - dzgf(ndx)/2.d0)
                    if (z_up >= 0.0 .and. z_bt >=0.0) then
                      root_fr(ndx) = root_fr(ndx)*rsd_p(4,pft)/crf(lndx)
                    endif
                 endif
               endif
             enddo
           endif
         endif
       enddo


 
     endif
     ICSN = ICSN-ICSNX
     SUBNM = SUBNM(1:ICSN)
!
!---  End of RDBC1 group  ---
!
      RETURN
      END

subroutine locate(xx,n,x,j)
!locate position in a ordered table
 implicit none
!
 integer :: jj, ju, jl, jm,j,n
 double precision, dimension(n) :: xx
 double precision :: x
!
 jl=0
 ju=n+1
 10    if(ju-jl.gt.1)then
 jm=(ju+jl)/2
 if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
   jl=jm
 else
   ju=jm
 endif
 go to 10
 endif
 j=jl
 if(j.eq.0) j=1
 return
end subroutine locate

subroutine parse_id_local(it,ix,iy,iz)
  use grid_mod
  implicit none
  integer i, it, ix, iy, iz
  i = it - 1
  ix = mod(i,ldx)
  i = (i-ix)/ldx
  iy = mod(i,ldy)
  iz = (i-iy)/ldy
  ix = ix + 1
  iy = iy + 1
  iz = iz + 1
end subroutine parse_id_local
