!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE INTLZ
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
!     Initialize all variables contained in common blocks.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, December 1992.
!     Last Modified by MD White, PNNL, October 15, 1997.
!     Last Modified by MD White, PNNL, 1 August 2002.
!     Last Modified by SK Wurstner, PNNL, December 04, 2007.




!     $Id: intlz.F,v 1.53 2008/02/13 01:03:00 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE UCODE
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE REACT
      USE PORMED
      USE POINTE
      USE JACOB
      USE HYST
      USE GRID
      USE FLUXP
      USE FILES
      USE FDVSO
      USE FDVP
      USE FDVH
      USE CONST
      use grid_mod
      use outpu
      USE coup_well
!  use sidl
!  use sidl_NotImplementedException
!  use sidl_BaseInterface
!  use sidl_RuntimeException
!  use gov_cca_Port
!  use grid_GridPort
!  use stmpgrid_GAGrid
!  use stmpgrid_GAGrid_impl
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
interface 
  subroutine get_node_dfield(t_string, t_field, t_ok)
  use grid_mod
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:)
  logical :: t_ok ! out
  end subroutine get_node_dfield
end interface

interface
  subroutine get_cnx_ifield(t_string, t_field,  &
  t_ok)
  use grid_mod
  implicit none
  character (len=*) :: t_string ! in
  integer, pointer :: t_field(:) ! out
  logical :: t_ok ! out
  end subroutine get_cnx_ifield
end interface 

interface
  subroutine get_cnx_dfield( t_string, t_field,  &
  t_ok)
  use grid_mod
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:) ! out
  logical :: t_ok ! out
  end subroutine get_cnx_dfield
end interface

!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      LOGICAL FLG_EX
      integer dim1, dim2, num_nodes_w
!  type(stmpgrid_GAGrid_t) :: gridPort ! in
!  type (grid_GridPort_t)    :: gridPort

!  type(sidl_BaseInterface_t) :: excpt ! out
!  type(sidl_double_1d) :: t_rvec
!  type(sidl_int_1d) :: t_ivec
  logical :: t_ok
  LOGICAL :: use_ga
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/INTLZ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(71)(1:1),'$').EQ.0 ) CVS_ID(71) = &
     '$Id: intlz.F,v 1.53 2008/02/13 01:03:00 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
  call get_node_dfield('xcent',xp,t_ok)
!  xp => t_rvec%d_data
  call get_node_dfield('ycent',yp,t_ok)
!  yp => t_rvec%d_data
  call get_node_dfield('zcent',zp,t_ok)
!  zp => t_rvec%d_data
  call get_node_dfield('volume',vol,t_ok)
!  vol => t_rvec%d_data
!      call update_nodes('xcent',1,0,1)
!      call update_nodes('ycent',1,0,1)
!      call update_nodes('zcent',1,0,1)
!      call update_nodes('volume',1,0,1)
  call get_cnx_ifield('node_id1',conn_dn,t_ok)
!  conn_dn => t_ivec%d_data
  call get_cnx_ifield('node_id2',conn_up,t_ok)
!  conn_up => t_ivec%d_data
  call get_cnx_dfield('area',areac,t_ok)
!  areac => t_rvec%d_data
  call get_cnx_dfield('distance',distc,t_ok)
!  distc => t_rvec%d_data
  call get_cnx_dfield('distance_up',dist_up,t_ok)
!  dist_up => t_rvec%d_data
  call get_cnx_dfield('distance_dn',dist_dn,t_ok)
!  dist_dn => t_rvec%d_data
!  call get_cnx_dfield('fractional_distance',t_rvec,t_ok)
!  fp => t_rvec%d_data
  call get_cnx_dfield('x_separation',unvxc,t_ok)
!  unvxc => t_rvec%d_data
  call get_cnx_dfield('y_separation',unvyc,t_ok)
!  unvyc => t_rvec%d_data
  call get_cnx_dfield('z_separation',unvzc,t_ok)
!  unvzc => t_rvec%d_data
!Gravity -BH
   call get_node_dfield('grvpx',grvpx,t_ok)
   call get_node_dfield('grvpy',grvpy,t_ok)
   call get_node_dfield('grvpz',grvpz,t_ok)
   if(ics/=8) then
     grvpx = 0.d0
     grvpy = 0.d0
     grvpz = 9.81d0
   endif
!
!---  [ BCV, BCVP, BCVT, BCVG, BCVN, BCVI, BCVS, BCVA ]
!     Boundary condition variables  ---
!
!      CALL IN_BOUN
!
!---  [ CONST ] Constants  ---
!
      SMALL = 1.D-20
      BIG = 1.D+20
      ZERO = 0.D+0
      ONE = 1.D+0
      EPSL = 1.D-14
      GRAV = 9.81D+0
      TABS = 273.15D+0
      TMX = 374.14D+0
      TMN = 0.01D+0
      PATM = 101325.D+0
      PMX = 2.212D+9
      PMN = 6.1125D+2
      RCU = 8.31434D+3
      RCW = 461.52D+0
      RCA = 287.0D+0
      RHORL = 998.32142721500441D+0
      RHORG = 1.199D+0
      VISRL = 1.0176489259594200D-3
      VISRG = 1.82D-5
      TSPRF = 293.15D+0
      WTMW = 18.015D+0
      WTMA = 28.97D+0
      WTMS = 58.4428D+0
      TCRW = 647.27D+0
      PCRW = 22105771.28D+0
      THKRW = 0.6068
      THKRA = 26.1D-3
      ZCRW = 0.235D+0
      VCRW = 57.1075D+0
      VCRA = 86.2269D+0
      PAFW = 0.344D+0
      DPMW = 1.8D+0
      TBW = 373.2D+0
      TBA = 83.35D+0
      DFGWC = 0.D+0
      DFGOC = 0.D+0
      DFGAC = 0.D+0
      DFLAC = 0.D+0
      DFLOC = 0.D+0
      DFLSC = 0.D+0
!      allocate(ptps(5))
!      PTPS(1) = 0.0765D+0
!      PTPS(2) = 0.2664D+0
!      PTPS(3) = 0.D+0
!      PTPS(4) = 0.00127D+0
!      PTPS(5) = 0.10898D+0
      HCAW = 6.7419D+9
      SUFW = 72.8D-3
      HDOD = 1.D+9/RHORL/GRAV
      GPI = 3.1415926536D+0
      TENTH = 1.D-1
      TOLN = LOG(1.D+1)
      ISMALL = -32000
      IBIG = 32000
!      allocate(iptps(4)
!      IPTPS(1) = 1
!      IPTPS(2) = 1
!      IPTPS(3) = 1
!      IPTPS(4) = 1
!
!---  [ FILES ] External file names and unit numbers ---
!
      IRD = 21
      IWR = 22
      IPL = 23
      IRS = 24
      allocate(isf(lsf))
      allocate(fnsf(lsf))
      ISF(1) = 25
      M = 2
      N = LSF
      IF( LSF.GE.2 ) THEN
        DO 410 L = M,N
          ISF(L) = 49 + L
  410   CONTINUE
      ENDIF
      ISC = 6
      FNRD = 'input'
      FNWR = 'output'
      FNPL = 'plot'
      FNRS = 'restart'
      DO 500 L = 1,LSF
        FNSF(L) = 'surface'
  500 CONTINUE
      FNSR = 'screen'
!      OPEN(UNIT=IWR, FILE=FNWR, STATUS='UNKNOWN', FORM='FORMATTED')
!      CLOSE(UNIT=IWR,STATUS='DELETE')
!      OPEN(UNIT=IWR, FILE=FNWR, STATUS='NEW', FORM='FORMATTED')
!      INQUIRE( FILE=FNRD, EXIST=FLG_EX )
!      IF( .NOT.FLG_EX ) THEN
!        INDX = 4
!        NCH = INDEX( FNRD(1:),'  ' )-1
!        CHMSG = 'Nonexistent Input File: ' // FNRD(1:NCH)
!        CARD = 'Simulation Title Card'
!        CALL WRMSGS( INDX )
!      ELSE
!        OPEN(UNIT=IRD, FILE=FNRD, STATUS='OLD', FORM='FORMATTED')
!      ENDIF
!
!---  [ FLUXP ] Primary flux variables  ---
!
      dim1 = lsfv
      call add_cnx_d2field('qflux',dim1,idx)
      q_flux => d_cnx_2fld(idx)%p
      q_flux = 0.d0

      dim1 = lsolu + lspt
      call add_cnx_d2field('cflux',dim1,idx)
      c_flux => d_cnx_2fld(idx)%p
      c_flux = 0.d0

      GRAVX = 0.D+0
      GRAVY = 0.D+0
      GRAVZ = 9.81D+0

!
!---  [ OUTPU ] Output control variables  ---
!
      CALL IN_OUTP
!
!---  [ SOLTN ] Solution control variables  ---
!
      TM = 1.D+20
      TMMX = -1.D+20
      TMPR = 1.D+20
      DT = 0.D+0
      DTI = 1.D+0
      DTMX = 0.D+0
      DTAF = 0.D+0
!      DTCF = 2.D-1
      DTCF = 5.D-1
      DTO = 0.D+0
      DTSO = 0.D+0
      RSDMX = 0.D+0
      RLXF = 1.D+0
      RLMSG = 0.D+0
      CPUMX = 0.D+0
      CLKMX = 0.D+0
      CPUSEC = 0.D+0
      CLKSEC = 0.D+0
      USER = 'null'
      CMPNY = 'null'
      TITLE = 'null'
      INPDAT = 'null'
      INPTIM = 'null'
      CARD = 'null'
      CHMSG = 'null'
      CH_VRSN = '3.2'
!vlf
!      allocate(notes(lnotes))
!      DO 1600 L = 1,LNOTES
!       NOTES(L) = ' '
! 1600 CONTINUE
!vlf
      ICD = 1
      IVR = 1
      IVRSN = 1
      ISIC = 0
      ICNV = 3
      IEO = 0
      ILES = 1
      IOM = 0
      IUNM = 0
      IUNKG = 0
      IUNS = 0
      IUNK = 0
      IUNMOL = 0
      IMSG = 0
      IEQT = 0
      IEQW = 0
      IEQA = 0
      IEQDA = 0
      IEQO = 0
      IEQC = 0
      IEQS = 0
      IEQD = 0
      IEQALC = 0
      DO 1604 L = 1,100
        ISLC(L) = 0
 1604 CONTINUE
      CRNTMXC = 0.D+0
      allocate(wfmn(20))
      DO 1620 L = 1,20
        IDMN(L) = 1
        WFMN(L) = 0.D+0
 1620 CONTINUE
      IDMN(2) = 4
      IDMN(3) = 4
      IDMN(4) = 4
      IDMN(8) = 4
      IDMN(9) = 4
      IDMN(10) = 4
      IDMN(19) = 8
      IDFLT = 0
      IDFLTD = 0
      MXSTEP = 0
      NRSD = 0
      NSTEP = 0
      NRST = 0
      NITER = 0
      NTSR = 0
      NGC = 0
      NEPD = 0
      MEPD = 0
      IEPD = 0
      DO 1650 L = 1,LFILES
        CVS_ID(L) = 'null'
 1650 CONTINUE
!
!---  [ SOURC ] Source variables  ---
!
!      DO 1710 J = 1,8+LSOLU
!        DO 1705 K = 1,LSTM
!          DO 1700 L = 1,LSR
!            SRC(J,K,L) = 0.D+0
! 1700     CONTINUE
! 1705   CONTINUE
! 1710 CONTINUE
!        ISRT(L) = 0
!        ISRM(L) = 0
!        NSOLSR(L) = 0
!        DO 1762 K = 1,LSOLSR
!          ISOLSR(K,L) = 0
! 1762   CONTINUE
! 1765 CONTINUE
      NSR = 0
!
!---  [ TABL ] Tabular data variables  ---
!
     allocate(tblddx(ltbl))
     allocate(tblddy(ltbl))
      DO 1800 L = 1,LTBL
!        TBLX(L) = 0.D+0
!        TBLY(L) = 0.D+0
        TBLDDX(L) = 0.D+0
        TBLDDY(L) = 0.D+0
 1800 CONTINUE
      NTBL = 0
!
!---  [ TRNSPT ] Solute transport variables  ---
!
    if(lr>0 .or. lc>0) then
!      dim1 = max(num_nodes**(lc),num_nodes**(lr))
!      num_nodes_w = num_nodes
!      num_nodes = lsolu
      dim1 = lsolu
      call add_node_d2field('cnl', dim1, idx)
      cnl => d_nd_2fld(idx)%p
      cnl = 0.d0
      dim1 = lsolu + lspt
      call add_node_i2field('ict', dim1, idx)
      ict => i_nd_2fld(idx)%p
      ict = 0
!
      dim1 = lsolu + lspt
      call add_node_d2field('srcic', dim1, idx)
      srcic => d_nd_2fld(idx)%p
      srcic = 0.d0
      call add_node_d2field('yl', dim1, idx)
      yl => d_nd_2fld(idx)%p
      yl = 0.d0
      call add_node_d2field('yn', dim1, idx)
      yn => d_nd_2fld(idx)%p
      yn = 0.d0
      call add_node_d2field('yg', dim1, idx)
      yg => d_nd_2fld(idx)%p
      yg = 0.d0
      call add_node_d2field('solute_conc', dim1, idx)
      c => d_nd_2fld(idx)%p
      c = 0.d0
      call add_node_d2field('solute_conc_old', dim1, idx)
      co => d_nd_2fld(idx)%p
      co = 0.d0
!--- To print out surface solute flux
      dim1 = 3
      dim2 = lsolu + lspt
      call add_node_d3field('cflux_nd',dim1,dim2,idx)
      c_flux_nd => d_nd_3fld(idx)%p
      c_flux_nd = 0.d0
!      num_nodes = num_nodes_w
     endif
      call add_node_dfield('crntl', idx)
      crntl => d_nd_fld(idx)%p
      crntl = 0.d0
      dim1 = 3
      call add_node_dfield('vfrac_inert', idx)
      vfrac_i => d_nd_fld(idx)%p
      vfrac_i = 0.d0
      call add_node_dfield('ph', idx)
      c_ph => d_nd_fld(idx)%p
      c_ph = 0.d0
      call add_node_d2field('velocity_c', dim1, idx)
      vnc => d_nd_2fld(idx)%p
      vnc = 0.d0
      call add_node_d2field('surface_flx', dim1, idx)
      s_fx => d_nd_2fld(idx)%p
      s_fx = 0.d0
!--- To print out surface flux
      call add_node_d2field('qflux_nd',dim1,idx)
      q_flux_nd => d_nd_2fld(idx)%p
      q_flux_nd = 0.d0
!
      call add_node_d2field('surface_area', dim1, idx)
      s_area => d_nd_2fld(idx)%p
      s_area = 0.d0
!
!---  [ TRNSPT ] Species transport variables  ---
!
      ELC_SOL = 'null'
      ELC_DUN = 0.D+0
      ELC_VUN = 0.D+0
      DT_CRN = 0.D+0
      DTI_CRN = 1.D+0
      TM_CRN = 0.D+0
      IDF_ELC = 0
      IVF_ELC = 0
      NSL_ELC = 0
      SMDLS = 0.D+0

      CRNTMXT = 1.D+0
      NSOLU = 0
      IDISP = 0
      IEDLS = 0
      IDSPS = 0
      IDSPD = 0
      ICRNT = 0
!
!---  [ FDVP ] Primary field variables  ---
!
      dim1 = lsv
      call add_node_d2field('pressure_w', dim1, idx)
      pl => d_nd_2fld(idx)%p
      pl = 0.d0
      call add_node_d2field('temperature', dim1, idx)
      t => d_nd_2fld(idx)%p
      t = 20.d0
      call add_node_d2field('pressure_g', dim1, idx)
      pg => d_nd_2fld(idx)%p
      pg = 0.d0
      call add_node_d2field('saturation_w', dim1, idx)
      sl => d_nd_2fld(idx)%p
      sl = 0.d0
      call add_node_d2field('saturation_g', dim1, idx)
      sg => d_nd_2fld(idx)%p
      sg = 0.d0
      call add_node_d2field('pord', dim1, idx)
      pord => d_nd_2fld(idx)%p
      pord = 0.d0
      call add_node_d2field('port', dim1, idx)
      port => d_nd_2fld(idx)%p
      port = 0.d0
      call add_node_d2field('rhol', dim1, idx)
      rhol => d_nd_2fld(idx)%p
      rhol = 0.d0
      call add_node_d2field('rhog', dim1, idx)
      rhog => d_nd_2fld(idx)%p
      rhog = 0.d0
      call add_node_d2field('visl', dim1, idx)
      visl => d_nd_2fld(idx)%p
      visl = 0.d0
      call add_node_d2field('permrf', dim1, idx)
      permrf => d_nd_2fld(idx)%p
      permrf = 1.d0
      call add_node_d2field('torl', dim1, idx)
      torl => d_nd_2fld(idx)%p
      torl = 0.d0
      call add_node_d2field('xlw', dim1, idx)
      xlw => d_nd_2fld(idx)%p
      xlw = 0.d0
      call add_node_d2field('psw', dim1, idx)
      psw => d_nd_2fld(idx)%p
      psw = 0.d0
      call add_node_d2field('pvw', dim1, idx)
      pvw => d_nd_2fld(idx)%p
      pvw = 0.d0
      call add_node_d2field('trpgl', lsu, idx)
      trpgl => d_nd_2fld(idx)%p
      trpgl = 0.d0
      call add_node_d2field('btgl', lsu, idx)
      btgl => d_nd_2fld(idx)%p
      btgl = 1.d0
!
      dim1 = 3
      dim2 = lsv
      call add_node_d3field('rkl', dim1, dim2, idx)
      rkl => d_nd_3fld(idx)%p
      rkl = 0.d0
      call add_node_dfield('pcmp', idx)
      pcmp => d_nd_fld(idx)%p
      pcmp = 0.d0
      dim1 = luk
      call add_node_d2field('dnr', dim1, idx)
      dnr => d_nd_2fld(idx)%p
      dnr = 0.d0
      dim1 = lsv
      call add_node_d2field('srcw', dim1, idx)
      srcw => d_nd_2fld(idx)%p
      srcw = 0.d0
      isvc = luk
      dim1 = isvc+1
      call add_node_d2field('accum_res', dim1, idx)
      accum_res => d_nd_2fld(idx)%p
      call add_node_d2field('accum_deriv', dim1, idx)
      accum_deriv => d_nd_2fld(idx)%p
!
      isvf = luk*2+1
      dim1 = isvf
      call add_cnx_d2field('flux_res', dim1, idx)
      flux_res => d_cnx_2fld(idx)%p
!
      dim1 = 2
      call add_node_d2field('flux_deriv', dim1, idx)
      flux_deriv => d_nd_2fld(idx)%p
      dim1 = isvc
      call add_node_d2field('residual', dim1,idx)
      residual => d_nd_2fld(idx)%p
      residual = 0.d0
      call add_node_d2field('blu', dim1,idx)
      blu => d_nd_2fld(idx)%p
      blu = 0.d0
      allocate(rsd(isvc))
      allocate(nsd(isvc))
      CHFDR = 0.D+0
      CHMER = 0.D+0
!
!---  [ FDVSO ] Second-order time field variables  ---
!
      call add_node_dfield('pl_o', idx)
      pl_o => d_nd_fld(idx)%p
      pl_o = 0.d0
      call add_node_dfield('pord_o', idx)
      pord_o => d_nd_fld(idx)%p
      pord_o = 0.d0
      call add_node_dfield('xlw_o', idx)
      xlw_o => d_nd_fld(idx)%p
      xlw_o = 0.d0
      call add_node_dfield('sl_o', idx)
      sl_o => d_nd_fld(idx)%p
      sl_o = 0.d0
      call add_node_dfield('rhol_o', idx)
      rhol_o => d_nd_fld(idx)%p
      rhol_o = 0.d0

!
!---  [ HYST ] Hysteretic k-s-P function variables ---
!
      dim1 = lsv
      call add_node_d2field('sgt',dim1,idx)
      sgt => d_nd_2fld(idx)%p
      dim1 = lsu
      call add_node_d2field('aslmin',dim1,idx)
      aslmin => d_nd_2fld(idx)%p
      aslmin = 1.d0
      call add_node_d2field('astmin',dim1,idx)
      astmin => d_nd_2fld(idx)%p
      astmin = 1.d0
      call add_node_i2field('nphaz',lsu,idx)
      nphaz => i_nd_2fld(idx)%p
      nphaz = 2
      call add_node_i2field('iph',lsu,idx)
      iph => i_nd_2fld(idx)%p
      iph = -1
      call add_node_dfield('asgt',idx)
      asgt => d_nd_fld(idx)%p
      asgt = 0.d0
      call add_node_dfield('asl',idx)
      asl => d_nd_fld(idx)%p
      asl = 0.d0
      if(.not.allocated(spnms)) allocate(spnms(lsps))
      if(.not.allocated(spnmg)) allocate(spnmg(lspg))
      if(.not.allocated(spnml)) allocate(spnml(lspl))
      if(.not.allocated(spnme)) allocate(spnme(lspe))
      if(.not.allocated(solut)) allocate(solut(lsolu+lspt))
      if(.not.allocated(n_crn)) allocate(n_crn(lsolu+1))
      n_crn = 0 
      
      BGN = 0.D+0
      BGL = 0.D+0
      BHL = 0.D+0
      BIL = 0.D+0
      BNL = 0.D+0
      CA_GN = 1.D+0
      CA_GL = 1.D+0
      CA_NL = 1.D+0
      SIG_GN = 0.D+0
      SIG_GL = 0.D+0
      SIG_NL = 0.D+0
      SIG_HL = 0.D+0
      SIG_IL = 0.D+0
      INSR = 0
!
!---  [ UCODE ] Inverse (UCode) variables ---
!
!      DO 2304 M = 1,LOBDT
!        DO 2302 L = 1,LOBDS
!          DO 2300 K = 1,2
!            R_OBDS(K,L,M) = 0.D+0
! 2300     CONTINUE
! 2302   CONTINUE
! 2304 CONTINUE
!      DO 2340 L = 1,LOBDT
!        DO 2320 K = 1,6
!          R_OBDT(K,L) = 0
! 2320   CONTINUE
!        DO 2330 K = 1,9
!          I_OBDT(K,L) = 0
! 2330   CONTINUE
!        C_OBDT(L) = 'null'
!        NOBDS(L) = 0
! 2340 CONTINUE
      NOBDT = 0
      NOBDP = 0
      IOBDEF = 40
      IOBDSF = 41
      IOBDUF = 42
      TMOB = 1.D+20
      FLG_UNI = .FALSE.
      FLG_EXT = .FALSE.
!
!---  [ REACT ] Reaction Variables  ---
!
!
!      dim1 = num_nodes**(lr*ll)
!      num_nodes_w = num_nodes
!      num_nodes = leqc+leqk
     if( lr*ll > 0 ) then
      dim1 = leqc+leqk
      call add_node_d2field('yspl', dim1, idx)
      yspl => d_nd_2fld(idx)%p
      yspl = 0.d0
      call add_node_d2field('yspg', dim1, idx)
      yspg => d_nd_2fld(idx)%p
      yspg = 0.d0
     endif
!      num_nodes = num_nodesw

      SP_MDL = 0.D+0
     if( lr>0 ) then
      dim1 = 3
      dim2 = lsps
!      num_nodes_w = num_nodes
!      num_nodes = num_nodes**lr
      call add_node_d3field('mineral_property', dim1, dim2, idx)
      rs_s => d_nd_3fld(idx)%p
      rs_s = 0.d0
      dim1 = lsps
      call add_node_i2field('isp_ow', dim1, idx)
      isp_ow => i_nd_2fld(idx)%p
      isp_ow = 0.d0
     endif
!      num_nodes = num_nodes_w
!	        
      SP_MDG = 0.D+0
!      DO 2982 L = 1,LSPN
!        SPNMN(L) = ' '
! 2982 CONTINUE
      SP_MDN = 0.D+0
      ACTVC = 1.D+0
      IACTV = 0
      NEQC = 0
      NEQE = 0
      NEQK = 0
      NRCE = 0
      NSPE = 0
      NRCK = 0
      NSPC = 0
      NSPG = 0
      NSPK = 0
      NSPL = 0
      NSPLK = 0
      NSPN = 0
      NSPS = 0
      NRTSI = 0
      N_RST = 10
      ECKE_ER = .FALSE.
     if(lr*ll > 0) then
!      dim1 = num_nodes**(lr*ll)
!      num_nodes_w = num_nodes
!      num_nodes = lspr
      dim1 = lspr
!print *,'dim1---',dim1
      call add_node_d2field('sp_c', dim1, idx)
      sp_c => d_nd_2fld(idx)%p
      sp_c = 0.d0
      call add_node_d2field('sp_ci', dim1, idx)
      sp_ci => d_nd_2fld(idx)%p
      sp_ci = 0.d0
      call add_node_d2field('sp_co', dim1, idx)
      sp_co => d_nd_2fld(idx)%p
      sp_co = 0.d0

      call add_node_i2field('ic_sp', dim1, idx)
      ic_sp => i_nd_2fld(idx)%p
      ic_sp = 0

      call add_node_d2field('sp_area', dim1, idx)
      sp_area => d_nd_2fld(idx)%p
      sp_area = 0.d0

      call add_node_d2field('sp_rate', dim1, idx)
      sp_rate => d_nd_2fld(idx)%p
      sp_rate = 0.d0

!      num_nodes = num_nodes_w
!      num_nodes_w = num_nodes
!      num_nodes = lsps
      dim1 = lsps
      call add_node_d2field('sp_cmn', dim1, idx)
      sp_cmn => d_nd_2fld(idx)%p
      sp_cmn = 0.d0
!      num_nodes = num_nodes_w

      dim1 = 2
      call add_node_d2field('por_m', dim1, idx)
      por_m => d_nd_2fld(idx)%p
      por_m = 0.d0
    endif
!

      DT_RST = 0.D+0
      DTI_RST = 0.D+0
      TM_RST = 0.D+0
!
!---  [ POINTE ] Numerical scheme pointer arrays  ---
!
!      MNOD /2,3,2,4,2,5,2,6,2,7,2,8,2,9,2/
!      MADJ /2,2,3,2,4,2,5,2,6,2,7,2,8,2,9/
!      MPOS /2,3,2,4,2,5,2,6,2,7,2,8,2,9,2/
!      MNEG /2,2,3,2,4,2,5,2,6,2,7,2,8,2,9/
!      MFLX /1,3,2,5,4,7,6,9,8,11,10,13,12,15,14/
!      MPOSB /1,2,4,6,8,10,12,14/
!      MNEGB /1,3,5,7,9,11,13,15/
!
      allocate(mnod(15))
      allocate(madj(15))
      allocate(mflx(15))
      allocate(mpos(15))
      allocate(mneg(15))
      allocate(mposb(8))
      allocate(mnegb(8))
      MNOD(1) = 2
      MNOD(2) = 3
      MNOD(3) = 2
      MNOD(4) = 4
      MNOD(5) = 2
      MNOD(6) = 5
      MNOD(7) = 2
      MNOD(8) = 6
      MNOD(9) = 2
      MNOD(10) = 7
      MNOD(11) = 2
      MNOD(12) = 8
      MNOD(13) = 2
      MNOD(14) = 9
      MNOD(15) = 2
      MADJ(1) = 2
      MADJ(2) = 2
      MADJ(3) = 3
      MADJ(4) = 2
      MADJ(5) = 4
      MADJ(6) = 2
      MADJ(7) = 5
      MADJ(8) = 2
      MADJ(9) = 6
      MADJ(10) = 2
      MADJ(11) = 7
      MADJ(12) = 2
      MADJ(13) = 8
      MADJ(14) = 2
      MADJ(15) = 9
      MPOS(1) = 2
      MPOS(2) = 3
      MPOS(3) = 2
      MPOS(4) = 4
      MPOS(5) = 2
      MPOS(6) = 5
      MPOS(7) = 2
      MPOS(8) = 6
      MPOS(9) = 2
      MPOS(10) = 7
      MPOS(11) = 2
      MPOS(12) = 8
      MPOS(13) = 2
      MPOS(14) = 9
      MPOS(15) = 2
      MNEG(1) = 2
      MNEG(2) = 2
      MNEG(3) = 3
      MNEG(4) = 2
      MNEG(5) = 4
      MNEG(6) = 2
      MNEG(7) = 5
      MNEG(8) = 2
      MNEG(9) = 6
      MNEG(10) = 2
      MNEG(11) = 7
      MNEG(12) = 2
      MNEG(13) = 8
      MNEG(14) = 2
      MNEG(15) = 9
      MFLX(1) = 1
      MFLX(2) = 3
      MFLX(3) = 2
      MFLX(4) = 5
      MFLX(5) = 4
      MFLX(6) = 7
      MFLX(7) = 6
      MFLX(8) = 9
      MFLX(9) = 8
      MFLX(10) = 11
      MFLX(11) = 10
      MFLX(12) = 13
      MFLX(13) = 12
      MFLX(14) = 15
      MFLX(15) = 14
      MPOSB(1) = 1
      MPOSB(2) = 2
      MPOSB(3) = 4
      MPOSB(4) = 6
      MPOSB(5) = 8
      MPOSB(6) = 10
      MPOSB(7) = 12
      MPOSB(8) = 14
      MNEGB(1) = 1
      MNEGB(2) = 3
      MNEGB(3) = 5
      MNEGB(4) = 7
      MNEGB(5) = 9
      MNEGB(6) = 11
      MNEGB(7) = 13
      MNEGB(8) = 15
!
!--- mapping to global id
!
      call add_node_ifield('imxp', idx)
      imxp => i_nd_fld(idx)%p
      imxp = 0
!      allocate(nnz_d(num_nodes))
!      allocate(nnz_o(num_nodes))
      allocate(nnz_d(num_loc_nodes))
      allocate(nnz_o(num_loc_nodes))
!-- temporary array for output 
      call add_node_dfield('varp_tmp', idx)
      varp_tmp => d_nd_fld(idx)%p
      varp_tmp = 0.d0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of INTLZ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE IN_BOUN
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
!     Initialize boundary condition variables.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 19 June 2001.
!     Last Modified by MD White, PNNL, 19 June 2001.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
!      USE BCVT
!      USE BCVS
      USE BCVP
!      USE BCVN
!      USE BCVI
!      USE BCVH
!      USE BCVGC
!      USE BCVG
!      USE BCVA
      USE BCV
      use trnspt
      use grid_mod
      use grid
      use react
!  use sidl
!  use sidl_NotImplementedException
!  use sidl_BaseInterface
!  use sidl_RuntimeException
!  use gov_cca_Port
!  use grid_GridPort
!  use stmpgrid_GAGrid
!  use stmpgrid_GAGrid_impl
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
!
!----------------------Parameter Statements----------------------------!
!
interface
  subroutine get_bcnx_dfield( t_string, t_field,  &
  t_ok)
  use grid_mod
  implicit none
  character (len=*) :: t_string ! in
  double precision, pointer :: t_field(:) ! out
  logical :: t_ok ! out
  integer i, slen, nlen, grid_clen
  end subroutine get_bcnx_dfield
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
!
!----------------------Common Blocks-----------------------------------!
!

!
!----------------------Type Declarations-------------------------------!
!
!  type (grid_GridPort_t)    :: gridPort
!  type(stmpgrid_GAGrid_t) :: gridPort ! in
!  type(sidl_BaseInterface_t) :: excpt ! out
!  type(sidl_double_1d) :: t_rvec
!  type(sidl_int_1d) :: t_ivec
  logical :: t_ok
  integer, pointer :: i_bid(:)
  integer :: idx
  LOGICAL :: use_ga
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/IN_BOUN'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(71)(1:1),'$').EQ.0 ) CVS_ID(71) = &
      '$Id: intlz.F,v 1.53 2008/02/13 01:03:00 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      if(num_bcnx.ne.0) then
!
!---  [ BCV ] Global boundary condition variables  ---
!
!      DO 30 L = 1,LBCIN
!        NBCM(L) = 0
!        IBCLL(L) = 0
!        JBCLL(L) = 0
!        KBCLL(L) = 0
!        MBCLL(L) = 0
!        NBCLL(L) = 0
!        DO 20 K = 1,LBTM
!          BCXYZG(K,1) = 0.D+0
!          BCXYZG(K,2) = 0.D+0
!          BCXYZG(K,3) = 0.D+0
!          DO 10 J = 1,LBCV
!            BC(J,K,L) = 0.D+0
!   10     CONTINUE
!   20   CONTINUE
!   30 CONTINUE
!      DO 100 L = 1,num_bcnx
!        PHDL(1,L) = 0.D+0
!        PHDL(2,L) = 0.D+0
!        PHDN(1,L) = 0.D+0
!        PHDN(2,L) = 0.D+0
!        IBCC(L) = 0
!        IBCD(L) = 0
!        IBCM(L) = 0
!        IBCN(L) = 0
!        IBCIN(L) = 0
!        IBCSN(L) = 0
!        DO 40 K = 1,LUK+LPH*LSOLU*LC
!          IBCT(K,L) = 0
!   40   CONTINUE
!  100 CONTINUE
      NBC = 0
!--- boundary surface area and distance to cell center
  call get_bcnx_dfield('b_area',areab,t_ok)
  call get_bcnx_dfield('b_grvx',grvxb,t_ok)
  call get_bcnx_dfield('b_tltx',tltxb,t_ok)

!  areab => t_rvec%d_data
  call get_bcnx_dfield('b_distance',distb,t_ok)
!  distb => t_rvec%d_data
  call get_bcnx_dfield('b_x_separation',uvxb,t_ok)
!  uvxb => t_rvec%d_data
  call get_bcnx_dfield('b_y_separation',uvyb,t_ok)
!  uvyb => t_rvec%d_data
  call get_bcnx_dfield('b_z_separation',uvzb,t_ok)
!  uvzb => t_rvec%d_data
  call add_bcnx_dfield('b_xcent',idx)
  xpb => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_ycent',idx)
  ypb => d_bcnx_fld(idx)%p
  call add_bcnx_dfield('b_zcent',idx)
  zpb => d_bcnx_fld(idx)%p
  xpb = 0.d0
  ypb = 0.d0
  zpb = 0.d0
! create boundary center coordinate 
  call get_bcnx_ifield('bcnx_id',i_bid,t_ok)
!  i_bid => t_ivec%d_data
  do icx = 1,num_bcnx
    idx = i_bid(icx)
    if(uvxb(icx) == -1) then
      xpb(icx) = xp(idx) - distb(icx)
      ypb(icx) = yp(idx)
      zpb(icx) = zp(idx)
    elseif(uvxb(icx) == 1) then
      xpb(icx) = xp(idx) + distb(icx)
      ypb(icx) = yp(idx)
      zpb(icx) = zp(idx)
    endif  
    if(uvyb(icx) == -1) then
      xpb(icx) = xp(idx)
      ypb(icx) = yp(idx) - distb(icx)
      zpb(icx) = zp(idx)
    elseif(uvyb(icx) == 1) then
      xpb(icx) = xp(idx)
      ypb(icx) = yp(idx) + distb(icx)
      zpb(icx) = zp(idx)
    endif
    if(uvzb(icx) == -1) then
      xpb(icx) = xp(idx)
      ypb(icx) = yp(idx)
      zpb(icx) = zp(idx) - distb(icx)
    elseif(uvzb(icx) == 1) then
      xpb(icx) = xp(idx)
      ypb(icx) = yp(idx)
      zpb(icx) = zp(idx) + distb(icx)
    endif
  enddo
!
!---  [ BCVP ] Primary boundary condition variables  ---
!
      call add_bcnx_d2field('tb',lsv,idx)
      tb => d_bcnx_2fld(idx)%p
      tb = 0.d0
      call add_bcnx_d2field('plb',lsv,idx)
      plb => d_bcnx_2fld(idx)%p
      plb = 0.d0
      call add_bcnx_d2field('pgb',lsv,idx)
      pgb => d_bcnx_2fld(idx)%p
      pgb = 0.d0
      call add_bcnx_d2field('slb',lsv,idx)
      slb => d_bcnx_2fld(idx)%p
      slb = 0.d0
      call add_bcnx_d2field('sgb',lsv,idx)
      sgb => d_bcnx_2fld(idx)%p
      sgb = 0.d0
      call add_bcnx_d2field('pordb',lsv,idx)
      pordb => d_bcnx_2fld(idx)%p
      pordb = 0.d0
      call add_bcnx_d2field('portb',lsv,idx)
      portb => d_bcnx_2fld(idx)%p
      portb = 0.d0
      call add_bcnx_d2field('rholb',lsv,idx)
      rholb => d_bcnx_2fld(idx)%p
      rholb = 0.d0
      call add_bcnx_d2field('pvwb',lsv,idx)
      pvwb => d_bcnx_2fld(idx)%p
      pvwb = 0.d0
      call add_bcnx_d2field('xlwb',lsv,idx)
      xlwb => d_bcnx_2fld(idx)%p
      xlwb = 0.d0
      call add_bcnx_d2field('torlb',lsv,idx)
      torlb => d_bcnx_2fld(idx)%p
      torlb = 0.d0
      call add_bcnx_d2field('vislb',lsv,idx)
      vislb => d_bcnx_2fld(idx)%p
      vislb = 0.d0
      idim3 = 3
      call add_bcnx_d3field('rklb',idim3,lsv,idx)
      rklb => d_bcnx_3fld(idx)%p
      rklb = 0.d0
      idim1 = lsfv 
      call add_bcnx_d2field('q_flux_b',idim1,idx)
      q_flux_b => d_bcnx_2fld(idx)%p
      q_flux_b = 0.d0
      idim1 = lsolu + lspt
      call add_bcnx_d2field('c_flux_b',idim1,idx)
      c_flux_b => d_bcnx_2fld(idx)%p
      c_flux_b = 0.d0
      if( lc.gt.0 .or. lr.gt.0 ) then
       num_bcnxw = num_bcnx
       num_bcnx = lsolu+lspt
       idim1 = num_bcnxw
       call add_bcnx_d2field('cbo',idim1,idx)
       cbo => d_bcnx_2fld(idx)%p
       cbo = 0.d0
       call add_bcnx_d2field('cb',idim1,idx)
       cb => d_bcnx_2fld(idx)%p
       cb = 0.d0
       call add_bcnx_d2field('ylb',idim1,idx)
       ylb => d_bcnx_2fld(idx)%p
       ylb = 0.d0
       num_bcnx = num_bcnxw
      endif
      if( lr.gt.0 ) then
       num_bcnxw = num_bcnx
       num_bcnx = lspr
       idim1 = num_bcnxw
       call add_bcnx_d2field('sp_cbo',idim1,idx)
       sp_cbo => d_bcnx_2fld(idx)%p
       sp_cbo = 0.d0
       num_bcnx = num_bcnxw
      endif
     endif
!
!---  Reset subroutine character string ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of IN_BOUN group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE IN_OUTP
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
!     Initialize output control variables.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 19 June 2001.
!     Last Modified by MD White, PNNL, 19 June 2001.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

!      USE WELL_FX
!      USE WELL_FD
      USE TRNSPT
!      USE SPILL
      USE SOURC
      USE SOLTN
      USE REACT
!      USE PLT_ATM
      USE OUTPU
!      USE NAPL
      USE HYST
!      USE FLUXT
!      USE FLUXS
      USE FLUXP
!      USE FLUXN
!      USE FLUXD
!      USE FDVS
      USE FDVP
!      use grid
!      use grid_mod
!      USE FDVN
!      USE FDVH
!      USE FDVA
      USE COUP_WELL

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
      EXTERNAL ICOUNT
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*4 FORM1
     LOGICAL :: use_ga
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1
      DATA FORM1 / '(I )' /
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/IN_OUTP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(71)(1:1),'$').EQ.0 ) CVS_ID(71) = &
     '$Id: intlz.F,v 1.53 2008/02/13 01:03:00 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  [ OUTPU ] Output control variables  ---
!
!      allocate(prtm(lptm))
!      DO 10 L = 1,LPTM
!        PRTM(L) = 0.D+0
!   10 CONTINUE
      allocate(sf(2,lsf))
      allocate(unsf(2,lsf))
      allocate(isft(lsf))
      allocate(isff(lsf))
      allocate(isfd(lsf))
      allocate(isfgc(lsf))
      allocate(nsfdom(lsf))
!      allocate(isfc(6,lsf))
      allocate(isfdom(4,lsfdom,lsf))
      allocate(isfgp(lsf))
      allocate(isfsn(lsf))
      DO 30 L = 1,LSF
        SF(1,L) = 0.D+0
        SF(2,L) = 0.D+0
        UNSF(1,L) = 'null'
        UNSF(2,L) = 'null'
        ISFT(L) = 0
        ISFF(L) = 0
        ISFD(L) = 0
        ISFGC(L) = 0
        NSFDOM(L) = 0
!        DO 20 K = 1,6
!          ISFC(K,L) = 0
!   20   CONTINUE
        DO 24 M = 1,LSFDOM
          DO 22 K = 1,4
            ISFDOM(K,M,L) = 0
   22     CONTINUE
   24   CONTINUE
        ISFGP(L) = 0
        ISFSN(L) = 0
   30 CONTINUE
      UNTM = 's'
      UNLN = 'm'
      UNAR = 'rad'
      allocate(unplot(loupv))
      allocate(unref(loupv))
      allocate(iplot(loupv))
      allocate(iref(loupv))
      allocate(iplotgc(loupv))
      allocate(irefgc(loupv))
      allocate(chref(loupv))
      allocate(chsf(loupv,3))
      allocate(iref_cw(loupv)) ! for coupled well - BH
        iref_cw = 0
      DO 50 L = 1,LOUPV
        UNPLOT(L) = 'null'
        UNREF(L) = 'null'
        IPLOT(L) = 0
        IREF(L) = 0
        IPLOTGC(L) = 0
        IREFGC(L) = 0
        CHREF(L) = ''
        DO 40 K = 1,3
          CHSF(L,K) = 'null'
   40   CONTINUE
   50 CONTINUE
!      allocate(ndref(lref))
!      DO 60 L = 1,LREF
!        NDREF(L) = 0
!   60 CONTINUE
      NSF = 0
      NSFGP = 1
      IHSF = 0
      NPRTM = 0
      NVPLOT = 0
      NREF = 0
      NVREF = 0
      ICNO = -1
      ICNS = -1
      IFQS = 0
      IFQO = 0
      ISGNS = 4
      ISGNO = 4
      ISGNP = 5
      CHREF(1) = ' PL '
      UNPLOT(1) = 'pa'
      UNREF(1) = 'pa'
      CHREF(2) = ' PG '
      UNPLOT(2) = 'pa'
      UNREF(2) = 'pa'
      CHREF(3) = ' PN '
      UNPLOT(3) = 'pa'
      UNREF(3) = 'pa'
      CHREF(4) = ' T  '
      UNPLOT(4) = 'c'
      UNREF(4) = 'c'
      CHREF(5) = 'PHCN'
      CHREF(6) = 'GPL '
      UNPLOT(6) = 'pa'
      UNREF(6) = 'pa'
      CHREF(7) = 'GPG '
      UNPLOT(7) = 'pa'
      UNREF(7) = 'pa'
      CHREF(8) = 'GPN '
      UNPLOT(8) = 'pa'
      UNREF(8) = 'pa'
      CHREF(9) = 'ASL '
      CHREF(10) = 'AST '
      CHREF(11) = ' SL '
      CHREF(12) = ' SG '
      CHREF(13) = ' SN '
      CHREF(14) = ' ST '
      CHREF(15) = 'MCL '
      CHREF(16) = 'MCN '
      CHREF(17) = 'MCT '
      CHREF(18) = 'ESNT'
      CHREF(19) = 'ESGT'
      CHREF(20) = 'PORD'
      CHREF(21) = 'XGW '
      CHREF(22) = 'XGA '
      CHREF(23) = 'XGO '
      CHREF(24) = 'XLW '
      CHREF(25) = 'XLA '
      CHREF(26) = 'XLO '
      CHREF(27) = 'HHL '
      UNPLOT(27) = 'm'
      UNREF(27) = 'm'
      CHREF(28) = 'HHG '
      UNPLOT(28) = 'm'
      UNREF(28) = 'm'
      CHREF(29) = 'HHN '
      UNPLOT(29) = 'm'
      UNREF(29) = 'm'
      CHREF(30) = 'RSZN'
      CHREF(31) = 'RPL '
      CHREF(32) = 'RPG '
      CHREF(33) = 'RPN '
      CHREF(34) = 'RHOL'
      UNPLOT(34) = 'kg/m^3'
      UNREF(34) = 'kg/m^3'
      CHREF(35) = 'RHOG'
      UNPLOT(35) = 'kg/m^3'
      UNREF(35) = 'kg/m^3'
      CHREF(36) = 'RHON'
      UNPLOT(36) = 'kg/m^3'
      UNREF(36) = 'kg/m^3'
      CHREF(37) = 'TMW '
      UNPLOT(37) = 'kg'
      UNREF(37) = 'kg'
      CHREF(38) = 'TMA '
      UNPLOT(38) = 'kg'
      UNREF(38) = 'kg'
      CHREF(39) = 'TMO '
      UNPLOT(39) = 'kg'
      UNREF(39) = 'kg'
      CHREF(40) = 'SRIW'
      UNPLOT(40) = 'kg'
      UNREF(40) = 'kg'
      CHREF(41) = 'SRIA'
      UNPLOT(41) = 'kg'
      UNREF(41) = 'kg'
      CHREF(42) = 'SRIO'
      UNPLOT(42) = 'kg'
      UNREF(42) = 'kg'
      CHREF(43) = 'SRIT'
      UNPLOT(43) = 'j'
      UNREF(43) = 'j'
      CHREF(44) = 'THKX'
      UNPLOT(44) = 'w/m k'
      UNREF(44) = 'w/m k'
      CHREF(45) = 'THKY'
      UNPLOT(45) = 'w/m k'
      UNREF(45) = 'w/m k'
      CHREF(46) = 'THKZ'
      UNPLOT(46) = 'w/m k'
      UNREF(46) = 'w/m k'
      CHREF(47) = ' CS '
      UNPLOT(47) = 'kg/m^3'
      UNREF(47) = 'kg/m^3'
      CHREF(48) = 'CSL '
      UNPLOT(48) = 'kg/m^3'
      UNREF(48) = 'kg/m^3'
      CHREF(49) = 'CRNL'
      CHREF(50) = 'TMS '
      UNPLOT(50) = 'kg'
      UNREF(50) = 'kg'
      CHREF(51) = ' UL '
      UNPLOT(51) = 'm/s'
      UNREF(51) = 'm/s'
      CHREF(52) = ' VL '
      UNPLOT(52) = 'm/s'
      UNREF(52) = 'm/s'
      CHREF(53) = ' WL '
      UNPLOT(53) = 'm/s'
      UNREF(53) = 'm/s'
      CHREF(54) = ' UG '
      UNPLOT(54) = 'm/s'
      UNREF(54) = 'm/s'
      CHREF(55) = ' VG '
      UNPLOT(55) = 'm/s'
      UNREF(55) = 'm/s'
      CHREF(56) = ' WG '
      UNPLOT(56) = 'm/s'
      UNREF(56) = 'm/s'
      CHREF(57) = ' UN '
      UNPLOT(57) = 'm/s'
      UNREF(57) = 'm/s'
      CHREF(58) = ' VN '
      UNPLOT(58) = 'm/s'
      UNREF(58) = 'm/s'
      CHREF(59) = ' WN '
      UNPLOT(59) = 'm/s'
      UNREF(59) = 'm/s'
      CHREF(60) = ' UQ '
      UNPLOT(60) = 'w/m^2 s'
      UNREF(60) = 'w/m^2 s'
      CHREF(61) = ' VQ '
      UNPLOT(61) = 'w/m^2 s'
      UNREF(61) = 'w/m^2 s'
      CHREF(62) = ' WQ '
      UNPLOT(62) = 'w/m^2 s'
      UNREF(62) = 'w/m^2 s'
      CHREF(63) = 'MPH '
      UNPLOT(63) = 'm'
      UNREF(63) = 'm'
      CHREF(64) = ' US '
      UNPLOT(64) = 'kg/m^2 s'
      UNREF(64) = 'kg/m^2 s'
      CHREF(65) = ' VS '
      UNPLOT(65) = 'kg/m^2 s'
      UNREF(65) = 'kg/m^2 s'
      CHREF(66) = ' WS '
      UNPLOT(66) = 'kg/m^2 s'
      UNREF(66) = 'kg/m^2 s'
      CHREF(67) = 'USNC'
      UNPLOT(67) = 'kg/m^2 s'
      UNREF(67) = 'kg/m^2 s'
      CHREF(68) = 'VSNC'
      UNPLOT(68) = 'kg/m^2 s'
      UNREF(68) = 'kg/m^2 s'
      CHREF(69) = 'WSNC'
      UNPLOT(69) = 'kg/m^2 s'
      UNREF(69) = 'kg/m^2 s'
      CHREF(70) = 'XMGW'
      CHREF(71) = 'XMGA'
      CHREF(72) = 'XMGO'
      CHREF(73) = 'CGW '
      UNPLOT(73) = 'kg/m^3'
      UNREF(73) = 'kg/m^3'
      CHREF(74) = 'CGA '
      UNPLOT(74) = 'kg/m^3'
      UNREF(74) = 'kg/m^3'
      CHREF(75) = 'CGO '
      UNPLOT(75) = 'kg/m^3'
      UNREF(75) = 'kg/m^3'
      CHREF(76) = 'CLW '
      UNPLOT(76) = 'kg/m^3'
      UNREF(76) = 'kg/m^3'
      CHREF(77) = 'CLA '
      UNPLOT(77) = 'kg/m^3'
      UNREF(77) = 'kg/m^3'
      CHREF(78) = 'CLO '
      UNPLOT(78) = 'kg/m^3'
      UNREF(78) = 'kg/m^3'
      CHREF(79) = 'CRNG'
      CHREF(80) = 'PI '
      UNPLOT(80) = 'pa'
      UNREF(80) = 'pa'
      CHREF(81) = 'SI '
      CHREF(82) = 'RHOF'
      UNPLOT(82) = 'kg/m^3'
      UNREF(82) = 'kg/m^3'
      CHREF(83) = 'DSLF'
      CHREF(84) = 'DSLM'
      CHREF(85) = 'DSGF'
      CHREF(86) = 'DSGM'
      CHREF(87) = 'ULNC'
      UNPLOT(87) = 'm/s'
      UNREF(87) = 'm/s'
      CHREF(88) = 'VLNC'
      UNPLOT(88) = 'm/s'
      UNREF(88) = 'm/s'
      CHREF(89) = 'WLNC'
      UNPLOT(89) = 'm/s'
      UNREF(89) = 'm/s'
      CHREF(90) = 'UGNC'
      UNPLOT(90) = 'm/s'
      UNREF(90) = 'm/s'
      CHREF(91) = 'VGNC'
      UNPLOT(91) = 'm/s'
      UNREF(91) = 'm/s'
      CHREF(92) = 'WGNC'
      UNPLOT(92) = 'm/s'
      UNREF(92) = 'm/s'
      CHREF(93) = 'UNNC'
      UNPLOT(93) = 'm/s'
      UNREF(93) = 'm/s'
      CHREF(94) = 'VNNC'
      UNPLOT(94) = 'm/s'
      UNREF(94) = 'm/s'
      CHREF(95) = 'WNNC'
      UNPLOT(95) = 'm/s'
      UNREF(95) = 'm/s'
      CHREF(96) = 'UQNC'
      UNPLOT(96) = 'w/m^2 s'
      UNREF(96) = 'w/m^2 s'
      CHREF(97) = 'VQNC'
      UNPLOT(97) = 'w/m^2 s'
      UNREF(97) = 'w/m^2 s'
      CHREF(98) = 'WQNC'
      UNPLOT(98) = 'w/m^2 s'
      UNREF(98) = 'w/m^2 s'
      CHREF(99) = 'CRNN'
      CHREF(100) = 'SPCM'
      CHREF(101) = 'POSM'
      UNPLOT(101) = 'pa'
      UNREF(101) = 'pa'
      CHREF(102) = 'OEC '
      CHREF(103) = 'CLA '
      UNPLOT(103) = 'kg/m^3'
      UNREF(103) = 'kg/m^3'
      CHREF(104) = 'CNA '
      UNPLOT(104) = 'kg/m^3'
      UNREF(104) = 'kg/m^3'
      CHREF(105) = 'SGT '
      CHREF(106) = 'SNT '
      CHREF(107) = 'SGTL'
      CHREF(108) = 'SGTN'
      CHREF(109) = 'CLO '
      UNREF(109) = 'kg/m^3'
      UNPLOT(109) = 'kg/m^3'
      CHREF(110) = 'XLS '
      CHREF(111) = ' CS '
      UNREF(111) = 'kg/m^3'
      UNPLOT(111) = 'kg/m^3'
      CHREF(112) = 'CLS '
      UNREF(112) = 'kg/m^3'
      UNPLOT(112) = 'kg/m^3'
      CHREF(113) = 'XLS '
      CHREF(114) = ' US '
      UNREF(114) = 'kg/m^2 s'
      UNPLOT(114) = 'kg/m^2 s'
      CHREF(115) = ' VS '
      UNREF(115) = 'kg/m^2 s'
      UNPLOT(115) = 'kg/m^2 s'
      CHREF(116) = ' WS '
      UNREF(116) = 'kg/m^2 s'
      UNPLOT(116) = 'kg/m^2 s'
      CHREF(117) = 'USNC'
      UNREF(117) = 'kg/m^2 s'
      UNPLOT(117) = 'kg/m^2 s'
      CHREF(118) = 'VSNC'
      UNREF(118) = 'kg/m^2 s'
      UNPLOT(118) = 'kg/m^2 s'
      CHREF(119) = 'WSNC'
      UNREF(119) = 'kg/m^2 s'
      UNPLOT(119) = 'kg/m^2 s'
      CHREF(120) = 'ULO '
      UNREF(120) = 'kg/m^2 s'
      UNPLOT(120) = 'kg/m^2 s'
      CHREF(121) = 'VLO '
      UNREF(121) = 'kg/m^2 s'
      UNPLOT(121) = 'kg/m^2 s'
      CHREF(122) = 'WLO '
      UNREF(122) = 'kg/m^2 s'
      UNPLOT(122) = 'kg/m^2 s'
      CHREF(123) = 'ULOC'
      UNREF(123) = 'kg/m^2 s'
      UNPLOT(123) = 'kg/m^2 s'
      CHREF(124) = 'VLOC'
      UNREF(124) = 'kg/m^2 s'
      UNPLOT(124) = 'kg/m^2 s'
      CHREF(125) = 'WLOC'
      UNREF(125) = 'kg/m^2 s'
      UNPLOT(125) = 'kg/m^2 s'
      CHREF(126) = 'TPNL'
      CHREF(127) = 'ESLM'
      CHREF(128) = 'PVW '
      UNPLOT(128) = 'pa'
      UNREF(128) = 'pa'
      CHREF(129) = 'PVA '
      UNPLOT(129) = 'pa'
      UNREF(129) = 'pa'
      CHREF(130) = 'XMLA'
      CHREF(131) = 'BGL '
      CHREF(132) = 'ANFL'
      UNPLOT(132) = 'm^2'
      UNREF(132) = 'm^2'
      CHREF(132) = 'ANTL'
      UNPLOT(132) = 'm^2'
      UNREF(132) = 'm^2'
      CHREF(134) = 'HKL '
      UNPLOT(134) = '1/s'
      UNREF(134) = '1/s'
      CHREF(135) = 'HKNF'
      UNPLOT(135) = '1/s'
      UNREF(135) = '1/s'
      CHREF(136) = 'HKNT'
      UNPLOT(136) = '1/s'
      UNREF(136) = '1/s'
      CHREF(140) = 'SRCW'
      UNPLOT(140) = 'kg/s'
      UNREF(140) = 'kg/s'
      CHREF(141) = 'SRCA'
      UNPLOT(141) = 'kg/s'
      UNREF(141) = 'kg/s'
      CHREF(142) = 'SRCO'
      UNPLOT(142) = 'kg/s'
      UNREF(142) = 'kg/s'
      CHREF(143) = 'SRCQ'
      UNPLOT(143) = 'w'
      UNREF(143) = 'w'
      CHREF(144) = 'PLWB'
      UNREF(144) = 'm'
      CHREF(145) = 'QLW '
      UNREF(145) = 'm^3'
      CHREF(146) = 'QLWI'
      UNREF(146) = 'm^3/s'
      CHREF(147) = 'SRCS'
      UNPLOT(147) = 'kg/s'
      UNREF(147) = 'kg/s'
      CHREF(148) = 'SRIS'
      UNPLOT(148) = 'kg'
      UNREF(148) = 'kg'
      CHREF(149) = 'PATH'
      CHREF(150) = 'DAPS'
      CHREF(151) = 'BVF '
      CHREF(152) = 'XBA '
      CHREF(153) = 'MCO2'
      UNPLOT(153) = 'kg/m^3'
      UNREF(153) = 'kg/m^3'
      CHREF(154) = 'QNW '
      UNREF(154) = 'm^3'
      CHREF(155) = 'QNWI'
      UNREF(155) = 'm^3/s'
      CHREF(156) = 'QTW '
      UNREF(156) = 'm^3'
      CHREF(157) = 'QTWI'
      UNREF(157) = 'm^3/s'
      CHREF(161) = 'CNW '
      UNPLOT(161) = 'kg/m^3'
      UNREF(161) = 'kg/m^3'
      CHREF(162) = 'XMNW'
      CHREF(163) = 'XNW '
      CHREF(164) = 'CNO '
      UNPLOT(164) = 'kg/m^3'
      UNREF(164) = 'kg/m^3'
      CHREF(165) = 'XMNO'
      CHREF(166) = 'XNO '
      CHREF(167) = 'TMA '
      UNPLOT(167) = 'kg/m^3'
      UNREF(167) = 'kg/m^3'
      CHREF(168) = 'XLW '
      CHREF(169) = 'SRIA'
      UNPLOT(169) = 'kg'
      UNREF(169) = 'kg'
      CHREF(170) = 'SRCA'
      UNPLOT(170) = 'kg/s'
      UNREF(170) = 'kg/s'
      CHREF(171) = 'IMA '
      UNREF(171) = 'kg'
      CHREF(172) = 'IMLA'
      UNREF(172) = 'kg'
      CHREF(173) = 'IMNW'
      UNREF(173) = 'kg'
      CHREF(174) = 'IMNO'
      UNREF(174) = 'kg'
      CHREF(175) = 'IMNA'
      UNREF(175) = 'kg'
      CHREF(176) = 'VISL'
      UNPLOT(176) = 'pa s'
      UNREF(176) = 'pa s'
      CHREF(177) = 'WDT '
      UNPLOT(177) = 'm'
      UNREF(177) = 'm'
      CHREF(178) = 'WDL '
      UNPLOT(178) = 'm'
      UNREF(178) = 'm'
      CHREF(179) = 'WDN '
      UNPLOT(179) = 'm'
      UNREF(179) = 'm'
      CHREF(180) = ' PW '
      UNPLOT(180) = 'pa'
      UNREF(180) = 'pa'
      CHREF(181) = 'SLW '
      CHREF(182) = 'XGWW'
      CHREF(183) = 'XLAW'
      CHREF(184) = 'UL_W'
      UNPLOT(184) = 'm/s'
      UNREF(184) = 'm/s'
      CHREF(185) = 'UG_W'
      UNPLOT(185) = 'm/s'
      UNREF(185) = 'm/s'
      CHREF(186) = 'WL_W'
      UNPLOT(186) = 'm/s'
      UNREF(186) = 'm/s'
      CHREF(187) = 'WG_W'
      UNPLOT(187) = 'm/s'
      UNREF(187) = 'm/s'
      CHREF(188) = 'IPLW'
      UNREF(188) = 'kg'
      CHREF(189) = 'IPNW'
      UNREF(189) = 'kg'
      CHREF(190) = 'IMGT'
      UNREF(190) = 'kg'
      CHREF(191) = 'IMW '
      UNREF(191) = 'kg'
      CHREF(192) = 'IMA '
      UNREF(192) = 'kg'
      CHREF(193) = 'IMO '
      UNREF(193) = 'kg'
      CHREF(194) = 'IMLW'
      UNREF(194) = 'kg'
      CHREF(195) = 'IMLA'
      UNREF(195) = 'kg'
      CHREF(196) = 'IMLO'
      UNREF(196) = 'kg'
      CHREF(197) = 'IMGW'
      UNREF(197) = 'kg'
      CHREF(198) = 'IMGA'
      UNREF(198) = 'kg'
      CHREF(199) = 'IMGO'
      UNREF(199) = 'kg'
      CHREF(201) = 'RKLX'
      CHREF(202) = 'RKLY'
      CHREF(203) = 'RKLZ'
      CHREF(204) = 'XMLA'
      CHREF(205) = 'XMLS'
      CHREF(206) = ' TA '
      UNREF(206) = 'k'
      UNPLOT(206) = 'k'
      CHREF(207) = ' RH '
      CHREF(208) = ' RN '
      UNREF(208) = 'w/m^2'
      UNPLOT(208) = 'w/m^2'
      CHREF(209) = ' WS '
      UNREF(209) = 'm/s'
      UNPLOT(209) = 'm/s'
      CHREF(210) = 'SNR '
      CHREF(211) = 'SNM '
      CHREF(212) = 'SNF '
      CHREF(213) = 'T_S '
      UNREF(213) = 'k'
      CHREF(214) = 'PV_S'
      UNREF(214) = 'pa'
      CHREF(215) = 'E_SA'
      UNREF(215) = 'kg/s'
      CHREF(216) = 'PESA'
      UNREF(216) = 'kg/s'
      CHREF(217) = 'T_PA'
      UNREF(217) = 'kg/s'
      UNPLOT(217) = 'kg/s'
      CHREF(218) = 'PTPA'
      UNREF(218) = 'kg/s'
      UNPLOT(218) = 'kg/s'
      CHREF(219) = 'SXLA'
      CHREF(220) = 'XMLA'
      CHREF(221) = 'XMNA'
      CHREF(222) = 'XLA '
      CHREF(223) = 'XNA '
      CHREF(224) = ' PA '
      UNREF(224) = 'pa'
      UNPLOT(224) = 'pa'
      CHREF(225) = 'PL_S'
      UNREF(225) = 'pa'
      UNPLOT(225) = 'pa'
      CHREF(226) = 'PG_S'
      UNREF(226) = 'pa'
      UNPLOT(226) = 'pa'
      CHREF(227) = 'SL_S'
      CHREF(228) = 'QL_S'
      UNREF(228) = 'w/m^2'
      CHREF(229) = 'QH_S'
      UNREF(229) = 'w/m^2'
      CHREF(230) = 'RL_S'
      UNREF(230) = 'w/m^2'
      CHREF(231) = 'RS_S'
      UNREF(231) = 'w/m^2'
      CHREF(232) = 'QG_S'
      UNREF(232) = 'w/m^2'
      CHREF(233) = 'WB_S'
      UNREF(233) = 'kg/s'
      CHREF(234) = 'T_P '
      UNREF(234) = 'k'
      CHREF(235) = 'TP  '
      UNREF(235) = 'k'
      CHREF(236) = 'TP  '
      UNREF(236) = 'k'
      CHREF(237) = 'TP  '
      UNREF(237) = 'k'
      CHREF(238) = 'TP  '
      UNREF(238) = 'k'
      CHREF(239) = 'RFIM'
      UNREF(239) = 'kg'
      CHREF(240) = 'TSO '
      UNREF(240) = 'kg'
      UNPLOT(240) = 'kg'
      CHREF(241) = 'XSO '
      CHREF(242) = 'CSO '
      UNREF(242) = 'kg/m^3'
      UNPLOT(242) = 'kg/m^3'
      CHREF(243) = 'RABS'
      UNREF(243) = 's/m'
      UNPLOT(243) = 's/m'
      CHREF(244) = 'PV_S'
      UNREF(244) = 'm^3/s'
      UNPLOT(244) = 'm^3/s'
      CHREF(245) = 'PM_S'
      UNREF(245) = 'kg/s'
      UNPLOT(245) = 'kg/s'
      CHREF(246) = 'PM_A'
      UNREF(246) = 'kg/s'
      UNPLOT(246) = 'kg/s'
      CHREF(247) = ' UK '
      UNREF(247) = 'm^2'
      UNPLOT(247) = 'm^2'
      CHREF(248) = ' VK '
      UNREF(248) = 'm^2'
      UNPLOT(248) = 'm^2'
      CHREF(249) = ' WK '
      UNREF(249) = 'm^2'
      UNPLOT(249) = 'm^2'
      CHREF(250) = ' XHW'
      CHREF(251) = ' XHA'
      CHREF(252) = ' XHO'
      CHREF(253) = 'RHOH'
      UNREF(253) = 'kg/m^3'
      UNPLOT(253) = 'kg/m^3'
      CHREF(254) = ' SH '
      CHREF(255) = ' PH '
      UNREF(255) = 'Pa'
      UNPLOT(255) = 'Pa'
      CHREF(256) = 'IMHW'
      UNREF(256) = 'kg'
      UNPLOT(256) = 'kg'
      CHREF(257) = 'IMHA'
      UNREF(257) = 'kg'
      UNPLOT(257) = 'kg'
      CHREF(258) = 'IMHO'
      UNREF(258) = 'kg'
      UNPLOT(258) = 'kg'
      CHREF(259) = 'IMLO'
      UNREF(259) = 'kg'
      UNPLOT(259) = 'kg'
      CHREF(260) = 'IMGO'
      UNREF(260) = 'kg'
      UNPLOT(260) = 'kg'
      CHREF(261) = 'IMSW'
      UNREF(261) = 'kg'
      UNPLOT(261) = 'kg'
      CHREF(262) = 'IMSA'
      UNREF(262) = 'kg'
      UNPLOT(262) = 'kg'
      CHREF(263) = 'IMSO'
      UNREF(263) = 'kg'
      UNPLOT(263) = 'kg'
      CHREF(264) = ' SS '
      CHREF(265) = 'XMHW'
      CHREF(266) = 'XMHA'
      CHREF(267) = 'XMHO'
      CHREF(274) = 'RWRO'
      UNREF(274) = 'm^3/s'
      UNPLOT(274) = 'm^3/s'
      CHREF(275) = 'WNTP'
      UNREF(275) = 'Pa'
      UNPLOT(275) = 'Pa'
      CHREF(276) = 'WNBP'
      UNREF(276) = 'Pa'
      UNPLOT(276) = 'Pa'
      CHREF(277) = 'DMHW'
      UNREF(277) = 'kg'
      UNPLOT(277) = 'kg'
      CHREF(278) = 'DMHA'
      UNREF(278) = 'kg'
      UNPLOT(278) = 'kg'
      CHREF(279) = 'DMHO'
      UNREF(279) = 'kg'
      UNPLOT(279) = 'kg'
      CHREF(280) = 'DMLO'
      UNREF(280) = 'kg'
      UNPLOT(280) = 'kg'
      CHREF(281) = 'DMGO'
      UNREF(281) = 'kg'
      UNPLOT(281) = 'kg'
      CHREF(282) = 'DMW'
      UNREF(282) = 'kg'
      UNPLOT(282) = 'kg'
      CHREF(283) = 'DMA'
      UNREF(283) = 'kg'
      UNPLOT(283) = 'kg'
      CHREF(284) = 'DMO'
      UNREF(284) = 'kg'
      UNPLOT(284) = 'kg'
      CHREF(285) = 'SWP'
      UNREF(285) = 'Pa'
      UNPLOT(285) = 'Pa'
      CHREF(286) = 'SWT'
      UNREF(286) = 'C'
      UNPLOT(286) = 'C'
      CHREF(287) = 'HLSP'
      UNREF(287) = 'm'
      UNPLOT(287) = 'm'
      CHREF(288) = 'HNSP'
      UNREF(288) = 'm'
      UNPLOT(288) = 'm'
      CHREF(289) = 'ET'
      UNREF(289) = 'm/s'
      UNPLOT(289) = 'm/s'
      CHSF(1,1) = ' UQV'
      CHSF(1,2) = ' VQV'
      CHSF(1,3) = ' WQV'
      CHSF(2,1) = ' ULV'
      CHSF(2,2) = ' VLV'
      CHSF(2,3) = ' WLV'
      CHSF(3,1) = ' UGV'
      CHSF(3,2) = ' VGV'
      CHSF(3,3) = ' WGV'
      CHSF(4,1) = ' UNV'
      CHSF(4,2) = ' VNV'
      CHSF(4,3) = ' WNV'
      CHSF(5,1) = ' ULM'
      CHSF(5,2) = ' VLM'
      CHSF(5,3) = ' WLM'
      CHSF(6,1) = ' UGM'
      CHSF(6,2) = ' VGM'
      CHSF(6,3) = ' WGM'
      CHSF(7,1) = ' UNM'
      CHSF(7,2) = ' VNM'
      CHSF(7,3) = ' WNM'
      CHSF(8,1) = ' USM'
      CHSF(8,2) = ' VSM'
      CHSF(8,3) = ' WSM'
      CHSF(9,1) = ' ULO'
      CHSF(9,2) = ' VLO'
      CHSF(9,3) = ' WLO'
      CHSF(10,1) = ' UWM'
      CHSF(10,2) = ' VWM'
      CHSF(10,3) = ' WWM'
      CHSF(11,1) = 'UGOM'
      CHSF(11,2) = 'VGOM'
      CHSF(11,3) = 'WGOM'
      CHSF(12,1) = 'ULOM'
      CHSF(12,2) = 'VLOM'
      CHSF(12,3) = 'WLOM'
      CHSF(13,1) = ' UOM'
      CHSF(13,2) = ' VOM'
      CHSF(13,3) = ' WOM'
      CHSF(20,1) = 'UGAQ'
      CHSF(20,2) = 'VGAQ'
      CHSF(20,3) = 'WGAQ'
      CHSF(21,1) = 'UGAW'
      CHSF(21,2) = 'VGAW'
      CHSF(21,3) = 'WGAW'
      CHSF(22,1) = 'UGAA'
      CHSF(22,2) = 'VGAA'
      CHSF(22,3) = 'WGAA'
      CHSF(25,1) = 'UGDQ'
      CHSF(25,2) = 'VGDQ'
      CHSF(25,3) = 'WGDQ'
      CHSF(26,1) = 'UGDW'
      CHSF(26,2) = 'VGDW'
      CHSF(26,3) = 'WGDW'
      CHSF(27,1) = 'UGDA'
      CHSF(27,2) = 'VGDA'
      CHSF(27,3) = 'WGDA'
      CHSF(28,1) = 'UGAM'
      CHSF(28,2) = 'VGAM'
      CHSF(28,3) = 'WGAM'
      CHSF(29,1) = 'ULAM'
      CHSF(29,2) = 'VLAM'
      CHSF(29,3) = 'WLAM'
      CHSF(30,1) = ' UAM'
      CHSF(30,2) = ' VAM'
      CHSF(30,3) = ' WAM'
      CHSF(31,1) = 'UGAO'
      CHSF(31,2) = 'VGAO'
      CHSF(31,3) = 'WGAO'
      CHSF(32,1) = 'UGDO'
      CHSF(32,2) = 'VGDO'
      CHSF(32,3) = 'WGDO'
      CHSF(33,1) = 'UGO'
      CHSF(33,2) = 'VGO'
      CHSF(33,3) = 'WGO'
      CHSF(34,1) = ' AE '
      CHSF(34,2) = ' AE '
      CHSF(34,3) = ' AE '
      CHSF(35,1) = ' PE '
      CHSF(35,2) = ' PE '
      CHSF(35,3) = ' PE '
      CHSF(36,1) = ' AT '
      CHSF(36,2) = ' AT '
      CHSF(36,3) = ' AT '
      CHSF(37,1) = ' PT '
      CHSF(37,2) = ' PT '
      CHSF(37,3) = ' PT '
      CHSF(38,1) = ' NTR'
      CHSF(38,2) = ' NTR'
      CHSF(38,3) = ' NTR'
      CHSF(39,1) = 'NSWR'
      CHSF(39,2) = 'NSWR'
      CHSF(39,3) = 'NSWR'
      CHSF(40,1) = 'NLWR'
      CHSF(40,2) = 'NLWR'
      CHSF(40,3) = 'NLWR'
      CHSF(41,1) = ' WMB'
      CHSF(41,2) = ' WMB'
      CHSF(41,3) = ' WMB'
      CHSF(42,1) = 'RWRO'
      CHSF(42,2) = 'RWRO'
      CHSF(42,3) = 'RWRO'
      CHSF(43,1) = 'ULWM'
      CHSF(43,2) = 'VLWM'
      CHSF(43,3) = 'WLWM'
      CHSF(44,1) = 'UGWM'
      CHSF(44,2) = 'VGWM'
      CHSF(44,3) = 'WGWM'
      CHSF(45,1) = 'UWM'
      CHSF(45,2) = 'VWM'
      CHSF(45,3) = 'WWM'
      CHSF(46,1) = 'ULAC'
      CHSF(46,2) = 'VLAC'
      CHSF(46,3) = 'WLAC'
      CHSF(47,1) = 'ULDC'
      CHSF(47,2) = 'VLDC'
      CHSF(47,3) = 'WLDC'
      CHSF(48,1) = 'UGAC'
      CHSF(48,2) = 'VGAC'
      CHSF(48,3) = 'WGAC'
      CHSF(49,1) = 'UGDC'
      CHSF(49,2) = 'VGDC'
      CHSF(49,3) = 'WGDC'
      CHSF(50,1) = 'UTAC'
      CHSF(50,2) = 'VTAC'
      CHSF(50,3) = 'WTAC'
      CHSF(51,1) = 'UTDC'
      CHSF(51,2) = 'VTDC'
      CHSF(51,3) = 'WTDC'
      CHSF(52,1) = 'ET'
      CHSF(52,2) = 'ET'
      CHSF(52,3) = 'ET'
      CHSF(100,1) = ' UCM'
      CHSF(100,2) = ' VCM'
      CHSF(100,3) = ' WCM'
      DO 70 L = 1,((LSOLU**LC)+((LSPR+LSPT)**LR)+2-LC-LR)
        M = 400 + (L-1)*33
        WRITE(FORM1(3:3),'(I1)') ICOUNT(L)
        CHREF(M+1)(1:1) = 'C'
        UNPLOT(M+1) = '1/m^3'
        UNREF(M+1) = '1/m^3'
        WRITE( CHREF(M+1)(2:),FORM1) L
        CHREF(M+2)(1:2) = 'CL'
        UNPLOT(M+2) = '1/m^3'
        UNREF(M+2) = '1/m^3'
        WRITE( CHREF(M+2)(3:),FORM1) L
        CHREF(M+3)(1:2) = 'CG'
        UNPLOT(M+3) = '1/m^3'
        UNREF(M+3) = '1/m^3'
        WRITE( CHREF(M+3)(3:),FORM1) L
        CHREF(M+4)(1:2) = 'CN'
        UNPLOT(M+4) = '1/m^3'
        UNREF(M+4) = '1/m^3'
        WRITE( CHREF(M+4)(3:),FORM1) L
        CHREF(M+5)(1:2) = 'YL'
        WRITE( CHREF(M+5)(3:),FORM1) L
        CHREF(M+6)(1:2) = 'YG'
        WRITE( CHREF(M+6)(3:),FORM1) L
        CHREF(M+7)(1:3) = 'YN'
        WRITE( CHREF(M+7)(3:),FORM1) L
        CHREF(M+8)(1:2) = 'UC'
        UNPLOT(M+8) = '1/m^2 s'
        UNREF(M+8) = '1/m^2 s'
        WRITE( CHREF(M+8)(3:),FORM1) L
        CHREF(M+9)(1:2) = 'VC'
        UNPLOT(M+9) = '1/m^2 s'
        UNREF(M+9) = '1/m^2 s'
        WRITE( CHREF(M+9)(3:),FORM1) L
        CHREF(M+10)(1:2) = 'WC'
        UNPLOT(M+10) = '1/m^2 s'
        UNREF(M+10) = '1/m^2 s'
        WRITE( CHREF(M+10)(3:),FORM1) L
        CHREF(M+11)(1:3) = 'SRC'
        WRITE( CHREF(M+11)(3:),FORM1) L
        CHREF(M+12)(1:1) = 'C'
        UNPLOT(M+12) = '1/m^3'
        UNREF(M+12) = '1/m^3'
        WRITE( CHREF(M+12)(3:),FORM1) L
        CHREF(M+13)(1:2) = 'CL'
        UNPLOT(M+13) = '1/m^3'
        UNREF(M+13) = '1/m^3'
        WRITE( CHREF(M+13)(3:),FORM1) L
        CHREF(M+14)(1:2) = 'CN'
        UNPLOT(M+14) = '1/m^3'
        UNREF(M+14) = '1/m^3'
        WRITE( CHREF(M+14)(3:),FORM1) L
        CHREF(M+15)(1:2) = 'CS'
        UNPLOT(M+15) = '1/m^3'
        UNREF(M+15) = '1/m^3'
        WRITE( CHREF(M+15)(3:),FORM1) L
        CHREF(M+16)(1:3) = 'UCL'
        UNPLOT(M+16) = '1/m^2 s'
        UNREF(M+16) = '1/m^2 s'
        WRITE( CHREF(M+16)(4:),FORM1) L
        CHREF(M+17)(1:3) = 'VCL'
        UNPLOT(M+17) = '1/m^2 s'
        UNREF(M+17) = '1/m^2 s'
        WRITE( CHREF(M+17)(4:),FORM1) L
        CHREF(M+18)(1:3) = 'WCL'
        UNPLOT(M+18) = '1/m^2 s'
        UNREF(M+18) = '1/m^2 s'
        WRITE( CHREF(M+18)(4:),FORM1) L
        CHREF(M+19)(1:3) = 'UCN'
        UNPLOT(M+19) = '1/m^2 s'
        UNREF(M+19) = '1/m^2 s'
        WRITE( CHREF(M+19)(4:),FORM1) L
        CHREF(M+20)(1:3) = 'VCN'
        UNPLOT(M+20) = '1/m^2 s'
        UNREF(M+20) = '1/m^2 s'
        WRITE( CHREF(M+20)(4:),FORM1) L
        CHREF(M+21)(1:3) = 'WCN'
        UNPLOT(M+21) = '1/m^2 s'
        UNREF(M+21) = '1/m^2 s'
        WRITE( CHREF(M+21)(4:),FORM1) L
        CHREF(M+22)(1:3) = 'FCB'
        UNPLOT(M+22) = '1/s'
        UNREF(M+22) = '1/s'
        WRITE( CHREF(M+22)(4:),FORM1) L
        CHREF(M+23)(1:3) = 'ICM'
        WRITE( CHREF(M+23)(4:),FORM1) L
   70 CONTINUE
!
!---  Reset subroutine character string ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!do i=1,num_nodes
!print *,'boundarywell ---------',me,num_nodes
!enddo

!
!---  End of IN_OUTP group  ---
!
      RETURN
      END



