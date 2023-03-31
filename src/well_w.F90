  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE CHK_COUP_WELL
!
!-------------------------Disclaimer-----------------------------------!
!
!     This material was prepared as an account of work sponsored by
!     an agency of the United States Government. Neither the
!     United States Government nor the United States Department of
!     Energy, nor Battelle, nor any of their emp#loyees, makes any
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
!
!     STOMP-CO2e
!
!     Define well nodes, determine trajectory points, and 
!     check for well trajectories within node surface planes
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 31 March 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE JACOB
  USE GRID
  USE FILES
  USE COUP_WELL
  USE CONST
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
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 XPX(5),YPX(5),ZPX(5)
  REAL*8 XIX(2),YIX(2),ZIX(2)
  REAL*8 PAX(3),PBX(3),PCX(3),PBCX(3)
  REAL*8 AJ(3,3),BJ(3),IJ(3)
  INTEGER ISX(4,6),JSX(4,6),KSX(4,6)
  INTEGER N1X(4),N2X(4)
  CHARACTER*9 FORM1
  CHARACTER*8 FORM2
!
!----------------------Data Statements---------------------------------!
!
  SAVE ISX,JSX,KSX,N1X,N2X
  SAVE FORM
  DATA ISX / 0,1,1,0,0,0,1,1,0,0,0,0,1,1,1,1,0,1,1,0,0,0,1,1 /
  DATA JSX / 0,0,1,1,0,0,0,0,0,1,1,0,0,0,1,1,1,1,1,1,0,1,1,0 /
  DATA KSX / 0,0,0,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,1,1,1,1 /
  DATA N1X / 2,3,4,1 /
  DATA N2X / 1,2,3,4 /
  DATA FORM1 /'(2X,I1,$)'/
  DATA FORM2 /'(2X,I1)'/
!
!----------------------Executable Lines--------------------------------!
!
  me=ga_nodeid()
!  ISUB_LOG = ISUB_LOG+1
!  SUB_LOG(ISUB_LOG) = '/CHK_COUP_WELL'
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $'
  EPSLX = 1.D-12
  allocate(p_cw(3,n_cw))
  IF( IEO.EQ.2 .OR. IEO.EQ.4 ) THEN  ! if 'zero solutes' is specified (set in step.F90) - Bryan
   DO NCW = 1,N_L_CW
    ngcwx = id_cw(7,ncw)
    n = iwn_cw(id_cw(3,ncw)) ! field node for local interval index 
        if(iwt_cw(n) > 0) then ! is the first interval of the well
                ngcwx = id_cw(7,ncw) ! global well number
                p_cw(2,ncw)=p_cw_g(ngcwx)
        endif
   ENDDO
    
  else
    p_cw = -1.d20
  endif
  allocate(dnr_cw(n_cw))
  dnr_cw = 0.d0
!
!---  Loop over coupled wells ---
!
!
!---  Initialize coupled well pressure to be in hydrostatic
!     equilibrium with local pressure  ---
!
  DO 700 NCW = 1,N_L_CW
!
!---    Coupled-well pressure previously initialized  ---
!
    ncw_g = id_cw(7,ncw)
    CALL EQUIL_COUP_WELL( NCW )
  700 CONTINUE

!
!---  End of CHK_COUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE EQUIL_COUP_WELL( NCW )
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
!
!     STOMP-CO2e
!
!     Equilibrate coupled-well pressure with formation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 10 May 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE HYST
  USE GRID
  USE FDVP
  USE COUP_WELL
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
!----------------------Executable Lines--------------------------------!
!
!  ISUB_LOG = ISUB_LOG+1
!  SUB_LOG(ISUB_LOG) = '/EQUIL_COUP_WELL'
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $'
  me=ga_nodeid()
!
!--- For both injection and withdrawl well, equilibrate with first well node  ---
!--- Inj well is defined from top to bottom; withdrawl is from bottom to top.
!
   N = IWN_CW(ID_CW(3,NCW)) ! field node index
   if(iwt_cw(n) > 0) then   ! it is the 1st interval
    ncw_g = id_cw(7,ncw)    ! global well index
    IDX = ID_CW(1,NCW_G)    ! global index for the staring interval 
    idlx = id_cw(3,ncw)     ! local index for the starting interval
    XCWX = XTP_CW(1,IDX)    ! starting x location of the first interval 
    YCWX = YTP_CW(1,IDX)
    ZCWX = ZTP_CW(1,IDX)
    XPCWX = 5.D-1*(XP_CW(2,IDLX)+XP_CW(1,IDLX))
    YPCWX = 5.D-1*(YP_CW(2,IDLX)+YP_CW(1,IDLX))
    ZPCWX = 5.D-1*(ZP_CW(2,IDLX)+ZP_CW(1,IDLX))
    
  endif

!---  Cylindrical coordinates with azimuthal symmetry,
!     centrally located wells  ---
!
  IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. JFLD.EQ.1 ) THEN
    XPNX = 0.D+0
    YPNX = 0.D+0
    ZPNX = ZP(N)
!
!---  Cylindrical coordinates  ---
!
  ELSEIF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
    XPNX = XP(N)*COS(YP(N))
    YPNX = XP(N)*SIN(YP(N))
    ZPNX = ZP(N)
!
!---  Cartesian or boundary-fitted orthogonal coordinates  ---
!
  ELSE
    XPNX = XP(N)
    YPNX = YP(N)
    ZPNX = ZP(N)
  ENDIF
!
!---  Adjust the formation pressure from the node centroid
!     to the coupled-well node centroid  ---
!
  IF( (SG(2,N)-SGT(2,N)).GT.EPSL ) THEN
    PGFX = PG(2,N) - ((XPCWX-XPNX)*GRVPX(N) +  &
      (YPCWX-YPNX)*GRVPY(N) +  &
      (ZPCWX-ZPNX)*GRVPZ(N))*RHOG(2,N)
  ELSE
     PGFX = PG(2,N) - ((XPCWX-XPNX)*GRVPX(N) +  &
       (YPCWX-YPNX)*GRVPY(N) +  &
       (ZPCWX-ZPNX)*GRVPZ(N))*RHOL(2,N)
  ENDIF
  PLFX = PL(2,N) - ((XPCWX-XPNX)*GRVPX(N) +  &
    (YPCWX-YPNX)*GRVPY(N) +  &
    (ZPCWX-ZPNX)*GRVPZ(N))*RHOL(2,N)
! Update water density - Bryan
  T_CWX = T(2,N)
  P_CWX = MAX( PGFX,PLFX ) 
  ngcwx = id_cw(7,ncw)
  CALL WATSP( T_CWX,PVW(2,N))
  PVW_CWX = PVW(2,N)
  PX_CWX = MAX(P_CWX+PATM,PVW_CWX)
  CALL WATLQD( T_CWX,PX_CWX,RHOG_CWX)
!
!---  Gas density  --- Need to ask Yilin


!  PVA_CWX = MAX( P_CWX+PATM-PVW_CWX,0.D+0 )
!  XMGA_CWX = PVA_CWX/(P_CWX+PATM)
!  XMGW_CWX = PVW_CWX/(P_CWX+PATM)
!  XGA_CWX = (XMGA_CWX*WTMA)/(XMGA_CWX*WTMA + XMGW_CWX*WTMW)
!  XGW_CWX = (XMGW_CWX*WTMW)/(XMGA_CWX*WTMA + XMGW_CWX*WTMW)
 ! CALL DENS_A( T_CWX,PVA_CWX,RHOGA_CWX,I_VX )
 ! ISRX = 2
!  CALL DENS_W( T_CWX,PVW_CWX,RHOLW_CWX,RHOGW_CWX,ISRX )
 ! RHOG_CWX = XGA_CWX*RHOGA_CWX + XGW_CWX*RHOGW_CWX

!
!---  Adjust the well pressure from the coupled-well node centroid 
!     to the top (injection) or bottom (withdrawl) of the 
!     coupled-well  ---
!
  P_CW(2,NCW) = P_CWX - ((XCWX-XPCWX)*GRVPX(N) +  &
    (YCWX-YPCWX)*GRVPY(N) + (ZCWX-ZPCWX)*GRVPZ(N))*RHOG_CWX
!  P_CW(2,NCW) = P_CWX - ((XCWX-XPCWX)*GRVPX(N) +  &
!    (YCWX-YPCWX)*GRVPY(N) + (ZCWX-ZPCWX)*GRVPZ(N))*RHOL(2,N)

!  write(*,'(a,I5,a,I3,a,I3,a,F16.8,a,F16.8,a,F16.8,a,F16.8)') 'ME:', me,', g well # ',ngcwx,', l well #:',ncw, &
!                ', PCWX:',P_CWX, ', PL: ',PL(2,N),', PLFX: ',PLFX,', P_CW2',P_CW(2,NCW)


!!
!!---  Aqueous unsaturated conditions  ---
!!
!      IF( SG(2,N)-SGT(2,N).GT.EPSL ) THEN
!        P_CW(2,NCW) = PG(2,N) - ((XCWX-XP(N))*GRVPX(N) + 
!     &    (YCWX-YP(N))*GRVPY(N) + (ZCWX-ZP(N))*GRVPZ(N))*RHOG_CWX
!!
!!---  Aqueous saturated conditions  ---
!!
!      ELSE
!        P_CW(2,NCW) = PL(2,N) - ((XCWX-XP(N))*GRVPX(N) + 
!     &    (YCWX-YP(N))*GRVPY(N) + (ZCWX-ZP(N))*GRVPZ(N))*RHOG_CWX
!      ENDIF
!
!---  Reset subroutine character string ---
!
!  ISUB_LOG = ISUB_LOG-1
!
!---  End of EQUIL_COUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE FLUX_COUP_WELL
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
!
!     STOMP-CO2e
!
!     Mass flux of water and CO2 between coupled-well nodes and 
!     field nodes.
!
!     CO2 mass balance residuals for injection type coupled wells.
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 19 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE PORMED
  USE JACOB
  USE HYST
  USE GRID
  USE FDVP
  USE COUP_WELL
  USE CONST
  USE TRNSPT
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  REAL*8 XPX(2),YPX(2),ZPX(2)
  REAL*8,DIMENSION(:),ALLOCATABLE:: VAR_CWX
  INTEGER, SAVE :: IALLOC
  DATA IALLOC /0/
  double precision, dimension(:), allocatable :: qm_cwx  
  double precision, dimension(:), allocatable :: tqm_cwx  
  double precision, dimension(:,:), allocatable :: ptablex
  double precision, dimension(:), allocatable :: f_cw_max
  integer, dimension(:), allocatable :: idp_cwx
  REAL*8 IND_GIT 
  REAL*8 sf_factor,alpha  
  REAL*8 K_abs,f_max 
  REAL*8 var_cwx2_tmp  
  
!
!----------------------Executable Lines--------------------------------!
!
  
IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $'

  me=ga_nodeid()
! 
!--- set up pressure table for the wells
!
!
!---  Initialize coupled-well parameters ---
!
  ALLOCATE(VAR_CWX(5+NSOLU))

  rs_cw = 0.d0
  if(.not.allocated(idp_cwx)) then
    allocate(idp_cwx(n_cw))
  endif
  idp_cwx = 0
  t_acwx = 0.d0
  g_qm_cw = 0.d0
  DO 40 NCW = 1,N_L_CW
!
!---    Zero coupled-well fluxes ---
!
    QM_CW(1,NCW) = 0.D+0
    QM_CW(3,NCW) = 0.D+0
    QM_CW(5,NCW) = 0.D+0
!
!---    Flow controlled well ---
!
    ID_CW(8,NCW) = 0
!
!---    Loop over coupled-well nodes  ---
!
    DO 30 NWN = ID_CW(3,NCW),ID_CW(4,NCW)
!
!---      Loop over increment indices  ---
!
      DO 20 M = 1,ISVC+2
        FXW_CW(M,NWN) = 0.D+0
     20     CONTINUE
     30  CONTINUE
      ncwx = ncw
      ncx = 0
      do iwnx = id_cw(3,ncwx),id_cw(4,ncwx)
        ncx = ncx + 1
        igwnx = iwi_cw(iwnx)
        ngcwx = id_cw(7,ncwx)
        n=iwn_cw(iwnx)
        t_acwx(igwnx,ngcwx) = t(2,n)
      enddo
     40 CONTINUE       
     ngcwx2 = size(t_acwx,dim=1)*n_cw
     call ga_dgop(1,t_acwx,ngcwx2,'+')
     ngcwx2 = 2*n_cw
     if(.not.allocated(ptablex)) allocate(ptablex(2,n_cw))
     ptablex = 0.d0
     if(.not.allocated(qm_cwx))allocate(qm_cwx(n_cw))
     if(.not.allocated(tqm_cwx))allocate(tqm_cwx(n_cw))
!     if(.not.allocated(f_cw_max))allocate(f_cw_max(n_cw))
45   continue 
     p_cw_table = 0.d0
     ptablex = 0.d0
     do ncw=1,n_l_cw
       ncwx=ncw
       n=iwn_cw(id_cw(3,ncwx))
       if(iwt_cw(n) > 0) then
        icwx = w_loc(iwt_cw(n))
        ngcwx = id_cw(7,icwx)
        ptablex(1:2,ngcwx) = p_cw(2:3,icwx)
       endif
     enddo
     call ga_dgop(1,ptablex,ngcwx2,'+')
     do ncw=1,n_cw
       p_cw_table(1:2,1,ncw) = ptablex(1:2,ncw)
!      if (me==0) write(*,'(a,I5,a,I3,a,2F16.8)') 'ME: ',me,', ncw: ',ncw,', p_cw_table: ',p_cw_table(1:2,1,ncw)
     enddo
     nrepeat = 0
!
!---  Loop over coupled wells ---
!
  QM_CWX = 0.D+0
  tqm_cwx = 0.d0
!  f_cw_max = 0.d+0
  DO 500 NCW = 1,N_L_CW
    dq_cwx = 1.d-6
    ngcwx = id_cw(7,ncw) ! global well index
    p_cw(2:3,ncw) = p_cw_table(1:2,1,ngcwx) ! from ptablex

    IF (GIT_CW(ngcwx).GT.0) THEN
        IND_GIT = 1.0
    ELSEIF(GIT_CW(ngcwx).LT.0) THEN
        IND_GIT = -1.0
    ENDIF

    TMZ = TM
    IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
    IF( ICC_CW(NGCWX).EQ.1 ) TMZ = MOD( TM,VAR_CW(1,IM_CW(NGCWX),NGCWX) )
!
!---    Coupled well is inactive set well pressure to be in 
!       equilibrium with formation  ---
!
    IF( TMZ.LE.VAR_CW(1,1,NGCWX) .OR.  &
      G_QM_CW(2,NGCWX).GE.TML_CW(NGCWX) ) THEN   ! change to index 1-Bryan
      CALL EQUIL_COUP_WELL( NCW )
      ID_CW(8,NCW) = 1                       ! pressure control
      idp_cwx(ngcwx) = id_cw(8,ncw)
      GOTO 500
    ENDIF
    VAR_CWX(:) = 0.0
    IF( IM_CW(NGCWX).EQ.1 ) THEN
      DO 80 N = 2,5+NSOLU
        VAR_CWX(N) = VAR_CW(N,1,NGCWX)
     80     CONTINUE
!
!---      Limit injection rate by total injected mass  ---
!
      VAR_CWX(2) = MIN( VAR_CWX(2), &
        ((TML_CW(NGCWX)-G_QM_CW(2,NGCWX))*DTI) )
      PL_CW(NCW) = VAR_CWX(3) - patm
    ELSE
      DO 100 M = 2,IM_CW(NGCWX)
        IF( TMZ.LE.VAR_CW(1,M,NGCWX) ) THEN
          TD_CW = VAR_CW(1,M,NGCWX)-VAR_CW(1,M-1,NGCWX)
          DT_CW = MIN( VAR_CW(1,M,NGCWX)-TMZ,DT )
          TF_CW = (TMZ-VAR_CW(1,M-1,NGCWX))/TD_CW
          DO 90 N = 2,5+NSOLU
            VAR_CWX(N) = VAR_CW(N,M-1,NGCWX) +  &
              TF_CW*(VAR_CW(N,M,NGCWX)-VAR_CW(N,M-1,NGCWX))
     90         CONTINUE
!
!---          Limit injection rate by total injected mass  ---
!
          VAR_CWX(2) = MIN( VAR_CWX(2), &
            ((TML_CW(NGCWX)-G_QM_CW(2,NGCWX))*DTI) )
          PL_CW(NCW) = VAR_CWX(3) - patm
          if(VAR_CWX(2).NE.0.0) THEN
               GOTO 110
          else
               GOTO 101
          endif 
        ENDIF
    100     CONTINUE
!
!---      Coupled well is inactive set well pressure to be in 
!         equilibrium with formation  ---
!
    101 CONTINUE 
      CALL EQUIL_COUP_WELL( NCW )
      ID_CW(8,NCW) = 1
      idp_cwx(ngcwx) = id_cw(8,ncw)
      
      GOTO 500

    ENDIF
    nc = 0
    110   CONTINUE
    call PRES_COUP_WELL(NCW)

!
!---    Load CO2 mass flux for use in RSDL_COUP_WELL ---
!
    FX_CW(NCW) = VAR_CWX(2)
!
!---    Load pressure limit for use in UPDT_COUP_WELL ---
!
    PL_CW(NCW) = VAR_CWX(3) - patm
!
!---    Pressure controlled well ---
!
    IF((GIT_CW(NGCWX).GT.0) .AND.( PL_CW(NCW)-P_CW(2,NCW).LT.EPSL) ) THEN 
         ID_CW(8,NCW) = 1
    ELSEIF ((GIT_CW(NGCWX).LT.0) .AND.( PL_CW(NCW)-P_CW(2,NCW).GT.EPSL) ) THEN
         ID_CW(8,NCW) = 1    
    ENDIF
    idp_cwx(ngcwx) = id_cw(8,ncw)
!
!---    Loop over increment indices ---
!
    DO 300 M = 1,ISVC+2
      MW = MCW(M)
      MF = MFD(M)
!      IF( IT_CW(NCW).GT.0 ) THEN
        N = IWN_CW(ID_CW(3,NCW))
        P_CWX = p_cw_table(mw-1,1,ngcwx)
!
!---        Nonisothermal simulations  ---
!
        IF( ISLC(30).EQ.0 ) THEN
          T_CWX = VAR_CWX(5)
        ELSE
          T_CWX = T(2,N)
        ENDIF
        XLS_CWX = 0.D+0
        call WATSP( T_CWX,PSW_CWX)  ! satruration pressure - Bryan

!       Update water density - Bryan
        PX_CWX = P_CWX + PATM
        CALL WATLQD( T_CWX,PX_CWX,RHOG_CWX )

!
!---        Store top of coupled-well location in previous
!           coupled-well node location  ---
!
!      ENDIF
!
!---      Loop over the nodes in the coupled well ---
!
      npx = 0
      nfx = 0
     
      DO 200 NWN = ID_CW(3,NCW),ID_CW(4,NCW)
       
        ngwnx = iwi_cw(nwn)
        ngwngx = ngwnx+id_cw(1,ngcwx)-1
        N = IWN_CW(NWN)
        IZN = IZ(N)
!
!---        Nonisothermal simulations  ---

        IF( ISLC(30).EQ.0 ) THEN
          T_CWX = VAR_CWX(5)
        ELSE
          T_CWX = T(2,N)
        ENDIF
!
!---        Coupled-well node centroids and projections ---
!
        XLX = ABS(XP_CW(2,NWN)-XP_CW(1,NWN))
        YLX = ABS(YP_CW(2,NWN)-YP_CW(1,NWN))
        ZLX = ABS(ZP_CW(2,NWN)-ZP_CW(1,NWN))
        XPX(1) = XP_CW(1,nwn)
        YPX(1) = YP_CW(1,nwn)
        ZPX(1) = ZP_CW(1,nwn)
        XPX(2) = 5.D-1*(XP_CW(2,NWN)+XP_CW(1,NWN))
        YPX(2) = 5.D-1*(YP_CW(2,NWN)+YP_CW(1,NWN))
        ZPX(2) = 5.D-1*(ZP_CW(2,NWN)+ZP_CW(1,NWN))
!
!---        Cylindrical coordinates with azimuthal symmetry,
!           centrally located wells  ---
!
        IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. JFLD.EQ.1 ) THEN
          XPNX = 0.D+0
          YPNX = 0.D+0
          ZPNX = ZP(N)
!
!---        Cylindrical coordinates  ---
!
        ELSEIF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
          XPNX = XP(N)*COS(YP(N))
          YPNX = XP(N)*SIN(YP(N))
          ZPNX = ZP(N)
!
!---        Cartesian or boundary-fitted orthogonal coordinates  ---
!
        ELSE
          XPNX = XP(N)
          YPNX = YP(N)
          ZPNX = ZP(N)
        ENDIF
!
!---        Well pressure using previous coupled-well node density ---
!
        P_CWX = p_cw_table(mw-1,ngwnx+1,ngcwx)
!
!---        Well pressure at the node centroid, used for coupled-well
!           nodal output  ---
!
        IF( M.EQ.1 ) THEN
          if(npx /= n) then
           nfx = nfx+1
           PF_CW(NFX) = P_CWX - ((XPNX-XPX(1))*GRVPX(N) +  &  ! Need to confirm-Bryan
            (YPNX-YPX(1))*GRVPY(N) +  &
!                (ZPNX-ZPX(1))*GRVPZ(N))*RHOL(2,N)  ! change to water density - Bryan
            (ZPNX-ZPX(1))*GRVPZ(N))*RHOG_CWX
          endif
        ENDIF
!
!---        Adjust the formation pressure to the coupled-well node
!           centroid  ---
!
        IF( (SG(MF,N)-SGT(MF,N)).GT.EPSL ) THEN
          PGFX = PG(MF,N) - ((XPX(2)-XPNX)*GRVPX(N) +  &
            (YPX(2)-YPNX)*GRVPY(N) +  &
            (ZPX(2)-ZPNX)*GRVPZ(N))*RHOG(MF,N)
        ELSE
          PGFX = PG(MF,N) - ((XPX(2)-XPNX)*GRVPX(N) +  &
            (YPX(2)-YPNX)*GRVPY(N) +  &
            (ZPX(2)-ZPNX)*GRVPZ(N))*RHOL(MF,N)
        ENDIF

          PLFX = PL(MF,N) - ((XPX(2)-XPNX)*GRVPX(N) +  &
            (YPX(2)-YPNX)*GRVPY(N) +  &
            (ZPX(2)-ZPNX)*GRVPZ(N))*RHOL(MF,N)

!        write(*,'(a,I3,a,2I3,a,F16.8,a,F16.8,a,F16.8)')'Well #',NGCWX,', MF,NWN: ',MF,NWN,', PL',PL(MF,N),', RHOL',RHOL(MF,N),', PLFX',PLFX
!
!       Update water density - Bryan
        PX_CWX = P_CWX + PATM
        CALL WATLQD( T_CWX,PX_CWX,RHOG_CWX )

!       Update water viscosity - Bryan
        CALL WATLQV( T_CWX,PX_CWX,PSW_CWX,VISG_CWX )
!
!---        Equivalent field node radius components  ---
!
        PERMX = MAX( PERM(1,N),1.D-20 )
        PERMY = MAX( PERM(2,N),1.D-20 )
        PERMZ = MAX( PERM(3,N),1.D-20 )
        RWX = MAX( WBR_CW(ngwngx),1.D-20 )
!
!---        Cylindrical coordinates with azimuthal symmetry,
!           centrally located wells  ---

        IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. JFLD.EQ.1 ) THEN
!          ROZ = RP(I)
          RWX = MIN( RWX,9.999D-1*ROZ )
          PERMX = PERMRF(MF,N)*PERM(1,N)
          PERMY = PERMRF(MF,N)*PERM(2,N)
          WI_CWX = 2.D+0*GPI*SQRT(PERMX*PERMY)*ZLX/ &
            (LOG(ROZ/RWX)+SK_CW(ngwngx))
        ELSE
          PERMYZ = SQRT(PERMY/PERMZ)
          PERMZY = SQRT(PERMZ/PERMY)
          DXGFX = DXGF(N)/FF_CW(1,NCW)
          DYGFX = DYGF(N)/FF_CW(2,NCW)
          DZGFX = DZGF(N)/FF_CW(3,NCW)
          ROX = 2.8D-1*SQRT(PERMYZ*(DZGFX**2) + PERMZY*(DYGFX**2)) &
          /(SQRT(PERMYZ)+SQRT(PERMZY))
          PERMZX = SQRT(PERMZ/PERMX)
          PERMXZ = SQRT(PERMX/PERMZ)
          ROY = 2.8D-1*SQRT(PERMZX*(DXGFX**2) + PERMXZ*(DZGFX**2)) &
            /(SQRT(PERMZX)+SQRT(PERMXZ))
          PERMYX = SQRT(PERMY/PERMX)
          PERMXY = SQRT(PERMX/PERMY)
          ROZ = 2.8D-1*SQRT(PERMYX*(DXGFX**2) + PERMXY*(DYGFX**2)) &
            /(SQRT(PERMYX)+SQRT(PERMXY))
!
!---          Well index components  ---
!
!          PERMX = PERMRF(MF,N)*PERM(1,N)
!          PERMY = PERMRF(MF,N)*PERM(2,N)
!          PERMZ = PERMRF(MF,N)*PERM(3,N)

! Use absolute permeability to account for unsaturated condition -Bryan
!        IF (GIT_CW(NGCWX) .GT. 0) THEN 
!          PERMX = PERM(1,N)
!          PERMY = PERM(2,N)
!          PERMZ = PERM(3,N)
!        ELSEIF (GIT_CW(NGCWX) .LT. 0) THEN         
          PERMX = RKL(1,MF,N)*PERM(1,N)
          PERMY = RKL(2,MF,N)*PERM(2,N)
          PERMZ = RKL(3,MF,N)*PERM(3,N)
!        ENDIF
!          write(*,'(a,3(e16.4,2X))') 'PERMX,PERMY,PERMZ: ',PERMX,PERMY,PERMZ
          WIX = 2.D+0*GPI*SQRT(PERMY*PERMZ)*XLX/ &
            (LOG(ROX/RWX)+SK_CW(ngwngx))
          WIY = 2.D+0*GPI*SQRT(PERMX*PERMZ)*YLX/ &
            (LOG(ROY/RWX)+SK_CW(ngwngx))
          WIZ = 2.D+0*GPI*SQRT(PERMX*PERMY)*ZLX/ &
            (LOG(ROZ/RWX)+SK_CW(ngwngx))
          WI_CWX = SQRT((WIX**2) + (WIY**2) + (WIZ**2))
        ENDIF
!        IF (M==1) THEN
!            K_abs = SQRT(PERMX**2+PERMY**2+PERMZ**2)*RHOG_CWX*GRAV/VISG_CWX  !m/s
!            f_max = IND_GIT*K_abs*2.D+0*GPI*RWX*SQRT(XLX**2+YLX**2+ZLX**2)*RHOG_CWX ! kg/s
!            f_cw_max(ngcwx) = f_cw_max(ngcwx) + f_max
!        ENDIF

!
!---        Mass fluxes, positive into the node  ---

        !
        IF (GIT_CW(NGCWX) .GT. 0) THEN ! injection well
                DPLX = MAX( P_CWX-PLFX,0.D+0 )
                FX_CWX = WI_CWX*RHOG_CWX*DPLX/VISG_CWX
        ELSEIF (GIT_CW(NGCWX) .LT. 0) THEN ! production well
                DPLX = MAX( PLFX-P_CWX,0.D+0 )
                FX_CWX = - WI_CWX*RHOG_CWX*DPLX/VISG_CWX
        ENDIF
        
!        write(*,'(a,I3,a,2(I16,2X),a,1(F16.5,2X),a,3(F16.5,2X))') 'Well #',NGCWX,', M,NWN: ',M,NWN, &
!                                'FX_CWX: ',FX_CWX,'P_CWX,PLFX,DPLX',P_CWX,PLFX,DPLX       

!---        Flow from well to formation  ---
!
          FXW_CW(M,NWN) = FX_CWX
!
!---        Store current coupled-well node location in previous
!           coupled-well node location  ---
!
        XPX(1) = XPX(2)
        YPX(1) = YPX(2)
        ZPX(1) = ZPX(2)
    200     CONTINUE
    300   CONTINUE
!
!---    CO2 mass balance residuals for injection type coupled well  ---
!
    NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
    MX = (NWFX*ISVC)+2
    if(iwt_cw(iwn_cw(1))>0) then
       RS_CW(1,NCW) = VAR_CWX(2)
       RS_CW(MX,NCW) = VAR_CWX(2)
    endif
    QM_CW(1,NCW) = 0.D+0
    QM_CW(3,NCW) = 0.D+0
    QM_CW(5,NCW) = 0.D+0

!
!---    Loop over coupled-well nodes  ---
!
    DO 400 NWN = ID_CW(3,NCW),ID_CW(4,NCW)
      RS_CW(1,NCW) = RS_CW(1,NCW) - FXW_CW(1,NWN)           !change to water
      RS_CW(MX,NCW) = RS_CW(MX,NCW) - FXW_CW(ISVC+2,NWN)    !
      QM_CW(1,NCW) = QM_CW(1,NCW) + FXW_CW(1,NWN)  
      QM_CW(3,NCW) = QM_CW(3,NCW) + FXW_CW(1,NWN)
      QM_CWX(NGCWX) = QM_CWX(NGCWX) + FXW_CW(ISVC+2,NWN)
    400   CONTINUE
    tqm_cwx(ngcwx) = qm_cw(1,ncw)
!             if(ABS(VAR_CWX(2)).GT.EPSL ) write(*,'(a,I4,a,I4,a,2(F16.5,2X),a,F16.5,2X)') 'ME:',me,'. Well # ',& 
!                         ngcwx,' total mass flow rate(tqm,qm):',tqm_cwx(ngcwx),qm_cwx(ngcwx),', it should be:',VAR_CWX(2)
    500 CONTINUE
!
!---    Insufficient increment in coupled-well pressure to create
!       flow from well, increase increment  ---
!
    call ga_dgop(1,qm_cwx,n_cw,'+')
    call ga_dgop(1,tqm_cwx,n_cw,'+')
!    call ga_dgop(1,f_cw_max,n_cw,'+')

    do ncw=1,n_l_cw
     ngcwx = id_cw(7,ncw)

     IF (GIT_CW(ngcwx).GT.0) THEN
        IND_GIT = 1.0
     ELSEIF(GIT_CW(ngcwx).LT.0) THEN
        IND_GIT = -1.0
     ENDIF
     TMZ = TM
     IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
     IF( ICC_CW(NGCWX).EQ.1 ) TMZ = MOD( TM,VAR_CW(1,IM_CW(NGCWX),NGCWX) )

    IF( IM_CW(NGCWX).EQ.1 ) THEN

      DO 801 N = 2,5+NSOLU
        VAR_CWX(N) = VAR_CW(N,1,NGCWX)
     801     CONTINUE
!
!---      Limit injection rate by total injected mass  ---
!
      VAR_CWX(2) = MIN( VAR_CWX(2), &
        ((TML_CW(NGCWX)-G_QM_CW(2,NGCWX))*DTI) )
    ELSE
      DO 1001 M = 2,IM_CW(NGCWX)
        IF( TMZ.LE.VAR_CW(1,M,NGCWX) ) THEN
          TD_CW = VAR_CW(1,M,NGCWX)-VAR_CW(1,M-1,NGCWX)
          DT_CW = MIN( VAR_CW(1,M,NGCWX)-TMZ,DT )
          TF_CW = (TMZ-VAR_CW(1,M-1,NGCWX))/TD_CW
          DO 901 N = 2,5+NSOLU
            VAR_CWX(N) = VAR_CW(N,M-1,NGCWX) +  &
              TF_CW*(VAR_CW(N,M,NGCWX)-VAR_CW(N,M-1,NGCWX))
     901         CONTINUE
!
!---          Limit injection rate by total injected mass  ---
!
          VAR_CWX(2) = MIN( VAR_CWX(2), &
            ((TML_CW(NGCWX)-G_QM_CW(2,NGCWX))*DTI) )
          GOTO 1101
        ENDIF
    1001     CONTINUE
    ENDIF
    1101   CONTINUE
! Need to check this. When total K_abs*area less than the target flux
! change the target flux to it.  -Bryan
!     write(*,*) 'f_cw_max,RS_CW: ',f_cw_max(ngcwx),RS_CW(:,NCW)
!     IF (abs(f_cw_max(ngcwx))<abs(VAR_CWX(2))) THEN
!        dVAR_CWX2 = abs(f_cw_max(ngcwx)-VAR_CWX(2))
!        VAR_CWX(2) = f_cw_max(ngcwx)
!        NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
!        MX = (NWFX*ISVC)+2
!        RS_CW(1,ncw) = RS_CW(1,ncw)-IND_GIT*dVAR_CWX2
!        RS_CW(MX,ncw) = RS_CW(MX,ncw)-IND_GIT*dVAR_CWX2
!     ENDIF

     IF(ABS(QM_CWX(ngcwx)).LT.EPSL .and. idp_cwx(ngcwx) /=1 ) THEN
!        write(*,'(a,I4)') 'Insufficient pressure increment at well #',ngcwx
       DNR_CW(NCW) = 2.D+0*DNR_CW(NCW)
       P_CW(3,NCW) = P_CW(2,NCW) + IND_GIT * DNR_CW(NCW)
      IF (GIT_CW(ngcwx).GT.0 .AND. P_CW(3,NCW).GT.PL_CW(NCW) ) THEN
!        write(*,'(a,I4)') 'Well pressure larger than max pressure at well #',ngcwx
        P_CW(2,NCW) = PL_CW(NCW)
        DNR_CW(NCW) = 1.D-1
        P_CW(3,NCW) = P_CW(2,NCW) + IND_GIT* DNR_CW(NCW)
        ID_CW(8,NCW) = 1
        idp_cwx(ngcwx) = id_cw(8,ncw)
        cycle
      ELSEIF (GIT_CW(ngcwx).LT.0 .AND. P_CW(3,NCW).LT.PL_CW(NCW) ) THEN  ! less than mini pressure - Bryan
!       write(*,'(a,I4)') 'Well pressure less than minimum pressure at well #',ngcwx
        P_CW(2,NCW) = PL_CW(NCW)
        DNR_CW(NCW) = 1.D-1
        P_CW(3,NCW) = P_CW(2,NCW) - DNR_CW(NCW)
        ID_CW(8,NCW) = 1
        idp_cwx(ngcwx) = id_cw(8,ncw)
        cycle
      ENDIF
      nrepeat = ncw
!
!---      Zero coupled-well fluxes ---
!
      QM_CW(1,NCW) = 0.D+0
      QM_CW(3,NCW) = 0.D+0
!
!---      Flow controlled well ---
!
      ID_CW(8,NCW) = 0
        idp_cwx(ngcwx) = id_cw(8,ncw)
!
!---      Loop over coupled-well nodes  ---
!
      DO NWN = ID_CW(3,NCW),ID_CW(4,NCW)
!
!---        Loop over increment indices  ---
!
        DO  M = 1,LUK+2
          FXW_CW(M,NWN) = 0.D+0
        enddo
      enddo
      exit
!
!---    Excessive well flow for pressure controlled well,
!       reduce well pressure  ---
!
    ELSEIF( (ID_CW(8,NCW)).EQ.1 .AND. &
         ABS(TQM_CWX(NGCWX)).GT.ABS(VAR_CWX(2)) ) THEN
!         write(*,'(a,I4)') 'Excessive well flow at well #',NGCWX
          RS_CWX = VAR_CWX(2) - TQM_CWX(NGCWX)*(1.D+0-DQ_CWX)
          DQ_CWX = 1.D+1*DQ_CWX
          DP_CWX = -RS_CWX*DNR_CW(NCW)/ &
           (RS_CW(MX,NCW)-RS_CW(1,NCW))

      P_CW(2,NCW) = P_CW(2,NCW) + DP_CWX
      P_CW(3,NCW) = P_CW(2,NCW) + IND_GIT*DNR_CW(NCW)
!
!---      Zero coupled-well fluxes ---
!
      QM_CW(1,NCW) = 0.D+0
      QM_CW(3,NCW) = 0.D+0
!
!---      Pressure correction for pressure controlled well ---
!
      ID_CW(8,NCW) = -1
      idp_cwx(ngcwx) = id_cw(8,ncw)
!
!---      Loop over coupled-well nodes  ---
!
      DO NWN = ID_CW(3,NCW),ID_CW(4,NCW)
!
!---        Loop over increment indices  ---
!
        DO  M = 1,LUK+2
              FXW_CW(M,NWN) = 0.D+0
        enddo
       enddo
            
       nrepeat = ncw
       exit
!
!---    Acceptable pressure reduction for pressure controlled well, 
!       switch to flow controlled well  ---
!
     ELSEIF( ID_CW(8,NCW).EQ.-1 .AND. &
        ABS(TQM_CWX(NGCWX)).LT.ABS(VAR_CWX(2)) ) THEN
!          write(*,'(a,I4)') 'Acceptable pressure reduction at well #',NGCWX
          ID_CW(8,NCW) = 0
        idp_cwx(ngcwx) = id_cw(8,ncw)
     ENDIF
    enddo
         call ga_igop(1,nrepeat,1,'+')
    if(nrepeat > 0) GOTO 45 

    do ncw=1,n_l_cw
!VAR_CWX was loaded again to have correct values- Bryan
      ngcwx = id_cw(7,ncw)
      IF (GIT_CW(ngcwx).GT.0) THEN
        IND_GIT = 1.0
      ELSEIF(GIT_CW(ngcwx).LT.0) THEN
        IND_GIT = -1.0
      ENDIF 
      TMZ = TM
     IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
     IF( ICC_CW(NGCWX).EQ.1 ) TMZ = MOD( TM,VAR_CW(1,IM_CW(NGCWX),NGCWX) )

    IF( IM_CW(NGCWX).EQ.1 ) THEN

      DO 802 N = 2,5+NSOLU
        VAR_CWX(N) = VAR_CW(N,1,NGCWX)
     802     CONTINUE
!
!---      Limit injection rate by total injected mass  ---
!
      VAR_CWX(2) = MIN( VAR_CWX(2), &
        ((TML_CW(NGCWX)-G_QM_CW(2,NGCWX))*DTI) )
    ELSE
      DO 1002 M = 2,IM_CW(NGCWX)
        IF( TMZ.LE.VAR_CW(1,M,NGCWX) ) THEN
          TD_CW = VAR_CW(1,M,NGCWX)-VAR_CW(1,M-1,NGCWX)
          DT_CW = MIN( VAR_CW(1,M,NGCWX)-TMZ,DT )
          TF_CW = (TMZ-VAR_CW(1,M-1,NGCWX))/TD_CW
          DO 902 N = 2,5+NSOLU
            VAR_CWX(N) = VAR_CW(N,M-1,NGCWX) +  &
              TF_CW*(VAR_CW(N,M,NGCWX)-VAR_CW(N,M-1,NGCWX))
     902         CONTINUE
!
!---          Limit injection rate by total injected mass  ---
!
          VAR_CWX(2) = MIN( VAR_CWX(2), &
            ((TML_CW(NGCWX)-G_QM_CW(2,NGCWX))*DTI) )
               GOTO 1102
         ENDIF
    1002     CONTINUE
    ENDIF
    1102   CONTINUE
    ! Need to check this. When total K_abs*area less than the target flux
! change the target flux to it.  -Bryan
!     var_cwx2_tmp = VAR_CWX(2)
!     IF (abs(f_cw_max(ngcwx))<abs(VAR_CWX(2))) THEN
!!        write(*,'(a,I3,a,I3,a,F16.8,a,F16.8,a)') 'ME:',me,'. Target flux for well #',ngcwx,' is adjusted from ', &
!!                VAR_CWX(2),' to ', f_cw_max(ngcwx),' based on K_abs*area.'
!          VAR_CWX(2) = f_cw_max(ngcwx)
!     ENDIF

!    if (var_cwx2_tmp.NE. 0.0) then
!      if (tqm_cwx(ngcwx)-var_cwx(2) .eq.0.0) then
!         write(*,'(a,I3,a,I3,a,F16.8,a,F16.8,a)') 'ME:',me,'. Well # ',ngcwx,' target flux is ',var_cwx(2),&
!                        ' [kg/s]. Total flux is ',tqm_cwx(ngcwx),' [kg/s]. &
!                        Relative difference is: 0.0.'
!      else
!         write(*,'(a,I3,a,I3,a,F16.8,a,F16.8,a,F16.8)') 'ME:',me,'. Well # ',ngcwx,' target flux is ',var_cwx(2),&
!                        ' [kg/s]. Total flux is ',tqm_cwx(ngcwx),' [kg/s]. &
!                        Relative difference is: ',(tqm_cwx(ngcwx)-var_cwx(2))/abs(var_cwx(2))
!      endif
!    endif

!
!---    Loop over field nodes that contain coupled-well nodes  ---
!
     DO 430 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
      M1 = (NWF-ID_CW(5,NCW))*ISVC + 1
      DO 420 M2 = 1,ISVC
        M3 = M1+M2
        if(iwt_cw(iwf_cw(1))>0) then
          RS_CW(M3,NCW) = VAR_CWX(2)
        endif
!
!---        Loop over coupled-well nodes  ---
!
        DO 410 NWN = ID_CW(3,NCW),ID_CW(4,NCW)
!
!---          If coupled-well node is within the current field
!             node, use incremented fluxes  ---
!
          IF( IWF_CW(NWF).EQ.IWN_CW(NWN) ) THEN
            RS_CW(M3,NCW) = RS_CW(M3,NCW) - FXW_CW(M2+1,NWN)         ! change to water
!
!---          If coupled-well node is outside the current field
!             node, use un-incremented fluxes  ---
!
          ELSE
            RS_CW(M3,NCW) = RS_CW(M3,NCW) - FXW_CW(1,NWN)     
          ENDIF
    410       CONTINUE
    420     CONTINUE
    430   CONTINUE
    enddo

!    write_well_flux = 1
!    IF (write_well_flux==1 .and. ME==0) then
!      write(*,'(a)')  '============================================================================'
!       write(*,'(a)') 'Reporting total fluxes at each well:'
!        write(IWF,'(1PE22.15,1X,<n_cw>(1PE22.15,1X))') TMZ,(tqm_cwx(iw),iw=1,n_cw)
!        do ncw = 1, n_cw
!          if (tqm_cwx(ncw) .NE. 0.0) then
!             write(*,'(a,I3,a,F16.8,a)') 'Global Well # ',ncw,', total flux is ',tqm_cwx(ncw),' [kg/s].' 
!          endif  
!        enddo
!       write(*,'(a)')  '============================================================================'
!    ENDIF
    if (write_well_flux(1) == 1) then
      q_flux_cw(1:n_cw)=tqm_cwx(1:n_cw)
    endif
    id_cw(8,:) = 0
    call ga_igop(1,idp_cwx,n_cw,'max')
    do ncw=1,n_l_cw
      ngcwx = id_cw(7,ncw)
      id_cw(8,ncw) = idp_cwx(ngcwx)
    enddo
    DEALLOCATE(VAR_CWX) 
!
!---  End of FLUX_COUP_WELL group  ---
!
  RETURN
  END
  

!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE PRES_COUP_WELL(NCW)
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
!
!     STOMP-CO2e
!
!     Pressure within coupled-well nodes  
!
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 19 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE PORMED
  USE JACOB
  USE HYST
  USE GRID
  USE FDVP
  USE COUP_WELL
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
  REAL*8 XPX(2),YPX(2),ZPX(2)
  REAL*8,DIMENSION(:),ALLOCATABLE::VAR_CWX
  INTEGER, SAVE :: IALLOC
  DATA IALLOC /0/
  INTEGER:: i_end 
  
  
  
  
  
!
!----------------------Executable Lines--------------------------------!
!
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 

    me=ga_nodeid()
!
!---  Loop over coupled wells ---
!
    ALLOCATE(VAR_CWX(5+NSOLU))
    ngcwx = id_cw(7,ncw)
    TMZ = TM
    IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
    IF( ICC_CW(NGCWX).EQ.1 ) TMZ = MOD( TM,VAR_CW(1,IM_CW(NGCWX),NGCWX) )
!
!---    Coupled well is inactive set well pressure to be in 
!       equilibrium with formation  ---
!
    IF( TMZ.LE.VAR_CW(1,1,NGCWX) .OR.  &
      G_QM_CW(2,NGCWX).GE.TML_CW(NGCWX) ) THEN
      GOTO 500
    ENDIF
    IF( IM_CW(NGCWX).EQ.1 ) THEN
      DO 80 N = 2,5+NSOLU
        VAR_CWX(N) = VAR_CW(N,1,NGCWX)
     80     CONTINUE
    ELSE
      DO 100 M = 2,IM_CW(NGCWX)
        IF( TMZ.LE.VAR_CW(1,M,NGCWX) ) THEN
          TD_CW = VAR_CW(1,M,NGCWX)-VAR_CW(1,M-1,NGCWX)
          DT_CW = MIN( VAR_CW(1,M,NGCWX)-TMZ,DT )
          TF_CW = (TMZ-VAR_CW(1,M-1,NGCWX))/TD_CW
          DO 90 N = 2,5+NSOLU
            VAR_CWX(N) = VAR_CW(N,M-1,NGCWX) +  &
              TF_CW*(VAR_CW(N,M,NGCWX)-VAR_CW(N,M-1,NGCWX))
     90         CONTINUE
        ENDIF
        exit
    100     CONTINUE
    ENDIF
!
!---    Loop over increment indices ---
!
!
!---      Injection well  ---
!
500 continue

      do 300 mw = 1,2
         P_CWX = P_CW_TABLE(MW,1,ngcwx)
!
!---        Nonisothermal simulations  ---
!
         IF( ISLC(30).EQ.0 ) THEN
          T_CWX = VAR_CWX(5)
         ELSE
          T_CWX = t_acwx(1,ngcwx)
         ENDIF
         XLS_CWX = 0.D+0
         call WATSP( T_CWX,PSW_CWX)  ! satruration pressure - Bryan
          
          ! Update water density - Bryan
          PX_CWX = P_CWX + PATM
          CALL WATLQD( T_CWX,PX_CWX,RHOG_CWX)

!
!---        Store top of coupled-well location in previous
!           coupled-well node location  ---
!
         XPX(1) = XTP_CW(1,ID_CW(1,NGCWX))
         YPX(1) = YTP_CW(1,ID_CW(1,NGCWX))
         ZPX(1) = ZTP_CW(1,ID_CW(1,NGCWX))
!
!---      Loop over the nodes in the coupled well ---
!
       iwnx = 1 
       DO 200 NWN = ID_CW(1,NGCWX),ID_CW(2,NGCWX)
!
!---        Coupled-well node centroids and projections ---
!
        inwn = nwn-id_cw(1,ngcwx)+1
        IF( ISLC(30).EQ.0 ) THEN
          T_CWX = VAR_CWX(5)
        ELSE
          T_CWX = T_acwx(inwn,ngcwx)
        ENDIF
        XLX = ABS(XTP_CW(2,NWN)-XTP_CW(1,NWN))
        YLX = ABS(YTP_CW(2,NWN)-YTP_CW(1,NWN))
        ZLX = ABS(ZTP_CW(2,NWN)-ZTP_CW(1,NWN))
        XPX(2) = 5.D-1*(XTP_CW(2,NWN)+XTP_CW(1,NWN))
        YPX(2) = 5.D-1*(YTP_CW(2,NWN)+YTP_CW(1,NWN))
        ZPX(2) = 5.D-1*(ZTP_CW(2,NWN)+ZTP_CW(1,NWN))
!
!---        Well pressure using previous coupled-well node density ---
!
        P_CWX = P_CWX - ((XPX(2)-XPX(1))*GRVPX(1) +  &
          (YPX(2)-YPX(1))*GRVPY(1) +  &
          (ZPX(2)-ZPX(1))*GRVPZ(1))*RHOG_CWX

        iwnx = iwnx+1
        p_cw_table(mw,iwnx,ngcwx) = p_cwx

! Update water density - Bryan
          PX_CWX = P_CWX + PATM
          CALL WATLQD( T_CWX,PX_CWX,RHOG_CWX)
!
!---        Store current coupled-well node location in previous
!           coupled-well node location  ---
!
        XPX(1) = XPX(2)
        YPX(1) = YPX(2)
        ZPX(1) = ZPX(2)
      200     CONTINUE
     300 continue     
  
     DEALLOCATE(VAR_CWX)
!
!---  End of PRES_COUP_WELL group  ---
!
  RETURN
  END

  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE INCRM_COUP_WELL
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
!
!     STOMP-CO2e
!
!     Define well nodes, determine trajectory points, and 
!     check for well trajectories within node surface planes
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 14 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE COUP_WELL
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
!----------------------Executable Lines--------------------------------!
!
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 


  me=ga_nodeid()
!---  Loop over coupled wells ---
!
  DO 100 NCW = 1,N_L_CW
    ngcwx = id_cw(7,ncw)
    n = iwn_cw(id_cw(3,ncw))
    DNR_CW(NCW) = 1.D-1 ! 1.D-1
    if(iwt_cw(n) > 0) then
      if (GIT_CW(NGCWX).GT. 0) then
        P_CW(3,NCW) = P_CW(2,NCW) + DNR_CW(NCW)
      elseif (GIT_CW(NGCWX).LT. 0) then
        P_CW(3,NCW) = P_CW(2,NCW) - DNR_CW(NCW)
      endif
    endif
    100 CONTINUE
!
!---  End of INCRM_COUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE JCB_COUP_WELL(petsc_A)
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
!
!     STOMP-CO2e
!
!     Modify Jacobian matrix for the coupled-well equations
!     and load Jacobian matrix for the coupled-well equations
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 20 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE JACOB
  USE GRID
  USE FDVP
  USE COUP_WELL
  USE PETSCAPP
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
!--- Petsc includes
!
!#include "include/finclude/petsc.h"
#include "include/finclude/petscvec.h"
#include "include/finclude/petscvec.h90"
#include "include/finclude/petscmat.h"
#include "include/finclude/petscmat.h90"
#include "include/finclude/petscpc.h"
#include "include/finclude/petscksp.h"
#include "include/finclude/petscsys.h"
#include "include/finclude/petscviewer.h"
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  PetscInt, dimension(:), allocatable :: ic,ir
  PetscInt :: nr,nc 
  PetscScalar, dimension(:), allocatable :: values_
  PetscErrorCode :: ierr
  double precision, dimension(:), allocatable :: rscwx
  integer, dimension(:,:), allocatable :: ir_tmp,ic_tmp
  double precision, dimension(:,:), allocatable :: values_tmp
  
!
!----------------------Executable Lines--------------------------------!
!
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 

  me=ga_nodeid()
!
!---    Loop over coupled wells ---
!
    allocate(rscwx(n_cw))
    rscwx = 0.d0
    nr = 1 
    nc = isvc+1
    msize = nr*nc
    DO 500 NCW = 1,N_L_CW
!
!---      Loop over coupled-well well nodes ---
!
      ngcwx = id_cw(7,ncw)
      npx = 0
      nfirst = iwf_cw(id_cw(5,ncw))
      allocate(ir(msize))
      allocate(ic(msize))
      allocate(values_(msize))
      allocate(ir_tmp(nr,nc))
      allocate(ic_tmp(nr,nc))
      allocate(values_tmp(nr,nc))
      DO 200 NWN = ID_CW(3,NCW),ID_CW(4,NCW)
      ivx = 0
      ir = 0
      ic = 0
      values_ = 0.d0
        N = IWN_CW(NWN)
        nx = id_g2l(n)
!
!---        Water mass balance equation at field node ---
!
!---        Change in water mass flow into field node with respect
!           to change in field node primary variables  ---
!
      ivx = 0
      ir = 0
      ic = 0
      values_ = 0.d0
        DO 1109 M = 1,ISVC
!
!---          Field node  ---
!
          DNRX = DNR(M,N)
          if(n.ne.npx) then
            ivx = ivx+1
            ir(ivx) = imxp(n)-luk+ieqw-1
            ic(ivx) = imxp(n)-luk+m-1 
            values_(ivx) = values_(ivx)+(fxw_cw(1,nwn)-fxw_cw(m+1,nwn))/dnrx
          endif
    1109       CONTINUE
!
!---        Change in water mass flow into field node with respect
!           to change in coupled-well pressure  ---
!
        MX = ISVC+2
        if(n.ne.npx) then
         ivx = ivx+1
         values_(ivx) = values_(ivx)+(fxw_cw(1,nwn)-fxw_cw(mx,nwn))/dnr_cw(ncw)
         ir(ivx) = imxp(n)-luk+ieqw-1
         ic(ivx) = mmap_cw(ngcwx)
        endif
          nr = 1 
          nc = isvc+1
         call MatSetValues( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
!
!---        Water mass flow into field node, kg/s  ---
!
         residual(ieqw,n) = residual(ieqw,n)-fxw_cw(1,nwn)
!
    200     CONTINUE
!*******************************************************************************

         deallocate(ir)
         deallocate(ic)
         deallocate(values_)
         deallocate(values_tmp)
         deallocate(ir_tmp)
         deallocate(ic_tmp)
!
!---      Coupled-well mass balance  ---
!
!
!---      Pressure controlled coupled well  ---
!
!      IF( ID_CW(8,NCW).EQ.1 ) BLU(MP) = 0.D+0
!
!---      Change in coupled-well mass balance with respect to
!         change in coupled-well pressure  ---
!
      NWFX = ID_CW(6,NCW)-ID_CW(5,NCW)+1
      MX = (NWFX*ISVC)+2
      nr = 1
      nc = 1 + nwfx*isvc
      allocate(ir(nc))
      ir = 0
      allocate(ic(nc))
      ic = 0
      allocate(values_(nc))
      values_ = 0.d0
      ivx = 1
      values_(ivx) = (rs_cw(mx,ncw)-rs_cw(1,ncw))/dnr_cw(ncw)
      ir(ivx) = mmap_cw(ngcwx)
      ic(ivx) = mmap_cw(ngcwx)
!
!---      Pressure controlled coupled well  ---
!
      IF( ID_CW(8,NCW).EQ.1 ) then
        nwellx_ = iwn_cw(id_cw(3,ncw))
        if(iwt_cw(nwellx_) > 0) then
          values_(ivx) = 1.D+0
        else
          values_(ivx) = 0.d0
        endif
      endif
!
!---      Loop over field nodes with coupled-well nodes ---
!
      DO 400 NWF = ID_CW(5,NCW),ID_CW(6,NCW)
        N = IWF_CW(NWF)
        nx = id_g2l(n)
        MX = (NWF-ID_CW(5,NCW))*ISVC + 1
!
!---        Change in coupled-well mass balance with respect to
!           change in field node primary variables  ---
!
        DO 300 M = 1,ISVC
!
!---          Field node  ---
!
          DNRX = DNR(M,N)
          ivx = ivx+1
          ndifx = (imxp(n)-lstart)-nx
          ir(ivx) = mmap_cw(ngcwx)
          ic(ivx) = imxp(n)-luk+m-1 
          values_(ivx) = (rs_cw(mx+m,ncw)-rs_cw(1,ncw))/dnrx
!
!---          Pressure controlled coupled well  ---
!
          IF( ID_CW(8,NCW).EQ.1 ) values_(ivx) = 0.D+0
    300       CONTINUE
    400     CONTINUE

           call MatSetValues( petsc_A,nr,ir,nc,ic,values_,ADD_VALUES,ierr )
           deallocate(ir)
           deallocate(ic)
           deallocate(values_)
           rscwx(ngcwx) = rs_cw(1,ncw)
    500   CONTINUE
          call ga_dgop(1,rscwx,n_cw,'+')
          rs_cw = 0.d0
          do ncw=1,n_l_cw
            ngcwx = id_cw(7,ncw) 
            rs_cw(1,ncw) = -rscwx(ngcwx)
            if(id_cw(8,ncw).eq.1) rs_cw(1,ncw) = 0.d0
          enddo
          deallocate(rscwx)
    
!
!---  End of JCB_COUP_WELL group  ---
!
  RETURN
  END

!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE SOLUT_COUP_WELL(NSL,petsc_A)
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
!
!     STOMP-W
!
!     Solute transport in coupled well 
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 20 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE JACOB
  USE GRID
  USE FDVP
  USE COUP_WELL
  USE PETSCAPP
  USE TRNSPT
!
  
!----------------------Implicit Double Precision-----------------------!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
  REAL*8 SORTX_CW
  REAL*8,DIMENSION(:),ALLOCATABLE::VAR_CWX
  REAL*8,DIMENSION(:),ALLOCATABLE::SOLU_CW
#include "petscwrapper.h"

  PetscInt :: ic(2),ir(2),nr,nc
  PetscScalar :: values_(4)
  PetscErrorCode :: ierr
  Mat :: jac
!


!
!----------------------Executable Lines--------------------------------!
!
!  ISUB_LOG = ISUB_LOG+1
!  SUB_LOG(ISUB_LOG) = '/SOLUT_COUP_WELL'
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 

    me=ga_nodeid()
    ALLOCATE(VAR_CWX(5+NSOLU))
    ALLOCATE(SOLU_CW(N_CW_SOLU))
    VAR_CWX(:) = 0.0
    SOLU_CW(:) = 0.0
    !---    Loop over coupled wells ---
!
    if (N_L_CW .GT. 0) then
     

    residual(:,:) = 0.e+0
    DO 500 NCW = 1,N_L_CW
          
     ngcwx = id_cw(7,ncw)

     IF (GIT_CW(NGCWX) .EQ. 24 .OR. GIT_CW(NGCWX) .EQ. -22) THEN   
      TMZ = TM
      IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
      IF( ICC_CW(NGCWX).EQ.1 ) TMZ = MOD( TM,VAR_CW(1,IM_CW(NGCWX),NGCWX) )

      IF( IM_CW(NGCWX).EQ.1 ) THEN
        DO 80 N = 2,5+NSOLU
          VAR_CWX(N) = VAR_CW(N,1,NGCWX)
       80     CONTINUE
      ELSE
       DO 100 M = 2,IM_CW(NGCWX)
        IF( TMZ.LE.VAR_CW(1,M,NGCWX) ) THEN
          TD_CW = VAR_CW(1,M,NGCWX)-VAR_CW(1,M-1,NGCWX)
          DT_CW = MIN( VAR_CW(1,M,NGCWX)-TMZ,DT )
          TF_CW = (TMZ-VAR_CW(1,M-1,NGCWX))/TD_CW
          DO 90 N = 2,5+NSOLU
            VAR_CWX(N) = VAR_CW(N,M-1,NGCWX) +  &
              TF_CW*(VAR_CW(N,M,NGCWX)-VAR_CW(N,M-1,NGCWX))
          90         CONTINUE
        ENDIF
        exit
       100     CONTINUE
      ENDIF

      
!
!---      Loop over coupled-well well nodes ---
!
        
      npx = 0
      DO 200 NWN = ID_CW(3,NCW),ID_CW(4,NCW)
        N = IWN_CW(NWN)

!---        Water mass flow into field node, kg/s  ---
!
        IF (GIT_CW(NGCWX).GT.0) THEN
                residual(1,n) = residual(1,n)+ fxw_cw(1,nwn)*VAR_CWX(5+NSL)/RHOL(2,N)
                solu_cw(ngcwx) = solu_cw(ngcwx) + fxw_cw(1,nwn)*VAR_CWX(5+NSL)/RHOL(2,N)
        ELSEIF (GIT_CW(NGCWX).LT.0) THEN
                SORTX_CW =-fxw_cw(1,nwn)*YL(NSL,N)/(SL(2,N)*PORD(2,N)*RHOL(2,N))
                values_= 0.d0
                ir(1) = gloc_map(n)-1
                ic(1) = ir(1)
                nr = 1
                nc = 1
                buffer = sortx_cw
                values_(1) = sortx_cw
                solu_cw(ngcwx) = solu_cw(ngcwx) - sortx_cw*C(NSL,N)
                call MatSetValuesLocal( petsc_A,nr,ir,nc,ic,buffer,ADD_VALUES,ierr)
        ENDIF
        200     CONTINUE
     ENDIF
   500 CONTINUE 
   endif
   call ga_dgop(1,solu_cw,n_cw_solu,'+')  
   c_flux_cw(nsl,:)=solu_cw(:)

  DEALLOCATE(VAR_CWX)
  DEALLOCATE(SOLU_CW)
!
!---  End of SOLUT_COUP_WELL group  ---
!
  RETURN
  END
 

!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE SOLUIT_COUP_WELL(NSL)
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
!
!     STOMP-W
!
!     Integrated Solute transport 
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 20 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE TRNSPT
  USE JACOB
  USE GRID
  USE FDVP
  USE COUP_WELL
  USE TRNSPT
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
  REAL*8,DIMENSION(:),ALLOCATABLE:: VAR_CWX
  REAL*8,DIMENSION(:),ALLOCATABLE:: SOLUIT_CW
!
!----------------------Executable Lines--------------------------------!
!
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 

    me=ga_nodeid()
    ALLOCATE(VAR_CWX(5+NSOLU))
    ALLOCATE(SOLUIT_CW(N_CW_SOLU))
    VAR_CWX(:) = 0.0
    SOLUIT_CW(:) = 0.0
    !---    Loop over coupled wells ---
!
    if(N_L_CW .GT. 0) then

    DO 500 NCW = 1,N_L_CW
     ngcwx = id_cw(7,ncw)
     IF (GIT_CW(NGCWX) .EQ. 24 .OR. GIT_CW(NGCWX) .EQ. -22) THEN   
      TMZ = TM
      IF( NSTEP-NRST.EQ.0 ) TMZ = TMZ*(1.D+0+EPSL)+EPSL
      IF( ICC_CW(NGCWX).EQ.1 ) TMZ = MOD( TM,VAR_CW(1,IM_CW(NGCWX),NGCWX) )

      IF( IM_CW(NGCWX).EQ.1 ) THEN
        DO 80 N = 2,5+NSOLU
          VAR_CWX(N) = VAR_CW(N,1,NGCWX)
       80     CONTINUE
      ELSE
       DO 100 M = 2,IM_CW(NGCWX)
        IF( TMZ.LE.VAR_CW(1,M,NGCWX) ) THEN
          TD_CW = VAR_CW(1,M,NGCWX)-VAR_CW(1,M-1,NGCWX)
          DT_CW = MIN( VAR_CW(1,M,NGCWX)-TMZ,DT )
          TF_CW = (TMZ-VAR_CW(1,M-1,NGCWX))/TD_CW
          DO 90 N = 2,5+NSOLU
            VAR_CWX(N) = VAR_CW(N,M-1,NGCWX) +  &
              TF_CW*(VAR_CW(N,M,NGCWX)-VAR_CW(N,M-1,NGCWX))
          90         CONTINUE
        ENDIF
        exit
       100     CONTINUE
      ENDIF

      
!
!---      Loop over coupled-well well nodes ---
!
      npx = 0
      DO 200 NWN = ID_CW(3,NCW),ID_CW(4,NCW)
        N = IWN_CW(NWN)

!---        Water mass flow into field node, kg/s  ---
!
         IF (GIT_CW(NGCWX).GT.0) THEN
                SRCIC(NSL,N) = SRCIC(NSL,N) + fxw_cw(1,nwn)*DT*VAR_CWX(5+NSL)/ RHOL(2,N)
                SOLUIT_CW(NGCWX) = SOLUIT_CW(NGCWX)+SRCIC(NSL,N)
         ELSEIF (GIT_CW(NGCWX).LT.0) THEN
                SRCIC(NSL,N) = SRCIC(NSL,N) - C(NSL,N)*fxw_cw(1,nwn)*YL(NSL,N)*DT/(SL(2,N)*PORD(2,N)*RHOL(2,N)) 
                SOLUIT_CW(NGCWX) = SOLUIT_CW(NGCWX)-SRCIC(NSL,N)
         ENDIF
       200     CONTINUE
         
     ENDIF
   500 CONTINUE  
   endif
   call ga_dgop(1,soluit_cw,n_cw_solu,'+')
   c_total_cw(nsl,:)=soluit_cw(:)

   DEALLOCATE(VAR_CWX)
   DEALLOCATE(SOLUIT_CW)
!
!---  End of SOLUIT_COUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!

  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE LDO_COUP_WELL
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
!
!     STOMP-CO2e
!
!     Load old-time-step values for coupled-well arrays.
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 21 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE COUP_WELL
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
#include "mafdecls.fh"
#include "global.fh"
!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Executable Lines--------------------------------!
!
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 

  me=ga_nodeid()
!
!---  Loop over coupled wells ---
!
  p_cw_g = 0.d0
  DO 100 NCW = 1,N_L_CW
    n = iwn_cw(id_cw(3,ncw))
      P_CW(1,NCW) = P_CW(2,NCW)
    if(iwt_cw(n) > 0) then
      ngcwx = id_cw(7,ncw)
      p_cw_g(ngcwx) = p_cw(2,ncw)
    endif
    100 CONTINUE
    call ga_dgop(1,p_cw_g,n_cw,'+')
!
!---  End of LDO_COUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE RDCOUP_WELL
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
!
!     STOMP-CO2e
!
!     Reads the coupled well card.
!     
!     FF_CW(1,LN_CW) - x-direction well fraction factor for well
!     FF_CW(2,LN_CW) - y-direction well fraction factor for well
!     FF_CW(3,LN_CW) - z-direction well fraction factor for well
!     ICC_CW(LN_CW) - cyclic well time index for well
!     IM_CW(LN_CW) - number of time points for well
!     ID_CW(1,LN_CW) - starting well interval index for well
!     ID_CW(2,LN_CW) - ending well interval index for well
!     ID_CW(3,LN_CW) - starting well node index for well
!     ID_CW(4,LN_CW) - ending well node index for well
!     ID_CW(5,LN_CW) - principal well node index for well
!     IT_CW(LN_CW) - type index for well
!     JM_CW(LN_CW) - location of the well equation in the Jacobian
!     N_CW - number of coupled wells
!     SK_CW(LWI_CW) - skin factor for well interval
!     TML_CW(LN_CW) - total mass injected/withdrawn limit for well, kg
!     VAR_CW(1,LWT_CW,LN_CW) - well time for time point for well, s
!     VAR_CW(2,LWT_CW,LN_CW) - mass rate for time point for well, kg/s
!     VAR_CW(3,LWT_CW,LN_CW) - press. limit for time point for well, Pa
!     VAR_CW(4,LWT_CW,LN_CW) - CO2 water conc. for time point for well
!     VAR_CW(4,LWT_CW,LN_CW) - aqu. CO2 conc. for time point for well
!     VAR_CW(5,LWT_CW,LN_CW) - temperature for time point for well
!     VAR_CW(6,LWT_CW,LN_CW) - 1st aqu. solut conc. for time point for well
!     VAR_CW(7,LWT_CW,LN_CW) - 1st aqu. solut conc. for time point for well
!     ...
!     VAR_CW(5+nsolu,LWT_CW,LN_CW) - nth aqu. solut conc. for time point for well
!     WBR_CW(LWI_CW) - well bore radius for well interval, m
!     XTP_CW(2,LWI_CW) - x-transition points for well interval, m
!     YTP_CW(2,LWI_CW) - y-transition points for well interval, m
!     ZTP_CW(2,LWI_CW) - z-transition points for well interval, m
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 22 March 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE TRNSPT
  USE SOLTN
  USE GRID
  USE FILES
  USE COUP_WELL
  USE CONST
  use jacob
  use grid_mod
  USE BUFFEREDREAD
!  USE GRID_MOD
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
interface
  subroutine get_node_mask(t_mask)
  use grid_mod
  implicit none
  integer, pointer :: t_mask(:) ! out
  end subroutine get_node_mask
end interface

!
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  CHARACTER*64 ADUM,BDUM,UNTS
  CHARACTER*512 CHDUM
  CHARACTER*40 FORM_W1
  SAVE FORM_W1
  DATA FORM_W1 /'( 3X,A,3X,A,3X,A)'/
  INTEGER NCH
  integer c_ijk,c_ijko,n_entry
  integer g_buf2
  integer lo(4),hi(4),ldxx(4),dims(4)
  integer, dimension(:,:), allocatable :: idx_buf2
  integer, dimension(:), allocatable :: n_wfx
!  integer, dimension(:,:), allocatable :: id_iwx
  integer, dimension(:,:), allocatable :: iwn_cwx
  integer, dimension(:,:), allocatable :: iwf_cwx
  integer, dimension(:,:), allocatable :: inv_cwx
  integer, dimension(:), allocatable :: invx
  integer, dimension(:,:), allocatable :: is_cwx

  double precision, dimension(:), allocatable :: val_buft
  double precision, dimension(:,:), allocatable :: temp_f
  double precision, dimension(:,:,:,:), allocatable :: ibuf4
  double precision, dimension(:,:,:), allocatable :: xp_cw_g
  double precision, dimension(:,:,:), allocatable :: yp_cw_g
  double precision, dimension(:,:,:), allocatable :: zp_cw_g
  double precision, dimension(:), allocatable :: xp_cwx
  double precision, dimension(:,:), allocatable :: ff_cwx
  double precision, dimension(:),allocatable :: xtmp,ytmp,ztmp
  integer :: bufx_(3),N_SL
  integer, dimension(:), allocatable :: ni_cw
  logical status
  real*8 :: z_tmp
!
!----------------------Executable Lines--------------------------------!
!
  me = ga_nodeid()

!top of the well
  call add_node_ifield('iwt_cw', idx)
  iwt_cw => i_nd_fld(idx)%p
  iwt_cw = 0
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 
!
!---  Write card information to ouput file  ---
!
  CARD = 'Coupled Well Card'
  ICD = INDEX( CARD,'  ' )-1
  if(me.eq.0)WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read number of wells  ---
!
  ISTART = 1
  T_OK = BUFFEREDREAD_GETLINE(CHDUM)
  CALL LCASE( CHDUM )
  VARB = 'Number of Coupled Wells'
  CALL RDINT(ISTART,ICOMMA,CHDUM,N_CW)  ! read number of coupled wells
  n_cw_solu = n_cw ! n_cw will be set to 0 for solute matrix, use n_cw_solu to keep the real number
  allocate(it_cw(n_cw))
  it_cw = 0
  allocate(git_cw(n_cw))
  git_cw = 0
  allocate(ff_cwx(3,n_cw))
  ff_cwx = 0.d0
  allocate(ff_cw(3,n_cw))
  ff_cw = 0.d0
  allocate(tml_cw(n_cw))
  tml_cw = 0.d0
  allocate(id_cw(8,n_cw))
  id_cw = 0
  allocate(im_cw(n_cw))
  im_cw = 0
  allocate(icc_cw(n_cw))
  icc_cw = 0
  allocate(var_cw(5+nsolu,lwt_cw,1:n_cw)) ! lwt_cw:max number of time points one well has, obtained in step.F90 - Bryan
  var_cw = 0.d0

  n_entry = 27   ! why 27 ??? -Bryan 
                 ! maximum 3 intervals, 9 parameters (1.global interval index; 2.
                 ! well number; 3. xyz location of two end points (6 data); 4. if
                 ! screened. -Bryan  

  allocate(idx_buf2(4,nit_cw*n_entry))
  idx_buf2 = 0
  allocate(temp_f(nit_cw,n_entry))
  temp_f = 0.d0
  allocate(val_buft(nit_cw*n_entry))
  val_buft = 0.d0
  allocate(wbr_cw(nit_cw))
  wbr_cw = 0.d0
  allocate(sk_cw(nit_cw))
  sk_cw = 0.d0
  allocate(xtp_cw(2,nit_cw))
  xtp_cw = 0.d0
  allocate(ytp_cw(2,nit_cw))
  ytp_cw = 0.d0
  allocate(ztp_cw(2,nit_cw))
  ztp_cw = 0.d0
  allocate(xp_cw(2,nit_cw))
  xp_cw = 0.d0
  allocate(yp_cw(2,nit_cw))
  yp_cw = 0.d0
  allocate(zp_cw(2,nit_cw))
  zp_cw = 0.d0
  allocate(xp_cw_g(2,nit_cw,n_cw))
  xp_cw = 0.d0
  allocate(yp_cw_g(2,nit_cw,n_cw))
  yp_cw = 0.d0
  allocate(zp_cw_g(2,nit_cw,n_cw))
  zp_cw = 0.d0
  allocate(is_cw(nit_cw,n_cw))
  is_cw = 0
  allocate(iwn_cwx(nit_cw,n_cw))
  iwn_cwx = 0
  allocate(iwn_cw(nit_cw))
  iwn_cw = 0
  allocate(iwi_cw(nit_cw))
  iwi_cw = 0
  allocate(iwp_cw(nit_cw))
  iwp_cw = 0
  allocate(iwf_cw(nit_cw))
  iwf_cw = 0
  allocate(inv_cwx(nit_cw,n_cw))
  inv_cwx = 0
  allocate(is_cwx(nit_cw,n_cw))
  is_cwx = 0
  allocate(p_cw_table(2,lwi_cw+1,n_cw))  ! lwi_cw: max number of interval that one well has, obtained in step.F90 - Bryan
  p_cw_table = 0.d0
  if(.not. allocated(p_cw_g))  allocate(p_cw_g(n_cw))
  p_cw_g = 0.d0
!
! Prepare for flux calculation
!
  allocate(qm_cw(6,n_cw))
  allocate(g_qm_cw(6,n_cw))
  g_qm_cw = 0.d0
  qm_cw = 0.d0
  allocate(ni_cw(n_cw))

! prepare for aqueous and solute flux output -BH
  write_well_flux(1:2) = 0
  CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
  IF( INDEX(ADUM(1:),'write').NE.0 .AND. &
      INDEX(ADUM(1:),'aqueous').NE.0 .AND. &
      INDEX(ADUM(1:),'flux').NE.0 ) THEN 
      write_well_flux(1) = 1
      allocate(q_flux_cw(n_cw))
      allocate(q_total_cw(n_cw))
  ENDIF
  CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
  IF( INDEX(ADUM(1:),'write').NE.0 .AND. &
      INDEX(ADUM(1:),'solute').NE.0 .AND. &
      INDEX(ADUM(1:),'flux').NE.0 .AND. & 
      NSOLU>0) THEN
      write_well_flux(2) = 1
      allocate(c_flux_cw(NSOLU,n_cw))
      allocate(c_total_cw(NSOLU,n_cw))
  ENDIF
  if (write_well_flux(1)==1 .and. me.eq.0) then
    IWAF = 1001
    OPEN(UNIT=IWAF, FILE='well_aqueous_flux.dat', STATUS='unknown')
    write(IWAF,'(A)') ' ---  Coupled well aqueous flux and accumulated amount of water  ---'
    write(IWAF,FORM_W1) 'Time [s]','Aqueous flux [kg/s]','Cumulative water [kg]'
  endif
  if (write_well_flux(2)==1 .and. me.eq.0) then
    IWSF = 1002
    OPEN(UNIT=IWSF, FILE='well_solute_flux.dat', STATUS='unknown')
    write(IWSF,'(A)') ' ---  Coupled well solute flux and accumulated amount of solute  ---'
    write(IWSF,FORM_W1) 'Time [s]','Solute flux [1/s]','Cumulative solute [1]'
  endif

!
!---  Loop over number of coupled wells  ---
!
  NIT_CW = 0
  ncount = 0
  DO 300 NCW = 1,N_CW
!
!---    Read well type  ---
!
    ISTART = 1
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    VARB = 'Coupled Well Type'
    CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
    if(me.eq.0)WRITE(IWR,'(/,2A,$)') VARB(1:IVR),': '
    IF( INDEX(ADUM(1:),'aqueous').NE.0 .AND. &
      INDEX(ADUM(1:),'injection').NE.0 ) THEN
      if(me.eq.0)WRITE(IWR,'(2X,A)') 'Coupled Aqueous Injection Well'
        IT_CW(NCW) = 4
      VARB = 'Solute transport Option'
      CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
      IF( INDEX(BDUM(1:),'aqueous').NE.0 .AND. &
        INDEX(BDUM(1:),'solute').NE.0 .AND. &
        INDEX(BDUM(1:),'conc').NE.0 ) THEN
        if(me.eq.0)WRITE(IWR,'(2X,A)') 'Solute Aqueous Concentration'
        IT_CW(NCW) = IT_CW(NCW) + 20
      ELSE
        if(me.eq.0)WRITE(IWR,'(2X,A)') 'No Solute'
        IT_CW(NCW) = IT_CW(NCW) + 30
      ENDIF
    ELSEIF( (INDEX(ADUM(1:),'withdrawl').NE.0 .OR. &
      INDEX(ADUM(1:),'production').NE.0) .AND. &
      INDEX(ADUM(1:),'mass').NE.0  ) THEN
      if(me.eq.0)WRITE(IWR,'(2X,A)') 'Coupled Mass Flow Rate Withdrawl Well'
      IT_CW(NCW) = -2
      VARB = 'Solute transport Option'
      CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,BDUM)
      IF( INDEX(BDUM(1:),'aqueous').NE.0 .AND. &
        INDEX(BDUM(1:),'solute').NE.0 .AND. &
        INDEX(BDUM(1:),'conc').NE.0 ) THEN
        if(me.eq.0)WRITE(IWR,'(2X,A)') 'Solute Aqueous Concentration'
        IT_CW(NCW) = IT_CW(NCW) - 20
      ELSE
        if(me.eq.0)WRITE(IWR,'(2X,A)') 'No Solute'
        IT_CW(NCW) = IT_CW(NCW) - 30
      ENDIF
    ELSE
      INDX = 4
      CHMSG = 'Unrecognized Coupled Well Type: ' // ADUM(1:NCH)
      CALL WRMSGS( INDX )
    ENDIF


!
!---    Read x-direction well fraction factor  ---
!
    VARB = 'X-Well Fraction Factor'
    IDFLT = 1
    FF_CW(1,NCW) = 1.D+0
    CALL RDDPR(ISTART,ICOMMA,CHDUM,FF_CW(1,NCW))
    if(me.eq.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',FF_CW(1,NCW)
    IF( FF_CW(1,NCW).LT.EPSL ) THEN
      INDX = 16
      CHMSG = 'Zero X-Well Fraction Factor: Well Number'
      IMSG = NCW
      RMSG = FF_CW(1,NCW)
      CALL WRMSGS( INDX )
    ENDIF
!
!---    Read y-direction well fraction factor  ---
!
    VARB = 'Y-Well Fraction Factor'
    IDFLT = 1
    FF_CW(2,NCW) = 1.D+0
    CALL RDDPR(ISTART,ICOMMA,CHDUM,FF_CW(2,NCW))
    if(me.eq.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',FF_CW(2,NCW)
    IF( FF_CW(2,NCW).LT.EPSL ) THEN
      INDX = 16
      CHMSG = 'Zero Y-Well Fraction Factor: Well Number'
      IMSG = NCW
      RMSG = FF_CW(2,NCW)
      CALL WRMSGS( INDX )
    ENDIF
!
!---    Read z-direction well fraction factor  ---
!
    VARB = 'Z-Well Fraction Factor'
    IDFLT = 1
    FF_CW(3,NCW) = 1.D+0
    CALL RDDPR(ISTART,ICOMMA,CHDUM,FF_CW(3,NCW))
    if(me.eq.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',FF_CW(3,NCW)
    IF( FF_CW(3,NCW).LT.EPSL ) THEN
      INDX = 16
      CHMSG = 'Zero Z-Well Fraction Factor: Well Number'
      IMSG = NCW
      RMSG = FF_CW(3,NCW)
      CALL WRMSGS( INDX )
    ENDIF
!
!---    Check for total mass limit  ---
!
    TML_CW(NCW) = 1.D+20   ! total mass limit-Bryan
    CALL CHKDPR( ISTART,ICOMMA,CHDUM,INDX )
    IF( INDX.EQ.1 ) THEN
!
!---      Read total mass injected limit  ---
!
      IF( IT_CW(NCW).GT.0 ) THEN
        VARB = 'Total Mass Injected Limit'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,TML_CW(NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',TML_CW(NCW)
        INDX = 0
        IUNKG = 1
        CALL RDUNIT(UNTS,TML_CW(NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',TML_CW(NCW),', kg)'
!
!---      Read total mass withdrawn/produced limit  ---
!
      ELSEIF( IT_CW(NCW).LT.0 ) THEN    ! should be -2 - Bryan
          VARB = 'Total Mass Withdrawn/Produced Limit'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,TML_CW(NCW))
        TML_CW(NCW) =  TML_CW(NCW)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',TML_CW(NCW)
        INDX = 0
        IUNKG = 1
        CALL RDUNIT(UNTS,TML_CW(NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',TML_CW(NCW),', kg)'
      ENDIF
    ENDIF
!
!---    Read number of well intervals  ---
!
    ISTART = 1
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    VARB = 'Number of Well Intervals'
    CALL RDINT(ISTART,ICOMMA,CHDUM,NI_CWX)
    ni_cw(ncw) = ni_cwx
    NIT_CW = NIT_CW + NI_CWX  ! Total number of intervals of all wells? - Bryan
!
!---    Assign well transition pointers  --- ! Global ID for transition point - Bryan
!
    IF( NCW.EQ.1 ) THEN
      ID_CW(1,NCW) = 1
    ELSE
      ID_CW(1,NCW) = ID_CW(2,NCW-1) + 1
    ENDIF
    ID_CW(2,NCW) = ID_CW(1,NCW) + NI_CWX - 1
!
!---    Loop over number of well intervals  ---
!
    ie = 0
    je = 0
    ke = 0 
    DO 100 NICW = ID_CW(1,NCW),ID_CW(2,NCW)  ! loop over global ID of the intervals of each well - Bryan 
      nicw_lx = nicw-id_cw(1,ncw)+1     ! For each well, interval index always starts from 1 - Bryan
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
!
      !****************************************************
      !(solved) Not sure what is the following code about - Bryan 12/10/2019
      ! 1. Limit the number of intervals of each well in each field node - Bryan 12/11/2019
      ! 2. For each well, max THREE intervals can share the same field node
      !****************************************************
!
!---      Read first x-transition point  ---
!
      VARB = 'Interval First X-Transition Point'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,XTP_CW(1,NICW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',XTP_CW(1,NICW)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,XTP_CW(1,NICW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',XTP_CW(1,NICW),', m)'
!
!---      Read first y-transition point  ---
!
      VARB = 'Interval First Y-Transition Point'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,YTP_CW(1,NICW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',YTP_CW(1,NICW)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,YTP_CW(1,NICW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',YTP_CW(1,NICW),', m)'
!
!---      Cylindrical coordinates with azimuthal symmetry  ---
!
      IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. JFLD.EQ.1 ) THEN
        IF( ABS(XTP_CW(1,NICW))/EPSL.GT.EPSL ) THEN
          INDX = 9
          CHMSG = 'Non-Zero Interval First X-Transition Point ' //  &
            'for Radially Symmetric Domain: XTP_CW ='
          RLMSG = XTP_CW(1,NICW)
          CALL WRMSGS( INDX )
        ENDIF
        IF( ABS(YTP_CW(1,NICW))/EPSL.GT.EPSL ) THEN
          INDX = 9
          CHMSG = 'Non-Zero Interval First Y-Transition Point ' //  &
            'for Radially Symmetric Domain: YTP_CW ='
          RLMSG = YTP_CW(1,NICW)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---      Read first z-transition point  ---
!
      VARB = 'Interval First Z-Transition Point'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,ZTP_CW(1,NICW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',ZTP_CW(1,NICW)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,ZTP_CW(1,NICW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',ZTP_CW(1,NICW),', m)'
!
!---      Read second x-transition point  ---
!
      VARB = 'Interval Second X-Transition Point'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,XTP_CW(2,NICW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',XTP_CW(2,NICW)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,XTP_CW(2,NICW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',XTP_CW(2,NICW),', m)'
!
!---      Read second y-transition point  ---
!
      VARB = 'Interval Second Y-Transition Point'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,YTP_CW(2,NICW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',YTP_CW(2,NICW)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,YTP_CW(2,NICW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',YTP_CW(2,NICW),', m)'
!
!---      Cylindrical coordinates with azimuthal symmetry  ---
!
      IF( (ICS.EQ.2 .OR. ICS.EQ.6) .AND. JFLD.EQ.1 ) THEN
        IF( ABS(XTP_CW(2,NICW))/EPSL.GT.EPSL ) THEN
          INDX = 9
          CHMSG = 'Non-Zero Interval Second X-Transition Point ' //  &
            'for Radially Symmetric Domain: XTP_CW ='
          RLMSG = XTP_CW(2,NICW)
          CALL WRMSGS( INDX )
        ENDIF
        IF( ABS(YTP_CW(2,NICW))/EPSL.GT.EPSL ) THEN
          INDX = 9
          CHMSG = 'Non-Zero Interval Second Y-Transition Point ' //  &
            'for Radially Symmetric Domain: YTP_CW ='
          RLMSG = YTP_CW(2,NICW)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---      Read second z-transition point  ---
!
      VARB = 'Interval Second Z-Transition Point'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,ZTP_CW(2,NICW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',ZTP_CW(2,NICW)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,ZTP_CW(2,NICW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',ZTP_CW(2,NICW),', m)'


! Based on the x/y/z coordinates to determine the IJK index !BH
      if (ics .eq.3 .or. ics.eq.8) then
            allocate(xtmp(nxdim+1))
            allocate(ytmp(nydim+1))
            allocate(ztmp(nzdim+1))
            xtmp = XBF(:,1,1)
            ytmp = YBF(1,:,1)
            isx =  LOCATEINDEX( XTP_CW(1,NICW),nxdim+1,xtmp)
            js = LOCATEINDEX( YTP_CW(1,NICW),nydim+1,ytmp)
            z_tmp = (ZTP_CW(1,NICW)+ZTP_CW(2,NICW))/2
            DO k_s = 1,nzdim+1
                ztmp(k_s) = 0.25*(ZBF(isx,js,k_s)+ZBF(isx+1,js,k_s)+ZBF(isx,js+1,k_s)+ZBF(isx+1,js+1,k_s))
            ENDDO
            ks = LOCATEINDEX( z_tmp,nzdim+1,ztmp)
            
            if(me==0) then
               write(*,'(a,I,a,I,a,3I)') 'Well #',ncw,' interval #',nicw_lx,' i,j,k',isx,js,ks
               if (isx==0 .or. js ==0 .or. ks==0) then
                  WRITE(*,'(a,I,a,I)') 'WARNING: Fail to loacte the well segment: Well#',ncw,' interval #',nicw_lx
               endif
            endif
            deallocate(xtmp)
            deallocate(ytmp)
            deallocate(ztmp)
      else
            isx =  LOCATEINDEX( XTP_CW(1,NICW),nxdim+1,X)
            js = LOCATEINDEX( YTP_CW(1,NICW),nydim+1,Y)
            z_tmp = (ZTP_CW(1,NICW)+ZTP_CW(2,NICW))/2
            ks = LOCATEINDEX( z_tmp,nzdim+1,Z)
      endif

      n_entry = 27
      if( ie.eq.0 .and. je.eq.0 .and. ke.eq.0 ) then
        c_ijk = 7 
        c_ijko = 0    
      elseif( ie.eq.isx.and.je.eq.js.and.ke.eq.ks ) then
        c_ijk = c_ijk - 1
        c_ijko = c_ijko+9
        if(c_ijk <= 4.and.me.eq.0) then
          print *, 'Well interval limit in one node is reached...job killed'
          stop
        else
        endif
      else
        c_ijk = 7 
        c_ijko = 0
      endif
      ie = isx
      je = js
      ke = ks
      ncount = ncount + 1       ! should yield total number of intervals = NIT_CW -Bryan
      idx_buf2(1,ncount) = isx  ! i index of interval
      idx_buf2(2,ncount) = js   ! j index of interval
      idx_buf2(3,ncount) = ks   ! k index of interval


!
!
!---      Read well-bore radius  ---
!
      VARB = 'Interval Well-Bore Radius'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,WBR_CW(NICW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',WBR_CW(NICW)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,WBR_CW(NICW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',WBR_CW(NICW),', m)'
!
!---      Read well skin factor  ---
!
      VARB = 'Interval Skin Factor'
      IDFLT = 1
      SK_CW(NICW) = 0.D+0
      CALL RDDPR(ISTART,ICOMMA,CHDUM,SK_CW(NICW))
      if(me.eq.0)WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',SK_CW(NICW)
!
!---      Read well interval screen option  ---
!
      VARB = 'Interval Screen Option'
      CALL RDCHR(ISTART,ICOMMA,NCHB,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'screened').NE.0 ) THEN
        if(me.eq.0)WRITE(IWR,'(2X,A)') 'Screened Well Interval'
        IS_CW(NICW,NCW) = 1
      ELSE
        if(me.eq.0)WRITE(IWR,'(2X,A)') 'Unscreened Well Interval'
        IS_CW(NICW,NCW) = 0
      ENDIF
      temp_f(ncount,c_ijko+1) = nicw_lx   ! interval index for each well (global) - Bryan
      temp_f(ncount,c_ijko+2) = ncw       ! well index
      temp_f(ncount,c_ijko+3) = xtp_cw(1,nicw) ! Interval Starting X Point
      temp_f(ncount,c_ijko+4) = ytp_cw(1,nicw) ! Interval Starting Y Point
      temp_f(ncount,c_ijko+5) = ztp_cw(1,nicw) ! Interval Starting Z Point
      temp_f(ncount,c_ijko+6) = xtp_cw(2,nicw) ! Interval Ending X Point
      temp_f(ncount,c_ijko+7) = ytp_cw(2,nicw) ! Interval Ending Y Point
      temp_f(ncount,c_ijko+8) = ztp_cw(2,nicw) ! Interval Ending Z Point
      temp_f(ncount,c_ijko+9) = is_cw(nicw,ncw) ! if screened 
    100   CONTINUE
!
!---    Read number of well time points  ---
!
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    ISTART = 1
    VARB = 'Number of Well Time Points'
    CALL RDINT(ISTART,ICOMMA,CHDUM,IM_CW(NCW))
!
!---    Check number of well time points parameter  ---
!
    IF( IM_CW(NCW).GT.LWT_CW ) THEN
      INDX = 5
      CHMSG = 'Number of Well Time Points > LWT_CW'  ! LWT_CW: max number of time points that one well has, computed in step.F90 - Bryan
      CALL WRMSGS( INDX )  ! print error message - Bryan
    ENDIF
!
!---    Check for cyclic well times  ---
!
    IF( IM_CW(NCW).LE.-3 ) THEN
      ICC_CW(NCW) = 1
      IM_CW(NCW) = ABS(IM_CW(NCW))
      if(me.eq.0)WRITE(IWR,'(2X,A)') 'Cyclic Well Times'
    ELSEIF( IM_CW(NCW).GE.1 ) THEN
      ICC_CW(NCW) = 0
      if(me.eq.0)WRITE(IWR,'(2X,A)') 'Noncyclic Well Times'
    ELSE
      INDX = 4
      CHMSG = 'Number of Cyclic Well Times < 3'
      CALL WRMSGS( INDX )
    ENDIF
!
!---    Loop over the number of well times  ---
!
    DO 200 M = 1,IM_CW(NCW)
!
!---      Read new line  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)      
      CALL LCASE( CHDUM )
      ISTART = 1
!
!---      Read well time  ---
!
      VARB = 'Well Time'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(1,M,NCW))
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me.eq.0)WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
        UNTS(1:NCH),': ',VAR_CW(1,M,NCW)
      INDX = 0
      IUNS = 1
      CALL RDUNIT(UNTS,VAR_CW(1,M,NCW),INDX)
      if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR_CW(1,M,NCW),', s)'
!
!---      Read injection mass rate  ---
!
      IF( IT_CW(NCW).GT.0 ) THEN
        VARB = 'Injection Mass Rate'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(2,M,NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',VAR_CW(2,M,NCW)
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNTS,VAR_CW(2,M,NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR_CW(2,M,NCW),', kg/s)'
!
!---        Read maximum well-top pressure  ---
!
        VARB = 'Maximum Well-Top Pressure'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(3,M,NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',VAR_CW(3,M,NCW)
        INDX = 0
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNTS,VAR_CW(3,M,NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR_CW(3,M,NCW),', Pa)'
!
!---      Read withdrawl/production volume flow rate  ---
!
      ELSEIF( IT_CW(NCW).EQ.-1 ) THEN
        VARB = 'Withdrawl/Production Volume Flow Rate'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(2,M,NCW))
        VAR_CW(2,M,NCW) = -1.0*(VAR_CW(2,M,NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',VAR_CW(2,M,NCW)
        IUNM = 3
        IUNS = -1
        CALL RDUNIT(UNTS,VAR_CW(2,M,NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR_CW(2,M,NCW),', m^3/s)'
!
!---        Read minimum well-bottom pressure  ---
!
        VARB = 'Minimum Well-Bottom Pressure'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(3,M,NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',VAR_CW(3,M,NCW)
        INDX = 0
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNTS,VAR_CW(3,M,NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR_CW(3,M,NCW),', Pa)'
!
!---      Read withdrawl/production mass flow rate  ---
!
      ELSEIF( IT_CW(NCW).EQ.-2 .OR. IT_CW(NCW).EQ.-22 .OR. IT_CW(NCW).EQ.-32 ) THEN
        VARB = 'Withdrawl/Production Mass Flow Rate'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(2,M,NCW))
        VAR_CW(2,M,NCW) = - 1.0*(VAR_CW(2,M,NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',VAR_CW(2,M,NCW)
        IUNKG = 1
        IUNS = -1
        CALL RDUNIT(UNTS,VAR_CW(2,M,NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR_CW(2,M,NCW),', kg/s)'
!
!---        Read minimum well-bottom pressure  ---
!
        VARB = 'Minimum Well-Bottom Pressure'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(3,M,NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(4X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
          UNTS(1:NCH),': ',VAR_CW(3,M,NCW)
        INDX = 0
        IUNM = -1
        IUNKG = 1
        IUNS = -2
        CALL RDUNIT(UNTS,VAR_CW(3,M,NCW),INDX)
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A)') ' (',VAR_CW(3,M,NCW),', Pa)'
      ENDIF
!
!---      Read water concentration dissolved in CO2  ---
!
      IF( IT_CW(NCW).EQ.3 ) THEN
        VARB = 'Water Relative Humidity'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(4,M,NCW))
        if(me.eq.0)WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR), &
          ': ',VAR_CW(4,M,NCW)
      ELSEIF( IT_CW(NCW).EQ.2 ) THEN
        VARB = 'Water Mass Fraction'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(4,M,NCW))
        if(me.eq.0)WRITE(IWR,'(4X,2A,1PE11.4)') VARB(1:IVR), &
          ': ',VAR_CW(4,M,NCW)
      ELSEIF( IT_CW(NCW).EQ.1 ) THEN
        VAR_CW(4,M,NCW) = 0.D+0
      ENDIF
!
!---      Read salt concentration dissolved in aqueous  ---
!
      IT_CWX = MOD(IT_CW(NCW),10)
      IT_CWX = IT_CW(NCW)-IT_CWX
      IF( ABS(IT_CWX).EQ.20 ) THEN
       DO N_SL = 1,NSOLU
        VARB = 'Aqueous Solute Concentration'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR_CW(5+N_SL,M,NCW))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0)WRITE(IWR,'(4X,2A,A10,3A,1PE11.4)') VARB(1:IVR),', ', SOLUT(N_SL),', ', &
          UNTS(1:NCH),': ',VAR_CW(5+N_SL,M,NCW)
       ENDDO
!
      ENDIF
    200   CONTINUE
    300 CONTINUE


!!!!! Global array stuff - Bryan !!!!!! 

  ldxx(1) = iaxmax - iaxmin + 1 ! local number of grid in x direction
  ldxx(2) = iaymax - iaymin + 1 ! y
  ldxx(3) = iazmax - iazmin + 1 ! z
  lo(1) = iaxmin ! min x grid index
  lo(2) = iaymin
  lo(3) = iazmin
  hi(1) = iaxmax ! max x grid index
  hi(2) = iaymax
  hi(3) = iazmax
  dims(1) = nxdim
  dims(2) = nydim
  dims(3) = nzdim
  dims(4) = n_entry
  ndimx = 4
  g_buf2 = ga_create_handle()  ! creat a global array g_buf2
  call ga_set_data(g_buf2, ndimx, dims, MT_DBL) ! 1. set the dimension and type.
                                                ! 2. ndimx: number of dimensions,
                                                ! should be a interger;
                                                ! 3. dims: size of arrary in each
                                                ! dimension, should be an array
                                                ! of integers;
                                                ! 4. MT_DBL: double precision
  status = ga_allocate(g_buf2)
    
    ncount = nit_cw
    ncount6 = n_entry*ncount
   if(me.eq.0) then
    do icntx = 1,n_entry  ! each entry has all the intervals - Bryan
      icbx = (icntx-1)*ncount + 1 ! set the beginnning and ending points for each entry
      icex = icntx*ncount
      ! replicate the index matrix 27 times- Bryan
      idx_buf2(1,icbx:icex) = idx_buf2(1,1:ncount) ! i index of all intervals 
      idx_buf2(2,icbx:icex) = idx_buf2(2,1:ncount) ! j 
      idx_buf2(3,icbx:icex) = idx_buf2(3,1:ncount) ! k
      idx_buf2(4,icbx:icex) = icntx                ! index of the entry
      val_buft(icbx:icex) = temp_f(1:ncount,icntx) ! reshape and store the data in temp_f
    enddo
    alpha = 1.d0
        ! Scatter acc put (1).local interval index;(2).well index;
        !    (3). starting and ending xyz location (6 parameters); and (4)if screened
        !    into a shared memory array g_buf2.
        ! Now g_buf2 has infor that in each field grid, if there are wells, if
        ! yes, 9 parameters for each well are inlcuded.  - Bryan  
    call nga_scatter_acc(g_buf2,val_buft(1),idx_buf2(1,1),ncount6,alpha)  ! idx_buf2(1:3,1) ??? - Bryan
   endif
   call ga_sync
!    call nga_scatter(g_buf2,val_buft(1),idx_buf2(1,1),ncount6)
    allocate(ibuf4(ldxx(1),ldxx(2),ldxx(3),n_entry))
    ibuf4 = 0.d0
    lo(4) = 1
    hi(4) = n_entry
    call nga_get(g_buf2,lo,hi,ibuf4(1,1,1,1),ldxx)

    allocate(xp_cwx(18))  ! 6 should be enough - Bryan
    nx = 0
    i_iwx = 0
    allocate(n_wfx(n_cw))
    n_wfx = 0
    call get_node_mask(id_g2l) ! whether node is local or ghost
    do kx=1,ldxx(3)
    do jx=1,ldxx(2)
    do ix=1,ldxx(1)
      nx = nx+1  ! local index the field node in a 1D array??? - Bryan
      if(id_g2l(nx) <= 0) cycle
      iwix = 0
      ibufx = int(ibuf4(ix,jx,kx,1)) !global index of 1st interval - Bryan

!      write(*,*) 'rank=',me,'ibufx',ibufx

      if(ibufx == 0) cycle ! if no well in this gird, skip the rest
      iten9 = 10  !global index of 2nd interval 
      iten8 = 19  !global index of 3rd interval
      ibuf2x_ = int(ibuf4(ix,jx,kx,iten9))
      ibuf3x_ = int(ibuf4(ix,jx,kx,iten8))
      bufx_(1) = ibufx
      bufx_(2) = ibuf2x_
      bufx_(3) = ibuf3x_
      iwix = 1 ! check how many intervals this grid has - Bryan
      if(ibuf3x_.ne.0) then
        iwix = 3
      elseif(ibuf2x_.ne.0) then
        iwix = 2
      else
        iwix = 1
      endif
!well number
      nwx_ = int(ibuf4(ix,jx,kx,2))
!        write(*,*) 'rank=',me,'ibufx,nwx_',ibufx,nwx_,'i,j,k',ix,jx,kx
!      allocate(id_iwx(lwi_cw,n_cw))
      if(iwix.ne.0) then
!temporary array to hold number of nodes covered by the well nwx_
        n_wfx(nwx_) = n_wfx(nwx_)+1  ! local number of nodes used in each well - Bryan
      endif 
      do iwx = 1,iwix ! loop over intervals in the grid
!interval
        i_iwx = bufx_(iwx) ! global interval index -Bryan
!principal node for i_iwx well segment
        iwn_cwx(i_iwx,nwx_) = nx  ! Index of field node has this well segment - Bryan
!global well segment present in this process
        inv_cwx(i_iwx,nwx_) = 1 ! Is this well segment existing in this partition? - Bryan
        icordx = 0
        do icx = 3+9*(iwx-1),6+2+9*(iwx-1)
          icordx = icordx+1
          xp_cwx(icordx) = ibuf4(ix,jx,kx,icx) ! xp_cwx has the starting and
                                                !ending xyz locations of the current intervals(2x3=6 parameters) - Bryan
        enddo
        ! store the interval starting and ending locations - Bryan
        xp_cw_g(1,i_iwx,nwx_) = xp_cwx(1)
        yp_cw_g(1,i_iwx,nwx_) = xp_cwx(2)
        zp_cw_g(1,i_iwx,nwx_) = xp_cwx(3)
        xp_cw_g(2,i_iwx,nwx_) = xp_cwx(4)
        yp_cw_g(2,i_iwx,nwx_) = xp_cwx(5)
        zp_cw_g(2,i_iwx,nwx_) = xp_cwx(6)
        is_cwx(i_iwx,nwx_) = int(ibuf4(ix,jx,kx,9*iwx)) ! if screened - Bryan
      enddo
    enddo
    enddo
    enddo
    deallocate(idx_buf2)
    deallocate(val_buft)
    deallocate(temp_f)
    deallocate(ibuf4)
    deallocate(xp_cwx)
    git_cw = it_cw  ! well type -Bryan
    ff_cwx = ff_cw  ! fraction factor-Bryan
    
    it_cw = 0
!sort
    n_l_cw = 0
    l_nwn_cw = 0
    l_nwf_cw = 0
    do n_cwx = 1,n_cw
     idim1 = size(inv_cwx,dim=1)
     iwx_ = 0
     allocate(invx(idim1))
     invx = 0
    
     do i_iwx = 1,ni_cw(n_cwx)
      if(inv_cwx(i_iwx,n_cwx) == 1) then ! current partition has segment in this well - Bryan
        iwx_ = iwx_+1                    ! update number of intervals of this well on this partition
        invx(iwx_) = i_iwx               ! store local well interval index
      endif
     enddo
     if(iwx_ > 0) then ! if there is well segment in this partition-Bryan
! need to convert for pumping well, use negative
      call sort(iwx_,invx)
      n_l_cw = n_l_cw+1       ! local number of wells in this partition -Bryan
!global well number

      ! id_cw(3:7),it_cw,ff_cw are local variables - Bryan
      id_cw(7,n_l_cw) = n_cwx          ! global well index
      it_cw(n_l_cw) = git_cw(n_cwx)    ! well type
      ff_cw(1:3,n_l_cw) = ff_cwx(1:3,n_cwx) ! well fraction factor
      if(n_l_cw == 1) then
       id_cw(3,n_l_cw) = 1                     ! local starting well interval index of this well in this partition
       id_cw(5,n_l_cw) = 1                     !       
      elseif(n_l_cw > 1) then
       id_cw(3,n_l_cw) = id_cw(4,n_l_cw-1)+1   ! local starting well interval index of this well in this partition
       id_cw(5,n_l_cw) = id_cw(6,n_l_cw-1)+1
      endif
      id_cw(4,n_l_cw) = id_cw(3,n_l_cw)+iwx_-1 ! local ending well interval index of this well in this partition
      id_cw(6,n_l_cw) = id_cw(5,n_l_cw)+n_wfx(n_cwx)-1 ! update how many nodes are covered by this well locally
      nc_nx = 0
      np_nx = 0
      do ix_ = 1,iwx_ ! loop over intervals of this well on this partition - Bryan
        i_iwx = invx(ix_)  ! global well inverval index
        nx = iwn_cwx(i_iwx,n_cwx) ! field node index which has this inverval
        l_nwn_cw = l_nwn_cw+1 ! update number of intervals of all wells in this partition
        iwn_cw(l_nwn_cw) = nx ! store field node index for this interval
!local interval to global interval
        iwi_cw(l_nwn_cw) = i_iwx ! store global well interval index in the local variable
        if(np_nx /= nx) then
          nc_nx = nc_nx+1          ! update for this well only (not used anywhere else)
          l_nwf_cw = l_nwf_cw + 1  ! number of field nodes covered by the intervals, update for all wells
          iwf_cw(l_nwf_cw) = nx    ! flag the 1st interval in this node  
          if(i_iwx == 1) iwt_cw(nx) = 1 ! flag the field grid containing the 1st inverval of the well
!          if(i_iwx == 1) write(*,*) 'First interval is at node: ', nx
!          if(i_iwx == ni_cw(n_cwx)) iwt_cw(nx) = -1 ! flag the field grid containing the last inverval of the well - For production well -Bryan
!          if(i_iwx == ni_cw(n_cwx)) write(*,*) 'Last interval is at node: ', nx
          np_nx = nx                    ! pass down the current field node index
        endif
        iwp_cw(l_nwn_cw) = l_nwf_cw   !NOT used anywhere else 
        xp_cw(1,l_nwn_cw) = xp_cw_g(1,i_iwx,n_cwx) ! starting x position for this interval
        yp_cw(1,l_nwn_cw) = yp_cw_g(1,i_iwx,n_cwx)
        zp_cw(1,l_nwn_cw) = zp_cw_g(1,i_iwx,n_cwx)
        xp_cw(2,l_nwn_cw) = xp_cw_g(2,i_iwx,n_cwx) ! ending x position or this interval
        yp_cw(2,l_nwn_cw) = yp_cw_g(2,i_iwx,n_cwx)
        zp_cw(2,l_nwn_cw) = zp_cw_g(2,i_iwx,n_cwx)
      enddo
     endif
     deallocate(invx)
    enddo
!    iwp_cw = iwn_cw
!    id(6,:) = n_wfx(:)
    deallocate(n_wfx)
    deallocate(ff_cwx)
!    allocate(xp_cw_tmp(2,l_nwn_cw))
!deallocate later
!    xp_cw_tmp(1:2,1:l_nwn_cw) = xp_cw(1:2,1:l_nwn_cw)
!    deallocate(xp_cw)

!LUK indicates which mode is used, set in RD_SOLU in step.F90 - Bryan
! For water mode, LUK = 1 
    isvc2 = luk+2

    allocate(fxw_cw(isvc2,nit_cw))
    allocate(pf_cw(nit_cw))
    allocate(fx_cw(n_l_cw))
    allocate(pl_cw(n_l_cw))
    allocate(mcw(luk+2))
    allocate(mfd(luk+2))
    DO M = 1,ISVC+2
      IF( M.NE.ISVC+2 ) THEN
        MCW(M) = 2 
      ELSE
        MCW(M) = 3
      ENDIF
      IF( M.NE.ISVC+2 ) THEN
        MFD(M) = M+1
      ELSE
        MFD(M) = 2
      ENDIF
    enddo
    lukx = 2*luk
    luk_cw = 0
    luk1 = 0
    do ncwx=1,n_l_cw
      luk_cw = max(luk_cw,(lukx*(id_cw(6,ncwx)-id_cw(5,ncwx)+1))) 
    enddo
    luk_cw = luk_cw+1
    allocate(rs_cw(luk_cw,n_l_cw))
    allocate(blu_cw(n_l_cw))
    do ncwx = 1,n_cw
      luk1=max(luk1,(id_cw(2,ncwx)-id_cw(1,ncwx))+1)
    enddo
    allocate(t_acwx(luk1,n_cw))

!
!---  End of RDCOUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE RSDL_COUP_WELL
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
!
!     STOMP-CO2e
!
!     Coupled-well equation residuals
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 21 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE OUTPU
  USE JACOB
  USE HYST
  USE FILES
!  USE FDVS
  USE FDVP
  USE COUP_WELL
  USE CONST
  use grid_mod
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
!----------------------Parameter Statements----------------------------!
!
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Executable Lines--------------------------------!
!
  me = ga_nodeid()
  IF( ICNV.EQ.1 .OR. ICNV.EQ.4 ) RETURN
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $' 
!
!---  Zero maximum residuals  ---
!
  RSD_CW = 0.D+0
  NSD_CW = 0
!
!---  Loop over coupled wells ---
!
  DO 100 NCW = 1,N_L_CW
    ngcwx = id_cw(7,ncw)
    DP_CWX = BLU_Cw(ncw)
!
!---    Injection well ---
!
    IF( GIT_CW(NGCWX).GT.0 ) THEN
!
!---      Pressure controlled coupled well  ---
!
      IF( ID_CW(8,NCW).EQ.1 ) THEN
        RSDX = 0.D+0
!
!---      Flow controlled coupled well  ---
!
      ELSE
        RSDX = ABS(DP_CWX)/(ABS(P_CW(2,NCW))+PATM)
      ENDIF
!
!---    Withdrawl well ---
!
    ELSEIF( GIT_CW(NGCWX).LT.0 ) THEN
!
!---      Pressure controlled coupled well  ---
!
      IF( ID_CW(8,NCW).EQ.1 ) THEN
        RSDX = 0.D+0
!
!---      Flow controlled coupled well  ---
!
      ELSE
        RSDX = ABS(DP_CWX)/(ABS(P_CW(2,NCW))+PATM)
      ENDIF
    ENDIF
    IF( RSDX.GT.RSD_CW ) THEN
      RSD_CW = RSDX
      NSD_CW = NCW
    ENDIF
   100 CONTINUE
  call ga_dgop(1,rsd_cw,1,'max')
  IF( RSD_CW.GT.RSDMX ) ICNV = 2
!
!---  End of RSDL_COUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE UPDT_COUP_WELL
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
!
!     STOMP-CO2e
!
!     Update coupled-well pressure.  Injection wells are limited
!     by a high-pressure limit, and withdrawl wells are limited by a 
!     low-pressure limit.
!
!----------------------Authors-----------------------------------------!
!
!     Written by M.D. White, PNNL, 20 April 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE JACOB
  USE COUP_WELL
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
!----------------------Executable Lines--------------------------------!
!
!  ISUB_LOG = ISUB_LOG+1
!  SUB_LOG(ISUB_LOG) = '/UPDT_COUP_WELL'
  IF( INDEX(CVS_ID(334)(1:1),'$').EQ.0 ) CVS_ID(334) = &
   '$Id: well_co2e.F,v 1.1 2011/09/09 17:15:38 d3c002 Exp $'

  me=ga_nodeid()
!
!---  Loop over coupled wells ---
!
  DO 100 NCW = 1,N_L_CW
    ngcwx = id_cw(7,ncw)
    dp_cwx = blu_cw(ncw)
    dpx = 5.d5
    DP_CWX = SIGN( MIN(ABS(DPX),ABS(DP_CWX)),DP_CWX )
    IF( GIT_CW(NGCWX).GT.0 ) THEN
         P_CW(2,NCW) = P_CW(2,NCW) + DP_CWX
    ELSEIF( GIT_CW(NGCWX).LT.0 ) THEN
         P_CW(2,NCW) = P_CW(2,NCW) - DP_CWX
    ENDIF
!
!---    Limit coupled-well pressure to upper limit for injection
!       wells or lower limit for withdrawl wells  ---
!
    IF(GIT_CW(NGCWX).GT.0 ) THEN
      P_CW(2,NCW) = MIN( PL_CW(NCW),P_CW(2,NCW) )
    ELSEIF(GIT_CW(NGCWX).LT.0 ) THEN
      P_CW(2,NCW) = MAX( PL_CW(NCW),P_CW(2,NCW) )
    ENDIF
    100 CONTINUE
!
!---  End of UPDT_COUP_WELL group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE VCROSSP( AX,BX,CX )
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
!     Vector cross product.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 March 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
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
  REAL*8 AX(3),BX(3),CX(3)
!
!----------------------Executable Lines--------------------------------!
!
  CX(1) = AX(2)*BX(3) - AX(3)*BX(2)
  CX(2) = AX(3)*BX(1) - AX(1)*BX(3)
  CX(3) = AX(1)*BX(2) - AX(2)*BX(1)
!
!---  End of VCROSSP group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  FUNCTION VDOTP( AX,BX )
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
!     Vector dot product.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 March 2011.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
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
  REAL*8 AX(3),BX(3)
!
!----------------------Executable Lines--------------------------------!
!
  VDOTP = AX(1)*BX(1) + AX(2)*BX(2) + AX(3)*BX(3)
!
!---  End of VDOTP group  ---
!
  RETURN
  END

  !----------------------Subroutine--------------------------------------!
!
  FUNCTION LOCATEINDEX( X_WELL,X_DIM,X_GRID) RESULT(I_NODE)
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
!     Locate the well segment in grids and return the index
!
!----------------------Authors-----------------------------------------!
!
!     Written by Bryan He, PNNL, 21 April 2021.
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
!
  
!----------------------Implicit Double Precision-----------------------!
!
!  IMPLICIT REAL*8 (A-H,O-Z)
!  IMPLICIT INTEGER (I-N)
!
   IMPLICIT NONE
!----------------------Parameter Statements----------------------------!
!
   REAL*8,INTENT(IN) :: X_WELL
   INTEGER,INTENT(IN) :: X_DIM
   REAL*8,DIMENSION(1:X_DIM),INTENT(IN) :: X_GRID(1:X_DIM)
   INTEGER ::  I_NODE
   INTEGER :: i
  
!
!----------------------Type Declarations-------------------------------!
!
!
!----------------------Executable Lines--------------------------------!
!
  I_NODE = 0
  i = 0
  DO i = 1, X_DIM-1 
     IF (X_WELL <= X_GRID(i+1) .AND. X_WELL >= X_GRID(i)) THEN
        I_NODE = i
        exit
     ENDIF
  ENDDO

  RETURN
  END

 !
  SUBROUTINE WRITE_WELL_SOLUTE_FLUX 
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
!     Write coupled well solute flux to a file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 30 March 2011.
!

!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE JACOB
  USE GRID
  USE FDVP
  USE COUP_WELL
  USE PETSCAPP
  USE TRNSPT

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
  INTEGER:: nscw,i,j
!
!----------------------Executable Lines--------------------------------!
!
  me=ga_nodeid()
  nscw = n_cw*2*nsolu
!  write(*,*) 'n_cw,nsolu,nscw:',n_cw,nsolu,nscw
  if (write_well_flux(2)==1 .and. me==0) then
     write(iwsf,'(1PE22.15,1X,<nscw>(1PE22.15,1X))')&
                TM, ((c_flux_cw(j,i),c_total_cw(j,i),i=1,n_cw),j=1,nsolu)     
  endif
!
!---  End of write_well_solute_flux group  ---
!
  RETURN
  END 
