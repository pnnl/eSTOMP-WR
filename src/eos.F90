!  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE WEBB_BC( N )
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
!     Webb saturation and capillary pressure matching points for
!     the Brooks-Corey capillary pressure-saturation
!     function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 16 July 2010
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE PORMED
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
!  SUB_LOG(ISUB_LOG) = '/WEBB_BC'
!  IF( INDEX(CVS_ID(55)(1:1),'$').EQ.0 ) CVS_ID(55) = &
!   '$Id: eos_33.F,v 1.26 2009/05/15 14:58:52 d3c002 Exp $' 
!
!---  Find the saturation matching point  ---
!
  IZN = N
  SRX = SCHR(4,IZN)
  PSIX = SCHR(1,IZN)
  CLX = MAX( SCHR(3,IZN),SMALL )
!
!---  Use the matrix saturation at 0.4 LOG10(HDOD) as
!     the initial guess  ---
!
  HDX = 1.D+1**(4.D-1*LOG10(HDOD))
  SMPX = (PSIX/HDX)**CLX
  SMPX = SMPX*(1.D+0-SRX) + SRX
!
!---  Newton-Raphson iteration for the matrix saturation
!     matching point  ---
!
  NC = 0
    100 CONTINUE
  NC = NC + 1
  SEMPX = (SMPX-SRX)/(1.D+0-SRX)
  FX = LOG10(HDOD) - LOG10((PSIX/(SEMPX**(1.D+0/CLX)))) &
    - 1.D+0/(LOG(1.D+1)*CLX*(SMPX-SRX))
  DFX = 1.D+0/(LOG(1.D+1)*CLX*(SMPX-SRX)) &
    + 1.D+0/(LOG(1.D+1)*CLX*((SMPX-SRX)**2))
  DSMPX = -FX/DFX
  SMPX = MAX( SMPX+DSMPX,SRX+1.D-12 )
!
!---  No convergence on saturation matching point  ---
!
  IF( NC.GT.32 ) THEN
    INDX = 7
    IMSG = N
    CHMSG = 'No Convergence on Saturation ' &
      // 'Matching Point @ Node: '
    CALL WRMSGS( INDX )
  ENDIF
  IF( ABS(DSMPX).GT.1.D-9 ) GOTO 100
  SCHR(8,IZN) = SMPX
!
!---  Find the capillary head matching point  ---
!
  SEMPX = (SMPX-SRX)/(1.D+0-SRX)
  SCHR(9,IZN) = PSIX/(SEMPX**(1.D+0/CLX))
!
!---  Reset subroutine string sequence  ---
!
!  ISUB_LOG = ISUB_LOG-1
!
!---  End of WEBB_BC group  ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE WEBB_VG( N )
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
!     Webb saturation and capillary pressure matching points for
!     the van Genuchten capillary pressure-saturation
!     function.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 16 July 2010
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
  USE PORMED
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
!  SUB_LOG(ISUB_LOG) = '/WEBB_VG'
!  IF( INDEX(CVS_ID(55)(1:1),'$').EQ.0 ) CVS_ID(55) = &
!   '$Id: eos_33.F,v 1.26 2009/05/15 14:58:52 d3c002 Exp $' 
  IZN = N
!
!---  Find the matrix saturation matching point  ---
!
  SRX = SCHR(4,IZN)
  ALPHAX = SCHR(1,IZN)
  CNX = SCHR(3,IZN)
  CMX = SCHR(14,IZN)
!
!---  Use the matrix saturation at 0.4 LOG10(HDOD) as
!     the initial guess  ---
!
  HDX = 1.D+1**(4.D-1*LOG10(HDOD))
  SMPX = (1.D+0/(1.D+0 + (ALPHAX*HDX)**CNX))**CMX
  SMPX = SMPX*(1.D+0-SRX) + SRX
!
!---  Newton-Raphson iteration for the matrix saturation
!     matching point  ---
!
  NC = 0
    100 CONTINUE
  NC = NC + 1
  SEMPX = (SMPX-SRX)/(1.D+0-SRX)
  ESEMPX = (1.D+0/SEMPX)**(1.D+0/CMX)
  FX1 = LOG10(HDOD)
  FX1 = FX1 - LOG10(((ESEMPX-1.D+0)**(1.D+0/CNX))/ALPHAX)
  FX1 = FX1 - (SMPX/(SMPX-SRX))/(LOG(1.D+1)*CNX*CMX* &
    (1.D+0-(SEMPX**(1.D+0/CMX))))
  SMPY = SMPX + 1.D-8
  SEMPX = (SMPY-SRX)/(1.D+0-SRX)
  ESEMPX = (1.D+0/SEMPX)**(1.D+0/CMX)
  FX2 = LOG10(HDOD)
  FX2 = FX2 - LOG10(((ESEMPX-1.D+0)**(1.D+0/CNX))/ALPHAX)
  FX2 = FX2 - (SMPY/(SMPY-SRX))/(LOG(1.D+1)*CNX*CMX* &
    (1.D+0-(SEMPX**(1.D+0/CMX))))
  DFX = (FX2-FX1)/1.D-8
  DSMPX = -FX1/DFX
  SMPX = MAX( SMPX+DSMPX,SRX+1.D-12 )
!
!---  No convergence on matrix saturation matching point  ---
!
  IF( NC.GT.32 ) THEN
    INDX = 7
    IMSG = N
    CHMSG = 'No Convergence on Saturation ' &
      // 'Matching Point @ Node: '
    CALL WRMSGS( INDX )
  ENDIF
  IF( ABS(DSMPX).GT.1.D-9 ) GOTO 100
  SCHR(16,IZN) = SMPX
!
!---  Find the matrix capillary head matching point  ---
!
  SEMPX = (SMPX-SRX)/(1.D+0-SRX)
  SCHR(17,IZN) = (1.D+0/ALPHAX)* &
    (((1.D+0/(SEMPX**(1.D+0/CMX)))-1.D+0)**(1.D+0/CNX))
!
!---  Reset subroutine string sequence  ---
!
!  ISUB_LOG = ISUB_LOG-1
!
!---  End of WEBB_VG group  ---
!
  RETURN
  END
