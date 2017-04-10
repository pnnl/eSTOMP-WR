!----------------------Function----------------------------------------!
!
      FUNCTION DIFMN( FDL,FDH,DXL,DXH,FLX,INDX )
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
!     Computes interfacial averages
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNNL, 21 June 2001.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/DIFMN'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(48)(1:1),'$').EQ.0 ) CVS_ID(48) = &
     '$Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Negative indices for manual configuration  ---
!
      IF( INDX.LT.0 ) THEN
        IDMNX = ABS(INDX)
      ELSE
        IDMNX = IDMN(INDX)
      ENDIF
!
!---  Harmonic mean: default mode  ---
!
!print *,'idmnx==',idmnx,flx
      IF( IDMNX.EQ.1 ) THEN
        IF( ABS(FDL*DXL+FDH*DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
        ELSE
          DIFMN = (FDL*FDH*(DXL+DXH))/(FDL*DXL+FDH*DXH)
        ENDIF
!
!---  Geometric mean  ---
!
      ELSEIF( IDMNX.EQ.2 ) THEN
        IF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
        ELSE
          DIFMN = SQRT((FDH**((2.D+0*DXL)/(DXL+DXH)))* &
          (FDL**((2.D+0*DXH)/(DXL+DXH))))
        ENDIF
!
!---  Arithmetic mean  ---
!
      ELSEIF( IDMNX.EQ.3 ) THEN
        IF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
        ELSE
          DIFMN = (FDH*DXL+FDL*DXH)/(DXL+DXH)
        ENDIF
!
!---  Upwind mean  ---
!
      ELSEIF( IDMNX.EQ.4 ) THEN
        IF( FLX.GT.EPSL ) THEN
          DIFMN = FDL
        ELSEIF( FLX.LT.-EPSL ) THEN
          DIFMN = FDH
        ELSEIF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
        ELSE
          DIFMN = (FDH*DXL+FDL*DXH)/(DXL+DXH)
        ENDIF
!
!---  Downstream mean  ---
!
      ELSEIF( IDMNX.EQ.5 ) THEN
        IF( FLX.GT.EPSL ) THEN
          DIFMN = 5.D-1*((1.D+0+WFMN(INDX))*FDL+(1.D+0-WFMN(INDX))*FDH)
        ELSEIF( FLX.LT.-EPSL ) THEN
          DIFMN = 5.D-1*((1.D+0+WFMN(INDX))*FDH+(1.D+0-WFMN(INDX))*FDL)
        ELSEIF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
        ELSE
          DIFMN = (FDH*DXL+FDL*DXH)/(DXL+DXH)
        ENDIF
!
!---  Nieber downstream mean  ---
!
      ELSEIF( IDMNX.EQ.7 ) THEN
        IF( FLX.GT.EPSL ) THEN
         IF( FDH.LT.EPSL ) THEN
          DIFMN = 5.D-1*((1.D+0+WFMN(INDX))*FDL+(1.D+0-WFMN(INDX))*FDH)
         ELSEIF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
         ELSE
          DIFMN = (FDH*DXL+FDL*DXH)/(DXL+DXH)
         ENDIF
        ELSEIF( FLX.LT.-EPSL ) THEN
         IF( FDL.LT.EPSL ) THEN
          DIFMN = 5.D-1*((1.D+0+WFMN(INDX))*FDH+(1.D+0-WFMN(INDX))*FDL)
         ELSEIF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
         ELSE
          DIFMN = (FDH*DXL+FDL*DXH)/(DXL+DXH)
         ENDIF
        ELSEIF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
        ELSE
          DIFMN = (FDH*DXL+FDL*DXH)/(DXL+DXH)
        ENDIF
!
!---  Downwind  ---
!
      ELSEIF( IDMNX.EQ.8 ) THEN
        IF( FLX.GT.EPSL ) THEN
          DIFMN = FDH
        ELSEIF( FLX.LT.-EPSL ) THEN
          DIFMN = FDL
        ELSEIF( ABS(DXL+DXH)/EPSL.LT.EPSL ) THEN
          DIFMN = 0.D+0
        ELSE
          DIFMN = (FDH*DXL+FDL*DXH)/(DXL+DXH)
        ENDIF
      ENDIF
!
!---  End of DIFMN group
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION FLIMIT( R,CRN,INDX )
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
!     Computes interfacial averages
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, December 14, 1999.
!     Last Modified by MD White, Battelle, PNL, December 14, 1999.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/FLIMIT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(48)(1:1),'$').EQ.0 ) CVS_ID(48) = &
     '$Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Leonard-TVD flux limiter  ---
!
      IF( INDX.EQ.1 ) THEN
        FLIMIT = MAX( 0.D+0,MIN( 2.D+0,2.D+0*R, &
        ((2.D+0-CRN+R*(1.D+0+CRN))/3.D+0) ) )
!
!---  Roe's Superbee flux limiter  ---
!
      ELSEIF( INDX.EQ.2 ) THEN
        FLIMIT = MAX( 0.D+0,MIN( 2.D+0*R,1.D+0 ),MIN( R,2.D+0 ) )
!
!---  First-order upwind  ---
!
      ELSEIF( INDX.EQ.3 ) THEN
        FLIMIT = 0.D+0
      ENDIF
!
!---  End of FLIMIT group
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION ICOUNT( I )
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
!     Count the number of digits in an integer variable.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 1993.
!     Last Modified by MD White, Battelle, PNL, April 14, 1994.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/ICOUNT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(48)(1:1),'$').EQ.0 ) CVS_ID(48) = &
     '$Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      IC = ABS(I)
      ICOUNT = 0
   10 CONTINUE
      ICOUNT = ICOUNT + 1
      IC = IC/10
      IF( IC.GT.0 ) GOTO 10
      IF( I.LT.0 ) ICOUNT = ICOUNT + 1
!
!---  End of ICOUNT group
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION DSHIFT( SV,SVP,DPV,DPVP,IEQX,M )
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
!     Count the number of digits in an integer variable.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, March, 2000.
!     Last Modified by MD White, Battelle, PNL, March 27, 2000.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/DSHIFT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(48)(1:1),'$').EQ.0 ) CVS_ID(48) = &
     '$Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      IF( M.EQ.(IEQX+2) ) THEN
        DSHIFT = (SVP*DPV-SV*DPV+SV*DPVP)/(DPVP+SMALL)
      ELSE
        DSHIFT = SVP
      ENDIF
!
!---  End of DSHIFT group
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION SCALING( GX,VX,INDX )
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
!     Count the number of digits in an integer variable.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, 24 April 2001.
!     Last Modified by MD White, Battelle, PNNL, 24 April 2001.
!     $Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/SCALING'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(48)(1:1),'$').EQ.0 ) CVS_ID(48) = &
     '$Id: difmn.F,v 1.7 2005/02/03 21:24:32 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Logarithmic scaling  ---
!
      IF( INDX.EQ.2 ) THEN
        SCALING = EXP(GX*VX)
!
!---  Linear scaling  ---
!
      ELSEIF( INDX.EQ.1 ) THEN
        SCALING = GX*VX
!
!---  Unrecognized scaling  ---
!
      ELSE
        INDX = 3
        CHMSG = 'Unrecognized Scaling Function'
        CALL WRMSGS( INDX )
      ENDIF
!
!---  End of SCALING group
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

