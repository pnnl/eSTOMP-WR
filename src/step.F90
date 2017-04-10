!----------------------Program-----------------------------------------!
!

      SUBROUTINE STEP



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
!----------------------Description-------------------------------------!
!
!     STOMP: Subsurface Transport Over Multiple Phases
!
!     This utility program reads STOMP input files and writes a
!     STOMP parameter file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 29 September 2002.
!     Last Modified by MD White, PNNL, 29 September 2002.
!     Last Modified by WE Nichols, PNNL, 13 June 2003.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
      USE BUFFEREDREAD
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
!----------------------Type Declarations-------------------------------!
!



      CHARACTER*512 CHDUM
      LOGICAL FCHK
      EXTERNAL I_COUNT
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      USE_GA = .TRUE.
      ICSN = 0
      SUBNMX = 'STEP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Search input file for Solution Control Card --
!
      IF( BUFFEREDREAD_FIND( '~solution' ) )THEN
        INDX = 1
        CHMSG = 'Reading Solution Control Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_SOLU
      ENDIF
!
!---  Search input file for saturation function card --
!
      IF( BUFFEREDREAD_FIND( '~saturation' ) )THEN
        INDX = 1
        CHMSG = 'Reading Aqueous Saturation Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_SP
      ENDIF
!
!---  Search input file for aqueous relative permeability card --
!
      IF( BUFFEREDREAD_FIND( '~aqueous rel' ) )THEN
        INDX = 1
        CHMSG = 'Reading Aqueous Relative Permeability Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_LRP
      ENDIF
!
!---  Search input file for solute/fluid interaction card --
!
      IF( BUFFEREDREAD_FIND( '~solute/fluid' ) )THEN
        INDX = 1
        CHMSG = 'Reading Solute/Fluid Interactions Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_TF
      ENDIF
!
!---  Search input file for boundary conditions card --
!
      IF( BUFFEREDREAD_FIND( '~boundary' ) )THEN
        INDX = 1
        CHMSG = 'Reading Boundary Conditions Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_BC
      ENDIF
!
!---  Search input file for source card --
!
      IF( BUFFEREDREAD_FIND( '~source' ) )THEN
        INDX = 1
        CHMSG = 'Reading Source Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_SR
      ENDIF
!
!---  Search input file for surface flux card --
!
      IF(  BUFFEREDREAD_FIND( '~surface' ) )THEN
        INDX = 1
        CHMSG = 'Reading Surface Flux Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_SF
      ENDIF
!
!---  Search input file for aqueous species card --
!
      IF( BUFFEREDREAD_FIND( '~aqueous species' ) )THEN
        INDX = 1
        CHMSG = 'Reading Aqueous Species Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_AQSP
      ENDIF
!
!---  Search input file for solid species card --
!
      IF( BUFFEREDREAD_FIND( '~solid species' ) )THEN
        INDX = 1
        CHMSG = 'Reading Solid Species Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_SDSP
      ENDIF
!
!---  Search input file for exchanged species card --
!
      IF( BUFFEREDREAD_FIND( '~exchanged species' ) )THEN
        INDX = 1
        CHMSG = 'Reading Exchanged Species Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_EXSP
      ENDIF
!
!---  Search input file for gas species card --
!
      IF( BUFFEREDREAD_FIND( '~gas species' ) ) THEN
        INDX = 1
        CHMSG = 'Reading Gas Species Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_GSSP
      ENDIF
!
!---  Search input file for equilibrium reactions card --
!
      IF( BUFFEREDREAD_FIND( '~equilibrium react' ) )THEN
        INDX = 1
        CHMSG = 'Reading Equilibrium Reactions Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_EQRC
      ENDIF
!
!---  Search input file for kinetic reactions card --
!
      IF( BUFFEREDREAD_FIND( '~kinetic react' ) )THEN
        INDX = 1
        CHMSG = 'Reading Kinetic Reactions Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_KNRC
      ENDIF
!
!---  Search input file for equilibrium equations card --
!
      IF( BUFFEREDREAD_FIND( '~equilibrium equat' ) )THEN
        INDX = 1
        CHMSG = 'Reading Equilibrium Equations Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_EQEQ
      ENDIF
!
!---  Search input file for kinetic equations card --
!
      IF( BUFFEREDREAD_FIND( '~kinetic equat' ) )THEN
        INDX = 1
        CHMSG = 'Reading Kinetic Equations Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_KNEQ
      ENDIF
!
!---  Search input file for conservation equations card --
!
      IF( BUFFEREDREAD_FIND( '~conservation equat' ) )THEN
        INDX = 1
        CHMSG = 'Reading Conservation Equations Card in STEP'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL RD_CNEQ
      ENDIF
!
!---  Compute parameters ---
!
      CALL CGLBP
!
!---  Close input and allocation file  ---
!
      CLOSE( UNIT=IRD )
!
!---  End of STEP program  ---
!
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CGLBP
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
!     Computed global parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, 22 May 2003.
!     Last Modified by MD White, Battelle, PNNL, 22 May 2003.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/CGLBP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Computed Parameters  ---
!
      LANW = LAN+(LWELL*LFZ*LNW)+(LSPILL*LFX*LFY)
      LPH = LL+LG+LN+LHYD
      LMPH = LL+LG+LN
      LCMP = LL+LS+LD
      LFXY = LFX*LFY
      LFYZ = LFY*LFZ
      LFZX = LFZ*LFX
      LFD = LFX*LFY*LFZ
      LHBW = LUK*LBD*LMNP+LUK-1
!
!---  Linear System Solver Variable Parameters  ---
!
      LJA = LBD + LSP*LANW*LUK
      LJB = LSP*(2*LAD+1)*LANW*LUK*LUK + LBD
      LJC = LSP*(LANW*LUK+1) + LBD
      LJD = LBD*(3*LHBW+1) + LSP
      LJE = LBD*LANW*LUK + LSP
      LJF = LANW*LUK
      LJG = LBD*(3*LHBW+1) + LSP*LANW*LUK
      LJH = LBD*LANW*LUK + LSP*7*LUK + LSP*3*LUK*LWELL
      LJI = LBD*LANW*LUK + LSP
      LJJ = LBD*LANW*LUK + LSP
      LJK = LBD + LSP*LANW
      LJL = 7
      LJM = LBD + LSP*(2*LAD+1)*LANW
      LJN = LBD + LSP*(LANW+1)
      LSU = 2+(2*(LGRL-1))
      LSV = (LUK+2)+(2*(LGRL-1))
      LSFV = (2*LUK+1)+(LGRL-1)
!
!---  Field Variable Parameters  ---
!
      LFDT = LFD**LT
      LFDL = LFD**LL
      LFDG = LFD**LG
      LFDN = LFD**LN
      LFDNH = LFD**((LN+LHYD)-(LN*LHYD))
      LFDC = LFD**LC
      LFDR = LFD**LR
      LFDCR = LFD**((LC+LR)-(LC*LR))
      LFDRL = LFD**(LR*LL)
      LFDRG = LFD**(LR*LG)
      LFDRN = LFD**(LR*LN)
      LFDI = LFD**((LFW+LHYD)-(LFW*LHYD)) 
      LFDS = LFD**((LS+LALC)-(LS*LALC))
      LFDD = LFD**((LD+LDCO2)-(LD*LDCO2))
      LFDA = LFD**LALC
      LFDH = LFD**LHYD
      LFDGC = LFD**LGC
!
!---  Surface Variable Parameters  ---
!
      LSX = (LFX+1)*LFY*LFZ
      LSY = LFX*(LFY+1)*LFZ
      LSZ = LFX*LFY*(LFZ+1)
      LSXT = LSX**LT
      LSXL = LSX**LL
      LSXG = LSX**LG
      LSXN = LSX**((LN+LHYD)-(LN*LHYD))
      LSXC = LSX**((LC+LR)-(LC*LR))
      LSXS = LSX**((LS+LALC)-(LS*LALC))
      LSXD = LSX**LD
      LSXGC = LSX**LGC
      LSYT = LSY**LT
      LSYL = LSY**LL
      LSYG = LSY**LG
      LSYN = LSY**((LN+LHYD)-(LN*LHYD))
      LSYC = LSY**((LC+LR)-(LC*LR))
      LSYS = LSY**((LS+LALC)-(LS*LALC))
      LSYD = LSY**LD
      LSYGC = LSY**LGC
      LSZT = LSZ**LT
      LSZL = LSZ**LL
      LSZG = LSZ**LG
      LSZN = LSZ**((LN+LHYD)-(LN*LHYD))
      LSZC = LSZ**((LC+LR)-(LC*LR))
      LSZS = LSZ**((LS+LALC)-(LS*LALC))
      LSZD = LSZ**LD
      LSZGC = LSZ**LGC
!
!---  Rock/Soil Variable Parameters  ---
!
      LRCT = LRC**LT
      LRCL = LRC**LL
      LRCG = LRC**LG
      LRCN = LRC**LN
      LRCS = LRC**((LS+LALC)-(LS*LALC))
      LRCD = LRC**LD
!
!---  Boundary Condition Parameters  ---
!
      LBCT = LBC**LT
      LBCL = LBC**LL
      LBCG = LBC**LG
      LBCN = LBC**LN
      LBCC = LBC**((LC+LR)-(LC*LR))
      LBCI = LBC**((LFW+LHYD)-(LFW*LHYD))
      LBCS = LBC**((LS+LALC)-(LS*LALC))
      LBCD = LBC**LD
      LBCA = LBC**LALC
      LBCU = LUK+(LPH*LCN)+LT+((LPLANT-1)*LSW*2)+3+(LNGC-1)
      LBCV = LBCU+((LPH**LPC)*(LSOLU*LC))+(LSPBC*LR)
      IF( LXYZG.EQ.1 ) LBCV = LBCV + 3
      LBCH = LBC**LHYD
      LBCGC = LBC**LGC
!
!---  Exchange species parameters  ---
!
      LSPE = MAX( LSPE,1 )
      LESITE = MAX( LESITE,1 )
!
!---  ECKEChem parameters  ---
!
      LSPR = MAX( LSPG+LSPL+LSPN+LSPS+LSPE,1 )
      LSPG = MAX( LSPG,1 )
      LSPL = MAX( LSPL,1 )
      LSPN = MAX( LSPN,1 )
      LSPS = MAX( LSPS,1 )
      LSPT = MAX( LSPT,1 )
!
!---  Output Variable Parameter
!     Number of Source Code Files Parameter  ---
!
      LOUPV = 400+33*((LSOLU**LC)+((LSPR+LSPT)**LR)+2-LC-LR)
      LFILES = 400
!
!---  Soil Moisture-Retention Characteristic Parameters  ---
!     Aqueous Relative Permeability Parameters
!     Gas Relative Permeability Parameters
!     NAPL Relative Permeability Parameters
!     Relative Permeability Tensor Parameters  ---
!
      LSCHR = 18
      LRPLC = 12
      LRPGC = 6
      LRPNC = 4
      LRPL = 4
!
!---  Coupled Multifluid Well Model Parameters  ---
!
      LNWN = LNW*LFZ
      LSZW = LNW*(LFZ+1)
      LNWV = 6
      LUKW = LUK*(1+LWELL)
!
!---  Noncondensible Gas Property Table Parameters  ---
!
      LP_TA = 72**LPTA
      LT_TA = 70**LPTA
      LT_TH = 100**LPTA
      LO_TH = 11**LPTA
      LT_PH = 155**LPTA
      LO_PH = 11**LPTA
      L_LV = 115**LPTA
      LINH = 15
!
!---  End of CGLBP group
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
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
!     Check to see if a character-string input exists.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 2000.
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/CHK_CHR'
      ICSNX = MAX( INDEX( SUBNMX,'  ' )-1,0 )
      ICSNO = ICSN
      ICSNS = MIN( ICSNX,1 ) + ICSNO
      ICSN = MIN( ICSNO+ICSNX,132 )
      SUBNM(ICSNS:ICSN) = SUBNMX
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      ISX = ISTART
      ICX = ICOMMA
      INDX = 0
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
!        CALL WRMSGS( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing real data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        GOTO 200
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        INDX = 1
        GOTO 200
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Character string recognized as a character string  ---
!
        INDX = 1
      ENDIF
  200 CONTINUE
      ISTART = ISX
      ICOMMA = ICX
!
!---  End of CHK_CHR group  ---
!
      ICSN = ICSNO
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
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
!     Fill double precision variable VAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, November 3, 1994.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2
      DATA FORM1 /'(D .0)'/
      DATA FORM2 /'(D  .0)'/
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/CHK_DPR'
      IDFLTD = 0
      ICSNX = MAX( INDEX( SUBNMX,'  ' )-1,0 )
      ICSNO = ICSN
      ICSNS = MIN( ICSNX,1 ) + ICSNO
      ICSN = MIN( ICSNO+ICSNX,132 )
      SUBNM(ICSNS:ICSN) = SUBNMX
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      ISX = ISTART
      ICX = ICOMMA
      INDX = 0
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
!        CALL WRMSGS( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing real data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        GOTO 200
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        INDX = 1
        GOTO 200
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for scientific notation  ---
!
        IEXP = ISTART-1
        IF( INDEX( CHDUM(ISTART:ISTOP),'e' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'e' )+ISTART-1
        ELSEIF( INDEX( CHDUM(ISTART:ISTOP),'d' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'d' )+ISTART-1
        ENDIF
        IPER = INDEX( CHDUM(ISTART:ISTOP),'.' )+ISTART-1
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          IF( N.EQ.IEXP .OR. N.EQ.IPER ) GOTO 120
          NC = ICHAR(CHDUM(N:N))
          IF( ( N.EQ.ISTART .OR. N.EQ.IEXP+1 ) .AND. &
          ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) GOTO 200
  120   CONTINUE
!
!---  Character string recognized as a double precision real  ---
!
        INDX = 1
      ENDIF
  200 CONTINUE
      ISTART = ISX
      ICOMMA = ICX
!
!---  End of CHK_DPR group  ---
!
      ICSN = ICSNO
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK_INT( ISTART,ICOMMA,CHDUM,INDX )
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
!     Fill integer variable IVAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 2000.
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*4 FORM1
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/CHK_INT'
      ICSNX = MAX( INDEX( SUBNMX,'  ' )-1,0 )
      ICSNO = ICSN
      ICSNS = MIN( ICSNX,1 ) + ICSNO
      ICSN = MIN( ICSNO+ICSNX,132 )
      SUBNM(ICSNS:ICSN) = SUBNMX
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      ISX = ISTART
      ICX = ICOMMA
      INDX = 0
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read numbers between commas  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing integer data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        GOTO 200
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        INDX = 1
        GOTO 200
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          NC = ICHAR(CHDUM(N:N))
          IF( N.EQ.ISTART .AND. ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) GOTO 200
  120   CONTINUE
!
!---  Character string recognized as an integer  ---
!
        INDX = 1
      ENDIF
  200 CONTINUE
      ISTART = ISX
      ICOMMA = ICX
!
!---  End of CHK_INT group  ---
!
      ICSN = ICSNO
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Function----------------------------------------!
!
      FUNCTION I_COUNT( I )
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
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/I_COUNT'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      IC = I
      I_COUNT = 0
   10 CONTINUE
      I_COUNT = I_COUNT + 1
      IC = IC/10
      IF( IC.GT.0 ) GOTO 10
!
!---  End of I_COUNT group
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_AQSP
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
!     Read aqueous reaction species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     Last Modified by MD White, PNNL, 8 December 2004.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM,ADUM,UNTS
      REAL(KIND=DP) VAR
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_AQSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Aqueous Species Card'
!
!---  Read number of aqueous species  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Aqueous Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPL)
      LSPL = MAX( LSPL,NSPL )
!
!---  First check for Molecular diffusion option  ---
!
      VARB = 'Aqueous Species Molecular Diffusion Option'
      IVR = INDEX( VARB,'  ')-1
      CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.0 ) THEN
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ENDIF
!
!---  Check for Pitzer activity coefficient option  ---
!
      CALL RD_DPR(ISTART,ICOMMA,CHDUM,SP_MDL)
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)

      CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
      IF( INDX.EQ.1 ) THEN
        VARB = 'Activity Coefficient Option: '
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'pitzer').NE.0 ) THEN
!
!---  Loop over the aqueous species  ---
!
        LMCG = 0
        LCAT = 0
        LANI = 0
        LNEU = 0
        DO 500 NSP = 1,NSPL
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Aqueous Species Name: '
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          VARB = 'Charge'
          VAR = 0.0
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          IF (VAR > 0) THEN
            LCAT = LCAT+1
          ELSEIF(VAR < 0) THEN
            LANI = LANI+1
          ELSE
            LNEU = LNEU+1
          ENDIF
          LMCG = MAX(LMCG,INT(ABS(VAR)))
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
 500    CONTINUE
        ENDIF
      ENDIF

      LNAF = LANI*(LANI-1)+1
      LNCF = LCAT*(LCAT-1)+1
      LNNC = MAX(LCAT,LNEU)
      LNNA = MAX(LNEU,LANI)
      LNNF = MAX(LNNC,LNNA)
      LNNF = LNNF*LNEU
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_AQSP group
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_BC
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
!     Read Boundary Conditions Card for number of boundary conditions
!     and number of boundary condition times.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 October 2002.
!     Last Modified by MD White, PNNL, 10 October 2002.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE BCV
      USE GLB_PAR
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*132 ADUM,BDUM,CDUM,FDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_BC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Boundary Conditions Card'
!
!---  Read number of boundary condition inputs  ---
!
      LBC = 1
      LBCIN = 1
      LBTM = 1
      NBC = 0
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Boundary Condition Inputs: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
      LBCIN = MAX( LBCIN,NLIN )
      LXYZG = 0
      DO 400 NB = 1, NLIN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---    Read boundary orientation  ---
!
        VARB = 'Boundary Condition Orientation: '
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          IS = 1
          IE = 1
          JS = 1
          JE = 1
          KS = 1
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
          NCH = INDEX(FDUM,'  ')-1
          KE = BUFFEREDREAD_NLINES(FDUM(1:NCH))
        ENDIF
!
!---    Loop over coupled flow and transport boundary types  ---
!
        DO 50 NBT = 1,LL
!
!---      Check for Shuttleworth-Wallace boundary conditions  ---
!
          VARB = 'Boundary Condition Type'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          IF( INDEX(BDUM(1:),'x-y-z').NE.0 .AND. &
            INDEX(BDUM(1:),'hydraulic gradient').NE.0 .OR. &
            INDEX(BDUM(1:),'seepage face').NE.0 )  LXYZG = 1
   50   CONTINUE
!
!---    Loop over solute and reactive species boundary types,
!       allowing for returns in input lines  ---
!
        DO 60 NBT = 1,((LSOLU*LC)+(LL*LR))
          VARB = 'Solute or Reactive Species Boundary Condition Type'
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
   60   CONTINUE
!
!---    Read number of reactive species in boundary
!       condition  ---
!
        NBCSPX = 0
        IF( LR.EQ.1 ) THEN
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NBCSPX)
          LSPBC = MAX( LSPBC,NBCSPX )
!
!---      Loop over reactive species, allowing for returns
!         in the input lines  ---
!
          DO 70 NSPX = 1,NBCSPX
            VARB = 'Boundary Condition Species Name'
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
   70     CONTINUE
        ENDIF
!
!---    Read and write boundary domain indices  ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        IF( INDEX(ADUM(1:),'file').EQ.0 ) THEN
          VARB = 'Boundary Condition Domain: '
          CALL RD_INT(ISTART,ICOMMA,CHDUM,IS)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,IE)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,JS)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,JE)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,KS)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,KE)
!
!---  Check boundary domain  ---
!
          IF( IS.GT.IE .OR. JS.GT.JE .OR. KS.GT.KE ) THEN
            INDX = 4
            CHMSG = 'Nonascending Boundary Condition Domain Indices'
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---  Read number of boundary times  ---
!
        VARB = 'Number of Boundary Condition Times: '
        CALL RD_INT(ISTART,ICOMMA,CHDUM,IBCMX)
        IF( IBCMX.LE.-3 ) THEN
          IBCCX = 1
          IBCMX = -IBCMX
        ELSEIF( IBCMX.GE.1 ) THEN
          IBCCX = 0
        ELSEIF( IBCMX.EQ.0 ) THEN
          INDX = 4
          CHMSG = 'No Boundary Condition Times'
          CALL WRMSGS( INDX )
        ELSE
          INDX = 4
          CHMSG = 'Number of Cyclic Boundary Conditions Times < 3'
          CALL WRMSGS( INDX )
        ENDIF
        LBTM = MAX( LBTM,IBCMX )
!
!---    Skip over the boundary condition variables and units  ---
!
        DO 100 NTM = 1,IBCMX
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
!
!---      Check for external boundary condition time file  ---
!
          IF( NTM.EQ.1 ) CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,CDUM)
          CALL LCASE( CDUM )
          IF( INDEX(CDUM(1:),'file').NE.0 ) GOTO 110
!
!---      Loop over reactive species inputs, allowing for
!         returns in input lines  ---
      
          DO 90 NSPX = 1,NBCSPX
            IF( NSPX.EQ.1 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
   90     CONTINUE
  100   CONTINUE
  110   CONTINUE
!
!---    Compute the number of boundary surfaces  ---
!
        NBC = NBC + (KE-KS+1)*(JE-JS+1)*(IE-IS+1)
        LBC = MAX( LBC,NBC )
  400 CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_BC group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
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
!     Fill character string ADUM with characters between commas.
!     Return 'null' for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 1992.
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) ADUM
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_CHR'
      IDFLTD = 0
      ICSNX = MAX( INDEX( SUBNMX,'  ' )-1,0 )
      ICSNO = ICSN
      ICSNS = MIN( ICSNX,1 ) + ICSNO
      ICSN = MIN( ICSNO+ICSNX,132 )
      SUBNM(ICSNS:ICSN) = SUBNMX
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
      NCH = INDEX( ADUM,'  ')-1
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing character string data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        INDX = 4
        CHMSG = 'Missing Character-String Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        IF( IDFLT .EQ. 0 ) THEN
          ADUM = 'null'
          NCH = 4
          IDFLTD = 1
        ENDIF
        ISTART = ICOMMA + 1
        ICOMMA = ISTART
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Translate character string into a character string  ---
!
        ADUM = ' '
        NCH = ISTOP-ISTART+1
        READ (CHDUM(ISTART:ISTOP), '(A)') ADUM(1:NCH)
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RD_CHR group  ---
!
!
      ICSN = ICSNO
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_CNEQ
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
!     Read conservation equations for reactions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     Last Modified by MD White, PNNL, 9 December 2004.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_CNEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Conservation Equations Card'
!
!---  Read number of conservation equations  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Conservation Equations'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NEQC)
      LEQC = MAX( LEQC,NEQC )
      LSPT = LSPT + LEQC
!
!---  Loop over the conservation equations  ---
!
      DO 100 NEQ = 1,NEQC
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Component Species Name: '
        CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        VARB = 'Number of Species in Conservation Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSEC )
        LSEC = MAX( LSEC,NSEC )
!
!---    Loop over the conservation species allowing for
!       returns in the input  ---
!
        DO 90 NSP = 1,NSEC
          VARB = 'Conservation-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          VARB = 'Conservation-Equation Species Coefficient: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
   90   CONTINUE
  100 CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_CNEQ group
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_DPR( ISTART,ICOMMA,CHDUM,VAR )
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
!     Fill double precision variable VAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, November 1992.
!     Last Modified by MD White, Battelle, PNL, November 3, 1994.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*6 FORM1
      CHARACTER*7 FORM2
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1,FORM2
      DATA FORM1 /'(D .0)'/
      DATA FORM2 /'(D  .0)'/
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_DPR'
      IDFLTD = 0
      ICSNX = MAX( INDEX( SUBNMX,'  ' )-1,0 )
      ICSNO = ICSN
      ICSNS = MIN( ICSNX,1 ) + ICSNO
      ICSN = MIN( ICSNO+ICSNX,132 )
      SUBNM(ICSNS:ICSN) = SUBNMX
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Find next comma  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing real data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        INDX = 4
        CHMSG = 'Missing Real Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        IF( IDFLT .EQ. 0 ) THEN
          VAR = 0.D+0
          IDFLTD = 1
        ENDIF
        ISTART = ICOMMA + 1
        ICOMMA = ISTART
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for scientific notation  ---
!
        IEXP = ISTART-1
        IF( INDEX( CHDUM(ISTART:ISTOP),'e' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'e' )+ISTART-1
        ELSEIF( INDEX( CHDUM(ISTART:ISTOP),'d' ).NE.0 ) THEN
          IEXP = INDEX( CHDUM(ISTART:ISTOP),'d' )+ISTART-1
        ENDIF
        IPER = INDEX( CHDUM(ISTART:ISTOP),'.' )+ISTART-1
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          IF( N.EQ.IEXP .OR. N.EQ.IPER ) GOTO 120
          NC = ICHAR(CHDUM(N:N))
          IF( ( N.EQ.ISTART .OR. N.EQ.IEXP+1 ) .AND. &
          ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) THEN
            INDX = 4
            CHMSG = 'Real Format: Nonnumeric Character: ' // &
            VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
            CALL WRMSGS(INDX)
          ENDIF
  120   CONTINUE
!
!---  Translate character string into a double precision real  ---
!
        NCHR = ISTOP-ISTART+1
        IF( NCHR .LT. 10 ) THEN
          WRITE( FORM1(3:3), '(I1)' ) NCHR
          READ (CHDUM(ISTART:ISTOP), FORM1 ) VAR
        ELSEIF( NCHR .LT. 100 ) THEN
          WRITE( FORM2(3:4), '(I2)' ) NCHR
          READ (CHDUM(ISTART:ISTOP), FORM2 ) VAR
        ELSE
          INDX = 4
          CHMSG = 'Excessive Length Real Record: ' // &
          VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
          CALL WRMSGS( INDX )
        ENDIF
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RD_DPR group  ---
!
      ICSN = ICSNO
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_EQEQ
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
!     Read equilibrium equations for reactions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     Last Modified by MD White, PNNL, 9 December 2004.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*128 ADUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_EQEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Equilibrium Equations Card'
!
!---  Read number of equilibrium equations  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Equations'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NEQE)
      LEQE = MAX( LEQE,NEQE )
!
!---  Loop over the equilibrium equations  ---
!
      DO 100 NEQ = 1,NEQE
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Species in Equilibrium Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSEE )
        LSEE = MAX( LSEE,NSEE )
!
!---    Loop over the equilibrium-equation species allowing for
!       returns in the input  ---
!
        DO 90 NSP = 1,NSEE
          VARB = 'Equilibrium-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          IF( NSP.GT.1 ) THEN
            VARB = 'Equilibrium-Equation Species Exponent: '
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
   90   CONTINUE
  100 CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_EQEQ group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_EQRC
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
!     Read equilibrium reactions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     Last Modified by MD White, PNNL, 9 December 2004.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_EQRC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Equilibrium Reactions Card'
!
!---  Read number of equilibrium reactions  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Equilibrium Reactions'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NRCE)
      LRCE = MAX( LRCE,NRCE )
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_EQRC group
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_EXSP
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
!     Read exchanged species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     Last Modified by MD White, PNNL, 8 December 2004.
!     $Id: step.F,v 1.2 2011/09/09 17:15:37 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_EXSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Exchanged Species Card'
!
!---  Read number of exchanged species  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchanged Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPE)
      LSPE = MAX( LSPE,NSPE )
!
!---  Read number of exchange sites  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Exchange Sites'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NESITE)
      LESITE = MAX( LESITE,NESITE )
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_EXSP group
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_INPL( CHDUM )
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
!     Read input line.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, November 10, 1999.
!     Last Modified by MD White, PNNL, November 10, 1999.
!     Last Modified by MD White, PNNL, 29 September 2002.
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_INPL'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Skip input lines that begin with '#' or '!'  ---
!
   10 READ(IRD,'(A)') CHDUM
      IF( CHDUM(1:1).EQ.'#' .OR. CHDUM(1:1).EQ.'!' ) GOTO 10
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_INPL group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_INT( ISTART,ICOMMA,CHDUM,IVAR )
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
!     Fill integer variable IVAR with data between commas.
!     Return default value or zero for null entries.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNNL, November 1992.
!     Last Modified by MD White, Battelle, PNNL, November 8, 2000.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*(*) CHDUM
      CHARACTER*4 FORM1
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM1
      DATA FORM1 /'(I )'/
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_INT'
      IDFLTD = 0
      ICSNX = MAX( INDEX( SUBNMX,'  ' )-1,0 )
      ICSNO = ICSN
      ICSNS = MIN( ICSNX,1 ) + ICSNO
      ICSN = MIN( ICSNO+ICSNX,132 )
      SUBNM(ICSNS:ICSN) = SUBNMX
      ICD = INDEX( CARD,'  ')-1
      IVR = INDEX( VARB,'  ')-1
!
!---  End of card record error ---
!
      IF( CHDUM(1:1) .EQ. '~' ) THEN
        INDX = 4
        CHMSG = 'End of Card Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
      ENDIF
!
!---  Read numbers between commas  ---
!
      ICOMMA = INDEX (CHDUM(ISTART:), ',') + ISTART - 1
      ISTOP = ICOMMA
  100 CONTINUE
!
!---  Comma not found, missing integer data  ---
!
      IF( ISTOP.LT.ISTART ) THEN
        INDX = 4
        CHMSG = 'Missing Integer Record: ' // VARB(1:IVR)
        CALL WRMSGS( INDX )
!
!---  Null entry  ---
!
      ELSEIF( ISTOP.EQ.ISTART ) THEN
        IF( IDFLT .EQ. 0 ) THEN
          IVAR = 0
          IDFLTD = 1
        ENDIF
        ISTART = ICOMMA + 1
        ICOMMA = ISTART
!
!---  Characters between commas  ---
!
      ELSE
!
!---  Eliminate leading blank spaces  ---
!
        IF( ICHAR(CHDUM(ISTART:ISTART)).EQ.32 ) THEN
          ISTART = ISTART+1
          GOTO 100
        ENDIF
!
!---  Eliminate trailing blank spaces  ---
!
        ISTOP = ISTOP-1
  110   CONTINUE
        IF( ICHAR(CHDUM(ISTOP:ISTOP)).EQ.32 ) THEN
          ISTOP = ISTOP-1
          GOTO 110
        ENDIF
!
!---  Check for non-numerical characters  ---
!
        DO 120 N = ISTART,ISTOP
          NC = ICHAR(CHDUM(N:N))
          IF( N.EQ.ISTART .AND. ( NC.EQ.43 .OR. NC.EQ.45 ) ) GOTO 120
          IF( NC.LT.48 .OR. NC.GT.57 ) THEN
            INDX = 4
            CHMSG = 'Integer Format: Nonnumeric Character: ' // &
            VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
            CALL WRMSGS(INDX)
          ENDIF
  120   CONTINUE
!
!---  Translate character string into an integer  ---
!
        NCHR = ISTOP-ISTART+1
        IF( NCHR.LT.10 ) THEN
          WRITE( FORM1(3:3),'(I1)' ) NCHR
          READ( CHDUM(ISTART:ISTOP),FORM1 ) IVAR
        ELSE
          INDX = 4
          CHMSG = 'Excessive Length Integer Record: ' // &
          VARB(1:IVR) // ': ' // CHDUM(ISTART:ISTOP)
          CALL WRMSGS( INDX )
        ENDIF
        ISTART = ICOMMA + 1
      ENDIF
      IDFLT = 0
!
!---  End of RD_INT group  ---
!
      ICSN = ICSNO
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_GSSP
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
!     Read gas species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 16 August 2005.
!     Last Modified by MD White, PNNL, 16 August 2005.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_GSSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Gas Species Card'
!
!---  Read number of equilibrium reactions  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Gas Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPG)
      LSPG = MAX( LSPG,NSPG )
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_GSSP group
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_KNEQ
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
!     Read kinetic equations for reactions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     Last Modified by MD White, PNNL, 9 December 2004.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_KNEQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Kinetic Equations Card'
!
!---  Read number of kinetic equations  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Equations'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NEQK)
      LEQK = MAX( LEQK,NEQK )
      LSPT = LSPT + LEQK
!
!---  Loop over the kinetic reactions  ---
!
      DO 100 NEQ = 1,NEQK
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Component Species Name'
        CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        VARB = 'Number of Species in Kinetic Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSEK )
        LSEK = MAX( LSEK,NSEK )
!
!---    Loop over the kinetic-equation species allowing for
!       returns in the input  ---
!
        DO 80 NSP = 1,NSEK
          VARB = 'Kinetic-Equation Species Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          VARB = 'Kinetic-Equation Species Coefficient: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
   80   CONTINUE
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Number of Reactions in Kinetic Equation'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NREK )
        LREK = MAX( LREK,NREK )
!
!---    Loop over the kinetic-equation kinetic reactions
!        allowing for returns in the input  ---
!
        DO 90 NSP = 1,NREK
          VARB = 'Kinetic-Equation Kinetic-Reaction Name: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
          VARB = 'Kinetic-Equation Kinetic-Reaction Coefficient: '
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
   90   CONTINUE
  100 CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_KNEQ group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_KNRC
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
!     Read kinetic reactions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 9 December 2004.
!     Last Modified by MD White, PNNL, 9 December 2004.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 GDUM
      CHARACTER*128 ADUM,FDUM
      CHARACTER*512 CHDUM
      LOGICAL FLG_CHK
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_KNRC'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Kinetic Reactions Card'
!
!---  Read number of equilibrium reactions  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Kinetic Reactions'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NRCK)
      LRCK = MAX( LRCK,NRCK )
      LCKN = 1
!
!---  Loop over the kinetic reactions  ---
!
      DO 500 NRC = 1,NRCK
        JCX = 0
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Kinetic Reaction Name'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        VARB = 'Kinetic Reaction Type'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'dissolu').NE.0 .OR.  &
        INDEX(ADUM(1:),'precip').NE.0 .OR. &
       INDEX(ADUM(1:),'tst').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
            IRCKTX = 5
            IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
              IRCKTX = 6
            ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
              IRCKTX = 7
            ENDIF
          ELSE
            IRCKTX = 10
            IF( INDEX(ADUM(1:),'toward products').NE.0 ) THEN
              IRCKTX = 11
            ELSEIF( INDEX(ADUM(1:),'toward reactants').NE.0 ) THEN
              IRCKTX = 12
            ENDIF
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'multi').NE.0 .AND. &
        INDEX(ADUM(1:),'rate').NE.0 ) THEN
          IRCKTX = 20
        ELSEIF( INDEX(ADUM(1:),'forward').NE.0 .OR.  &
        INDEX(ADUM(1:),'backward').NE.0 ) THEN
          IRCKTX = 1
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 .AND.  &
       INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKTX = 2
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND. &
        INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKTX = 3
        ELSEIF( INDEX(ADUM(1:),'sorption').NE.0 .AND. &
        INDEX(ADUM(1:),'langmuir').NE.0 ) THEN
          IRCKTX = 13
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 .AND. &
        INDEX(ADUM(1:),'valocchi').NE.0 ) THEN
          IRCKTX = 4
        ELSEIF( INDEX(ADUM(1:),'monod').NE.0 ) THEN
          IRCKTX = 22
        ELSEIF( INDEX(ADUM(1:),'biomass').NE.0 ) THEN
          IRCKTX = 24
        ELSEIF( INDEX(ADUM(1:),'lognormal').NE.0 .AND. &
         INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKTX = 41
        ELSEIF( INDEX(ADUM(1:),'dualdomain').NE.0 .AND. &
         INDEX(ADUM(1:),'liu').NE.0 ) THEN
          IRCKTX = 42
        ENDIF
        IF ( INDEX(ADUM(1:),'w/coef').NE.0 ) THEN
          LMC = LFX*LFY*LFZ
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        ELSE
          LMC = 1
        ENDIF
!
!---    Mineral  ---
!
        IF( (IRCKTX.GE.10 .AND. IRCKTX.LE.12) .OR. &
        (IRCKTX.GE.5 .AND. IRCKTX.LE.7) .OR. &
        IRCKTX.EQ.20 ) THEN
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          VARB = 'Mineral Name'
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        ENDIF
!
!---    Multi-rate mineral  ---
!
        IF( IRCKTX.EQ.20 ) GOTO 400
!
!---    Number of reactants in kinetic reaction  ---
!
        VARB = 'Number of Reactants in Kinetic Reaction'
        CALL RD_INT( ISTART,ICOMMA,CHDUM,NSPRX )
!
!---    Loop over the kinetic reaction reactants allowing for
!       returns in the input  ---
!
        DO 10 NSP = 1,NSPRX
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
          VARB = 'Kinetic-Reaction Reactants Name: '
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Skip kinetic reaction reactant stoichiometric coefficient
!         for Monod and Biomass kinetics  ---
!
          IF( IRCKTX.NE.22 .AND. IRCKTX.NE.24 ) THEN
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            JCX = JCX+1
            VARB = 'Kinetic-Reaction Reactant Stoichiometric ' //  &
            'Coefficient: '
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
   10   CONTINUE
!
!---    Skip kinetic reaction products
!       for Monod and Biomass kinetics  ---
!
        IF( IRCKTX.NE.22 .AND. IRCKTX.NE.24 ) THEN
          VARB = 'Number of Products in Kinetic Reaction'
          CALL RD_INT( ISTART,ICOMMA,CHDUM,NSPPX )
!
!---      Loop over the kinetic reaction products allowing for
!         returns in the input  ---
!
          DO 20 NSP = 1,NSPPX
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            VARB = 'Kinetic-Reaction Products Name: '
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
            JCX = JCX+1
            VARB = 'Kinetic-Reaction Product Stoichiometric ' //  &
            'Coefficient: '
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
   20     CONTINUE
        ENDIF
!
!---    Read kinetic reaction parameters  ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---    TST type reactions  ---
!
        IF( (IRCKTX.GE.10 .AND. IRCKTX.LE.12) .OR. &
        (IRCKTX.GE.5 .AND. IRCKTX.LE.7) ) THEN
!
!---      Read forward dissolution-precipitation
!         reference reaction rate  ---
!
          JCX = JCX+1
          VARB = 'Kinetic Reaction Reference Rate'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read activation energy  ---
!
          JCX = JCX+1
          VARB = 'Kinetic Reaction Activation Energy'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read forward dissolution-precipitation
!         reference reaction temperature  ---
!
          JCX = JCX+1
          VARB = 'Kinetic Reaction Reference Temperature'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read dissolution-precipitation
!         pH exponent  ---
!
          IF( IRCKTX.GE.5 .AND. IRCKTX.LE.9 ) THEN
            JCX = JCX+1
            VARB = 'Kinetic Reaction pH Exponent'
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
             CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
          ENDIF
!
!---      Read equilibrium constant function coefficients
!         where, log(K) = b1*ln(T) + b2 + b3*T + b4/T + b5/(T^2)
!         and the equilibrium constant relates the aqueous
!         activity-molality product, gas fugacity, and
!         mineral activity  ---
!
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          DO 300 M = 1,5
!
!---        Read equilibrium constant function coefficients  ---
!
            JCX = JCX+1
            VARB = 'Equilibrium Reaction Constant Coefficient'
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
              CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
  300     CONTINUE
!
!---    Forward-backward type reactions  ---
!
        ELSEIF( IRCKTX.EQ.1 ) THEN
!
!---      Read forward reaction rate  ---
!
          JCX = JCX+1
          VARB = 'Forward Kinetic Reaction Rate Exponent'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read backward reaction rate  ---
!
          JCX = JCX+1
          VARB = 'Backward Kinetic Reaction Rate Exponent'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Valocchi-Monod type reactions  ---
!
        ELSEIF( IRCKTX.EQ.2 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // &
          'Half-Saturation Constant for Donor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read half-saturation constant for acceptor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // &
          'Half-Saturation Constant for Acceptor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read maximum specific rate of substrate utilization  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // & 
           'Maximum Specific Rate of Substrate Utilization'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Valocchi-Sorption type reactions  ---
!
        ELSEIF( IRCKTX.EQ.3 ) THEN
!
!---      Read mass transfer coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Sorption Kinetic Reaction: ' // &
          'Mass Transfer Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read distribution coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Sorption Kinetic Reaction: ' // &
          'Distribution Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Valocchi-Biomass type reactions  ---
!
        ELSEIF( IRCKTX.EQ.4 ) THEN
!
!---      Read half-saturation constant for donor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // &
          'Half-Saturation Constant for Donor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read half-saturation constant for acceptor  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // &
          'Half-Saturation Constant for Acceptor'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read maximum specific rate of substrate utilization  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // &
            'Maximum Specific Rate of Substrate Utilization'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read microbial yield coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // &
          'Microbial Yield Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Read first-order micobial decay coefficient  ---
!
          JCX = JCX+1
          VARB = 'Valocchi-Monod Kinetic Reaction: ' // &
          'First-Order Microbial Decay Coefficient'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Monod type reactions  ---
!
        ELSEIF( IRCKTX.EQ.22 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 320 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Monod Kinetic Reaction: ' // &
            'Half-Saturation Constant'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
              CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
  320     CONTINUE
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---      Read maximum specific rate of substrate utilization  ---
!
          VARB = 'Monod Kinetic Reaction: ' // &
          'Maximum Specific Rate of Reactant Utilization'
          JCX = JCX+1
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---    Biomass type reactions  ---
!
        ELSEIF( IRCKTX.EQ.24 ) THEN
!
!---      Loop over reactants, less one  ---
!
          DO 330 NSP = 1,NSPRX-1
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read half-saturation constant  ---
!
            VARB = 'Biomass Kinetic Reaction: ' // &
            'Half-Saturation Constant'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
              CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Allow for returns in input lines  ---
!
            CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
            IF( INDX.EQ.0 ) THEN
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
            ENDIF
!
!---        Read maximum specific rate of substrate utilization  ---
!
            VARB = 'Biomass Kinetic Reaction: ' // &
            'Maximum Specific Rate of Reactant Utilization'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
              CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
  330     CONTINUE
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---      Read microbial yield coefficient  ---
!
          VARB = 'Biomass Kinetic Reaction: ' // &
          'Microbial Yield Coefficient'
          JCX = JCX+1
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
!
!---      Allow for returns in input lines  ---
!
          CALL CHK_DPR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.EQ.0 ) THEN
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            CALL LCASE( CHDUM )
            ISTART = 1
          ENDIF
!
!---    Read first-order micobial decay coefficient  ---
!
        VARB = 'Biomass Kinetic Reaction: ' // &
        'Microbial Decay Coefficient'
        JCX = JCX+1
        IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
          T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
          FILEREAD = .TRUE.
        ELSE
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
        ENDIF
        CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
        ELSEIF( IRCKTX.EQ.41 ) THEN
!
!---      Read rate from lognormal distribution
!
          VARB = 'Liu Multi Rate kin. Reac.: ' // &
           'Rate constant'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read kinetic site density  ---
!
          VARB = 'Liu Multi Rate Kin. Reac.: ' // &
           'kinetic site density'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read pore ratio  ---
!
          VARB = 'Liu Multi Rate Kin. Reac.: ' // &
           'Pore Ratio.'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Read logK1 for the first sorped species on the site  ---
!
          VARB = 'Liu Multi Rate Kin. Reac.: ' // &
           'Log K1.'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---      Read logK2 for the second sorped species on the site ---
!
          VARB = 'Liu Multi Rate Kin. Reac.: ' // &
           'Log K2.'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
!
!---    Liu's dual domain rate type reactions  ---
!
        ELSEIF( IRCKTX.EQ.42 ) THEN
!
!---      Mass transfer rate
!
          VARB = 'Liu Dual Domain Kin. Reac.: ' // &
           'Rate constant'
          IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
            T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
            FILEREAD = .TRUE.
          ELSE
            CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
          ENDIF
          CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---      Read pore ratio (immobile pore/mobile pore) ---
!
          VARB = 'Liu Multi Rate Kin. Reac.: ' // &
           'Pore Ratio.'
          CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
      ENDIF
!
!---    Multi-rate mineral  ---
!
  400   CONTINUE
        IF( IRCKTX.EQ.20 ) THEN
          VARB = 'Number of Mechanisms in Kinetic Reaction'
          JCX = 3
          CALL RD_INT( ISTART,ICOMMA,CHDUM,NKRMX )
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
!
!---      Loop over mechanisms  ---
!
          DO 490 NKRM = 1,NKRMX
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
!
!---        Mechanism reference reaction rate  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Rate'
            JCX = JCX+1
!
!---        Mechanism reference reaction rate  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Rate'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
              CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Mechanism activation energy  ---
!
            VARB = 'Mechanism Kinetic Reaction Activation Energy'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
              CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Mechanism reference reaction temperature  ---
!
            VARB = 'Mechanism Kinetic Reaction Reference Temperature'
            JCX = JCX+1
            IF( INDEX(CHDUM(ISTART:ISTART+5),'file') /= 0 ) THEN
              T_OK = FILEEXISTS( CHDUM,ISTART,ICOMMA )
              FILEREAD = .TRUE.
            ELSE
              CALL RD_DPR(ISTART,ICOMMA,CHDUM,VAR)
            ENDIF
            CALL RD_CHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---        Number of species in mechanism  ---
!
            T_OK = BUFFEREDREAD_GETLINE(CHDUM)
            VARB = 'Number of Species in Mechanism'
            CALL RD_INT( ISTART,ICOMMA,CHDUM,NKRSX )
            JCX = JCX+NKRSX
  490     CONTINUE
        ENDIF
        NCKN = LFX*LFY*LFZ 
        LSPK = MAX( LSPK,JCX )
        LCKN = MAX( NCKN,LCKN )
  500 CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_KNRC group
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_LRP
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
!     Read Aqueous Relative Permeability Function Parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 November 2002.
!     Last Modified by MD White, PNNL, 26 November 2002.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,RDUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_LRP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Aqueous Relative Permeability Card'
!
!---  Read input line for rock/soil type ---
!
  10  CONTINUE
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'#' .OR. CHDUM(1:1).EQ.'!' )GOTO 10
      IF( CHDUM(1:1).EQ.' ' .OR. CHDUM(1:1).EQ.'~' )GOTO 500
      VARB = 'Rock/Soil Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
        IROCK = 1
      ENDIF
!
!---  Read aqueous relative permeability function for
!     tabular forms  ---
!
      VARB = 'Aqueous Relative Permeability Function'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---  Tabular (relative permeability versus liquid saturation)  ---
!
      IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
        IF( INDEX( ADUM(1:),'content' ).NE.0 ) THEN
          IRPLX = 10
        ELSEIF( INDEX( ADUM(1:),'head' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            IRPLX = 14
          ELSE
            IRPLX = 12
          ENDIF
        ELSE
          IRPLX = 10
        ENDIF
        VARB = 'Number of Tabular Entries'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
        DO 230 NL = 1,NLIN
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
 230    CONTINUE
        IF( IJK.GT.0 ) THEN
          NTBL = LFD*NLIN
        ELSE
          NTBL = NLIN
        ENDIF
        LTBL = LTBL + NTBL
!
!---  Polynomial function  ---
!
      ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
        IRPLX = 19
        VARB = 'Number of Polynomial Function Pieces'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NPOLY)
        LPOLYN = MAX( LPOLYN,NPOLY )
        DO 240 NP = 1,NPOLY
          ISTART = 1
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          VARB = 'Number of Polynomial Coefficients'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NCOEF)
          LPOLYC = MAX( LPOLYC,NCOEF+4 )
 240    CONTINUE
      ENDIF
!
!---  Continue reading rock/soil type names for a pattern match  ---
!
      GOTO 10

 500  CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_LRP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SDSP
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
!     Read aqueous reaction species.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     Last Modified by MD White, PNNL, 8 December 2004.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE FILES
      USE GLB_PAR
      USE REACT
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_SDSP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Solid Species Card'
!
!---  Read number of aqueous species  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solid Species'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSPS)
      LSPS = MAX( LSPS,NSPS )
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_SDSP group
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SF
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
!     Read Surface Flux Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 27 November 2002.
!     Last Modified by MD White, PNNL, 27 November 2002.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE OUTPU
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM,ADUM,BDUM,FDUM,GDUM
      LOGICAL FLG_CHK
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_SF'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Surface Flux Card'
!
!---  Read surface flux card information  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Surface Flux Inputs'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSF)
      LSF = MAX( LSF,NSF )
      NS = 0
      NSFF = 0
      NC = 0
      NNC = 0
      IF( NSF > 0 )THEN
        DO
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          CALL CHK_INT(ISTART,ICOMMA,CHDUM,INDX)
          IF( INDX == 1 ) THEN
            NSFF = NSFF+1
            CYCLE
          ENDIF
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'solute') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          END IF
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          IF( INDEX(ADUM(1:),'file') /= 0 ) THEN
            CALL RD_CHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
            NCHF = INDEX(FDUM,'  ')-1
!       
!---        Check that surface flux domain file exists  ---
!       
            INQUIRE( FILE=FDUM(1:NCHF), FORM=GDUM, EXIST=FLG_CHK )
            IF( .NOT.FLG_CHK ) THEN
              INDX = 4
              CHMSG = 'Surface-Flux-Domain File: ' &
              // FDUM(1:NCHF)
              CALL WRMSGS( INDX )
            ELSEIF( GDUM.EQ.'UNFORMATTED' ) THEN
              INDX = 4
              CHMSG = 'Unformatted Surface-Flux-Domain File: ' &
              // FDUM(1:NCHF)
              CALL WRMSGS( INDX )
            ENDIF
            NC = BUFFEREDREAD_NLINES(FDUM(1:NCHF))
            NNC = NC + NNC 
            LSFDOM = MAX( LSFDOM,NNC )
          END IF
          NS = NS+1
          IF( NS >= NSF ) EXIT
        END DO
      ENDIF
!
!---  All surface flux files are named  ---
!
      IF( NSFF.EQ.NSF ) LSF = LSF+1
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_SF group
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SOLU
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
!     Read Solution Control Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 21 November 2002.
!     Last Modified by MD White, PNNL, 21 November 2002.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_SOLU'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Solution Control Card'
!
!---  Read Execution Option  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Execution Option'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      IF( INDEX(ADUM(1:),'normal').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          IEO = 21
        ELSEIF( INDEX(ADUM(1:),'dynamic').NE.0 ) THEN
          IEO = 11
        ELSE
          IEO = 1
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'restart').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'no flow').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'zero solutes').NE.0 ) THEN
            IEO = 24
          ELSE
            IEO = 22
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'dynamic').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'zero solutes').NE.0 ) THEN
            IEO = 14
          ELSE
            IEO = 12
          ENDIF
       ELSE
          IF( INDEX(ADUM(1:),'zero solutes').NE.0 ) THEN
            IEO = 4
          ELSE
            IEO = 2
          ENDIF
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'initial').NE.0 ) THEN
        IEO = 3
      ENDIF
!
!---  Equation switch parameters  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Operational Mode'
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      LL = 1
      IF( INDEX(ADUM(1:),'transport').NE.0 ) LC = 1
      IF( INDEX(ADUM(1:),'eckechem').NE.0 )  LR = 1
      IF( INDEX(ADUM(1:),'5512').NE.0 ) LC = 1
      IF( INDEX(ADUM(1:),'ice').NE.0 ) LFW = 1
!
!---  Water (H2O) Operational Mode  ---
!
      IF( (INDEX(ADUM(1:),'water').NE.0 .OR. &
        INDEX(ADUM(1:),'h2o').NE.0) ) THEN
        IOM = 1
        IF( INDEX(ADUM(1:),'5512').NE.0 ) THEN
          IOM = 5512
        ENDIF
        LUK = 1
!
!---  Fluid Operational Mode  ---
!
      ELSEIF( INDEX(ADUM(1:),'fluid').NE.0 ) THEN
        IOM = 1
        LUK = 1
      ENDIF
!
!---  Number of execution periods parameter  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Execution Periods'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,LEPD)
      LEPD = MAX( 1,LEPD )
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_SOLU group
!
      RETURN
      END
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SP
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
!     Read Saturation Function Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 25 November 2002.
!     Last Modified by MD White, PNNL, 25 November 2002.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GRID
      USE GLB_PAR
      USE PORMED
      USE SOLTN
      USE TABL
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM,RDUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_SP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Saturation Function Card'
!
!---  Loop over the rock/soil saturation information lines  ---
!
  10  CONTINUE
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      IF( CHDUM(1:1).EQ.'#' .OR. CHDUM(1:1).EQ.'!' )GOTO 10
      IF( CHDUM(1:1).EQ.' ' .OR. CHDUM(1:1).EQ.'~' )GOTO 500
      VARB = 'Saturation Function: Rock Name: '
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,RDUM)
!
!---  IJK, KIJ, or JKI indexing  ---
!
      IF( INDEX(RDUM(1:),'indexing').NE.0 ) THEN
        IF( INDEX(RDUM,'ijk').NE.0 ) THEN
          IJK = 1
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Indexing Option' // RDUM(1:NCH)
          CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Read saturation/capillary pressure function for
!     tabular forms  ---
!
      VARB = 'Saturation Function Type: '
      CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      ISCHRX = 0
      IF( INDEX(ADUM(1:),'tabular').NE.0 ) THEN
        IF( INDEX( ADUM(1:),'spline' ).NE.0 ) THEN
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            ISCHRX = 13
          ELSE
            ISCHRX = 11
          ENDIF
        ELSE
          IF( INDEX( ADUM(1:),'log' ).NE.0 ) THEN
            ISCHRX = 12
          ELSE
            ISCHRX = 10
          ENDIF
        ENDIF
      ELSEIF( INDEX(ADUM(1:),'polynomial').NE.0 ) THEN
        ISCHRX = 19
      ENDIF
!
!---  Tabular  ---
!
      IF( ISCHRX.GE.10 .AND. ISCHRX.LE.13 ) THEN
        VARB = 'Number of Table Entries'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
        IF( NLIN.LT.2 ) THEN
          INDX = 4
          CHMSG = 'Saturation Invalid Table'
          CALL WRMSGS( INDX )
        ENDIF
        DO 230 NL = 1,NLIN
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
 230    CONTINUE
        IF( IJK.GT.0 ) THEN
          NTBL = LFD*NLIN
        ELSE
          NTBL = NLIN
        ENDIF
        LTBL = LTBL + NTBL
!
!---  Polynomial  ---
!
      ELSEIF( ISCHRX.EQ.19 ) THEN
        VARB = 'Number of Polynomial Function Pieces'
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NPOLY)
        LPOLYN = MAX( LPOLYN,NPOLY )
        DO 240 NP = 1,NPOLY
          ISTART = 1
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          VARB = 'Number of Polynomial Coefficients'
          CALL RD_INT(ISTART,ICOMMA,CHDUM,NCOEF)
          LPOLYC = MAX( LPOLYC,NCOEF+4 )
 240    CONTINUE
      ENDIF
!
!---  Read next rock/soil type or scaling group  ---
!
      GOTO 10
 500  CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_SP group
!
      RETURN
      END

!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_SR
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
!     Read Source Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 10 October 2002.
!     Last Modified by MD White, PNNL, 10 October 2002.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM,CHDUMX
      CHARACTER*128 ADUM,BDUM,FDUM,FMDUM
      LOGICAL FCHK
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_SR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Source Card'
!
!---  Read number of source inputs  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Sources: '
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NSR)
      NSRX = 0
      DO 200 NS = 1,NSR
!
!---    Read source type, domain, and number of times  ---
!
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        ISRTX = 0
!
!---    Read source type  ---
!
        VARB = 'Source Type'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'z-dir').NE.0 .AND.&
         INDEX(ADUM(1:),'multi-screened').NE.0 .AND.&
         INDEX(ADUM(1:),'well').NE.0 ) ISRTX = 22      
        IF( INDEX(ADUM(1:),'mass-rate').NE.0 .AND. &
        INDEX(ADUM(1:),'well').NE.0 ) ISRTX = 25
        IF( INDEX(ADUM(1:),'solute').NE.0 .AND. &
        INDEX(ADUM(1:),'inventory').NE.0 ) ISRTX = -(3*LSOLU)
!
!---    CO2 injection mass-rate well source, read
!       screened intervals  ---
!
        IF( ISRTX == 22 .OR. ISRTX == 25 ) THEN
!
!---      Source type option  ---
!
          VARB = 'Source Type Option'
          CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
!
!---      Check for integer  ---
!
          CALL CHK_INT(ISTART,ICOMMA,CHDUM,INDX)
          IF( INDX == 1 ) THEN
            VARB = 'Well I Index'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,IS)
            VARB = 'Well J Index'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,JS)
            VARB = 'Number of Well Screen Intervals'
            CALL RD_INT(ISTART,ICOMMA,CHDUM,NWSI)
            NC = 0
            DO NWS = 1,NWSI
              VARB = 'Well-Screen Lower K Index'
              CALL RD_INT(ISTART,ICOMMA,CHDUM,KS)
              VARB = 'Well-Screen Upper K Index'
              CALL RD_INT(ISTART,ICOMMA,CHDUM,KE)
              NC = NC + ABS(KE-KS) + 1
            END DO
            NSRX = NSRX+1
            LWSI = MAX( LWSI,NC )
!
!---      Open and read external file of screened well indices  ---
!
          ELSE
            VARB = 'Screened-Well Indice and Area File Name'
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,FDUM)
            ISX = ISTART
            ICX = ICOMMA
            NCH = INDEX(FDUM,'  ')-1
            INQUIRE( FILE=FDUM(1:NCH), FORM=FMDUM, EXIST=FCHK )
            IF( .NOT.FCHK ) THEN
              INDX = 4
              CHMSG = 'Missing Screened-Well Indice and Area File: ' //&
               FDUM(1:NCH)
               CALL WRMSGS( INDX )
            ELSEIF( FDUM.EQ.'unformatted' ) THEN
              INDX = 4
              CHMSG = 'Screened-Well Indice and Area File Format: ' // &
               FDUM(1:NCH)
               CALL WRMSGS( INDX )
            ENDIF
            NWSI = BUFFEREDREAD_FIRSTLINE(FDUM(1:NCH))
            LWSI = MAX( LWSI,NWSI )
          ENDIF
        ELSE
!
!---      Skip over character string inputs for
!         source type options and solute sources  ---
!
          DO
            CALL CHK_INT(ISTART,ICOMMA,CHDUM,INDX)
            IF( INDX == 1 ) EXIT
            VARB = 'Source Type Option'
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          END DO
!
!---      Read source domain indices  ---
!
          VARB = 'Source Domain Index: '
          ISX = ISTART
          CALL RD_INT(ISTART,ICOMMA,CHDUM,IS)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,IE)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,JS)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,JE)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,KS)
          CALL RD_INT(ISTART,ICOMMA,CHDUM,KE)
          ICX = ISTART
!
!---      Check for ill-defined source domains  ---
!
          IF( IS.LT.1 .OR. IE.LT.1 .OR.IE.LT.IS ) THEN
            INDX = 4
            CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
            CALL WRMSGS( INDX )
          ENDIF
          IF( JS.LT.1 .OR. JE.LT.1 .OR. JE.LT.JS ) THEN
            INDX = 4
            CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
            CALL WRMSGS( INDX )
          ENDIF
          IF( KS.LT.1 .OR. KE.LT.1 .OR. KE.LT.KS ) THEN
            INDX = 4
            CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
            CALL WRMSGS( INDX )
          ENDIF
!
!---      Define a unique source input for each node 
!         in the domain for the solute inventory source  ---
!
          IF( ISRTX.EQ.-(3*LSOLU) ) THEN
            NSRX = NSRX+(IE-IS+1)*(JE-JS+1)*(KE-KS+1)
          ELSE
            NSRX = NSRX+1
          ENDIF
        ENDIF
!
!---    Read number of source times  ---
!
        VARB = 'Number of Source Times: '
        CALL RD_INT(ISTART,ICOMMA,CHDUM,NSTM)
        LSTM = MAX( LSTM,NSTM )
!
!---    Solute injected with well gas  ---
!
        IF( ISRTX >= 22 .AND. ISRTX <= 27 .AND. LC.NE.0 ) THEN
          CALL CHK_CHR( ISTART,ICOMMA,CHDUM,INDX )
          IF( INDX.NE.0 ) THEN
            VARB = 'Solute Source Type'
            CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
            IF( INDEX(ADUM(1:),'solute').NE.0 ) THEN          
              T_OK = BUFFEREDREAD_GETLINE(CHDUM)
              CALL LCASE( CHDUM )
              ISTART = 1
              VARB = 'Number of Solutes'
              CALL RD_INT(ISTART,ICOMMA,CHDUM,NSOLSRX)
              LSOLSR = MAX( NSOLSRX,LSOLSR )
            ENDIF
          ENDIF
        ENDIF
!
!---    Skip over source variables  ---
!
        DO 100 NTM = 1,NSTM
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
  100   CONTINUE
  200 CONTINUE
      LSR = MAX( LSR,NSRX )
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_SR group
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RD_TF
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
!     Read Solute/Fluid Interaction Card for parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 26 November 2002.
!     Last Modified by MD White, PNNL, 26 November 2002.
!     $Id: step.F,v 1.51 2008/05/15 15:48:06 d3c002 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE TRNSPT
      USE TABL
      USE SOURC
      USE SOLTN
      USE PORMED
      USE GRID
      USE FILES
      USE BCV
      USE GLB_PAR
      USE BUFFEREDREAD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*128 ADUM
      CHARACTER*512 CHDUM
      TYPE(LIST_NODE), POINTER :: LOC_PTR,TMP_PTR
      LOGICAL T_OK
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RD_TF'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
!
!---  Assign card string  ---
!
      CARD = 'Solute/Fluid Interactions Card'
!
!---  Read number of different solutes  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Solutes'
      CALL RD_INT(ISTART,ICOMMA,CHDUM,NLIN)
      NSOLU = 0
      NULLIFY( SOLUT_PTR )
      DO 200 NL = 1, NLIN
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        ADUM(1:) = ' '
        VARB = 'Solute Name'
        CALL RD_CHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
!
!---    Empty solute name list  ---
!
        IF( .NOT.ASSOCIATED(SOLUT_PTR) ) THEN
          ALLOCATE( SOLUT_PTR,STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: SOLUT_PTR'
            CALL WRMSGS( INDX )
          ENDIF
          SOLUT_PTR%LIST_NAME = ADUM
          NULLIFY(SOLUT_PTR%NEXT)
          NSOLU = NSOLU + 1
!
!---    Established solute name list  ---
!
        ELSE
          LOC_PTR => SOLUT_PTR
!
!---      Check for repeated solute name  ---
!
          DO
            IF( .NOT.ASSOCIATED(LOC_PTR) ) EXIT
            IF( LOC_PTR%LIST_NAME == ADUM ) GOTO 110
            LOC_PTR => LOC_PTR%NEXT
          ENDDO
!
!---      Add solute name to solute list  ---
!
          NSOLU = NSOLU + 1
          ALLOCATE( TMP_PTR,STAT=ISTAT )
          IF( ISTAT.NE.0 ) THEN
            INDX = 3
            CHMSG = 'Allocation Error: SOLUT_PTR'
            CALL WRMSGS( INDX )
          ENDIF
          TMP_PTR%LIST_NAME = ADUM
          TMP_PTR%NEXT => SOLUT_PTR
          SOLUT_PTR => TMP_PTR
        ENDIF
  110   CONTINUE
        LSOLU = MAX( LSOLU,NSOLU )
        IF( NSOLU.GT.0 ) LC = 1
  200 CONTINUE
!
!---  Reset subroutine name  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RD_TF group
!
      RETURN
      END

