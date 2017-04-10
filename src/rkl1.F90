

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RKL1( HDGL,RKLX,SLP,SLPF,SLPM,SLX,IZN,IPHX,M )
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
!     Compute the aqueous relative permeability from the
!     aqueous saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!     Last Modified by MD White, PNNL, 16 December 2002.
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
!----------------------Type Declarations-------------------------------!
!
      REAL*8 RKLX(3)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RKL1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Constant relative permeability function  ---
!
      IF( MOD( IRPL(IZN),100 ).EQ.0 ) THEN
!
!---    Single-pressure dual-porosity saturation function  ---
!
        IF( ISCHR(IZN).EQ.3 .OR. ISCHR(IZN).EQ.4 ) THEN
          RKLM = RPLC(2,IZN)
          RKLF = RPLC(1,IZN)
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
          PERM(7,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN) &
          + SMALL )
           RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
          PERM(8,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN) &
          + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
          PERM(9,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN) &
          + SMALL )
!
!---    Triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            RKLX(2) = 0.D+0
            RKLX(3) = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(1) = RPLC(1,IZN)
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(2) = RPLC(1,IZN)
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(3) = RPLC(1,IZN)
          ELSE
            RKLX(1) = RPLC(2,IZN)
            RKLX(2) = RPLC(2,IZN)
            RKLX(3) = RPLC(2,IZN)
          ENDIF
!
!---    Other saturation functions  ---
!
        ELSE
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = RPLC(1,IZN)
            RKLX(2) = RPLC(1,IZN)
            RKLX(3) = RPLC(1,IZN)
          ELSE
            RKLX(1) = RPLC(2,IZN)
            RKLX(2) = RPLC(2,IZN)
            RKLX(3) = RPLC(2,IZN)
          ENDIF
       ENDIF
!
!---  Mualem-irreducible porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.21 ) THEN
        SLPX = (SLP-SLP*SCHR(4,IZN)+SCHR(4,IZN)-RPLC(1,IZN))/ &
        (1.D+0-RPLC(1,IZN))
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1  ) THEN
          RKLX(1) = SQRT(SLPX)*((1.D+0-(1.D+0-SLPX**(1.D+0/RPLC(2,IZN))) &
            **RPLC(2,IZN))**2)
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 ) THEN
          RKLX(1) = SLPX**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
        ENDIF
!
!---  Mualem-Anisotropy reference porosity distribution function  ---
!
      ELSEIF( IRPL(IZN).EQ.301 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
          RKLX(1) = (SLP**RPLC(3,IZN))*((1.D+0-(1.D+0-SLP** &
          (1.D+0/RPLC(2,IZN)))**RPLC(2,IZN))**2)
          RKLX(2) = RKLX(1)
          RKLX(3) = (SLP**RPLC(4,IZN))*((1.D+0-(1.D+0-SLP** &
          (1.D+0/RPLC(2,IZN)))**RPLC(2,IZN))**2)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
          RKLX(1) = (SLP**RPLC(3,IZN))*SLP**(2.D+0 + 2.0D+0/RPLC(2,IZN))
          RKLX(2) = RKLX(1)
          RKLX(3) = (SLP**RPLC(4,IZN))*SLP**(2.D+0 + 2.0D+0/RPLC(2,IZN))
        ENDIF
!
!---  Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.1 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = SQRT(SLP)*((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(1,IZN))) &
            **RPLC(1,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SQRT(SLP)*((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN))) &
            **RPLC(2,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    van Genuchten triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) ) RKLX(1) = SQRT(SLP)* &
            ((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(1,IZN)))**RPLC(1,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SQRT(SLP)*((1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN))) &
            **RPLC(2,IZN))**2)
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Brooks-Corey triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) ) &
            RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
        ELSEIF( ISCHR(IZN).EQ.3  ) THEN
          RKLM = 1.D+0 - (SLPM**(1.D+0/RPLC(2,IZN)))
          RKLM = SQRT(SLPM)*((1.D+0-(RKLM**RPLC(2,IZN)))**2)
          RKLF = 1.D+0 - (SLPF**(1.D+0/RPLC(1,IZN)))
          RKLF = SQRT(SLPF)*((1.D+0-(RKLF**RPLC(1,IZN)))**2)
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
            PERM(7,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN) &
            + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
          PERM(8,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN) &
          + SMALL )
           RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
          PERM(9,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN) &
          + SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
        ELSEIF( ISCHR(IZN).EQ.4  ) THEN
          RKLM = SLPM**(2.5D+0 + 2.0D+0/RPLC(2,IZN))
          RKLF = SLPF**(2.5D+0 + 2.0D+0/RPLC(1,IZN))
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
            PERM(7,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN) &
            + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
            PERM(8,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN) &
            + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
          PERM(9,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN) &
          + SMALL )
        ENDIF
!
!---  Burdine porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLC(1,IZN))) &
            **RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN))) &
            **RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Triple-curve van Genuchten saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX(1) = (SLP**2)*(1.D+0- &
            (1.D+0-SLP**(1.D+0/RPLC(1,IZN)))**RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLC(2,IZN))) &
            **RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Triple-curve Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX(1) = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) &
            RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(1,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ELSE
            RKLX(1) = SLP**(3.0D+0 + 2.0D+0/RPLC(2,IZN))
            RKLX(2) = RKLX(1)
            RKLX(3) = RKLX(1)
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
        ELSEIF( ISCHR(IZN).EQ.3 ) THEN
          RKLM = (SLPM**2)*(1.D+0-(1.D+0-SLPM**(1.D+0/RPLC(2,IZN))) &
            **RPLC(2,IZN))
          RKLF = (SLPF**2)*(1.D+0-(1.D+0-SLPF**(1.D+0/RPLC(1,IZN))) &
            **RPLC(1,IZN))
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
            PERM(7,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN) &
            + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
            PERM(8,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN) &
            + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +          &
            PERM(9,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN) &
            + SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
        ELSEIF( ISCHR(IZN).EQ.4 ) THEN
          RKLM = SLPM**(3.0D+0 + 2.0D+0/RPLC(2,IZN))
          RKLF = SLPF**(3.0D+0 + 2.0D+0/RPLC(1,IZN))
          RKLX(1) = ( PERM(4,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
            PERM(7,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(4,IZN)*(1.D+0-POR(4,IZN)) + PERM(7,IZN)*POR(4,IZN) &
            + SMALL )
          RKLX(2) = ( PERM(5,IZN)*RKLM*(1.D+0-POR(4,IZN)) +          &
            PERM(8,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(5,IZN)*(1.D+0-POR(4,IZN)) + PERM(8,IZN)*POR(4,IZN) &
            + SMALL )
          RKLX(3) = ( PERM(6,IZN)*RKLM*(1.D+0-POR(4,IZN)) +           &
            PERM(9,IZN)*RKLF*POR(4,IZN) )/ &
            ( PERM(6,IZN)*(1.D+0-POR(4,IZN)) + PERM(9,IZN)*POR(4,IZN) &
            + SMALL )
        ENDIF
!
!---  Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.3 ) THEN
        RKLX(1) = SLP**4
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Fatt and Klikoff relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.4 ) THEN
        RKLX(1) = SLP**3
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Haverkamp relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.5 ) THEN
        IF( HDGL.LE.RPLC(3,IZN) ) THEN
          RKLX(1) = 1.D+0
        ELSE
          ALPHAX = RPLC(1,IZN)/RPLC(4,IZN)
          HDGLX = HDGL/RPLC(4,IZN)
          RKLX(1) = ALPHAX/(ALPHAX + &
          (HDGLX**RPLC(2,IZN)))
        ENDIF
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Touma and Vauclin relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.6 ) THEN
        RKLX(1) = RPLC(1,IZN)*(SLP**RPLC(2,IZN))
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Free Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.7 ) THEN
        SLRX = RPLC(3,IZN)
        SGRX = RPLC(4,IZN)
        SLPX = MIN( MAX( (SLX-SLRX)/(1.D+0-SLRX-SGRX),0.D+0 ),1.D+0 )
        RKLX(1) = RPLC(1,IZN)*(SLPX**(RPLC(2,IZN)))
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Rijtema-Gardner modified exponential function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.9 ) THEN
        RKLX(1) = EXP( RPLC(1,IZN)*HDGL + RPLC(2,IZN) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Tabular function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.10 ) THEN
        ITBX = 0
        IF( M.NE.2 ) ITBX = 1
        RKLX(1) = FNTBLY( SLX,IRLTBL(1,IZN),IRLTBL(2,IZN),ITBX )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Cubic-spline tabular function versus saturation  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.11 ) THEN
        ITX = 1
        ITBX = 0
        RKLX(1) = FSPLNY( SLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Linear tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.12 ) THEN
        ITX = 1
        ITBX = 0
        RKLX(1) = FNTBLY( HDGL,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX), &
         ITBX )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Cubic-spline tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.13 ) THEN
        ITX = 1
        ITBX = 0
        RKLX(1) = FSPLNY( HDGL,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Linear tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.14 ) THEN
        ITX = 1
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX(1) = FNTBLY( HDGLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX), &
        ITBX )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Cubic-spline tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.15 ) THEN
        ITX = 1
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX(1) = FSPLNY( HDGLX,IRLTBLT(1,IZN,ITX),IRLTBLT(2,IZN,ITX) )
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Polynomial function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.19 ) THEN
!
!---    Convert head units for polynomial function basis  ---
!
        HDGLU = HDGL/RPLC(1,IZN)
        IF( HDGLU.LT.CPLY_RL(1,1,IZN) ) THEN
          RKLX(1) = 1.D+0
        ELSEIF( HDGLU.GE.CPLY_RL(2,NPLY_RL(IZN),IZN) ) THEN
          RKLX(1) = 0.D+0
        ELSE
          DO 1192 NP = 1,NPLY_RL(IZN)
            IF( HDGLU.GE.CPLY_RL(1,NP,IZN) .AND. &
            HDGLU.LT.CPLY_RL(2,NP,IZN) ) THEN
                RKLX(1) = 0.D+0
                NPOLYC = LPOLYC
                DO 1190 NC = 5,NPOLYC
                  RKLX(1) = RKLX(1) + CPLY_RL(NC,NP,IZN)* &
                  (LOG10(HDGLU)**(NC-5))
 1190           CONTINUE
                GOTO 1194
            ENDIF
 1192     CONTINUE
 1194     CONTINUE
!
!---  Normalize absolute conductivity with saturated
!     conductivity  ---
!
          RKLX(1) = (1.D+1**RKLX(1))/RPLC(2,IZN)
        ENDIF
        RKLX(2) = RKLX(1)
        RKLX(3) = RKLX(1)
!
!---  Modified-Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPL(IZN),100 ).EQ.22 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
          RKLX(1) = (SLP**RPLC(1,IZN))*((1.D+0-(1.D+0-SLP** &
          (1.D+0/RPLC(2,IZN)))**RPLC(2,IZN))**2)
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
          RKLX(1) = (SLP**RPLC(1,IZN))*SLP**(2.D+0 + 2.0D+0/RPLC(2,IZN))
          RKLX(2) = RKLX(1)
          RKLX(3) = RKLX(1)
        ENDIF
      ENDIF
!
!---  Polmann anisotropy permeability function  ---
!
      IF( IRPL(IZN).GE.100 .AND. IRPL(IZN).LT.200 ) THEN
        PSI = HDGL*1.D+2
        SKLX = RPLC(5,IZN) - RPLC(10,IZN)*PSI -          &
        RPLC(6,IZN)*RPLC(9,IZN)*( RPLC(7,IZN) - &
        (RPLC(7,IZN)**2)*PSI - (RPLC(8,IZN)**2)*PSI)/ &
        (1.D+0 + RPLC(10,IZN)*RPLC(9,IZN))
        SIGMA = RPLC(6,IZN)*(((1.D+0 - RPLC(7,IZN)*PSI)**2) + &
        (RPLC(8,IZN)**2)*(PSI**2))/ &
        (1.D+0 + RPLC(10,IZN)*RPLC(9,IZN))
        SKHX = EXP( SKLX + 5.D-1*SIGMA )
        SKVX = EXP( SKLX - 5.D-1*SIGMA )
        ANISOX = MIN( MAX( SKHX/SKVX,0.D+0 ),RPLC(11,IZN) )
        ANISOX = MAX( ANISOX,RPLC(12,IZN) )
        RKLX(1) = RKLX(3)*ANISOX
        RKLX(2) = RKLX(3)*ANISOX
      ENDIF
      RKLX(1) = MAX( RKLX(1),ZERO )
      RKLX(2) = MAX( RKLX(2),ZERO )
      RKLX(3) = MAX( RKLX(3),ZERO )
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RKL1 group  ---
!
      RETURN
      END
