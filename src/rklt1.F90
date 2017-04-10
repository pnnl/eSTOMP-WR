

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RKLT1( HDGL,RKLX,SLP,SLPF,SLPM,SLX,IZN,ITX,IPHX,M )
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
!     Compute the aqueous relative permeability tensor components
!     from the aqueous saturation.
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RKLT1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Constant relative permeability function  ---
!
      IF( MOD( IRPLT(ITX,IZN),100 ).EQ.0 ) THEN
!
!---    Single-pressure dual-porosity saturation function  ---
!
      IF( ISCHR(IZN).EQ.3 .OR. ISCHR(IZN).EQ.4 ) THEN
         RKLM = RPLT(ITX,2,IZN)
         RKLF = RPLT(ITX,1,IZN)
         IMX = ITX + 3
         IFX = ITX + 6
         RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) + &
          PERM(IFX,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN)) + &
          PERM(IFX,IZN)*POR(4,IZN) + SMALL )
!
!---    Triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX = RPLT(ITX,1,IZN)
          ELSE
            RKLX = RPLT(ITX,2,IZN)
          ENDIF
!
!---    Other saturation functions  ---
!
        ELSE
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = RPLT(ITX,1,IZN)
          ELSE
            RKLX = RPLT(ITX,2,IZN)
          ENDIF
		ENDIF
!
!---  Mualem-irreducible porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.21 ) THEN
        SLPX = (SLP-SLP*SCHR(4,IZN)+SCHR(4,IZN)-RPLT(ITX,1,IZN))/ &
        (1.D+0-RPLT(ITX,1,IZN))
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1  ) THEN
          RKLX = SQRT(SLPX)*((1.D+0-(1.D+0-SLPX** &
          (1.D+0/RPLT(ITX,2,IZN)))**RPLT(ITX,2,IZN))**2)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 ) THEN
          RKLX = SLPX**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
        ENDIF
!
!---  Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.1 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = SQRT(SLP)*((1.D+0-(1.D+0-SLP** &
            (1.D+0/RPLT(ITX,1,IZN)))**RPLT(ITX,1,IZN))**2)
          ELSE
            RKLX = SQRT(SLP)*((1.D+0-(1.D+0-SLP** &
            (1.D+0/RPLT(ITX,2,IZN)))**RPLT(ITX,2,IZN))**2)
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---    Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    van Genuchten triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) ) RKLX = SQRT(SLP)*       &
            ((1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,1,IZN)))** &
            RPLT(ITX,1,IZN))**2)
          ELSE
            RKLX = SQRT(SLP)*((1.D+0-(1.D+0-SLP** &
            (1.D+0/RPLT(ITX,2,IZN)))**RPLT(ITX,2,IZN))**2)
          ENDIF
!
!---    Brooks-Corey triple-curve saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GT.SCHR(4,IZN) ) &
            RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
		ELSEIF( ISCHR(IZN).EQ.3  ) THEN
		  RKLM = 1.D+0 - (SLPM**(1.D+0/RPLT(ITX,2,IZN)))
		  RKLM = SQRT(SLPM)*((1.D+0-(RKLM**RPLT(ITX,2,IZN)))**2)
		  RKLF = 1.D+0 - (SLPF**(1.D+0/RPLT(ITX,1,IZN)))
		  RKLF = SQRT(SLPF)*((1.D+0-(RKLF**RPLT(ITX,1,IZN)))**2)
		  IMX = ITX + 3
		  IFX = ITX + 6
		  RKLX = ( PERM(ITX,IZN)*RKLM*(1.D+0-POR(4,IZN)) + &
          PERM(IFX,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(ITX,IZN)*(1.D+0-POR(4,IZN)) &
          + PERM(IFX,IZN)*POR(4,IZN)+ SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
		ELSEIF( ISCHR(IZN).EQ.4  ) THEN
		  RKLM = SLPM**(2.5D+0 + 2.0D+0/RPLT(ITX,2,IZN))
		  RKLF = SLPF**(2.5D+0 + 2.0D+0/RPLT(ITX,1,IZN))
		  IMX = ITX + 3
		  IFX = ITX + 6
		  RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) + &
          PERM(ITX,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN)) &
          + PERM(ITX,IZN)*POR(4,IZN) + SMALL )
        ENDIF
!
!---  Burdine porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.2 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,1,IZN))) &
            **RPLT(ITX,1,IZN))
          ELSE
            RKLX = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,2,IZN))) &
            **RPLT(ITX,2,IZN))
          ENDIF
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
!
!---      Two-pressure dual-porosity  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    Triple-curve van Genuchten saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.301 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) RKLX = (SLP**2)*(1.D+0- &
            (1.D+0-SLP**(1.D+0/RPLT(ITX,1,IZN)))**RPLT(ITX,1,IZN))
          ELSE
            RKLX = (SLP**2)*(1.D+0-(1.D+0-SLP**(1.D+0/RPLT(ITX,2,IZN))) &
            **RPLT(ITX,2,IZN))
          ENDIF
!
!---    Triple-curve Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.302 ) THEN
          IF( IPHX.EQ.2 ) THEN
            RKLX = 0.D+0
            IF( SLX.GE.SCHR(4,IZN) ) &
            RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,1,IZN))
          ELSE
            RKLX = SLP**(3.0D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          ENDIF
!
!---    Single-pressure dual-porosity van Genuchten  ---
!
        ELSEIF( ISCHR(IZN).EQ.3 ) THEN
          RKLM = (SLPM**2)*(1.D+0-(1.D+0-SLPM**(1.D+0/RPLT(ITX,2,IZN))) &
          **RPLT(ITX,2,IZN))
          RKLF = (SLPF**2)*(1.D+0-(1.D+0-SLPF**(1.D+0/RPLT(ITX,1,IZN))) &
          **RPLT(ITX,1,IZN))
		  IMX = ITX + 3
		  IFX = ITX + 6
		  RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) + &
          PERM(IFX,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN)) + &
          PERM(IFX,IZN)*POR(4,IZN) &
          + PERM(IFX,IZN)*POR(4,IZN) + SMALL )
!
!---    Single-pressure dual-porosity Brooks and Corey  ---
!
        ELSEIF( ISCHR(IZN).EQ.4 ) THEN
          RKLM = SLPM**(3.0D+0 + 2.0D+0/RPLT(ITX,2,IZN))
          RKLF = SLPF**(3.0D+0 + 2.0D+0/RPLT(ITX,1,IZN))
		  IMX = ITX + 3
		  IFX = ITX + 6
		  RKLX = ( PERM(IMX,IZN)*RKLM*(1.D+0-POR(4,IZN)) + &
          PERM(IFX,IZN)*RKLF*POR(4,IZN) )/ &
          ( PERM(IMX,IZN)*(1.D+0-POR(4,IZN)) &
          + PERM(IFX,IZN)*POR(4,IZN)+ SMALL )
        ENDIF
!
!---  Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.3 ) THEN
        RKLX = SLP**4
!
!---  Fatt and Klikoff relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.4 ) THEN
        RKLX = SLP**3
!
!---  Haverkamp relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.5 ) THEN
        IF( HDGL.LE.RPLC(3,IZN) ) THEN
          RKLX = 1.D+0
        ELSE
          ALPHAX = RPLC(1,IZN)/RPLC(4,IZN)
          HDGLX = HDGL/RPLC(4,IZN)
          RKLX = ALPHAX/(ALPHAX + &
          (HDGLX**RPLC(2,IZN)))
        ENDIF
!
!---  Touma and Vauclin relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.6 ) THEN
        RKLX = RPLT(ITX,1,IZN)*(SLP**RPLT(ITX,2,IZN))
!
!---  Free Corey relative permeability function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.7 ) THEN
        SLRX = RPLC(3,IZN)
        SGRX = RPLC(4,IZN)
        SLPX = MIN( MAX( (SLX-SLRX)/(1.D+0-SLRX-SGRX),0.D+0 ),1.D+0 )
        RKLX = RPLC(1,IZN)*(SLPX**(RPLC(2,IZN)))
!
!---  Rijtema-Gardner modified exponential function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.9 ) THEN
        RKLX = EXP( RPLT(ITX,1,IZN)*HDGL + RPLT(ITX,2,IZN) )
!
!---  Modified-Mualem porosity distribution function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.22 ) THEN
!
!---    van Genuchten saturation function  ---
!
        IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
          RKLX = (SLP**RPLT(ITX,2,IZN))*((1.D+0-(1.D+0-SLP** &
          (1.D+0/RPLT(ITX,1,IZN)))**RPLT(ITX,1,IZN))**2)
!
!---    Brooks-Corey saturation function  ---
!
        ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
          RKLX = (SLP**RPLT(ITX,2,IZN))* &
          (SLP**(2.D+0 + 2.0D+0/RPLT(ITX,1,IZN)))
        ENDIF
!
!---  Tabular function  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.10 ) THEN
        ITBX = 0
        IF( M.NE.2 ) ITBX = 1
        RKLX = FNTBLY( SLX,IRLTBL(1,IZN),IRLTBL(2,IZN),ITBX )
!
!---  Cubic-spline tabular function versus saturation  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.11 ) THEN
        ITBX = 0
        RKLX = FSPLNY( SLX,IRLTBLT(ITX,1,IZN),IRLTBLT(ITX,2,IZN) )
!
!---  Linear tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.12 ) THEN
        ITBX = 0
        RKLX = FNTBLY( HDGL,IRLTBLT(ITX,1,IZN),IRLTBLT(ITX,2,IZN),ITBX )
!
!---  Cubic-spline tabular function versus capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.13 ) THEN
        ITBX = 0
        RKLX = FSPLNY( HDGL,IRLTBLT(ITX,1,IZN),IRLTBLT(ITX,2,IZN) )
!
!---  Linear tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.14 ) THEN
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX = FNTBLY( HDGLX,IRLTBLT(ITX,1,IZN),IRLTBLT(ITX,2,IZN), &
        ITBX )
!
!---  Cubic-spline tabular function versus log capillary head  ---
!
      ELSEIF( MOD( IRPLT(ITX,IZN),100 ).EQ.15 ) THEN
        ITBX = 0
        HDGLX = LOG(HDGL)
        RKLX = FSPLNY( HDGLX,IRLTBLT(ITX,1,IZN),IRLTBLT(ITX,2,IZN) )
      ENDIF
      RKLX = MAX( RKLX,ZERO )
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RKLT1 group  ---
!
      RETURN
      END
