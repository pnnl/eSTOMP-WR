

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE KSP1( N,IZN,M,PGX,PLX,SLX,RKLX,ASLX,ASLMINX, &
      ASGTX,SGRMX,INDX,IPHX,PGOX,PLOX,SLOX )
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
!     Compute the aqueous saturation from the gas/aqueous capillary
!     pressure, and compute the aqueous relative permeability from the
!     aqueous saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE HYST
      USE FDVP
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
!      REAL*8 SCHRX(LSCHR)
      REAL*8 RKLX(3)
!
      REAL*8 ESLX,ESLX_2,ASLX
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/KSP1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
        '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  van Genuchten saturation function  ---
!
      IF( ISCHR(IZN).EQ.1 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
!
!---    Webb extension  ---
!
        IF( ISM(IZN).EQ.2 ) THEN
          HMPX = SCHR(17,IZN)
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.0.D+0 ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          SRX = SCHR(4,IZN)
!
!---      Gas-aqueous capillary head above matching-point
!         head, use Webb extension  ---
!
          IF( HDGL.GT.HMPX ) THEN
            SMPX = SCHR(16,IZN)
            HDGL = MIN( HDGL,HDOD )
            DMPX = SMPX/(LOG10(HDOD)-LOG10(HMPX))
            SLX = -(LOG10(HDGL)-LOG10(HDOD))*DMPX
!            SLP = 0.D+0
            SGX = MAX( 1.D+0-SLX,0.D+0 )
!------------- From Zhang et al 2016-BH ------------------------------------
            ESLX_2 = (1.D+0 + (SCHR(1,N)*HDGL)**CN)**(-CM)
            SRX = 1.0D+0 - &
               (1.0D+0 - SLX)/(1.0D+0 - ESLX_2) ! eqn 6a in Zhang et al 2016
            ASLX = SLX
            SLP = MIN( MAX( (SLX-SRX)/(1.D+0-SRX),0.D+0 ),1.D+0 )
!--------------------------------------------------------------------------
!
!---      Gas-aqueous capillary head below
!         matching-point head  ---
!
          ELSE
            SLP = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
            SLX = SLP*(1.D+0-SRX) + SRX
            SGX = MAX( 1.D+0-SLX,0.D+0 )
            ASLX = SLP
          ENDIF
          ASGTX = 0.D+0
          ASLM = MIN( ASLX,ASLMINX )
        ELSE
!
!---    Two-pressure dual-porosity model  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            CN = MAX( SCHR(6,IZN),SMALL )
            IF( SCHR(15,IZN).LE.ZERO ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                CM = 1.D+0 - 2.D+0/CN
              ELSE
                CM = 1.D+0 - 1.D+0/CN
              ENDIF
            ELSE
              CM = SCHR(15,IZN)
            ENDIF
            SLP = (1.D+0/(1.D+0 + (SCHR(5,IZN)*HDGL)**CN))**CM
            REALX = REAL(ISM(IZN))
            HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
            SMP = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
          ELSE
            CN = MAX( SCHR(3,IZN),SMALL )
            IF( SCHR(14,IZN).LE.ZERO ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                CM = 1.D+0 - 2.D+0/CN
              ELSE
                CM = 1.D+0 - 1.D+0/CN
              ENDIF
            ELSE
              CM = SCHR(14,IZN)
            ENDIF
            SLP = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
            REALX = REAL(ISM(IZN))
            HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
            SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
          ASLX = SLP
          ASGTX = 0.D+0
          ASLM = MIN( ASLX,ASLMINX )
        ENDIF
!
!---  Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.2 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
!
!---    Webb extension  ---
!
        IF( ISM(IZN).EQ.2 ) THEN
          HMPX = SCHR(17,IZN)
          CL = MAX( SCHR(3,IZN),SMALL )
          SRX = SCHR(4,IZN)
!
!---      Gas-aqueous capillary head above matching-point
!         head, use Webb extension  ---
!
          IF( HDGL.GT.HMPX ) THEN
            SMPX = SCHR(16,IZN)
            HDGL = MIN( HDGL,HDOD )
            DMPX = SMPX/(LOG10(HDOD)-LOG10(HMPX))
            SLX = -(LOG10(HDGL)-LOG10(HDOD))*DMPX
!            SLP = 0.D+0
            SGX = MAX( 1.D+0-SLX,0.D+0 )
!------------- From Zhang et al 2016-BH ------------------------------------
            ESLX_2 = (SCHR(1,N)/HDGL)**(CL)
            SMP = MAX( (1.D+0-HSCL)*SRX,0.D+0 )
            SRX = 1.0D+0 - &
                (1.0D+0 - SLX)/(1.0D+0 - ESLX_2)
            ASLX = SLX
            SLP = MIN( MAX( (SLX-SRX)/(1.D+0-SRX),0.D+0 ),1.D+0 )
!---------------------------------------------------------------------
!
!---      Gas-aqueous capillary head below
!         matching-point head  ---
!
          ELSE
            IF( HDGL.LE.SCHR(1,IZN) ) THEN
              SLP = 1.D+0
            ELSE
              SLP = (SCHR(1,IZN)/HDGL)**CL
            ENDIF
            SLX = SLP*(1.D+0-SRX) + SRX
            SGX = MAX( 1.D+0-SLX,0.D+0 )
            ASLX = SLP
          ENDIF
          ASGTX = 0.D+0
          ASLM = MIN( ASLX,ASLMINX )
        ELSE
!
!---    Two-pressure dual-porosity model  ---
!
          IF( ABS(IDP(IZN)).EQ.1 ) THEN
            CL = MAX( SCHR(6,IZN),SMALL )
            IF( HDGL.LE.SCHR(5,IZN) ) THEN
              SLP = 1.D+0
            ELSE
              SLP = (SCHR(5,IZN)/HDGL)**CL
            ENDIF
            REALX = REAL(ISM(IZN))
            HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
            SMP = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
          ELSE
            CL = MAX( SCHR(3,IZN),SMALL )
            IF( HDGL.LE.SCHR(1,IZN) ) THEN
              SLP = 1.D+0
            ELSE
              SLP = (SCHR(1,IZN)/HDGL)**CL
            ENDIF
            REALX = REAL(ISM(IZN))
            HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
            SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
          ASLX = SLP
          ASGTX = 0.D+0
          ASLM = MIN( ASLX,ASLMINX )
        ENDIF
!
!---  Single-pressure dual-porosity
!     van Genuchten saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.3 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CN = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(14,IZN)
        ENDIF
        SLPM = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SDPM(N) = SLPM*(1.D+0-SMPM) + SMPM
        CN = MAX( SCHR(6,IZN),SMALL )
        IF( SCHR(15,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(15,IZN)
        ENDIF
        SLPF = (1.D+0/(1.D+0 + (SCHR(5,IZN)*HDGL)**CN))**CM
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPF = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        SDPF(N) = SLPF*(1.D+0-SMPF) + SMPF
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/ &
        ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/ & 
       ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        SLX = SDPF(N)*PORDF + SDPM(N)*PORDM
        ASLX = SLPF*PORDF + SLPM*PORDM
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Single-pressure dual-porosity
!     Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.4 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          SLPM = 1.D+0
        ELSE
          SLPM = (SCHR(1,IZN)/HDGL)**CL
        ENDIF
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SDPM(N) = SLPM*(1.D+0-SMPM) + SMPM
        CL = MAX( SCHR(6,IZN),SMALL )
        IF( HDGL.LE.SCHR(5,IZN) ) THEN
          SLPF = 1.D+0
        ELSE
          SLPF = (SCHR(5,IZN)/HDGL)**CL
        ENDIF
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPF = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        SDPF(N) = SLPF*(1.D+0-SMPF) + SMPF
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/ &
        ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/ &
        ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        SLX = SDPF(N)*PORDF + SDPM(N)*PORDM
        ASLX = SLPF*PORDF + SLPM*PORDM
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Haverkamp saturation function  ---
!
      ELSEIF( ABS(ISCHR(IZN)).EQ.5 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          SLP = 1.D+0
        ELSE
          ALPHAX = SCHR(2,IZN)/SCHR(5,IZN)
          IF( ISCHR(IZN).EQ.-5 ) THEN
            HDGLX = LOG(HDGL/SCHR(5,IZN))
          ELSE
            HDGLX = HDGL/SCHR(5,IZN)
          ENDIF
          SLP = ALPHAX/(ALPHAX &
          + (HDGLX**SCHR(3,IZN)))
        ENDIF
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Russo saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.9 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        SLP = (EXP(-5.D-1*SCHR(1,IZN)*HDGL)* &
        (1.D+0 + 5.D-1*SCHR(1,IZN)*HDGL))**(2.D+0/(SCHR(3,IZN)+2.D+0))
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Linear or linear-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.10 .OR. ISCHR(IZN).EQ.12 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        IF( ISCHR(IZN).EQ.12 ) HDGL = LOG(HDGL)
        ITBX = 0
        IF( M.NE.2 ) ITBX = 1
        SLP = FNTBLY( HDGL,ISLTBL(1,IZN),ISLTBL(2,IZN),ITBX )
        SLX = SLP
        SGX = MAX( 1.D+0-SLX,ZERO )
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Cubic-spline or cubic-spline-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.11 .OR. ISCHR(IZN).EQ.13 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        IF( ISCHR(IZN).EQ.13 ) HDGL = LOG(HDGL)
        SLP = FSPLNY( HDGL,ISLTBL(1,IZN),ISLTBL(2,IZN) )
        SLX = SLP
        SGX = MAX( 1.D+0-SLX,ZERO )
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Polynomial function  ---
!
      ELSEIF( ISCHR(IZN).EQ.19 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
!
!---    Convert head units for polynomial function basis  ---
!
        HDGLU = HDGL/SCHR(1,IZN)
        IF( HDGLU.LT.CPLY_SL(1,1,IZN) ) THEN
          SLX = 1.D+0
        ELSEIF( HDGLU.GE.CPLY_SL(2,NPLY_SL(IZN),IZN) ) THEN
          SLX = 0.D+0
        ELSE
          DO 192 NP = 1,NPLY_SL(IZN)
            IF( HDGLU.GE.CPLY_SL(1,NP,IZN) .AND. &
            HDGLU.LT.CPLY_SL(2,NP,IZN) ) THEN
                SLX = 0.D+0
                NPOLYC = LPOLYC
                DO 190 NC = 5,NPOLYC
                  SLX = SLX + CPLY_SL(NC,NP,IZN)*(LOG10(HDGLU)**(NC-5))
  190           CONTINUE
                GOTO 194
            ENDIF
  192     CONTINUE
  194     CONTINUE
        ENDIF
        SLP = SLX
        SGX = MAX( 1.D+0-SLX,0.D+0 )
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Cambridge function
!
      ELSEIF( ISCHR(IZN).EQ.41 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CN = MAX( SCHR(3,IZN),SMALL )
        SLP = ((SCHR(1,IZN)-HDGL)/(SCHR(1,IZN)+SMALL))**(1.D+0/CN)
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  van Genuchten saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.101 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CN = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CM = 1.D+0 - 2.D+0/CN
          ELSE
            CM = 1.D+0 - 1.D+0/CN
          ENDIF
        ELSE
          CM = SCHR(14,IZN)
        ENDIF
        ASLX = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
        IF( INDX.EQ.2 ) GOTO 200
        ASLM = MIN( ASLX,ASLMINX )
        IF( SGRMX.GT.EPSL ) THEN
          R = 1.D+0/SGRMX - 1.D+0
          ASGTX = (1.D+0-ASLM)/(1.D+0 + R*(1.D+0-ASLM)) - &
          (1.D+0-ASLX)/(1.D+0 + R*(1.D+0-ASLX))
        ELSE
          ASGTX = 0.D+0
        ENDIF
        SLP = ASLX - ASGTX
        SMP = MAX( SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Brooks and Corey saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.102 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          ASLX = 1.D+0
        ELSE
          ASLX = (SCHR(1,IZN)/HDGL)**CL
        ENDIF
        IF( INDX.EQ.2 ) GOTO 200
        ASLM = MIN( ASLX,ASLMINX )
        IF( SGRMX.GT.EPSL ) THEN
          R = 1.D+0/SGRMX - 1.D+0
          ASGTX = (1.D+0-ASLM)/(1.D+0 + R*(1.D+0-ASLM)) - &
          (1.D+0-ASLX)/(1.D+0 + R*(1.D+0-ASLX))
        ELSE
          ASGTX = 0.D+0
        ENDIF
        SLP = ASLX - ASGTX
        SMP = MAX( SCHR(4,IZN),ZERO )
        SLX = SLP*(1.D+0-SMP) + SMP
!
!---  van Genuchten triple curve saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.301 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CN = MAX( SCHR(3,IZN),SMALL )
            IF( SCHR(14,IZN).LE.ZERO ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                CM = 1.D+0 - 2.D+0/CN
              ELSE
                CM = 1.D+0 - 1.D+0/CN
              ENDIF
            ELSE
              CM = SCHR(14,IZN)
            ENDIF
            SLPHO = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
            SLP = SLP*SLPO/(SLPHO+SMALL)
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CN = MAX( SCHR(5,IZN),SMALL )
          IF( SCHR(7,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(7,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(2,IZN)*HDGL)**CN))**CM
          SMP = SCHR(6,IZN)
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CN = MAX( SCHR(3,IZN),SMALL )
          IF( SCHR(14,IZN).LE.ZERO ) THEN
            IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
              CM = 1.D+0 - 2.D+0/CN
            ELSE
              CM = 1.D+0 - 1.D+0/CN
            ENDIF
          ELSE
            CM = SCHR(14,IZN)
          ENDIF
          SLP = (1.D+0/(1.D+0 + (SCHR(12,IZN)*HDGL)**CN))**CM
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CN = MAX( SCHR(3,IZN),SMALL )
            IF( SCHR(14,IZN).LE.ZERO ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                CM = 1.D+0 - 2.D+0/CN
              ELSE
                CM = 1.D+0 - 1.D+0/CN
              ENDIF
            ELSE
              CM = SCHR(14,IZN)
            ENDIF
            SLPHO = (1.D+0/(1.D+0 + (SCHR(12,IZN)*HDGL)**CN))**CM
            SLP = (SLP-SLPM)*(SLPO-SLPM)/(SLPHO-SLPM+SMALL) + SLPM
            SLP = MIN( SLP,SLPM )
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
        ENDIF
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
!
!---  Brooks and Corey triple curve saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.302 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( HDGL.LE.SCHR(1,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(1,IZN)/HDGL)**CL
          ENDIF
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CL = MAX( SCHR(3,IZN),SMALL )
            IF( HDGL.LE.SCHR(1,IZN) ) THEN
              SLPHO = 1.D+0
            ELSE
              SLPHO = (SCHR(1,IZN)/HDGL)**CL
            ENDIF
            SLP = SLP*SLPO/(SLPHO+SMALL)
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CL = MAX( SCHR(5,IZN),SMALL )
          IF( HDGL.LE.SCHR(2,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(2,IZN)/HDGL)**CL
          ENDIF
          SMP = SCHR(6,IZN)
          SLX = SLP*(1.D+0-SMP) + SMP
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( HDGL.LE.SCHR(12,IZN) ) THEN
            SLP = 1.D+0
          ELSE
            SLP = (SCHR(12,IZN)/HDGL)**CL
          ENDIF
          SMP = SCHR(4,IZN)
          IF( SLOX.GT.EPSL ) THEN
            SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
            HDGL = MAX( (PGOX-PLOX)/RHORL/GRAV,1.D-14 )
            CL = MAX( SCHR(3,IZN),SMALL )
            IF( HDGL.LE.SCHR(12,IZN) ) THEN
              SLPHO = 1.D+0
            ELSE
              SLPHO = (SCHR(12,IZN)/HDGL)**CL
            ENDIF
            SLP = (SLP-SLPM)*(SLPO-SLPM)/(SLPHO-SLPM+SMALL) + SLPM
            SLP = MIN( SLP,SLPM )
          ENDIF
          SLX = SLP*(1.D+0-SMP) + SMP
        ENDIF
        ASLX = SLP
        ASGTX = 0.D+0
        ASLM = MIN( ASLX,ASLMINX )
      ENDIF
!
!---  Skip relative permeability functions  ---
!
      IF( INDX.EQ.0 ) GOTO 200
!
!---  Relative permeability  ---
!
      CALL RKL1( HDGL,RKLX,SLP,SLPF,SLPM,SLX,IZN,IPH(2,N),M )
!
!---  Relative permeability tensor  ---
!
      DO 180 ITX = 1,3
        IF( IRPLT(ITX,IZN).NE.0 )                           &
        CALL RKLT1( HDGL,RKLX(ITX),SLP,SLPF,SLPM,SLX,IZN, &
        ITX,IPH(2,N),M )
  180 CONTINUE
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of KSP1 group  ---
!
      RETURN
      END
