

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CAP1( IZN,SLX,SGTX,CPGL,SLOX,CPGLO,IPHX )
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
!     Compute the gas/aqueous capillary pressure from the aqueous
!     saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 19, 1997.
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
      SUBNMX = '/CAP1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  van Genuchten saturation function
!
      IF( ISCHR(IZN).EQ.1 ) THEN
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
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)** &
          (1.D+0/CN))/SCHR(1,IZN)
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-6
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  100   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)/(HDGL*LOG(HDOD))*REALX
        SLP = 1.D+0/((1.D+0 + (SCHR(1,IZN)*HDGL)**CN)**CM)
        DSLP = -CM*SCHR(1,IZN)*CN*((SCHR(1,IZN)*HDGL)**(CN-1.D+0)) &
      /((1.D+0 + (SCHR(1,IZN)*HDGL)**CN)**(CM+1.D+0))
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 100
        CPGL = HDGL*RHORL*GRAV
!
!---  Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.2 ) THEN
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          HDGL = SCHR(1,IZN)*(1.D+0/SLP)**(1.D+0/CL)
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-9
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  200   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)*REALX/(HDGL*LOG(HDOD))
        SLP = (SCHR(1,IZN)/HDGL)**CL
        DSLP = -CL*(SCHR(1,IZN)/(HDGL**2)) &
        *(SCHR(1,IZN)/HDGL)**(CL-1.D+0)
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 200
        CPGL = HDGL*RHORL*GRAV
!
!---  Dual porosity van Genuchten saturation function
!
      ELSEIF( ISCHR(IZN).EQ.3 ) THEN
        CNM = MAX( SCHR(3,IZN),SMALL )
        IF( SCHR(14,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CMM = 1.D+0 - 2.D+0/CNM
          ELSE
            CMM = 1.D+0 - 1.D+0/CNM
          ENDIF
        ELSE
          CMM = SCHR(14,IZN)
        ENDIF
        CNF = MAX( SCHR(6,IZN),SMALL )
        IF( SCHR(15,IZN).LE.ZERO ) THEN
          IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
            CMF = 1.D+0 - 2.D+0/CNF
          ELSE
            CMF = 1.D+0 - 1.D+0/CNF
          ENDIF
        ELSE
          CMF = SCHR(15,IZN)
        ENDIF
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/ &
        ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/ &
        ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        IF( SLX.GT.PORDM ) THEN
          IF( SLX.GT.SCHR(7,IZN ) ) THEN
            SLP = (SLX-SCHR(7,IZN))/(1.D+0-SCHR(7,IZN))
            HDGL = (((1.D+0/SLP)**(1.D+0/CMF)-1.D+0)** &
            (1.D+0/CNF))/SCHR(5,IZN)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(7,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(7,IZN) + 1.D-6
          ENDIF
        ELSE
          IF( SLX.GT.SCHR(4,IZN ) ) THEN
            SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
            HDGL = (((1.D+0/SLP)**(1.D+0/CMM)-1.D+0)** &
            (1.D+0/CNM))/SCHR(1,IZN)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(4,IZN) + 1.D-6
          ENDIF
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  300   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMPM = -SCHR(4,IZN)/(HDGL*LOG(HDOD))*REALX
        SLPM = 1.D+0/((1.D+0 + (SCHR(1,IZN)*HDGL)**CNM)**CMM)
        DSLPM = -CMM*SCHR(1,IZN)*CNM*((SCHR(1,IZN)*HDGL)**(CNM-1.D+0)) &
      /((1.D+0 + (SCHR(1,IZN)*HDGL)**CNM)**(CMM+1.D+0))
        SLZM = SLPM*(1.D+0-SMPM) + SMPM
        DSLZM = DSLPM*(1.D+0-SMPM) + DSMPM*(1.D+0-SLPM)
        SMPF = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMPF = -SCHR(7,IZN)/(HDGL*LOG(HDOD))*REALX
        SLPF = 1.D+0/((1.D+0 + (SCHR(5,IZN)*HDGL)**CNF)**CMF)
        DSLPF = -CMF*SCHR(5,IZN)*CNF*((SCHR(5,IZN)*HDGL)**(CNF-1.D+0)) &
      /((1.D+0 + (SCHR(5,IZN)*HDGL)**CNF)**(CMF+1.D+0))
        SLZF = SLPF*(1.D+0-SMPF) + SMPF
        DSLZF = DSLPF*(1.D+0-SMPF) + DSMPF*(1.D+0-SLPF)
        F = SLX - SLZM*PORDM - SLZF*PORDF
        DF = -DSLZM*PORDM - DSLZF*PORDF
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = MAX( HDGL+DH,1.D-14 )
        IF( ABS(DH).GT.1.D-8 ) GOTO 300
        CPGL = HDGL*RHORL*GRAV
!
!---  Dual Porosity Brooks and Corey saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.4 ) THEN
        CLM = MAX( SCHR(3,IZN),SMALL )
        CLF = MAX( SCHR(6,IZN),SMALL )
        PORDM = (1.D+0-POR(4,IZN))*POR(2,IZN)/ &
        ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        PORDF = POR(4,IZN)/ &
        ( POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + SMALL )
        IF( SLX.GT.PORDM ) THEN
          IF( SLX.GT.SCHR(4,IZN ) ) THEN
            SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
            HDGL = SCHR(1,IZN)*(1.D+0/SLP)**(1.D+0/CLM)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(4,IZN) + 1.D-9
          ENDIF
        ELSE
          IF( SLX.GT.SCHR(7,IZN ) ) THEN
            SLP = (SLX-SCHR(7,IZN))/(1.D+0-SCHR(7,IZN))
            HDGL = SCHR(5,IZN)*(1.D+0/SLP)**(1.D+0/CLF)
          ELSEIF( ISM(IZN).EQ.1 ) THEN
            HDGL = EXP((1.D+0-SLX/SCHR(7,IZN))*LOG(HDOD))
          ELSE
            HDGL = HDOD
            SLX = SCHR(7,IZN) + 1.D-9
          ENDIF
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  400   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMPM = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMPM = -SCHR(4,IZN)*REALX/(HDGL*LOG(HDOD))
        SLPM = (SCHR(1,IZN)/HDGL)**CLM
        DSLPM = -CLM*(SCHR(1,IZN)/(HDGL**2)) &
        *(SCHR(1,IZN)/HDGL)**(CLM-1.D+0)
        SLZM = SLPM*(1.D+0-SMPM) + SMPM
        DSLZM = DSLPM*(1.D+0-SMPM) + DSMPM*(1.D+0-SLPM)
        SMPF = MAX( (1.D+0-HSCL)*SCHR(7,IZN),ZERO )
        DSMPF = -SCHR(7,IZN)*REALX/(HDGL*LOG(HDOD))
        SLPF = (SCHR(5,IZN)/HDGL)**CLF
        DSLPF = -CLF*(SCHR(5,IZN)/(HDGL**2)) &
        *(SCHR(5,IZN)/HDGL)**(CLF-1.D+0)
        SLZF = SLPF*(1.D+0-SMPF) + SMPF
        DSLZF = DSLPF*(1.D+0-SMPF) + DSMPF*(1.D+0-SLPF)
        F = SLX - SLZM*PORDM - SLZF*PORDF
        DF = -DSLZM*PORDM - DSLZF*PORDF
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 400
        CPGL = HDGL*RHORL*GRAV
!
!---  Haverkamp saturation function  ---
!
      ELSEIF( ABS(ISCHR(IZN)).EQ.5 ) THEN
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          ALPHAX = SCHR(2,IZN)/SCHR(5,IZN)
          IF( ISCHR(IZN).EQ.-5 ) THEN
            HDGL = EXP((-(SLP*ALPHAX-ALPHAX)/SLP)**(1.D+0/SCHR(3,IZN)))* &
            SCHR(5,IZN)
          ELSE
            HDGL = ((-(SLP*ALPHAX-ALPHAX)/SLP)**(1.D+0/SCHR(3,IZN)))* &
            SCHR(5,IZN)
          ENDIF
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-9
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  500   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)*REALX/(HDGL*LOG(HDOD))
        ALPHAX = SCHR(2,IZN)/SCHR(5,IZN)
        IF( ISCHR(IZN).EQ.-5 ) THEN
          HDGLX = LOG(HDGL/SCHR(5,IZN))
          SLP = ALPHAX/(ALPHAX + (HDGLX**SCHR(3,IZN)))
          DSLP = -(ALPHAX*(HDGLX**SCHR(3,IZN))*       &
          (SCHR(3,IZN)/(HDGLX*HDGL/SCHR(5,IZN))))/ &
          ((ALPHAX+(HDGLX**SCHR(3,IZN)))**2)
        ELSE
          HDGLX = HDGL/SCHR(5,IZN)
          SLP = ALPHAX/(ALPHAX + (HDGLX**SCHR(3,IZN)))
          DSLP = -(ALPHAX*(HDGLX**SCHR(3,IZN))*(SCHR(3,IZN)/HDGLX))/ &
          ((ALPHAX+(HDGLX**SCHR(3,IZN)))**2)
        ENDIF
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 500
        CPGL = HDGL*RHORL*GRAV
!
!---  Russo saturation function
!
      ELSEIF( ISCHR(IZN).EQ.9 ) THEN
        IF( SLX.GT.SCHR(4,IZN ) ) THEN
          SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
          HDGL = (2.D+0/SCHR(1,IZN))* &
          SQRT(1.D+0 - (SLP**(5.D-1*SCHR(3,IZN)+1.D+0)))
        ELSEIF( ISM(IZN).EQ.1 ) THEN
          HDGL = EXP((1.D+0-SLX/SCHR(4,IZN))*LOG(HDOD))
        ELSE
          HDGL = HDOD
          SLX = SCHR(4,IZN) + 1.D-6
        ENDIF
        HDGL = MAX( HDGL,1.D-14 )
  900   CONTINUE
        REALX = REAL(ISM(IZN))
        HSCL = MAX( LOG(HDGL)/LOG(HDOD),ZERO )*REALX
        SMP = MAX( (1.D+0-HSCL)*SCHR(4,IZN),ZERO )
        DSMP = -SCHR(4,IZN)/(HDGL*LOG(HDOD))*REALX
        SLP = (EXP(-5.D-1*SCHR(1,IZN)*HDGL)* &
        (1.D+0 + 5.D-1*SCHR(1,IZN)*HDGL))**(2.D+0/(SCHR(3,IZN)+2.D+0))
        DSLP = -((2.5D-1*EXP(-SCHR(1,IZN)*HDGL)*                          &
       ((2.D+0+(SCHR(1,IZN)*HDGL))**2))**(1.D+0/(SCHR(3,IZN)+2.D+0)))* &
       (SCHR(1,IZN)**2)*(HDGL/((SCHR(3,IZN)+2.D+0)* &
       (2.D+0*SCHR(1,IZN)*HDGL)))
        SLZ = SLP*(1.D+0-SMP) + SMP
        DSLZ = DSLP*(1.D+0-SMP) + DSMP*(1.D+0-SLP)
        F = SLX - SLZ
        DF = -DSLZ
        DH = -F/(DF+SMALL)
        IF( HDGL+DH.LT.0.D+0 ) DH = 6.D-1*DH
        HDGL = HDGL + DH
        IF( ABS(DH).GT.1.D-8 ) GOTO 900
        CPGL = HDGL*RHORL*GRAV
!
!---  Linear or linear-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.10 .OR. ISCHR(IZN).EQ.12 ) THEN
        ITBX = 0
        HDGL = FNTBLX( SLX,ISLTBL(1,IZN),ISLTBL(2,IZN),ITBX )
        IF( ISCHR(IZN).EQ.12 ) HDGL = EXP(HDGL)
        CPGL = HDGL*RHORL*GRAV
!
!---  Cubic-spline .or. cubic-spline-log interpolation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.11 .OR. ISCHR(IZN).EQ.13 ) THEN
        HDGL = FSPLNX( SLX,ISLTBL(1,IZN),ISLTBL(2,IZN) )
        IF( ISCHR(IZN).EQ.13 ) HDGL = EXP(HDGL)
        CPGL = HDGL*RHORL*GRAV
!
!---  Cambridge saturation function
!
      ELSEIF( ISCHR(IZN).EQ.41 ) THEN
        CN = MAX( SCHR(3,IZN),SMALL )
        SLP = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
        SLP = MAX( SLP,0.D+0 )
        HDGL = SCHR(1,IZN)*(1.D+0-(SLP**CN))
        CPGL = HDGL*RHORL*GRAV
!
!---  van Genuchten saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.101 ) THEN
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
        ASGTX = SGTX/(1.D+0-SCHR(4,IZN))
        ESL = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
        ASLX = ESL + ASGTX
        HDGL = (((1.D+0/ASLX)**(1.D+0/CM)-1.D+0)** &
        (1.D+0/CN))/SCHR(1,IZN)
        CPGL = HDGL*RHORL*GRAV
!
!---  Brooks and Corey saturation function w/ gas entrapment  ---
!
      ELSEIF( ISCHR(IZN).EQ.102 ) THEN
        ASGTX = SGTX/(1.D+0-SCHR(4,IZN))
        ESL = (SLX-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
        ASLX = ESL + ASGTX
        IF( (1.D+0-ASLX)/EPSL.LT.EPSL ) THEN
          HDGL = 0.D+0
        ELSE
          HDGL = SCHR(1,IZN)/(ASLX**(1.D+0/SCHR(3,IZN)))
        END IF
        CPGL = HDGL*RHORL*GRAV
!
!---  van Genuchten triple curve saturation function  ---
!
      ELSEIF( ISCHR(IZN).EQ.301 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          SMP = SCHR(4,IZN)
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
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            SLPHO = (1.D+0/(1.D+0 + (SCHR(1,IZN)*HDGL)**CN))**CM
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ICSN = ICSN-ICSNX
            SUBNM = SUBNM(1:ICSN)
            RETURN
          ENDIF
          SLP = SLP*SLPHO/SLPO
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)** &
            (1.D+0/CN))/SCHR(1,IZN)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          SMP = SCHR(6,IZN)
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
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ICSN = ICSN-ICSNX
            SUBNM = SUBNM(1:ICSN)
            RETURN
          ENDIF
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)** &
            (1.D+0/CN))/SCHR(2,IZN)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          SMP = SCHR(4,IZN)
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
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            SLPHO = (1.D+0/(1.D+0 + (SCHR(12,IZN)*HDGL)**CN))**CM
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ICSN = ICSN-ICSNX
            SUBNM = SUBNM(1:ICSN)
            RETURN
          ENDIF
          SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
          SLP = (SLP*(SLPM-SLPHO) + SLPM*(SLPHO-SLPO))/(SLPM-SLPO)
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = (((1.D+0/SLP)**(1.D+0/CM)-1.D+0)** &
            (1.D+0/CN))/SCHR(12,IZN)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
        ENDIF
!
!---  Brooks and Corey  ---
!
      ELSEIF( ISCHR(IZN).EQ.302 ) THEN
!
!---  Drainage scanning (including main drainage)  ---
!
        IF( IPHX.EQ.-1 ) THEN
          SMP = SCHR(4,IZN)
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            IF( HDGL.LE.SCHR(1,IZN) ) THEN
              SLP = 1.D+0
            ELSE
              SLP = (SCHR(1,IZN)/HDGL)**CL
            ENDIF
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ICSN = ICSN-ICSNX
            SUBNM = SUBNM(1:ICSN)
            RETURN
          ENDIF
          SLP = SLP*SLPHO/SLPO
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = SCHR(1,IZN)*(1.D+0/SLP)**(1.D+0/CL)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Main wetting  ---
!
        ELSEIF( IPHX.EQ.2 ) THEN
          SMP = SCHR(6,IZN)
          CL = MAX( SCHR(5,IZN),SMALL )
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDOD*RHORL*GRAV
            ICSN = ICSN-ICSNX
            SUBNM = SUBNM(1:ICSN)
            RETURN
          ENDIF
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = SCHR(2,IZN)*(1.D+0/SLP)**(1.D+0/CL)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
!
!---  Wetting scanning (including boundary wetting scanning)  ---
!
        ELSE
          SMP = SCHR(4,IZN)
          CL = MAX( SCHR(3,IZN),SMALL )
          IF( SLOX.GT.EPSL ) THEN
            HDGL = CPGLO/RHORL/GRAV
            IF( HDGL.LE.SCHR(12,IZN) ) THEN
              SLP = 1.D+0
            ELSE
              SLP = (SCHR(12,IZN)/HDGL)**CL
            ENDIF
            SLPO = (SLOX-SMP)/(1.D+0-SMP)
          ELSE
            SLPO = 0.D+0
            SLPHO = 0.D+0
          ENDIF
          IF( SLX.GT.SMP ) THEN
            SLP = (SLX-SMP)/(1.D+0-SMP)
          ELSE
            CPGL = HDGL*RHORL*GRAV
            ICSN = ICSN-ICSNX
            SUBNM = SUBNM(1:ICSN)
            RETURN
          ENDIF
          SLPM = (SCHR(10,IZN)-SMP)/(1.D+0-SMP)
          SLP = (SLP*(SLPM-SLPHO) + SLPM*(SLPHO-SLPO))/(SLPM-SLPO)
          IF( 1.D+0-SLP.LT.EPSL ) THEN
            HDGL = 0.D+0
          ELSE
            HDGL = SCHR(12,IZN)*(1.D+0/SLP)**(1.D+0/CL)
          ENDIF
          CPGL = HDGL*RHORL*GRAV
        ENDIF
!
!---  Polynomial function  ---
!
      ELSEIF( ISCHR(IZN).EQ.19 ) THEN
!
!---    Check for saturation above first polynomial  ---
!
        IF( SLX.GT.CPLY_SL(3,1,IZN) ) THEN
          HDGL = CPLY_SL(1,1,IZN)*SCHR(1,IZN)
          CPGL = HDGL*RHORL*GRAV
!
!---    Check for saturation below last polynomial  ---
!
        ELSEIF( SLX.LT.CPLY_SL(4,NPLY_SL(IZN),IZN) ) THEN
          HDGL = CPLY_SL(2,NPLY_SL(IZN),IZN)*SCHR(1,IZN)
          CPGL = HDGL*RHORL*GRAV
!
!---    Find polynomial and set initial guess of head  ---
!
        ELSE
          DSLX = 1.D-6
          DO 1900 NP = 1,NPLY_SL(IZN)
            IF( (SLX-DSLX).LE.CPLY_SL(3,NP,IZN) .AND. &
            (SLX+DSLX).GE.CPLY_SL(4,NP,IZN) ) THEN
              HDGLU = 1.D+1**((SLX-CPLY_SL(4,NP,IZN))*               &
             (LOG10(CPLY_SL(1,NP,IZN))-LOG10(CPLY_SL(2,NP,IZN)))/ &
             (CPLY_SL(3,NP,IZN)-CPLY_SL(4,NP,IZN)) +  &
             LOG10(CPLY_SL(2,NP,IZN)))
              GOTO 1910
            ENDIF
 1900     CONTINUE
          INDX = 15
          CHMSG = 'Saturation Polynomial Not Found for Rock/Soil #'
          IMSG = IZN
          RLMSG = SLX
          CALL WRMSGS( INDX )
 1910     CONTINUE
          SLZ = 0.D+0
          DSLZ = 0.D+0
          NPOLYC = LPOLYC
          DO 1920 NC = 5,NPOLYC
            SLZ = SLZ + CPLY_SL(NC,NP,IZN)*(LOG10(HDGLU)**(NC-5))
 1920     CONTINUE
          DO 1930 NC = 6,NPOLYC
            REALX = REAL(NC-5)
            DSLZ = DSLZ + REALX*CPLY_SL(NC,NP,IZN)* &
            (LOG10(HDGLU)**(NC-6))/(HDGLU*2.302585093D+0)
 1930     CONTINUE
          F = SLX - SLZ
          DF = -DSLZ
          DH = -F/(DF+SMALL)
          IF( HDGLU+DH.LT.0.D+0 ) DH = 6.D-1*DH
          HDGLU = HDGLU + DH
          HDGLU = MAX( HDGLU,0.95D+0*CPLY_SL(1,NP,IZN) )
          HDGLU = MIN( HDGLU,1.05D+0*CPLY_SL(2,NP,IZN) )
          IF( ABS(DH).GT.1.D-8 .AND. ABS(F).GT.1.D-8 ) GOTO 1910
          HDGL = HDGLU*SCHR(1,IZN)
          CPGL = HDGL*RHORL*GRAV
        ENDIF
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of CAP1 group  ---
!
      RETURN
      END