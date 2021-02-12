

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE CHK1
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
!
!----------------------Description-------------------------------------!
!
!     Water Mode
!
!     Check the thermodynamic and hydrologic states declared through
!     user inputs.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 22, 1997.




!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOURC
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
      USE HYST
      USE GRID
      USE FDVP
!      USE FDVD
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
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      EXTERNAL SCALING
      LOGICAL :: use_ga
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/CHK1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Dual porosity reassignment of van Genuchten or Brooks Corey
!     characteristic functions  ---
!
      DO 10 IZN = 1,num_nodes
        IF( ISCHR(IZN).EQ.3 .AND. ABS(IDP(IZN)).EQ.1 ) ISCHR(IZN) = 1
        IF( ISCHR(IZN).EQ.4 .AND. ABS(IDP(IZN)).EQ.1 ) ISCHR(IZN) = 2
   10 CONTINUE
!
!---  Scaling factors  ---
!
      IF( ISLC(19).EQ.1 ) THEN
!
!---    Simple scaling  ---
!
        DO 20 IZN = 1,num_nodes
          PERM(1,IZN) = SCALING( GAMMA(1,IZN),PERM(1,IZN),IGAMMA(1) )
          PERM(2,IZN) = SCALING( GAMMA(1,IZN),PERM(2,IZN),IGAMMA(1) )
          PERM(3,IZN) = SCALING( GAMMA(1,IZN),PERM(3,IZN),IGAMMA(1) )
          PERM(6,IZN) = SCALING( GAMMA(1,IZN),PERM(6,IZN),IGAMMA(1) )
          PERM(7,IZN) = SCALING( GAMMA(6,IZN),PERM(7,IZN),IGAMMA(1) )
          PERM(8,IZN) = SCALING( GAMMA(6,IZN),PERM(8,IZN),IGAMMA(1) )
          PERM(9,IZN) = SCALING( GAMMA(6,IZN),PERM(9,IZN),IGAMMA(1) )
          POR(1,IZN) = SCALING( GAMMA(2,IZN),POR(1,IZN),IGAMMA(2) )
          POR(2,IZN) = SCALING( GAMMA(2,IZN),POR(2,IZN),IGAMMA(2) )
          POR(3,IZN) = SCALING( GAMMA(7,IZN),POR(3,IZN),IGAMMA(2) )
          POR(4,IZN) = SCALING( GAMMA(7,IZN),POR(4,IZN),IGAMMA(2) )
          SCHR(1,IZN) = SCALING( GAMMA(3,IZN),SCHR(1,IZN),IGAMMA(3) )
          SCHR(3,IZN) = SCALING( GAMMA(4,IZN),SCHR(3,IZN),IGAMMA(4) )
!
!---      Scaling for defaulted Mualem/Burdine parameters  ---
!
          IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
            IF( SCHR(14,IZN)/EPSL.LE.EPSL ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                SCHR(11,IZN) = 1.D+0 - 2.D+0/(SCHR(3,IZN)+SMALL)
              ELSE
                SCHR(11,IZN) = 1.D+0 - 1.D+0/(SCHR(3,IZN)+SMALL)
              ENDIF
            ENDIF
          ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
            IF( SCHR(14,IZN)/EPSL.LE.EPSL ) THEN
              IF( MOD( IRPL(IZN),100 ).EQ.2 ) THEN
                SCHR(11,IZN) = SCHR(3,IZN)
              ELSE
                SCHR(11,IZN) = SCHR(3,IZN)
              ENDIF
            ENDIF
          ENDIF
!
!---      Scaling for Mualem-Anisotropy parameters  ---
!
          IF( ISCHR(IZN).EQ.1 .OR. ISCHR(IZN).EQ.101 ) THEN
            IF( IRPL(IZN).EQ.301 ) THEN
              SCHR(11,IZN) = SCALING( GAMMA(11,IZN),SCHR(11,IZN), &
              IGAMMA(4) )
              SCHR(12,IZN) = SCALING( GAMMA(12,IZN),SCHR(12,IZN), &
              IGAMMA(4) )
              SCHR(13,IZN) = SCALING( GAMMA(13,IZN),SCHR(13,IZN), &
              IGAMMA(4) )
            ENDIF
          ELSEIF( ISCHR(IZN).EQ.2 .OR. ISCHR(IZN).EQ.102 ) THEN
            IF( IRPL(IZN).EQ.301 ) THEN
              SCHR(11,IZN) = SCALING( GAMMA(11,IZN),SCHR(11,IZN), &
              IGAMMA(4) )
              SCHR(12,IZN) = SCALING( GAMMA(12,IZN),SCHR(12,IZN), &
              IGAMMA(4) )
              SCHR(13,IZN) = SCALING( GAMMA(13,IZN),SCHR(13,IZN), &
              IGAMMA(4) )
            ENDIF
          ENDIF
          SCHR(4,IZN) = SCALING( GAMMA(5,IZN),SCHR(4,IZN),IGAMMA(5) )
          SCHR(5,IZN) = SCALING( GAMMA(8,IZN),SCHR(5,IZN),IGAMMA(3) )
          SCHR(6,IZN) = SCALING( GAMMA(9,IZN),SCHR(6,IZN),IGAMMA(4) )
          SCHR(7,IZN) = SCALING( GAMMA(10,IZN),SCHR(7,IZN),IGAMMA(5) )
          PERM(4,IZN) = PERM(1,IZN)
          PERM(1,IZN) = PERM(4,IZN)*(1.D+0-POR(4,IZN)) + &
          PERM(7,IZN)*POR(4,IZN)
          PERM(5,IZN) = PERM(2,IZN)
          PERM(2,IZN) = PERM(5,IZN)*(1.D+0-POR(4,IZN)) + &
          PERM(8,IZN)*POR(4,IZN)
          PERM(6,IZN) = PERM(3,IZN)
          PERM(3,IZN) = PERM(6,IZN)*(1.D+0-POR(4,IZN)) + &
          PERM(9,IZN)*POR(4,IZN)
   20   CONTINUE
      ENDIF
!
!---  Compute at reference aqueous density  ---
!
      TX = 2.D+1
      PX = PATM
      CALL WATLQD( TX,PX,RHORL )
!
!---  Check initial pressures and temperature  ---
!
      INDX = 0
      DO 100 N = 1,num_nodes
        IF( IXP(N).EQ.0 ) GOTO 100
        DO M=1,ISVC+2
          T(M,N) = T(2,N)
          PL(M,N) = PL(2,N)
          PG(M,N) = PG(2,N)
          SL(M,N) = SL(2,N)
          SG(M,N) = SG(2,N)
        ENDDO
!        IZN = IZ(N)
        IF( T(2,N).GT.TMX .OR. T(2,N).LT.TMN ) THEN
          INDX = 16
          IMSG = N
          RLMSG = T(2,N)
          CHMSG = 'Initial Temperature(C) @ Node'
          CALL WRMSGS( INDX )
        ENDIF
        IF( PL(2,N).GT.PMX-PATM ) THEN
          INDX = 16
          IMSG = N
          CHMSG = 'Initial Aqueous Pressure(Pa) @ Node'
          RLMSG = PL(2,N)+PATM
          CALL WRMSGS( INDX )
        ENDIF
        IF( SL(2,N).GT.1.D+0 .OR. SL(2,N).LT.0.D+0 ) THEN
          INDX = 16
          IMSG = N
          CHMSG = 'Initial Aqueous Saturation @ Node'
          RLMSG = SL(2,N)
          CALL WRMSGS( INDX )
        ENDIF
        IF( PG(2,N).GT.PMX-PATM .OR. PG(2,N).LT.PMN-PATM ) THEN
          INDX = 16
          IMSG = N
          CHMSG = 'Initial Gas Pressure(Pa) @ Node'
          RLMSG = PG(2,N)+PATM
          CALL WRMSGS( INDX )
        ENDIF
        SGT(2,N) = SG(2,N)

!
!---    Load reactive transport total and diffusive porosity  ---
!
         if(islc(40).eq.1) then
          POR_M(1,N) = POR(1,N)
          POR_M(2,N) = POR(2,N)
         endif       

  100 CONTINUE
      IF( INDX.GT.0 ) STOP

!
!---  Compute capillary pressure head for water entry on the
!     main wetting path for rocks using the van Genuchten or
!     Brooks and Corey triple curve saturation functions  ---
!
      call add_node_dfield('hcmwe', idx)
      hcmwe => d_nd_fld(idx)%p
      hcmwe = 0.d0
      DO 120 IZN = 1,NUM_NODES
        IF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
          SLX = 9.9D-1*(1.D+0-SCHR(6,IZN)) + SCHR(6,IZN)
          SGTX = 0.D+0
          SLOX = 0.D+0
          CPGLOX = 0.D+0
          IPHX = 2
          CALL CAP1( IZN,SLX,SGTX,CPGL,SLOX,CPGLOX,IPHX )
          HCMWE(IZN) = CPGL/RHORL/GRAV
        ENDIF
  120 CONTINUE
!
!---  Compute saturated vapor pressures and aqueous density  ---
!
      DO 160 N = 1,num_nodes
        IF( IXP(N).EQ.0 ) GOTO 160
        CALL WATSP( T(2,N),PSW(2,N) )
        PX = MAX( PL(2,N)+PATM,PG(2,N)+PATM,PSW(2,N) )
        CALL WATLQD( T(2,N),PX,RHOL(2,N) )
  160 CONTINUE
!
!---  Compute the total trapping number  ---
!
!      CALL TRPGL1
!
!---  Return for restart simulations  ---
!
      IF( IEO.EQ.2 .OR. IEO.EQ.4 ) THEN
!
!---    Establish reference pressure for soil compressibility  ---
!
        DO 170 N = 1,num_nodes
          IF( IXP(N).EQ.0 ) GOTO 170
!          IZN = IZ(N)
          izn = n
          IF( CMP(3,IZN).GT.PATM ) THEN
            PCMP(N) = CMP(3,IZN)
          ELSE
            PCMP(N) = MAX( PL(2,N),PG(2,N) )+PATM
          ENDIF
  170   CONTINUE
        GOTO 248
      ENDIF
!
!---  Compute initial saturation conditions  ---
!
      IF( ISIC.EQ.1 .OR. ISIC.EQ.11 ) THEN
        DO 210 N = 1,num_nodes
          IF( IXP(N).EQ.0 ) GOTO 210
!          IZN = IZ(N)
          izn = n
!
!---      Moisture content initial condition  ---
!
          IF( ISIC.EQ.11 ) THEN
            SL(2,N) = 1.D+0
            IF( POR(2,IZN)/EPSL.GT.EPSL ) SL(2,N) = RHOG(2,N)/POR(2,IZN)
            SL(2,N) = MIN( SL(2,N),1.D+0 )
          ENDIF
          IF( SL(2,N).LT.SCHR(4,IZN) ) THEN
            IF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
             IF( SL(2,N).LT.SCHR(6,IZN) ) THEN
               INDX = 16
               IMSG = N
               RLMSG = SL(2,N)
               CHMSG = 'Initial Saturation < Residual Saturation @ Node'
               CALL WRMSGS( INDX )
             ELSE
               IPH(2,N) = 2
             ENDIF
            ELSE
              INDX = 16
              IMSG = N
              RLMSG = SL(2,N)
              CHMSG = 'Initial Saturation < Residual Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
            SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/(SCHR(9,IZN)+SMALL))
            IF( SGRMX.GT.EPSL ) THEN
              IF( SGT(2,N).GT.1.D+2 ) THEN
                SGT(2,N) = SGT(2,N)-1.D+2
                IF( SGT(2,N).GT.1.D+0 .OR. SGT(2,N).LT.0.D+0 ) THEN
                  INDX = 17
                  N_DB = N
                  CHMSG = 'Initial Relative Trapped Gas Saturation: '
                  CALL WRMSGS( INDX )
                ENDIF
                R = 1.D+0/SGRMX - 1.D+0
                ESLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
                ASGTX = (-((-1.D+0 - SGRMX + ESLX)*R) -        & 
                SQRT(((-1.D+0 - SGRMX + ESLX)**2)*(R**2) - &
                4.D+0*R*(-1.D+0 + SGRMX + ESLX + SGRMX*R - &
                SGRMX*ESLX*R)))/(2.D+0*R)
                ASGTX = SGT(2,N)*ASGTX
                SGT(2,N) = ASGTX*(1.D+0-SCHR(4,IZN))
              ELSE
                R = 1.D+0/SGRMX - 1.D+0
                ASGTX = SGT(2,N)/(1.D+0-SCHR(4,IZN))
              ENDIF
            ELSE
              ASGTX = 0.D+0
            ENDIF
            ASLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN)) + ASGTX
            ASLMIN(2,N) = (ASLX - ASGTX - 2.D+0*R*ASGTX + ASLX*R*ASGTX &
            - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)/(1.D+0 - R*ASGTX &
            - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)
            IF( ASLMIN(2,N).LT.ZERO ) THEN
              INDX = 16
              IMSG = N
              RLMSG = SGT(2,N)
              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          CPGLO = PG(1,N)-PL(1,N)
          CALL CAP1( IZN,SL(2,N),SGT(2,N),CPGL,SL(1,N),CPGLO,IPH(2,N) )
          PL(2,N) = PG(2,N) - CPGL
          PX = MAX( PG(2,N),PL(2,N) )

          if(islc(40).eq.1) then
           POR(5,IZN) = POR_M(1,N)
           POR(6,IZN) = POR_M(2,N)
          endif

          CALL PORSTY(IZN,PX,PX,PORD(2,N),PORT(2,N))
          DO 209 M = 1,ISVC+2
            T(M,N) = T(2,N)
            PL(M,N) = PL(2,N)
            PG(M,N) = PG(2,N)
  209     CONTINUE
          ASLMIN(1,N) = ASLMIN(2,N)
          SL(1,N) = SL(2,N)
  210   CONTINUE
      ELSEIF( ISIC.EQ.2 .OR. ISIC.EQ.12 ) THEN
        DO 220 N = 1,num_nodes
          IF( IXP(N).EQ.0 ) GOTO 220
          IZN = N
!
!---      Moisture content initial condition  ---
!
          IF( ISIC.EQ.12 ) THEN
            SL(2,N) = 1.D+0
            IF( POR(2,IZN)/EPSL.GT.EPSL ) SL(2,N) = RHOG(2,N)/POR(2,IZN)
            SL(2,N) = MIN( SL(2,N),1.D+0 )
          ENDIF
          IF( SL(2,N).LT.SCHR(4,IZN) ) THEN
            IF( ISCHR(IZN).EQ.301 .OR. ISCHR(IZN).EQ.302 ) THEN
             IF( SL(2,N).LT.SCHR(6,IZN) ) THEN
               INDX = 16
               IMSG = N
               RLMSG = SL(2,N)
               CHMSG = 'Initial Saturation < Residual Saturation @ Node'
               CALL WRMSGS( INDX )
             ELSE
               IPH(2,N) = 2
             ENDIF
            ELSE
              INDX = 16
              IMSG = N
              RLMSG = SL(2,N)
              CHMSG = 'Initial Saturation < Residual Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
            SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/(SCHR(9,IZN)+SMALL))
            IF( SGRMX.GT.EPSL ) THEN
              IF( SGT(2,N).GT.1.D+2 ) THEN
                SGT(2,N) = SGT(2,N)-1.D+2
                IF( SGT(2,N).GT.1.D+0 .OR. SGT(2,N).LT.0.D+0 ) THEN
                  INDX = 17
                  N_DB = N
                  CHMSG = 'Initial Relative Trapped Gas Saturation: '
                  CALL WRMSGS( INDX )
                ENDIF
                R = 1.D+0/SGRMX - 1.D+0
                ESLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN))
                ASGTX = (-((-1.D+0 - SGRMX + ESLX)*R) -		     &
                SQRT(((-1.D+0 - SGRMX + ESLX)**2)*(R**2) - &
                4.D+0*R*(-1.D+0 + SGRMX + ESLX + SGRMX*R - &
                SGRMX*ESLX*R)))/(2.D+0*R)
                ASGTX = SGT(2,N)*ASGTX
                SGT(2,N) = ASGTX*(1.D+0-SCHR(4,IZN))
              ELSE
                R = 1.D+0/SGRMX - 1.D+0
                ASGTX = SGT(2,N)/(1.D+0-SCHR(4,IZN))
              ENDIF
            ELSE
              ASGTX = 0.D+0
            ENDIF
            ASLX = (SL(2,N)-SCHR(4,IZN))/(1.D+0-SCHR(4,IZN)) + ASGTX
            ASLMIN(2,N) = (ASLX - ASGTX - 2.D+0*R*ASGTX + ASLX*R*ASGTX  &
            - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)/(1.D+0 - R*ASGTX &
            - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)
            IF( ASLMIN(2,N).LT.ZERO ) THEN
              INDX = 16
              IMSG = N
              RLMSG = SGT(2,N)
              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
          CPGLO = PG(1,N)-PL(1,N)
          CALL CAP1( IZN,SL(2,N),SGT(2,N),CPGL,SL(1,N),CPGLO,IPH(2,N) )
          PG(2,N) = PL(2,N) + CPGL
          PX = MAX( PG(2,N),PL(2,N) )
          if(islc(40).eq.1) then
           POR(5,IZN) = POR_M(1,N)
           POR(6,IZN) = POR_M(2,N)
          endif
          CALL PORSTY(IZN,PX,PX,PORD(2,N),PORT(2,N))
          DO 219 M = 1,ISVC+2
            T(M,N) = T(2,N)
            PL(M,N) = PL(2,N)
            PG(M,N) = PG(2,N)
  219     CONTINUE
          ASLMIN(1,N) = ASLMIN(2,N)
          SL(1,N) = SL(2,N)
  220   CONTINUE
      ELSEIF( ISIC.EQ.3 ) THEN
        INDX = 0
        DO 230 N = 1,num_nodes
          IF( IXP(N).le.0 ) cycle
          IZN = N
          MX = 2
          INDX = 2
          CALL KSP1( N,IZN,MX,PG(2,N),PL(2,N),SL(2,N),RKL(1,2,N), &
          ASLX,ASLMIN(2,N),ASGTX,SGRMX,INDX,IPH(2,N), &
          PG(1,N),PL(1,N),SL(1,N) )
!          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
!            SGRMX = SCHR(15,IZN)/(1.D+0+TRPGL(2,N)/(SCHR(9,IZN)+SMALL))
!            IF( SGRMX.GT.EPSL ) THEN
!              IF( SGT(2,N).GT.1.D+2 ) THEN
!                SGT(2,N) = SGT(2,N)-1.D+2
!                IF( SGT(2,N).GT.1.D+0 .OR. SGT(2,N).LT.0.D+0 ) THEN
!                  INDX = 17
!                  N_DB = N
!                  CHMSG = 'Initial Relative Trapped Gas Saturation: '
!                  CALL WRMSGS( INDX )
!                ENDIF
!                R = 1.D+0/SGRMX - 1.D+0
!                ASGTX = SGRMX - (1.D+0-ASLX)/(1.D+0 + R*(1.D+0-ASLX))
!                ASGTX = SGT(2,N)*ASGTX
!                SGT(2,N) = ASGTX*(1.D+0-SCHR(4,IZN))
!              ELSE
!                R = 1.D+0/SGRMX - 1.D+0
!                ASGTX = SGT(2,N)/(1.D+0-SCHR(4,IZN))
!              ENDIF
!            ELSE
!              ASGTX = 0.D+0
!            ENDIF
!            IF( ASGTX-SGRMX.GT.EPSL ) THEN
!              INDX = 16
!              IMSG = N
!              RLMSG = SGT(2,N)
!              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
!              CALL WRMSGS( INDX )
!            ENDIF
!          ENDIF
!          IF( ISCHR(IZN).EQ.101 .OR. ISCHR(IZN).EQ.102 ) THEN
!            ASLMIN(2,N) = (ASLX - ASGTX - 2.D+0*R*ASGTX + ASLX*R*ASGTX &
!            - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)/(1.D+0 - R*ASGTX &
!            - (R**2)*ASGTX + ASLX*(R**2)*ASGTX)
!            IF( ASLMIN(2,N).LT.ZERO ) THEN
!              INDX = 16
!              IMSG = N
!              RLMSG = SGT(2,N)
!              CHMSG = 'Initial Aqueous Trapped Gas Saturation @ Node'
!              CALL WRMSGS( INDX )
!            ENDIF
!          ENDIF
          PX = MAX( PG(2,N),PL(2,N) )
          if(islc(40).eq.1) then
           POR(5,IZN) = POR_M(1,N)
           POR(6,IZN) = POR_M(2,N)
          endif

          CALL PORSTY(IZN,PX,PX,PORD(2,N),PORT(2,N))
          DO 229 M = 1,ISVC+2
            T(M,N) = T(2,N)
            PL(M,N) = PL(2,N)
            PG(M,N) = PG(2,N)
  229     CONTINUE
          ASLMIN(1,N) = ASLMIN(2,N)
          SL(1,N) = SL(2,N)
  230   CONTINUE
      ENDIF
!
!---    Establish reference pressure for soil compressibility  ---
!
      DO 240 N = 1,num_nodes
        IF( IXP(N).EQ.0 ) GOTO 240
        IZN = N
        IF( CMP(3,IZN).GT.PATM ) THEN
          PCMP(N) = CMP(3,IZN)
        ELSE
          PCMP(N) = MAX( PL(2,N),PG(2,N) )+PATM
        ENDIF
  240 CONTINUE
!
!---  Normal and restart simulations  ---
!
  248 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of CHK1 group  ---
!
      RETURN
      END
