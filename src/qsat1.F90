

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE QSAT1( PLX,PGX,DPGLX,N )
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
!     Calculate pressure differential required to yield a differential
!     change in saturation.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, February, 2001.
!     Last Modified by MD White, Battelle, February 9, 2001.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
      USE GRID
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
      SUBNMX = '/QSAT1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Zonation index   ---
!
      IZN = N
!
!---  van Genuchten saturation functions  ---
!
      IF( ISCHR(IZN).EQ.1 ) THEN
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
        IF( SLP.LT.5.D-1 ) THEN
          SLP = SLP + 1.D-7
        ELSE
          SLP = SLP - 1.D-7
        ENDIF
        HAW = ((-1.D+0 + (1.D+0/SLP)**(1.D+0/CM))**(1.D+0/CN))/ &
        SCHR(1,IZN)
        DPGLX = HAW*RHORL*GRAV + PLX - PGX
!
!---  Brooks and Corey saturation functions  ---
!
      ELSEIF( ISCHR(IZN).EQ.2 ) THEN
        HDGL = MAX( (PGX-PLX)/RHORL/GRAV,1.D-14 )
        CL = MAX( SCHR(3,IZN),SMALL )
        IF( HDGL.LE.SCHR(1,IZN) ) THEN
          SLP = 1.D+0
        ELSE
          SLP = (SCHR(1,IZN)/HDGL)**CL
        ENDIF
        IF( SLP.LT.5.D-1 ) THEN
          SLP = SLP + 1.D-6
        ELSE
          SLP = SLP - 1.D-6
        ENDIF
        HAW = ((1.D+0/SLP)**(1.D+0/CL))*SCHR(1,IZN)
        DPGLX = HAW*RHORL*GRAV + PLX - PGX
      ENDIF
!
!---  End of QSAT1 group  ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      RETURN
      END
