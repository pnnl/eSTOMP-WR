!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE PORSTY( IZN,PX,PREFX,PORDX,PORTX )
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
!     Compute diffusive and total porosities.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, Battelle, PNL, December, 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: porsty.F,v 1.11 2007/04/26 22:27:00 d3c002 Exp $
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE SOLTN
      USE PORMED
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
      SUBNMX = '/PORSTY'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(134)(1:1),'$').EQ.0 ) CVS_ID(134) = &
      '$Id: porsty.F,v 1.11 2007/04/26 22:27:00 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DPX = PX-PREFX
!
!---  Pore compressibility  ---
!
      IF( ISLC(15).EQ.1 )THEN
!
!---  Fracture properties (dual porosity model)  ---
!
        IF( ABS(IDP(IZN)).EQ.1 ) THEN
!          PORTX = POR(3,IZN) + DPX*CMP(2,IZN)
!          PORDX = POR(4,IZN) + DPX*CMP(2,IZN)
           PORTX = POR(3,IZN)*EXP(DPX*CMP(2,IZN))
           PORDX = POR(4,IZN)*EXP(DPX*CMP(2,IZN))
!
!---  Matrix properties (dual porosity model)  ---
!
        ELSEIF( ABS(IDP(IZN)).EQ.2 ) THEN
!         PORTX = POR(1,IZN) + DPX*CMP(1,IZN)
!         PORDX = POR(2,IZN) + DPX*CMP(1,IZN)
          PORTX = POR(1,IZN)*EXP(DPX*CMP(1,IZN))
          PORDX = POR(2,IZN)*EXP(DPX*CMP(1,IZN))

        ELSE
!       
!---      Reactive transport porosity alteration  ---
!       
          IF( ISLC(43).EQ.1 ) THEN
!            PORTX = POR(5,IZN) + DPX*CMP(1,IZN)
!            PORDX = POR(6,IZN) + DPX*CMP(1,IZN)
             PORTX = POR(5,IZN)*EXP(DPX*CMP(1,IZN))
             PORDX = POR(6,IZN)*EXP(DPX*CMP(1,IZN))
          ELSE
!            PORTX = POR(3,IZN) + (1.D+0-POR(3,IZN))*POR(1,IZN) + &
!            DPX*CMP(1,IZN)
!            PORDX = POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) + &
!            DPX*CMP(1,IZN)
              PORTX = POR(3,IZN) + (1.D+0-POR(3,IZN))*POR(1,IZN) &
               *EXP(DPX*CMP(1,IZN))
              PORDX = POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) &
               *EXP(DPX*CMP(1,IZN))
       
          ENDIF
        ENDIF
!
!---  Bulk compressibility  ---
!
      ELSE
!
!---    Fracture properties (dual porosity model)  ---
!
        IF( ABS(IDP(IZN)).EQ.1 ) THEN
          PORTX = POR(3,IZN) + DPX*CMP(2,IZN)
          PORDX = POR(4,IZN) + DPX*CMP(2,IZN)
!
!---    Matrix properties (dual porosity model)  ---
!
        ELSEIF( ABS(IDP(IZN)).EQ.2 ) THEN
          PORTX = POR(1,IZN) + DPX*CMP(1,IZN)
          PORDX = POR(2,IZN) + DPX*CMP(1,IZN)
        ELSE
!
!---      Reactive transport porosity alteration  ---
!
!          IF( ISLC(43).EQ.1 .or. islc(43).eq.2) THEN
          IF( ISLC(43).EQ.1) THEN
            PORTX = POR(5,IZN) + DPX*CMP(1,IZN)
            PORDX = POR(6,IZN) + DPX*CMP(1,IZN)
          ELSE
            PORTX = POR(3,IZN) + (1.D+0-POR(3,IZN))*POR(1,IZN) &
             + DPX*CMP(1,IZN)
            PORDX = POR(4,IZN) + (1.D+0-POR(4,IZN))*POR(2,IZN) &
             + DPX*CMP(1,IZN)
          ENDIF
        ENDIF
      ENDIF
      PORTX = MAX( MIN( PORTX,1.D+0 ),1.D-6 )
      PORDX = MAX( MIN( PORDX,1.D+0 ),1.D-6 )
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of PORSTY group  ---
!
      RETURN
      END
