

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE ELC1
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
!     Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, October 2000.
!     Last Modified by MD White, PNNL, October 11, 2000.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE JACOB
      USE GRID
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
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/ELC1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      DO 400 N = 1,NFLD
        IF( IXP(N).LE.0 ) GOTO 400
        PGX = PG(2,N) + PATM
        DO 300 M = 2,ISVC+2
          PLX = PL(M,N) + PATM
          CALL WATSP( T(2,N),PVW(M,N) )
          PX = MAX( PLX,PGX,PVW(M,N) )
          CALL WATLQD( T(2,N),PX,RHOL(M,N) )
          CALL WATLQV( T(2,N),PX,PSW(2,N),VISL(M,N) )
          CLX = C(NSL_ELC,N)*YL(NSL_ELC,N)/(SL(M,N)*PORD(M,N)+SMALL)
          XLW(M,N) = RHOL(M,N)
          CALL ELC_DEN( RHOL(M,N),CLX,ELC_DCF )
          XLW(M,N) = XLW(M,N)/RHOL(M,N)
          CALL ELC_VIS( VISL(M,N),CLX,ELC_VCF )
  300   CONTINUE
  400 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of ELC1 group  ---
!
      RETURN
      END
