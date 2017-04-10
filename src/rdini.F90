!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINIP( DFV,DVAR,DADD,IFV,IVAR,LO,HI,LDXX,IDOM )
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
!     Load primary variables w/ initial conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE CONST
      USE GRID_MOD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IDOM(6),IVAR,IFV(*),ILIM(6)
      INTEGER LO(3), HI(3),LDXX(3)
      REAL*8 DVAR(*),DFV(*)
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      SUBNMX = '/RDINIP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      IDOM(1) = MAX( 1,IDOM(1) )
      IDOM(1) = MIN( IDOM(1),IDOM(2),nxdim )
      IDOM(2) = MAX( 1,IDOM(1),IDOM(2) )
      IDOM(2) = MIN( IDOM(2),nxdim )
      IDOM(3) = MAX( 1,IDOM(3) )
      IDOM(3) = MIN( IDOM(3),IDOM(4),nydim )
      IDOM(4) = MAX( 1,IDOM(3),IDOM(4) )
      IDOM(4) = MIN( IDOM(4),nydim )
      IDOM(5) = MAX( 1,IDOM(5) )
      IDOM(5) = MIN( IDOM(5),IDOM(6),nzdim )
      IDOM(6) = MAX( 1,IDOM(5),IDOM(6) )
      IDOM(6) = MIN( IDOM(6),nzdim )
!      WRITE(ISC,'(4X,A)') 'Domain: '
!      WRITE (ISC, '(6X,2(A,I6))') 'I = ',IDOM(1),  ' to ',IDOM(2)
!      WRITE (ISC, '(6X,2(A,I6))') 'J = ',IDOM(3),  ' to ',IDOM(4)
!      WRITE (ISC, '(6X,2(A,I6))') 'K = ',IDOM(5),  ' to ',IDOM(6)
      IF( IDOM(1) .GT. IDOM(2) .OR. IDOM(3) .GT. IDOM(4) .OR. &
        IDOM(5) .GT. IDOM(6) ) THEN
        NCH = INDEX( VARB(1:),'  ' )-1
        CHMSG = 'Invalid Initial Condition Domain: '//VARB(1:NCH)
        INDX = 4
        CALL WRMSGS( INDX )
      ENDIF
      ILIM(1) = LO(1)
      ILIM(2) = HI(1)
      ILIM(3) = LO(2)
      ILIM(4) = HI(2)
      ILIM(5) = LO(3)
      ILIM(6) = HI(3)
      CALL GET_LIM(ILIM, LDI, LDIJ)
      CALL GET_XYZ(IDOM(1),IDOM(3),IDOM(5),XM,YM,ZM)
!
!---  pH  ---
!
      IF( ABS(DADD-7.D+0)/EPSL.LT.EPSL ) THEN
        DO 100 K = LO(3),HI(3)
          DO 100 J = LO(2),HI(2)
            DO 100 I = LO(1),HI(1)
              IZZ = K - IAZMIN + 1
              IYY = J - IAYMIN + 1
              IXX = I - IAXMIN + 1
              IF( K.GE.IDOM(5).AND.K.LE.IDOM(6).AND. &
                J.GE.IDOM(3).AND.J.LE.IDOM(4).AND. &
                I.GE.IDOM(1).AND.I.LE.IDOM(2) )THEN
                  N = (IXX-1) + LDI*(IYY-1) + LDIJ*(IZZ-1) + 1
                  IF( IXP(N).LE.0 ) GOTO 100
                  DFV(N) = DVAR(1) + &
                    (D_XC(N) - XM)*DVAR(2) + &
                    (D_YC(N) - YM)*DVAR(3) + &
                    (D_ZC(N) - ZM)*DVAR(4)
                  DFV(N) = MIN( MAX( DFV(N),0.D+0 ), 1.4D+1 )
                  DFV(N) = 1.D+1**(-DFV(N))
                  IFV(N) = IVAR
              ENDIF
  100   CONTINUE
      ELSE
        DO 110 K = LO(3),HI(3)
          DO 110 J = LO(2),HI(2)
            DO 110 I = LO(1),HI(1)
              IZZ = K - IAZMIN + 1
              IYY = J - IAYMIN + 1
              IXX = I - IAXMIN + 1
              IF( K.GE.IDOM(5).AND.K.LE.IDOM(6).AND. &
                J.GE.IDOM(3).AND.J.LE.IDOM(4).AND. &
                I.GE.IDOM(1).AND.I.LE.IDOM(2) )THEN
                N = (IXX-1) + LDI*(IYY-1) + LDIJ*(IZZ-1) + 1
                IF( IXP(N).LE.0 ) GOTO 110
                DFV(N) = DVAR(1) + DADD + &
                  (D_XC(N) - XM)*DVAR(2) + &
                  (D_YC(N) - YM)*DVAR(3) + &
                  (D_ZC(N) - ZM)*DVAR(4)
                IFV(N) = IVAR
              ENDIF
  110   CONTINUE
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINIP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINIQ( DFV,DVAR,DADD,LO,HI,LDXX,IDOM )
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
!     Load primary variables w/ initial conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, October 1997.
!     Last Modified by MD White, Battelle, October 21, 1997.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE GRID_MOD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IDOM(6),ILIM(6)
      INTEGER LO(3), HI(3),LDXX(3)
      REAL*8 DVAR(*),DFV(2,*)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RDINIQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      IDOM(1) = MAX( 1,IDOM(1) )
      IDOM(1) = MIN( IDOM(1),IDOM(2),NDXIM )
      IDOM(2) = MAX( 1,IDOM(1),IDOM(2) )
      IDOM(2) = MIN( IDOM(2),NXDIM )
      IDOM(3) = MAX( 1,IDOM(3) )
      IDOM(3) = MIN( IDOM(3),IDOM(4),NYDIM )
      IDOM(4) = MAX( 1,IDOM(3),IDOM(4) )
      IDOM(4) = MIN( IDOM(4),NYDIM )
      IDOM(5) = MAX( 1,IDOM(5) )
      IDOM(5) = MIN( IDOM(5),IDOM(6),NZDIM )
      IDOM(6) = MAX( 1,IDOM(5),IDOM(6) )
      IDOM(6) = MIN( IDOM(6),NZDIM )
!     WRITE(ISC,'(4X,A)') 'Domain: '
!     WRITE (ISC, '(6X,2(A,I6))') 'I = ',IDOM(1),  ' to ',IDOM(2)
!     WRITE (ISC, '(6X,2(A,I6))') 'J = ',IDOM(3),  ' to ',IDOM(4)
!     WRITE (ISC, '(6X,2(A,I6))') 'K = ',IDOM(5),  ' to ',IDOM(6)
      IF( IDOM(1) .GT. IDOM(2) .OR. IDOM(3) .GT. IDOM(4) .OR. &
        IDOM(5) .GT. IDOM(6) ) THEN
        NCH = INDEX( VARB(1:),'  ' )-1
        CHMSG = 'Invalid Initial Condition Domain: '//VARB(1:NCH)
        INDX = 4
        CALL WRMSGS( INDX )
      ENDIF
      ILIM(1) = LO(1)
      ILIM(2) = HI(1)
      ILIM(3) = LO(2)
      ILIM(4) = HI(2)
      ILIM(5) = LO(3)
      ILIM(6) = HI(3)
      CALL GET_LIM(ILIM, LDI, LDIJ)
      CALL GET_XYZ(IDOM(1),IDOM(3),IDOM(5),XM,YM,ZM)
      DO 100 K = LO(3),HI(3)
        DO 100 J = LO(2),HI(2)
          DO 100 I = LO(1),HI(1)
            IZZ = K - IAZMIN + 1
            IYY = J - IAYMIN + 1
            IXX = I - IAXMIN + 1
            IF( K.GE.IDOM(5).AND.K.LE.IDOM(6).AND. &
              J.GE.IDOM(3).AND.J.LE.IDOM(4).AND. &
              I.GE.IDOM(1).AND.I.LE.IDOM(2) )THEN
              N = (IXX-1) + LDI*(IYY-1) + LDIJ*(IZZ-1) + 1
              IF( IXP(N).LE.0 ) GOTO 100
              DFV(2,N) = DVAR(1) + DADD + &
                (D_XC(N) - XM)*DVAR(2) + &
                (D_YC(N) - YM)*DVAR(3) + &
                (D_ZC(N) - ZM)*DVAR(4)
            ENDIF
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINIQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINIR( DFV,DVAR,DADD,LO,HI,LDXX,IDOM )
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
!     Load secondary variables w/ initial conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE GRID_MOD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IDOM(6),ILIM(6)
      INTEGER LO(3), HI(3),LDXX(3)
      REAL*8 DVAR(*),DFV(*)
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RDINIR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      IDOM(1) = MAX( 1,IDOM(1) )
      IDOM(1) = MIN( IDOM(1),IDOM(2),NXDIM )
      IDOM(2) = MAX( 1,IDOM(1),IDOM(2) )
      IDOM(2) = MIN( IDOM(2),NXDIM )
      IDOM(3) = MAX( 1,IDOM(3) )
      IDOM(3) = MIN( IDOM(3),IDOM(4),NYDIM )
      IDOM(4) = MAX( 1,IDOM(3),IDOM(4) )
      IDOM(4) = MIN( IDOM(4),NYDIM )
      IDOM(5) = MAX( 1,IDOM(5) )
      IDOM(5) = MIN( IDOM(5),IDOM(6),NZDIM )
      IDOM(6) = MAX( 1,IDOM(5),IDOM(6) )
      IDOM(6) = MIN( IDOM(6),NZDIM )
      WRITE (ISC,'(4X,A)') 'Domain: '
      WRITE (ISC,'(6X,2(A,I6))') 'I = ',IDOM(1),' to ',IDOM(2)
      WRITE (ISC,'(6X,2(A,I6))') 'J = ',IDOM(3),' to ',IDOM(4)
      WRITE (ISC,'(6X,2(A,I6))') 'K = ',IDOM(5),' to ',IDOM(6)
      IF( IDOM(1) .GT. IDOM(2) .OR. IDOM(3) .GT. IDOM(4) .OR. &
        IDOM(5) .GT. IDOM(6) ) THEN
        NCH = INDEX( VARB(1:),'  ' )-1
        CHMSG = 'Invalid Initial Condition Domain: '//VARB(1:NCH)
        INDX = 4
        CALL WRMSGS( INDX )
      ENDIF
      ILIM(1) = LO(1)
      ILIM(2) = HI(1)
      ILIM(3) = LO(2)
      ILIM(4) = HI(2)
      ILIM(5) = LO(3)
      ILIM(6) = HI(3)
      CALL GET_LIM(ILIM, LDI, LDIJ)
      CALL GET_XYZ(IDOM(1),IDOM(3),IDOM(5),XM,YM,ZM)
      DO 100 K = LO(3),HI(3)
        DO 100 J = LO(2),HI(2)
          DO 100 I = LO(1),HI(1)
            IZZ = K - IAZMIN + 1
            IYY = J - IAYMIN + 1
            IXX = I - IAXMIN + 1
            IF( K.GE.IDOM(5).AND.K.LE.IDOM(6).AND. &
              J.GE.IDOM(3).AND.J.LE.IDOM(4).AND. &
              I.GE.IDOM(1).AND.I.LE.IDOM(2) )THEN
              N = (IXX-1) + LDI*(IYY-1) + LDIJ*(IZZ-1) + 1
              IF( IXP(N).LE.0 ) GOTO 100
              DFV(N) = DVAR(1) + DADD + &
                (D_XC(N) - XM)*DVAR(2) + &
                (D_YC(N) - YM)*DVAR(3) + &
                (D_ZC(N) - ZM)*DVAR(4)
            ENDIF
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINIR group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINIS( DFV,DVAR,DADD,LO,HI,LDXX,IDOM,MM )
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
!     Load secondary variables w/ initial conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, December 31, 1992.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE FILES
      USE GRID_MOD
      USE GLB_PAR
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
#ifdef USE_H5HUT
     include "mpif.h"
#endif
#include "utils.h"
!
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IDOM(6),ILIM(6)
      INTEGER LO(3), HI(3),LDXX(3)
      REAL*8 DVAR(*),DFV(3,*)
      LOGICAL USE_GA, STATUS
!
!----------------------Executable Lines--------------------------------!
!
      SUBNMX = '/RDINIS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      USE_GA = .TRUE.
      
      IDOM(1) = MAX( 1,IDOM(1) )
      IDOM(1) = MIN( IDOM(1),IDOM(2),NXDIM )
      IDOM(2) = MAX( 1,IDOM(1),IDOM(2) )
      IDOM(2) = MIN( IDOM(2),NXDIM )
      IDOM(3) = MAX( 1,IDOM(3) )
      IDOM(3) = MIN( IDOM(3),IDOM(4),NYDIM )
      IDOM(4) = MAX( 1,IDOM(3),IDOM(4) )
      IDOM(4) = MIN( IDOM(4),nydim )
      IDOM(5) = MAX( 1,IDOM(5) )
      IDOM(5) = MIN( IDOM(5),IDOM(6),NZDIM )
      IDOM(6) = MAX( 1,IDOM(5),IDOM(6) )
      IDOM(6) = MIN( IDOM(6),NZDIM )
      IF( ME.EQ.0 ) THEN
       WRITE (ISC,'(4X,A)') 'Domain: '
       WRITE (ISC,'(6X,2(A,I6))') 'I = ',IDOM(1),' to ',IDOM(2)
       WRITE (ISC,'(6X,2(A,I6))') 'J = ',IDOM(3),' to ',IDOM(4)
       WRITE (ISC,'(6X,2(A,I6))') 'K = ',IDOM(5),' to ',IDOM(6)
      ENDIF
      IF( IDOM(1) .GT. IDOM(2) .OR. IDOM(3) .GT. IDOM(4) .OR. &
        IDOM(5) .GT. IDOM(6) ) THEN
        NCH = INDEX( VARB(1:),'  ' )-1
        CHMSG = 'Invalid Initial Condition Domain: '//VARB(1:NCH)
        INDX = 4
        CALL WRMSGS( INDX )
      ENDIF
      ILIM(1) = LO(1)
      ILIM(2) = HI(1)
      ILIM(3) = LO(2)
      ILIM(4) = HI(2)
      ILIM(5) = LO(3)
      ILIM(6) = HI(3)
      CALL GET_LIM(ILIM, LDI, LDIJ)
      CALL GET_XYZ(IDOM(1),IDOM(3),IDOM(5),XM,YM,ZM)
      DO 200 K = LO(3),HI(3)
        DO 200 J = LO(2),HI(2)
          DO 200 I = LO(1),HI(1)
            IZZ = K - IAZMIN + 1
            IYY = J - IAYMIN + 1
            IXX = I - IAXMIN + 1
            IF( K.GE.IDOM(5).AND.K.LE.IDOM(6).AND. &
              J.GE.IDOM(3).AND.J.LE.IDOM(4).AND. &
              I.GE.IDOM(1).AND.I.LE.IDOM(2) )THEN
              N = (IXX-1) + LDI*(IYY-1) + LDIJ*(IZZ-1) + 1
              IF( IXP(N).LE.0 ) GOTO 200
                DFV(MM,N) = DVAR(1) + DADD + &
                 (D_XC(N) - XM)*DVAR(2) + &
                 (D_YC(N) - YM)*DVAR(3) + &
                 (D_ZC(N) - ZM)*DVAR(4)
            ENDIF
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINIS group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINFP( DFV,DVAR,D_ADD,IFV,IVAR,UNTS )
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
!     Load primary variables w/ initial conditions from an external
!     file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, June 16, 1997.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE CONST
      USE GRID_MOD
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
      CHARACTER*64 UNTS
      INTEGER*4 IVAR,IFV(*)
      REAL*8 DVAR(*),DFV(*)
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3), G_BUF, GI_BUF,THREE
      REAL*8, ALLOCATABLE :: VAL_BUF(:), buf3(:,:,:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:),ival_buf(:),IBUF3(:,:,:)
      LOGICAL STATUS,use_ga
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      dims(1) = nxdim
      dims(2) = nydim
      dims(3) = nzdim
      three = 3
      g_buf = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(g_buf,GAGRP)
#endif
      call ga_set_data(g_buf, three, dims, MT_DBL)
      status = ga_allocate(g_buf)
!
      gi_buf = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(g_buf,GAGRP)
#endif
      call ga_set_data(gi_buf, three, dims, MT_INT)
      status = ga_allocate(gi_buf)

      SUBNMX = '/RDINFP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      ILUM = IUNM
      ILUS = IUNS
      ILUKG = IUNKG
      ILUK = IUNK
      ILUMOL = IUNMOL
!
!---  pH  ---
!
      IF( ABS(D_ADD-7.D+0)/EPSL.LT.EPSL ) THEN
! TODO: Fix up DVAR behavior
        DO 10 N = 1,num_nodes
          IF( IXP(N).LE.0 ) GOTO 10
          DVAR(1) = MIN( MAX( DVAR(1),0.D+0 ), 1.4D+1 )
          DFV(N) = 1.D+1**(-DVAR(1))
          IFV(N) = IVAR
   10   CONTINUE
        IF (ME.EQ.0) THEN
          NCOUNT = 0
          IJDIM = NXDIM*NYDIM*NZDIM
          ALLOCATE(IDX_BUF(3,IJDIM))
          ALLOCATE(VAL_BUF(IJDIM))
          ILD = IAXMAX - IAXMIN + 1
          IJLD = ILD * (IAYMAX - IAYMIN + 1)
   20     CONTINUE
          READ(26,*,END=30) I,J,K,VAR
          NCOUNT = NCOUNT + 1
          IDX_BUF(1,NCOUNT) = I
          IDX_BUF(2,NCOUNT) = J
          IDX_BUF(3,NCOUNT) = K
          IF( I.LT.1 .OR. I.GT.NXDIM .OR. J.LT.1 .OR. J.GT.NYDIM &
            .OR. K.LT.1 .OR. K.GT.NZDIM ) THEN
            CHMSG = 'Domain Index Out of Range: '
            INDX = 4
            CALL WRMSGS( INDX )
          ENDIF
          IXX = I - IAXMIN
          IYY = J - IAYMIN
          IZZ = K - IAZMIN
          N = IXX + IYY*ILD + IZZ*IJLD + 1
          IF( IXP(N).LE.0 ) GOTO 20
          INDX = 0
          IUNM = ILUM
          IUNS = ILUS
          IUNKG = ILUKG
          IUNK = ILUK
          IUNMOL = ILUMOL
          CALL RDUNIT( UNTS,VAR,INDX )
          VAL_BUF(NCOUNT) = 1.d+3*(1.d+1**(-VAR))
          IVAL_BUF(NCOUNT) = IVAR
!          IF( MOD(NCOUNT,IJDIM).EQ.0 ) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            CALL NGA_SCATTER(GI_BUF,IVAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
!          ENDIF
!          DFV(N) = 1.D+3*(1.D+1**(-VAR))
!          IFV(N) = IVAR
          GOTO 20
   30     CONTINUE
!          IF( MOD(NCOUNT,IJDIM).GT.0 ) THEN
!            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
!            CALL NGA_SCATTER(GI_BUF,IVAL_BUF(1),IDX_BUF(1,1),NCOUNT)
!            NCOUNT = 0
!          ENDIF
        ENDIF
        CALL GA_SYNC
        LDXX(1) = IAXMAX - IAXMIN + 1
        LDXX(2) = IAYMAX - IAYMIN + 1
        LDXX(3) = IAZMAX - IAZMIN + 1
        LO(1) = IAXMIN
        LO(2) = IAYMIN
        LO(3) = IAZMIN
        HI(1) = IAXMAX
        HI(2) = IAYMAX
        HI(3) = IAZMAX
        ALLOCATE(BUF3(LDXX(1),LDXX(2),LDXX(3)))
        ALLOCATE(IBUF3(LDXX(1),LDXX(2),LDXX(3)))
        BUF3 = 0.D0
        IBUF3 = 0
        CALL NGA_GET(G_BUF,LO,HI,BUF3(1,1,1),LDXX)
        CALL NGA_GET(GI_BUF,LO,HI,IBUF3(1,1,1),LDXX)
        N = 0
        DO K = 1,LDXX(3)
        DO J = 1,LDXX(2)
        DO I = 1,LDXX(1)
          N = N + 1
          IF(IXP(N).EQ.0 .AND. GRID_MASK(N).EQ.1 ) Then
            DFV(N) = BUF3(I,J,K)
            IFV(N) = IBUF3(I,J,K)
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        STATUS = GA_DESTROY(G_BUF)
        STATUS = GA_DESTROY(GI_BUF)
        DEALLOCATE(VAL_BUF)
        DEALLOCATE(IDX_BUF)
        DEALLOCATE(IVAL_BUF)
        CALL GA_SYNC
      ELSE
        do 110 n = 1,NUM_NODES
          IF( IXP(N).LE.0 ) GOTO 110
          DFV(N) = DVAR(1)+D_ADD
          IFV(N) = IVAR
  110   CONTINUE
        IF (ME.EQ.0) THEN
          NCOUNT = 0
          IJDIM = NXDIM*NYDIM*NZDIM
          ALLOCATE(IDX_BUF(3,IJDIM))
          ALLOCATE(VAL_BUF(IJDIM))
          ALLOCATE(IVAL_BUF(IJDIM))
  120     CONTINUE
          ILD = IAXMAX - IAXMIN + 1
          IJLD = ILD * (IAYMAX - IAYMIN + 1)
          READ(26,*,END=130) I,J,K,VAR
          NCOUNT = NCOUNT + 1
          IDX_BUF(1,NCOUNT) = I
          IDX_BUF(2,NCOUNT) = J
          IDX_BUF(3,NCOUNT) = K
          IF( I.LT.1 .OR. I.GT.NXDIM .OR. J.LT.1 .OR. J.GT.NYDIM &
            .OR. K.LT.1 .OR. K.GT.NZDIM ) THEN
            CHMSG = 'Domain Index Out of Range: '
            INDX = 4
            CALL WRMSGS( INDX )
          ENDIF
          IXX = I - IAXMIN
          IYY = J - IAYMIN
          IZZ = K - IAZMIN
          N = IXX + IYY*ILD + IZZ*IJLD + 1
!          IF( IXP(N).LE.0 ) GOTO 120
          INDX = 0
          IUNM = ILUM
          IUNS = ILUS
          IUNKG = ILUKG
          IUNK = ILUK
          IUNMOL = ILUMOL
          CALL RDUNIT( UNTS,VAR,INDX )
          VAL_BUF(NCOUNT) = VAR + D_ADD
          IVAL_BUF(NCOUNT) = IVAR
!          IF( MOD(NCOUNT,IJDIM).EQ.0 ) THEN
!           CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
!           CALL NGA_SCATTER(GI_BUF,IVAL_BUF(1),IDX_BUF(1,1),NCOUNT)
!           NCOUNT = 0
!          ENDIF
          !DFV(N) = VAR+D_ADD
          !IFV(N) = IVAR
          GOTO 120
  130     CONTINUE
!          IF( MOD(NCOUNT,IJDIM).GT.0 ) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUf(1,1),NCOUNT)
            CALL NGA_SCATTER(GI_BUF,IVAL_BUF(1),IDX_buf(1,1),NCOUNT)
!            NCOUNT = 0
!          ENDIF
        ENDIF
        CALL GA_SYNC
        LDXX(1) = IAXMAX - IAXMIN + 1
        LDXX(2) = IAYMAX - IAYMIN + 1
        LDXX(3) = IAZMAX - IAZMIN + 1
        LO(1) = IAXMIN
        LO(2) = IAYMIN
        LO(3) = IAZMIN
        HI(1) = IAXMAX
        HI(2) = IAYMAX
        HI(3) = IAZMAX
        ALLOCATE(BUF3(LDXX(1),LDXX(2),LDXX(3)))
        ALLOCATE(IBUF3(LDXX(1),LDXX(2),LDXX(3)))
        BUF3 = 0.D0
        IBUF3 = 0
        CALL NGA_GET(G_BUF,LO,HI,BUF3(1,1,1),LDXX)
        CALL NGA_GET(GI_BUF,LO,HI,IBUF3(1,1,1),LDXX)
        N = 0
        DO K = 1,LDXX(3)
        DO J = 1,LDXX(2)
        DO I = 1,LDXX(1)
          N = N + 1
          IF(IXP(N).GT.0 .AND. GRID_MASK(N).GT.0 ) Then
            DFV(N) = BUF3(I,J,K)
            IFV(N) = IBUF3(I,J,K)
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        STATUS = GA_DESTROY(G_BUF)
        STATUS = GA_DESTROY(GI_BUF)
        IF(ME.EQ.0)THEN
          DEALLOCATE(VAL_BUF)
          DEALLOCATE(IDX_BUF)
          DEALLOCATE(IVAL_BUF)
        ENDIF
        CALL GA_SYNC
      ENDIF
      IUNM = 0
      IUNS = 0
      IUNKG = 0
      IUNK = 0
      IUNMOL = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINFP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINFQ( DFV,DVAR,DADD,UNTS )
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
!     Load primary variables w/ initial conditions from an external
!     file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, June 16, 1997.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
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
      CHARACTER*64 UNTS
      REAL*8 DVAR(*),DFV(2,*)
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3), G_BUF, THREE, GI_BUF
      INTEGER, ALLOCATABLE :: BUF(:), VALI_BUF(:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:),BUFI3(:,:,:)
      REAL*8,ALLOCATABLE :: VAL_BUF(:), BUF3(:,:,:)
      LOGICAL STATUS,USE_GA
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
      THREE = 3
      G_BUF = GA_CREATE_HANDLE()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(g_buf,GAGRP)
#endif
      CALL GA_SET_DATA(G_BUF, THREE, DIMS, MT_DBL)
      STATUS = GA_ALLOCATE(G_BUF)
!
      SUBNMX = '/RDINFP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F,v 1.10 2006/11/06 23:31:21 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
      ILUM = IUNM
      ILUS = IUNS
      ILUKG = IUNKG
      ILUK = IUNK
      ILUMOL = IUNMOL
!
!---  pH  ---
!
      IF( ABS(DADD-7.D+0)/EPSL.LT.EPSL ) THEN
! TODO: Fix up DVAR behavior
        DO 10 N = 1,NUM_NODES
          IF( IXP(N).LE.0 ) GOTO 10
          DVAR(1) = MIN( MAX( DVAR(1),0.D+0 ), 1.4D+1 )
          DFV(2,N) = 1.D+1**(-DVAR(1))
!          IFV(2,N) = IVAR
   10   CONTINUE
        IF (ME.EQ.0) THEN
          NCOUNT = 0
          IJDIM = NXDIM*NYDIM
          ALLOCATE(IDX_BUF(3,IJDIM))
          ALLOCATE(VAL_BUF(IJDIM))
   20     CONTINUE
          ILD = IAXMAX - IAXMIN + 1
          IJLD = ILD * (IAYMAX - IAYMIN + 1)
          READ(26,*,END=30) I,J,K,VAR
          NCOUNT = NCOUNT + 1
          IDX_BUF(1,NCOUNT) = I
          IDX_BUF(2,NCOUNT) = J
          IDX_BUF(3,NCOUNT) = K
          IF( I.LT.1 .OR. I.GT.NXDIM .OR. J.LT.1 .OR. J.GT.NYDIM &
            .OR. K.LT.1 .OR. K.GT.NZDIM ) THEN
            CHMSG = 'Domain Index Out of Range: '
            INDX = 4
            CALL WRMSGS( INDX )
          ENDIF
          IXX = I - IAXMIN
          IYY = J - IAYMIN
          IZZ = K - IAZMIN
          N = IXX + IYY*ILD + IZZ*IJLD + 1
          IF( IXP(N).LE.0 ) GOTO 20
          INDX = 0
          IUNM = ILUM
          IUNS = ILUS
          IUNKG = ILUKG
          IUNK = ILUK
          IUNMOL = ILUMOL
          CALL RDUNIT( UNTS,VAR,INDX )
          VAL_BUF(NCOUNT) = 1.D+3*(1.D+1**(-VAR))
          IF( MOD(NCOUNT,IJDIM).EQ.0 ) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
          ENDIF
          GOTO 20
   30     CONTINUE
          IF( MOD(NCOUNT,IJDIM).GT.0 ) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
          ENDIF
          DEALLOCATE(VAL_BUF)
          DEALLOCATE(IDX_BUF)
        ENDIF
        CALL GA_SYNC
        LDXX(1) = IAXMAX - IAXMIN + 1
        LDXX(2) = IAYMAX - IAYMIN + 1
        LDXX(3) = IAZMAX - IAZMIN + 1
        LO(1) = IAXMIN
        LO(2) = IAYMIN
        LO(3) = IAZMIN
        HI(1) = IAXMAX
        HI(2) = IAYMAX
        HI(3) = IAZMAX
        ALLOCATE(BUF3(LDXX(1),LDXX(2),LDXX(3)))
        ALLOCATE(BUFI3(LDXX(1),LDXX(2),LDXX(3)))
        BUF3 = 0.D0
        BUFI3 = 0
        CALL NGA_GET(G_BUF,LO,HI,BUF3(1,1,1),LDXX)
        CALL NGA_GET(GI_BUF,LO,HI,BUFI3(1,1,1),LDXX)
        N = 0
        DO K = 1,LDXX(3)
        DO J = 1,LDXX(2)
        DO I = 1,LDXX(1)
          N = N + 1
          IF(IXP(N).EQ.0 .AND. GRID_MASK(N).EQ.1 ) THEN
            DFV(2,N) = BUF3(I,J,K)
!           IFV(2,N) = BUFI3(I,J,K)
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        STATUS = GA_DESTROY(G_BUF)
      ELSE
        DO 110 N = 1,num_nodes
          IF( IXP(N).LE.0 ) GOTO 110
          DFV(2,N) = DVAR(1)+DADD
!          IFV(2,N) = IVAR
  110   CONTINUE
        IF( ME.EQ.0 ) THEN
         NCOUNT = 0
         IJDIM = NXDIM*NYDIM
         ALLOCATE(IDX_BUF(3,IJDIM))
         ALLOCATE(VAL_BUF(IJDIM))
  120    CONTINUE
         ILD = IAXMAX - IAXMIN + 1
         IJLD = ILD * (IAYMAX - IAYMIN + 1)
         READ(26,*,END=130) I,J,K,VAR
         NCOUNT = NCOUNT + 1
         IDX_BUF(1,NCOUNT) = I
         IDX_BUF(2,NCOUNT) = J
         IDX_BUF(3,NCOUNT) = K
         IF( I.LT.1 .OR. I.GT.NXDIM .OR. J.LT.1 .OR. J.GT.NYDIM &
          .OR. K.LT.1 .OR. K.GT.NZDIM ) THEN
          CHMSG = 'Domain Index Out of Range: '
          INDX = 4
          CALL WRMSGS( INDX )
         ENDIF
         IXX = I - IAXMIN
         IYY = J - IAYMIN
         IZZ = K - IAZMIN
         N = IXX + IYY*ILD + IZZ*IJLD + 1
         IF( IXP(N).LE.0 ) GOTO 120
         INDX = 0
         IUNM = ILUM
         IUNS = ILUS
         IUNKG = ILUKG
         IUNK = ILUK
         IUNMOL = ILUMOL
         CALL RDUNIT( UNTS,VAR,INDX )
         VAL_BUF(NCOUNT) = VAR+DADD
         IF( MOD(NCOUNT,IJDIM).EQ.0 ) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
         ENDIF
         GOTO 120
  130    CONTINUE
!         if( mod(ncount,ijdim).gt.0 ) then
            call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
!            call nga_scatter(gi_buf,vali_buf(1),idx_buf(1,1),ncount)
!            ncount = 0
!         endif
         deallocate(val_buf)
         deallocate(idx_buf)
!         deallocate(vali_buf)
        endif
        call ga_sync
        ldxx(1) = iaxmax - iaxmin + 1
        ldxx(2) = iaymax - iaymin + 1
        ldxx(3) = iazmax - iazmin + 1
        lo(1) = iaxmin
        lo(2) = iaymin
        lo(3) = iazmin
        hi(1) = iaxmax
        hi(2) = iaymax
        hi(3) = iazmax
        allocate(buf3(ldxx(1),ldxx(2),ldxx(3)))
        allocate(bufi3(ldxx(1),ldxx(2),ldxx(3)))
        buf3 = 0.d0
        bufi3 = 0
        call nga_get(g_buf,lo,hi,buf3(1,1,1),ldxx)
        call nga_get(gi_buf,lo,hi,bufi3(1,1,1),ldxx)
        n = 0
        do k = 1,ldxx(3)
        do j = 1,ldxx(2)
        do i = 1,ldxx(1)
          n = n + 1
          if(ixp(n).eq.0 .and. grid_mask(n).eq.1 ) then
            dfv(2,n) = buf3(i,j,k)
!            ifv(2,n) = bufi3(i,j,k)
          endif
        enddo
        enddo
        enddo
        status = ga_destroy(g_buf)
         IF( MOD(NCOUNT,IJDIM).GT.0 ) THEN
            CALL NGA_SCATTER(G_BUF,VAL_BUF(1),IDX_BUF(1,1),NCOUNT)
            NCOUNT = 0
         ENDIF
         DEALLOCATE(VAL_BUF)
         DEALLOCATE(IDX_BUF)
!         DEALLOCATE(VALI_BUF)
        CALL GA_SYNC
        LDXX(1) = IAXMAX - IAXMIN + 1
        LDXX(2) = IAYMAX - IAYMIN + 1
        LDXX(3) = IAZMAX - IAZMIN + 1
        LO(1) = IAXMIN
        LO(2) = IAYMIN
        LO(3) = IAZMIN
        HI(1) = IAXMAX
        HI(2) = IAYMAX
        HI(3) = IAZMAX
        ALLOCATE(BUF3(LDXX(1),LDXX(2),LDXX(3)))
        ALLOCATE(BUFI3(LDXX(1),LDXX(2),LDXX(3)))
        BUF3 = 0.D0
        BUFI3 = 0
        CALL NGA_GET(G_BUF,LO,HI,BUF3(1,1,1),LDXX)
        CALL NGA_GET(GI_BUF,LO,HI,BUFI3(1,1,1),LDXX)
        N = 0
        DO K = 1,LDXX(3)
        DO J = 1,LDXX(2)
        DO I = 1,LDXX(1)
          N = N + 1
          IF(IXP(N).EQ.0 .AND. GRID_MASK(N).EQ.1 ) THEN
            DFV(2,N) = BUF3(I,J,K)
!            IFV(2,N) = BUFI3(I,J,K)
          ENDIF
        ENDDO
        ENDDO
        ENDDO
        STATUS = GA_DESTROY(G_BUF)
      ENDIF
      IUNM = 0
      IUNS = 0
      IUNKG = 0
      IUNK = 0
      IUNMOL = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINFP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINFR( DFV,DVAR,DADD,UNTS )
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
!     Load primary variables w/ initial conditions from an external
!     file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, June 16, 1997.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
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
      CHARACTER*64 UNTS
      REAL*8 DVAR(*),DFV(*)
!
      INTEGER DIMS(3), LO(3), HI(3), LDxx(3), G_BUF, THREE
      INTEGER, ALLOCATABLE :: BUF(:), vali_buf(:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:),bufi3(:,:,:)
      real*8,allocatable :: val_buf(:), buf3(:,:,:)
      LOGICAL STATUS,use_ga
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      dims(1) = nxdim
      dims(2) = nydim
      dims(3) = nzdim
      three = 3
      g_buf = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(g_buf,GAGRP)
#endif
      call ga_set_data(g_buf, three, dims, MT_INT)
      status = ga_allocate(g_buf)
!
      SUBNMX = '/RDINFR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      ILUM = IUNM
      ILUS = IUNS
      ILUKG = IUNKG
      ILUK = IUNK
      ILUMOL = IUNMOL
      DO 10 N = 1,num_nodes
        IF( IXP(N).LE.0 ) GOTO 10
        DFV(N) = DVAR(1)+DADD
   10 CONTINUE
      if (me.eq.0) then
        ncount = 0
        ijdim = nxdim*nydim
        allocate(idx_buf(3,ijdim))
        allocate(val_buf(ijdim))
     20 CONTINUE
        ild = iaxmax - iaxmin + 1
        ijld = ild * (iaymax - iaymin + 1)
        READ(26,*,END=30) I,J,K,VAR
        ncount = ncount + 1
        idx_buf(1,ncount) = i
        idx_buf(2,ncount) = j
        idx_buf(3,ncount) = k
        IF( I.LT.1 .OR. I.GT.nxdim .OR. J.LT.1 .OR. J.GT.nydim &
            .OR. K.LT.1 .OR. K.GT.nzdim ) THEN
         CHMSG = 'Domain Index Out of Range: '
         INDX = 4
         CALL WRMSGS( INDX )
        ENDIF
        ixx = i - iaxmin
        iyy = j - iaymin
        izz = k - iazmin
        n = ixx + iyy*ild + izz*ijld + 1
        IF( IXP(N).LE.0 ) GOTO 20
        INDX = 0
        IUNM = ILUM
        IUNS = ILUS
        IUNKG = ILUKG
        IUNK = ILUK
        IUNMOL = ILUMOL
        CALL RDUNIT( UNTS,VAR,INDX )
        val_buf(ncount) = var + dadd
        if( mod(ncount,ijdim).eq.0 ) then
          call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
          ncount = 0
        endif
        GOTO 20
   30   CONTINUE
        if( mod(ncount,ijdim).gt.0 ) then
          call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
          ncount = 0
        endif
        deallocate(val_buf)
        deallocate(idx_buf)
      endif
      call ga_sync
      IUNM = 0
      IUNS = 0
      IUNKG = 0
      IUNK = 0
      IUNMOL = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      if( use_ga ) then
        ldxx(1) = iaxmax - iaxmin + 1
        ldxx(2) = iaymax - iaymin + 1
        ldxx(3) = iazmax - iazmin + 1
        lo(1) = iaxmin
        lo(2) = iaymin
        lo(3) = iazmin
        hi(1) = iaxmax
        hi(2) = iaymax
        hi(3) = iazmax
        allocate(buf3(ldxx(1),ldxx(2),ldxx(3)))
        buf3 = 0.d0
        call nga_get(g_buf,lo,hi,buf3(1,1,1),ldxx)
        n = 0
        do k = 1,ldxx(3)
        do j = 1,ldxx(2)
        do i = 1,ldxx(1)
          n = n + 1
          if(ixp(n).eq.0 .and. grid_mask(n).eq.1 ) then
            dfv(n) = buf3(i,j,k)
          endif
        enddo
        enddo
        enddo
        status = ga_destroy(g_buf)
      endif
!
!---  End of RDINFR group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINFS( DFV,DVAR,DADD,UNTS,MM )
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
!     Load primary variables w/ initial conditions from an external
!     file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, June 16, 1997.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE GLB_PAR
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
      CHARACTER*64 UNTS
      REAL*8 DVAR(*),DFV(LSV,*)
!
      INTEGER DIMS(3), LO(3), HI(3), LDxx(3), G_BUF, THREE
      INTEGER, ALLOCATABLE :: BUF(:), vali_buf(:)
      INTEGER, ALLOCATABLE :: IDX_BUF(:,:),bufi3(:,:,:)
      real*8,allocatable :: val_buf(:), buf3(:,:,:)
      LOGICAL STATUS,use_ga
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      dims(1) = nxdim
      dims(2) = nydim
      dims(3) = nzdim
      three = 3
      g_buf = ga_create_handle()
#ifdef USE_E4D
!-- E4D Patch 
       IF (GAE4D) CALL GA_SET_PGROUP(g_buf,GAGRP)
#endif
      call ga_set_data(g_buf, three, dims, MT_INT)
      status = ga_allocate(g_buf)
!
      SUBNMX = '/RDINFS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      ILUM = IUNM
      ILUS = IUNS
      ILUKG = IUNKG
      ILUK = IUNK
      ILUMOL = IUNMOL
      DO 10 N = 1,num_nodes
        IF( IXP(N).LE.0 ) GOTO 10
        DFV(MM,N) = DVAR(1)+DADD
   10 CONTINUE
      if (me.eq.0) then
        ncount = 0
        ijdim = nxdim*nydim
        allocate(idx_buf(3,ijdim))
        allocate(val_buf(ijdim))
   20   CONTINUE
        ild = iaxmax - iaxmin + 1
        ijld = ild * (iaymax - iaymin + 1)
        READ(26,*,END=30) I,J,K,VAR
        ncount = ncount + 1
        idx_buf(1,ncount) = i
        idx_buf(2,ncount) = j
        idx_buf(3,ncount) = k
        IF( I.LT.1 .OR. I.GT.nxdim .OR. J.LT.1 .OR. J.GT.nydim &
            .OR. K.LT.1 .OR. K.GT.nzdim ) THEN
          CHMSG = 'Domain Index Out of Range: '
          INDX = 4
          CALL WRMSGS( INDX )
        ENDIF
        ixx = i - iaxmin
        iyy = j - iaymin
        izz = k - iazmin
        n = ixx + iyy*ild + izz*ijld + 1
        IF( IXP(N).LE.0 ) GOTO 20
        INDX = 0
        IUNM = ILUM
        IUNS = ILUS
        IUNKG = ILUKG
        IUNK = ILUK
        IUNMOL = ILUMOL
        CALL RDUNIT( UNTS,VAR,INDX )
        val_buf(ncount) = var + dadd
        if( mod(ncount,ijdim).eq.0 ) then
          call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
          ncount = 0
        endif
        GOTO 20
   30   CONTINUE
        if( mod(ncount,ijdim).gt.0 ) then
          call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
          ncount = 0
        endif
        deallocate(val_buf)
        deallocate(idx_buf)
      endif
      call ga_sync
      IUNM = 0
      IUNS = 0
      IUNKG = 0
      IUNK = 0
      IUNMOL = 0
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      if( use_ga ) then
        ldxx(1) = iaxmax - iaxmin + 1
        ldxx(2) = iaymax - iaymin + 1
        ldxx(3) = iazmax - iazmin + 1
        lo(1) = iaxmin
        lo(2) = iaymin
        lo(3) = iazmin
        hi(1) = iaxmax
        hi(2) = iaymax
        hi(3) = iazmax
        allocate(buf3(ldxx(1),ldxx(2),ldxx(3)))
        buf3 = 0.d0
        call nga_get(g_buf,lo,hi,buf3(1,1,1),ldxx)
        n = 0
        do k = 1,ldxx(3)
        do j = 1,ldxx(2)
        do i = 1,ldxx(1)
          n = n + 1
          if(ixp(n).eq.0 .and. grid_mask(n).eq.1 ) then
            dfv(mm,n) = buf3(i,j,k)
          endif
        enddo
        enddo
        enddo
        status = ga_destroy(g_buf)
      endif

!
!---  End of RDINFS group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINZP( DFV,DVAR,DADD,IFV,IVAR,IZN )
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
!     Load primary variables w/ initial conditions according to
!     rock/soil zonations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, August 1998.
!     Last Modified by MD White, Battelle, PNL, August 25, 1998.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE CONST
      USE GRID_MOD
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
      INTEGER IVAR,IFV(*)
      REAL*8 DVAR,DFV(*)
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3)
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
!
      SUBNMX = '/RDINZP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
!
!---  pH  ---
!
      IF( ABS(DADD-7.D+0)/EPSL.LT.EPSL ) THEN
        DO K = LO(3),HI(3)
          DO J = LO(2),HI(2)
            DO I = LO(1),HI(1)
!             IF( IXP(N).LE.0 ) GOTO 100
              NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
              IF( IZ(NX).EQ.IZN ) THEN
                DVAR = MIN( MAX( DVAR,0.D+0 ), 1.4D+1 )
                DFV(NX) = 1.D+1**(-DVAR)
                IFV(NX) = IVAR
              ENDIF
            ENDDO
          ENDDO
        ENDDO
!  100  CONTINUE
      ELSE
!       IF( IXP(N).LE.0 ) GOTO 110
        DO K = LO(3),HI(3)
          DO J = LO(2),HI(2)
            DO I = LO(1),HI(1)
              NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
              IF( IZ(NX).EQ.IZN ) THEN
                DFV(NX) = DVAR + DADD
                IFV(NX) = IVAR
              ENDIF
            ENDDO
          ENDDO
        ENDDO
!  110   CONTINUE
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINZP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINZQ( DFV,DVAR,DADD,IZN )
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
!     Load primary variables w/ initial conditions according to
!     rock/soil zonations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, August 1998.
!     Last Modified by MD White, Battelle, August 25, 1998.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
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
      REAL*8 DFV(2,*)
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3) 
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
!
      SUBNMX = '/RDINZQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM

      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
!           IF( IXP(N).LE.0 ) GOTO 100
              NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
              IF( IZ(NX).EQ.IZN ) THEN
                DFV(2,NX) = DVAR + DADD
              ENDIF
          ENDDO
        ENDDO
      ENDDO
!  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINZQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINZR( DFV,DVAR,DADD,IZN )
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
!     Load secondary variables w/ initial conditions according to
!     rock/soil zonations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, August 1998.
!     Last Modified by MD White, Battelle, PNL, August 25, 1998.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
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
      REAL*8 DFV(*)
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3)
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
!
      SUBNMX = '/RDINZR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
!           IF( IXP(N).LE.0 ) GOTO 100
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
            IF( IZ(NX).EQ.IZN ) THEN
              DFV(NX) = DVAR + DADD
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINZR group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINZS( DFV,DVAR,DADD,IZN,MM )
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
!     Load secondary variables w/ initial conditions according to
!     rock/soil zonations.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, August 1998.
!     Last Modified by MD White, Battelle, PNL, August 25, 1998.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE GLB_PAR
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
      REAL*8 DFV(LSV,NUM_NODES)
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3), THREE
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
      SUBNMX = '/RDINZS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
!           IF( IXP(N).LE.0 ) GOTO 100
            IF( IZ(NX).EQ.IZN ) THEN
              DFV(MM,NX) = DVAR + DADD
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINZS group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINBP( DFV,DADD,IFV,IVAR,UNTS,T_FILENAME )
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
!     Load primary variables w/ initial conditions from an external
!     binary file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE CONST
      USE GRID_MOD
      USE GLB_PAR
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
      CHARACTER*64 UNTS, T_FILENAME
      INTEGER*4 IVAR,IFV(*)
      REAL*8 DFV(*)
      REAL*8, DIMENSION(:), ALLOCATABLE  :: SFV
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3)
      LOGICAL STATUS, T_OK, ISHDF5, ISBIN
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
      ISBIN = .TRUE.
      ISHDF5 = .FALSE.
!
      SUBNMX = '/RDINBP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      INDX = 0
      VAR = 1.D+0
      CALL RDUNIT( UNTS,VAR,INDX )
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM

      ALLOCATE(SFV(LDXX(1)*LDXX(2)*LDXX(3)))
      T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,SFV,VAR,ISBIN,ISHDF5 )

      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
            DFV(NX) = SFV(NX)*VAR + DADD
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(SFV)
!
!---  pH  ---
!
      IF( ABS(DADD-7.D+0)/EPSL.LT.EPSL ) THEN
        DO K = LO(3),HI(3)
          DO J = LO(2),HI(2)
            DO I = LO(1),HI(1)
              NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
!             IF( IXP(N).LE.0 ) GOTO 10
              IFV(NX) = IVAR
              DFV(NX) = MIN( MAX( DFV(NX),0.D0 ), 14.D0 )
              DFV(NX) = 1.D+1**(-DFV(NX))
            ENDDO
          ENDDO
        ENDDO
!   10  CONTINUE
      ELSE
        DO K = LO(3),HI(3)
          DO J = LO(2),HI(2)
            DO I = LO(1),HI(1)
!             IF( IXP(N).LE.0 ) GOTO 20
              NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
              IFV(NX) = IVAR
              DFV(NX) = DFV(NX)*VAR + DADD
            ENDDO
          ENDDO
        ENDDO
!   20   CONTINUE
      ENDIF      
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINBP group  ---
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINBS( DFV,DADD,UNTS,M,T_FILENAME )
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
!     Load primary variables w/ initial conditions from an external
!     binary file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE GLB_PAR
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
      CHARACTER*64 UNTS, T_FILENAME
      REAL*8 DFV(LSV,*)
      REAL*8, DIMENSION(:), ALLOCATABLE ::  SFV
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3)
      LOGICAL STATUS, T_OK, ISHDF5, ISBIN
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
      THREE = 3
      ISBIN = .TRUE.
      ISHDF5 = .FALSE.
!
      SUBNMX = '/RDINBS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      INDX = 0
      VAR = 1.D+0
      CALL RDUNIT( UNTS,VAR,INDX )
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM

      ALLOCATE(SFV(LDXX(1)*LDXX(2)*LDXX(3)))
      T_OK = RDIJK1D( T_FILENAME,LDXX,LO,HI,SFV,VAR,ISBIN,ISHDF5 )

      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
            DFV(M,NX) = SFV(NX)*VAR + DADD
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(SFV)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINBS group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINAP( DFV,DADD,IFV,IVAR,UNTS )
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
!     Load primary variables w/ initial conditions from an external
!     binary file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE CONST
      USE GRID_MOD
      USE GLB_PAR
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
      CHARACTER*64 UNTS
      INTEGER*4 IVAR,IFV(num_nodes)
      REAL*8 DFV(NUM_NODES)
      REAL*8, DIMENSION(:), ALLOCATABLE :: SFV
     
!
      INTEGER DIMS(3), LO(3), HI(3), LDxx(3)
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
!
      SUBNMX = '/RDINAP'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      INDX = 0
      VAR = 1.D+0
      CALL RDUNIT( UNTS,VAR,INDX )
      ALLOCATE(SFV(NXDIM*NYDIM*NZDIM))
      READ(26,*) (SFV(N),N=1,NXDIM*NYDIM*NZDIM)
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
            IROCK = (K-1)*IJDIM+(J-1)*NXDIM+I
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
            DFV(NX) = SFV(IROCK)
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(SFV)
!
!---  pH  ---
!
      IF( ABS(DADD-7.D+0)/EPSL.LT.EPSL ) THEN
        DO 10 N = 1,NUM_NODES
          IF( IXP(N).le.0 ) GOTO 10
          IFV(N) = IVAR
          DFV(N) = MIN(MAX(DFV(N),0.d0),14.d0)
          DFV(N) = 1.D+1**(-DFV(N))
   10   CONTINUE
      ELSE
        DO 20 N = 1,NUM_NODES
          IF( IXP(N).le.0 ) GOTO 20
          IFV(N) = IVAR
          DFV(N) = DFV(N)*VAR + DADD
   20   CONTINUE
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINAP group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINAQ( DFV,DADD,UNTS )
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
!     Load primary variables w/ initial conditions from an external
!     binary file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE GLB_PAR
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
      CHARACTER*64 UNTS
      REAL*8 DFV(2,*)
      REAL*8, DIMENSION(:), ALLOCATABLE  :: SFV
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3)
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
!
      SUBNMX = '/RDINAQ'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      INDX = 0
      VAR = 1.D+0
      CALL RDUNIT( UNTS,VAR,INDX )
      ALLOCATE(SFV(NXDIM*NYDIM*NZDIM))
      READ(26,*) (SFV(N),N=1,NUM_NODES)
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
            IROCK = (K-1)*IJDIM+(J-1)*NXDIM+I
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
!           IF( IXP(N).LE.0 ) GOTO 10
            DFV(2,NX) = SFV(IROCK)*VAR + DADD
          ENDDO
        ENDDO
      ENDDO
!  10 CONTINUE
      DEALLOCATE(SFV)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINAQ group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINAR( DFV,DADD,UNTS )
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
!     Load primary variables w/ initial conditions from an external
!     binary file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE GLB_PAR
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
      CHARACTER*64 UNTS
      REAL*8 DFV(*)
      REAL*8, DIMENSION(:), ALLOCATABLE  :: SFV
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3)
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
!
      SUBNMX = '/RDINAR'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      INDX = 0
      VAR = 1.D+0
      CALL RDUNIT( UNTS,VAR,INDX )
      ALLOCATE(SFV(NXDIM*NYDIM*NZDIM))
      READ(26,*) (SFV(N),N=1,NUM_NODES)
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
            IROCK = (K-1)*IJDIM+(J-1)*NXDIM+i
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
!           IF( IXP(N).LE.0 ) GOTO 10
            DFV(NX) = SFV(IROCK)*VAR + DADD
          ENDDO
        ENDDO
      ENDDO
!  10 CONTINUE
      DEALLOCATE(SFV)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINAR group  ---
!
      RETURN
      END

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDINAS( DFV,DADD,UNTS,M )
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
!     Load primary variables w/ initial conditions from an external
!     binary file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE GLB_PAR
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
      CHARACTER*64 UNTS
      REAL*8 DFV(LSV,*)
      REAL*8, DIMENSION(:), ALLOCATABLE  :: SFV
!
      INTEGER DIMS(3), LO(3), HI(3), LDXX(3)
      LOGICAL STATUS
!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!--- create GA for zone indices ---
!
      DIMS(1) = NXDIM
      DIMS(2) = NYDIM
      DIMS(3) = NZDIM
!
      SUBNMX = '/RDINAS'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(148)(1:1),'$').EQ.0 ) CVS_ID(148) = &
       '$Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $' 
      ICSN = ICSN+ICSNX
      INDX = 0
      VAR = 1.D+0
      CALL RDUNIT( UNTS,VAR,INDX )
      ALLOCATE(SFV(NXDIM*NYDIM*NZDIM))
      READ(26,*) (SFV(N),N=1,NXDIM*NYDIM*NZDIM)
      LDXX(1) = IAXMAX - IAXMIN + 1
      LDXX(2) = IAYMAX - IAYMIN + 1
      LDXX(3) = IAZMAX - IAZMIN + 1
      LO(1) = IAXMIN
      LO(2) = IAYMIN
      LO(3) = IAZMIN
      HI(1) = IAXMAX
      HI(2) = IAYMAX
      HI(3) = IAZMAX
      IJDIM = NXDIM*NYDIM
      DO K = LO(3),HI(3)
        DO J = LO(2),HI(2)
          DO I = LO(1),HI(1)
            IROCK = (K-1)*IJDIM+(J-1)*NXDIM+i
            NX = (K-LO(3))*LDXX(1)*LDXX(2)+(J-LO(2))*LDXX(1)+I-LO(1)+1
            DFV(M,NX) = SFV(IROCK)*VAR + DADD
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(SFV)
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDINAS group  ---
!
      RETURN
      END
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GET_XYZ(I,J,K,X,Y,Z)
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
!     find the values of x, y, z for the cell center given global
!     indices i,j,k
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GRID_MOD
!
!----------------------Implicit Double Precision-----------------------!
!
      implicit none
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER I, J, K, ILD, JLD, IX, IY, IZ, N
      DOUBLE PRECISION CRDS(3), X, Y, Z
!
!----------------------Executable Lines--------------------------------!
!
      IF (I.GE.IXMIN.AND.I.LE.IXMAX.AND.J.GE.IYMIN.AND.J.LE.IYMAX.AND. &
          K.GE.IZMIN.AND.K.LE.IZMAX) THEN
          IX = I-IAXMIN
          IY = J-IAYMIN
          IZ = K-IAZMIN
          ILD = IAXMAX - IAXMIN + 1
          JLD = IAYMAX - IAYMIN + 1
          N = IX + IY*ILD + IZ*ILD*JLD + 1
          CRDS(1) = D_XC(N)
          CRDS(2) = D_YC(N)
          CRDS(3) = D_ZC(N)
      ELSE
        CRDS(1) = 0.0D00
        CRDS(2) = 0.0D00
        CRDS(3) = 0.0D00
      ENDIF
      CALL GA_DGOP(5,CRDS,3,'+')
      X = CRDS(1)
      Y = CRDS(2)
      Z = CRDS(3)
      RETURN
      END SUBROUTINE GET_XYZ
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE GET_LIM(IDOM, LDI, LDIJ)
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
!     Convert the limits in the dom array to local limits
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, July, 2000.
!     Last Modified by MD White, PNNL, July 27, 2000.
!     $Id: rdini.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE GRID_MOD
!
!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT NONE
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER IDOM(*)
      INTEGER IMIN, IMAX, JMIN, JMAX, KMIN, KMAX
      INTEGER LDI, LDIJ

      IMIN = IDOM(1)
      IMAX = IDOM(2)
      JMIN = IDOM(3)
      JMAX = IDOM(4)
      KMIN = IDOM(5)
      KMAX = IDOM(6)
      IF (IMIN.LT.IAXMIN) IMIN = IAXMIN
      IF (IMAX.GT.IAXMAX) IMAX = IAXMAX
      IF (IMAX.LT.IAXMIN.OR.IMIN.GT.IAXMAX) THEN
        IMIN = 1
        IMAX = 0
      ENDIF
      IF (JMIN.LT.IAYMIN) JMIN = IAYMIN
      IF (JMAX.GT.IAYMAX) JMAX = IAYMAX
      IF (JMAX.LT.IAYMIN.OR.JMIN.GT.IAYMAX) THEN
        JMIN = 1
        JMAX = 0
      ENDIF
      IF (KMIN.LT.IAZMIN) KMIN = IAZMIN
      IF (KMAX.GT.IAZMAX) KMAX = IAZMAX
      IF (KMAX.LT.IAZMIN.OR.KMIN.GT.IAZMAX) THEN
        KMIN = 1
        KMAX = 0
      ENDIF
      IDOM(1) = IMIN
      IDOM(2) = IMAX
      IDOM(3) = JMIN
      IDOM(4) = JMAX
      IDOM(5) = KMIN
      IDOM(6) = KMAX
      LDI = IAXMAX - IAXMIN + 1
      LDIJ = LDI * (IAYMAX - IAYMIN + 1)
      RETURN
      END SUBROUTINE GET_LIM
