

!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE SFXLB( NSL )
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
!     Modify the Jacobian matrix for the solute transport equation
!     to incorporate boundary conditions.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!

!----------------------Fortran 90 Modules------------------------------!
!




      USE GLB_PAR

      USE TRNSPT
      USE SOLTN
      USE REACT
      USE PORMED
      USE JACOB
      USE GRID
      USE FLUXP
      USE FDVP
      USE CONST
      USE BCVP
      USE BCV
      USE GRID_MOD
      USE PLT_ATM
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
!----------------------Parameter Statements----------------------------!
!
       LOGICAL :: ISJCB
       LOGICAL :: use_ga
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/SBND1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
       '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Loop over number of specified boundary conditions  ---
!
      NBCT = MIN( NSL+LUK,NSOLU+LUK+1 )
      AP = 0.D+0
      FLB = 0.D+0
      FCL = 0.D+0
      FCLP = 0.D+0
      DLZ = 0.D+0
      BCXX = 0.D+0
      isjcb = .FALSE.
!      C_FLUX_B(nsl,:) = 0.D+0
      DO 200 NB = 1,NUM_BCNX
        N = IBCN(NB)
        ICYCLE = 0
        CALL CBND1(NB,NBCT,NSL,ICYCLE,AP,DLZ,FLB,FCL,FCLP,BCXX,ISJCB)
        IF( ICYCLE.EQ.1 )CYCLE
        DO IFCX = 1,6
          ICNX = ND2CNX(IFCX,N)
          IF( ICNX < 0 )THEN
          ICNX = ABS(ICNX)
          IF( ICNX == NB )THEN
!
!--- Dirichlet  ---
!
            IF( IBCT(NBCT,NB).EQ.1 .OR. IBCT(NBCT,NB).EQ.8 &
                .OR. IBCT(NBCT,NB).EQ.12 ) THEN
                ALUX = MAX(-FLB,ZERO) !UP AS EAST
                ALDX = MAX(FLB,ZERO) !DN AS NODE
                ALDF = DLZ*MAX((ONE-(TENTH*ABS(FLB)/(DLZ+SMALL)))**5,ZERO)
                ALP = ALUX+ALDF
                AL = ALDX+ALDF
              IF( IBCD(NB).LT.0 )THEN
                C_FLUX_B(NSL,NB) = C_FLUX_B(NSL,NB) + &
                  BCXX*FCL*AL - C(NSL,N)*ALP*FCLP
              ELSE
                C_FLUX_B(NSL,NB) = C_FLUX_B(NSL,NB) + &
                  C(NSL,N)*AL*FCLP - BCXX*FCL*ALP
              ENDIF
!
!--- Outflow  ---
!
              ELSEIF( IBCT(NBCT,NB).EQ.7 .OR. &
                IBCT(NBCT,NB).EQ.19 .OR. IBCT(NBCT,NB).EQ.23 )THEN
                IF( IBCD(NB).LT.0 )THEN
                  IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
                    AP = 0.D+0
                  ELSE
                    AP = MAX( -Q_FLUX_B(1,NB),ZERO )
                  ENDIF
                  C_FLUX_B(NSL,NB) = C_FLUX_B(NSL,NB) - C(NSL,N)*AP*FCLP
                ELSE
                  IF( ISLC(1).GE.1 .AND. KFLD.GT.1 )  THEN
                    AP = 0.D+0
                  ELSE
                    AP = MAX( Q_FLUX_B(1,NB),ZERO )
                  ENDIF
                  C_FLUX_B(NSL,NB) = C_FLUX_B(NSL,NB) + C(NSL,N)*AP*FCLP
                ENDIF
!
!--- Inflow  ---
!
              ELSE
                IF( IBCT(NBCT,NB).EQ.24.and. lplant == 1) BCXX = 0.d0
                IF( IBCD(NB).LT.0 )THEN
                  C_FLUX_B(NSL,NB) = C_FLUX_B(NSL,NB) - BCXX*FCLP*FLB
                ELSE
                  C_FLUX_B(NSL,NB) = C_FLUX_B(NSL,NB) + BCXX*FCLP*FLB
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
!--- prepare for surface flux output
        idrx = ibcd(nb)
        if(idrx == -1) then
          c_flux_nd(1,nsl,n) = c_flux_b(nsl,nb)
        elseif(idrx == -2) then
          c_flux_nd(2,nsl,n) = c_flux_b(nsl,nb)
        elseif(idrx == -3) then
          c_flux_nd(3,nsl,n) = c_flux_b(nsl,nb)
        endif
  200 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of SBND1 group  ---
!

      RETURN
      END
