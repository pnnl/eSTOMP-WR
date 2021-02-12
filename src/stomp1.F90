SUBROUTINE STOMP1
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
!     STOMP: Subsurface Transport Over Multiple Phases
!
!     Water Mode
!
!     This engineering program numerically simulates thermal
!     and hydrologic transport phenomena in variably saturated
!     subsurface environments, contaminated with a water immiscible
!     volatile organic compound.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!     $Id: stomp1.F90,v 1.1.1.1 2009/03/30 18:42:52 d3m045 Exp $





!

!----------------------Fortran 90 Modules------------------------------!
!




      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE REACT
      USE OUTPU
      USE JACOB
      USE FILES
      USE CONST
      USE UCODE
      use grid_mod
      use grid
      use fluxp
      use fdvp
      use sio
#ifdef USE_E4D
      USE E4D_LINK
#endif
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Include Statements------------------------------!

#include "mafdecls.fh"
#include "global.fh"

#include "petscwrapper.h"
      
!
!----------------------Parameter Statements----------------------------!
!
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      LOGICAL HALT,PLOT,RESTART
      PetscErrorCode :: ierr
      integer :: size
      CHARACTER*5 ZONE
      INTEGER IDTVAL(8)

      double precision :: t_b,t_e1,t_e,t_te,t_tb,t_bk,t_ek
#ifdef USE_E4D
!
! --- E4D Local Vars 
      integer :: data_flag, data_flag2
      integer :: tone, trank
      integer :: tnnodes
      integer :: lo_get(5), hi_get(5), eld(5)
      integer :: idx,ldim,iflg,dflg,dim1,dim2, stomptailrank
      logical :: t_ok
      REAL(KIND=8), dimension(:), allocatable  :: sbuf, tbuf
      REAL :: m3tol
      integer :: stat(MPI_STATUS_SIZE)
      LOGICAL :: use_ga
!--- END E4D Local Vars
#endif
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      ICSN = 0
      SUBNMX = 'STOMP1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      ICSN = ICSN+ICSNX
      ICODE = 1
!      if(.not.allocated(cvs_id))allocate(cvs_id(400))
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F90,v 1.1.1.1 2009/03/30 18:42:52 d3m045 Exp $' 


      t_tb= MPI_Wtime()

!
!---  Intialize variables in common blocks and open files  ---
!
!      CALL INTLZ
!
!---  Print banner on screen and output file  ---
!
!      CALL BANNER
!      CALL RD_STOMP(gridPort)
      tolr = rtol
      tola = atol
      maxiter = 4000
!
!---  Check thermodynamic and hydrologic initial states  ---
!
! update ghosted cells
!      call update_nodes('blu',2,0,1)
      call update_nodes('pressure_w',2,0,1)
  t_b = MPI_Wtime()
      CALL CHK1
  t_e = MPI_Wtime()
 chk_time = t_e-t_b
!
!---  Sequence reaction equations  ---
!
      IF( ISLC(40).EQ.1 ) THEN
        IF(ISLC(57).LE.1) THEN
          CALL SEQEQ
!        ELSE
!          CALL SEQEQ_K
        ENDIF
      ENDIF
#ifdef USE_E4D
!
!---  E4D Patch - start E4D Setup 
!
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, iwrank, ierr)
      IF (E4DFLAG) THEN

          IF (iwrank.eq.E4D_SLAVE_START-1) THEN
             !WRITE(*,*) "in STOMP1/E4DFLAG iwrank=",iwrank,E4D_SLAVE_START
             tone = 1
             ! Last STOMP Processor - Send info to E4D
             !WRITE(*,*) "STOMP1 - about to send command 2 to E4D wrank=",iwrank
             CALL MPI_SEND(2,1,MPI_INTEGER,E4D_SLAVE_START,1,MPI_COMM_WORLD,ierr)
             !WRITE(*,*) "STOMP1 - sent command 2 to E4D wrank=",iwrank
        
             ! Send the STOMP Grid
             CALL MPI_SEND(nxdim,tone,MPI_INTEGER,E4D_SLAVE_START,2,MPI_COMM_WORLD,ierr)
             CALL MPI_SEND(nydim,tone,MPI_INTEGER,E4D_SLAVE_START,2,MPI_COMM_WORLD,ierr)
             CALL MPI_SEND(nzdim,tone,MPI_INTEGER,E4D_SLAVE_START,2,MPI_COMM_WORLD,ierr)
             !WRITE(*,*) "Sent Grid dims to E4D",nxdim,nydim,nzdim
             !WRITE(*,*) "Sending Surface Positions to E4D",iwrank
             !WRITE(*,*) X(1:nxdim+1),Y(1:nydim+1),Z(1:nzdim+1)
             !write(*,*) x,y,z 
            CALL MPI_SEND(X,nxdim+1,MPI_DOUBLE_PRECISION,E4D_SLAVE_START,3,MPI_COMM_WORLD,ierr)
            CALL MPI_SEND(Y,nydim+1,MPI_DOUBLE_PRECISION,E4D_SLAVE_START,3,MPI_COMM_WORLD,ierr)
            CALL MPI_SEND(Z,nzdim+1,MPI_DOUBLE_PRECISION,E4D_SLAVE_START,3,MPI_COMM_WORLD,ierr)
             ! WRITE(*,*) "Sent Surface Positions to E4D",iwrank
        ENDIF
     ENDIF
!
!--- End of E4D Patch
!
#endif
!
!--- Initialize PETSC solver
!     
      iter = -1 
  t_b = MPI_Wtime()
      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
  t_e = MPI_Wtime()
petsc_init_time=t_e-t_b
!
!---  Compute Jacobian matrix pointers  ---
!
  t_b = MPI_Wtime()
      CALL JCBP
  t_e = MPI_Wtime()
jcbp_time = t_e-t_b
  t_b = MPI_Wtime()
      call PETSC_SOLVER_INIT( petsc_option,tolr,tola,maxiter,isvc,lsize,llsize,gsize, &
        num_nodes,num_loc_nodes,nnz_d,nnz_o,ixp,imxp,id_l2g )
  t_e = MPI_Wtime()
petsc_sinit_time=t_e-t_b
!
!---  Set start date and time  ----
!
      CALL DATE_AND_TIME(CHDATE,CHTIME,ZONE,IDTVAL)
!
!---  Compute primary variable increments  ---
!
  t_b = MPI_Wtime()
      CALL INCRM1
  t_e = MPI_Wtime()
incrm_time = incrm_time + t_e-t_b
!
!---  Saturation, relative permeability, porosity, and tortuosity  ---
!
  t_b = MPI_Wtime()
      CALL SMC1
  t_e = MPI_Wtime()
smc_time = smc_time + t_e-t_b
!
!---  Thermodynamic properties and equations of state  ---
!
  t_b = MPI_Wtime()
      CALL TMPR1
  t_e = MPI_Wtime()
tmpr_time =tmpr_time+t_e-t_b
!
!---  Initial hydrologic and thermodynamic properties on
!     boundary surfaces  ---
!
  t_b = MPI_Wtime()
       CALL BCP1
  t_e = MPI_Wtime()
bcp_time = bcp_time+t_e-t_b
!
!---  Compute initial solute concentrations  ---
!
  t_b = MPI_Wtime()
      CALL CISC1
  t_e = MPI_Wtime()
cisc_time = cisc_time+t_e-t_b


!
!---  Reactive transport  ---
!
      IF( ISLC(40).EQ.1 ) THEN
!
!---    Normalize mineral species for restart  ---
!
        IF( IEO.EQ.2 .OR. IEO.EQ.4 ) CALL NMNSP
!
!---    Convert initial reactive species concentrations to
!       node volume basis, mol/m^3  ---
!
        CALL FLHSP
!
!---    Temporarily store time stepping  ---
!
        DT_RST = DT
        DTI_RST = DTI
        TM_RST = TM
!
!---    Loop over number of conservation component species  ---
!
        DO 20 NEQ = 1,NEQC
          NSL = NEQ + NSOLU
!
!---      Mobile conservation component fractions   ---
!
          CALL MOBCF( NEQ )
!
!---      Add immobile conservation component fractions   ---
!
          CALL IMOBCF( NEQ )
!
!---    End of conservation component species transport  ---
!
   20   CONTINUE
!
!---    Loop over number of kinetic component species  ---
!
        DO 40 NEQ = 1,NEQK
          NSL = NEQ + NEQC + NSOLU
! 
!---      Mobile kinetic component fractions   ---
!
          CALL MOBKF( NEQ )
! 
!---      Add immobile kinetic component fractions   ---
!
          CALL IMOBKF( NEQ )
!
!---    End of conservation component species transport  ---
!
   40   CONTINUE
!
!---    Equilibrium-conservation-kinetic reaction chemistry   ---
!
  t_bk = MPI_Wtime()
        CALL ECKECHEM
      call update_nodes('sp_c',2,0,1)
      call update_nodes('sp_cmn',2,0,1)
      call update_nodes('por_m',2,0,1)
      call update_nodes('mineral_property',3,0,1)
  t_ek = MPI_Wtime()
eckechem_time = eckechem_time+t_ek-t_bk
!
!---    Reconstitute mineral species concentrations for initial
!       output  ---
!
        CALL RMNSP
      ENDIF

!
!---  Compute initial fluxes on interior and boundary surfaces  ---
!
      ISVF = 1
  t_b = MPI_Wtime()
      CALL DRCVL
  t_e = MPI_Wtime()
drcvl_time = drcvl_time+t_e-t_b
  t_b = MPI_Wtime()
      CALL BCF1
  t_e = MPI_Wtime()
bcf_time =bcf_time+t_e-t_b
      ISVF = 2*ISVC+1
!
!---  New Time Step ---
!
  100 CONTINUE
!
!---  Load old time step arrays  ---
!
  t_b = MPI_Wtime()
      CALL LDO1
  t_e = MPI_Wtime()
ldo_time = ldo_time + t_e-t_b
  t_b = MPI_Wtime()
      call average_v
  t_e = MPI_Wtime()
average_time =average_time+t_e-t_b
  data_flag = 0
#ifdef USE_E4D
!
!---  E4D at Top of solve 
      IF (E4DFLAG) THEN
         tone = 1

         ! use the last STOMP Processor to send Data to E4D
         CALL MPI_COMM_RANK(MPI_COMM_WORLD, iwrank, ierr)
         IF (iwrank.eq.E4D_SLAVE_START-1) THEN

             ! Last STOMP Processor - Send info to E4D         
             CALL MPI_SEND(4,tone,MPI_INTEGER,E4D_SLAVE_START,1,MPI_COMM_WORLD,ierr)

             ! Send STOMP TIME         
             CALL MPI_SEND(TM,tone,MPI_DOUBLE_PRECISION,E4D_SLAVE_START,4,MPI_COMM_WORLD,ierr)

             ! See if this is a time for E4D to Solve 
             CALL MPI_RECV(data_flag,tone,MPI_INTEGER,E4D_SLAVE_START,5,MPI_COMM_WORLD,stat,ierr)

             if(data_flag==1) then
                write(*,*)
                write(*,*) '________EXECUTING E4D AT TIME: ',TM,' _________'
             end if
        ENDIF

        stomptailrank = E4D_SLAVE_START-1
        call ga_igop(1,data_flag,1,'max')
        !WRITE(*,*) "Got Data Flag from E4d =",data_flag,iwrank
        IF (data_flag.ne.0) THEN
!           Time period for an e4d_solve, send the data
!           SEND AQUEOUS SATURATION
            !write(*,*) "nxyzdims",nxdim,nydim,nzdim
            ldim = 2
            dim1 = 2
            iflg = 0
            dflg = 1
            var = 1.d0
            avar = 0.d0
            tnnodes = nxdim * nydim * nzdim
            allocate(sbuf(tnnodes),tbuf(tnnodes))
            !write(*,*) "Calling string2idx",ldim
            call string2idx(ldim,iflg,dflg,'saturation_w',idx,t_ok)

            !write(*,*) "Back from calling string2idx",idx,t_ok,me

            call get_data_for_e4d(ldim,dim1,dim2,idx,var,avar,sbuf,tnnodes,stomptailrank)

!           Send to E4D
            IF (iwrank.eq.E4D_SLAVE_START-1) THEN
                !Write(*,*) "STOMP - MPI_SEND to E4D - STOMP Sats",tnnodes
! Last STOMP Processor - Send info to E4D
                CALL MPI_SEND(sbuf,tnnodes,MPI_DOUBLE_PRECISION,E4D_SLAVE_START,6,MPI_COMM_WORLD,ierr)
                !WRITE(*,*) "STOMP - Sent Saturations to E4D"
            ENDIF

!           Calculate Aqueous Concentration
            nsl = 1
            ! conversion factor from m^3 to L
            m3tol = 1.0E-3
            do n=1,num_nodes
                varp_tmp(n) = m3tol*c(nsl,n)*yl(nsl,n)/(sl(2,n)*pord(2,n)+small)
            enddo
            ldim = 1
            iflg = 0
            dflg = 1
            call string2idx(ldim,iflg,dflg,'varp_tmp',idx,t_ok)
            avar = 0.d0

!           Send to E4D
            call get_data_for_e4d(ldim,dim1,dim2,idx,var,avar,tbuf,tnnodes,stomptailrank)

!           Send to E4D
            IF (iwrank.eq.E4D_SLAVE_START-1) THEN
                !Write(*,*) "STOMP - MPI_SEND to E4D - STOMP Aqu Tracer
                !Concs:",trim(SOLUT(NSL))
                CALL MPI_SEND(tbuf,tnnodes,MPI_DOUBLE_PRECISION,E4D_SLAVE_START,7,MPI_COMM_WORLD,ierr)
                !WRITE(*,*) "STOMP - Sent Tracer to E4D"
            ENDIF

            deallocate(sbuf)
            deallocate(tbuf)
         ENDIF
      ENDIF
!
!--- End E4D Patch
!
#endif


!
!---  Reference node(s) output  ---
!
      IF( MOD( (NSTEP-NRST),IFQS ).EQ.0 .OR. &
       MOD( (NSTEP-NRST),ABS(IFQO) ).EQ.0 .OR. &
       IFQO.LT.1.AND.ABS(TMPR-TM)/EPSL.LE.EPSL ) CALL REFNOD

!
!---    Normalize mineral species concentrations after initial
!       output  ---
!
        IF( (NSTEP-NRST).EQ.0 .and. islc(40).eq.1) CALL NMNSP

!
!---  End of initial conditions simulations  ---
!
      IF( IEO.EQ.3 ) THEN
        INDX = 1
        CHMSG = 'Simulation Stopped:  Initial Condition'
        CALL WRMSGS( INDX )
        GOTO 900
      ENDIF
#ifdef USE_E4D
!
!---  E4D at Bottom of solve 
!
      IF (E4DFLAG) THEN

        ! Not sure if this is the better place to call E4D

      ENDIF
!--- END E4D
#endif
!
!---  Stop simulation if simulation time exceeds limit  ---
!
      IF( ABS(TMMX-TM)/EPSL.LT.EPSL ) THEN
        INDX = 1
        CHMSG = 'Simulation Stopped:  Simulation Time Limit'
        CALL WRMSGS( INDX )
        GOTO 900
      ENDIF
!
!---  Stop simulation if file "stop_stomp" exists  ---
!
      INQUIRE( FILE="stop_stomp", EXIST=HALT )
      IF( HALT ) THEN
        if(me == 0) then
          OPEN( UNIT=19, FILE="stop_stomp" )
          CLOSE( UNIT=19, STATUS='DELETE' )
        endif
        INDX = 1
        CHMSG = 'Simulation Stopped:  User Interrupt'
        CALL WRMSGS( INDX )
        ISLC(18) = 0
        GOTO 900
      ENDIF
!
!---  Generate plot file if file "plot_stomp" exists  ---
!
      INQUIRE( FILE="plot_stomp", EXIST=PLOT )
      IF( PLOT ) THEN
        if(me == 0) then
          OPEN( UNIT=19, FILE="plot_stomp" )
          CLOSE( UNIT=19, STATUS='DELETE' )
        endif
        CALL WRPLOT
        IF( ISLC(18).LT.1 ) CALL WRRST        
      ENDIF
!
!---  Generate restart file if file "restart_stomp" exists  ---
!
      INQUIRE( FILE="restart_stomp", EXIST=RESTART )
      IF( RESTART ) THEN
        if(me == 0) then
          OPEN( UNIT=19, FILE="restart_stomp" )
          CLOSE( UNIT=19, STATUS='DELETE' )
        endif
        CALL WRRST        
      ENDIF
!
!---  Restart and plot file outputs  ---
!
      IF( ABS(TMPR-TM)/EPSL.LE.EPSL ) THEN
        CALL WRPLOT
        IF( ISLC(18).LT.1 ) CALL WRRST
      ENDIF
!
!---  Inverse output  ---
!
      IF( ISLC(20).EQ.1 .AND. ABS(TMOB-TM)/EPSL.LT.EPSL ) CALL WROBDA
!
!---  Compute the next time step and increment time step counter  ---
!
      DTSO = DT
  t_b = MPI_Wtime()
      CALL TMSTEP
  t_e = MPI_Wtime()
tmstep_time=tmstep_time+t_e-t_b
      IF( NSTEP.EQ.0 ) DTSO = DT
      NSTEP = NSTEP + 1
      IF( NSTEP-NRST.GT.MXSTEP ) THEN
        INDX = 1
        CHMSG = 'Simulation Stopped:  Time Step Limit'
        CALL WRMSGS( INDX )
        NSTEP = NSTEP - 1
        GOTO 900
      ENDIF
!
!---  No flow solution  ---
!
      IF( ISLC(47).EQ.1 ) THEN
        CALL BCP1
        GOTO 600
      ENDIF
      NTSR = 0
!
!---  Newton-Raphson iteration restart  ---
!
  200 CONTINUE
      NITER = 0
!
!---  Newton-Raphson iteration start  ---
!
  300 CONTINUE
      NITER = NITER + 1
!
!---  Compute boundary saturation, relative permeability, and
!     thermodynamic properties  ---
!
  t_b = MPI_Wtime()
      CALL BCP1
  t_e = MPI_Wtime()
bcp_time=bcp_time+t_e-t_b
!
!---  Compute source contributions  ---
!
  t_b = MPI_Wtime()
      CALL SORC1
  t_e = MPI_Wtime()
sorc_time=sorc_time+t_e-t_b
!
!---  Compute aqueous-phase volumetric flux (interior surfaces)  ---
!
  t_b = MPI_Wtime()
      CALL DRCVL
  t_e = MPI_Wtime()
drcvl_time=drcvl_time+t_e-t_b
!
!---  Compute aqueous-phase volumetric flux (boundary surfaces)  ---
!
  t_b = MPI_Wtime()
      CALL BCF1
  t_e = MPI_Wtime()
bcf_time=bcf_time+t_e-t_b
!
!---  Load Jacobian matrix for the water equation
!     (zero flux boundary)  ---
!
  t_b = MPI_Wtime()
      CALL JCBWL
  t_e = MPI_Wtime()
jcbwl_time=jcbwl_time+t_e-t_b
!
!---  Modify the Jacobian matrix for boundary conditions  ---
!
  t_b = MPI_Wtime()
      CALL BCJ1
  t_e = MPI_Wtime()
bcj_time =bcj_time+t_e-t_b

!
       icnv = 3
       call petsc_solver_solve(icnv,iter,nstep)
!       IF( ITER >= MAXITER ) THEN
!          IF( ME.EQ.0 ) WRITE(ISC,'(4X,A,I4,A)') &
!            'Number of Solver Iterations > Maxiumum, ( ',ITER,' )'
!          IF( ME.EQ.0 ) WRITE(IWR,'(4X,A,I4,A,I4,A)') &
!            'Number of Solver Iterations > Maxiumum, ( ',ITER,' )'
!          ICNV = 1 
!        ENDIF
!
!---  Update primary variables  ---
!
!      call update_nodes
      call update_nodes('blu',2,0,1)
  t_b = MPI_Wtime()
      CALL UPDT1
  t_e = MPI_Wtime()
updt_time=updt_time+t_e-t_b
!goto 900
!
!---  Compute convergence from maximum relative residuals  ---
!
  t_b = MPI_Wtime()
      CALL RSDL1
  t_e = MPI_Wtime()
rsdl_time=rsdl_time+t_e-t_b
!      if(icnv.eq.2) call update_nodes(gridPort,excpt)
!
!---  Compute primary variable increments, saturation,
!     relative permeability, porosity, tortuosity,
!     thermodynamic properties for interior nodes,
!     except immediately after a new time step  ---
!

  t_b = MPI_Wtime()
      CALL INCRM1
  t_e = MPI_Wtime()
incrm_time=incrm_time+t_e-t_b
  t_b = MPI_Wtime()
      CALL SMC1
  t_e = MPI_Wtime()
smc_time=smc_time+t_e-t_b
  t_b = MPI_Wtime()
      CALL TMPR1
  t_e = MPI_Wtime()
tmpr_time=tmpr_time+t_e-t_b
      GOTO( 200,300,600,900 ) ICNV
  600 CONTINUE
      stomp_iter = stomp_iter + niter
!
!---  Compute aqueous-phase volumetric flux (interior surfaces)  ---
!
      ISVF = 1
  t_b = MPI_Wtime()
      CALL DRCVL
  t_e = MPI_Wtime()
drcvl_time=drcvl_time+t_e-t_b
!
!---  Compute aqueous-phase volumetric flux (boundary surfaces)  ---
!
  t_b = MPI_Wtime()
      CALL BCF1
  t_e = MPI_Wtime()
bcf_time =bcf_time+t_e-t_b
!
!---  Compute Local Courant Numbers  ---
!
      IF( ICRNT.EQ.1 ) CALL CRNTNB
      ISVF = 2*ISVC+1
!
!---  Beginning of transport equation solution  ---
!
      IF( IEQC.EQ.0 .AND. ISLC(40).EQ.0 ) GOTO 800
!
!---  Reset Jacobian matrix pointers excluding inactive nodes,
!     for 1 domain solution schemes  ---
!
!      IF( ISLC(48).EQ.1 ) CALL JCBPI
!
!---  Loop over number of solutes  ---
!
      if(nsolu+nspr.gt.0) then
!        call petsc_solver_destroy()
!        call PETSC_SOLVER_INIT( tolr,tola,maxiter,isvc,lsize,llsize,gsize, &
!        num_nodes,num_loc_nodes,nnz_d,nnz_o,ixp,imxp,id_l2g )
      endif
      call average_v
      DO 700 NSL = 1,NSOLU
!
!---  Courant number limiting  ---
!
        N_CRN(NSL) = 1
        IF( ISLC(17).NE.0 ) CALL CRN_LIM( NSL )
!
!---    Sub-time step loop  ---
!
        DO 690 NC = 1,N_CRN(NSL)
          IF( ISLC(17).NE.0 ) TM = MIN( TM+DT,TM_CRN )
!
!---      Compute solute mole fractions ---
!
          CALL SPRP1( NSL )
!
!---      Solute transport ---
!
          CALL TPORT1( NSL )
!
!---      Load old sub-time-step solute concentrations  ---
!
          IF( ISLC(17).NE.0 ) CALL UPDTCO( NSL )
!
!---    Bottom of sub-time step loop  ---
!
  690   CONTINUE
!
!---  Courant number limiting, reset time stepping  ---
!
        IF( ISLC(17).NE.0 ) THEN
          DT = DT_CRN
          DTI = DTI_CRN
          TM = TM_CRN
        ENDIF
!
!---  End of transport equation solution  ---
!
  700 CONTINUE

!
!---  Reactive transport  ---
!
      IF( ISLC(40).EQ.1 ) THEN
        N_CRN(NSOLU+1) = 1
        IF( ISLC(17).NE.0 ) CALL CRN_LIM( NSOLU+1 )
!
!---    Courant-limiting sub-time step loop  ---
!
        DO 792 NCR = 1,N_CRN(NSOLU+1)
          IF( ISLC(17).NE.0 ) TM = MIN( TM+DT,TM_CRN )
          DT_RST = DT
          DTI_RST = DTI
          TM_RST = TM
          TM = TM - DT
          N_RST = 1
  710     CONTINUE
          IF( N_RST.GT.16 ) THEN
            if(me.eq.0) WRITE(ISC,'(A)') '          ---  ECKEChem ' //  &
            'Sub-Time Step Reduction Limit Exceeded  ---'
            if(me.eq.0) WRITE(IWR,'(A)') '          ---  ECKEChem ' //  &
            'Sub-Time Step Reduction Limit Exceeded  ---'
            DT = DT_RST
            DTI = DTI_RST
            TM = TM_RST
            NSTEP = NSTEP-1
            TM = TM-DT
            DT = DTO
!            CALL BCK_STP
            GOTO 900
          ENDIF
!
!---      ECKEChem sub-time step loop  ---
!
          DO 790 NC = 1,N_RST
            TM = TM + DT
!
!---        Loop over number of conservation component species  ---
!
            DO 730 NEQ = 1,NEQC
              NSL = NEQ + NSOLU
!
!---          Mobile conservation component fractions   ---
!
              CALL MOBCF( NEQ )
!
!---          Skip transport for immobile conservation component 
!             species   ---
!
              IF( IMMB(NEQ).EQ.1 ) GOTO 720
!
!---          Solute transport ---
!
              CALL TPORT1( NSL )
!
!---          Add immobile conservation component fractions   ---
!
  720         CONTINUE
              CALL IMOBCF( NEQ )
!
!---        End of conservation component species transport  ---
!
  730       CONTINUE
!
!---        Loop over number of kinetic component species  ---
!
            DO 750 NEQ = 1,NEQK
              NSL = NEQ + NEQC + NSOLU
! 
!---          Mobile kinetic component fractions   ---
!
              CALL MOBKF( NEQ )
!
!---          Skip transport for immobile conservation component 
!             species   ---
!
              IF( IMMB(NEQ+NEQC).EQ.1 ) GOTO 740
!
!---          Solute transport ---
!
              CALL TPORT1( NSL )
! 
!---          Add immobile kinetic component fractions   ---
!
  740         CONTINUE
              CALL IMOBKF( NEQ )
!
!---        End of conservation component species transport  ---
!
  750       CONTINUE
!
!---        Equilibrium-conservation-kinetic reaction chemistry   ---
!
  t_bk = MPI_Wtime()
            sp_co(:,:) = sp_c(:,:)
            CALL ECKECHEM
            IF( ECKE_ER ) THEN
              CALL RESET_SP
              CALL UPDATE_NODES('sp_c',2,0,1)
              call update_nodes('sp_cmn',2,0,1)
              call update_nodes('por_m',2,0,1)
              call update_nodes('mineral_property',3,0,1)
              ecke_er = .false.
              GOTO 710
            ELSE
              CALL UPDATE_NODES('sp_c',2,0,1)
              call update_nodes('sp_cmn',2,0,1)
              call update_nodes('por_m',2,0,1)
              call update_nodes('mineral_property',3,0,1)
            ENDIF
  t_ek = MPI_Wtime()
eckechem_time = eckechem_time+t_ek-t_bk
!            IF( ECKE_ER ) GOTO 710
!
!---        Load old sub-time-step reactive species
!           concentrations and component species concentrations  ---
!
            IF( ISLC(17).NE.0 ) CALL UPDTCHEM
!
!---      Bottom of sub-time step loop  ---
!
  790     CONTINUE
!
!---      Reset time stepping  ---
!
          IF( N_RST.GT.1 ) THEN
            DT = DT_RST
            DTI = DTI_RST
            TM = TM_RST
          ENDIF
  792   CONTINUE
!
!---    Courant number limiting, reset time stepping  ---
!
        IF( ISLC(17).NE.0 ) THEN
          DT = DT_CRN
          DTI = DTI_CRN
          TM = TM_CRN
        ENDIF
      ENDIF

!      call petsc_solver_destroy()
!for other modes, need to call jcbp
!      call PETSC_SOLVER_INIT( tolr,tola,maxiter,isvc,lsize,llsize,gsize, &
!        num_nodes,num_loc_nodes,nnz_d,nnz_o,ixp,imxp,id_l2g )
!
!---  Correct aqueous liquid density and viscosity for electrolyte
!     solute concentration  ---
!
!      IF( ISLC(16).EQ.1 ) CALL ELC1
  800 CONTINUE
!
!---  Surface flux integrator  ---
!
      CALL SFIN
!
!---  Proceed to new time step  ---
!
      GOTO 100
!
!---  Write plot file, restart file, close files, and
!     terminate simulation  ---
!
  900 CONTINUE
!
!--- close petsc
!      
      ! When using parallel IO, must call WRPLOT before communicator is desstroyed
      ! I am not exactly sure why this call occurs here at the end - kls
      CALL WRPLOT
      IF( ISLC(18).LT.2 ) CALL WRRST

      call petsc_solver_destroy()
      call petsc_solver_final()
!
!---  Inverse output  ---
!
      IF( ISLC(20).EQ.1 ) THEN
        IF( ABS(TMOB-TM)/EPSL.LT.EPSL ) CALL WROBDA
        IF( FLG_EXT .and. me == 0) WRITE(IOBDEF,'(A,/)') 'END'
        IF( FLG_UNI .and. me == 0) WRITE(IOBDUF,'(A,/)') 'END'
      ENDIF
      if(me.eq.0) WRITE(IWR,'(/,A)') '---  End of STOMP Simulation  ---'
      if(me.eq.0) WRITE(ISC,'(/,A)') '---  End of STOMP Simulation  ---'
   !   CALL WRCVS

   t_te= MPI_Wtime()
   stomp_time=t_te-t_tb
END

#ifdef USE_E4D
subroutine get_data_for_e4d(ldim,dim1,dim2,idx,var,avar,dbuf,tnodes,myrank)
  use grid_mod
  implicit none
!
!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
!
!
  integer :: dim1,one,dim2,ldim,nlayx,ijdim,kx,i,k,idx,idivx,nzdimx
  integer :: i_offset, ix_offset,iy_offset,iz_offset,tnodes
  integer :: lo_get(5), hi_get(5), ld(5), lo_put(5), hi_put(5)
  double precision :: var,avar
  double precision, dimension(tnodes) :: dbuf
  integer :: me,myrank
  logical :: use_ga
!
  me = ga_nodeid()
  use_ga = .true.
  one = 1
  if (ixmin.eq.1) then
    ix_offset = 0
  else
    ix_offset = gwidth
  endif
  if (iymin.eq.1) then
    iy_offset = 0
  else
    iy_offset = gwidth
  endif
  if (izmin.eq.1) then
    iz_offset = 0
  else
    iz_offset = gwidth
  endif
  one = 1
  i_offset = ix_offset + iy_offset*ldx + iz_offset*ldx*ldy + 1
!
!
  if(ldim.eq.1) then
      lo_put(1) = ixmin
      lo_put(2) = iymin
      lo_put(3) = izmin
      hi_put(1) = ixmax
      hi_put(2) = iymax
      hi_put(3) = izmax
!
      ld(1) = ldx
      ld(2) = ldy
      ld(3) = ldz
      call nga_put(ga_dbl,lo_put,hi_put,d_nd_fld(idx)%p(i_offset),ld)
      call ga_sync
      if(me.eq.myrank) then
        lo_get(1) = 1
        hi_get(1) = nxdim
        lo_get(2) = 1
        hi_get(2) = nydim
        ld(1) = nxdim
        ld(2) = nydim
        ld(3) = nzdim
        lo_get(3) = 1
        hi_get(3) = nzdim 
        call nga_get(ga_dbl,lo_get,hi_get,dbuf,ld)
        dbuf(1:tnodes) = var*dbuf(1:tnodes)+avar
      endif
  elseif(ldim.eq.2) then
      lo_put(1) = 1
      hi_put(1) = d_nd_2dim1(idx)
      lo_put(2) = ixmin
      lo_put(3) = iymin
      lo_put(4) = izmin
      hi_put(2) = ixmax
      hi_put(3) = iymax
      hi_put(4) = izmax
      ld(1) = d_nd_2dim1(idx)
      ld(2) = ldx
      ld(3) = ldy
      ld(4) = ldz
      call nga_put(ga_dbl2,lo_put,hi_put,d_nd_2fld(idx)%p(one,i_offset),ld)
      call ga_sync
      if(me.eq.myrank) then
        lo_get(1) = dim1
        hi_get(1) = dim1
        lo_get(2) = 1
        hi_get(2) = nxdim
        lo_get(3) = 1
        hi_get(3) = nydim
        ld(1) = 1 
        ld(2) = nxdim
        ld(3) = nydim
        ld(4) = nzdim
        lo_get(4) = 1
        hi_get(4) = nzdim 
        call nga_get(ga_dbl2,lo_get,hi_get,dbuf,ld)
        dbuf(1:tnodes) = var*dbuf(1:tnodes)+avar
      endif
  elseif(ldim.eq.3) then
      lo_put(1) = 1
      hi_put(1) = d_nd_3dim1(idx)
      lo_put(2) = 1
      hi_put(2) = d_nd_3dim2(idx)
      lo_put(3) = ixmin
      lo_put(4) = iymin
      lo_put(5) = izmin
      hi_put(3) = ixmax
      hi_put(4) = iymax
      hi_put(5) = izmax
      ld(1) = d_nd_3dim1(idx)
      ld(2) = d_nd_3dim2(idx)
      ld(3) = ldx
      ld(4) = ldy
      ld(5) = ldz

      call nga_put(ga_dbl3,lo_put,hi_put,d_nd_3fld(idx)%p(one,one,i_offset),ld)
      call ga_sync
      if(me.eq.myrank) then
        lo_get(1) = dim1
        hi_get(1) = dim1
        lo_get(2) = dim2
        hi_get(2) = dim2
        lo_get(3) = 1
        hi_get(3) = nxdim
        lo_get(4) = 1
        hi_get(4) = nydim
        ld(1) = 1
        ld(2) = 1
        ld(3) = nxdim
        ld(4) = nydim
        ld(5) = nzdim
        lo_get(5) = 1
        hi_get(5) = nzdim 
        call nga_get(ga_dbl3,lo_get,hi_get,dbuf,ld)
        dbuf(1:tnodes) = var*dbuf(1:tnodes)+avar
      endif
    endif
!
  return
end subroutine get_data_for_e4d
#endif
