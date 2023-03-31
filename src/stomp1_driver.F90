program stomp_driver
  use grid_mod
  use soltn
  use sio
  use bufferedread
#ifdef USE_E4D
  use e4d_link
#endif
  implicit none

 
#include "mafdecls.fh"
#include "global.fh"
!#include "mpif.h"
#include "petscwrapper.h"

  integer :: ierr

! For E4D
#ifdef USE_E4D
  INTEGER :: NARGS, IARG,I
  CHARACTER*4 :: ARG,TSTR
  INTEGER, dimension(:), allocatable :: gagrp_ranks,slavegrp_ranks
  INTEGER :: slavegrp
  INTEGER :: itcom(2)
  INTEGER :: MY_INTER_RANK
  INTEGER :: tone
  PetscMPIInt :: INTER_GROUP, MPI_GROUP_WORLD
#endif

  integer heap, stack, me
  integer :: t_num, iwr
  character*32 filename, cname, ctype
  logical t_ok,fchk
  integer size
  double precision :: t_b, t_e
  ga_init_time = 0.d0

  call mpi_init(ierr)

#ifdef USE_E4D
! Check if running an E4D Problem (command line argument)
! if so, get number of processors to allocate to E4D
      E4DPROCS = 0
      NARGS = COMMAND_ARGUMENT_COUNT()
      DO IARG = 1,NARGS
        CALL GET_COMMAND_ARGUMENT(IARG,ARG)
        ARG = TRIM(ARG)
        CALL LCASE(ARG)
        IF (ARG == "-e4d") THEN
           E4DFLAG = .true.
           CALL GET_COMMAND_ARGUMENT(IARG+1,TSTR)
           READ(TSTR,*) E4DPROCS
        ENDIF
      ENDDO

      IF (E4DFLAG) THEN
        WRITE(*,*) "E4D Option Selected, # of E4D Procs requested",E4DPROCS
!
!--- Create separate communicators for PETSC E4D
!
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,TOTPROCS,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD, MY_WRANK, ierr)
        E4D_SLAVE_START = (TOTPROCS - E4DPROCS)

!       Create a communicator with just the two world ranks for intercommunication 
!       between last STOMP Rank and the First E4D rank (main driver)
!       IF ((MY_WRANK.EQ.E4D_SLAVE_START).OR.(MY_WRANK.EQ.E4D_SLAVE_START-1)) THEN
        itcom(1)=E4D_SLAVE_START-1
        itcom(2)=E4D_SLAVE_START
        CALL MPI_COMM_GROUP(MPI_COMM_WORLD, MPI_GROUP_WORLD,ierr)
        CALL MPI_GROUP_INCL(MPI_GROUP_WORLD,2,itcom,INTER_GROUP,ierr)
        CALL MPI_COMM_CREATE(MPI_COMM_WORLD,INTER_GROUP,INTER_COMM,ierr)

        if (MY_WRANK.ge.E4D_SLAVE_START) THEN
            MYCOLOR = 1
            MYKEY = MY_WRANK - E4D_SLAVE_START
        ELSE
            MYCOLOR = 0
            MYKEY = MY_WRANK
        ENDIF

!       This will override the communicator created by PetscInitialize
        CALL MPI_COMM_SPLIT(MPI_COMM_WORLD, MYCOLOR, MYKEY, PETSC_COMM_WORLD, ierr)
        if (MY_WRANK.ge.E4D_SLAVE_START) THEN
               call MPI_COMM_DUP(PETSC_COMM_WORLD,SE4D_COMM, ierr)
        ENDIF

!       Check to see what happens
        CALL MPI_COMM_GROUP(PETSC_COMM_WORLD,MYGROUP, ierr)
        CALL MPI_COMM_RANK(PETSC_COMM_WORLD,MYGRANK, ierr)
        CALL MPI_COMM_SIZE(PETSC_COMM_WORLD,GROUPSIZE, ierr)
        WRITE(*,*) "Total Processors ",TOTPROCS
        WRITE(*,*) "MPI SPLIT: WORLD RANK",MY_WRANK," E4D_SLAVE_START",E4D_SLAVE_START
        WRITE(*,*) "Group",MYGROUP,"GROUPRANK",MYGRANK,"GROUPSIZE",GROUPSIZE
     ENDIF
     CALL MPI_COMM_RANK(MPI_COMM_WORLD, MY_WRANK, ierr)

!--- END E4D 
#endif
  t_b = MPI_Wtime()
  call ga_initialize()
  t_e = MPI_Wtime()
  ga_init_time = t_e-t_b

!
!--- Create two Groups of global arrays - it tries to use all of them otherwise
!
#ifdef USE_E4D
    IF (E4DFLAG) THEN
       allocate (gagrp_ranks(E4D_SLAVE_START))
       allocate (slavegrp_ranks(E4DPROCS))
       DO I = 1,E4D_SLAVE_START
           gagrp_ranks(I)=I-1
           !WRITE(*,*) "GA GROUP RANK",I-1
       ENDDO
       DO I = 1,E4DPROCS
           slavegrp_ranks(I)=E4D_SLAVE_START+I-1
           !WRITE(*,*) "SLAVE GROUP RANK",E4D_SLAVE_START+I-1
       ENDDO
       gagrp=ga_pgroup_create(gagrp_ranks,E4D_SLAVE_START)
       ! we don't do any GA with the slave group
       slavegrp=ga_pgroup_create(slavegrp_ranks,E4DPROCS)
       ! set the default group
       call ga_pgroup_set_default(gagrp)
       !WRITE(*,*) "GA NODES=",GA_NNODES()
       gae4d = .true.
   ENDIF

!
!--- The E4D Processors should stay in E4D_Driver for the duration of the simulation
!--- The last STOMP processor will send info the E4D
!
    IF (E4DFLAG) THEN
        IF (MY_WRANK.ge.E4D_SLAVE_START) THEN
            call e4d_driver
            GOTO 2001
        ENDIF
     ENDIF
!--- End E4D Patch
#endif

  !--- Initialize global arrays ---
  heap = 100000000
  stack = 100000000
  if (.not.ma_init(MT_DBL, stack, heap)) &
    call ga_error("ma_init failed in STOMP Driver ",-1)
  me = ga_nodeid()

  ! Initialize timers - should be its own module
  call grid_timer_initialize()

  t_b = MPI_Wtime()

  !--- Allocate memory for version control log ---
  allocate( cvs_id(1:400) )


  !--- Open output file ---
  if (me.eq.0) then
    iwr = 22 
    OPEN(UNIT=IWR, FILE='output', STATUS='unknown')
  endif

  ! Initialize in memory input buffer stream
  t_ok = bufferedread_init('input')

  !-- Read input file to buffer and determine memory requirements ---
  call step
  ! -- Process grid card and set up distributed arrays
  if( bufferedread_find( '~GRID' ) )then
     call rdgrid
  else
     chmsg = 'Missing Grid Card'
     if( me.eq.0 ) call wrmsgs( 4 )
  endif

#ifdef USE_E4D
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, MY_WRANK, ierr)
#endif

  ! Initilize parallel IO - required grid to be descritized
  call sio_init

   !---  Allocate and intialize global variables  ---
  call intlz

  !---  Read user input files  ---
  call rdinpt1

  !---  Intialize boundary variables  ---
  call in_boun

  setup_time=MPI_Wtime() - t_b

  !---  Call main program ---
  call stomp1

#ifdef USE_E4D
!--- E4D Close Out - 
IF ((E4DFLAG).AND.(MY_WRANK.eq.E4D_SLAVE_START-1)) THEN
     ! Send 0 to E4D to shut down and return
     WRITE(*,*) "About to Send closeout to E4D",MY_WRANK
     tone = 1
     CALL MPI_SEND(0,tone,MPI_INTEGER,E4D_SLAVE_START,1,MPI_COMM_WORLD,ierr)
     WRITE(*,*) "Sent closeout to E4D",MY_WRANK
ENDIF

!--- END E4D Patch- 
#endif

  total_time = MPI_Wtime() - t_b

  call grid_timer_terminate()
  call sio_terminate

  2001 CONTINUE
  !call ga_terminate
  call mpi_finalize(ierr)

  if (me.eq.0) close(iwr)

 stop 
end 
