!----------------------Subroutine--------------------------------------!
!
SUBROUTINE WRRST
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
   !     Write restart files
   !
   !----------------------Authors-----------------------------------------!
   !
   !     Written by MD White, Battelle, PNL, February, 1993.
   !     Last Modified by MD White, Battelle, PNL, October 15, 1997.




   !     $Id: wrrst.F,v 1.16 2008/02/13 01:00:56 d3c002 Exp $
   !

   !----------------------Fortran 90 Modules------------------------------!
   !

   USE GLB_PAR

   USE TRNSPT
   USE SOURC
   USE SOLTN
   USE REACT
   USE HYST
   USE GRID
   USE FILES
   USE FDVP
   USE FDVH
   use grid_mod
   use sio
   USE COUP_WELL
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
   !
   !
   !----------------------Parameter Statements----------------------------!
   !



   !
   !----------------------Type Declarations-------------------------------!
   !
   CHARACTER*16 FN
   CHARACTER*4 FORM1
   CHARACTER*19 FORM2
   CHARACTER*37 FORM3
   CHARACTER*38 FORM4
   CHARACTER*20 FORM5
   CHARACTER*38 FORM6
   CHARACTER*39 FORM7
   CHARACTER*22 FORM8
   CHARACTER*40 FORM9
   CHARACTER*41 FORM10
   CHARACTER*16 FORM11
   CHARACTER*17 FORM12
   REAL*8 SP_CX(LSPR)
   double precision, dimension(:,:), allocatable :: dbuf
   logical t_ok
   integer idx,dim, dim1,dim2, iflg, dflg
   integer istat
   CHARACTER*64 name
   CHARACTER*64 GETSPNM
   EXTERNAL GETSPNM
   LOGICAL :: use_ga
   !      character*64 t_string
   !
   !----------------------Data Statements---------------------------------!
   !
   SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6,FORM7
   SAVE FORM8,FORM9,FORM10
   DATA FORM1 /'(I6)'/
   DATA FORM2 /'(1(1PE22.15,1X),I2)'/
   DATA FORM3 /'(1(1PE22.15,1X),I2,1X,1(1PE22.15,1X))'/
   DATA FORM4 /'(1(1PE22.15,1X),I2,1X,10(1PE22.15,1X))'/
   DATA FORM5 /'(10(1PE22.15,1X),I2)'/
   DATA FORM6 /'(10(1PE22.15,1X),I2,1X,1(1PE22.15,1X))'/
   DATA FORM7 /'(10(1PE22.15,1X),I2,1X,10(1PE22.15,1X))'/
   DATA FORM8 /'(1(1PE22.15,1X),2(I2))'/
   DATA FORM9 /'(1(1PE22.15,1X),2(I2),1X,1(1PE22.15,1X))'/
   DATA FORM10 /'(1(1PE22.15,1X),2(I2),1X,10(1PE22.15,1X))'/
   DATA FORM11 /'(1(1PE22.15,1X))'/
   DATA FORM12 /'(10(1PE22.15,1X))'/
   !
   !----------------------Executable Lines--------------------------------!
   !
   me = ga_nodeid()
   use_ga = .true.
   SUBNMX = '/WRRST'
   ICSNX = INDEX( SUBNMX,'  ' )-1
   SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
   IF( INDEX(CVS_ID(278)(1:1),'$').EQ.0 ) CVS_ID(278) = &
      '$Id: wrrst.F,v 1.16 2008/02/13 01:00:56 d3c002 Exp $' 
   ICSN = ICSN+ICSNX
   !
   !---  Create a new restart file with number of time steps
   !     as the file name extension  ---
   !
   N = 1
   NS = NSTEP
   10  NS = NS/10
   IF( NS .GE. 1 ) THEN
      N = N + 1
      GOTO 10
   ENDIF
   FN(1:8) = 'restart.'
   WRITE(FORM1(3:3),'(I1)') N
   WRITE( FN(9:),FORM1) NSTEP

   !Not sure what this is - was associated with writing double and int info;
   !Moved to be accessible to both IO strategies -kls
   NSPR = NSPG + NSPL + NSPN + NSPS

   var = 1.d0 ! no scaling for restart files
   avar = 0.d0 ! no offset for restart files
   DIM1 = 0
   DIM2 = 0

#ifdef USE_H5HUT
   istat = sio_openw("restart_", MPI_COMM_WORLD, NSTEP, TM)
   call sio_check(istat,"Warning: Unable to open HDF restart file"//trim(fn))

   istat = sio_write_string("Version",trim(CH_VRSN))
   call sio_check(istat,"Warning: Unable to write version")

   !---  Write timing data, field data by node numbers  ---
   istat = sio_write_scalar_dbl("TM",TM)
   istat =  ior(sio_write_scalar_dbl("DT",DT),istat)
   istat =  ior(sio_write_scalar_dbl("DTMX",DTMX),istat)
   istat = ior(sio_write_scalar_dbl("DTAF",DTAF),istat)
   istat = ior(sio_write_scalar_dbl("RSDMX",RSDMX),istat)
   istat = ior(sio_write_scalar_dbl("DTCF",DTCF),istat)

   istat = ior(sio_write_scalar_int("NRIMX",NRIMX),istat)
   istat = ior(sio_write_scalar_int("NSTEP",NSTEP),istat)
   istat = ior(sio_write_scalar_int("NFLD",NFLD),istat)
   istat = ior(sio_write_scalar_int("NSOLU",NSOLU),istat)
   istat = ior(sio_write_scalar_int("IOM",IOM),istat)
   istat = ior(sio_write_scalar_int("ISLC(5)",ISLC(5)),istat)
   istat = ior(sio_write_scalar_int("NSPR",NSPR),istat)
   if (istat /= 0) call ga_error("Unable to write restart attributes",istat);
#else
   if(me.eq.0) then
      OPEN(UNIT=IRS, FILE=FN, STATUS='UNKNOWN', FORM='FORMATTED')
      CLOSE(UNIT=IRS, STATUS='DELETE')
      OPEN(UNIT=IRS, FILE=FN, STATUS='NEW', FORM='FORMATTED')
      !
      !---  Write header  ---
      !
      WRITE(IRS,'(A,//)')' Welcome to ...'
      WRITE(IRS,'(A)')   '                          eSTOMP'
      WRITE(IRS,'(A,//)')'                  A scalable version of'
      WRITE(IRS,'(A,//)')'        Subsurface Transport Over Multiple Phases'
      WRITE(IRS,'(A)')   ' This file was produced by eSTOMP, a derivative work of STOMP.'
      WRITE(IRS,'(A)')   ' STOMP was developed by the Pacific Northwest Laboratory, with'
      WRITE(IRS,'(A)')   ' support from the VOC-Arid Integrated Demonstration Project,'
      WRITE(IRS,'(A)')   ' Office of Technology Development, U.S. Department of Energy.'
      WRITE(IRS,'(A)')   ' Results from this version of STOMP should not be used for'
      WRITE(IRS,'(A,/)') ' license related applications.'
      WRITE(IRS,'(A,/)') ' For inquiries or assistance:  Call (509) 372-4067'
      WRITE(IRS,'(A,//)')'                       ---  RESTART  ---'

      WRITE(IRS,'(2A)') 'Version: ',CH_VRSN
      WRITE(IRS,'(A)') 'Date: '
      WRITE(IRS,'(A)') 'Time: '

   !
   !---  Write timing data, field data by node numbers  ---
   !
     WRITE(IRS,'(/,6(1PE22.15),7(I9))') TM,DT,DTMX,DTAF,RSDMX,DTCF, &
           NRIMX,NSTEP,NFLD,NSOLU,IOM,ISLC(5),NSPR
   endif
#endif

   !
   !---  Water Operational Mode ---
   !

   IF( IOM.EQ.1 ) THEN
      NRSV = 6
      IF( (NSOLU+NSPR).LE.0 ) THEN
         WRITE( FORM8(2:2),'(I1)' ) NRSV
         iflg = 0
         dflg = 1
         dim = 2
         dim1 = 2
         call string2idx(dim,iflg,dflg,'temperature',idx,t_ok)
         call write_var('temperature',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'pressure_w',idx,t_ok)
         call write_var('pressure_w',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'pressure_g',idx,t_ok)
         call write_var('pressure_g',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'saturation_w',idx,t_ok)
         call write_var('saturation_w',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'sgt',idx,t_ok)
         call write_var('sgt',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'aslmin',idx,t_ok)
         call write_var('aslmin',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         iflg = 1
         dflg = 0
         call string2idx(dim,iflg,dflg,'nphaz',idx,t_ok)
         call write_var('nphaz',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'iph',idx,t_ok)
         call write_var('iph',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

!          DO N = 1,NFLD
!            WRITE(IRS,FORM8) T(2,N),PL(2,N),PG(2,N),SL(2,N), &
!            SGT(2,N),ASLMIN(2,N),NPHAZ(2,N),IPH(2,N)
!          ENDDO
      ELSE
         !		IF( (NSOLU+NSPR).LT.10 ) THEN
         WRITE( FORM9(2:2),'(I1)' ) NRSV
         WRITE( FORM9(26:26),'(I1)' ) NSOLU+NSPR


         IF( ISLC(40).EQ.1 ) THEN
            allocate(dbuf(nspr,num_nodes))
            !           dbuf = 
            !           iflg = 0
            !           dflg = 1
            !           dim = 2
            !           dim1 = 2
            !           avar = 0.d0
            !           call string2idx(dim,iflg,dflg,'sp_c',idx,t_ok)
            dbuf(1:nspr,1:num_nodes) = sp_c(1:nspr,1:num_nodes)
            DO N = 1,num_nodes
               !
               !---        Species concentrations  ---
               !
               DO NSP = 1,NSPR
                  IF( NSP.GT.NSPL .AND. NSP.LE.NSPL+NSPS ) THEN
                     NSP_M = NSP-NSPL
                     IF( ISP_MN(NSP).EQ.1 ) THEN
                        SP_C(NSP,n) = SP_C(NSP,n)+SP_CMN(NSP_M,n)
                     ENDIF
                  ENDIF
                  IF( SP_C(NSP,n).LT.1.D-30 ) SP_C(NSP,n) = 0.D+0
               ENDDO
               !              WRITE(IRS,FORM9) T(2,N),PL(2,N),PG(2,N),SL(2,N), &
               !              SGT(2,N),ASLMIN(2,N),NPHAZ(2,N),IPH(2,N), &
               !              (C(N,NSL),NSL=1,NSOLU),(SP_CX(NSP),NSP=1,NSPR)
            ENDDO
         ENDIF

         iflg = 0
         dflg = 1
         dim = 2
         dim1 = 2
         call string2idx(dim,iflg,dflg,'temperature',idx,t_ok)
         call write_var('temperature',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'pressure_w',idx,t_ok)
         call write_var('pressure_w',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'pressure_g',idx,t_ok)
         call write_var('pressure_g',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'saturation_w',idx,t_ok)
         call write_var('saturation_w',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'sgt',idx,t_ok)
         call write_var('sgt',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'aslmin',idx,t_ok)
         call write_var('aslmin',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         iflg = 1
         dflg = 0
         call string2idx(dim,iflg,dflg,'nphaz',idx,t_ok)
         call write_var('nphaz',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         call string2idx(dim,iflg,dflg,'iph',idx,t_ok)
         call write_var('iph',' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)

         iflg = 0
         dflg = 1
         dim = 2
         avar = 0.d0
         do isolu = 1,nsolu
            dim1 = isolu
            call string2idx(dim,iflg,dflg,'solute_conc',idx,t_ok)
            call write_var(solut(isolu),' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)
         enddo
         if(nspr.gt.0) then
            iflg = 0
            dflg = 1
            dim = 2

            do ispr = 1,nspr
               dim1 = ispr
               avar = 0.d0
               call string2idx(dim,iflg,dflg,'sp_c',idx,t_ok)
               name = getspnm(ispr)
               call write_var(trim(name),' ',iflg,dflg,idx,dim,dim1,dim2,var,avar,me,ipl,form12,irs)
            enddo
            sp_c(1:nspr,1:num_nodes) = dbuf(1:nspr,1:num_nodes)

            deallocate(dbuf)
         endif

      ENDIF
   ENDIF
!***************Coupled well - Bryan****************
!
!---  Coupled-well data  ---
!
#ifdef USE_H5HUT
   if(n_cw > 0) then
     istat = ior(sio_write_scalar_int("N_CW",N_CW),istat)
        DO NCW = 1,N_CW
!          WRITE(IRS,'(1PE22.15)') P_CW_G(NCW)
          write(varname,"(A6,I1)") "P_CW_G",NCW
          istat =  ior(sio_write_scalar_dbl(varname,P_CW_G(NCW)),istat)
        END DO
   endif
#else
      IF( N_CW.GT.0 .and.me.eq.0) THEN
        WRITE(IRS,'(A)') 'Coupled-Well Model Data'
        WRITE(FORM1(3:3),'(I1)') ICOUNT(N_CW)
        WRITE(IRS,FORM1) N_CW
        DO 730 NCW = 1,N_CW
          WRITE(IRS,'(1PE22.15)') P_CW_G(NCW)
  730   CONTINUE
      ENDIF
#endif
!****************************************************
   !
   !---  Close the restart file  ---
   !
#ifdef USE_H5HUT
     istat = sio_close()
     call sio_check(istat,"Warning: Unable to close HDF restart file")
#else
   if(me.eq.0)CLOSE( UNIT=IRS )
#endif
   ICSN = ICSN-ICSNX
   SUBNM = SUBNM(1:ICSN)
   !
   !---  End of WRRST group
   !
   RETURN
   END
