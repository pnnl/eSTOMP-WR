!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDRST( INDX )
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
!     Read restart file.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, December 1992.
!     Last Modified by MD White, Battelle, PNL, October 15, 1997.




!     $Id: rdrst.F,v 1.18 2008/02/13 01:04:55 d3c002 Exp $
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
      USE COUP_WELL
      USE SIO
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!



!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*64 ADUM
      LOGICAL EX
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
!
!
      double precision, allocatable :: dvp(:)


!
!----------------------Data Statements---------------------------------!
!
 
      SAVE NSOLUX,IROM,IRFC,NSPRX
      SAVE FORM1,FORM2,FORM3,FORM4,FORM5,FORM6,FORM7
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
      SUBNMX = '/RDRST'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(159)(1:1),'$').EQ.0 ) CVS_ID(159) = &
      '$Id: rdrst.F,v 1.18 2008/02/13 01:04:55 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Open the restart file  ---
!
      me = ga_nodeid()
      IF( INDX.EQ.1 ) THEN
        INQUIRE( FILE=FNRS, EXIST=EX )

        IF( .NOT.EX ) THEN
          INDX = 3
          NCH = INDEX(FNRS(1:),'  ')-1
          CHMSG = 'Nonexistent Restart File: ' // FNRS(1:NCH)
          CALL WRMSGS( INDX )
        ELSE
!          OPEN(UNIT=IRS, FILE=FNRS, STATUS='OLD', FORM='FORMATTED')
          OPEN(UNIT=IRS, FILE=FNRS, STATUS='OLD',access='sequential')
        ENDIF
        DO 100 N = 1,26
          READ (IRS, '(A)') CHDUM
          IF( INDEX(CHDUM(1:),'Version').NE.0 ) THEN
            NCHA = INDEX(CHDUM(1:),'  ')-1
            IF( NCHA.GT.10 ) THEN
              ADUM = CHDUM(10:NCHA)
            ELSE
              ADUM = 'Unknown'
            ENDIF
            NCH = INDEX(CH_VRSN(1:),'  ')-1
            NCHA = INDEX(ADUM(1:),'  ')-1
            IF( ADUM(1:NCHA).NE.CH_VRSN(1:NCH) ) THEN
              INDX = 1
              CHMSG = 'Restart Version Number: ' // ADUM(1:NCHA)
              CALL WRMSGS( INDX )
            ENDIF
          ENDIF
  100   CONTINUE
!
!---  Read timing data  ---
!

        READ(IRS,'(6(1PE22.15),7(I9))') TMPS(1),TMPD(1),TMPX(1),TMPA(1), &
        RSDM(1),TMPC(1),NRIM(1),NRST,NFLD,NSOLUX,IROM,IRFC,NSPRX
        NSTEP = NRST
        IF( IROM.NE.IOM .AND. IROM.NE.ISLC(21) ) THEN
          INDX = 3
          CHMSG = 'Restart File Operational Mode Conflict'
          CALL WRMSGS( INDX )
       ENDIF
!
!---  Read field data by node numbers  ---
!
      ELSE
!
!---  Water Operational Mode ---
!
        allocate(dvp(num_nodes))
        IF( IROM.EQ.1 ) THEN
          NRSV = 6
          IF( (NSOLUX+NSPRX).LE.0 ) THEN
!            WRITE( FORM8(2:2),'(I1)' ) NRSV
!            DO 110 N = 1,NFLD
!              READ(IRS,FORM8) T(2,N),PL(2,N),PG(2,N),SL(2,N),
!     &          SGT(2,N),ASLMIN(2,N),NPHAZ(2,N),IPH(2,N)
             call read_rst( dvp )
             t(2,:) = dvp(:)
             call read_rst( dvp )
             pl(2,:) = dvp(:)
             call read_rst( dvp )
             pg(2,:) = dvp(:)
             call read_rst( dvp )
             sl(2,:) = dvp(:)
             call read_rst( dvp )
             sgt(2,:) = dvp(:)
             call read_rst( dvp )
             aslmin(2,:) = dvp(:)
             call read_rst( dvp )
             nphaz(2,:) = dvp(:)
             call read_rst( dvp )
             iph(2,:) = dvp(:)

  110       CONTINUE
          ELSE
!            WRITE( FORM9(2:2),'(I1)' ) NRSV
!            WRITE( FORM9(26:26),'(I1)' ) NSOLUX+NSPRX
!            DO 112 N = 1,NFLD
!              READ(IRS,FORM9) T(2,N),PL(2,N),PG(2,N),SL(2,N),
!     &          SGT(2,N),ASLMIN(2,N),NPHAZ(2,N),IPH(2,N),
!     &          (C(N,M),M=1,NSOLUX),(SP_C(N,NSP),NSP=1,NSPRX)
!  112       CONTINUE
             call read_rst( dvp )
             t(2,:) = dvp(:)
!print *,'t---t',t(2,46)
!stop
             call read_rst( dvp )
             pl(2,:) = dvp(:)
             call read_rst( dvp )
             pg(2,:) = dvp(:)
             call read_rst( dvp )
             sl(2,:) = dvp(:)
             call read_rst( dvp )
             sgt(2,:) = dvp(:)
             call read_rst( dvp )
             aslmin(2,:) = dvp(:)
             call read_rst( dvp )
             nphaz(2,:) = dvp(:)
             call read_rst( dvp )
             iph(2,:) = dvp(:)
             do isolu = 1,nsolux
               call read_rst( dvp )
               c(isolu,:) = dvp(:)
             enddo
             do isprx = 1,nsprx
               call read_rst( dvp )
               sp_c(isprx,:) = dvp(:)
             enddo
          endif
        ENDIF
        deallocate(dvp)
!***************Coupled well - Bryan*******************************
!---    Coupled-well data  ---
!
#ifdef USE_H5HUT
!        if(me.eq.0) print *,'n_cw',n_cw
        if(n_cw > 0) then
             allocate(p_cw_g(n_cw))
             p_cw_g = -1.d20
             n_cwx = 0
             istat = ior(sio_read_scalar_int(h5file,"N_CW",N_CWX),istat)
        DO NCW = 1,N_CWX
!          WRITE(IRS,'(1PE22.15)') P_CW_G(NCW)
          write(varname,"(A6,I1)") "P_CW_G",NCW
          istat =ior(sio_read_scalar_dbl(h5file,varname,P_CW_G(NCW)),istat)
        END DO
        endif
#else
        if(me.eq.0) then
!        write(*,*) 'me, n_cw:',me,n_cw
        allocate(p_cw_g(n_cw))
        p_cw_g = -1.d20
        N_CWX = 0
        READ(IRS,'(A)',end=900) CHDUM
        IF( INDEX(CHDUM(1:),'Coupled-Well Model Data').NE.0 .AND. &
         N_CW.GT.0 ) THEN
          READ(IRS,'(I6)') N_CWX
          DO 760 NCW = 1,N_CWX
            READ(IRS,'(1PE22.15)') P_CW_G(NCW)
  760     CONTINUE
        ENDIF
  900   CONTINUE
        endif
        call ga_igop(1,n_cwx,1,'max')
        if(n_cwx /=0 ) then
                call ga_dgop(1,p_cw_g,n_cwx,'+')
        endif
#endif
!
!---    Check for compatibility in number of coupled wells  ---
!
        IF( N_CW.GT.0 ) THEN
!
!---      Current simulation has more coupled wells than restart file,
!         issue warning  ---
!
          IF( N_CWX.LT.N_CW ) THEN
            INDX = 24
            CHMSG = 'Number of Coupled Wells > ' //  &
             'Number of Coupled Wells in Restart File'
            IMSG = N_CWX
            CALL WRMSGS( INDX )
!
!---      Current simulation has fewer coupled wells than restart file,
!         issue error  ---
!
          ELSEIF( N_CWX.LT.N_CW ) THEN
            INDX = 7
            CHMSG = 'Number of Coupled Wells < ' // &
             'Number of Coupled Wells in Restart File'
            IMSG = N_CWX
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
!
!---  Close the restart file  ---
!
        CLOSE( UNIT=IRS )
      ENDIF
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDRST group  ---
!
      RETURN
      END
!
!
!
!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE read_rst( DFV )
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
      use files
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
!
      INTEGER DIMS(3), LO(3), HI(3), LDxx(3), G_BUF, THREE
      LOGICAL STATUS
      integer, allocatable :: idx_buf(:,:)
      double precision, ALLOCATABLE :: VAL_BUF(:), buf3(:,:,:)
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
      SUBNMX = '/read_rst'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
!
      ICSN = ICSN+ICSNX
      INDX = 0
      VAR = 1.D+0
      if (me.eq.0) then
          ncount = 0
          nlayx = 4
          ijdim = nxdim*nydim*nlayx
!          ijdim = nxdim*nydim
          allocate(idx_buf(3,ijdim))
          allocate(val_buf(ijdim))
          ild = iaxmax - iaxmin + 1
          ijld = ild * (iaymax - iaymin + 1)
   20     CONTINUE
          do k=1,nzdim
          do j=1,nydim    
          do i=1,nxdim
            ncount = ncount + 1
            idx_buf(1,ncount) = i
            idx_buf(2,ncount) = j
            idx_buf(3,ncount) = k
!            ixx = i - iaxmin
!            iyy = j - iaymin
!            izz = k - iazmin
!            n = ixx + iyy*ild + izz*ijld + 1
            if( mod(ncount,ijdim).eq.0 ) then
              READ(irs,*) val_buf(1:ijdim)
              call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
              ncount = 0
            endif
          enddo
          enddo
          enddo
          if( mod(ncount,ijdim).gt.0 ) then
              ijdim = ncount
              READ(irs,*) val_buf(1:ijdim)
              call nga_scatter(g_buf,val_buf(1),idx_buf(1,1),ncount)
              ncount = 0
          endif
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
      buf3 = 0.d0
      call nga_get(g_buf,lo,hi,buf3(1,1,1),ldxx)
      n = 0
      do k = 1,ldxx(3)
      do j = 1,ldxx(2)
      do i = 1,ldxx(1)						      
        n = n + 1
        dfv(n) = buf3(i,j,k)
      enddo
      enddo
      enddo
!print *,'dfv',me,dfv(46)
      status = ga_destroy(g_buf)
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      status = ga_destroy(g_buf)
!
!---  End of read_rst group  ---
!
      RETURN
      END

