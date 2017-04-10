!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDOU1
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
!     Read input file for output information.
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
      USE OUTPU
      USE GRID
      USE FILES
      USE CONST
      USE GRID_MOD
      USE BUFFEREDREAD
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
      EXTERNAL ICOUNT
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
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,UNTS,SOLNM,SPNM
      CHARACTER*512 CHDUM
      CHARACTER*6 FORM
      CHARACTER*4 FORM1
      LOGICAL T_OK
!
!----------------------Data Statements---------------------------------!
!
      SAVE FORM,FORM1
      DATA FORM / '(I6,$)' /
      DATA FORM1 / '(I )' /
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/RDOU1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
     '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
!
!---  Write card information to ouput file  ---
!
      CARD = 'Output Control Card'
      ICD = INDEX( CARD,'  ' )-1
      if(me.eq.0) WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read reference node information  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reference Nodes: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NREF)
!      IF( NREF.GT.LREF ) THEN
!        INDX = 5
!        CHMSG = 'Number of Reference Nodes > LREF'
!        CALL WRMSGS( INDX )
!      ENDIF
      if(me.eq.0) WRITE(IWR,'(/,A)') 'Reference Node No. and Indices'
      if(.not.allocated(ndref))allocate(ndref(nref))
      DO 100 N = 1,NREF
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Reference Node Index'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IRF)
        CALL RDINT(ISTART,ICOMMA,CHDUM,JRF)
        CALL RDINT(ISTART,ICOMMA,CHDUM,KRF)
        IF( IRF.LT.1 .OR. IRF.GT.nxdim .OR. JRF.LT.1 .OR. &
        JRF.GT.nydim .OR. KRF.LT.1 .OR. KRF.GT.nzdim) THEN
          INDX = 4
          CHMSG = 'Out-of-Range Reference Node Index'
          CALL WRMSGS( INDX )
        ENDIF
        nidx = (irf-1)+(jrf-1)*nxdim+(krf-1)*nxdim*nydim+1
        lidx = 0
        do nx = 1,num_nodes
         if(loc2nat(nx).eq.nidx) then
           lidx = nx
           exit
         endif
        enddo
        NDREF(N) = LIDX
        CALL GA_IGOP(1,NIDX,1,'max')
        WRITE(FORM(3:3),'(I1)') ICOUNT(nidx)
        if(me.eq.0) WRITE(IWR,'(2X,A,$)') 'Reference Node No. '
        if(me.eq.0) WRITE(IWR,FORM) NIDX
        WRITE(FORM(3:3),'(I1)') ICOUNT(IRF)
        if(me.eq.0) WRITE(IWR,'(2X,A,$)') 'I = '
        if(me.eq.0) WRITE(IWR,FORM) IRF
        WRITE(FORM(3:3),'(I1)') ICOUNT(JRF)
        if(me.eq.0) WRITE(IWR,'(2X,A,$)') 'J = '
        if(me.eq.0) WRITE(IWR,FORM) JRF
        WRITE(FORM(3:3),'(I1)') ICOUNT(KRF)
        if(me.eq.0) WRITE(IWR,'(2X,A,$)') 'K = '
        if(me.eq.0) WRITE(IWR,FORM) KRF
        if(me.eq.0) WRITE(IWR,'(2X,A)' ) 'Indices'
  100 CONTINUE
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      IDFLT = 1
      IFQS = IBIG
      VARB = 'Reference Node Screen Output Frequency'
      CALL RDINT(ISTART,ICOMMA,CHDUM,IFQS)
      if(me.eq.0) WRITE(IWR,'(/,2A,I6,A)') VARB(1:IVR),': Every ',IFQS, &
    ' Time Step(s)'
      IF( IFQS.LE.0 ) IFQS = IBIG
      IDFLT = 1
      IFQO = IBIG
      VARB = 'Reference Node Output File Frequency'
      CALL RDINT(ISTART,ICOMMA,CHDUM,IFQO)
      if(me.eq.0) WRITE(IWR,'(2A,I6,A)') VARB(1:IVR),': Every ',IFQO,' Time Step(s)'
      IF( IFQO.LE.0 ) IFQO = IBIG
      IDFLT = 1
      VARB = 'Time Output Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTM)
      if(me.eq.0) WRITE(IWR,'(3A)') VARB(1:IVR),': ',UNTM(1:NCH)
      IDFLT = 1
      VARB = 'Length Output Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNLN)
      if(me.eq.0) WRITE(IWR,'(3A)') VARB(1:IVR),': ',UNLN(1:NCH)
      IF( ICS.EQ.2 .OR. ICS.EQ.6 ) THEN
        IDFLT = 1
        VARB = 'Arc Output Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNAR)
        if(me.eq.0) WRITE(IWR,'(3A)') VARB(1:IVR),': ',UNAR(1:NCH)
      ENDIF
      IDFLT = 1
      VARB = 'Screen Significant Digits'
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISGNS)
      if(me.eq.0) WRITE(IWR,'(2A,I2)') VARB(1:IVR),': ',ISGNS
      IDFLT = 1
      VARB = 'Output File Significant Digits'
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISGNO)
      if(me.eq.0) WRITE(IWR,'(2A,I2)') VARB(1:IVR),': ',ISGNO
      IDFLT = 1
      VARB = 'Plot File Significant Digits'
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISGNP)
      if(me.eq.0) WRITE(IWR,'(2A,I2)') VARB(1:IVR),': ',ISGNP
!
!---  Read reference node variables  ---
!
      IF( ME.EQ.0 )WRITE( IWR,'(/,A)') 'Reference Node Variables'
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Reference Node Variables: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NVREF)
      NVC = 0
      DO 200 NV = 1,NVREF
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART=1
        VARB = 'Reference Node Variable: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          VARB = 'Reference Node Variable: Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
          DO 110 NSL = 1,NSOLU
            IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 120
  110     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Reference-Node Solute Name: '//SOLNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 200
  120     CONTINUE
        ENDIF

        IF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          IF( ISLC(40).EQ.0 ) THEN
            NVC = NVC -1
            GOTO 200
          ENDIF
          VARB = 'Reference Node Variable: Reactive Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SPNM)
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
            DO 130 NSL = NSOLU+1,NSOLU+NEQC+NEQK
              IF( SPNM.EQ.SOLUT(NSL) ) GOTO 150
  130       CONTINUE
          ENDIF
!
!---      Aqueous species  ---
!
          DO 132 M = 1,NSPL
            NSP = M
            IF( SPNM.EQ.SPNML(M) ) GOTO 150
  132     CONTINUE
!
!---      Solid species  ---
!
          DO 134 M = 1,NSPS
            NSP = M+NSPL
            IF( SPNM.EQ.SPNMS(M) ) GOTO 150
  134     CONTINUE
!
!---      Exchanged species  ---
!
          DO 135 M = 1,NSPE
            NSP = M+NSPL+NSPS
            IF( SPNM.EQ.SPNME(M) ) GOTO 150
  135     CONTINUE
!
!---      Unrecognized species name  ---
!
          INDX = 4
          CHMSG = 'Unrecognized Reference-Node Reactive Species Name: ' &
          // SPNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 200
  150     CONTINUE
        ENDIF
        IF( INDEX(ADUM(1:),'aqueous pressure').NE.0 ) THEN
          IREF(NV) = 1
        ELSEIF( INDEX(ADUM(1:),'gas pressure').NE.0 ) THEN
          IREF(NV) = 2
        ELSEIF( INDEX(ADUM(1:),'matrix pressure').NE.0 ) THEN
          CHREF(3) = ' PM '
          IREF(NV) = 3
        ELSEIF( INDEX(ADUM(1:),'temperature').NE.0 ) THEN
          IREF(NV) = 4
        ELSEIF( INDEX(ADUM(1:),'phase condition').NE.0 ) THEN
          IREF(NV) = 5
        ELSEIF( INDEX(ADUM(1:),'aqueous gauge pressure').NE.0 ) THEN
          IREF(NV) = 6
        ELSEIF( INDEX(ADUM(1:),'gas gauge pressure').NE.0 ) THEN
          IREF(NV) = 7
        ELSEIF( INDEX(ADUM(1:),'apparent aqueous sat').NE.0 ) THEN
          IREF(NV) = 9
        ELSEIF( INDEX(ADUM(1:),'effective trapped gas').NE.0 ) THEN
          IREF(NV) = 19
        ELSEIF( INDEX(ADUM(1:),'trapped gas sat').NE.0 ) THEN
          IREF(NV) = 105
        ELSEIF( INDEX(ADUM(1:),'aqueous saturation').NE.0 ) THEN
          IREF(NV) = 11
        ELSEIF( INDEX(ADUM(1:),'gas saturation').NE.0 ) THEN
          IREF(NV) = 12
        ELSEIF( INDEX(ADUM(1:),'aqueous moisture cont').NE.0 ) THEN
          IREF(NV) = 15
        ELSEIF( INDEX(ADUM(1:),'gravimetric moisture cont').NE.0 ) THEN
          IREF(NV) = 16
        ELSEIF( INDEX(ADUM(1:),'diffusive porosity').NE.0 ) THEN
          IREF(NV) = 20
        ELSEIF( INDEX(ADUM(1:),'water aqueous mass frac').NE.0 ) THEN
          IREF(NV) = 24
        ELSEIF( INDEX(ADUM(1:),'aqueous hydraulic head').NE.0 ) THEN
          IREF(NV) = 27
        ELSEIF( INDEX(ADUM(1:),'rock/soil type').NE.0 ) THEN
          IREF(NV) = 30
        ELSEIF( INDEX(ADUM(1:),'aqueous density').NE.0 ) THEN
          IREF(NV) = 34
        ELSEIF( INDEX(ADUM(1:),'total water mass').NE.0 ) THEN
          IREF(NV) = 37
        ELSEIF( INDEX(ADUM(1:),'water mass source int').NE.0 ) THEN
          IREF(NV) = 40
        ELSEIF( INDEX(ADUM(1:),'aqueous courant').NE.0 ) THEN
          ICRNT = 1
          IREF(NV) = 49
        ELSEIF( INDEX(ADUM(1:),'x aqueous vol').NE.0 ) THEN
          IREF(NV) = 51
!vlf
!          INDX = 4
!          CHMSG = 'Surface velocities not supported: '//ADUM
!          CALL WRMSGS( INDX )
!vlf
        ELSEIF( INDEX(ADUM(1:),'y aqueous vol').NE.0 ) THEN
          IREF(NV) = 52
!vlf
!          INDX = 4
!          CHMSG = 'Surface velocities not supported: '//ADUM
!          CALL WRMSGS( INDX )
!vlf
        ELSEIF( INDEX(ADUM(1:),'z aqueous vol').NE.0 ) THEN
          IREF(NV) = 53
!vlf
!          INDX = 4
!          CHMSG = 'Surface velocities not supported: '//ADUM
!          CALL WRMSGS( INDX )
!vlf
        ELSEIF( INDEX(ADUM(1:),'aqueous matrix').NE.0 ) THEN
          IREF(NV) = 83
        ELSEIF( INDEX(ADUM(1:),'aqueous fracture').NE.0 ) THEN
          IREF(NV) = 84
        ELSEIF( INDEX(ADUM(1:),'xnc aqueous vol').NE.0 ) THEN
          IREF(NV) = 87
        ELSEIF( INDEX(ADUM(1:),'ync aqueous vol').NE.0 ) THEN
          IREF(NV) = 88
        ELSEIF( INDEX(ADUM(1:),'znc aqueous vol').NE.0 ) THEN
          IREF(NV) = 89
        ELSEIF( INDEX(ADUM(1:),'space monitor').NE.0 ) THEN
          IREF(NV) = 100
        ELSEIF( INDEX(ADUM(1:),'water mass source rate').NE.0 ) THEN
          IREF(NV) = 140
        ELSEIF( INDEX(ADUM(1:),'aqueous well depth').NE.0 ) THEN
          IREF(NV) = 144
        ELSEIF( INDEX(ADUM(1:),'well flow rate').NE.0 ) THEN
          IREF(NV) = 145
        ELSEIF( INDEX(ADUM(1:),'well flow integral').NE.0 ) THEN
          IREF(NV) = 146
        ELSEIF( INDEX(ADUM(1:),'scanning path').NE.0 ) THEN
          IREF(NV) = 149
        ELSEIF( INDEX(ADUM(1:),'aqueous viscosity').NE.0 ) THEN
          IREF(NV) = 176
        ELSEIF( INDEX(ADUM(1:),'matric pot').NE.0 ) THEN
          IREF(NV) = 63
        ELSEIF( INDEX(ADUM(1:),'integrated water mass').NE.0 ) THEN
          IREF(NV) = 191
        ELSEIF( INDEX(ADUM(1:),'x aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 201
        ELSEIF( INDEX(ADUM(1:),'y aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 202
        ELSEIF( INDEX(ADUM(1:),'z aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 203
        ELSEIF( INDEX(ADUM(1:),'x intrinsic perm').NE.0 ) THEN
          IREF(NV) = 247
        ELSEIF( INDEX(ADUM(1:),'y intrinsic perm').NE.0 ) THEN
          IREF(NV) = 248
        ELSEIF( INDEX(ADUM(1:),'z intrinsic perm').NE.0 ) THEN
          IREF(NV) = 249
        ELSEIF( INDEX(ADUM(1:),'aqueous relative perm').NE.0 ) THEN
          IREF(NV) = 31
        ELSEIF( INDEX(ADUM(1:),'ponding height').NE.0 ) THEN
          IREF(NV) = 277

! Add exchange, solid, gas, napl species conc per total volume
        ELSEIF( (INDEX(ADUM(1:),'solid solute volumetric conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'solid species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 26 
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 26
            CHREF(INDX) = ' SP '
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF(  (INDEX(ADUM(1:),'exchange species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          INDX = 400+(NSL-1)*33 + 27
          iref(nv) = indx
          CHREF(INDX) = ' SP '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'gas solute volumetric conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'gas species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 28 
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 28
            CHREF(INDX) = ' SP '
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( (INDEX(ADUM(1:),'napl species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          INDX = 400+(NSL-1)*33 + 29
          iref(nv) = indx
          CHREF(INDX) = ' SP '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'solute volumetric conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 1
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 1
            CHREF(INDX) = ' SP '
            UNREF(INDX) = 'mol/m^3'
          ENDIF
!
! 
! Add exchange, solid, gas, napl species conc per total water volume
!
        ELSEIF( (INDEX(ADUM(1:),'solid solute aqueous conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'solid species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 30
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 30
            CHREF(INDX) = 'SPL '
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( &
        (INDEX(ADUM(1:),'exchange species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 31
          INDX = 400+(NSL-1)*33 + 31
          CHREF(INDX) = 'SPL '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'gas solute aqueous conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'gas species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 32
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 32 
            CHREF(INDX) = 'SPL '
            UNREF(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( &
        (INDEX(ADUM(1:),'napl species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 33
          INDX = 400+(NSL-1)*33 + 33 
          CHREF(INDX) = 'SPL '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'solute aqueous conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 2
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 2
            CHREF(INDX) = 'SPL '
            UNREF(INDX) = 'mol/m^3'
          ENDIF

        ELSEIF( INDEX(ADUM(1:),'solute aqueous mol').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 5
        ELSEIF( INDEX(ADUM(1:),'solute integrated aqueous').NE.0 ) THEN
          WRITE(FORM1(3:3),'(I1)') ICOUNT(NSL)
          INDX = 400+(NSL-1)*33 + 6
          IREF(NV) = INDX
          CHREF(INDX)(1:3) = 'ICL'
          WRITE( CHREF(INDX)(4:),FORM1) NSL
          UNREF(INDX) = 'null'
        ELSEIF( INDEX(ADUM(1:),'solute inventory').NE.0 ) THEN
          INDX = 400+(NSL-1)*33 + 7
          IREF(NV) = INDX
          CHREF(INDX)(1:2) = 'CI'
        ELSEIF( INDEX(ADUM(1:),'x solute flux').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 8
        ELSEIF( INDEX(ADUM(1:),'y solute flux').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 9
        ELSEIF( INDEX(ADUM(1:),'z solute flux').NE.0 ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 10
        ELSEIF( (INDEX(ADUM(1:),'solute source').NE.0) .OR.  &
       ((INDEX(ADUM(1:),'species source').NE.0) .AND.  &
       (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 11
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 11
            CHREF(INDX) = 'SPSR'
            UNREF(INDX) = 'mol/s'
          ENDIF
        ELSEIF( (INDEX(ADUM(1:),'solute integrated mass').NE.0) .OR. &
        ((INDEX(ADUM(1:),'species integrated mass').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IREF(NV) = 400+(NSL-1)*33 + 23
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 23
            CHREF(INDX) = 'SPIM'
            UNREF(INDX) = 'mol'
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'species volumetric conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 1
          IREF(NV) = INDX
          CHREF(INDX) = ' SP '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species aqueous conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 2
          IREF(NV) = INDX
          CHREF(INDX) = 'SPL '
          UNREF(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species source').NE.0 ) THEN
          IREF(NV) = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 11
        ELSEIF( INDEX(ADUM(1:),'species integrated mass').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 23
          IREF(NV) = INDX
          CHREF(INDX) = 'SPIM'
          UNREF(INDX) = 'mol'
       ELSEIF( INDEX(ADUM(1:),'mineral area').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 24
          IREF(NV) = INDX
!           print *, 'area indx: ',indx,neqc,neqk,nsp
          CHREF(INDX) = 'SPMA'
          UNREF(INDX) = 'm^2'
        ELSEIF( INDEX(ADUM(1:),'mineral rate').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 25
          IREF(NV) = INDX
!           print *, 'rate indx: ',indx,neqc,neqk,nsp
          CHREF(INDX) = 'SPMR'
          UNREF(INDX) = 'mol/s'
        ELSEIF( INDEX(ADUM(1:),'volume fraction').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 26
          IREF(NV) = INDX
          CHREF(INDX) = 'SPVF'
        ELSEIF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 27
          IREF(NV) = INDX
          CHREF(INDX) = 'pH'
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Reference Node Variable: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
        IDFLT = 1
        VARB = 'Reference Node Variable Unit: '
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNREF(IREF(NV)))
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          IF( ME.EQ.0 )WRITE( IWR,'(2X,3A,2X,2A,I2,A)' ) ADUM(1:NCH),', ', &
          UNREF(IREF(NV))(1:NCU),SOLNM(1:NCS),' Solute(',NSL,')'

        ELSEIF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          IF( ME.EQ.0 )WRITE( IWR,'(2X,3A,2X,2A,I2,A)' ) ADUM(1:NCH),', ', &
          UNREF(IREF(NV))(1:NCU),SPNM(1:NCS),' Species(',NSP,')'

        ELSE
          IF( ME.EQ.0 )WRITE( IWR,'(2X,3A)' ) ADUM(1:NCH),', ',UNREF(IREF(NV))(1:NCU)
        ENDIF
        CALL RDOUUN( IREF(NV) )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT( UNREF(IREF(NV)),VAR,INDX )
  200 CONTINUE
      NVREF = NVREF + NVC
!
!---  Plot file output times  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Plot File Output Times: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NPRTM)
!      IF( NPRTM.GT.LPTM ) THEN
!        INDX = 5
!        CHMSG = 'Number of Plot File Times > Parameter LPTM'
!        CALL WRMSGS( INDX )
!      ENDIF
      if(.not.allocated(prtm))allocate(prtm(nprtm))
      if(me.eq.0) WRITE(IWR,'(/,A)') VARB(1:IVR)
      DO 300 N = 1, NPRTM
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
        VARB = 'Plot File Output Time'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,PRTM(N))
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        if(me.eq.0) WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',PRTM(N)
        INDX = 0
        IUNS = 1
        CALL RDUNIT(UNTS,PRTM(N),INDX)
        if(me.eq.0) WRITE(IWR,'(A,1PE11.4,A)') ' (',PRTM(N),', s)'
        TMPR = MIN( TMPR,PRTM(N) )
 300  CONTINUE
      if(me.eq.0) WRITE(IWR,'(2X,A)') 'After the Final Time Step'
!
!---  Read Plot File Variables  ---
!
      IF( ME.EQ.0 )WRITE( IWR,'(/,A)') 'Plot File Variables:'
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Plot File Variables: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NVPLOT)
      NVC = 0
      DO 400 NV = 1,NVPLOT
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART=1
        VARB = 'Plot File Variable: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          VARB = 'Plot File Variable: Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
          DO 310 NSL = 1,NSOLU
            IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 320
  310     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Plot File Solute Name: '//SOLNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 400
  320     CONTINUE
        ENDIF

        IF( INDEX( ADUM(1:),'species' ).NE.0 ) THEN
          IF( ISLC(40).EQ.0 ) THEN
            NVC = NVC -1
            GOTO 400
          ENDIF
          VARB = 'Plot File Variable: Reactive Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SPNM)
!
!---      Conservation- or kinetic-component species  ---
!
          IF( INDEX( SPNM(1:),'total_' ).NE.0 ) THEN
            DO 330 NSL = NSOLU+1,NSOLU+NEQC+NEQK
              IF( SPNM.EQ.SOLUT(NSL) ) GOTO 350
  330       CONTINUE
          ENDIF
!
!---      Aqueous species  ---
!
          DO 332 M = 1,NSPL
            NSP = M
            IF( SPNM.EQ.SPNML(M) ) GOTO 350
  332     CONTINUE
!
!---      Solid species  ---
!
          DO 334 M = 1,NSPS
            NSP = M+NSPL
            IF( SPNM.EQ.SPNMS(M) ) GOTO 350
  334     CONTINUE
!
!---      Exchanged species  ---
!
          DO 336 M = 1,NSPE
            NSP = M + NSPL + NSPS
            IF( SPNM.EQ.SPNME(M) ) GOTO 350
  336     CONTINUE
          INDX = 4
          CHMSG = 'Unrecognized Plot File Reactive Species Name: ' &
          // SPNM
          CALL WRMSGS( INDX )
          NVC = NVC -1
          GOTO 400
  350     CONTINUE
        ENDIF

        IF( INDEX(ADUM(1:),'final restart').NE.0 ) THEN
          ISLC(18) = 1
          IPLOT(NV) = 200
        ELSEIF( INDEX(ADUM(1:),'no restart').NE.0 ) THEN
          ISLC(18) = 2
          IPLOT(NV) = 200
        ELSEIF( INDEX(ADUM(1:),'aqueous pressure').NE.0 ) THEN
          IPLOT(NV) = 1
        ELSEIF( INDEX(ADUM(1:),'gas pressure').NE.0 ) THEN
          IPLOT(NV) = 2
        ELSEIF( INDEX(ADUM(1:),'matrix pressure').NE.0 ) THEN
          IPLOT(NV) = 3
        ELSEIF( INDEX(ADUM(1:),'temperature').NE.0 ) THEN
          IPLOT(NV) = 4
        ELSEIF( INDEX(ADUM(1:),'phase condition').NE.0 ) THEN
          IPLOT(NV) = 5
        ELSEIF( INDEX(ADUM(1:),'aqueous gauge pressure').NE.0 ) THEN
          IPLOT(NV) = 6
        ELSEIF( INDEX(ADUM(1:),'gas gauge pressure').NE.0 ) THEN
          IPLOT(NV) = 7
        ELSEIF( INDEX(ADUM(1:),'apparent aqueous sat').NE.0 ) THEN
          IPLOT(NV) = 9
        ELSEIF( INDEX(ADUM(1:),'effective trapped gas').NE.0 ) THEN
          IPLOT(NV) = 19
        ELSEIF( INDEX(ADUM(1:),'trapped gas sat').NE.0 ) THEN
          IPLOT(NV) = 105
        ELSEIF( INDEX(ADUM(1:),'aqueous saturation').NE.0 ) THEN
          IPLOT(NV) = 11
        ELSEIF( INDEX(ADUM(1:),'gas saturation').NE.0 ) THEN
          IPLOT(NV) = 12
        ELSEIF( INDEX(ADUM(1:),'aqueous moisture cont').NE.0 ) THEN
          IPLOT(NV) = 15
        ELSEIF( INDEX(ADUM(1:),'gravimetric moisture cont').NE.0 ) THEN
          IPLOT(NV) = 16
        ELSEIF( INDEX(ADUM(1:),'diffusive porosity').NE.0 ) THEN
          IPLOT(NV) = 20
        ELSEIF( INDEX(ADUM(1:),'water aqueous mass frac').NE.0 ) THEN
          IPLOT(NV) = 24
        ELSEIF( INDEX(ADUM(1:),'aqueous hydraulic head').NE.0 ) THEN
          IPLOT(NV) = 27
        ELSEIF( INDEX(ADUM(1:),'rock/soil type').NE.0 ) THEN
          IPLOT(NV) = 30
        ELSEIF( INDEX(ADUM(1:),'aqueous density').NE.0 ) THEN
          IPLOT(NV) = 34
        ELSEIF( INDEX(ADUM(1:),'total water mass').NE.0 ) THEN
          IPLOT(NV) = 37
        ELSEIF( INDEX(ADUM(1:),'water mass source int').NE.0 ) THEN
          IPLOT(NV) = 40
        ELSEIF( INDEX(ADUM(1:),'aqueous courant').NE.0 ) THEN
          ICRNT = 1
          IPLOT(NV) = 49
        ELSEIF( INDEX(ADUM(1:),'x aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 51
!vlf
          INDX = 4
          CHMSG = 'Surface velocities not supported: '//ADUM
          CALL WRMSGS( INDX )
!vlf
        ELSEIF( INDEX(ADUM(1:),'y aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 52
!vlf
          INDX = 4
          CHMSG = 'Surface velocities not supported: '//ADUM
          CALL WRMSGS( INDX )
!vlf
        ELSEIF( INDEX(ADUM(1:),'z aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 53
!vlf
          INDX = 4
          CHMSG = 'Surface velocities not supported: '//ADUM
          CALL WRMSGS( INDX )
!vlf
        ELSEIF( INDEX(ADUM(1:),'matric pot').NE.0 ) THEN
          IPLOT(NV) = 63
        ELSEIF( INDEX(ADUM(1:),'aqueous matrix').NE.0 ) THEN
          IPLOT(NV) = 83
        ELSEIF( INDEX(ADUM(1:),'aqueous fracture').NE.0 ) THEN
          IPLOT(NV) = 84
        ELSEIF( INDEX(ADUM(1:),'xnc aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 87
        ELSEIF( INDEX(ADUM(1:),'ync aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 88
        ELSEIF( INDEX(ADUM(1:),'znc aqueous vol').NE.0 ) THEN
          IPLOT(NV) = 89
        ELSEIF( INDEX(ADUM(1:),'space monitor').NE.0 ) THEN
          IPLOT(NV) = 100
        ELSEIF( INDEX(ADUM(1:),'water mass source rate').NE.0 ) THEN
          IPLOT(NV) = 140
        ELSEIF( INDEX(ADUM(1:),'scanning path').NE.0 ) THEN
          IPLOT(NV) = 149
        ELSEIF( INDEX(ADUM(1:),'aqueous viscosity').NE.0 ) THEN
          IPLOT(NV) = 176
        ELSEIF( INDEX(ADUM(1:),'x aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 201
        ELSEIF( INDEX(ADUM(1:),'y aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 202
        ELSEIF( INDEX(ADUM(1:),'z aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 203
        ELSEIF( INDEX(ADUM(1:),'x intrinsic perm').NE.0 ) THEN
          IPLOT(NV) = 247
        ELSEIF( INDEX(ADUM(1:),'y intrinsic perm').NE.0 ) THEN
          IPLOT(NV) = 248
        ELSEIF( INDEX(ADUM(1:),'z intrinsic perm').NE.0 ) THEN
          IPLOT(NV) = 249
        ELSEIF( INDEX(ADUM(1:),'aqueous relative perm').NE.0 ) THEN
          IPLOT(NV) = 31

! Add total exchanged, solid, gas and NAPL per total volume
        ELSEIF( (INDEX(ADUM(1:),'solid solute volumetric conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'solid species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 26
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 26
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'exchange species volumetric conc').NE.0 &
         .AND. (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 27
          INDX = 400+(NSL-1)*33 + 27
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'gas solute volumetric conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'gas species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 28
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 28
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'napl species volumetric conc').NE.0 &
         .AND. (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 29 
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'solute volumetric conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'species volumetric conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 1
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 1
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF

! Add total exchanged, solid, gas and NAPL per total water volume
        ELSEIF( (INDEX(ADUM(1:),'solid solute aqueous conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'solid species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 30
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 30
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF((INDEX(ADUM(1:),'exchange species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          INDX = 400+(NSL-1)*33 + 31 
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'gas solute aqueous conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'gas species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 32 
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 32
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
        ELSEIF((INDEX(ADUM(1:),'napl species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0) ) THEN
          INDX = 400+(NSL-1)*33 + 33
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( (INDEX(ADUM(1:),'solute aqueous conc').NE.0) .OR. &
        ((INDEX(ADUM(1:),'species aqueous conc').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 2
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 2
            UNPLOT(INDX) = 'mol/m^3'
          ENDIF
!
        ELSEIF( INDEX(ADUM(1:),'solute aqueous mol').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 5
        ELSEIF( INDEX(ADUM(1:),'solute inventory').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 7
        ELSEIF( INDEX(ADUM(1:),'x solute flux').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 8
          INDX = 4
          CHMSG = 'Surface solute flux not supported: '//ADUM
          CALL WRMSGS( INDX )
        ELSEIF( INDEX(ADUM(1:),'y solute flux').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 9
          INDX = 4
          CHMSG = 'Surface solute flux not supported: '//ADUM
          CALL WRMSGS( INDX )
        ELSEIF( INDEX(ADUM(1:),'z solute flux').NE.0 ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 10
          INDX = 4
          CHMSG = 'Surface solute flux not supported: '//ADUM
          CALL WRMSGS( INDX )
        ELSEIF( (INDEX(ADUM(1:),'solute source').NE.0) .OR. &
        ((INDEX(ADUM(1:),'species source').NE.0) .AND. &
        (INDEX( SPNM(1:),'total_' ).NE.0)) ) THEN
          IPLOT(NV) = 400+(NSL-1)*33 + 11
          IF( NSL.GT.NSOLU ) THEN
            INDX = 400+(NSL-1)*33 + 11
            UNPLOT(INDX) = 'mol/s'
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'species volumetric conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 1
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species aqueous conc').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 2
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/m^3'
        ELSEIF( INDEX(ADUM(1:),'species source').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 11
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol'
        ELSEIF( INDEX(ADUM(1:),'mineral area').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 24
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'm^2'
        ELSEIF( INDEX(ADUM(1:),'mineral rate').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 25
          IPLOT(NV) = INDX
          UNPLOT(INDX) = 'mol/s'
        ELSEIF( INDEX(ADUM(1:),'volume fraction').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 26
          IPLOT(NV) = INDX
        ELSEIF( INDEX(ADUM(1:),'ph').NE.0 ) THEN
          INDX = 400+(NSOLU*33)+((NEQC+NEQK)*33)+(NSP-1)*33 + 27  
          IPLOT(NV) = INDX
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Plot File Variable: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
        IDFLT = 1
        VARB = 'Plot File Variable Units: '
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNPLOT(IPLOT(NV)))
        IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
          IF( ME.EQ.0 )WRITE( IWR,'(2X,3A,2X,2A,I2,A)' ) ADUM(1:NCH),', ', &
          UNPLOT(IPLOT(NV))(1:NCU),SOLNM(1:NCS),' Solute(',NSL,')'
        ELSE
          IF( ME.EQ.0 )WRITE( IWR,'(2X,3A)' ) ADUM(1:NCH),', ', &
          UNPLOT(IPLOT(NV))(1:NCU)
        ENDIF
        CALL RDOUUN( IPLOT(NV) )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT( UNPLOT(IPLOT(NV)),VAR,INDX )
  400 CONTINUE
      NVPLOT = NVPLOT + NVC
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDOU1 group  ---
!
      RETURN
      END
