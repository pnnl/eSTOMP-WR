!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDGRID
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
!     Read input file for grid geometry information.
!     Compute geometric length, area, and volume parameters.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, November 1992.
!     Last Modified by MD White, PNNL, December 7, 1995.
!     Generalized coord mods by JA Fort, PNNL, May 17, 1999.
!     $Id: rdgrid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $
!
!----------------------Fortran 90 Modules------------------------------!
!
      USE SOLTN
      USE GRID
      USE GRID_MOD
      USE FILES
      USE CONST
      USE BUFFEREDREAD
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
      REAL*8 XVX(8),YVX(8),ZVX(8)
      CHARACTER*64 ADUM,FDUM,FMDUM,UNTS
      CHARACTER*512 CHDUM
      LOGICAL FCHK,T_OK
! Added by BJP
!      CHARACTER(132) ::  SUBNM
!      CHARACTER(132) ::  SUBNMX
!      CHARACTER(132), DIMENSION(:), ALLOCATABLE ::  CVS_ID
!      CHARACTER(64) ::  CARD
!      CHARACTER(64) ::  VARB
!      CHARACTER(132) ::  CHMSG
!
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      EPSL = 1.D-14
!
      SUBNMX = '/RDGRID'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(143)(1:1),'$').EQ.0 ) CVS_ID(143) = &
       '$Id: rdgrid.F90,v 1.1 2008/07/29 15:20:39 d3g293 Exp $'
      ICSN = ICSN+ICSNX
      IWRGRD = 0
!
!---  Write card information to ouput file  ---
!
      CARD = 'Grid Geometry Card'
      ICD = INDEX( CARD,'  ' )-1
      if(me.eq.0)WRITE (IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read coordinate system type  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Coordinate System'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
! User provided processor topology
      IUSRP = 0
      IF( INDEX(ADUM(1:),'w/proc').NE.0 ) THEN
       IUSRP = 1
      ENDIF
      IF( INDEX(ADUM(1:),'cartesian').NE.0 ) THEN
        IF( INDEX(ADUM(1:),'uniform').NE.0 ) THEN
          ICS = 5
          if(me.eq.0)WRITE(IWR,'(/,A)') 'Uniform Cartesian Coordinate System'
        ELSE
          ICS = 1
          if(me.eq.0)WRITE(IWR,'(/,A)') 'Cartesian Coordinate System'
        ENDIF
  ELSEIF( INDEX(ADUM(1:),'cylindrical').NE.0 ) THEN
    IF( INDEX(ADUM(1:),'uniform').NE.0 ) THEN
      ICS = 6
      if(me.eq.0)WRITE(IWR,'(/,A)') 'Uniform Cylindrical Coordinate System'
    ELSE
      ICS = 2
      if(me.eq.0)WRITE(IWR,'(/,A)') 'Cylindrical Coordinate System'
    ENDIF
  ELSEIF( INDEX(ADUM(1:),'boundary').NE.0 .OR. &
    INDEX(ADUM(1:),'fitted').NE.0  .OR. &
    INDEX(ADUM(1:),'orthogonal').NE.0 ) THEN
    ICS = 3
    if(me.eq.0)WRITE(IWR,'(/,A)') 'Orthogonal Boundary Fitted ' // &
      'Coordinate System'
  ELSEIF( INDEX(ADUM(1:),'earthvision').NE.0 .AND. &
    INDEX(ADUM(1:),'sampled').NE.0 ) THEN
    ICS = 8
    if(me.eq.0)WRITE(IWR,'(/,A)') 'EarthVision Sampled Input'
    if(me.eq.0)WRITE(IWR,'(/,A)') 'Orthogonal Boundary Fitted ' // &
      'Coordinate System'
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Coordinate System Type: ' // ADUM
        CALL WRMSGS( INDX )
      ENDIF
      IF( INDEX(ADUM(1:),'write').NE.0 ) IWRGRD = 1
!
!---  allocate array containing reference points  ---
!
      ALLOCATE(XREF(3))
!
!---  Read reference point for output  ---
!
      IF( INDEX(ADUM(1:),'reference').NE.0 ) THEN
        INDX = 0
        VARB = 'X Reference Point'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XREF(1))
        VARB = 'X Reference Point Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,XREFU(1))
        VARB = 'X Direction from Reference Point'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IXREF(1))
        if(me.eq.0)WRITE(IWR,'(2X,3A,1PE11.4,$)') 'X Reference Point, ',XREFU(1), &
          ': ',XREF(1)
        IUNM = 1
        CALL RDUNIT( XREFU(1),XREF(1),INDX )
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A,$)') ' (',XREF(1),', m)'
        IF( IXREF(1).LT.0 ) THEN
          if(me.eq.0)WRITE(IWR,'(A)') ': Descending direction.'
        ELSE
          if(me.eq.0)WRITE(IWR,'(A)') ': Ascending direction.'
        ENDIF
        VARB = 'Y Reference Point'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XREF(2))
        VARB = 'Y Reference Point Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,XREFU(2))
        VARB = 'Y Direction from Reference Point'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IXREF(2))
        if(me.eq.0)WRITE(IWR,'(2X,3A,1PE11.4,$)') 'Y Reference Point, ',XREFU(2), &
          ': ',XREF(2)
        IUNM = 1
        CALL RDUNIT( XREFU(2),XREF(2),INDX )
        if(me.eq.0)WRITE(IWR,'(A,1PE11.4,A,$)') ' (',XREF(2),', m)'
        IF( IXREF(2).LT.0 ) THEN
!         IF(ME.EQ.0)WRITE(IWR,'(A)') ': Descending direction.'
        ELSE
!         IF(ME.EQ.0)WRITE(IWR,'(A)') ': Ascending direction.'
        ENDIF
        VARB = 'Z Reference Point'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XREF(3))
        VARB = 'Z Reference Point Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,XREFU(3))
        VARB = 'Z Direction from Reference Point'
        CALL RDINT(ISTART,ICOMMA,CHDUM,IXREF(3))
!       IF(ME.EQ.0)WRITE(IWR,'(2X,3A,1PE11.4,$)') 'Z Reference Point, ',XREFU(3), &
!          ': ',XREF(3)
        IUNM = 1
        CALL RDUNIT( XREFU(3),XREF(3),INDX )
        IF( IXREF(3).LT.0 ) THEN
!         IF(ME.EQ.0)WRITE(IWR,'(A)') ': Descending direction.'
        ELSE
!         IF(ME.EQ.0)WRITE(IWR,'(A)') ': Ascending direction.'
        ENDIF
      ENDIF
!
!---  Read coordinate system node dimensions  ---
!
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      VARB = 'Number of I-indexed Nodes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,IFLD)
      VARB = 'Number of J-indexed Nodes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,JFLD)
      VARB = 'Number of K-indexed Nodes'
      CALL RDINT(ISTART,ICOMMA,CHDUM,KFLD)
      upx = 0
      upy = 0
      upz = 0

      IF(IUSRP == 1) THEN
       VARB = 'Number of I-indexed subblock'
       CALL RDINT(ISTART,ICOMMA,CHDUM,upx)
       VARB = 'Number of J-indexed subblock'
       CALL RDINT(ISTART,ICOMMA,CHDUM,upy)
       VARB = 'Number of K-indexed subblock'
       CALL RDINT(ISTART,ICOMMA,CHDUM,upz)

      ENDIF
!
!---  allocate arrays containing grid boundaries  ---
!
     if(ics /=8 ) then
      ALLOCATE(X(IFLD+1))
      ALLOCATE(Y(JFLD+1))
      ALLOCATE(Z(KFLD+1))
     endif
!
!---  Write coordinate information and compute dimensionality  ---
!
      ALLOCATE( MDIM(6) )
      MDIM = 0
      IF( ME.EQ.0 ) THEN
        WRITE (IWR,'(/,A)') 'Coordinate System Dimensions'
        WRITE (IWR,'(2X,A,I6)') 'Number of I-indexed Nodes:   ',IFLD
        WRITE (IWR,'(2X,A,I6)') 'Number of J-indexed Nodes:   ',JFLD
        WRITE (IWR,'(2X,A,I6)') 'Number of K-indexed Nodes:   ',KFLD
      ENDIF
      NDIM = 0
      IF( IFLD.GT.1 ) NDIM = NDIM+1
      IF( JFLD.GT.1 ) NDIM = NDIM+1
      IF( KFLD.GT.1 ) NDIM = NDIM+1
      IF( NDIM.LT.1 ) NDIM = 1
      MDX = 0
      IF( KFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(1) = MDX
      ENDIF
      IF( JFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(2) = MDX
      ENDIF
      IF( IFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(3) = MDX
      ENDIF
      IF( IFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(4) = MDX
      ENDIF
      IF( JFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(5) = MDX
      ENDIF
      IF( KFLD.GT.1 ) THEN
        MDX = MDX+1
        MDIM(6) = MDX
      ENDIF
      IJFLD = IFLD*JFLD
      JKFLD = JFLD*KFLD
      KIFLD = KFLD*IFLD
      NFLD  = IJFLD*KFLD
!
!      WRITE (ISC,'(/,A)') 'Coordinate System Physical Dimensions'
!
!---  First coordinate direction ---
!
!     IF(ME.EQ.0)WRITE(ISC,'(/,A)')'X-Direction Coordinates'
!
!---  Uniform grid spacing ---
!
      IF( ICS.EQ.5) THEN
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        VARB = 'X Node Dimension'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,XSPC)
        VARB = 'X Node Dimension Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        I = 1
        X(1) = 0.D+0
       IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4)')'X(',I,'), ', &
          UNTS(1:NCH),': ',X(I)
        IW = 2
        DO 30 I = 2,IFLD+1
          X(I) = X(I-1)+XSPC
          IF( IW.EQ.5 .OR. I.EQ.IFLD+1 ) THEN
           IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'X(',I,'), ', &
                  UNTS(1:NCH),': ',X(I)
            IW = 1
          ELSE
           IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'X(',I,'), ', &
                UNTS(1:NCH),': ',X(I)
            IW = IW+1
          ENDIF
   30   CONTINUE
        IF( ABS(X(IFLD+1))/EPSL.LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Zero X-Direction Domain'
          CALL WRMSGS( INDX )
        ENDIF
        INDX = 0
        DO 40 I = 1,IFLD+1
          IUNM = 1
          CALL RDUNIT(UNTS,X(I),INDX)
   40   CONTINUE
!
!---  Variable grid spacing  ---
!
      ELSEIF(ICS.EQ.1 .OR. ICS.EQ.2) THEN
        IC = 0
        IW = 0
  100   CONTINUE
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        IR = IFLD+1-IC
        DO 102 I = 1,IR
          ICM = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
          IF( ICM.EQ.ISTART-1 ) GOTO 100
          IAT = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
          IF( IAT.LT.ISTART .OR. IAT.GT.ICM ) THEN
            IC = IC + 1
            IW = IW + 1
            VARB = 'X Dimension'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,X(IC))
            VARB = 'X Dimension Units'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( IW.EQ.5 .OR. IC.EQ.IFLD+1 ) THEN
!             IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4)') 'X(',IC,'), ', &
!                UNTS(1:NCH),': ',X(IC)
              IW = 0
            ELSE
!             IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4,$)') 'X(',IC,'), ', &
!                UNTS(1:NCH),': ',X(IC)
            ENDIF
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,X(IC),INDX)
            IF( IC.EQ.IFLD+1 ) GOTO 103
          ELSE
            CHDUM(IAT:IAT) = ','
            VARB = 'Count Integer'
            CALL RDINT(ISTART,ICOMMA,CHDUM,IAT )
            VARB = 'X Dimension'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DXVAR )
            VARB = 'X Dimension Units'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,DXVAR,INDX)
            DO 101 II = 1,IAT
              IC = IC + 1
              IW = IW + 1
              IF( IC.EQ.1 ) THEN
                X(IC) = 0.D+0
              ELSE
                X(IC) = X(IC-1) + DXVAR
              ENDIF
              XVAR = X(IC)
              INDX = 1
              IUNM = 1
              CALL RDUNIT(UNTS,XVAR,INDX )
              IF( IW.EQ.5 .OR. IC.EQ.IFLD+1 ) THEN
!               IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4)') 'X(',IC,'), ', &
!                  UNTS(1:NCH),': ',XVAR
                IW = 0
              ELSE
!               IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4,$)') 'X(',IC,'), ', &
!                  UNTS(1:NCH),': ',XVAR
              ENDIF
              IF( IC.EQ.IFLD+1 ) GOTO 103
  101       CONTINUE
          ENDIF
  102   CONTINUE
  103   CONTINUE
      ENDIF
!
!---  Second coordinate direction  ---
!
!     IF(ME.EQ.0)WRITE(ISC,'(/,A)')'Y-Direction Coordinates'
!
!---  Uniform grid spacing  ---
!
      IF(ICS.EQ.5) THEN
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        VARB = 'Y Node Dimension'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,YSPC)
        VARB = 'Y Node Dimension Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        J = 1
        Y(1) = 0.D+0
       IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',J,'), ', &
          UNTS(1:NCH),': ',Y(J)
        IW = 2
        DO 120 J = 2,JFLD+1
          Y(J) = Y(J-1)+YSPC
          IF( IW.EQ.5 .OR. J.EQ.JFLD+1 ) THEN
           IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',J,'), ', &
              UNTS(1:NCH),': ',Y(J)
            IW = 1
          ELSE
           IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',J,'), ', &
              UNTS(1:NCH),': ',Y(J)
            IW = IW+1
          ENDIF
  120   CONTINUE
        IF( ABS(Y(JFLD+1))/EPSL.LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Zero Y-Direction Domain'
          CALL WRMSGS( INDX )
        ENDIF
        INDX = 0
        DO 130 J = 1,JFLD+1
          IUNM = 1
          IF( ICS.EQ.6 ) IUNM = 0
          CALL RDUNIT(UNTS,Y(J),INDX)
  130   CONTINUE
!
!---  Variable grid spacing  ---
!
      ELSEIF(ICS.EQ.1 .OR. ICS.EQ.2) THEN
        JC = 0
        JW = 0
  200   CONTINUE
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        JR = JFLD+1-JC
        DO 202 J = 1,JR
          JCM = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
          IF( JCM.EQ.ISTART-1 ) GOTO 200
          JAT = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
          IF( JAT.LT.ISTART .OR. JAT.GT.JCM ) THEN
            JC = JC + 1
            JW = JW + 1
            VARB = 'Y Dimension'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,Y(JC))
            VARB = 'Y Dimension Units'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( JW.EQ.5 .OR. JC.EQ.JFLD+1 ) THEN
!             IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Y(',JC,'), ', &
!                UNTS(1:NCH),': ',Y(JC)
              JW = 0
            ELSE
!             IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',JC,'), ', &
!                UNTS(1:NCH),': ',Y(JC)
            ENDIF
            INDX = 0
            IUNM = 1
            IF( ICS.EQ.2 ) IUNM = 0
            CALL RDUNIT(UNTS,Y(JC),INDX)
            IF( JC.EQ.JFLD+1 ) GOTO 203
          ELSE
            CHDUM(JAT:JAT) = ','
            VARB = 'Count Integer'
            CALL RDINT(ISTART,ICOMMA,CHDUM,JAT )
            VARB = 'Y Dimension'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DYVAR)
            VARB = 'Y Dimension Units'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            INDX = 0
            IUNM = 1
            IF( ICS.EQ.2 ) IUNM = 0
            CALL RDUNIT(UNTS,DYVAR,INDX)
            DO 201 JJ = 1,JAT
              JC = JC + 1
              JW = JW + 1
              IF( JC.EQ.1 ) THEN
                Y(JC) = 0.D+0
              ELSE
                Y(JC) = Y(JC-1) + DYVAR
              ENDIF
              YVAR = Y(JC)
              INDX = 1
              IUNM = 1
              IF( ICS.EQ.2 ) IUNM = 0
              CALL RDUNIT(UNTS,YVAR,INDX )
              IF( JW.EQ.5 .OR. JC.EQ.JFLD+1 ) THEN
!               IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4)') 'Y(',JC,'), ', &
!                  UNTS(1:NCH),': ',YVAR
                JW = 0
              ELSE
!               IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4,$)') 'Y(',JC,'), ', &
!                  UNTS(1:NCH),': ',YVAR
              ENDIF
              IF( JC.EQ.JFLD+1 ) GOTO 203
  201       CONTINUE
          ENDIF
  202   CONTINUE
  203   CONTINUE
!---  For generalized coordinates, read grid file  ---
!   
  ELSEIF( ICS.EQ.3 .OR. ICS.EQ.8 ) THEN
    ISTART = 1
    CALL RDINPL( CHDUM )
    CALL LCASE( CHDUM ) 
    VARB = 'External Grid File Name'
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
    OPEN( UNIT=9,FILE=ADUM(1:NCH),STATUS='OLD',FORM='FORMATTED' )
    REWIND( UNIT=9 )
    if(me.eq.0)WRITE(IWR,'(/,3A)') VARB(1:IVR),': ',ADUM(1:NCH)
    READ(9,*) IFLDX,JFLDX,KFLDX
    allocate(xbf(ifldx,jfldx,kfldx))
    xbf = 0.d0
    allocate(ybf(ifldx,jfldx,kfldx))
    ybf = 0.d0
    allocate(zbf(ifldx,jfldx,kfldx))
    zbf = 0.d0
    IF( IFLDX.NE.IFLD .OR. JFLDX.NE.JFLD .OR. KFLDX.NE.KFLD ) THEN
      INDX = 4
      CHMSG = 'Orthogonal Grid Dimension Conflict'
      CALL WRMSGS( INDX )
    ENDIF
    IF(IFLD.GT.1) THEN
      READ(9,*)(((XBF(I,J,K),I=1,IFLD),J=1,JFLD),K=1,KFLD)
    ELSE
!
!---      Two dimensional YZ domain, unit X dimension  ---
!
!
      DO 810 K = 1,KFLD
      DO 810 J = 1,JFLD
        XBF(1,J,K) = 0.D+0
        XBF(2,J,K) = 1.0+0
    810     CONTINUE
    ENDIF
    IF(JFLD.GT.1) THEN
      READ(9,*)(((YBF(I,J,K),I=1,IFLD),J=1,JFLD),K=1,KFLD)
    ELSE
!
!---      Two dimensional XZ domain, unit Y dimension  ---
!
      DO 820 K = 1,KFLD
      DO 820 I = 1,IFLD
        YBF(I,1,K) = 0.D+0
        YBF(I,2,K) = 1.0+0
    820     CONTINUE
    ENDIF
    IF(KFLD.GT.1) THEN
      READ(9,*)(((ZBF(I,J,K),I=1,IFLD),J=1,JFLD),K=1,KFLD)
    ELSE
!
!---      Two dimensional XY domain, unit Z dimension  ---
!
      DO 830 J = 1,JFLD
      DO 830 I = 1,IFLD
        ZBF(I,J,1) = 0.D+0
        ZBF(I,J,2) = 1.0+0
    830     CONTINUE
    ENDIF
    CLOSE(UNIT=9)
!
!---    Single dimensioned orthogonal grid  ---
!
    IF( IFLD*JFLD.EQ.1 .OR. JFLD*KFLD.EQ.1 .OR.  &
      KFLD*IFLD.EQ.1 ) THEN
      INDX = 4
      CHMSG = 'Single Dimensioned Orthogonal Grid'
      CALL WRMSGS( INDX )
    ENDIF
!
!---    For generalized coordinates, only read dimensional units  ---
!
    VARB = 'Grid file dimensional units'
    CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
    if(me.eq.0)WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ',UNTS(1:NCH)
    INDX = 0
    VARX = 1.D+0
    IUNM = 1
    CALL RDUNIT(UNTS,VARX,INDX)
!
!---    Unit conversion to meters  ---
!
    DO 840 K = 1,KFLD
    DO 840 J = 1,JFLD
    DO 840 I = 1,IFLD
      XBF(I,J,K) = XBF(I,J,K)*VARX
      YBF(I,J,K) = YBF(I,J,K)*VARX
      ZBF(I,J,K) = ZBF(I,J,K)*VARX
    840   CONTINUE
!
!---    Single-dimensioned in the z-direction  ---
!
    IF( KFLD.EQ.1 ) THEN
      DO 842 J = 1,JFLD
      DO 842 I = 1,IFLD
        XBF(I,J,2) = XBF(I,J,1)
        YBF(I,J,2) = YBF(I,J,1)
    842     CONTINUE
    ENDIF
!
!---    Single-dimensioned in the y-direction  ---
!
    IF( JFLD.EQ.1 ) THEN
      DO 844 K = 1,KFLD
      DO 844 I = 1,IFLD
        XBF(I,2,K) = XBF(I,1,K)
        ZBF(I,2,K) = ZBF(I,1,K)
    844     CONTINUE
    ENDIF
!
!---    Single-dimensioned in the x-direction  ---
!
    IF( IFLD.EQ.1 ) THEN
      DO 846 K = 1,KFLD
      DO 846 J = 1,JFLD
        YBF(2,J,K) = YBF(1,J,K)
        ZBF(2,J,K) = ZBF(1,J,K)
    846     CONTINUE
    ENDIF
    IF(IFLD.GT.1) IFLD = IFLD-1
    IF(JFLD.GT.1) JFLD = JFLD-1
    IF(KFLD.GT.1) KFLD = KFLD-1
  ENDIF
!
!---  Write coordinate information and compute dimensionality  ---
!
!      IF( LMNP.LT.MIN(IFLD*JFLD, IFLD*KFLD, JFLD*KFLD) .AND.
!     &  ILES.EQ.1 ) THEN
!        INDX = 5
!        CHMSG = 'Minimum Coplanar Nodes > Parameter LMNP'
!        CALL WRMSGS( INDX )
!      ENDIF
  if(me.eq.0)WRITE (IWR,'(/,A)') 'Coordinate System Dimensions'
  if(me.eq.0)WRITE (IWR,'(2X,A,I6)') 'Number of I-indexed Nodes:   ',IFLD
  if(me.eq.0)WRITE (IWR,'(2X,A,I6)') 'Number of J-indexed Nodes:   ',JFLD
  if(me.eq.0)WRITE (IWR,'(2X,A,I6)') 'Number of K-indexed Nodes:   ',KFLD
  NDIM = 0
  IF( IFLD.GT.1 ) NDIM = NDIM+1
  IF( JFLD.GT.1 ) NDIM = NDIM+1
  IF( KFLD.GT.1 ) NDIM = NDIM+1


  IF( NDIM.LT.1 ) NDIM = 1
  MDX = 0
  IF( KFLD.GT.1 ) THEN
    MDX = MDX+1
    MDIM(1) = MDX
  ENDIF
  IF( JFLD.GT.1 ) THEN
    MDX = MDX+1
    MDIM(2) = MDX
  ENDIF
  IF( IFLD.GT.1 ) THEN
    MDX = MDX+1
    MDIM(3) = MDX
  ENDIF
  IF( IFLD.GT.1 ) THEN
    MDX = MDX+1
    MDIM(4) = MDX
  ENDIF
  IF( JFLD.GT.1 ) THEN
    MDX = MDX+1
    MDIM(5) = MDX
  ENDIF
  IF( KFLD.GT.1 ) THEN
    MDX = MDX+1
    MDIM(6) = MDX
  ENDIF
  IJFLD = IFLD*JFLD
  JKFLD = JFLD*KFLD
  KIFLD = KFLD*IFLD
  NFLD  = IJFLD*KFLD
!
!---  Third coordinate direction  ---
!
!     IF(ME.EQ.0)WRITE(ISC,'(/,A)')'Z-Direction Coordinates'
!
      ISTART = 1
!
!---  Uniform grid spacing  ---
!
      IF(ICS.EQ.5) THEN
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        VARB = 'Z Node Dimension'
        CALL RDDPR(ISTART,ICOMMA,CHDUM,ZSPC)
        VARB = 'Z Node Dimension Units'
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
        K = 1
        Z(1) = 0.D+0
       IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',K,'), ', &
          UNTS(1:NCH),': ',Z(K)
        IW = 2
        DO 220 K = 2,KFLD+1
          Z(K) = Z(K-1)+ZSPC
          IF( IW.EQ.5 .OR. K.EQ.KFLD+1 ) THEN
           IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',K,'), ', &
              UNTS(1:NCH),': ',Z(K)
            IW = 1
          ELSE
           IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',K,'), ', &
              UNTS(1:NCH),': ',Z(K)
            IW = IW+1
          ENDIF
  220   CONTINUE
        IF( ABS(Z(KFLD+1))/EPSL.LT.EPSL ) THEN
          INDX = 4
          CHMSG = 'Zero Z-Direction Domain'
          CALL WRMSGS( INDX )
        ENDIF
        INDX = 0
        DO 230 K = 1,KFLD+1
          IUNM = 1
          CALL RDUNIT(UNTS,Z(K),INDX)
  230   CONTINUE
!
!---  Variable grid spacing  ---
!
      ELSEIF(ICS.EQ.1 .OR. ICS.EQ.2) THEN
        KC = 0
        KW = 0
  300   CONTINUE
        ISTART = 1
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        KR = KFLD+1-KC
        DO 302 K = 1,KR
          KCM = INDEX( CHDUM(ISTART:), ',' ) + ISTART - 1
          IF( KCM.EQ.ISTART-1 ) GOTO 300
          KAT = INDEX( CHDUM(ISTART:), '@' ) + ISTART - 1
          IF( KAT.LT.ISTART .OR. KAT.GT.KCM ) THEN
            KC = KC + 1
            KW = KW + 1
            VARB = 'Z Dimension'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,Z(KC))
            VARB = 'Z Dimension Units'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            IF( KW.EQ.5 .OR. KC.EQ.KFLD+1 ) THEN
!             IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4)') 'Z(',KC,'), ', &
!                UNTS(1:NCH),': ',Z(KC)
              KW = 0
            ELSE
!             IF(ME.EQ.0)WRITE(IWR,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',KC,'), ', &
!                UNTS(1:NCH),': ',Z(KC)
            ENDIF
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,Z(KC),INDX)
            IF( KC.EQ.KFLD+1 ) GOTO 303
          ELSE
            CHDUM(KAT:KAT) = ','
            VARB = 'Count Integer'
            CALL RDINT(ISTART,ICOMMA,CHDUM,KAT )
            VARB = 'Z Dimension'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,DZVAR)
            VARB = 'Z Dimension Units'
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,DZVAR,INDX)
            DO 301 KK = 1,KAT
              KC = KC + 1
              KW = KW + 1
              IF( KC.EQ.1 ) THEN
                Z(KC) = 0.D+0
              ELSE
                Z(KC) = Z(KC-1) + DZVAR
              ENDIF
              ZVAR = Z(KC)
              INDX = 1
              IUNM = 1
              CALL RDUNIT(UNTS,ZVAR,INDX )
              IF( KW.EQ.5 .OR. KC.EQ.KFLD+1 ) THEN
!               IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4)') 'Z(',KC,'), ', &
!                  UNTS(1:NCH),': ',ZVAR
                KW = 0
              ELSE
!               IF(ME.EQ.0)WRITE(ISC,'(2X,A,I4,3A,1PE11.4,$)') 'Z(',KC,'), ', &
!                  UNTS(1:NCH),': ',ZVAR
              ENDIF
              IF( KC.EQ.KFLD+1 ) GOTO 303
  301       CONTINUE
          ENDIF
  302   CONTINUE
  303   CONTINUE
      ENDIF
  400 CONTINUE
      !
      !---  Write grid to an external file --
      !
      IF( IWRGRD.EQ.1 ) THEN
         if (me .eq. 0) then
            OPEN( UNIT=26,FILE='grid',STATUS='UNKNOWN',FORM='FORMATTED' )
            IFLD1 = IFLD+1
            JFLD1 = JFLD+1
            KFLD1 = KFLD+1
            WRITE(26,*) IFLD1,JFLD1,KFLD1
            WRITE(26,*)(X(I),I=1,IFLD1)
            WRITE(26,*)(Y(I),I=1,JFLD1)
            WRITE(26,*)(Z(I),I=1,KFLD1)
            CLOSE(UNIT=26)
         endif
      ENDIF

      ! Taken from Bruce's version to support IO
      ! hdf IO.  There may be a different way to get at this
      ! information.  These vars were in grid_mod but never set or used
      dx = 0.0d00
      dy = 0.0d00
      dz = 0.0d00
      if (IFLD.gt.0) then
        dx = (x(IFLD+1)-x(1))/dble(IFLD)
      endif
      if (JFLD.gt.0) then
        dy = (y(JFLD+1)-y(1))/dble(JFLD)
      endif
      if (KFLD.gt.0) then
        dz = (z(KFLD+1)-z(1))/dble(KFLD)
      endif
      xdim = dx*dble(IFLD)
      ydim = dy*dble(JFLD)
      zdim = dz*dble(KFLD)
!
!---  Initialize GA grid ---
!
      fchk = .false.
      t_b = MPI_Wtime()
      call grid_init
      t_e = MPI_Wtime()
      ga_init_time=t_e-t_b
!
!---  End of RDGRID group ---
!
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
      DEALLOCATE(MDIM)
      RETURN
      END
