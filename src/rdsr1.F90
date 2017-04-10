!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSR1
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
!     Read input file for source information.
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
      USE SOURC
      USE SOLTN
      USE REACT
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
      CHARACTER*64 ADUM,BDUM,UNTS
      CHARACTER*512 CHDUM
      REAL*8 VAR(LSTM,8+LSOLU)
  integer :: g_buf,three,idx
  integer :: zerox
  INTEGER, ALLOCATABLE :: BUF3(:,:,:)
  integer, allocatable :: isrdmw1(:,:)
  integer, allocatable :: isrdmw2(:,:)
  integer :: lo(3),hi(3),ldim(3),dims(3)
  logical :: status, t_ok
!
!----------------------Executable Lines--------------------------------!
!
      me = ga_nodeid()
      use_ga = .true.
      SUBNMX = '/RDSR1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
      '$Id: stomp1.F,v 1.50 2008/02/13 16:22:59 d3c002 Exp $' 
      ICSN = ICSN+ICSNX
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
     zerox = 0
     call ga_fill(g_buf,zerox)
!
!---  Write card information to ouput file  ---
!
      CARD = 'Source Card'
!      allocate(isrdm(13,lsr))
      allocate(isrdmw1(num_nodes,lsr))
      allocate(isrdmw2(num_nodes+1,lsr))
      isrdmw1 = 0
      isrdmw2 = 0
      nx_w1 = 0
      nx_w2 = 0
      nx1 = 0
      nx2 = 0
      allocate(isrt(lsr))
      allocate(isrm(lsr))
      allocate(src(8+lsolu+lspt+lngc,lstm,lsr))
      ICD = INDEX( CARD,'  ' )-1
      if(me.eq.0) write(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
      NSR = 0
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Sources: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,NLIN)
      DO 140 NS = 1, NLIN
        NSL = 0
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---  Read source type  ---
!
        VARB = 'Source Type: '
        if(me.eq.0) write(IWR,'(/,A,$)') VARB(1:IVR)
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( INDEX(ADUM(1:),'aqueous').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'mass').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'density').NE.0 ) THEN
              if(me.eq.0) write(IWR,'(2X,A)') 'Aqueous Mass Density Source'
              ISRTX = 5
            ELSE
              if(me.eq.0) write(IWR,'(2X,A)') 'Aqueous Mass Source'
              ISRTX = 4
            ENDIF
          ELSEIF( INDEX(ADUM(1:),'volumetric').NE.0 ) THEN
            IF( INDEX(ADUM(1:),'density').NE.0 ) THEN
              if(me.eq.0) write(IWR,'(2X,A)') 'Aqueous Volumetric Density Source'
              ISRTX = 3
            ELSE
              if(me.eq.0) write(IWR,'(2X,A)') 'Aqueous Volumetric Source'
              ISRTX = 2
            ENDIF
          ENDIF
        ELSEIF( INDEX(ADUM(1:),'z-dir').NE.0 .AND. &
        INDEX(ADUM(1:),'injec').NE.0 .AND. &
        INDEX(ADUM(1:),'well').NE.0 ) THEN
          if(me.eq.0) write(IWR,'(2X,A)') 'Z-Direction Vertical ' // &
          'Injection Well Source'
            ISRTX = 31
        ELSEIF( INDEX(ADUM(1:),'x-dir').NE.0 .AND. &
        INDEX(ADUM(1:),'injec').NE.0 .AND. &
        INDEX(ADUM(1:),'well').NE.0 ) THEN
          if(me.eq.0) write(IWR,'(2X,A)') 'X-Direction Horizontal ' // &
          'Injection Well Source'
            ISRTX = 32
        ELSEIF( INDEX(ADUM(1:),'y-dir').NE.0 .AND. &
        INDEX(ADUM(1:),'injec').NE.0 .AND. &
        INDEX(ADUM(1:),'well').NE.0 ) THEN
          if(me.eq.0) write(IWR,'(2X,A)') 'Y-Direction Horizontal ' // &
          'Injection Well Source'
            ISRTX = 33
        ELSEIF( IEQC.NE.0 .AND. INDEX(ADUM(1:),'solute').NE.0 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 30 NSL = 1,NSOLU
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              IF( INDEX(ADUM(1:),'iwvs').NE.0 ) THEN
                ISRTX = -(NSL+2*NSOLU)
                if(me.eq.0) write(IWR,'(2X,2A)') &
                'In-Well Vapor-Stripping Solute Source: ',SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'density').NE.0 ) THEN
                ISRTX = -(NSL+NSOLU)
                if(me.eq.0) write(IWR,'(2X,2A)')'Solute Source Density: ',SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'inventory').NE.0 ) THEN
                ISRTX = -(NSL+3*NSOLU)
                if(me.eq.0) write(IWR,'(2X,2A)')'Solute Inventory Source: ', &
                SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'advection').NE.0 ) THEN
                ISRTX = -(NSL+4*NSOLU)
                if(me.eq.0) write(IWR,'(2X,2A)')'Advection-Dominated Solute ' // &
                'Release Model: ',SOLUT(NSL)
              ELSEIF( INDEX(ADUM(1:),'diffusion').NE.0 ) THEN
                IF( INDEX(ADUM(1:),'variable').NE.0 ) THEN
                  ISRTX = -(NSL+8*NSOLU)
                  if(me.eq.0) write(IWR,'(2X,2A)')'Variable Diffusion-Dominated ' // &
                  'Solute Release Model: ',SOLUT(NSL)
                ELSE
                  ISRTX = -(NSL+5*NSOLU)
                  if(me.eq.0) write(IWR,'(2X,2A)')'Diffusion-Dominated Solute ' // &
                  'Release Model: ',SOLUT(NSL)
                ENDIF
              ELSEIF( INDEX(ADUM(1:),'solubility').NE.0 ) THEN
                IF( INDEX(ADUM(1:),'salt').NE.0 .OR. &
                INDEX(ADUM(1:),'cake').NE.0 ) THEN
                  ISRTX = -(NSL+7*NSOLU)
                  if(me.eq.0) write(IWR,'(2X,2A)')'Solubility-Controlled Salt ' // &
                  'Cake Release Model: ',SOLUT(NSL)
                ELSE
                  ISRTX = -(NSL+6*NSOLU)
                  if(me.eq.0) write(IWR,'(2X,2A)')'Solubility-Controlled Solute ' // &
                  'Release Model: ',SOLUT(NSL)
                ENDIF
              ELSE
                ISRTX = -NSL
                if(me.eq.0) write(IWR,'(2X,2A)')'Solute Source: ',SOLUT(NSL)
              ENDIF
              GOTO 40
            ENDIF
   30     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Source Solute Name: '//BDUM
            CALL WRMSGS( INDX )
   40     CONTINUE

        ELSEIF( INDEX(ADUM(1:),'specie').NE.0 ) THEN
          VARB = 'Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
!
!---      Aqueous species  ---
!          
          DO 42 NSP = 1,NSPL
            IDB = INDEX(SPNML(NSP)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SPNML(NSP)(1:IDB) ) THEN
              IF( INDEX(ADUM(1:),'density').NE.0 ) THEN
                ISRTX = 100+NSPL+NSPS+NSP
                if(me.eq.0) write(IWR,'(2X,2A)')'Solute Source Density: ', &
                SPNML(NSP)(1:IDB)
              ELSE
                ISRTX = 100+NSP
                if(me.eq.0) write(IWR,'(2X,2A)')'Species Source: ', &
                SPNML(NSP)(1:IDB)
              ENDIF
              GOTO 44
            ENDIF
   42     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Source Species Name: '//BDUM
            CALL WRMSGS( INDX )
   44     CONTINUE

        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Source Type: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read source domain indices  ---
!
        VARB = 'Source Domain Index'
        ISX = ISTART
        CALL RDINT(ISTART,ICOMMA,CHDUM,IS)
        CALL RDINT(ISTART,ICOMMA,CHDUM,IE)
        CALL RDINT(ISTART,ICOMMA,CHDUM,JS)
        CALL RDINT(ISTART,ICOMMA,CHDUM,JE)
        CALL RDINT(ISTART,ICOMMA,CHDUM,KS)
        CALL RDINT(ISTART,ICOMMA,CHDUM,KE)
        IS = MAX( 1,IS )
        IS = MIN( IS,IE,nxdim )
        IE = MAX( 1,IS,IE )
        IE = MIN( IE,nxdim )
        JS = MAX( 1,JS )
        JS = MIN( JS,JE,nydim )
        JE = MAX( 1,JS,JE )
        JE = MIN( JE,nydim )
        KS = MAX( 1,KS )
        KS = MIN( KS,KE,nzdim )
        KE = MAX( 1,KS,KE )
        KE = MIN( KE,nzdim )

        ICX = ISTART
        if(me.eq.0) write(IWR,'(/,2X,A)') 'Source Domain:'
        if(me.eq.0) write(IWR,'(4X,A,I6,A,I6)') 'I = ',IS,' to ',IE
        if(me.eq.0) write(IWR,'(4X,A,I6,A,I6)') 'J = ',JS,' to ',JE
        if(me.eq.0) write(IWR,'(4X,A,I6,A,I6)') 'K = ',KS,' to ',KE
!
!---  Check for ill-defined source domains  ---
!
        IF( IS.LT.1 .OR. IS.GT.nxdim .OR. IE.LT.1 .OR. &
        IE.GT.nxdim .OR. IE.LT.IS ) THEN
          INDX = 4
          CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
          CALL WRMSGS( INDX )
        ENDIF
        IF( JS.LT.1 .OR. JS.GT.nydim .OR. JE.LT.1 .OR. &
        JE.GT.nydim .OR. JE.LT.JS ) THEN
          INDX = 4
          CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
          CALL WRMSGS( INDX )
        ENDIF
        IF( KS.LT.1 .OR. KS.GT.nzdim .OR. KE.LT.1 .OR. &
        KE.GT.nzdim .OR. KE.LT.KS ) THEN
          INDX = 4
          CHMSG = 'Invalid Source Domain: ' // CHDUM(ISX:ICX)
          CALL WRMSGS( INDX )
        ENDIF
        lo(1) = is
        lo(2) = js
        lo(3) = ks
        hi(1) = ie
        hi(2) = je
        hi(3) = ke
        call nga_fill_patch(g_buf,lo,hi,1)
        lo(1) = iaxmin
        lo(2) = iaymin
        lo(3) = iazmin
        hi(1) = iaxmax
        hi(2) = iaymax
        hi(3) = iazmax
        ldim(1) = iaxmax - iaxmin + 1
        ldim(2) = iaymax - iaymin + 1
        ldim(3) = iazmax - iazmin + 1
        allocate(buf3(ldim(1),ldim(2),ldim(3)))
        buf3 = 0
        call nga_get(g_buf,lo,hi,buf3(1,1,1),ldim)
        n_x = 0
        nx  = 0
        if (ixmin.eq.1) then
          ix_start = 1
        else
          ix_start = gwidth + 1
        endif
        if (iymin.eq.1) then
          iy_start = 1
        else
          iy_start = gwidth + 1
        endif
        if (izmin.eq.1) then
          iz_start = 1
        else
          iz_start = gwidth + 1
        endif
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
        if (ixmax.eq.nxdim) then
          ix_hi = ldim(1)
        else
          ix_hi = ldim(1) - gwidth
        endif
        if (iymax.eq.nydim) then
          iy_hi = ldim(2)
        else
          iy_hi = ldim(2) - gwidth
        endif
        if (izmax.eq.nzdim) then
          iz_hi = ldim(3)
        else
          iz_hi = ldim(3) - gwidth
        endif
        do k = 1,ldim(3)
          do j = 1,ldim(2)
            do i = 1,ldim(1)
              n_x = n_x + 1
              if(k.le.iz_offset.or.k.gt.iz_hi)cycle
              if(j.le.iy_offset.or.j.gt.iy_hi)cycle
              if(i.le.ix_offset.or.i.gt.ix_hi)cycle
              ldi = ixmax-ixmin+1
              ldij = (iymax-iymin+1)*ldi
              if(buf3(i,j,k).gt.0) then
                nx = nx+1
                N = (I-1) + LDI*(J-1) + LDIJ*(K-1) + 1
                n = n_x
                isrdmw1(nx,ns) = n !the nx-th node on source profile ns is n
              endif
            enddo
          enddo
        enddo
        nx_w1 = max( nx_w1, nx )
        deallocate(buf3)
     call ga_fill(g_buf,zerox)
!
!---  Read surface direction and surface domain indices for
!     in-well vapor-stripping type sources  ---
!
        VOLX = 0.D+0
        IF( ISRTX.LT.-2*NSOLU .AND. ISRTX.GE.-3*NSOLU ) THEN
          do n_x = 1,nx
             n = isrdmw1(n_x,ns)
             VOLX = VOLX + VOL(N)
          enddo 
          call ga_dgop(1,volx,1,'+')
          call ga_fill(g_buf,zerox)
!
          VARB = 'In-Well Vapor-Stripping Orientation: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
          if(me.eq.0) write(IWR,'(/,A,$)') VARB(1:IVR)
          IF( INDEX(ADUM(1:),'west').NE.0 ) THEN
            IVSDX = -1
            if(me.eq.0) write(IWR,'(A)') 'X-Direction: West Surface'
          ELSEIF( INDEX(ADUM(1:),'east').NE.0 ) THEN
            IVSDX = 1
            if(me.eq.0) write(IWR,'(A)') 'X-Direction: East Surface'
          ELSEIF( INDEX(ADUM(1:),'south').NE.0 ) THEN
            IVSDX = -2
            if(me.eq.0) write(IWR,'(A)') 'Y-Direction: South Surface'
          ELSEIF( INDEX(ADUM(1:),'north').NE.0 ) THEN
            IVSDX = 2
            if(me.eq.0) write(IWR,'(A)') 'Y-Direction: North Surface'
          ELSEIF( INDEX(ADUM(1:),'bottom').NE.0 ) THEN
            IVSDX = -3
            if(me.eq.0) write(IWR,'(A)') 'Z-Direction: Bottom Surface'
          ELSEIF( INDEX(ADUM(1:),'top').NE.0 ) THEN
            IVSDX = 3
            if(me.eq.0) write(IWR,'(A)') 'Z-Direction: Top Surface'
          ENDIF
          VARB = 'In-Well Vapor-Stripping Domain Index: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,IEVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,JSVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,JEVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,KSVS)
          CALL RDINT(ISTART,ICOMMA,CHDUM,KEVS)
          IF( ABS(IVSDX).EQ.1 .AND. ABS(ISVS-IEVS).GT.0 ) THEN
            INDX = 4
            CHMSG = 'Invalid East/West Surface Flux Domain'
            CALL WRMSGS( INDX )
          ELSEIF( ABS(IVSDX).EQ.2 .AND. ABS(JSVS-JEVS).GT.0 ) THEN
            INDX = 4
            CHMSG = 'Invalid North/South Surface Flux Domain'
            CALL WRMSGS( INDX )
          ELSEIF( ABS(IVSDX).EQ.3 .AND. ABS(KSVS-KEVS).GT.0 ) THEN
            INDX = 4
            CHMSG = 'Invalid Top/Bottom Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          IF( ISVS.LT.1 .OR. ISVS.GT.nxdim .OR. IEVS.LT.1 .OR. &
          IEVS.GT.nxdim .OR. IEVS.LT.ISVS ) THEN
            INDX = 4
            CHMSG = 'Invalid Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          IF( JSVS.LT.1 .OR. JSVS.GT.nydim .OR. JEVS.LT.1 .OR. &
          JEVS.GT.nydim .OR. JEVS.LT.JSVS ) THEN
            INDX = 4
            CHMSG = 'Invalid Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          IF( KSVS.LT.1 .OR. KSVS.GT.nzdim .OR. KEVS.LT.1 .OR. &
          KEVS.GT.nzdim .OR. KEVS.LT.KSVS ) THEN
            INDX = 4
            CHMSG = 'Invalid Surface Flux Domain'
            CALL WRMSGS( INDX )
          ENDIF
          if(me.eq.0) write(IWR,'(/,A)') 'In-Well Vapor-Stripping Domain:'
          if(me.eq.0) write(IWR,'(2X,A,I6,A,I6)') 'I = ',ISVS,' to ',IEVS
          if(me.eq.0) write(IWR,'(2X,A,I6,A,I6)') 'J = ',JSVS,' to ',JEVS
          if(me.eq.0) write(IWR,'(2X,A,I6,A,I6)') 'K = ',KSVS,' to ',KEVS
          ISVS = MAX( 1,ISVS )
          ISVS = MIN( ISVS,IEVS,nxdim )
          IEVS = MAX( 1,ISVS,IEVS )
          IEVS = MIN( IEVS,nxdim )
          JSVS = MAX( 1,JSVS )
          JSVS = MIN( JSVS,JEVS,nydim )
          JEVS = MAX( 1,JSVS,JEVS )
          JEVS = MIN( JEVS,nydim )
          KSVS = MAX( 1,KSVS )
          KSVS = MIN( KSVS,KEVS,nzdim )
          KEVS = MAX( 1,KSVS,KEVS )
          KEVS = MIN( KEVS,nzdim )
          lo(1) = isvs
          lo(2) = jsvs
          lo(3) = ksvs
          hi(1) = ievs
          hi(2) = jevs
          hi(3) = kevs
          call nga_fill_patch(g_buf,lo,hi,1)
!
!          call nga_distribution(g_buf, me, lo, hi)
        lo(1) = iaxmin
        lo(2) = iaymin
        lo(3) = iazmin
        hi(1) = iaxmax
        hi(2) = iaymax
        hi(3) = iazmax
        ldim(1) = iaxmax - iaxmin + 1
        ldim(2) = iaymax - iaymin + 1
        ldim(3) = iazmax - iazmin + 1
          allocate(buf3(ldim(1),ldim(2),ldim(3)))
          buf3 = 0
          call nga_get(g_buf,lo,hi,buf3(1,1,1),ldim)
!          nsvsx = 0
!          nevsx = 0
          n_x2 = 0
          nx2 = 0
          do k = 1, ldim(3)
          do j = 1, ldim(2)
            do i = 1, ldim(1)
              n_x2 = n_x2+1
              if(buf3(i,j,k).gt.0) then
!                n = id_l2g(n_x2)
                n = n_x2
                nx2 = nx2+1
                isrdmw2(nx2,ns) = n
!                if(nsvsx.eq.0) nsvsx = n
!                nevsx = n
              endif
            enddo
          enddo
          enddo
          nx_w2 = max(nx_w2,nx2)
          deallocate(buf3)
     call ga_fill(g_buf,zerox)
        ENDIF
!
!---  Check for sources applied to inactive nodes  ---
!
        do n_x2 = 1,nx2
              n=isrdmw2(n_x2,ns)
              IF( IXP(n).LE.0 ) THEN
                INDX = 4
                CHMSG = 'Source Applied to an Inactive Node'
                CALL WRMSGS( INDX )
              ENDIF
        enddo
!
!---  Read number of source times  ---
!
        VARB = 'Number of Source Times: '
        CALL RDINT(ISTART,ICOMMA,CHDUM,ISRM(NS))
        IF( ISRM(NS).GT.LSTM ) THEN
          INDX = 5
          CHMSG = 'Number of Source Times > Parameter LSTM'
          CALL WRMSGS( INDX )
        ENDIF
        IF( ISRTX.EQ.(-(NSL+3*NSOLU)) .AND. ISRM(NS).GT.1 ) THEN
          INDX = 4
          CHMSG = 'Multiple Times with Solute Inventory Type Source'
          CALL WRMSGS( INDX )
        ENDIF
        SRTMO = -SMALL
        DO 100 NTM = 1,ISRM(NS)
          DO 60 M = 1,6
            VAR(NTM,M) = 0.D+0
   60     CONTINUE
!
!---  Read start time, source values, and units  ---
!
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
          VARB = 'Source Time'
          CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,1))
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
          if(me.eq.0) write(IWR,'(/,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
          ': ',VAR(NTM,1)
          INDX = 0
          IUNS = 1
          CALL RDUNIT(UNTS,VAR(NTM,1),INDX)
          if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,1),', s)'
          IF( ISRTX.EQ.3 ) THEN
            VARB = 'Source Aqueous Volumetric Density Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/s)'
          ELSEIF( ISRTX.EQ.2 ) THEN
            VARB = 'Source Aqueous Volumetric Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 3
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m^3/s)'
          ELSEIF( ISRTX.EQ.4 ) THEN
            VARB = 'Source Aqueous Mass Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNKG = 1
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', kg/s)'
          ELSEIF( ISRTX.EQ.5 ) THEN
            VARB = 'Source Aqueous Mass Density Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNKG = 1
            IUNM = -3
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', kg/m^3 s)'
!
!---      Injection Well Source  ---
!
          ELSEIF( ISRTX.GE.31 .AND. ISRTX.LE.33 ) THEN
            VARB = 'Well Pressure'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ', &
            UNTS(1:NCH),': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            VARB = 'Well Diameter'
            IDFLT = 1
            VAR(NTM,3) = 1.7D-1
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,3)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', m)'
            VARB = 'Symmetry Factor'
            IDFLT = 1
            VAR(NTM,4) = 1.D+0
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            if(me.eq.0) write(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',VAR(NTM,4)
!
!---        Convert minimum well pressure to guage and well diameter
!           to well radius  ---
!
            VAR(NTM,2) = VAR(NTM,2)-PATM
            VAR(NTM,3) = 5.D-1*VAR(NTM,3)
          ELSEIF( ISRTX.LT.0 .AND. ISRTX.GE.-NSOLU ) THEN
            VARB = 'Source Solute Rate: '
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/s)'
          ELSEIF( ISRTX.LT.-NSOLU .AND. ISRTX.GE.-2*NSOLU ) THEN
            VARB = 'Solute Density Rate'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNS = -1
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/m^3 s)'
          ELSEIF( ISRTX.LT.-2*NSOLU .AND. ISRTX.GE.-3*NSOLU ) THEN
            VARB = 'Source Exhaust Gas Temperature'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,6))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,6)
            INDX = 0
            IUNK = 1
            CALL RDUNIT(UNTS,VAR(NTM,6),INDX)
            VARB = 'Source Exhaust Gas Pressure'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,2)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,2),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,2),', Pa)'
            CALL WATSP( VAR(NTM,6),PSWX )
            IF( VAR(NTM,2).LT.PSWX ) THEN
              INDX = 4
              CHMSG = 'Exhaust Gas Pressure < Sat. Water Vapor Pres.'
              CALL WRMSGS( INDX )
            ENDIF
            VARB = 'Source Air/Water Volumetric Flow Ratio'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            if(me.eq.0) write(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Air/Water Partition Coefficient (Henry''s Const.)'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNM = -1
            IUNKG = 1
            IUNS = -2
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', Pa)'
            VAR(NTM,5) = VOLX
            IDFLT = 1
            VAR(NTM,7) = 1.D+0
            VARB = 'Source Vapor Stripping Efficiency'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,7))
            if(me.eq.0) write(IWR,'(2A,1PE11.4)') VARB(1:IVR),': ',VAR(NTM,7)
            IF( VAR(NTM,7).GT.1.D+0 ) &
            VAR(NTM,7)=MAX(VAR(NTM,7)/1.D+2,1.D+0)
          ELSEIF( ISRTX.LT.-3*NSOLU .AND. ISRTX.GE.-4*NSOLU ) THEN
            VARB = 'Domain Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Solute Aqueous Concentration'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/m^3)'
            VAR(NTM,5) = -1.D+0
!
!---  Spread inventory uniformily over domain according to node volume
!     and define a unique source input for each node in the domain  ---
!
            VOLX = 0.D+0
            do n_x=1,nx
              n = isrdmw1(n_x,ns) 
              VOLX = VOLX + VOL(N)
            enddo
            call ga_dgop(1,volx,1,'+')
            do  n_x=1,nx
              n = isrdmw1(n_x,ns)
              NSR = NSR + 1
              IF( NSR.GT.LSR ) THEN
                INDX = 5
                CHMSG = 'Number of Sources > Parameter LSR'
                CALL WRMSGS( INDX )
              ENDIF
!              ISRDM(1,NSR) = n
!              ISRDM(2,NSR) = n
!              ISRDM(3,NSR) = J
!              ISRDM(4,NSR) = J
!              ISRDM(5,NSR) = K
!              ISRDM(6,NSR) = K
              ISRT(NSR) = ISRTX
              ISRM(NSR) = 1
              SRC(1,NTM,NSR) = VAR(NTM,1)
              SRC(3,NTM,NSR) = VAR(NTM,3)*VOL(N)/VOLX
              YN(NSL,N) = SRC(3,NTM,NSR)
              SRC(4,NTM,NSR) = VAR(NTM,4)
              SRC(5,NTM,NSR) = VAR(NTM,5)
            enddo
            GOTO 140
!
!---      Advection-dominated solute release model  ---
!
          ELSEIF( ISRTX.LT.-4*NSOLU .AND. ISRTX.GE.-5*NSOLU ) THEN
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Vertical Depth of Residual Waste'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m)'
            VARB = 'Number of Mixing Cells'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,5)
!
!---      Diffusion-dominated solute release model  ---
!
          ELSEIF( ISRTX.LT.-5*NSOLU .AND. ISRTX.GE.-6*NSOLU ) THEN
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Vertical Depth of Residual Waste'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m)'
            VARB = 'Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,5)
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', m^2/s)'
!
!---      Solubility-controlled solute release model  ---
!
          ELSEIF( ISRTX.LT.-6*NSOLU .AND. ISRTX.GE.-7*NSOLU ) THEN
!
!---        SRX(2): nodal solute inventory
!           SRX(3): aqueous solubility  ---
!
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,2)
            VARB = 'Aqueous Solubility of Solute'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,3)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,3),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,3),', 1/m^3)'
!
!---      Solubility-controlled salt cake release model  ---
!
          ELSEIF( ISRTX.LT.-7*NSOLU .AND. ISRTX.GE.-8*NSOLU ) THEN
!
!---        SRX(2): nodal solute inventory
!           SRX(3): nodal salt cake inventory
!           SRX(4): salt cake solubility  ---
!
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,2))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,2)
            VARB = 'Nodal Salt Cake Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Aqueous Solubility of Salt Cake'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNM = -3
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', 1/m^3)'
!
!---      Diffusion-dominated solute release model 
!         (w/ variable diffusion) ---
!
          ELSEIF( ISRTX.LT.-8*NSOLU .AND. ISRTX.GE.-9*NSOLU ) THEN
            VARB = 'Nodal Solute Inventory'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,3))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,3)
            VARB = 'Vertical Depth of Residual Waste'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,4))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,4)
            INDX = 0
            IUNM = 1
            CALL RDUNIT(UNTS,VAR(NTM,4),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,4),', m)'
            VARB = 'Diffusion Coefficient'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,5))
            CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
            if(me.eq.0) write(IWR,'(4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
            ': ',VAR(NTM,5)
            INDX = 0
            IUNM = 2
            IUNS = -1
            CALL RDUNIT(UNTS,VAR(NTM,5),INDX)
            if(me.eq.0) write(IWR,'(A,1PE11.4,A)') ' (',VAR(NTM,5),', m^2/s)'
            VARB = 'Constrictivity'
            CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR(NTM,6))
            if(me.eq.0) write(IWR,'(2A,1PE11.4,$)') VARB(1:IVR),': ',VAR(NTM,6)
          ENDIF
!
!---  Check for nonascending source times  ---
!
          IF( VAR(NTM,1).LT.SRTMO ) THEN
            INDX = 4
            CHMSG = 'Invalid Source Time Sequencing'
            CALL WRMSGS( INDX )
          ENDIF
          SRTMO = VAR(NTM,1)
  100   CONTINUE
!
!---  Assign values to source variables  ---
!
        NSR = NSR + 1
        IF( NSR.GT.LSR ) THEN
          INDX = 5
          CHMSG = 'Number of Sources > Parameter LSR'
          CALL WRMSGS( INDX )
        ENDIF
        IF( ISRTX.LT.-2*NSOLU .AND. ISRTX.GE.-3*NSOLU ) THEN
          ISRDMW2(num_nodes+1,NSR) = IVSDX
        ENDIF
        ISRT(NSR) = ISRTX
        DO 130 NTM = 1,ISRM(NS)
          DO 120 M = 1,8+NSOLU
            SRC(M,NTM,NSR) = VAR(NTM,M)
  120     CONTINUE
  130   CONTINUE
  140 CONTINUE
      allocate(isrdm(nx_w1,nsr))
      allocate(isrdm_(nx_w2+1,nsr))
      isrdm(1:nx_w1,:) = isrdmw1(1:nx_w1,:)
      isrdm_(1:nx_w2,:) = isrdmw2(1:nx_w2,:)
      isrdm_(nx_w2+1,:) = isrdmw2(num_nodes+1,:)
      deallocate(isrdmw1)
      deallocate(isrdmw2)
!
!---  Check that solute inventory source domains are unique   ---
!
      IF( IEQC.EQ.0 ) GOTO 310
      DO 300 NS = 1,NSR
        IF( ISRT(NS).LT.(-3*NSOLU) .AND. ISRT(NS).GE.(-4*NSOLU) ) THEN
          n = ISRDM(1,NS)
          DO 290 MS = 1,NSR
            IF( MS.EQ.NS .OR. ( ISRT(MS).GT.0 .AND. ISRT(MS).LT.20 ) ) &
            GOTO 290
            IF( ISRT(MS).NE.ISRT(NS) ) GOTO 290
            do nx = 1,nx_w1
             n_ = isrdm(nx,ms)
             IF( n.eq.n_ ) THEN
              INDX = 7
              CHMSG = 'Multiple Solute Sources for a ' // &
              'Node with Solute Inventory Source Type, Node: '
              IMSG = N
             ENDIF
            enddo
  290     CONTINUE
        ENDIF
  300 CONTINUE
  310 CONTINUE
      status = ga_destroy(g_buf)

      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDSR1 group  ---
!
      RETURN
      END
