!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE RDOBDA
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
!     Read observed data card.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 21 May 2001.
!     Last Modified by MD White, PNNL, 21 May 2001.
!     $Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE UCODE
  USE TRNSPT
  USE SOLTN
  USE OUTPU
  USE GRID
  USE FILES
  USE BUFFEREDREAD
  use grid_mod

!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#include "utils.h"
#include "gagrid.h"    
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  CHARACTER*64 ADUM,BDUM,CDUM,FDUM,SOLNM,UNTS,FLNM
  CHARACTER*512 CHDUM
  CHARACTER*10 FORM1
  CHARACTER*4 FORM2,FORM4
  CHARACTER*6 FORM3
  LOGICAL FLG_CHK, T_OK
  double precision, dimension(:), allocatable :: xyz_min
  double precision, dimension(:), allocatable :: xyz_max
  INTEGER :: LDXX(3)
  INTEGER :: LO(3),HI(3)
  integer :: k_tmp(2),j_tmp(2),i_tmp(2)
  integer, dimension(6) :: isfc_tmp
!
!----------------------Data Statements---------------------------------!
!
  SAVE FORM1,FORM2,FORM3,FORM4
  DATA FORM1 / '(2X,2A,I2)' /
  DATA FORM2 / '(I2)' /
  DATA FORM3 / '(I6,$)' /
  DATA FORM4 / '(I6)' /
!
!----------------------Executable Lines--------------------------------!
!
  ME = GA_NODEID()
!  ISUB_LOG = ISUB_LOG+1
!  SUB_LOG(ISUB_LOG) = '/RDOBDA'
!  IF( INDEX(CVS_ID(245)(1:1),'$').EQ.0 ) CVS_ID(245) = &
!   '$Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $' 
!
!---  Write card information to output file  ---
!
  CARD = 'Observed-Data Card'
  allocate(i_obdt(9,lobdt))
  i_obdt = 0
  allocate(nro(lobdt))
  nro = 0
  allocate(nobds(lobdt))
  nobds = 0
  allocate(r_obds(2,lobds,lobdt))
  r_obds = 0.d0
  allocate(c_obdt(lobdt))
  c_obdt = 'null'
  allocate(r_obdt(6,lobdt))
  allocate(xyz_b(8,3,lobdt))
  allocate(nc_bs(8,lobdt))
  allocate(nc_b(lobdt))
  nc_b = 0
  r_obdt = 0.d0
  xyz_b(1:8,1:3,:) = -1.d5  !bounds
  nc_bs(1:8,:) = 0
  if(.not. associated(isfc)) then
      CALL ADD_NODE_I2FIELD('isfc',LSF,IDX)
      ISFC => I_ND_2FLD(IDX)%P
      ISFC = 0
  endif
  ICD = INDEX( CARD,'  ' )-1
  if(me == 0) then
    WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
  endif
!
!---  Read number of observed-data types  ---
!
  ISTART = 1
  T_OK = BUFFEREDREAD_GETLINE(CHDUM)
  CALL LCASE( CHDUM )
  VARB = 'Number of Observed-Data Types'
  CALL RDINT(ISTART,ICOMMA,CHDUM,NOBDT)
  IF( NOBDT.GT.LOBDT ) THEN
    INDX = 6
    CHMSG = 'Number of Observed-Data Types > Parameter LOBDT'
    CALL WRMSGS( INDX )
  ENDIF
!
!---  Loop over the number of observed-data types  ---
!
  DO 300 NT = 1,NOBDT
    ISTART = 1
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
    VARB = 'Observed-Data Type'
    CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
!
!---  Field-observation variable  ---
!
    IF( INDEX(ADUM(1:),'field').NE.0 ) THEN
      VARB = 'Field-Observation Variable'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,BDUM )
      IVRX = IVR
      IF( INDEX( BDUM(1:),'solute' ).NE.0 ) THEN
        VARB = 'Field-Observation Variable: Solute Name'
        CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
        DO 10 NSL = 1,NSOLU
          IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 20
     10       CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Field-Observation Variable ' // &
          'Solute Name: ' // SOLNM
        CALL WRMSGS( INDX )
     20       CONTINUE
      ENDIF
      I_OBDT(1,NT) = 1
!
!---      Convert field-observation variable to an index  ---
!
      VARB = 'Field-Observation Variable'
      IVR = IVRX
      IF( INDEX(BDUM(1:),'aqueous pressure').NE.0 ) THEN
        I_OBDT(2,NT) = 1
        if(me == 0) &
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR),': Aqueous Pressure'
      ELSEIF( INDEX(BDUM(1:),'aqueous saturation').NE.0 ) THEN
        I_OBDT(2,NT) = 11
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR),': Aqueous Saturation'
      ELSEIF( INDEX(BDUM(1:),'aqueous moisture cont').NE.0 ) THEN
        I_OBDT(2,NT) = 15
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Aqueous Moisture Content'
      ELSEIF( INDEX(BDUM(1:),'aqueous hydraulic head').NE.0 ) THEN
        I_OBDT(2,NT) = 27
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Aqueous Hydraulic Head'
! zfz Change x aqueous vol to xnc aqueous vol
! zfz Change y aqueous vol to ync aqueous vol
! zfz Change z aqueous vol to znc aqueous vol
!
!      ELSEIF( INDEX(BDUM(1:),'x aqueous vol').NE.0 ) THEN
!        I_OBDT(2,NT) = 51
      ELSEIF( INDEX(BDUM(1:),'x aqueous vol').NE.0 ) THEN
        I_OBDT(2,NT) = 87
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': X Aqueous Volumetric Flux'
!      ELSEIF( INDEX(BDUM(1:),'y aqueous vol').NE.0 ) THEN
!        I_OBDT(2,NT) = 52
      ELSEIF( INDEX(BDUM(1:),'y aqueous vol').NE.0 ) THEN
        I_OBDT(2,NT) = 88
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Y Aqueous Volumetric Flux'
!      ELSEIF( INDEX(BDUM(1:),'z aqueous vol').NE.0 ) THEN
!        I_OBDT(2,NT) = 53
      ELSEIF( INDEX(BDUM(1:),'z aqueous vol').NE.0 ) THEN
        I_OBDT(2,NT) = 89
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Z Aqueous Volumetric Flux'
      ELSEIF( INDEX(BDUM(1:),'matric potential').NE.0 ) THEN
        I_OBDT(2,NT) = 178
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Matric Potential'
      ELSEIF( INDEX(BDUM(1:),'solute volumetric conc').NE.0 ) THEN
! zfz
      I_OBDT(2,NT) = 400 + (NSL-1)*33 + 1
!        I_OBDT(2,NT) = 200 + (NSL-1)*33 + 2
        if(me == 0) then
            WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
              ': Solute Volumetric Concentration'
            WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'solute aqueous conc').NE.0 ) THEN
! zfz
      I_OBDT(2,NT) = 400 + (NSL-1)*33 + 2
!        I_OBDT(2,NT) = 200 + (NSL-1)*33 + 2
        if(me == 0) then
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Solute Aqueous Concentration'
          WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'solute aqueous mol').NE.0 ) THEN
        I_OBDT(2,NT) = 400 + (NSL-1)*33 + 5
        if(me == 0) then
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Solute Aqueous Mole Fracton'
          WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'x solute flux').NE.0 ) THEN
        I_OBDT(2,NT) = 400 + (NSL-1)*33 + 8
        if(me == 0) then
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': X Solute Flux'
          WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'y solute flux').NE.0 ) THEN
        I_OBDT(2,NT) = 400 + (NSL-1)*33 + 9
        if(me == 0) then
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Y Solute Flux'
          WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'z solute flux').NE.0 ) THEN
        I_OBDT(2,NT) = 400 + (NSL-1)*33 + 10
        if(me == 0) then
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Z Solute Flux'
          WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Field-Observation Variable: ' &
          // BDUM
        CALL WRMSGS( INDX )
      ENDIF
!
!---      Read field-observation output units  ---
!
      VARB = 'Field-Observation Output Units'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,C_OBDT(NT) )
      if(me == 0) &
      WRITE(IWR,'(2X,3A)') VARB(1:IVR), &
        ': ',C_OBDT(NT)(1:NCH)
!
!---      Read field-observation x-dir. coordinate and units  ---
!
      allocate(xyz_min(3))
      allocate(xyz_max(3))
! zfz starts: The observations should not be at the boundary cells
!      xyz_min(1) = xp(1)-dxgf(1)/2.d0
!      xyz_max(1) = xp(num_nodes)+dxgf(num_nodes)/2.d0
!      xyz_min(2) = yp(1)-dygf(1)/2.d0
!      xyz_max(2) = yp(num_nodes)+dygf(num_nodes)/2.d0
!      xyz_min(3) = zp(1)-dzgf(1)/2.d0
!      xyz_max(3) = zp(num_nodes)+dzgf(num_nodes)/2.d0
      xyz_min(1) = xp(1)+dxgf(1)/2.d0
      xyz_max(1) = xp(num_nodes)-dxgf(num_nodes)/2.d0
      xyz_min(2) = yp(1)+dygf(1)/2.d0
      xyz_max(2) = yp(num_nodes)-dygf(num_nodes)/2.d0
      xyz_min(3) = zp(1)+dzgf(1)/2.d0
      xyz_max(3) = zp(num_nodes)-dzgf(num_nodes)/2.d0
! zfz ends
      call ga_dgop(1,xyz_min,3,'min') 
      call ga_dgop(1,xyz_max,3,'max') 
      VARB = 'Field-Observation X-Dir. Coordinate'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDT(1,NT))
      VARB = 'Field-Observation X-Dir. Coordinate Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me == 0) &
      WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',R_OBDT(1,NT)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,R_OBDT(1,NT),INDX)
      if(me == 0) &
      WRITE(IWR,'(A,1PE11.4,A)') ' (',R_OBDT(1,NT),', m)'
      IF( R_OBDT(1,NT).LT.xyz_min(1) .OR. &
        R_OBDT(1,NT).GT.xyz_max(1) ) THEN
        INDX = 4
        CHMSG = 'Field-Observation X-Dir. Coordinate ' // &
          'Outside of Computational Domain or at the ' // &
          'Boundary Cell'
        CALL WRMSGS( INDX )
      ENDIF
!
!---      Read field-observation y-dir. coordinate and units  ---
!
      VARB = 'Field-Observation Y-Dir. Coordinate'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDT(2,NT))
      VARB = 'Field-Observation Y-Dir. Coordinate Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me == 0) &
      WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',R_OBDT(2,NT)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,R_OBDT(2,NT),INDX)
      if(me == 0) &
      WRITE(IWR,'(A,1PE11.4,A)') ' (',R_OBDT(2,NT),', m)'
      IF( R_OBDT(2,NT).LT.xyz_min(2) .OR. &
        R_OBDT(2,NT).GT.xyz_max(2) ) THEN
        INDX = 4
        CHMSG = 'Field-Observation Y-Dir. Coordinate ' // &
          'Outside of Computational Domain or at the ' // &
          'Boundary Cell'
        CALL WRMSGS( INDX )
      ENDIF
!
!---      Read field-observation z-dir. coordinate and units  ---
!
      VARB = 'Field-Observation Z-Dir. Coordinate'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDT(3,NT))
      VARB = 'Field-Observation Z-Dir. Coordinate Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me == 0) &
      WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
        ': ',R_OBDT(3,NT)
      INDX = 0
      IUNM = 1
      CALL RDUNIT(UNTS,R_OBDT(3,NT),INDX)
      if(me == 0) &
      WRITE(IWR,'(A,1PE11.4,A)') ' (',R_OBDT(3,NT),', m)'
      IF( R_OBDT(3,NT).LT.xyz_min(3) .OR. &
        R_OBDT(3,NT).GT.xyz_max(3) ) THEN
        INDX = 4
        CHMSG = 'Field-Observation Z-Dir. Coordinate ' // &
          'Outside of Computational Domain or at the ' // &
          'Boundary Cell'
        CALL WRMSGS( INDX )
      ENDIF
      deallocate(xyz_min)
      deallocate(xyz_max)
!
!---      Determine field-observation bounding I,J,K indices  ---
!
      ldxx(1) = iaxmax - iaxmin + 1
      ldxx(2) = iaymax - iaymin + 1
      ldxx(3) = iazmax - iazmin + 1
      lo(1) = iaxmin
      lo(2) = iaymin
      lo(3) = iazmin
      hi(1) = iaxmax
      hi(2) = iaymax
      hi(3) = iazmax
      ILO = lo(1)
      IHI = hi(1)
      JLO = lo(2)
      JHI = hi(2)
      KLO = lo(3)
      KHI = hi(3)
     
      IF( R_OBDT(3,NT).GT.ZP(NUM_NODES-dzgf(num_nodes)/2.d0) .and. &
        r_obdt(3,nt) < zp(num_nodes) + dzgf(num_nodes)/2.d0) THEN
        KLO = iazmax
      ELSEIF( R_OBDT(3,NT).LT.ZP(1)+dzgf(1)/2.d0 .and. &
        r_obdt(3,nt) > zp(1) - dzgf(1)/2.d0) THEN
        KHI = 1
      ENDIF
      dx1 = dygf(num_nodes)/2.d0
      dx2 = dygf(1)/2.d0
      IF( R_OBDT(2,NT).GT.YP(NUM_NODES)-dx1 .and. &
          r_obdt(2,nt) <  yp(num_nodes)+dx1 ) THEN
        JLO = iaymax
      ELSEIF( R_OBDT(2,NT).LT.YP(1) + dx2 .and. & 
          r_obdt(2,nt) > yp(1) - dx2) THEN
        JHI = 1
      ENDIF
      dx1 = dxgf(num_nodes)/2.d0
      dx2 = dxgf(1)/2.d0
      IF( R_OBDT(1,NT).GT.XP(num_nodes) -dx1 .and. &
          r_obdt(1,nt) < xp(num_nodes) + dx1) THEN
        ILO = iaxmax
      ELSEIF( R_OBDT(1,NT).LT.XP(1) + dx2 .and. &
              r_obdt(1,nt) > xp(1) - dx2 ) THEN
        IHI = 1
      ENDIF
      klo_t = -1
      khi_t = -1
      if(klo == khi) then
        klo_t = 1
        khi_t = 1
      endif
     30     CONTINUE
      IF( KHI-KLO.GT.1 ) THEN
        K = (KHI+KLO)/2
!
        n = (k-lo(3))*ldxx(1)*ldxx(2)+(hi(2)-lo(2))*ldxx(1)+ldxx(1)
        IF( ZP(N).GT.R_OBDT(3,NT) ) THEN
          KHI = K
          khi_t = 1
        ELSE
          KLO = K
          klo_t = 1
        ENDIF
        GOTO 30
      ENDIF
      jlo_t = -1
      jhi_t = -1
      if(jlo == jhi) then
        jlo_t = 1
        jhi_t = 1
      endif
     40     CONTINUE
      IF( JHI-JLO.GT.1 ) THEN
        J = (JHI+JLO)/2
        n = (hi(3)-lo(3))*ldxx(1)*ldxx(2)+(j-lo(2))*ldxx(1)+ldxx(1)
        IF( YP(N).GT.R_OBDT(2,NT) ) THEN
          JHI = J
          jhi_t = 1
        ELSE
          JLO = J
          jlo_t = 1
        ENDIF
        GOTO 40
      ENDIF
      ihi_t = -1
      ilo_t = -1
      if(ilo == ihi) then
        ilo_t = 1
        ihi_t = 1
      endif
     50     CONTINUE
      IF( IHI-ILO.GT.1 ) THEN
        I = (IHI+ILO)/2
        n = (hi(3)-lo(3))*ldxx(1)*ldxx(2)+(hi(2)-lo(2))*ldxx(1)+i
        IF( XP(N).GT.R_OBDT(1,NT) ) THEN
          IHI = I
          ihi_t = 1
        ELSE
          ILO = I
          ilo_t = 1
        ENDIF
        GOTO 50
      ENDIF
      nc_b(nt) = 0
      if(klo_t > 0 .and. khi_t > 0 .and. jlo_t > 0 .and. jhi_t > 0 .and. &
          ilo_t > 0 .and. ihi_t > 0) then
      k_tmp(1) = klo
      k_tmp(2) = khi
      j_tmp(1) = jlo
      j_tmp(2) = jhi
      i_tmp(1) = ilo
      i_tmp(2) = ihi
      DO 60 KX = 1,2
      DO 60 JX = 1,2
      DO 60 IX = 1,2
        k = k_tmp(kx)
        j = j_tmp(jx)
        i = i_tmp(ix)
        nc_b(nt) = nc_b(nt) + 1
        n = (k-lo(3))*ldxx(1)*ldxx(2)+(j-lo(2))*ldxx(1)+i
        IF( IXP(N).EQ.0 ) THEN
          INDX = 7
          IMSG = N
          CHMSG = 'Field-Observation ' // &
            'Spacial Interpolation: Inactive Node:'
          CALL WRMSGS( INDX )
        ENDIF
        xyz_b(nc_b(nt),1,nt) = xp(n)
        xyz_b(nc_b(nt),2,nt) = yp(n)
        xyz_b(nc_b(nt),3,nt) = zp(n)
        nc_bs(nc_b(nt),nt) = n
     60     CONTINUE
     endif
!
!---  Reference-observation variable  ---
!
    ELSEIF( INDEX(ADUM(1:),'reference').NE.0 ) THEN
      I_OBDT(1,NT) = 2
      VARB = 'Reference-Observation Variable'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
      IVRX = IVR
! zfz      
      IF( INDEX( BDUM(1:),'solute' ).NE.0 ) THEN
!      IF( INDEX( ADUM(1:),'solute' ).NE.0 ) THEN
        VARB = 'Reference-Observation Solute Name'
        CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
        DO 70 NSL = 1,NSOLU
          IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 72
     70       CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Reference-Observation Solute Name: ' &
          // SOLNM
        CALL WRMSGS( INDX )
     72       CONTINUE
      ENDIF
!
!---      Convert reference-observation variable
!         to an index  ---
!
      VARB = 'Reference-Observation Variable'
      IVR = IVRX
      IF( INDEX(BDUM(1:),'aqueous pressure').NE.0 ) THEN
        I_OBDT(2,NT) = 1
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR),': Aqueous Pressure'
      ELSEIF( INDEX(BDUM(1:),'aqueous saturation').NE.0 ) THEN
        I_OBDT(2,NT) = 11
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR),': Aqueous Saturation'
      ELSEIF( INDEX(BDUM(1:),'aqueous moisture cont').NE.0 ) THEN
        I_OBDT(2,NT) = 15
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Aqueous Moisture Content'
      ELSEIF( INDEX(BDUM(1:),'aqueous hydraulic head').NE.0 ) THEN
        I_OBDT(2,NT) = 27
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Aqueous Hydraulic Head'
      ELSEIF( INDEX(BDUM(1:),'x aqueous vol').NE.0 ) THEN
        I_OBDT(2,NT) = 51
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': X Aqueous Volumetric Flux'
      ELSEIF( INDEX(BDUM(1:),'y aqueous vol').NE.0 ) THEN
        I_OBDT(2,NT) = 52
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Y Aqueous Volumetric Flux'
      ELSEIF( INDEX(BDUM(1:),'z aqueous vol').NE.0 ) THEN
        I_OBDT(2,NT) = 53
        if(me == 0) &
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Z Aqueous Volumetric Flux'
      ELSEIF( INDEX(BDUM(1:),'matric potential').NE.0 ) THEN
        I_OBDT(2,NT) = 178
        if(me == 0) &
         WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Matric Potential'
      ELSEIF( INDEX(BDUM(1:),'solute volumetric conc').NE.0 ) THEN
! zfz
      I_OBDT(2,NT) = 400 + (NSL-1)*33 + 1
!        I_OBDT(2,NT) = 200 + (NSL-1)*33 + 1
        if(me == 0) then
            WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
              ': Solute Volumetric Concentration'
            WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'solute aqueous conc').NE.0 ) THEN
! zfz
      I_OBDT(2,NT) = 400 + (NSL-1)*33 + 2
!        I_OBDT(2,NT) = 200 + (NSL-1)*33 + 2
        if(me == 0) then
            WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
              ': Solute Aqueous Concentration'
            WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'solute aqueous mol').NE.0 ) THEN
        I_OBDT(2,NT) = 200 + (NSL-1)*33 + 5
        if(me == 0) then
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Solute Aqueous Mole Fracton'
        WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'x solute flux').NE.0 ) THEN
        I_OBDT(2,NT) = 400 + (NSL-1)*33 + 8
        if(me == 0) then
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': X Solute Flux'
        WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'y solute flux').NE.0 ) THEN
        I_OBDT(2,NT) = 400 + (NSL-1)*33 + 9
        if(me == 0) then
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Y Solute Flux'
        WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSEIF( INDEX(BDUM(1:),'z solute flux').NE.0 ) THEN
        I_OBDT(2,NT) = 400 + (NSL-1)*33 + 10
        if(me == 0) then
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Z Solute Flux'
        WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Reference-Observation Variable: ' &
          // BDUM
        CALL WRMSGS( INDX )
      ENDIF
!
!---      Read and check reference-observation output units  ---
!
      IDFLT = 1
      VARB = 'Reference-Observation Output Units'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,C_OBDT(NT) )
      if(me == 0) &
      WRITE(IWR,'(2X,3A)') VARB(1:IVR), &
        ': ',C_OBDT(NT)(1:NCH)
      CALL RDOUUN( I_OBDT(2,NT) )
      VAR = 0.D+0
      INDX = 0
      CALL RDUNIT( C_OBDT(NT),VAR,INDX )
!
!---      Read IJK Indices  ---
!
      VARB = 'Reference-Observation IJK Indices'
      CALL RDINT(ISTART,ICOMMA,CHDUM,I_OBDT(4,NT))
      CALL RDINT(ISTART,ICOMMA,CHDUM,I_OBDT(5,NT))
      CALL RDINT(ISTART,ICOMMA,CHDUM,I_OBDT(6,NT))
      irf = i_obdt(4,nt)
      jrf = i_obdt(5,nt)
      krf = i_obdt(6,nt)
      nidx = (irf-1)+(jrf-1)*nxdim+(krf-1)*nxdim*nydim+1
      lidx = 0
      do nx = 1,num_nodes
         if(loc2nat(nx).eq.nidx) then
           lidx = nx
           exit
         endif
      enddo
      nro(nt) = lidx
      IF( I_OBDT(4,NT).LT.1 .OR. I_OBDT(4,NT).GT.nxdim .OR. &
        I_OBDT(5,NT).LT.1 .OR. I_OBDT(5,NT).GT.nydim .OR. &
        I_OBDT(6,NT).LT.1 .OR. I_OBDT(6,NT).GT.nzdim) THEN
        INDX = 4
        CHMSG = 'Out-of-Range Reference-Observation Index'
        CALL WRMSGS( INDX )
      ENDIF
      if(me == 0) then
      WRITE(IWR,'(2X,A,$)') 'Reference-Observation Indices: '
      WRITE(FORM3(3:3),'(I1)') ICOUNT(I_OBDT(4,NT))
      WRITE(IWR,'(2X,A,$)') 'I = '
      WRITE(IWR,FORM3) I_OBDT(4,NT)
      WRITE(FORM3(3:3),'(I1)') ICOUNT(I_OBDT(5,NT))
      WRITE(IWR,'(2X,A,$)') 'J = '
      WRITE(IWR,FORM3) I_OBDT(5,NT)
      WRITE(FORM4(3:3),'(I1)') ICOUNT(I_OBDT(6,NT))
      WRITE(IWR,'(2X,A,$)') 'K = '
      WRITE(IWR,FORM4) I_OBDT(6,NT)
      endif
!
!---  Surface-rate-observation variable  ---
!
    ELSEIF( INDEX(ADUM(1:),'surface').NE.0 .AND. &
      ( INDEX(ADUM(1:),'rate').NE.0 .OR. &
      INDEX(ADUM(1:),'flux').NE.0 ) ) THEN
      I_OBDT(1,NT) = 3
      NSF = NSF + 1
      IF( NSF.GT.LSF ) THEN
        INDX = 5
        CHMSG = 'Number of Surface Flux Domains > Parameter LSF'
        CALL WRMSGS( INDX )
      ENDIF
      VARB = 'Surface-Rate-Observation Variable'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
      IVRX = IVR
      IF( INDEX( BDUM(1:),'solute' ).NE.0 ) THEN
        VARB = 'Surface-Rate-Observation Solute Name'
        CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
        DO 80 NSL = 1,NSOLU
          IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 82
     80       CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Surface-Rate-Observ. Solute Name: ' &
          // SOLNM
        CALL WRMSGS( INDX )
     82       CONTINUE
      ENDIF
!
!---      Convert surface-rate-observation variable
!         to an index  ---
!
      VARB = 'Surface-Rate-Observation Variable'
      IVR = IVRX
      I_OBDT(4,NT) = NSF
      IF( INDEX(BDUM(1:),'aqueous').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'volum').NE.0 ) THEN
          I_OBDT(2,NT) = 2
          C_OBDT(NT) = 'm^3/s'
          IUNM = 3
          IUNS = -1
        if(me == 0) &
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
            ': Aqueous Volumetric Flux'
        ELSE
          I_OBDT(2,NT) = 5
          C_OBDT(NT) = 'kg/s'
          IUNKG = 1
          IUNS = -1
        if(me == 0) &
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
            ': Aqueous Mass Flux'
        ENDIF
      ELSEIF( INDEX(BDUM(1:),'solute').NE.0 ) THEN
!
      I_OBDT(2,NT) = 400 + NSL
!        I_OBDT(2,NT) = 100 + NSL
        C_OBDT(NT) = 'sol/s'
        IUNS = -1
        if(me == 0) then
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Solute Flux'
        WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Surface-Rate-Observation Variable: ' &
          // BDUM
        CALL WRMSGS( INDX )
      ENDIF
      ISFT(NSF) = I_OBDT(2,NT)
!
!---      Read and check units  ---
!
      IDFLT = 1
      VARB = 'Surface-Rate-Observation Output Units'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,C_OBDT(NT) )
      if(me == 0) &
      WRITE(IWR,'(2X,3A)') VARB(1:IVR), &
        ': ',C_OBDT(NT)(1:NCH)
      VAR = 0.D+0
      INDX = 0
      CALL RDUNIT( C_OBDT(NT),VAR,INDX )
!
!---      Read surface-rate-observation orientation  ---
!
      VARB = 'Surface-Rate-Observation Orientation'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      if(me == 0) &
      WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),': '
      IF( INDEX(ADUM(1:),'west').NE.0) THEN
        ISFD(NSF) = -1
        if(me == 0) &
        WRITE(IWR,'(A)') 'X-Direction: West Surface'
      ELSEIF( INDEX(ADUM(1:),'east').NE.0) THEN
        ISFD(NSF) = 1
        if(me == 0) &
        WRITE(IWR,'(A)') 'X-Direction: East Surface'
      ELSEIF( INDEX(ADUM(1:),'south').NE.0) THEN
        ISFD(NSF) = -2
        if(me == 0) &
        WRITE(IWR,'(A)') 'Y-Direction: South Surface'
      ELSEIF( INDEX(ADUM(1:),'north').NE.0) THEN
        ISFD(NSF) = 2
        if(me == 0) &
        WRITE(IWR,'(A)') 'Y-Direction: North Surface'
      ELSEIF( INDEX(ADUM(1:),'bottom').NE.0) THEN
        ISFD(NSF) = -3
        if(me == 0) &
        WRITE(IWR,'(A)') 'Z-Direction: Bottom Surface'
      ELSEIF( INDEX(ADUM(1:),'top').NE.0) THEN
        ISFD(NSF) = 3
        if(me == 0) &
        WRITE(IWR,'(A)') 'Z-Direction: Top Surface'
      ENDIF
!
!---      Read surface-rate-observation domain  ---
!
      VARB = 'Surface-Rate-Observation Domain: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(1))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(2))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(3))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(4))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(5))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(6))
      ISFC_TMP(1) = MAX( 1,ISFC_TMP(1) )
      ISFC_TMP(1) = MIN( IFLD,ISFC_TMP(1),ISFC_TMP(2) )
      ISFC_TMP(2) = MAX( 1,ISFC_TMP(1),ISFC_TMP(2) )
      ISFC_TMP(2) = MIN( IFLD,ISFC_TMP(2) )
      ISFC_TMP(3) = MAX( 1,ISFC_TMP(3) )
      ISFC_TMP(3) = MIN( JFLD,ISFC_TMP(3),ISFC_TMP(4) )
      ISFC_TMP(4) = MAX( 1,ISFC_TMP(3),ISFC_TMP(4) )
      ISFC_TMP(4) = MIN( JFLD,ISFC_TMP(4) )
      ISFC_TMP(5) = MAX( 1,ISFC_TMP(5) )
      ISFC_TMP(5) = MIN( KFLD,ISFC_TMP(5),ISFC_TMP(6) )
      ISFC_TMP(6) = MAX( 1,ISFC_TMP(5),ISFC_TMP(6) )
      ISFC_TMP(6) = MIN( KFLD,ISFC_TMP(6) )
      if(me == 0) then
      WRITE(IWR,'(T3,A)') VARB(1:IVR)
      WRITE(IWR, '(T5,2(A,I6))') 'I = ',ISFC_TMP(1),' to ', &
        ISFC_TMP(2)
      WRITE(IWR, '(T5,2(A,I6))') 'J = ',ISFC_TMP(3),' to ', &
        ISFC_TMP(4)
      WRITE(IWR, '(T5,2(A,I6))') 'K = ',ISFC_TMP(5),' to ', &
        ISFC_TMP(6)
      endif
      CALL GET_LIM(ISFC_TMP, LDI, LDIJ)
      DO K = ISFC_TMP(5), ISFC_TMP(6)
            IZZ = K - IAZMIN
            DO J = ISFC_TMP(3), ISFC_TMP(4)
              IYY = J - IAYMIN
              DO I = ISFC_TMP(1), ISFC_TMP(2)
                IXX = I - IAXMIN
                N = IXX + LDI*IYY + LDIJ*IZZ + 1
                ISFC(NSF,N) = 1
              ENDDO
            ENDDO
      ENDDO

!
!---  Surface-integral-observation variable  ---
!
    ELSEIF( INDEX(ADUM(1:),'surface').NE.0 .AND. &
      INDEX(ADUM(1:),'integral').NE.0 ) THEN
      I_OBDT(1,NT) = 4
      NSF = NSF + 1
      IF( NSF.GT.LSF ) THEN
        INDX = 5
        CHMSG = 'Number of Surface Flux Domains > Parameter LSF'
        CALL WRMSGS( INDX )
      ENDIF
      VARB = 'Surface-Rate-Observation Variable'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
      IVRX = IVR
      IF( INDEX( BDUM(1:),'solute' ).NE.0 ) THEN
        VARB = 'Surface-Rate-Observation Solute Name'
        CALL RDCHR(ISTART,ICOMMA,NCS,CHDUM,SOLNM)
        DO 90 NSL = 1,NSOLU
          IF( SOLNM.EQ.SOLUT(NSL) ) GOTO 92
     90       CONTINUE
        INDX = 4
        CHMSG = 'Unrecognized Surface-Rate-Observ. Solute Name: ' &
          // SOLNM
        CALL WRMSGS( INDX )
     92       CONTINUE
      ENDIF
!
!---      Convert surface-integral-observation variable
!         to an index  ---
!
      I_OBDT(4,NT) = NSF
      VARB = 'Surface-Integral-Observation Variable'
      IVR = IVRX
      IF( INDEX(BDUM(1:),'aqueous').NE.0 ) THEN
        IF( INDEX(BDUM(1:),'volum').NE.0 ) THEN
          I_OBDT(2,NT) = 2
          C_OBDT(NT) = 'm^3'
          IUNM = 3
          if(me == 0) &
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
            ': Aqueous Volumetric Flux Integral'
        ELSE
          I_OBDT(2,NT) = 5
          C_OBDT(NT) = 'kg'
          IUNKG = 1
          if(me == 0) &
          WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
            ': Aqueous Mass Flux Integral'
        ENDIF
      ELSEIF( INDEX(BDUM(1:),'solute').NE.0 ) THEN
! zfz
!      I_OBDT(2,NT) = 400 + NSL
        I_OBDT(2,NT) = 100 + NSL
        C_OBDT(NT) = 'sol'
        if(me == 0) then
        WRITE(IWR,'(/,2X,2A)') VARB(1:IVR), &
          ': Solute Flux Integral'
        WRITE(IWR,'(2X,2A)') 'Solute Name: ',SOLUT(NSL)
        endif
      ELSE
        INDX = 4
        CHMSG = 'Unrecognized Surface-Rate-Observation Variable: ' &
          // BDUM
        CALL WRMSGS( INDX )
      ENDIF
      ISFT(NSF) = I_OBDT(2,NT)
!
!---      Read and check units  ---
!
      IDFLT = 1
      VARB = 'Surface-Integral-Observation Output Units'
      CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,C_OBDT(NT) )
      if(me == 0) &
      WRITE(IWR,'(2X,3A)') VARB(1:IVR), &
        ': ',C_OBDT(NT)(1:NCH)
       VAR = 0.D+0
      INDX = 0
      CALL RDUNIT( C_OBDT(NT),VAR,INDX )
!
!---      Read surface-integral-observation orientation  ---
!
      VARB = 'Surface-Integral-Observation Orientation'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      if(me == 0) &
      WRITE(IWR,'(2X,2A,$)') VARB(1:IVR),': '
      IF( INDEX(ADUM(1:),'west').NE.0) THEN
        ISFD(NSF) = -1
        if(me == 0) &
        WRITE(IWR,'(A)') 'X-Direction: West Surface'
      ELSEIF( INDEX(ADUM(1:),'east').NE.0) THEN
        ISFD(NSF) = 1
        if(me == 0) &
        WRITE(IWR,'(A)') 'X-Direction: East Surface'
      ELSEIF( INDEX(ADUM(1:),'south').NE.0) THEN
        ISFD(NSF) = -2
        if(me == 0) &
        WRITE(IWR,'(A)') 'Y-Direction: South Surface'
      ELSEIF( INDEX(ADUM(1:),'north').NE.0) THEN
        ISFD(NSF) = 2
        if(me == 0) &
        WRITE(IWR,'(A)') 'Y-Direction: North Surface'
      ELSEIF( INDEX(ADUM(1:),'bottom').NE.0) THEN
        ISFD(NSF) = -3
        if(me == 0) &
        WRITE(IWR,'(A)') 'Z-Direction: Bottom Surface'
      ELSEIF( INDEX(ADUM(1:),'top').NE.0) THEN
        ISFD(NSF) = 3
        if(me == 0) &
        WRITE(IWR,'(A)') 'Z-Direction: Top Surface'
      ENDIF
!
!---      Read surface-rate-observation domain  ---
!
      VARB = 'Surface-Rate-Observation Domain: '
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(1))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(2))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(3))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(4))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(5))
      CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(6))
      ISFC_TMP(1) = MAX( 1,ISFC_TMP(1) )
      ISFC_TMP(1) = MIN( IFLD,ISFC_TMP(1),ISFC_TMP(2) )
      ISFC_TMP(2) = MAX( 1,ISFC_TMP(1),ISFC_TMP(2) )
      ISFC_TMP(2) = MIN( IFLD,ISFC_TMP(2) )
      ISFC_TMP(3) = MAX( 1,ISFC_TMP(3) )
      ISFC_TMP(3) = MIN( JFLD,ISFC_TMP(3),ISFC_TMP(4) )
      ISFC_TMP(4) = MAX( 1,ISFC_TMP(3),ISFC_TMP(4) )
      ISFC_TMP(4) = MIN( JFLD,ISFC_TMP(4) )
      ISFC_TMP(5) = MAX( 1,ISFC_TMP(5) )
      ISFC_TMP(5) = MIN( KFLD,ISFC_TMP(5),ISFC_TMP(6) )
      ISFC_TMP(6) = MAX( 1,ISFC_TMP(5),ISFC_TMP(6) )
      ISFC_TMP(6) = MIN( KFLD,ISFC_TMP(6) )
      if(me == 0) then
      WRITE(IWR,'(T3,A)') VARB(1:IVR)
      WRITE(IWR, '(T5,2(A,I6))') 'I = ',ISFC_TMP(1),' to ', &
        ISFC_TMP(2)
      WRITE(IWR, '(T5,2(A,I6))') 'J = ',ISFC_TMP(3),' to ', &
        ISFC_TMP(4)
      WRITE(IWR, '(T5,2(A,I6))') 'K = ',ISFC_TMP(5),' to ', &
        ISFC_TMP(6)
      endif
      CALL GET_LIM(ISFC_TMP, LDI, LDIJ)
      DO K = ISFC_TMP(5), ISFC_TMP(6)
            IZZ = K - IAZMIN
            DO J = ISFC_TMP(3), ISFC_TMP(4)
              IYY = J - IAYMIN
              DO I = ISFC_TMP(1), ISFC_TMP(2)
                IXX = I - IAXMIN
                N = IXX + LDI*IYY + LDIJ*IZZ + 1
                ISFC(NSF,N) = 1
              ENDDO
            ENDDO
      ENDDO
!
!---    Unrecognized Observed-Data Type  ---
!
    ELSE
      INDX = 2
      CHMSG = 'Unrecognized Observed-Data Type: '//ADUM(1:NCH)
      CALL WRMSGS( INDX )
    ENDIF
!
!---    Read observed data statistical index  ---
!
    VARB = 'Observed Data Statistical Index'
    CALL RDINT(ISTART,ICOMMA,CHDUM,I_OBDT(3,NT))
    if(me == 0) then
    WRITE(FORM1(9:9),'(I1)') ICOUNT( I_OBDT(3,NT) )
    WRITE(IWR,FORM1) VARB(1:IVR),': ',I_OBDT(3,NT)
    endif
    IF( I_OBDT(3,NT).LT.1 .OR. I_OBDT(3,NT).GT.1 ) THEN
      INDX = 7
      CHMSG = 'Out of Range Observed Data Statistical Index: '
      IMSG = I_OBDT(3,NT)
      CALL WRMSGS( INDX )
    ENDIF
!
!---    Read observed data statistical parameters  ---
!
    VARB = 'Observed Data Statistic'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDT(4,NT))
    if(me == 0) &
    WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',R_OBDT(4,NT)
    VARB = 'Observed Data Time Weighting Factor'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDT(5,NT))
    if(me == 0) &
    WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',R_OBDT(5,NT)
    VARB = 'Observed Data Space Weighting Factor'
    CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDT(6,NT))
    if(me == 0) &
    WRITE(IWR,'(2X,2A,1PE11.4)') VARB(1:IVR),': ',R_OBDT(6,NT)
!
!---    Read number of observed data samples
!       or an external file name  ---
!
    ISTART = 1
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    CALL LCASE( CHDUM )
!
!---    Read observed-data samples from an external file  ---
!
    IF( INDEX( CHDUM(1:),'file').NE.0 ) THEN
      VARB = 'Observed-Data External File Name'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
      CALL RDCHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
      if(me == 0) &
      WRITE(IWR,'(2X,3A)') VARB(1:IVR),': ',FDUM(1:NCHF)
!
!---      Read external file time units  ---
!
      VARB = 'Observed-Data External File Time Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me == 0) &
      WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),': ',UNTS(1:NCH)
      INDX = 0
      IUNS = 1
      VART = 1.D+0
      CALL RDUNIT(UNTS,VART,INDX)
!
!---      Read external file variable units  ---
!
      VARB = 'Observed-Data External File Variable Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me == 0) &
      WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),': ',UNTS(1:NCH)
      INDX = 0
      CALL OBDAUNT( NT )
      VARV = 1.D+0
      CALL RDUNIT(UNTS,VARV,INDX)
!
!---      Check that external file exists  ---
!
      INQUIRE( FILE=FDUM(1:NCHF), FORM=CDUM, EXIST=FLG_CHK )
      IF( .NOT.FLG_CHK ) THEN
        INDX = 4
        CHMSG = 'Missing Observed-Data External File: ' &
          // FDUM(1:NCHF)
        CALL WRMSGS( INDX )
      ELSEIF( CDUM.EQ.'UNFORMATTED' ) THEN
        INDX = 4
        CHMSG = 'Unformatted Observed-Data External File: ' &
          // FDUM(1:NCHF)
        CALL WRMSGS( INDX )
      ENDIF
      !if(me == 0) OPEN(UNIT=43,FILE=FDUM(1:NCHF),STATUS='OLD',FORM='FORMATTED')
	  OPEN(UNIT=43,FILE=FDUM(1:NCHF),STATUS='OLD',FORM='FORMATTED')
      NS = 0
    100     READ(43,'(A)',END=110) CHDUM
      IF( CHDUM(1:1).EQ.'#' .OR. CHDUM(1:1).EQ.'!' ) GOTO 100
      BACKSPACE(43)
      NS = NS + 1
      IF( NS.GT.LOBDS ) THEN
        INDX = 6
        CHMSG = 'Number of Observed Data Samples > Parameter LOBDS' &
          // 'External File: ' // FDUM(1:NCHF)
        CALL WRMSGS( INDX )
      ENDIF
      READ(43,*,END=110) R_OBDS(2,NS,NT),R_OBDS(1,NS,NT)
      R_OBDS(2,NS,NT) = R_OBDS(2,NS,NT)*VART
      R_OBDS(1,NS,NT) = R_OBDS(1,NS,NT)*VARV
      GOTO 100
    110     CONTINUE
      NOBDS(NT) = NS
      if(me == 0) CLOSE(UNIT=43)
      GOTO 300
    ENDIF
!
!---    Read observed-data samples from input file  ---
!
    VARB = 'Number of Observed Data Samples'
    CALL RDINT(ISTART,ICOMMA,CHDUM,NOBDS(NT))
    IF( NOBDS(NT).GT.LOBDS ) THEN
      INDX = 6
      CHMSG = 'Number of Observed Data Samples > Parameter LOBDS'
      CALL WRMSGS( INDX )
    ENDIF
!
!---    Loop over number of observed data samples  ---
!
    DO 200 NS = 1,NOBDS(NT)
      ISTART = 1
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
!
!---      Read observed data time and units  ---
!
      VARB = 'Observed-Data Time'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDS(2,NS,NT))
      VARB = 'Observed-Data Time Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me == 0) &
      WRITE(IWR,'(2X,4A,1PE11.4,$)') VARB(1:IVR),', ',UNTS(1:NCH), &
      ': ',R_OBDS(2,NS,NT)
      INDX = 0
      IUNS = 1
      CALL RDUNIT(UNTS,R_OBDS(2,NS,NT),INDX)
      if(me == 0) &
      WRITE(IWR,'(A,1PE11.4,A)') ' (',R_OBDS(2,NS,NT),', s)'
      TMOB = MIN( R_OBDS(2,NS,NT),TMOB )
!
!---      Read observed data value and units  ---
!
      VARB = 'Observed Data Value'
      CALL RDDPR(ISTART,ICOMMA,CHDUM,R_OBDS(1,NS,NT))
      VARB = 'Observed Data Value Units'
      CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,UNTS)
      if(me == 0) &
      WRITE(IWR,'(2X,4A,1PE11.4)') VARB(1:IVR),', ',UNTS(1:NCH), &
      ': ',R_OBDS(1,NS,NT)
      INDX = 0
      CALL OBDAUNT( NT )
      CALL RDUNIT(UNTS,R_OBDS(1,NS,NT),INDX)
    200   CONTINUE
    300 CONTINUE
!
!---  Create out_uc.sto file for UCODE ---
!
  FLNM = 'out_uc'
  IX = 7
  JX = IX+ICOUNT(IOM)-1
  WRITE(FORM2(3:3),'(I1)') ICOUNT(IOM)
  WRITE(FLNM(IX:JX),FORM2) IOM
  IX = JX+1
  JX = JX+4
  FLNM(IX:JX) = '.sto'
  if(me == 0) OPEN(UNIT=IOBDSF, FILE=FLNM(1:JX), STATUS='UNKNOWN', &
    FORM='FORMATTED')
  if(me == 0) CLOSE(UNIT=IOBDSF, STATUS='DELETE')
  if(me == 0) OPEN(UNIT=IOBDSF, FILE=FLNM(1:JX), STATUS='NEW', FORM='FORMATTED')
!
!---  Create stompx.uni file for UCODE ---
!
  FLNM(IX:JX) = '.uni'
  INQUIRE( FILE=FLNM(1:JX), EXIST=FLG_CHK )
  IF( .NOT. FLG_CHK ) THEN
    FLG_UNI = .TRUE.
  ELSE
    FLG_UNI = .FALSE.
  ENDIF
  IF( FLG_UNI .and. me == 0) THEN
    OPEN(UNIT=IOBDUF, FILE=FLNM(1:JX), STATUS='UNKNOWN', &
      FORM='FORMATTED')
    CLOSE(UNIT=IOBDUF, STATUS='DELETE')
    OPEN(UNIT=IOBDUF, FILE=FLNM(1:JX), STATUS='NEW', &
      FORM='FORMATTED')
  ENDIF
!
!---  Create stompx.ext file for UCODE ---
!
  FLNM(IX:JX) = '.ext'
  INQUIRE( FILE=FLNM(1:JX), EXIST=FLG_CHK )
  IF( .NOT. FLG_CHK ) THEN
    FLG_EXT = .TRUE.
  ELSE
    FLG_EXT = .FALSE.
  ENDIF
  IF( FLG_EXT .and. me == 0) THEN
    OPEN(UNIT=IOBDEF, FILE=FLNM(1:JX), STATUS='UNKNOWN', &
      FORM='FORMATTED')
    CLOSE(UNIT=IOBDEF, STATUS='DELETE')
    OPEN(UNIT=IOBDEF, FILE=FLNM(1:JX), STATUS='NEW', &
      FORM='FORMATTED')
  ENDIF
!
!---  Reset subroutine character string ---
!
  !ISUB_LOG = ISUB_LOG-1
!
!---  End of RDOBDA group ---
!
  RETURN
  END
  
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE RDUCODE
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
!     Read UCode control card.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 11 October 2001.
!     Last Modified by MD White, PNNL, 11 October 2001.
!     $Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE UCODE
  USE SOLTN
  USE FILES
  USE BUFFEREDREAD
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
  CHARACTER*64 ADUM
  CHARACTER*512 CHDUM
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  CHARACTER*10 FORM1
  LOGICAL T_OK
!
!----------------------Data Statements---------------------------------!
!
  SAVE FORM1
  DATA FORM1 / '(I2,T24,A)' /
!
!----------------------Executable Lines--------------------------------!
!
  !ISUB_LOG = ISUB_LOG+1
  !SUB_LOG(ISUB_LOG) = '/RDUCODE'
  !IF( INDEX(CVS_ID(245)(1:1),'$').EQ.0 ) CVS_ID(245) = &
  ! '$Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $' 
!
!---  Write card information to output file  ---
!
  CARD = 'UCode Control Card'
  ICD = INDEX( CARD,'  ' )-1
  if(me == 0) &
  WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Write header data to stomp.uni file  ---
!
  IF( FLG_UNI .and. me == 0) THEN
    WRITE(IOBDUF,9001) '#STOMP.UNI FILE FOR UCODE'
    WRITE(IOBDUF,9001) '#'
  ENDIF
!
!---  Read input and write output to stomp.uni file  ---
!
  ISTART = 1
  T_OK = BUFFEREDREAD_GETLINE(CHDUM)
  CALL LCASE( CHDUM )
!
!---  Phase  ---
!
  VARB = 'UCode Phase'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  IF( IVAR.EQ.1 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Forward Modeling'
  ELSEIF( IVAR.EQ.11 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Calculates Sum-of-Squares'
  ELSEIF( IVAR.EQ.2 ) THEN
    if(me == 0) & 
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Sensitivities at ' // &
      'Starting Parameters'
  ELSEIF( IVAR.EQ.22 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Sensitivities at ' // &
      'Starting Parameters using Centeral Differences'
  ELSEIF( IVAR.EQ.3 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Peform Regression'
  ELSEIF( IVAR.EQ.33 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Calculate Model ' // &
      'Linearity'
  ELSEIF( IVAR.EQ.44 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Calculate Prediction ' // &
      'Intervals'
  ELSEIF( IVAR.EQ.45 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Calculate Differences ' // &
      'and Prediction Intervals'
  ELSE
    INDX = 7
    CHMSG = 'Unrecognized UCode Phase'
    IMSG = IVAR
    CALL WRMSGS( INDX )
  ENDIF
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI .and. me==0) WRITE(IOBDUF,FORM1) IVAR,'#phase'
!
!---  Differencing  ---
!
  VARB = 'UCode Differencing Index'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  IF( IVAR.EQ.1 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Forward Differencing'
  ELSEIF( IVAR.EQ.2 ) THEN
    if(me == 0) &
    WRITE(IWR,'(/,T2,2A)') VARB(1:IVR),': Central Differencing'
  ELSE
    INDX = 7
    CHMSG = 'Unrecognized UCode Differencing Index'
    IMSG = IVAR
    CALL WRMSGS( INDX )
  ENDIF
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI .and. me == 0 ) WRITE(IOBDUF,FORM1) IVAR, &
    '#differencing (1=forward [recommended], 2=central)'
!
!---  Tolerance  ---
!
  VARB = 'UCode Tolerance'
  CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
  if(me == 0) then
  WRITE(IWR,'(T2,2A,1PE11.4)') VARB(1:IVR),': ',VAR
  IF( FLG_UNI ) WRITE(IOBDUF,9002) VAR, &
    '#tol (0.01 recommended)'
  endif
!
!---  Sum-of-Squared Residual Factor  ---
!
  VARB = 'UCode Sum-of-Squared Residual Factor'
  CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
  if(me == 0) then
  WRITE(IWR,'(T2,2A,1PE11.4)') VARB(1:IVR),': ',VAR
  IF( FLG_UNI ) WRITE(IOBDUF,9002) VAR, &
    '#tolerance sosr (0.01 or 0.1 [recommended])'
  endif
!
!---  Quasi-Newton Updating Index  ---
!
  VARB = 'UCode Quasi-Newton Updating Index'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  IF( IVAR.EQ.0 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': No Quasi-Newton Updating'
  ELSEIF( IVAR.EQ.1 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': Apply Quasi-Newton Updating'
  ELSE
    INDX = 7
    CHMSG = 'Unrecognized UCode Quasi-Newton Updating Index'
    IMSG = IVAR
    CALL WRMSGS( INDX )
  ENDIF
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI .and.me == 0) WRITE(IOBDUF,FORM1) IVAR, &
    '#nopt (0=no quasi-Newton updating, ' // &
    '1=quasi-Newton updating)'
!
!---  Maximum number of iterations  ---
!
  VARB = 'UCode Maximum Number of Iterations'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  if(me == 0) then
  WRITE(IWR,'(T2,2A,I4)') VARB(1:IVR),': ',IVAR
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI ) WRITE(IOBDUF,FORM1) IVAR, &
    '#maximum number of iterations'
  endif
!
!---  Maximum change factor  ---
!
  VARB = 'UCode Maximum Change Factor'
  CALL RDDPR(ISTART,ICOMMA,CHDUM,VAR)
  if(me == 0) then
  WRITE(IWR,'(T2,2A,1PE11.4)') VARB(1:IVR),': ',VAR
  IF( FLG_UNI ) WRITE(IOBDUF,9002) VAR, &
    '#maximum fractional parameter change'
  endif
!
!---  Read new input line  ---
!
  ISTART = 1
  T_OK = BUFFEREDREAD_GETLINE(CHDUM)
!
!---  Path and Name of Inverse Code  ---
!
  VARB = 'UCode Path and Name of Inverse Code'
  CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
  if(me == 0) then
  WRITE(IWR,'(T2,3A)') VARB(1:IVR),': ',ADUM(1:NCH)
  IF( FLG_UNI ) WRITE(IOBDUF,9003) ADUM(1:NCH), &
    '#path and name of inverse code'
  endif
!
!---  Read new input line  ---
!
  ISTART = 1
  T_OK = BUFFEREDREAD_GETLINE(CHDUM)
  CALL LCASE( CHDUM )
!
!---  Number of Application Models  ---
!
  VARB = 'UCode Number of Application Models'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  if(me == 0) then
  WRITE(IWR,'(T2,2A,I4)') VARB(1:IVR),': ',IVAR
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI ) WRITE(IOBDUF,FORM1) IVAR, &
    '#number of application models'
  endif
!
!---  Application Model Execution Commands  ---
!
  DO 100 N = 1,IVAR
    ISTART = 1
    T_OK = BUFFEREDREAD_GETLINE(CHDUM)
    VARB = 'UCode Application Model Execution Commands'
    CALL RDCHR( ISTART,ICOMMA,NCH,CHDUM,ADUM )
    if(me == 0) then
    WRITE(IWR,'(T2,3A)') VARB(1:IVR),': ',ADUM(1:NCH)
    IF( FLG_UNI ) WRITE(IOBDUF,9003) ADUM(1:NCH), &
      '#application model execution commands'
    endif
    100 CONTINUE
!
!---  Read new input line  ---
!
  ISTART = 1
  T_OK = BUFFEREDREAD_GETLINE(CHDUM)
  CALL LCASE( CHDUM )
!
!---  Scale Sensitivities  ---
!
  VARB = 'UCode Scale Sensitivities Index'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  IF( IVAR.EQ.0 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': No scaling is applied, ' // &
      'and unscaled sensitivities are printed.'
  ELSEIF( IVAR.EQ.1 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': Dimensionless scaled ' // &
      'sensitivities are printed.'
  ELSEIF( IVAR.EQ.2 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': One-percent scaled ' // &
      'sensitivities are printed.'
  ELSEIF( IVAR.EQ.3 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': Both dimensionless and ' // &
      'one-percent scaled sensitivities are printed.'
  ELSE
    INDX = 7
    CHMSG = 'Unrecognized UCode Scale Sensitivities Index'
    IMSG = IVAR
    CALL WRMSGS( INDX )
  ENDIF
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI .and. me==0) WRITE(IOBDUF,FORM1) IVAR, &
    '#scale-sensitivities ( 0=no scaling, 1=dimensionless, ' // &
    '2=1%, and 3=both 1 and 2)'
!
!---  Print Intermediate Index  ---
!
  VARB = 'UCode Print Intermediate Index'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  IF( IVAR.EQ.0 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': No printing for ' // &
      'intermediate iterations.'
  ELSEIF( IVAR.EQ.1 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': Printing for ' // &
      'intermediate iterations.'
  ELSE
    INDX = 7
    CHMSG = 'Unrecognized UCode Print Intermediate Index'
    IMSG = IVAR
    CALL WRMSGS( INDX )
  ENDIF
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI .and. me == 0) WRITE(IOBDUF,FORM1) IVAR, &
    '#print intermediate ( 0=no printing, 1=print )'
!
!---  Graph Index  ---
!
  VARB = 'UCode Graph Index'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  IF( IVAR.EQ.0 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': Do not print ' // &
      'post-processing files.'
  ELSEIF( IVAR.EQ.1 ) THEN
    if(me == 0) &
    WRITE(IWR,'(T2,2A)') VARB(1:IVR),': Print post-' // &
      'processing files.'
  ELSE
    INDX = 7
    CHMSG = 'Unrecognized UCode Graph Index'
    IMSG = IVAR
    CALL WRMSGS( INDX )
  ENDIF
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI .and. me == 0) WRITE(IOBDUF,FORM1) IVAR, &
    '#graph ( 0=no printing, 1=print )'
!
!---  Number of Residual Sets  ---
!
  VARB = 'Number of Residual Sets'
  CALL RDINT(ISTART,ICOMMA,CHDUM,IVAR)
  WRITE(FORM1(3:3),'(I1)') ICOUNT(IVAR)
  IF( FLG_UNI .and. me == 0) WRITE(IOBDUF,FORM1) IVAR, &
    '#number-residual-sets'
!
!---  Observation header  ---
!
  IF( FLG_UNI .and. me == 0) THEN
   
    WRITE(IOBDUF,9001) '#'
    WRITE(IOBDUF,9001) '# Observations'
    WRITE(IOBDUF,9001) '# Stat-Flag (0=variance, 1=standard ' // &
      'deviation, 2=coefficient of variation)'
    WRITE(IOBDUF,9004) '# Obs-Name','Obs-Value','Stat.', &
      'Stat-Flag','Plot-Symbol'
    WRITE(IOBDUF,9001) '#'
  ENDIF
!
!---  Format statements ---
!
   9001 FORMAT(A)
   9002 FORMAT(F7.4,T24,A)
   9003 FORMAT(A,T24,A)
   9004 FORMAT(A,T12,A,T24,A,T36,A,T48,A)
!
!---  Reset subroutine character string ---
!
  !ISUB_LOG = ISUB_LOG-1
!
!---  End of RDUCODE group ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE WROBDA
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
!     Write computed observed data.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 23 May 2001.
!     Last Modified by MD White, PNNL, 23 May 2001.
!     $Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE UCODE
  USE TRNSPT
  USE SOLTN
  USE OUTPU
  USE GRID
  USE FDVP
  USE FLUXP
  USE CONST
!
  
!----------------------Implicit Double Precision-----------------------!
!
  IMPLICIT REAL*8 (A-H,O-Z)
  IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#include "utils.h"
#include "gagrid.h"
  
  
  
!
!----------------------Common Blocks-----------------------------------!
!
  
  
  
!
!----------------------Type Declarations-------------------------------!
!
  CHARACTER*64 FLNM
  CHARACTER*16 OBDSNM
  CHARACTER*4 FORM1
  REAL*8 VAR(8),XVAR(8),YVAR(8),ZVAR(8)
  CHARACTER*4 SOLSTRING ! CRY
  INTEGER*4 SOLNUM ! CRY
!
!----------------------Data Statements---------------------------------!
!
  SAVE FORM1
  DATA FORM1 / '(I )' /
!
!----------------------Executable Lines--------------------------------!
!
  !ISUB_LOG = ISUB_LOG+1
  !SUB_LOG(ISUB_LOG) = '/WROBDA'
  !IF( INDEX(CVS_ID(245)(1:1),'$').EQ.0 ) CVS_ID(245) = &
  ! '$Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $' 
!
!---  Write ".sto" file name at begining of ".ext" file  ---
!
  ME = GA_NODEID()
  IF( NOBDP.EQ.0 .AND. FLG_EXT .and. me == 0) THEN
    FLNM = '<out_uc'
    IX = 8
    JX = IX+ICOUNT(IOM)-1
    WRITE(FORM1(3:3),'(I1)') ICOUNT(IOM)
    WRITE(FLNM(IX:JX),FORM1) IOM
    IX = JX+1
    JX = JX+4
    FLNM(IX:JX) = '.sto'
    if(me == 0) WRITE(IOBDEF,'(A)') FLNM(1:JX)
  ENDIF
!
!---  Find observed data samples with matching times ---
!
  DO 1000 NT = 1,NOBDT
  DO 1000 NS = 1,NOBDS(NT)
    IF( ABS(TM-R_OBDS(2,NS,NT))/EPSL.LT.EPSL ) THEN
      NOBDP = NOBDP + 1
      OBDSNM = '                '
!
!---      Field-Observation Output ---
!
      varx = -1.d8
      IF( I_OBDT(1,NT).EQ.1 ) THEN
        XOB = R_OBDT(1,NT)
        YOB = R_OBDT(2,NT)
        ZOB = R_OBDT(3,NT)
        NC = 0
        do nc = 1,nc_b(nt)
          XVAR(NC) = xyz_b(nc,1,nt)
          YVAR(NC) = xyz_b(nc,2,nt)
          ZVAR(NC) = xyz_b(nc,3,nt)
       enddo
        IF( I_OBDT(2,NT).EQ.1 ) THEN
          NC = 0
          do 101 nc = 1,nc_b(nt)
            n = nc_bs(nc,nt)
! Fred: Added atmosphere pressure to PL
            VAR(NC) = PL(2,N)+PATM
!            VAR(NC) = PL(2,N)
    101         CONTINUE
          OBDSNM(1:3) = 'pl_'
        ELSEIF( I_OBDT(2,NT).EQ.11 ) THEN
          NC = 0
          do 111 nc = 1,nc_b(nt)
            n = nc_bs(nc,nt)
            VAR(NC) = SL(2,N)
    111         CONTINUE
          OBDSNM(1:3) = 'sl_'
        ELSEIF( I_OBDT(2,NT).EQ.15 ) THEN
          NC = 0
          do 115 nc = 1,nc_b(nt)
            N = nc_bs(nc,nt)
            VAR(NC) = PORD(2,N)*SL(2,N)
    115         CONTINUE
          OBDSNM(1:3) = 'mcl'
        ELSEIF( I_OBDT(2,NT).EQ.27 ) THEN
          NC = 0
          do 127 nc=1,nc_b(nt)
            N = nc_bs(nc,nt)
            VAR(NC) = PL(2,N)/RHORL/GRAV + ZP(N)
    127         CONTINUE
          OBDSNM(1:3) = 'hhl'
! zfz: added xnc, ync, and znc aq fluxes
		  ELSEIF( I_OBDT(2,NT).EQ.87 ) THEN
          NC = 0
          do 135 nc = 1,nc_b(nt)
            n = nc_bs(nc,nt)
!            VAR(NC) = 0.5D+0*(UL(1,NT)+UL(1,NT+1))
            VAR(NC) = vnc(1,NC)
    135         CONTINUE
          OBDSNM(1:3) = 'xql'
		  ELSEIF( I_OBDT(2,NT).EQ.88 ) THEN
          NC = 0
          do 145 nc = 1,nc_b(nt)
            n = nc_bs(nc,nt)
!            VAR(NC) = 0.5D+0*(VL(1,NT)+WL(1,NT+1))
            VAR(NC) = vnc(2,NC)
    145         CONTINUE
          OBDSNM(1:3) = 'yql'
		  ELSEIF( I_OBDT(2,NT).EQ.89 ) THEN
          NC = 0
          do 155 nc = 1,nc_b(nt)
            n = nc_bs(nc,nt)
            VAR(NC) = vnc(3,NC)
    155         CONTINUE
          OBDSNM(1:3) = 'zql'
        ELSEIF( I_OBDT(2,NT).EQ.178 ) THEN
          NC = 0
          do 278 nc = 1,nc_b(nt)
            n = nc_bs(nc,nt)
            VAR(NC) = (PL(2,N)-PG(2,N))/RHORL/GRAV
    278         CONTINUE
          OBDSNM(1:3) = 'mph'
        ELSEIF( NSOLU.GT.0 ) THEN
          DO 128 NSL = 1, NSOLU
! zfz
          IF( I_OBDT(2,NT).EQ.(400 + (NSL-1)*33 + 1) ) THEN
              NC = 0
              do 279 nc = 1,nc_b(nt)
                N = nc_bs(nc,nt)
                VAR(NC) = C(nsl,N)*YL(nsl,N) 
    279             CONTINUE
              SOLNUM=NSL
              WRITE(SOLSTRING, '(I0)') SOLNUM
              OBDSNM(1:3) = 'cv'//SOLSTRING
          ELSEIF( I_OBDT(2,NT).EQ.(400 + (NSL-1)*33 + 2) ) THEN
              NC = 0
              do 280 nc = 1,nc_b(nt)
                N = nc_bs(nc,nt)
                VAR(NC) = C(nsl,N)*YL(nsl,N)/(SL(2,N)*PORD(2,N)+SMALL) 
    280             CONTINUE
              SOLNUM=NSL
              WRITE(SOLSTRING, '(I0)') SOLNUM
              OBDSNM(1:3) = 'cl'//SOLSTRING
!              print*,"OBDSNM(1:3) =",OBDSNM(1:3)  
          ELSEIF( I_OBDT(2,NT).EQ.(400 + (NSL-1)*33 + 8) ) THEN
              NC = 0
              do 282 nc = 1,nc_b(nt)
                N = nc_bs(nc,nt)
                VAR(NC) = vnc(1,N)*C(nsl,N)*YL(nsl,N)/(SL(2,N)*PORD(2,N)+SMALL) 
    282             CONTINUE
              SOLNUM=NSL
              WRITE(SOLSTRING, '(I0)') SOLNUM
              OBDSNM(1:3) = 'xc'//SOLSTRING
          ELSEIF( I_OBDT(2,NT).EQ.(400 + (NSL-1)*33 + 9) ) THEN
              NC = 0
              do 284 nc = 1,nc_b(nt)
                N = nc_bs(nc,nt)
                VAR(NC) = vnc(2,N)*C(nsl,N)*YL(nsl,N)/(SL(2,N)*PORD(2,N)+SMALL) 
    284             CONTINUE
              SOLNUM=NSL
              WRITE(SOLSTRING, '(I0)') SOLNUM
              OBDSNM(1:3) = 'yc'//SOLSTRING
          ELSEIF( I_OBDT(2,NT).EQ.(400 + (NSL-1)*33 + 10) ) THEN
              NC = 0
              do 286 nc = 1,nc_b(nt)
                N = nc_bs(nc,nt)
                VAR(NC) = vnc(3,N)*C(nsl,N)*YL(nsl,N)/(SL(2,N)*PORD(2,N)+SMALL) 
    286             CONTINUE
              SOLNUM=NSL
              WRITE(SOLSTRING, '(I0)') SOLNUM
              OBDSNM(1:3) = 'zc'//SOLSTRING
          ENDIF
    128         CONTINUE
        ENDIF
!
!---        Interpolate field-observation to observation point ---
!
       if(xyz_b(nc,1,nt) > -1.d5) then
        CALL TRI_LIN( VAR,XVAR,YVAR,ZVAR,XOB,YOB,ZOB,VARX )
       endif
!
!---      Reference-observation variable ---
!
      ELSEIF( I_OBDT(1,NT).EQ.2 ) THEN
        n = nro(nt)
     
        IRNV = I_OBDT(2,NT)
        IF( I_OBDT(2,NT).EQ.1 ) THEN
          OBDSNM(1:3) = 'pl_'
          if(n > 0) VARX = PL(2,N)+PATM
!          if(n > 0) VARX = PL(2,N)
        ELSEIF( I_OBDT(2,NT).EQ.11 ) THEN
          OBDSNM(1:3) = 'sl_'
          if(n > 0) VARX = SL(2,N)
        ELSEIF( I_OBDT(2,NT).EQ.15 ) THEN
          OBDSNM(1:3) = 'mcl'
          if(n > 0) VARX = SL(2,N)*PORD(2,N)
        ELSEIF( I_OBDT(2,NT).EQ.27 ) THEN
          OBDSNM(1:3) = 'hhl'
          if(n > 0) VARX = PL(2,N)/RHORL/GRAV + ZP(N)
		ELSEIF( I_OBDT(2,NT).EQ.87 ) THEN
          OBDSNM(1:3) = 'xnl'
          if(n > 0) VARX = vnc(1,NC)
		ELSEIF( I_OBDT(2,NT).EQ.88 ) THEN
          OBDSNM(1:3) = 'ynl'
          if(n > 0) VARX = vnc(2,NC)
		ELSEIF( I_OBDT(2,NT).EQ.89 ) THEN
          OBDSNM(1:3) = 'znl'
          if(n > 0) VARX = vnc(3,NC)
		ELSEIF( I_OBDT(2,NT).EQ.178 ) THEN
          OBDSNM(1:3) = 'mph'
          if(n > 0) VARX = (PL(2,N)-PG(2,N))/RHORL/GRAV
        ENDIF
!
!---      Surface-rate-observation variable ---
!
      ELSEIF( I_OBDT(1,NT).EQ.3 ) THEN
        IF( I_OBDT(2,NT).EQ.2 ) THEN
          OBDSNM(1:3) = 'vfl'
        ELSEIF( I_OBDT(2,NT).EQ.5 ) THEN
          OBDSNM(1:3) = 'mfl'
        ELSEIF( I_OBDT(2,NT).GT.(100) ) THEN
          OBDSNM(1:3) = 'cf_'
        ENDIF
        VARX = SF(1,I_OBDT(4,NT))
!
!---      Surface-integral-observation variable ---
!
      ELSEIF( I_OBDT(1,NT).EQ.4 ) THEN
        IF( I_OBDT(2,NT).EQ.2 ) THEN
          OBDSNM(1:3) = 'vil'
        ELSEIF( I_OBDT(2,NT).EQ.5 ) THEN
          OBDSNM(1:3) = 'mil'
        ELSEIF( I_OBDT(2,NT).GT.(100) ) THEN
          OBDSNM(1:3) = 'ci_'
        ENDIF
        VARX = SF(2,I_OBDT(4,NT))
      ENDIF
!
!---      Convert to STOMP observation data to output units ---
!
      IF( C_OBDT(NT).NE.'null' ) THEN
        CALL OBDAUNT( NT )
        INDX = 1
        CALL RDUNIT(C_OBDT(NT),VARX,INDX)
      ENDIF
!
!---      Create a unique sample name ---
!
      NCH = INDEX( OBDSNM(1:),'  ')
      IC = ICOUNT(NOBDP)
      WRITE(FORM1(3:3),'(I1)') IC
      IF( IC.EQ.1 ) THEN
        OBDSNM(NCH:NCH+4) = '00000'
        WRITE(OBDSNM(NCH+5:),FORM1) NOBDP
      ELSEIF( IC.EQ.2 ) THEN
        OBDSNM(NCH:NCH+3) = '0000'
        WRITE(OBDSNM(NCH+4:),FORM1) NOBDP
      ELSEIF( IC.EQ.3 ) THEN
        OBDSNM(NCH:NCH+2) = '000'
        WRITE(OBDSNM(NCH+3:),FORM1) NOBDP
      ELSEIF( IC.EQ.4 ) THEN
        OBDSNM(NCH:NCH+1) = '00'
        WRITE(OBDSNM(NCH+2:),FORM1) NOBDP
      ELSEIF( IC.EQ.5 ) THEN
        OBDSNM(NCH:NCH) = '0'
        WRITE(OBDSNM(NCH+1:),FORM1) NOBDP
      ELSEIF( IC.EQ.6 ) THEN
        WRITE(OBDSNM(NCH:),FORM1) NOBDP
      ENDIF
      NCH = INDEX( OBDSNM(1:),'  ')-1
!
!---      Write STOMP observation data to out_ucx.sto file  ---
!
      call ga_dgop(1,varx,1,'max')
      if(me == 0) WRITE(IOBDSF,9001) OBDSNM(1:NCH),VARX
!
!---      Convert to field observation data to output units ---
!
      VARX = R_OBDS(1,NS,NT)
      IF( C_OBDT(NT).NE.'null' ) THEN
        CALL OBDAUNT( NT )
        INDX = 1
        CALL RDUNIT(C_OBDT(NT),VARX,INDX)
      ENDIF
!
!---      Write field observation data to out_ucx.uni file  ---
!
      IF( FLG_UNI .and.me ==0) WRITE(IOBDUF,9002) OBDSNM(1:NCH),VARX, &
        R_OBDT(4,NT),I_OBDT(3,NT),NT
!
!---      Write correlation between field and STOMP observation data
!         to out_ucx.ext file  ---
!
      IF( FLG_EXT .and.me==0) THEN
        IF( NOBDP.EQ.1 ) WRITE(IOBDEF,'(A)') '#'
        WRITE(IOBDEF,'(A,T5,A)') 'o',OBDSNM(1:NCH)
        WRITE(IOBDEF,'(3A)') '/',OBDSNM(1:NCH),'/'
        WRITE(IOBDEF,'(A)') 'C12_23'
        WRITE(IOBDEF,'(A)') '#'
      ENDIF
    ENDIF
   1000 CONTINUE
!
!---  Format statements ---
!
   9001 FORMAT(A,T12,1PE11.4)
   9002 FORMAT(A,T12,1PE11.4,T24,1PE11.4,T36,I2,T48,I4)
!
!---  Reset subroutine character string ---
!
  !ISUB_LOG = ISUB_LOG-1
!
!---  End of WROBDA group ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE TRI_LIN( VAR,XVAR,YVAR,ZVAR,XOB,YOB,ZOB,VARX )
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
!     Tri-linear interpolation on a structured grid.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 24 May 2001.
!     Last Modified by MD White, PNNL, 24 May 2001.
!     $Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE SOLTN
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
!----------------------Type Declarations-------------------------------!
!
  REAL*8 VAR(8),XVAR(8),YVAR(8),ZVAR(8)
!
!----------------------Executable Lines--------------------------------!
!
  !ISUB_LOG = ISUB_LOG+1
  !SUB_LOG(ISUB_LOG) = '/TRI_LIN'
  !IF( INDEX(CVS_ID(245)(1:1),'$').EQ.0 ) CVS_ID(245) = &
  ! '$Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $' 
  UX = (XOB-XVAR(1))/(XVAR(2)-XVAR(1)+EPSL)
  VX = (YOB-YVAR(1))/(YVAR(3)-YVAR(1)+EPSL)
  WX = (ZOB-ZVAR(1))/(ZVAR(5)-ZVAR(1)+EPSL)
  VARX = (1.D+0-UX)*(1.D+0-VX)*(1.D+0-WX)*VAR(1) + &
    UX*(1.D+0-VX)*(1.D+0-WX)*VAR(2) + &
    (1.D+0-UX)*VX*(1.D+0-WX)*VAR(3) + &
    UX*VX*(1.D+0-WX)*VAR(4) + &
    (1.D+0-UX)*(1.D+0-VX)*WX*VAR(5) + &
    UX*(1.D+0-VX)*WX*VAR(6) + &
    (1.D+0-UX)*VX*WX*VAR(7) + &
    UX*VX*WX*VAR(8)
!
!---  Reset subroutine character string ---
!
  !ISUB_LOG = ISUB_LOG-1
!
!---  End of TRI_LIN group ---
!
  RETURN
  END
  
!----------------------Subroutine--------------------------------------!
!
  SUBROUTINE OBDAUNT( NT )
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
!     Observed data units.
!
!----------------------Authors-----------------------------------------!
!
!     Written by Mark White, PNNL, 31 May 2001.
!     Last Modified by MD White, PNNL, 31 May 2001.
!     $Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $
!
  
!----------------------Fortran 90 Modules------------------------------!
!
  USE GLB_PAR
  USE UCODE
  USE SOLTN
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
  !ISUB_LOG = ISUB_LOG+1
  !SUB_LOG(ISUB_LOG) = '/OBDAUNT'
  !IF( INDEX(CVS_ID(245)(1:1),'$').EQ.0 ) CVS_ID(245) = &
  ! '$Id: ucode1.F 956 2015-03-03 15:54:05Z d3c002@PNL.GOV $' 
!
!---  Field-observation units  ---
!
  IF( I_OBDT(1,NT).EQ.1 ) THEN
    CALL RDOUUN( I_OBDT(2,NT) )
!
!---  Reference node  ---
!
  ELSEIF( I_OBDT(1,NT).EQ.2 ) THEN
    CALL RDOUUN( I_OBDT(2,NT) )
!
!---  Surface flux rate  ---
!
  ELSEIF( I_OBDT(1,NT).EQ.3 ) THEN
    CALL RDSFUN( I_OBDT(2,NT) )
!
!---  Surface flux integral  ---
!
  ELSEIF( I_OBDT(1,NT).EQ.4 ) THEN
    INDX = I_OBDT(2,NT)
    CALL RDSFUN( INDX )
  ELSE
    INDX = 7
    CHMSG = 'Unrecognized Observed-Data Type: '
    IMSG = 	I_OBDT(1,NT)
    CALL WRMSGS( INDX )
  ENDIF
!
!---  Reset subroutine character string ---
!
  !ISUB_LOG = ISUB_LOG-1
!
!---  End of OBDAUNT group ---
!
  RETURN
  END
  
