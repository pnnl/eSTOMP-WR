!----------------------Subroutine--------------------------------------!
!
      SUBROUTINE RDSF1
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
!     Read input file for surface flux information.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, February, 1994.
!     Last Modified by MD White, PNNL, December 8, 1995.
!     Last Modified by CV Freedman, PNNL, 7 January 2003.
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE TRNSPT
      USE SOLTN
      USE OUTPU
      USE GRID
      USE FILES
      USE REACT
      USE GRID_MOD
      USE BUFFEREDREAD
!

!----------------------Implicit Double Precision-----------------------!
!
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!
!----------------------Parameter Statements----------------------------!
!

!----------------------Include Statements------------------------------!
!
#include "mafdecls.fh"
#include "global.fh"
#include "utils.h"
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*64 ADUM,BDUM,FDUM,GDUM
      CHARACTER*512 CHDUM,CHDUMX
      INTEGER, DIMENSION(6) :: ISFC_TMP
      LOGICAL ISBIN,T_OK
!
!----------------------Common Blocks-----------------------------------!
!



!
!----------------------Executable Lines--------------------------------!
!
      ME = GA_NODEID()
      SUBNMX = '/RDSF1'
      ICSNX = INDEX( SUBNMX,'  ' )-1
      SUBNM(ICSN+1:ICSN+ICSNX) = SUBNMX
      IF( INDEX(CVS_ID(204)(1:1),'$').EQ.0 ) CVS_ID(204) = &
       '$Id: rdsf1.F90,v 1.1.1.1 2009/03/30 18:42:52 d3m045 Exp $' 
      ICSN = ICSN+ICSNX
      ISBIN = .FALSE.
!
!---  Write card information to ouput file  ---
!
      CARD = 'Surface Flux Card'
      ICD = INDEX( CARD,'  ' )-1
      IF( ME.EQ.0 )WRITE(IWR,'(//,3A)') ' ~ ',CARD(1:ICD),': '
!
!---  Read surface flux card information  ---
!
      T_OK = BUFFEREDREAD_GETLINE(CHDUM)
      CALL LCASE( CHDUM )
      ISTART = 1
      VARB = 'Number of Surface Flux Inputs'
      CALL RDINT(ISTART,ICOMMA,CHDUM,NSF)
      CALL ADD_NODE_I2FIELD('isfc',NSF,IDX)
      ISFC => I_ND_2FLD(IDX)%P
      ISFC = 0
      IF( NSF.GT.LSF ) THEN
        INDX = 5
        CHMSG = 'Number of Surface Flux Domains > Parameter LSF'
        CALL WRMSGS( INDX )
      ENDIF
      NSFF = 0
      SF = 0.D0
      DO 100 NS = 1, NSF
        IF( NS.NE.1.AND.ME.EQ.0 ) WRITE(IWR, '(/)')
        T_OK = BUFFEREDREAD_GETLINE(CHDUM)
        CHDUMX = CHDUM
        CALL LCASE( CHDUM )
        ISTART = 1
!
!---  Check for specified surface flux filename  ---
!
        CALL CHKINT(ISTART,ICOMMA,CHDUM,INDX)
        IF( INDX .EQ. 1 ) THEN
          VARB = 'Number of Surface Flux Inputs for the Specified File'
          CALL RDINT(ISTART,ICOMMA,CHDUMX,NSFF)
          IF( NSFF.LT.1 ) THEN
            INDX = 4
            CHMSG = 'Number of Surface Flux Inputs < 1'
            CALL WRMSGS( INDX )
          ENDIF
          VARB = 'Surface Output Filename: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUMX,ADUM)
          NSFGP = NSFGP + 1
          ISFGP(NSFGP) = NSFF
          FNSF(NSFGP) = ADUM
          T_OK = BUFFEREDREAD_GETLINE(CHDUM)
          CALL LCASE( CHDUM )
          ISTART = 1
        ENDIF
        IF( NSFF.GT.0 ) THEN
          ISFF(NS) = NSFGP
          NSFF = NSFF-1
        ELSE
          NSFF = 0
          ISFF(NS) = 1
          ISFGP(1) = ISFGP(1) + 1
        ENDIF
!
!---  Read surface flux type  ---
!
        VARB = 'Surface Flux Type: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( ME.EQ.0 )WRITE(IWR,'(/,A,$)') VARB(1:IVR)
        IF( INDEX(ADUM(1:),'aqueous').NE.0) THEN
          IF( INDEX(ADUM(1:),'volum').NE.0) THEN
            ISFT(NS) = 2
            IF( ME.EQ.0 )WRITE(IWR,'(A)') &
              'Aqueous-Phase Volumetric Flux Surface'
            UNSF(1,NS) = 'm^3/s'
            UNSF(2,NS) = 'm^3'
          ELSE
            ISFT(NS) = 5
            IF( ME.EQ. 0 ) &
               WRITE(IWR,'(A)') 'Aqueous-Phase Mass Flux Surface'
            UNSF(1,NS) = 'kg/s'
            UNSF(2,NS) = 'kg'
          ENDIF
! ET-BH
        ELSEIF( INDEX(ADUM(1:),'et').NE.0  .OR. &
                INDEX(ADUM(1:),'evapotrans').NE.0 ) THEN
          IF( ME.EQ.0 )WRITE(IWR,'(A)') &
              'Actual evapotranspiration'
          UNSF(1,NS) = 'm/s'
          UNSF(2,NS) = 'm'
          ISFT(NS) = 52

        ELSEIF( INDEX(ADUM(1:),'solute').NE.0 ) THEN
          VARB = 'Solute Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 10 NSL = 1,NSOLU
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ISFT(NS) = NSL+100
              IF( ME.EQ.0 ) &
                WRITE(IWR,'(2X,2A)') SOLUT(NSL),' Flux Surface'
              UNSF(1,NS) = 'sol/s'
              UNSF(2,NS) = 'sol'
              GOTO 12
            ENDIF
   10     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Surface Flux Solute Name: '//BDUM
            CALL WRMSGS( INDX )
   12     CONTINUE
!
!---    Conservation-component species surface flux input  ---
!
        ELSEIF( INDEX(ADUM(1:),'conservation').NE.0 .AND. &
          INDEX(ADUM(1:),'component').NE.0 ) THEN
          VARB = 'Conservation-Component Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 14 NSL = NSOLU+1,NSOLU+NEQC
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ISFT(NS) = NSL+100
              IF ( ME.EQ.0 ) &
                WRITE(IWR,'(2X,2A)') SOLUT(NSL),' Flux Surface'
              UNSF(1,NS) = 'mol/s'
              UNSF(2,NS) = 'mol'
              GOTO 16
            ENDIF
   14     CONTINUE
            INDX = 4
            CHMSG = 'Unrecognized Conservation-Component ' // &
             'Species Name: '//BDUM
            CALL WRMSGS( INDX )
   16     CONTINUE
!
!---    Kinetic-component species surface flux input  ---
!
        ELSEIF( INDEX(ADUM(1:),'kinetic').NE.0 .AND. &
          INDEX(ADUM(1:),'component').NE.0 ) THEN
          VARB = 'Kinetic-Component Species Name: '
          CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,BDUM)
          DO 18 NSL = NSOLU+NEQC+1,NSOLU+NEQC+NEQK
            IDB = INDEX(SOLUT(NSL)(1:),'  ')
            IF( BDUM(1:IDB).EQ.SOLUT(NSL)(1:IDB) ) THEN
              ISFT(NS) = NSL+100
              if ( me.eq.0 ) &
                WRITE(IWR,'(2X,2A)') SOLUT(NSL),' Flux Surface'
              UNSF(1,NS) = 'mol/s'
              UNSF(2,NS) = 'mol'
              GOTO 20
            ENDIF
   18     CONTINUE
          INDX = 4
            CHMSG = 'Unrecognized Kinetic-Component ' // &
             'Species Name: '//BDUM
            CALL WRMSGS( INDX )
   20     CONTINUE
        ELSE
          INDX = 4
          CHMSG = 'Unrecognized Surface Flux Type: '//ADUM
          CALL WRMSGS( INDX )
        ENDIF
!
!---  Read surface flux variable units  ---
!
        IDFLT = 1
        VARB = 'Surface Flux Rate Variable Unit'
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNSF(1,NS))
        CALL RDSFUN( ISFT(NS) )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT(UNSF(1,NS),VAR,INDX)
        IDFLT = 1
        VARB = 'Surface Flux Integral Variable Unit'
        CALL RDCHR(ISTART,ICOMMA,NCU,CHDUM,UNSF(2,NS))
        INDX = -ISFT(NS)
        CALL RDSFUN( INDX )
        VAR = 0.D+0
        INDX = 0
        CALL RDUNIT(UNSF(2,NS),VAR,INDX)
!
!---  Read surface flux orientation  ---
!
        VARB = 'Surface Flux Orientation: '
        CALL RDCHR(ISTART,ICOMMA,NCH,CHDUM,ADUM)
        IF( ME.EQ.0 )WRITE(IWR,'(A,$)') VARB(1:IVR)
        ISFSN(NS) = 0
        IF( INDEX(ADUM(1:),'surface normal').NE.0 )  ISFSN(NS) = 1
        IF( INDEX(ADUM(1:),'west').NE.0) THEN
          ISFD(NS) = -1
          IF( ME.EQ.0 ) &
            WRITE(IWR,'(A)') 'X-Direction: West Surface'
        ELSEIF( INDEX(ADUM(1:),'east').NE.0) THEN
          ISFD(NS) = 1
          IF( ME.EQ.0 ) &
            WRITE(IWR,'(A)') 'X-Direction: East Surface'
        ELSEIF( INDEX(ADUM(1:),'south').NE.0) THEN
          ISFD(NS) = -2
          IF( ME.EQ.0 ) &
            WRITE(IWR,'(A)') 'Y-Direction: South Surface'
        ELSEIF( INDEX(ADUM(1:),'north').NE.0) THEN
          ISFD(NS) = 2
          IF( ME.EQ.0 ) &
            WRITE(IWR,'(A)') 'Y-Direction: North Surface'
        ELSEIF( INDEX(ADUM(1:),'bottom').NE.0) THEN
          ISFD(NS) = -3
          IF( ME.EQ.0 ) &
            WRITE(IWR,'(A)') 'Z-Direction: Bottom Surface'
        ELSEIF( INDEX(ADUM(1:),'top').NE.0) THEN
          ISFD(NS) = 3
          IF( ME.EQ.0 ) &
            WRITE(IWR,'(A)') 'Z-Direction: Top Surface'
        ELSEIF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          IF( INDEX(ADUM(1:),'binary').NE.0 .OR. &
             INDEX(ADUM(1:),'bfile').NE.0 .OR. &
             INDEX(ADUM(1:),'b_file').NE.0 ) ISBIN = .TRUE.
          CALL RDCHR(ISTART,ICOMMA,NCHF,CHDUM,FDUM)
          NCHF = INDEX(FDUM,'  ')-1
          T_OK = OPENFILE( FDUM,IUNIT,BIN )
          NC = 0
          ISFC_TMP(1) = IAXMIN
          ISFC_TMP(2) = IAXMAX
          ISFC_TMP(3) = IAYMIN
          ISFC_TMP(4) = IAYMAX
          ISFC_TMP(5) = IAZMIN
          ISFC_TMP(6) = IAZMAX
          CALL GET_LIM(ISFC_TMP, LDI, LDIJ)
   30     CONTINUE
          READ(IUNIT,*,END=40) IX,JX,KX,ISFDX
          IXX = IX - IAXMIN
          IYY = JX - IAYMIN
          IZZ = KX - IAZMIN
          IF( KX.GE.ISFC_TMP(5).AND.KX.LE.ISFC_TMP(6).AND. &
            JX.GE.ISFC_TMP(3).AND.JX.LE.ISFC_TMP(4).AND. &
            IX.GE.ISFC_TMP(1).AND.IX.LE.ISFC_TMP(2) ) THEN
            N = IXX + LDI*IYY + LDIJ*IZZ + 1
            ISFD(NS) = ISFDX
            ISFC(NS,N) = 1
          ENDIF
          NC = NC + 1
          IF( NC.GT.LSFDOM ) THEN
            INDX = 5
            CHMSG = 'Number of Surface-Flux-Domain Surfaces ' // &
            '> Parameter LSFDOM'
            CALL WRMSGS( INDX )
          ENDIF
          ISFDOM(1,NC,NS) = IX
          ISFDOM(2,NC,NS) = JX
          ISFDOM(3,NC,NS) = KX
          ISFDOM(4,NC,NS) = ISFDX
          GOTO 30
   40     CONTINUE
          NSFDOM(NS) = NC
          CLOSE(26)
        ENDIF
!
!---    Check surface flux domain  ---
!
        IF( INDEX(ADUM(1:),'file').NE.0 ) THEN
          DO 50 NC = 1,NSFDOM(NS)
            IX = ISFDOM(1,NC,NS)
            JX = ISFDOM(2,NC,NS)
            KX = ISFDOM(3,NC,NS)
            ISFDX = ISFDOM(4,NC,NS)
            IF( IX.LT.1 .OR. IX.GT.IFLD ) THEN
              INDX = 4
              CHMSG = 'Illegal Surface Flux Domain: I Index'
              CALL WRMSGS( INDX )
            ENDIF
            IF( JX.LT.1 .OR. JX.GT.JFLD ) THEN
              INDX = 4
              CHMSG = 'Illegal Surface Flux Domain: J Index'
              CALL WRMSGS( INDX )
            ENDIF
            IF( KX.LT.1 .OR. KX.GT.KFLD ) THEN
              INDX = 4
              CHMSG = 'Illegal Surface Flux Domain: K Index'
              CALL WRMSGS( INDX )
            ENDIF
   50     CONTINUE  

        ELSE
!
!---  Read and check surface flux domain  ---
!
          VARB = 'Surface Flux Domain: '
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(1))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(2))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(3))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(4))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(5))
          CALL RDINT(ISTART,ICOMMA,CHDUM,ISFC_TMP(6))
          IF( ISFC_TMP(1).LT.1 .OR. ISFC_TMP(1).GT.IFLD .OR.  &
            ISFC_TMP(2).LT.1 .OR. ISFC_TMP(2).GT.IFLD .OR.  &
            ISFC_TMP(1).GT.ISFC_TMP(2) ) THEN
            INDX = 4
            CHMSG = 'Illegal Surface Flux Domain: I Indices'
            CALL WRMSGS( INDX )
          ENDIF
          IF( ISFC_TMP(3).LT.1 .OR. ISFC_TMP(3).GT.JFLD .OR. &
          ISFC_TMP(4).LT.1 .OR. ISFC_TMP(4).GT.JFLD .OR. &
          ISFC_TMP(3).GT.ISFC_TMP(4) ) THEN
            INDX = 4
            CHMSG = 'Illegal Surface Flux Domain: J Indices'
            CALL WRMSGS( INDX )
          ENDIF
          IF( ISFC_TMP(5).LT.1 .OR. ISFC_TMP(5).GT.KFLD .OR. &
          ISFC_TMP(6).LT.1 .OR. ISFC_TMP(6).GT.KFLD .OR. &
          ISFC_TMP(5).GT.ISFC_TMP(6) ) THEN
            INDX = 4
            CHMSG = 'Illegal Surface Flux Domain: K Indices'
            CALL WRMSGS( INDX )
          ENDIF
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
          IF( ME.EQ.0 ) &
            WRITE(IWR,'(/,A)') VARB(1:IVR)
          IF( ME.EQ.0 ) &
            WRITE(IWR, '(2X,2(A,I6))') 'I = ',ISFC_TMP(1),' to ',ISFC_TMP(2)
          IF( ME.EQ.0 ) &
            WRITE(IWR, '(2X,2(A,I6))') 'J = ',ISFC_TMP(3),' to ',ISFC_TMP(4)
          IF( ME.EQ.0 ) &
            WRITE(IWR, '(2X,2(A,I6))') 'K = ',ISFC_TMP(5),' to ',ISFC_TMP(6)
          CALL GET_LIM(ISFC_TMP, LDI, LDIJ)
          DO K = ISFC_TMP(5), ISFC_TMP(6)
            IZZ = K - IAZMIN
            DO J = ISFC_TMP(3), ISFC_TMP(4)
              IYY = J - IAYMIN
              DO I = ISFC_TMP(1), ISFC_TMP(2)
                IXX = I - IAXMIN
                N = IXX + LDI*IYY + LDIJ*IZZ + 1
!
!---     Defined surface for output
!
               ISFC(NS,N) = 1
              ENDDO
            ENDDO
          ENDDO
        ENDIF
  100 CONTINUE
      ICSN = ICSN-ICSNX
      SUBNM = SUBNM(1:ICSN)
!
!---  End of RDSF1 group.
!
      RETURN
      END
