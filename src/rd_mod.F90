!---------------------Fortran 90 Module--------------------------------!
!
      MODULE DB_PR
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
!     Define double precision.
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.42 2007/02/13 21:20:53 d3c002 Exp $
!
!---------------------Type Declarations--------------------------!
!
      INTEGER, PARAMETER :: DP = SELECTED_REAL_KIND(14)
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE CONST
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
!     Constants
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.42 2007/02/13 21:20:53 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP) :: SMALL
      REAL(KIND=DP) :: BIG
      REAL(KIND=DP) :: ZERO
      REAL(KIND=DP) :: ONE
      REAL(KIND=DP) :: EPSL
      REAL(KIND=DP) :: GRAV
      REAL(KIND=DP) :: TABS
      REAL(KIND=DP) :: TMX
      REAL(KIND=DP) :: TMN
      REAL(KIND=DP) :: PATM
      REAL(KIND=DP) :: PMX
      REAL(KIND=DP) :: PMN
      REAL(KIND=DP) :: RCU
      REAL(KIND=DP) :: RCW
      REAL(KIND=DP) :: RCA
      REAL(KIND=DP) :: RHORL
      REAL(KIND=DP) :: RHORG
      REAL(KIND=DP) :: VCRA
      REAL(KIND=DP) :: VISRL
      REAL(KIND=DP) :: VISRG
      REAL(KIND=DP) :: TSPRF
      REAL(KIND=DP) :: HDOD
      REAL(KIND=DP) :: WTMA
      REAL(KIND=DP) :: WTMW
      REAL(KIND=DP) :: WTMS
      REAL(KIND=DP) :: TCRW
      REAL(KIND=DP) :: PCRW
      REAL(KIND=DP) :: ZCRW
      REAL(KIND=DP) :: VCRW
      REAL(KIND=DP) :: PAFW
      REAL(KIND=DP) :: DPMW
      REAL(KIND=DP) :: RHOLI
      REAL(KIND=DP) :: VISLI
      REAL(KIND=DP) :: TBA
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  PTPS
      REAL(KIND=DP) :: SUFW
      REAL(KIND=DP) :: THKRW
      REAL(KIND=DP) :: THKRA
      REAL(KIND=DP) :: TBW
      REAL(KIND=DP) :: DFGWC
      REAL(KIND=DP) :: DFGOC
      REAL(KIND=DP) :: DFGAC
      REAL(KIND=DP) :: DFLAC
      REAL(KIND=DP) :: DFLOC
      REAL(KIND=DP) :: DFLSC
      REAL(KIND=DP) :: HCAW
      REAL(KIND=DP) :: GPI
      REAL(KIND=DP) :: TENTH
      REAL(KIND=DP) :: TOLN
      INTEGER :: ISMALL
      INTEGER :: IBIG
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IPTPS
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE FILES
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
!     External file name and unit number variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.42 2007/02/13 21:20:53 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER :: IRD = 21
      INTEGER :: IWR = 22
      INTEGER :: IPL = 23
      INTEGER :: IRS = 24
      INTEGER :: ISC = 6
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISF
      INTEGER :: IBL,IVD,IRL,IRC
      CHARACTER(64) :: FNRD = 'input'
      CHARACTER(64) :: FNWR = 'output'
      CHARACTER(64) :: FNPL = 'plot'
      CHARACTER(64) :: FNRS = 'restart'
      CHARACTER(64) :: FNSR = 'screen'
      CHARACTER(64), DIMENSION(:), ALLOCATABLE :: FNSF
      CHARACTER(64) :: FNBL,FNVD,FNRL,FNRC
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE GRID
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
!     Grid variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.42 2007/02/13 21:20:53 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  X
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  Y
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  Z
      REAL(KIND=DP), DIMENSION(:), pointer ::  XP
      REAL(KIND=DP), DIMENSION(:), pointer ::  YP
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XREF
      REAL(KIND=DP), DIMENSION(:), pointer ::  ZP
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RP
      REAL(KIND=DP), DIMENSION(:), pointer ::  DXGF
      REAL(KIND=DP), DIMENSION(:), pointer ::  DYGF
      REAL(KIND=DP), DIMENSION(:), pointer ::  DZGF
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DXGP
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DYGP
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DZGP
!      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  AFX
!      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  AFY
!      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  AFZ
      real(kind=dp), dimension(:), pointer :: areab
      real(kind=dp), dimension(:), pointer :: distb
      real(kind=dp), dimension(:), pointer :: uvxb
      real(kind=dp), dimension(:), pointer :: uvyb
      real(kind=dp), dimension(:), pointer :: uvzb
      real(kind=dp), dimension(:), pointer :: xpb
      real(kind=dp), dimension(:), pointer :: ypb
      real(kind=dp), dimension(:), pointer :: zpb
      real(kind=dp), dimension(:), pointer :: grvxb
      real(kind=dp), dimension(:), pointer :: tltxb

!     zero flux boundaries
      real(kind=dp), dimension(:), allocatable :: areab_zf
      real(kind=dp), dimension(:), allocatable :: uvxb_zf
      real(kind=dp), dimension(:), allocatable :: uvyb_zf
      real(kind=dp), dimension(:), allocatable :: uvzb_zf
      real(kind=dp), dimension(:), allocatable :: xpb_zf
      real(kind=dp), dimension(:), allocatable :: ypb_zf
      real(kind=dp), dimension(:), allocatable :: zpb_zf
      real(kind=dp), dimension(:), allocatable :: bid_zf
      REAL(KIND=DP), DIMENSION(:), pointer ::  VOL
      REAL(KIND=DP) ::  GRAVX
      REAL(KIND=DP) ::  GRAVY
      REAL(KIND=DP) ::  GRAVZ
      REAL(KIND=DP), DIMENSION(:), pointer ::  TLTX
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TLTY
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TLTZ
      REAL(KIND=DP), DIMENSION(:), pointer ::  GRVX
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  GRVY
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  GRVZ
      REAL(KIND=DP), DIMENSION(:), pointer ::  GRVPC
      REAL(KIND=DP), DIMENSION(:), pointer ::  GRVPX
      REAL(KIND=DP), DIMENSION(:), pointer ::  GRVPY
      REAL(KIND=DP), DIMENSION(:), pointer ::  GRVPZ
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  ROCK
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  ROCK2
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  XREFU
      INTEGER :: IFLD
      INTEGER :: JFLD
      INTEGER :: KFLD
      INTEGER :: IJFLD
      INTEGER :: JKFLD
      INTEGER :: KIFLD
      INTEGER :: NFLD
      INTEGER :: ICS
      INTEGER :: N_DB
      INTEGER :: NXP
      INTEGER, DIMENSION(:), POINTER ::  IXP
      INTEGER, DIMENSION(:), POINTER ::  IZ
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IZx
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IXF
      INTEGER :: NXF
      INTEGER :: NROCK
      INTEGER, DIMENSION(:), pointer ::  conn_up
      INTEGER, DIMENSION(:), pointer ::  conn_dn
      real(kind=dp), dimension(:), pointer :: areac
      real(kind=dp), dimension(:), pointer :: distc
      real(kind=dp), dimension(:), pointer :: unvxc
      real(kind=dp), dimension(:), pointer :: unvyc
      real(kind=dp), dimension(:), pointer :: unvzc
      real(kind=dp), dimension(:), pointer :: dist_up
      real(kind=dp), dimension(:), pointer :: dist_dn
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MDIM
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ID
      INTEGER, DIMENSION(:), ALLOCATABLE ::  JD
      INTEGER, DIMENSION(:), ALLOCATABLE ::  KD
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE ::  ND
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NSX
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NSY
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NSZ
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IXREF
      integer, dimension(:), pointer :: id_l2g
      integer, dimension(:), pointer :: id_g2l
      integer, dimension(:), pointer :: loc2nat
      integer, dimension(:,:), allocatable :: nd2cnx 
      real(kind=dp), dimension(:,:), allocatable :: hgz_table_p 
      real(kind=dp), dimension(:), allocatable :: hgz_table_z
      INTEGER :: UPX
      INTEGER :: UPY
      INTEGER :: UPZ
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  XBF
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  YBF
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  ZBF
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE SOLTN
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
!     Solution control variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP) :: TM
      REAL(KIND=DP) :: TMMX
      REAL(KIND=DP) :: TMPR
      REAL(KIND=DP) :: DT
      REAL(KIND=DP) :: DTI
      REAL(KIND=DP) :: DTMX
      REAL(KIND=DP) :: DTAF
      REAL(KIND=DP) :: DTCF
      REAL(KIND=DP) :: DTO
      REAL(KIND=DP) :: DTSO
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TMPS
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TMPE
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TMPD
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TMPX
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TMPA
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  TMPC
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RSD
      REAL(KIND=DP) :: RSDMX
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RSDM
      REAL(KIND=DP) :: RLXF
      REAL(KIND=DP) :: RLMSG
      REAL(KIND=DP) :: CPUMX
      REAL(KIND=DP) :: CLKMX
      REAL(KIND=DP) :: CPUSEC
      REAL(KIND=DP) :: CLKSEC
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  WFMN
      REAL(KIND=DP) :: CRNTMXC
      INTEGER :: IVRSN
      INTEGER :: ISIC
      INTEGER :: ICNV
      INTEGER :: IEO
      INTEGER :: ILES
      INTEGER :: IOM
      INTEGER :: ICODE
      INTEGER :: IEQT
      INTEGER :: IEQW
      INTEGER :: IEQA
      INTEGER :: IEQO
      INTEGER :: IEQC
      INTEGER :: IEQS
      INTEGER :: IEQD
      INTEGER :: IEQDO
      INTEGER :: IEQHA
      INTEGER :: IEQHO
      INTEGER :: IEQALC
      INTEGER :: IEQDA
      INTEGER :: IAQU
      INTEGER :: IGAS
      INTEGER :: INAPL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IEQGC
      INTEGER, DIMENSION(1:100) ::  ISLC
      INTEGER, DIMENSION(1:20) ::  IDMN
      INTEGER :: NEPD
      INTEGER :: MEPD
      INTEGER :: IEPD
      INTEGER :: ICSN
      INTEGER :: IMSG
      INTEGER :: ICD
      INTEGER :: IVR
      INTEGER :: NRIMX
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NRIM
      INTEGER :: NSTEP
      INTEGER :: NRST
      INTEGER :: NITER
      INTEGER :: NTSR
      INTEGER :: NGC
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NSD
      INTEGER :: MXSTEP
      INTEGER :: IDFLT
      INTEGER :: IDFLTD
      INTEGER :: IUNM
      INTEGER :: IUNKG
      INTEGER :: IUNS
      INTEGER :: IUNK
      INTEGER :: IUNMOL
      INTEGER :: N_RST
      CHARACTER(132) ::  USER
      CHARACTER(132) ::  CMPNY
      CHARACTER(132) ::  TITLE
      CHARACTER(132) ::  INPDAT
      CHARACTER(132) ::  INPTIM
      CHARACTER(64) ::  CARD
      CHARACTER(64) ::  VARB
      CHARACTER(8) :: CHDATE
      CHARACTER(64) :: CH_VRSN
      CHARACTER(10) :: CHTIME
      CHARACTER(132), DIMENSION(:), ALLOCATABLE ::  NOTES
      CHARACTER(132) ::  SUBNM
      CHARACTER(132) ::  SUBNMX
      CHARACTER(132) ::  CHMSG
      CHARACTER(132), DIMENSION(:), ALLOCATABLE ::  CVS_ID
!      CHARACTER(132), DIMENSION(400) ::  CVS_ID
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  GCNM
      integer :: petsc_option
      REAL(KIND=DP) :: ATOL
      REAL(KIND=DP) :: RTOL
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE PORMED
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
!     Porous media property variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.42 2007/02/13 21:20:53 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  POR
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  TOR
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  CMP
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  THKS
      REAL(KIND=DP), DIMENSION(:), pointer ::  RHOS
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  CPS
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PERM
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SCHR
      REAL(KIND=DP), DIMENSION(:), pointer ::  CHML
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  GAMMA
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SCALNM
      REAL(KIND=DP), DIMENSION(:,:,:), pointer ::  RPLT
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  DFEF
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RPGC
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  RPLC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RPNC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ALBEDO
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  CPLY_SL
      REAL(KIND=DP), DIMENSION(:,:,:), POINTER ::  CPLY_RL
      REAL(KIND=DP), DIMENSION(:), pointer ::  HCMWE
      REAL(kind=dp), DIMENSION(:,:), pointer ::  trpgl
      INTEGER, DIMENSION(:), pointer ::  ITOR
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IRPG
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IGAMMA
      INTEGER :: NSCALE
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISCALE
      INTEGER, DIMENSION(:), pointer ::  IRPL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IRPN
      INTEGER, DIMENSION(:), pointer ::  ISCHR
      INTEGER, DIMENSION(:), pointer ::  ISM
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ITHK
      INTEGER, DIMENSION(:,:), pointer ::  IRPLT
      INTEGER, DIMENSION(:,:), POINTER ::  ISLTBL
      INTEGER, DIMENSION(:,:), POINTER ::  IRLTBL
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IRGTBL
      INTEGER, DIMENSION(:,:,:), POINTER ::  IRLTBLT
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IRNTBL
      INTEGER, DIMENSION(:), pointer ::  IDP
      INTEGER, DIMENSION(:), pointer ::  ISKP
      INTEGER, DIMENSION(:), pointer ::  IPRF
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IALB
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NPLY_SL
      INTEGER, DIMENSION(:), POINTER ::  NPLY_RL
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE GLB_PAR
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
!     Parameter global variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     Last Modified by WE Nichols, PNNL, 13 June 2003.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!---------------------Type Declarations--------------------------!
!
      TYPE LIST_NODE
        CHARACTER(64) :: LIST_NAME
        TYPE(LIST_NODE), POINTER :: NEXT
      END TYPE LIST_NODE
      TYPE(LIST_NODE), POINTER :: ROCK_PTR,SCALNM_PTR,SOLUT_PTR
      INTEGER :: IPF
      INTEGER :: LNOTES=1,LEPD=1
      INTEGER :: LFX=1,LFY=1,LFZ=1
      INTEGER :: LAN=1,LAD=1,LMNP=1
      INTEGER :: LT=0,LL=1,LG=0,LN=0,LC=0,LFW=0,LS=0,LD=0
      INTEGER :: LPC=0,LALC=0,LWELL=0,LDCO2=0
      INTEGER :: LBD=0,LSP=1,LPT=0
      INTEGER :: LBC=1,LBCIN=1,LBTM=1
      INTEGER :: LSR=1,LSTM=1,LNW=1,LNWT=1,LNWS=1
      INTEGER :: LRC=1,LSOLU=1,LCN=1
      INTEGER :: LREF=1,LPTM=1,LSF=1,LSFDOM=1
      INTEGER :: LOBDT=1,LOBDS=1
      INTEGER :: LTBL=0,LCHEM=1,LRK=1,LUGR=0,LGRL=1
      INTEGER :: LPTA=0,LPLANT=1,LSW=0,LATM=1,LNNGC=1
      INTEGER :: LBAL=1,LREM=1,LREL=1,LHYD=0,LR=0
      INTEGER :: LANW=1,LGC=0
      INTEGER :: LUK=1
      INTEGER :: LPH=1,LCMP=1,LSALC=1,LMPH=0
      INTEGER :: LFXY=1,LFYZ=1,LFZX=1
      INTEGER :: LFD=1
      INTEGER :: LHBW=1
      INTEGER :: LJA=1
      INTEGER :: LJB=1
      INTEGER :: LJC=1
      INTEGER :: LJD=1
      INTEGER :: LJE=1
      INTEGER :: LJF=1
      INTEGER :: LJG=1
      INTEGER :: LJH=1
      INTEGER :: LJI=1
      INTEGER :: LJJ=1
      INTEGER :: LJK=1
      INTEGER :: LJL=1
      INTEGER :: LJM=1
      INTEGER :: LJN=1
      INTEGER :: LSU=1
      INTEGER :: LSV=1,LSFV=1,LSFVGC=1
      INTEGER :: LFDT=1,LFDL=1,LFDG=1,LFDN=1
      INTEGER :: LFDC=1,LFDI=1,LFDS=1,LFDCR=1
      INTEGER :: LFDR=1,LFDRL=1,LFDRG=1,LFDRN=1
      INTEGER :: LFDD=1,LFDA=1,LFDH=1,LFDNH=1
      INTEGER :: LFDGC=1
      INTEGER :: LSX=1
      INTEGER :: LSY=1
      INTEGER :: LSZ=1
      INTEGER :: LSXT=1,LSXL=1,LSXG=1,LSXN=1
      INTEGER :: LSXC=1,LSXS=1,LSXD=1,LSXGC=1
      INTEGER :: LSYT=1,LSYL=1,LSYG=1,LSYN=1
      INTEGER :: LSYC=1,LSYS=1,LSYD=1,LSYGC=1
      INTEGER :: LSZT=1,LSZL=1,LSZG=1,LSZN=1
      INTEGER :: LSZC=1,LSZS=1,LSZD=1,LSZGC=1
      INTEGER :: LRCT=1,LRCL=1,LRCG=1,LRCN=1
      INTEGER :: LRCS=1
      INTEGER :: LRCD=1
      INTEGER :: LBCT=1,LBCL=1,LBCG=1,LBCN=1
      INTEGER :: LBCC=1,LBCI=1,LBCS=1
      INTEGER :: LBCD=1,LBCA=1,LBCH=1,LBCGC=1
      INTEGER :: LBCU=1,LBCV=1
      INTEGER :: LOUPV=1,LFILES=1
      INTEGER :: LSCHR=1,LRPLC=4,LRPGC=6,LRPNC=1,LRPL=1
      INTEGER :: LNWN=1,LSZW=1,LWSI=1
      INTEGER :: LNWV=1,LUKW=1
      INTEGER :: LP_TA=1,LT_TA=1,L_LV=1,LINH=15
      INTEGER :: LT_PH=155,LO_PH=11
      INTEGER :: LT_TH=100,LO_TH=11
      INTEGER :: LPOLYN=1,LPOLYC=4
      INTEGER :: LEQC=1,LEQE=1,LEQK=1
      INTEGER :: LRCE=1,LRCK=1,LREK=1
      INTEGER :: LSEC=1,LSEE=1,LSEK=1
      INTEGER :: LSPE=0,LESITE=0,LSPLK=0
      INTEGER :: LSPG=0,LSPK=1,LSPT=0,LCKN=1,LMC=1
      INTEGER :: LSPL=0,LSPN=0,LSPS=0
      INTEGER :: LSPR=0,LSPBC=0,LSOLSR=1
      INTEGER :: LXYZG=0
      INTEGER :: LNGC=1
      INTEGER :: LSPILL=0
      INTEGER :: LANI=1,LCAT=1,LNEU=1,LNAF=1,LNCF=1,LNNF=1,LMCG=1
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE REACT
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
!     Reaction variables
!
!     ACTVC - constant activity coefficient
!     EQ_C(LSEC,LEQC) - conservation equation parameters
!       species coefficients, conservation equations
!     EQ_E(LSEE,LEQE) - equilibrium equation parameters
!       species coefficients, equilibrium equations
!     EQ_K(LSEK+LREK,LEQK) - kinetic equation parameters
!       species coefficients + reaction coefficients, kinetic equations
!     RC_E(5,LRCE) - equilibrium reaction parameters
!     RC_K(LSPK+11,LCKN,LRCK) - kinetic reaction parameters
!     SP_L(3,LSPL) - aqueous species parameters
!     SP_S(2,LSPS) - solid species parameters
!     RS_S(1,LSPS,LFDR) - initial mineral (solid species) area
!     RS_S(2,LSPS,LFDR) - initial mineral (solid species) volume fraction
!     RS_S(3,LSPS,LFDR) - current mineral (solid species) volume fraction
!     IACTV - activity coefficient model option
!       0 - B-Dot Equation
!       1 - Davies Equation
!       2 - Pitzer Equation
!       3 - Constant
!     NEQC - number of conservation equations
!     NEQE - number of equilibrium equations
!     NEQK - number of kinetic equations
!     NRCE - number of equilibrium reactions
!     NRCK - number of kinetic reactions
!     NSPC - number of component species
!     NSPG - number of gas species
!     NSPK - number of kinetic species
!     NSPLK - number of linked species
!     NSPL - number of aqueous species
!     NSPN - number of NAPL species
!     NSPR - number of reactive species
!     NSPS - number of solid species
!     NRTSI - number of reactive transport sequence iterations
!     SP_RATE(LFDR,LSPS) - mineral reaction rate, mol/s
!     SP_AREA(LFDR,LSPS) - mineral surface area, m^2
!     SPNMC(LEQC) - conservation component species name
!     SPNMK(LEQC) - kinetic component species name
!     SPNMG(LSPG) - gas species name
!     SPNML(LSPL) - aqueous species name
!     SPNME(LSPE) - exchange species name
!     SPNMN(LSPN) - NAPL species name
!     SPNMS(LSPS) - solid species name
!     YSPG(LFDRG,LEQC+LEQK) - gas transport fraction
!     YSPL(LFDRL,LEQC+LEQK) - aqueous transport fraction
!     YSPN(LFDRN,LEQC+LEQK) - NAPL transport fraction
!     SP_C(LFDR,LSPR) - species concentration, mol/m^3 node volume
!     SP_CI(LFDR,LSPR) - initial species concentration, mol/m^3 node volume
!     SP_CO(LFDR,LSPR) - old species concentration, mol/m^3 node volume
!     SP_CMN(LFDR,LSPS) - base mineral species conc., mol/m^3 node volume
!     SP_CBO(LBCC,LSPR) - old species bound. conc., mol/m^3 node volume
!     IC_SP(LFDR,LSPR) - species initial condition type index
!     ISP_MN(LSPR) - species mineral index
!     RCNME(LRCE) - equilibrium reaction name
!     RCNMK(LRCK) - kinetic reaction name
!     POR_M(1,LFDR) - current total porosity
!     POR_M(2,LFDR) - current diffusive porosity
!     IEQ_C(LSEC+1,LEQC) - integer conservation equation parameters
!       number of species + species number + ... +
!     IEQ_E(LSEE+1,LEQE) - integer equilibrium equation parameters
!       number of species + species number + ... +
!     IEQ_K(LSEK+LREK+2,LEQK) - integer kinetic equation parameters
!       number of species + species number + ... +
!       number of reactions + reaction number + ..., kinetic equations
!     IEQ_S(LSPR) - species --> equation sequence
!     ISP_E(LSPE) - exchange site
!     IEL_LK(LSPE) - exchanged/cation species link
!     ISP_S(LEQE+LEQC+LEQK) - equation --> species sequence
!     IRC_K(LSPK+3,LRCK) - integer kinetic reaction parameters
!     IRCKN(LSPK+11) - flag to read spatially variable kinetic reaction parameters
!       0 - No spatial variation
!       1 - Rxn parameters assigned on a node by node basis
!     IRCKT(LRCK) - kinetic reaction type
!       0 - dissolution-precipitation
!       1 - forward-backward
!     CFMX(1:NUM_NODES) - spatially variable mixing coefficient; default=1
!     ISPLK(14) - conservation species pointer
!     ISPGL(LSPG) - gas species to associate aqueous species pointer
!     ISP_OW(LSPS,LFDR) - solid species lithology card overwrite
!     LEQC - number of conservation equations 
!     LEQE - number of equilibrium equations
!     LEQK - number of kinetic equations 
!     LRCE - number of equilibrium reactions
!     LRCK - number of kinetic reactions 
!     LREK - number of kinetic equation reactions 
!     LSEC - number of conservation equation species
!     LSEE - number of equilibrium equation species
!     LSEK - number of kinetic equation species 
!     LSPG - number of gas species 
!     LSPK - number of kinetic reaction species 
!     LSPL - number of aqeuous species
!     LSPN - number of NAPL species
!     LSPS - number of solid species
!     LSPE - number of exchange species
!     LSPR - number of reactive species
!     LCAT - number of cations (pitzer)
!     LANI - number of anions (pitzer)
!     LNEU - number of neutrals (pitzer)
!     LNFA - array size for anion interaction parameters (pitzer)
!     LNFC - array size for cation interaction parameters (pitzer)
!     LNFN -array size for neutral interaction parameters (pitzer)
!     LMCG - maximum absolute value of species charge (pitzer)
!     CMIN - minimum concentration for eckechem
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 8 December 2004.
!     Last Modified by MD White, PNNL, 8 December 2004.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP) ::  ACTVC,DT_RST,DTI_RST,TM_RST
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  CFMX
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  EQ_C
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  EQ_E
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  EQ_K
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RC_E
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  RC_K
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  RCNME
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  RCNMK
      REAL(KIND=DP), DIMENSION(:,:,:), pointer ::  RS_S
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SP_C
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SP_CI
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SP_CO
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SP_CMN
      REAL(KIND=DP), DIMENSION(:,:), pointer :: SP_RATE
      REAL(KIND=DP), DIMENSION(:,:), pointer :: SP_AREA
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SP_CBO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SP_L
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SP_G
      REAL(KIND=DP) ::  SP_MDL,SP_MDG,SP_MDN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SP_S
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: SP_SDCL
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SPNMC
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SPNMK
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SPNMG
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SPNML
      CHARACTER(64), DIMENSION(:), ALLOCATABLE :: SPNME
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SPNMN
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SPNMS
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YSPG
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YSPL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YSPN
      REAL(KIND=DP) :: CMIN
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  POR_M
      INTEGER, DIMENSION(:,:), pointer ::  IC_SP
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISP_MN
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IEQ_C
      INTEGER, DIMENSION(:), ALLOCATABLE :: IEL_LK
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IEQ_E
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IEQ_K
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IEQ_S
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISP_E
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISP_S
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IRC_K
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRCKN
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IRCKT
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISPGL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISPLK
      INTEGER, DIMENSION(:,:), pointer ::  ISP_OW
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IMMB
      INTEGER :: IACTV,IACTEX,NEQC,NEQE,NEQK,NRCE,NRCK,NSPE,NESITE
      INTEGER :: NSPC,NSPG,NSPK,NSPL,NSPLK,NSPN,NSPR,NSPS,NRTSI
      LOGICAL :: ECKE_ER
      LOGICAL :: FILEREAD
      REAL(KIND=DP), DIMENSION(16) :: ACTV16
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CHARG
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: FACTV !fixed species activity
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: ACTVS ! species activity
      INTEGER, DIMENSION(:), ALLOCATABLE :: IMS
      REAL(KIND=DP) :: ACTVH
      INTEGER :: NEQK_N
      real(kind=dp), DIMENSION(:), pointer ::  c_ph
      real(kind=dp), DIMENSION(:), pointer ::  vfrac_i
      integer :: cement = 0
!
!---  End of module  ---
!
      END MODULE
!
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE PTZRCOEF
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
!     Pitzer activity coefficients
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 June 2005.
!     Last Modified by MD White, PNNL, 20 June 2005.
!     $Id: allo.F,v 1.4 2011/09/19 15:53:56 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: B0
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: B1
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: B2
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: CMXX
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: TCC
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: TAA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: PSIC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: PSIA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: ALAMB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: CLAMB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: ELAMB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: HOLAMB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: BPPR
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: BPHI
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: BPR
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: BMMX
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATB0
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATB1
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATB2
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATCMX
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATNLAM
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATCLAM
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATALAM
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATHLAM
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: ATTC
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATPC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: ATTA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE :: ATPA
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTCPH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTC
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTCPR
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTCPPR
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTAPH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTA
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTAPR
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: CTAPPR
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: ETH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: ETHP
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: ETHP2
      INTEGER, DIMENSION(:), ALLOCATABLE :: JPA
      INTEGER, DIMENSION(:), ALLOCATABLE :: JPC
      INTEGER, DIMENSION(:), ALLOCATABLE :: JPN
!
!---  End of module  ---
!
      END MODULE
!
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE PTZR
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
!     Pitzer variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 20 June 2005.
!     Last Modified by MD White, PNNL, 20 June 2005.
!     $Id: allo.F,v 1.4 2011/09/19 15:53:56 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
     USE DB_PR
!
      INTEGER :: NCC
      INTEGER :: NA
      INTEGER :: NNN
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDD
!
!---  End of module  ---
!
      END MODULE
!
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE TRNSPT
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
!     Solute transport variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  C
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  CO
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  CNL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  c_flux
      REAL(KIND=DP), DIMENSION(:,:,:), pointer ::  c_flux_nd
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  CNLO
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SMDEF
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UCN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VCN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WCN
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  HLF
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHLF
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SRCIC
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SMDL
      REAL(KIND=DP) :: SMDLS
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SMDN
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SMDG
      REAL(KIND=DP), DIMENSION(:,:,:), pointer ::  SDCL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SDCLS
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YG
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YN
      REAL(KIND=DP), DIMENSION(:,:,:), pointer ::  PCSL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PCSLD
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  CMTLN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PCLN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PCGL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  CB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  CBO
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YLB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YGB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  YNB
      REAL(KIND=DP), DIMENSION(:), pointer ::  DISPL
      REAL(KIND=DP), DIMENSION(:), pointer ::  DISPT
      REAL(KIND=DP), DIMENSION(:), pointer ::  DISPTV  !BH Vertical transverse dispersivity
      INTEGER,DIMENSION(:), ALLOCATABLE:: XYZ_DISPERSIVITY !BH whether there are two or there components of dispersivity
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  RCHDF
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SOLML
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  RCHDFL
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  RCHDFN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHLFL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHLFN
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  ELC_DCF
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  ELC_VCF
      REAL(KIND=DP) :: ELC_DUN
      REAL(KIND=DP) :: ELC_VUN
      REAL(KIND=DP) :: DT_CRN
      REAL(KIND=DP) :: DTI_CRN
      REAL(KIND=DP) :: TM_CRN
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  CCL_CRN
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DPLGS
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DPTRS
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DPLD
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DPTD
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  D50
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  PMDD
      CHARACTER(64) ::  ELC_SOL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  CHDF
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  SOLUT
      REAL(KIND=DP), DIMENSION(:), pointer ::  CRNTL
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  CRNTG
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  CRNTN
      REAL(KIND=DP) :: CRNTMXT
      REAL(KIND=DP), dimension(:), allocatable :: TCRNTMXT
      INTEGER :: NSOLU
      INTEGER :: IDISP
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IEDL
      INTEGER :: IEDLS
      INTEGER :: ISP_IEDL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IPCL
      INTEGER, DIMENSION(:,:), pointer ::  IPCSL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IPCSLD
      INTEGER, DIMENSION(:,:), pointer ::  ICT
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  ICTN
      INTEGER :: IDSPS
      INTEGER :: IDSPD
      INTEGER :: ICRNT
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NCHEM
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IPCLN
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IPCGL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IMTLN
      INTEGER :: NSL_ELC
      INTEGER :: IDF_ELC
      INTEGER :: IVF_ELC
      INTEGER, DIMENSION(:), ALLOCATABLE ::  N_CRN
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE HYST
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
!     Hysteretic k-s-P function variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:), pointer ::  ASL
      REAL(KIND=DP), DIMENSION(:), pointer ::  AST
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  ASLMIN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ASTMAX
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  ASTMIN
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  ASNT
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  ASNR
      REAL(KIND=DP), DIMENSION(:), pointer ::  ASGT
      REAL(KIND=DP), DIMENSION(:,:), POINTER ::  SGT
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SNR
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SNT
      REAL(KIND=DP) :: BGN,BGL,BHL,BIL,BNL
      REAL(KIND=DP) :: CA_GN,CA_GL,CA_NL
      REAL(KIND=DP) :: SIG_GN,SIG_GL,SIG_NL,SIG_HL,SIG_IL
      INTEGER, DIMENSION(:,:), pointer ::  NPHAZ
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  ASGTL
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  ASGTN
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SGTL
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SGTN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ASNMIN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ASLSC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SLSC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  HDSC
      INTEGER, DIMENSION(:,:), POINTER ::  IPH
      INTEGER ::  ISNR
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE TABL
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
!     Tabular data variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: TBLX
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: TBLY
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: TBLDDX
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE :: TBLDDY
      INTEGER :: NTBL
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE NCG_PT
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
!     Noncondensible gas property table variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  P_TA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  T_TA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  RHO_TA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  H_TA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  U_TA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  FUG_TA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  S_TA
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  RHO_ST
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  H_ST
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  U_ST
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  FUG_ST
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  S_ST
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  P_PH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  T_TH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  YMHO_PH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  YMHO_TH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XSCA_PH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XSCA_TH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLCA_PH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLCA_TH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XSCO_PH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XSCO_TH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLCO_PH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLCO_TH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  T_PH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  P_TH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  YMGO_PH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  YMGO_TH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  T_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  P_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOL_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  HL_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UL_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOV_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  HV_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UV_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  FUG_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SL_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SV_LV
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  CINH
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XLIMX
      INTEGER :: IT_PH
      INTEGER :: IO_PH
      INTEGER :: IT_TH
      INTEGER :: IS_TH
      INTEGER :: IO_TH
      INTEGER :: I_INH
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IP_TA
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IT_TA
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IV_TA
      INTEGER :: INCG
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IC_NCG
      INTEGER, DIMENSION(:), ALLOCATABLE ::  I_LV
      CHARACTER(64), DIMENSION(:), ALLOCATABLE :: INHNM
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE JACOB
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
!     Linear system variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ALU
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  BLU
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  CLU
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  DLU
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RSDL
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IM
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ILU
      INTEGER, DIMENSION(:), ALLOCATABLE ::  JLU
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  JM
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  KLU
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MLU
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NLU
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  KLUC
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MLUC
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NLUC
      integer, dimension(:), pointer :: imxp
      integer, dimension(:), allocatable :: loc_map
      integer, dimension(:), allocatable :: nnz_o
      integer, dimension(:), allocatable :: nnz_d
      INTEGER :: ISVC
      INTEGER :: ISVT
      INTEGER :: ISVF
      INTEGER :: MKC
      INTEGER :: MKT
      INTEGER :: MUC
      INTEGER :: MLC
      INTEGER :: MDC
      INTEGER :: MUT
      INTEGER :: MLT
      INTEGER :: MDT
      integer :: lsize
      integer :: llsize
      integer :: gsize
      integer :: lstart
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  accum_res
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  flux_res
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  accum_deriv
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  flux_deriv
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  residual
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE FLUXP
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
!     Primary flux variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  q_flux
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  q_flux_nd
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  vnc
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  s_fx
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  s_area
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UDLO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UDLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UG
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UDGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UDGO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ULA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ULW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UDGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UDS
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UNA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VDLO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VDLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VG
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VDGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VDGO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VLW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VDGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VDS
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VNA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WDLO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WDLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WG
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WDGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WDGO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WLW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WDGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WDS
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WNA
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE FLUXS
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
!     Salt/surfactant flux variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  US
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VS
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WS
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE FLUXD
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
!     Dissolved-oil flux variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ULO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VLO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WLO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UGO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  VGO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  WGO
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE FDVP
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
!     Primary field variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  T
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PG
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PN
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  DNR
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PSO
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SG
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SN
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PORD
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PORT
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PSW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PVA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PVO
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PVW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XGO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMGO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLO
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  XLW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMLO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMLW
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  RHOL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  RHOG
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHON
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  VISL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  TORL
      REAL(KIND=DP), DIMENSION(:,:,:), pointer ::  RKL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  DFLO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  DFLA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  DFLS
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOMG
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SI
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PI
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOML
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SDPM
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SDPF
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  S
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  POSM
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  BTGL
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PERMRF
      REAL(KIND=DP), DIMENSION(:), pointer ::  PCMP
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XMLOS
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE FDVH
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
!     Hydrate field variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 4 September 2004.
!     Last Modified by MD White, PNNL, 4 September 2004.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XHW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XHA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XHO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  THKH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  HH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SH
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  UEGA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  YMGO
      REAL(KIND=DP) ::  CHFDR
      REAL(KIND=DP) ::  CHMER
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IC_SH
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE BCV
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
!     Global boundary condition variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!---------------------Type Declarations--------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:,:), allocatable ::  BC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PHDL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PHDN
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE :: BCXYZG
      INTEGER, DIMENSION(:), pointer ::  IBCD
      INTEGER, DIMENSION(:), pointer ::  IBCN
      INTEGER, DIMENSION(:), pointer ::  irefb
      INTEGER, DIMENSION(:), pointer ::  IBCIN
      INTEGER, DIMENSION(:,:), pointer ::  IBCT
      INTEGER, DIMENSION(:), pointer ::  IBCM
      INTEGER, DIMENSION(:), pointer ::  IBCC
      INTEGER, DIMENSION(:,:), pointer ::  IBCSP
      INTEGER, DIMENSION(:), pointer ::  IBCSN
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IBCLL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  JBCLL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  KBCLL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MBCLL
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NBCLL
      INTEGER, DIMENSION(:), ALLOCATABLE :: NBCM 
      integer, dimension(:), pointer :: ibcbs
      INTEGER :: NBC
      integer, dimension(:), allocatable :: base_node
      real(kind=dp), dimension(:), allocatable :: basex
      real(kind=dp), dimension(:), allocatable :: basey
      real(kind=dp), dimension(:), allocatable :: basez
      real(kind=dp), dimension(:), allocatable :: basep
      integer, dimension(:), allocatable :: basen
      integer :: num_zf
! arrays to fix calculation in tmstep.F90
      integer, dimension(:), allocatable :: ibcc_t
      integer, dimension(:), allocatable :: ibcm_t
      real(kind=dp), dimension(:,:), allocatable :: bc_t
      integer :: nb_t
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE SOURC
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
!     Source variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  SRC
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SRCP
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SRCT
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SRCW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SRCA
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SRCO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SRCS
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SRCD
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  SRCGC
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SRCIT
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SRCIW
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SRCIA
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SRCIO
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SRCIS
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SRCID
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SRCIGC
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SRCSOL
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PLWB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PGW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  QLW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  QNW
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  QTW
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  GWSI
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  SWSI
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  IWSI
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  ISRDM
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  ISRDM_
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISRM
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISRT
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NSOLSR
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  ISOLSR
      INTEGER :: NSR
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE PLT_ATM
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
!     Plant and atmospheric variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PARMS_P
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  RFIM_P
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  T_SO
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PL_SO
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  T_PO
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  T_CO
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  PGW_CO
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  HLW_P
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  ATMOS
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  ATMC
      REAL(KIND=DP) :: ATMST
      REAL(KIND=DP) :: SWBCD
      REAL(KIND=DP), DIMENSION(2) :: RWRO
      INTEGER :: NPLANT
      INTEGER :: NATM_T
      INTEGER :: IATM_C
      INTEGER, DIMENSION(:), ALLOCATABLE :: IRSM_P
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  PLANT
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITMP_P
      INTEGER, DIMENSION(:), ALLOCATABLE :: IALB_P
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISRM_P
      INTEGER :: ISWBCF
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE OUTPU
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
!     Output control variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  PRTM
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SF
      CHARACTER(64) ::  UNTM
      CHARACTER(64) ::  UNLN
      CHARACTER(64) ::  UNAR
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  UNPLOT
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  UNREF
      CHARACTER(64), DIMENSION(:,:), ALLOCATABLE ::  UNSF
      CHARACTER(6), DIMENSION(:), ALLOCATABLE ::  CHREF
      CHARACTER(5), DIMENSION(:,:), ALLOCATABLE ::  CHSF
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IPLOT
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IREF
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IPLOTGC
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IREFGC
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NDREF
      INTEGER :: NPRTM
      INTEGER :: NVPLOT
      INTEGER :: NREF
      INTEGER :: NVREF
      INTEGER :: ICNO
      INTEGER :: ICNS
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISFT
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISFGC
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISFF
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISFD
      INTEGER, DIMENSION(:,:), POINTER ::  ISFC
      INTEGER :: NSF
      INTEGER :: NSFGP
      INTEGER :: IHSF
      INTEGER :: IFQS
      INTEGER :: IFQO
      INTEGER :: ISGNS
      INTEGER :: ISGNO
      INTEGER :: ISGNP
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISFGP
      INTEGER, DIMENSION(:), ALLOCATABLE ::  ISFSN
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NSFDOM
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE ::  ISFDOM
      REAL(KIND=DP), DIMENSION(:), pointer ::  varp_tmp
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE POINTE
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
!     Numerical scheme pointer variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MNOD
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MADJ
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MFLX
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MPOS
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MNEG
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MPOSB
      INTEGER, DIMENSION(:), ALLOCATABLE ::  MNEGB
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE FDVSO
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
!     Second-order-time field variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:), pointer ::  PORD_O
      REAL(KIND=DP), DIMENSION(:), pointer ::  XLW_O
      REAL(KIND=DP), DIMENSION(:), pointer ::  SL_O
      REAL(KIND=DP), DIMENSION(:), pointer ::  RHOL_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XLA_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XLO_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XGA_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XGW_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XGO_O
      REAL(KIND=DP), DIMENSION(:), pointer ::  PL_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SG_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SN_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RHON_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  PN_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RHOG_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  HN_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  UEG_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  HL_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  PORT_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  T_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XLS_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  S_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RHOI_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  HI_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SI_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  YLS_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SS_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XSO_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XNA_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XNW_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XNO_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RHOSP_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  HSP_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XHA_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XHO_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  XHW_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  RHOH_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  HH_O
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SH_O
!
!---  End of module  ---
!
      END MODULE
!---------------------Fortran 90 Module--------------------------------!
!
      MODULE UCODE
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
!     Inverse (UCode) variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:,:), ALLOCATABLE ::  R_OBDS
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  R_OBDT
      REAL(KIND=DP) :: TMOB
      CHARACTER(64), DIMENSION(:), ALLOCATABLE ::  C_OBDT
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::  I_OBDT
      INTEGER :: IOBDEF
      INTEGER :: IOBDSF
      INTEGER :: IOBDUF
      INTEGER :: NOBDT
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NOBDS
      INTEGER :: NOBDP
      LOGICAL :: FLG_UNI
      LOGICAL :: FLG_EXT
      integer,dimension(:),allocatable :: nc_b
      integer, dimension(:,:), allocatable :: nc_bs
      real(kind=dp), dimension(:,:,:), allocatable :: xyz_b
      integer, dimension(:), allocatable :: nro
!
!---  End of module  ---
!
      END MODULE

!---------------------Fortran 90 Module--------------------------------!
!
      MODULE BCVP
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
!     Primary boundary condition variables
!
!----------------------Authors-----------------------------------------!
!
!     Written by MD White, PNNL, 5 November 2002.
!     Last Modified by MD White, PNNL, 5 November 2002.
!     $Id: allo.F,v 1.46 2008/02/13 01:01:35 d3c002 Exp $
!
!
!----------------------Fortran 90 Modules-----------------------!
!
      USE DB_PR
!
!----------------------Type Declarations-------------------------------!
!
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  TB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PLB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PGB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PNB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PSOB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PSWB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SLB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  SGB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SNB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PORDB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PORTB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOMGB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PVAB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PVOB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  PVWB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XGAB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XGOB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XGWB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMGAB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMGOB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMGWB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLAB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XLOB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  XLWB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMLWB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMLAB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  XMLOB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  RHOLB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOGB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHONB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  TORLB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  VISLB
      REAL(KIND=DP), DIMENSION(:,:,:), pointer ::  RKLB
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  q_flux_b
      REAL(KIND=DP), DIMENSION(:,:), pointer ::  c_flux_b
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  DFLOB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  DFLAB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  DFLSB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SIB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  PIB
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SDPMB
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE ::  SDPFB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  SB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  POSB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  BTGLB
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE ::  RHOMLB
!
!---  End of module  ---
!
      END MODULE

