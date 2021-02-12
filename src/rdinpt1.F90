      SUBROUTINE RDINPT1
!

!----------------------Fortran 90 Modules------------------------------!
!

      USE GLB_PAR
      USE SOLTN
      USE GRID
      USE FILES
      USE GRID_MOD
      USE REACT
      USE TRNSPT
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
!--- PETSc
#include "petscwrapper.h"

!
!----------------------Parameter Statements----------------------------!
!
      INTERFACE
        SUBROUTINE GET_NODE_IFIELD(T_STRING, T_FIELD,  &
        T_OK)
        USE GRID_MOD
        IMPLICIT NONE
        CHARACTER (LEN=*) :: T_STRING ! in
        INTEGER, POINTER :: T_FIELD(:) ! out
        LOGICAL :: T_OK ! OUT
        INTEGER I, SLEN, NLEN, GRID_CLEN
        END SUBROUTINE GET_NODE_IFIELD
      END INTERFACE
!
!----------------------Type Declarations-------------------------------!
!
      CHARACTER*512 CHDUM
      CHARACTER*64 ROCKFILENAME
      DOUBLE PRECISION :: T_E,T_B,BC_TIME_
      INTEGER :: IERR
      LOGICAL :: T_OK
!
!--- Search buffer for input cards ---
!
      ME = GA_NODEID()
      INDX = 0
      ME = GA_NODEID()
      NPROC = GA_NNODES()
!
!---  Search input file for simulation title card  ---
!
      IF( BUFFEREDREAD_FIND( '~simulation' ) )THEN
        CALL RDSIMU
      ELSE
        INDX = 18
        CHMSG = 'Missing Simulation Title Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for solution control card  ---
!
      IF( BUFFEREDREAD_FIND( '~solution' ) )THEN
        CALL RDSOLU
        IF( IOM.NE.1 ) THEN
          INDX = 18
          CHMSG = 'Incompatible Operational Mode'
          CALL WRMSGS( INDX )
        ENDIF
      ELSE
        INDX = 18
        CHMSG = 'Missing Solution Control Card!'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for rock/soil zonation card  ---
!
      IF( BUFFEREDREAD_FIND( '~rock/soil' ) )THEN
        CALL RDROCK( ROCKFILENAME )
      ELSE
        INDX = 4
        CHMSG = 'Missing Rock/Soil Zonation Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for inactive nodes card  ---
!
      IF( BUFFEREDREAD_FIND( '~inactive' ) )THEN
        CALL RDINAC( ROCKFILENAME )
      ELSE
        INDX = 1
        CHMSG = 'Missing Inactive Nodes Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        CALL ADD_NODE_IFIELD('inactive',IDX)
        IXP => i_nd_fld(idx)%p
        IXP = 1
      ENDIF
!
!---  Search input file for mechanical properties card  ---
!
      IF( BUFFEREDREAD_FIND( '~mechanical' ) )THEN
        CALL RDMECH
      ELSE
        INDX = 18
        CHMSG = 'Missing Mechanical Properties Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for hydraulic properties card  ---
!
      IF( BUFFEREDREAD_FIND( '~hydraulic' ) ) THEN
        CALL RDHYDR
      ELSE
        INDX = 18
        CHMSG = 'Missing Hydraulic Properties Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for saturation function card  ---
!
      IF( BUFFEREDREAD_FIND( '~saturation' ) )THEN
        CALL RDSP1
      ELSE
        INDX = 18
        CHMSG = 'Missing Saturation Function Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for aqueous relative permeability cards  ---
!
      IF( BUFFEREDREAD_FIND('~x-aqueous rel' ) .AND. &
          BUFFEREDREAD_FIND('~y-aqueous rel' ) .AND. &
          BUFFEREDREAD_FIND('~z-aqueous rel' ) ) THEN
        IF( BUFFEREDREAD_FIND('~x-aqueous rel' ) )THEN
          ITX = 1
          CALL RDLRPT( ITX )
          IRPLX = 1
        ENDIF
        IF( BUFFEREDREAD_FIND('~y-aqueous rel' ) )THEN
          ITX = 2
          CALL RDLRPT( ITX )
          IRPLY = 1
        ENDIF
        IF( BUFFEREDREAD_FIND('~z-aqueous rel' ) )THEN
          ITX = 3
          CALL RDLRPT( ITX )
          IRPLZ = 1
        ENDIF
        IF( IRPLX.EQ.0 .AND. IRPLY.EQ.0 .AND. IRPLZ.EQ.0 ) THEN
          INDX = 18
          CHMSG = 'Missing Aqueous Relative Permeability Card'
          CALL WRMSGS( INDX )
        ELSE
          IF( IRPLX.EQ.0 .AND. IFLD.GT.1 ) THEN
            INDX = 18
            CHMSG = 'Missing X-Aqueous Relative Permeability Card'
            CALL WRMSGS( INDX )
          ELSEIF( IRPLY.EQ.0 .AND. JFLD.GT.1 ) THEN
            INDX = 18
            CHMSG = 'Missing Y-Aqueous Relative Permeability Card'
            CALL WRMSGS( INDX )
          ELSEIF( IRPLZ.EQ.0 .AND. KFLD.GT.1 ) THEN
            INDX = 18
            CHMSG = 'Missing Z-Aqueous Relative Permeability Card'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRPLX.EQ.0 .AND. IFLD.EQ.1 ) THEN
            INDX = 1
            CHMSG = 'Missing X-Aqueous Relative Permeability Card'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRPLY.EQ.0 .AND. JFLD.EQ.1 ) THEN
            INDX = 1
            CHMSG = 'Missing Y-Aqueous Relative Permeability Card'
            CALL WRMSGS( INDX )
          ENDIF
          IF( IRPLZ.EQ.0 .AND. KFLD.EQ.1 ) THEN
            INDX = 1
            CHMSG = 'Missing Z-Aqueous Relative Permeability Card'
            CALL WRMSGS( INDX )
          ENDIF
        ENDIF
      ELSEIF( BUFFEREDREAD_FIND('~aqueous rel' ) )THEN
        CALL RDLRP
        IRPLX = 1
        IRPLY = 1
        IRPLZ = 1
      ELSE
        INDX = 18
        CHMSG = 'Missing Aqueous Relative Permeability Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for solute/fluid interaction card --
!
      IF( BUFFEREDREAD_FIND( '~solute/fluid' ) )THEN
        CALL RDTF1
      ELSE
        IF( IEQC.NE.0 ) THEN
          INDX = 18
          CHMSG = 'Missing Solute/Fluid Interactions Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Search input file for solute/porous media interaction card --
!
      IF( BUFFEREDREAD_FIND( '~solute/porous' ) )THEN
        CALL RDTP1
      ELSE
        IF( IEQC.NE.0 .AND. ISLC(40).NE.0 ) THEN
          INDX = 18
          CHMSG = 'Missing Solute/Porous Media Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Skip reaction and equation cards, no
!     reactive transport  ---
!
      IF( ISLC(40).EQ.1 )THEN

!
!---  Search input file for aqueous species card  ---
!
        IF( BUFFEREDREAD_FIND( '~aqueous specie' ) )THEN
          CALL RDAQSP
        ELSE
          INDX = 1
          CHMSG = 'Missing Aqueous Species Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for solid species card  ---
!
        IF( BUFFEREDREAD_FIND( '~solid specie' ) )THEN
          CALL RDSDSP
        ELSE
          INDX = 1
          CHMSG = 'Missing Solid Species Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for exchanged species card  ---
!
        IF( BUFFEREDREAD_FIND( '~exchanged specie' ) )THEN
          CALL RDEXSP
        ELSE
          INDX = 1
          CHMSG = 'Missing Exchanged Species Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for equilibrium reactions card  ---
!
        IF( BUFFEREDREAD_FIND( '~equilibrium react' ) )THEN
          CALL RDEQRC
        ELSE
          INDX = 1
          CHMSG = 'Missing Equilibrium Reactions Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for kinetic reactions card  ---
!
        IF( BUFFEREDREAD_FIND( '~kinetic react' ) )THEN
          CALL RDKNRC
        ELSE
          INDX = 1
          CHMSG = 'Missing Kinetic Reactions Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for equilibrium equation card  ---
!
        IF( BUFFEREDREAD_FIND( '~equilibrium equat' ) )THEN
          CALL RDEQEQ
        ELSE
          INDX = 1
          CHMSG = 'Missing Equilibrium Equations Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for kinetic equations card  ---
!
        IF( BUFFEREDREAD_FIND( '~kinetic equat' ) )THEN
          CALL RDKNEQ
        ELSE
          INDX = 1
          CHMSG = 'Missing Kinetic Equations Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for conservation equations card  ---
!
        IF( BUFFEREDREAD_FIND( '~conservation equat' ) )THEN
          CALL RDCNEQ
        ELSE
          INDX = 1
          CHMSG = 'Missing Conservation Equations Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for lithology card  ---
!
        IF( BUFFEREDREAD_FIND( '~lithol' ) )THEN
          CALL RDLITH
        ELSE
          INDX = 1
          CHMSG = 'Missing Lithology Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
!
!---  Search input file for reactive species link card  ---
!
        IF( BUFFEREDREAD_FIND( '~species link' ) )THEN
          CALL RDSPLK
        ELSE
          INDX = 1
          CHMSG = 'Missing Reactive Species Link Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
      ENDIF 
!
!---  Search input file for initial conditions card --
!
      IF( BUFFEREDREAD_FIND( '~initial' ) )THEN
        t_b = mpi_wtime()
        CALL RDIC1
        t_e = mpi_wtime()
        initial_condition_time=t_e-t_b
      ELSE
        IF( IEO.EQ.2 .OR. IEO.EQ.4 ) THEN
          INDX = 1
          CHMSG = 'Missing Initial Conditions Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
          INDX = 2
          CALL RDRST(INDX)
          ISIC = 3
        ELSE
          INDX = 18
          CHMSG = 'Missing Initial Conditions Card'
          IF( ME.EQ.0 ) CALL WRMSGS( INDX )
        ENDIF
      ENDIF
!
!---  Search input file for boundary conditions card --
!
      IF( BUFFEREDREAD_FIND( '~boundary' ) )THEN
        t_b = mpi_wtime()
        CALL RDBC1
        t_e = mpi_wtime()
      ELSE
        INDX = 1
        CHMSG = 'Missing Boundary Conditions Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for source card --
!
      IF( BUFFEREDREAD_FIND( '~source' ) )THEN
        CALL RDSR1
      ELSE
        INDX = 1
        CHMSG = 'Missing Source Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for output control card --
!
      IF( BUFFEREDREAD_FIND( '~output' ) )THEN
        call get_node_ifield('natural_id',loc2nat,t_ok)
!       loc2nat => t_ivec%d_data
        CALL RDOU1
      ELSE
        INDX = 1
        CHMSG = 'Missing Output Control Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for surface flux card --
!
      IF( BUFFEREDREAD_FIND( '~surface' ) )THEN
        CALL RDSF1
      ELSE
        INDX = 1
        CHMSG = 'Missing Surface Flux Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF
!
!---  Search input file for observed data card --
!
      IF( BUFFEREDREAD_FIND( '~observed' ) )THEN
        CALL RDOBDA
      ELSE
        INDX = 1
        CHMSG = 'Missing Observed Data Card'
        IF( ME.EQ.0 ) CALL WRMSGS( INDX )
      ENDIF

!
!---  Free up memory
!
      T_OK = BUFFEREDREAD_TERM()
      call ga_sync
      close(21)
return
end subroutine rdinpt1
