! This implementation assumes HDF5 file format and parallel access. 
! File preparation and read uses H5hut interface to HDF5 primitives 
!   http://www-vis.lbl.gov/Research/H5hut
!
! Assumptions
!   Only one file can be open/written to at a time
!   One timestep per file
! TODO
!    address undetermined stuff below

! Using this to stub in new H5BLOCK API additions so they are ready to go once we get the 
! new library.  Target is late March 2013.
!#define HAVE_NEWAPI

MODULE SIO

  IMPLICIT NONE


  integer INT_SIZE, DBL_SIZE
#ifdef USE_H5HUT
  parameter (INT_SIZE = 8, DBL_SIZE = 8)
#else
  parameter (INT_SIZE = 4, DBL_SIZE = 8)
#endif



  ! index range for locally held nodes
  integer*8 ixmin8, ixmax8, iymin8, iymax8, izmin8, izmax8

  ! local dimensions
  integer ldimx, ldimy, ldimz

  ! data streaming buffers
  real*8, allocatable :: r8buf(:,:,:)
  integer*8, allocatable :: i8buf(:,:,:)

  ! domain origin
  double precision x_orig, y_orig, z_orig

  ! Handle to current file
  integer*8 sio_curfileid 

  ! Counters for IO statistics
  integer*8 mbytes_write
  double precision sio_time_read, sio_time_write
  double precision sio_time_open, sio_time_close


  ! The following are directly accessed from grid_mod
  ! Would be nice to at least localize this to the init method
  !integer ixmin, ixmax, iymin, iymax, izmin, izmax
  !integer nxdim, nydim, nzdim
  !double precision xdim, ydim, zdim
  !double precision dx, dy, dz
  !integer ldx, ldy, ldz
  !integer iaxmin, iaymin, iazmin, iaxmax, iaymax, iazmax
  !double precision, pointer :: d_xc(:),d_yc(:),d_zc(:)

  private ixmin8, ixmax8, iymin8, iymax8, izmin8, izmax8
  private ldimx, ldimy, ldimz
  private i8buf, r8buf
  private x_orig, y_orig, z_orig
  private sio_curfileid
  private mbytes_write
  private sio_time_read, sio_time_write, sio_time_open, sio_time_close

  CONTAINS

  !> 
  !! Initialize IO system
  !! Initilize:
  !!   information about the mesh and how its divided over processors
  !!   timers
  !<
  subroutine sio_init()  
    use grid_mod
    implicit none

    ! Get index range for locally held nodes
    ixmin8 = ixmin
    ixmax8 = ixmax
    iymin8 = iymin
    iymax8 = iymax
    izmin8 = izmin
    izmax8 = izmax

    ! local dimensions
    ldimx = ixmax-ixmin+1
    ldimy = iymax-iymin+1
    ldimz = izmax-izmin+1

    x_orig = xmini
    y_orig = ymini
    z_orig = zmini


#ifdef USE_H5HUT
    ! Allocate IO buffers
    allocate(r8buf(ldimx,ldimy,ldimz))
    allocate(i8buf(ldimx,ldimy,ldimz))
#endif

    sio_curfileid = -1

    ! Initalize timers
    sio_time_open = 0.
    sio_time_close = 0.
    sio_time_read = 0.
    sio_time_write = 0.
  end subroutine sio_init


  !< 
  !! Terminates io module and dumps permformance statistics
  !!
  subroutine sio_terminate
     implicit none
#include "mafdecls.fh"
#include "global.fh"
     integer me
     double precision dgb, bw, bwt, time_owc, onegb, nproc
     me = ga_nodeid()
     nproc = ga_nnodes()
     bw = 0.
     bwt = 0.
     onegb = 1073741824
     dgb = dble(mbytes_write)/onegb

#ifdef USE_H5HUT

     ! Sum times across processors
     call ga_dgop(1,dgb,1,'+')
     call ga_dgop(2,sio_time_read,1,'+')
     call ga_dgop(3,sio_time_write,1,'+')
     call ga_dgop(4,sio_time_open,1,'+')
     call ga_dgop(5,sio_time_close,1,'+')

     time_owc = sio_time_write + sio_time_read + sio_time_open + sio_time_close
     if (sio_time_write.gt.0.0d00) bw = dgb/(sio_time_write/nproc)
     if (time_owc .gt. 0.0d00) bwt = dgb/(time_owc/nproc)

     if (me.eq.0) then
        write(6,*) ''
        write(6,110)
        write(6,111) dgb
        write(6,112) bwt
        write(6,113) bw
        write(6,*) ""
        write(6,114) time_owc
        write(6,115) sio_time_open
        write(6,116) sio_time_read
        write(6,117) sio_time_write
        write(6,118) sio_time_close
        write(6,*) ""
        write(6,119) time_owc/nproc
        write(6,120) sio_time_open/nproc
        write(6,121) sio_time_read/nproc
        write(6,122) sio_time_write/nproc
        write(6,123) sio_time_close/nproc

110 format('IO Timing Statistics:')
111 format('   Total Bytes Written    (GB)            : ',f16.8)
112 format('   Bandwidth (orwc)       (GB/s)          : ',f16.8)
113 format('   Bandwidth (write-only) (GB/s)          : ',f16.8)

114 format('   Total Time (owc) (sec)                 : ',f16.8)
115 format('   Total Time Open  (sec)                 : ',f16.8)
116 format('   Total Time Read  (sec)                 : ',f16.8)
117 format('   Total Time Write (sec)                 : ',f16.8)
118 format('   Total Time Close (sec)                 : ',f16.8)

119 format('   Average Time (owc)    (sec)            : ',f16.8)
120 format('   Average Time in Open  (sec)            : ',f16.8)
121 format('   Average Time in Read  (sec)            : ',f16.8)
122 format('   Average Time in Write (sec)            : ',f16.8)
123 format('   Average Time in Close (sec)            : ',f16.8)
     endif

     deallocate(r8buf)
     deallocate(i8buf)

#endif

end subroutine sio_terminate






  !> 
  !! Open H5Block file.
  !! We assume:
  !!    mpiio is far better option on most systems today.
  !!    1M stripe
  !!    1 timestep per file
  !<
  integer function sio_openw(fileroot, comm, istep, tstep)  result(istat)
     use grid_mod   ! needed for d_?c array
     implicit none

     character(*), intent(in)    :: fileroot
     integer,intent(in)          :: comm
     integer,intent(in)          :: istep
     double precision,intent(in) :: tstep

#ifdef USE_H5HUT
     include "mpif.h"
     include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     double precision tbeg
     integer*8 itmp8
     character*128 filename
     character*64 tmp
     integer slen, i

     istat = -1
#ifdef USE_H5HUT
     tbeg = ga_wtime()


     tmp(1:) = '0'
     slen = len_trim(fileroot)
     tmp(1:slen) = trim(fileroot)
     WRITE( tmp(slen+1:),'(I6)') istep
     slen = len_trim(tmp)
     do i = 1, slen
        if (tmp(i:i) .eq. ' ') tmp(i:i) = '0'
     enddo
     filename(1:slen) = tmp(1:slen)
     filename(slen+1:slen+8) = '.h5block'
     filename(slen+9:) = ''

     !Default is 1 I believe; turn it up if you need to debug
     !istat = h5pt_set_verbosity_level (4_8);
     sio_curfileid = h5pt_openw_par_align(trim(filename), comm, 1048576_8, "fs_lustre") 
     istat = h5bl_define3dlayout(sio_curfileid,ixmin8,ixmax8,iymin8,iymax8,izmin8,izmax8)
#ifdef HAVE_NEWAPI
     istat = h5bl_3d_set_origin(sio_curfileid,x_orig,y_orig,z_orig)
     istat = h5bl_3d_set_spacing(sio_curfileid,title,dx,dy,dz)
     itmp8 = istep ! expecting bug fix so we can report true step number
     istat = h5pt_setstep(sio_curfileid,itmp8)
     istat = h5pt_settime ( sio_curfileid, tstep )
     istat = h5pt_settimeunit ( sio_curfileid, "seconds since 0-0-0 00:00:00  365.25 days/year")

     !TODO dynamically decide if this should be written or just spacing
     itmp8 = nxdim
     istat = h5bl_3d_set_field_xcoords(sio_curfileid, title, d_xc(1), itmp8)
     itmp8 = nydim
     istat = h5bl_3d_set_field_ycoords(sio_curfileid, title, d_yc(1), itmp8)
     itmp8 = nzdim
     istat = h5bl_3d_set_field_zcoords(sio_curfileid, title, d_yc(1), itmp8)
#else
     itmp8 = 1   ! always use 1 within a single file or visit will reject it (fortran indexing)
     istat = h5pt_setstep(sio_curfileid,itmp8)
     istat = sio_write_scalar_dbl("time step", tstep)
#endif

     sio_time_open = sio_time_open + ga_wtime() - tbeg
#endif

  end function sio_openw




  !> 
  !! Open H5Block file for read.
  !! We assume mpiio is far better option on most systems today.
  !! Assuming 1M stripe
  !! Currently ignores the step under the assumption that there is one
  !! step per file and as per VisIt requirement, it is always 0 (c indexing)
  !<
  integer*8 function sio_openr(filename, comm, istep)  result(ifile)
     implicit none

     character(*), intent(in) :: filename
     integer,intent(in)       :: comm
     integer,intent(in)       :: istep

#ifdef USE_H5HUT
     include "mpif.h"
     include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     integer*8 istat
     double precision tbeg
     integer*8 step8

     istat = -1
#ifdef USE_H5HUT
     tbeg = ga_wtime()

     step8 = 1   ! always use 1 within a single file or visit will reject it (fortran indexing)

     !istat = h5pt_set_verbosity_level (4_8);
     ifile = h5pt_openr_par_align(trim(filename), comm, 1048576_8, "fs_lustre") 
     if (ifile .gt. 0) then
        !define local processors layout
        istat = h5bl_define3dlayout(ifile,ixmin8,ixmax8,iymin8,iymax8,izmin8,izmax8)
        if (istat /= 0) then
           ifile = - abs(istat)
        endif
        istat = h5pt_setstep(ifile,step8)
        if (istat /= 0) then
           ifile = - abs(istat)
        endif
     endif
     sio_time_open = sio_time_open + ga_wtime() - tbeg
#endif

  end function sio_openr




  !> 
  !! Close H5BLock file
  !<
  integer function sio_close()  result(istat)
     implicit none

#ifdef USE_H5HUT
     include "mpif.h"
     include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     double precision tbeg

     istat = -1

     tbeg = ga_wtime()

#ifdef USE_H5HUT
     istat = h5pt_close(sio_curfileid)
#endif
     sio_curfileid = -1

     sio_time_close = sio_time_close + ga_wtime() - tbeg

  end function sio_close

  !> 
  !! Close H5BLock file
  !<
  integer function sio_close_file(fid)  result(istat)
     implicit none

#ifdef USE_H5HUT
     include "mpif.h"
     include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     integer*8,intent(in) :: fid

     double precision tbeg

     istat = -1

     tbeg = ga_wtime()

#ifdef USE_H5HUT
     istat = h5pt_close(fid)
#endif
     sio_curfileid = -1

     sio_time_close = sio_time_close + ga_wtime() - tbeg

  end function sio_close_file



  !<  Routine to remove "/" characters from title strings and replace them with
  !!  "_". This prevents problems in HDF5 when outputs are named things like
  !! Rock Soil/Zonation for example.
  subroutine sio_purify(str)
    implicit none

    character(len=*) str
    integer i, slen
    slen = len_trim(str)
    do i = 1, slen
      if (str(i:i).eq.'/') str(i:i) = '_'
    end do
  end subroutine sio_purify



!-----------------------------------  Write Methods --------------------------------------



   !< Write variable units as an attribute string to H5BLOCK external file
   !! We will write units of the form
   !!   <varname::units <units>
   !! @param scalr_name: name of scalar variable
   !! @param units: string that represents the units
   !! @param ok: returns true if operation successful
   integer function sio_write_units(varname, units) result(istat)
     implicit none
#include "mpif.h"  
#ifdef USE_H5HUT
#include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     character(len=*),intent(in) :: varname
     character(*),intent(in) :: units

     double precision tbeg
     integer me
   
#ifdef USE_H5HUT
     call checkfid()

     tbeg = ga_wtime()
   
     istat = h5pt_writefileattrib_string(sio_curfileid, trim(varname)//"::units", units)
     call sio_check(istat, 'Error: sio_write_units, istat: '//trim(varname))

     sio_time_write = sio_time_write + ga_wtime() - tbeg
#endif

   end function sio_write_units





   !< Write an arbitrary string attribute string to H5BLOCK external file
   !! @param name: name of attribute
   !! @param value - value of string attribute
   !! @param ok: returns true if operation successful
   integer function sio_write_string(name, value) result(istat)
     implicit none
#include "mpif.h"  
#ifdef USE_H5HUT
#include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     character(len=*),intent(in) :: name
     character(*),intent(in) :: value

     double precision tbeg
   
#ifdef USE_H5HUT
     call checkfid()

     tbeg = ga_wtime()
   
     istat = h5pt_writefileattrib_string(sio_curfileid, trim(name), value)
     call sio_check(istat, 'sio_write_string'//trim(name)//'='//trim(value))

     sio_time_write = sio_time_write + ga_wtime() - tbeg
#endif

   end function sio_write_string



   !< Write scalar attributes to H5BLOCK external file
   !! @param scalr_name: name of scalar 
   !! @param isca_data: scalar integer data value 
   !! @param ok: returns true if operation successful
   integer function sio_write_scalar_int(scalr_name, isca_data) result(istat)
     implicit none
#include "mpif.h"  
#ifdef USE_H5HUT
#include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     character(len=*) scalr_name
     character(len=128) var_string, sca_string, tsca_string
     integer slen
     integer isca_data
     double precision tbeg
   
#ifdef USE_H5HUT
     call checkfid()

     tbeg = ga_wtime()
     var_string = trim(scalr_name)
   
     write (sca_string, '(i8)') isca_data
     tsca_string = trim(sca_string)
   
     istat = h5pt_writefileattrib_string(sio_curfileid, var_string, tsca_string)
     call sio_check(istat, 'sio_write_scalar_int'//trim(var_string)//'='//trim(tsca_string))

     sio_time_write = sio_time_write + ga_wtime() - tbeg
#endif

   end function sio_write_scalar_int



   ! Write scalar attributes to H5BLOCK external file
   ! @param scalr_name: name of scalar 
   ! @param dsca_data: scalar double precision data value 
   ! @param ok: returns true if operation successful
   !
   integer function sio_write_scalar_dbl(scalr_name, dsca_data) result(istat)
     use grid_mod
     implicit none
#include "mpif.h"  
#ifdef USE_H5HUT
#include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     character(len=*) scalr_name
     character(len=128) var_string, sca_string, tsca_string
     double precision dsca_data
     double precision tbeg
   
#ifdef USE_H5HUT
     call checkfid()

     tbeg = ga_wtime()
     var_string = trim(scalr_name)

     write (sca_string, '(g12.5)') dsca_data
     tsca_string = trim(sca_string)
   
     istat = h5pt_writefileattrib_string(sio_curfileid, var_string, tsca_string)
     call sio_check(istat, 'sio_write_scalar_dbl'//trim(var_string)//'='//trim(tsca_string))

     sio_time_write = sio_time_write + ga_wtime() - tbeg

#endif

   end function sio_write_scalar_dbl





   !<
   !! Write array.  
   !! This method is meant to write any type of stomp dimensioned data, both integer and float.
   !! Retain same style as existing serial IO.
   !! 
   integer function sio_write_field(varname,iflg,dflg,idx,dim,dim1,dim2,scal,avar) result(rstat)
     use grid_mod
     implicit none
#include "mpif.h"  
#ifdef USE_H5HUT
#include "H5PartF.h"
#endif
#include "mafdecls.fh"
#include "global.fh"
     character(*), intent(in)     :: varname
     integer,intent(in)           :: iflg      
     integer,intent(in)           :: dflg      
     integer                      :: idx      
     integer                      :: dim
     integer                      :: dim1
     integer                      :: dim2    ! if 3d
     double precision, intent(in) :: scal    
     double precision, intent(in) :: avar   

     character(len=128) title,species,species_name
     double precision offset
     double precision, pointer :: d_3data(:,:,:), d_2data(:,:),d_data(:)
     integer, pointer          :: i_2data(:,:), i_data(:)
     integer, pointer :: i_3data(:,:,:)
     integer*8 ivar8       ! for hdf5 type conversion
     double precision tbeg
     integer tmp_dim1, tmp_dim2
   
     rstat = 0
   
#ifdef USE_H5HUT
     call checkfid()

     ! prepare and write files in HDF5 format 
     tbeg = ga_wtime()
     title = trim(varname)  ! don't modify passed parameter
     call sio_purify(title)
     if (iflg.eq.1) then
       if (dim == 1) then
         i_data => i_nd_fld(idx)%p
         call sio_int_copy2d_to_buf(i_data,dim,dim1)
       elseif (dim == 2) then
         i_2data => i_nd_2fld(idx)%p
         tmp_dim1 = i_nd_2dim1(idx)
         call sio_int_copy2d_to_buf(i_2data,dim,tmp_dim1)
       elseif (dim == 3) then
         i_3data => i_nd_3fld(idx)%p
         tmp_dim1 = i_nd_3dim1(idx)
         tmp_dim2 = i_nd_3dim2(idx)
         call sio_int_copy3d_to_buf(i_3data,idx,dim,tmp_dim1,tmp_dim2)
       else
          write(*,*) "Warning: unknown integer variable of dimension",trim(varname),dim1
       endif
       rstat = h5bl_3d_write_scalar_field_i8(sio_curfileid, title, i8buf(1,1,1))
     elseif (dflg.eq.1) then
       !TODO why is dim1 being modified?
       if (dim == 1) then
         d_data => d_nd_fld(idx)%p
         call sio_dbl_copy1d_to_buf(d_data(1:),scal,avar)
       elseif(dim == 2) then 
         d_2data => d_nd_2fld(idx)%p
         tmp_dim1 = d_nd_2dim1(idx)
         call sio_dbl_copy2d_to_buf(d_2data(1:,1:),dim,tmp_dim1,scal,avar)
       elseif(dim == 3) then 
         d_3data => d_nd_3fld(idx)%p
         tmp_dim1 = d_nd_3dim1(idx)
         tmp_dim2 = d_nd_3dim2(idx)
         call sio_dbl_copy3d_to_buf(d_3data(1:,1:,1:),idx,dim,tmp_dim1,tmp_dim2,scal,avar)
       else
          write(*,*) "Warning: unknown integer variable of dimension",trim(varname),dim1
       endif
       rstat = h5bl_3d_write_scalar_field_r8(sio_curfileid, title, r8buf(1,1,1))
     endif
     if (rstat.ne.0) then
       write(6,'(a,a,i4)') 'In sio_write_2d_field_to_file, field, rstat: ', title, rstat
       return
     endif
#ifndef HAVE_NEWAPI
     rstat = h5bl_3d_set_field_origin(sio_curfileid,title,x_orig,y_orig,z_orig)
     rstat = h5bl_3d_set_field_spacing(sio_curfileid,title,dx,dy,dz)
#endif

#if defined(H5PART_1_6_7) && !defined(HAVE_NEWAPI)
     !TODO dynamically decide if this should be written
     ivar8 = nxdim
     rstat = h5bl_3d_set_field_xcoords(sio_curfileid, title, d_xc(1), ivar8)
     ivar8 = nydim
     rstat = h5bl_3d_set_field_ycoords(sio_curfileid, title, d_yc(1), ivar8)
     ivar8 = nzdim
     rstat = h5bl_3d_set_field_zcoords(sio_curfileid, title, d_yc(1), ivar8)
#endif

#endif
     sio_time_write = sio_time_write + ga_wtime() - tbeg

   end function sio_write_field





!-----------------------------------  Read Methods --------------------------------------

  !<  Read ijk double field from an hdf5 file.
  !!  Currently assumes the field name is always "ijkdata" which corresponds to what 
  !!  the ijk to hdf5 tool writes.
  !!  Assumes local grid is defined ahead of time and set in the open call.
  !!
  integer function sio_read_ijkd(fid,buf,ix,iy,iz) result(istat)
     implicit none

     integer,intent(in) :: ix,iy,iz
     integer*8,intent(in) :: fid
     double precision buf(ix,iy,iz)

#ifdef USE_H5HUT
#include "H5PartF.h"
#include "mafdecls.fh"
#include "global.fh"


     integer*8 istat8
     integer*8 type, nelem
     character(len=32) name
     double precision :: tbeg

     tbeg = ga_wtime()

     call check_dims(ix,iy,iz)

     ! TODO if type is integer, read it and convert automatically
     ! This code is not in the fortran api - sent inquiry to M. Howison 3/11?
     !istat8 = H5PartGetDatasetInfo(fid, 0, name, 32, type, nelem)
     

     istat8 = h5bl_3d_read_scalar_field_r8(fid, "ijkdata", buf(1,1,1))
     istat = istat8

     sio_time_read = sio_time_read + ga_wtime() - tbeg
#else
     istat = -1
#endif

  end function sio_read_ijkd


  !<  Read ijk integer field from an hdf5 file.
  !!  Currently assumes the field name is always "ijkdata" which corresponds to what 
  !!  the ijk to hdf5 tool writes.
  !!  Assumes local grid is defined ahead of time and set in the open call.
  !!
  integer function sio_read_ijki(fid,buf,ix,iy,iz) result(istat)
     implicit none

     integer,intent(in) :: ix,iy,iz
     integer*8,intent(in) :: fid
     integer :: buf(ix,iy,iz)

#ifdef USE_H5HUT

#include "H5PartF.h"
#include "mafdecls.fh"
#include "global.fh"

     integer*8 istat8
     double precision :: tbeg

     tbeg = ga_wtime()

     call check_dims(ix,iy,iz)
     istat8 = h5bl_3d_read_scalar_field_i4(fid, "ijkdata", buf)
     istat = istat8

     sio_time_read = sio_time_read + ga_wtime() - tbeg
#else
     istat = -1
#endif

  end function sio_read_ijki



  !<
  !! Verify that the read buffer dims are the same as the grid size.
  !! This is just to guard against the common problem of using a buffer
  !! dimensioned by ghost dims rather than the procs actual mesh chunk.
  !! The file does not contain ghosts.  ghost updates must be done after
  !! the read.
  !>
  subroutine check_dims(ix,iy,iz)
     use grid_mod
     implicit none

     integer,intent(in) :: ix,iy,iz


     if (ix /= (ixmax-ixmin+1) .or.  iy /= (iymax-iymin+1) .or.  &
        iz /= (izmax-izmin+1)) then
        print *, "Error, incorrect array dimensions for read call"
     endif
  end subroutine check_dims




!-----------------------------------  Copy Methods ---------------------------------------

  !<  Copy node data from grid buffer into a local buffer
  !!  @param data: grid field data
  !!  @param scal: scale value for field
  !!  @param offset: ???
  !!
  subroutine sio_dbl_copy1d_to_buf(data,scal,offset)
     use grid_mod
     implicit none

     double precision data(ldx,ldy,ldz),scal,offset
     integer xoff, yoff, zoff
     integer ix, iy, iz, i, j, k
     xoff = ixmin - iaxmin
     yoff = iymin - iaymin
     zoff = izmin - iazmin
     do k = 1, ldimz
       iz = k + zoff
       do j = 1, ldimy
         iy = j + yoff
         do i = 1, ldimx
           ix = i + xoff
           r8buf(i,j,k) = scal * data(ix,iy,iz) + offset
         end do
       end do
     end do
     mbytes_write = mbytes_write + DBL_SIZE*ldimx*ldimy*ldimz

  end subroutine sio_dbl_copy1d_to_buf




  !<  
  !
  subroutine sio_int_copy1d_to_buf(data)
    use grid_mod
    implicit none
    integer data(ldx,ldy,ldz)
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          i8buf(i,j,k) = data(ix,iy,iz)
        end do
      end do
    end do
    mbytes_write = mbytes_write + INT_SIZE*ldimx*ldimy*ldimz
  end subroutine sio_int_copy1d_to_buf


  !<  Copy node data from grid buffer into a local buffer
  !!  @param data: grid field data
  !!  @param idx: index of variable representing slice that is
  !!              to be written.
  !!  @param dim1: dimension of variable
  !!  @param scal: scale value for field
  !!  @param offset: ???
  !!
  subroutine sio_dbl_copy2d_to_buf(data,idx,dim1,scal,offset)
    use grid_mod
    implicit none
    integer idx, dim1
    double precision data(dim1,ldx,ldy,ldz),scal,offset
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          r8buf(i,j,k) = scal * data(idx,ix,iy,iz) + offset
        end do
      end do
    end do
    mbytes_write = mbytes_write + DBL_SIZE*ldimx*ldimy*ldimz
  end subroutine sio_dbl_copy2d_to_buf




  !
  !  Copy node data from grid buffer into a local buffer
  !  @param data: grid field data
  !  @param idx: index of variable representing slice that is
  !              to be written.
  !  @param dim1: dimension of second
  !
  subroutine sio_int_copy2d_to_buf(data,idx,dim1)
    use grid_mod
    implicit none
    integer idx, dim1
    integer data(dim1,ldx,ldy,ldz)
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          i8buf(i,j,k) = data(idx,ix,iy,iz)
        end do
      end do
    end do
    mbytes_write = mbytes_write + INT_SIZE*ldimx*ldimy*ldimz
  end subroutine sio_int_copy2d_to_buf



  !  Copy node data from grid buffer into a local buffer
  !  @param data: grid field data
  !  @param idx1: index of first variable representing slice that is
  !              to be written.
  !  @param idx2: index of second variable representing slice that is
  !              to be written.
  !  @param dim1: dimension of first variable
  !  @param dim2: dimension of second variable
  !  @param scal: scale value for field
  subroutine sio_dbl_copy3d_to_buf(data,idx1,idx2,dim1,dim2,scal,offset)
    use grid_mod
    implicit none
    integer idx1, idx2, dim1, dim2
    double precision data(dim1,dim2,ldx,ldy,ldz),scal,offset
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          r8buf(i,j,k) = scal * data(idx1,idx2,ix,iy,iz) + offset
        end do
      end do
    end do
    mbytes_write = mbytes_write + DBL_SIZE*ldimx*ldimy*ldimz
  end subroutine sio_dbl_copy3d_to_buf






  !< Copy node data from grid buffer into a local buffer
  !  @param data: grid field data
  !  @param idx1: index of first variable representing slice that is
  !              to be written.
  !  @param idx2: index of second variable representing slice that is
  !              to be written.
  !  @param dim1: dimension of first variable
  !  @param dim2: dimension of second variable
  subroutine sio_int_copy3d_to_buf(data,idx1,idx2,dim1,dim2)
    use grid_mod
    implicit none
    integer idx1, idx2, dim1, dim2
    integer data(dim1,dim2,ldx,ldy,ldz)
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          i8buf(i,j,k) = data(idx1,idx2,ix,iy,iz)
        end do
      end do
    end do
    mbytes_write = mbytes_write + INT_SIZE*ldimx*ldimy*ldimz
  end subroutine sio_int_copy3d_to_buf



  !
  !  Copy node data from a local buffer into grid buffer
  !  @param data: grid field data
  !  @param idx: index location of data to be read
  !  @param dim1: dimension of variable
  !
  subroutine sio_dbl_copy1d_from_buf(data)
    use grid_mod
    implicit none
    integer idx, dim1
    double precision data(ldx,ldy,ldz)
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          data(ix,iy,iz) = r8buf(i,j,k)
        end do
      end do
    end do
  end subroutine sio_dbl_copy1d_from_buf




  !  Copy node data from a local buffer into grid buffer
  !  @param data: grid field data
  !  @param idx: index location of data to be read
  !  @param dim1: dimension of variable
  !
  subroutine sio_dbl_copy2d_from_buf(data,idx,dim1)
    use grid_mod
    implicit none
    integer idx, dim1
    double precision data(dim1,ldx,ldy,ldz)
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          data(idx,ix,iy,iz) = r8buf(i,j,k)
        end do
      end do
    end do
  end subroutine sio_dbl_copy2d_from_buf



  !  Copy node data from a local buffer into grid buffer
  !  @param data: grid field data
  !  @param idx: index location of data to be read
  !  @param dim1: dimension of variable
  subroutine sio_int_copy2d_from_buf(data,idx,dim1)
    use grid_mod
    implicit none
    integer idx, dim1
    integer data(dim1,ldx,ldy,ldz)
    integer xoff, yoff, zoff
    integer ix, iy, iz, i, j, k
    xoff = ixmin - iaxmin
    yoff = iymin - iaymin
    zoff = izmin - iazmin
    do k = 1, ldimz
      iz = k + zoff 
      do j = 1, ldimy
        iy = j + yoff
        do i = 1, ldimx
          ix = i + xoff
          data(idx,ix,iy,iz) = i8buf(i,j,k)
        end do
      end do
    end do
  end subroutine sio_int_copy2d_from_buf

  !< Check status return value, print message if its an error and die if fatal
  !!>
  subroutine sio_check(istat,msg)
  implicit none
#include "mpif.h"  
#include "mafdecls.fh"
#include "global.fh"
  integer,intent(in)      :: istat    ! status to check
  character(*),intent(in) :: msg     ! message to print if there is an error
  integer me
  integer ierr

  if (istat /= 0) then
     me = ga_nodeid()
     if (me == 0) then
       write(*,*) "Error in sio: ",msg
     endif
     call MPI_ABORT(MPI_COMM_WORLD,istat,ierr)
  endif

  end subroutine sio_check



  !< Check internal file handle.
  !!>
  subroutine checkfid()
  implicit none
#include "mpif.h"  
#include "mafdecls.fh"
#include "global.fh"
  integer me
  integer ierr

  if (sio_curfileid .eq. -1) then
     me = ga_nodeid()
     if (me == 0) then
       write(6,*) "Invalid filehandle",sio_curfileid
     endif
     call MPI_ABORT(MPI_COMM_WORLD,sio_curfileid,ierr)
  endif

  end subroutine checkfid




END MODULE SIO
