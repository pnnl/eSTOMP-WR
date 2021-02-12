!! Module to use an in memory buffer to process a input file comprised of "cards" where
!! cards are defined as starting with ~.
!!
!! The file is read in on the head node and distributed to all processors.
!! A MAX_LINES is enforced via MAX_LINES
!! Only one file can be processed at a time.
!! Usage:
!!    After mpi is initialized,
!!       bufferedread_init
!!       bufferedread_find()   seek to string (usually a card name)
!!       bufferedread_getline
!!       bufferedread_term
!!       
!>

module bufferedread

  implicit none

  integer MAX_CARDS, MAX_STRLEN, CARD_BUFLEN, MAX_LINES
  parameter (MAX_CARDS=100, MAX_STRLEN=1024, CARD_BUFLEN=1000)
  character(MAX_STRLEN), allocatable :: input_lines(:)
  character(MAX_STRLEN), allocatable :: extfile(:)
  character(MAX_STRLEN) card_name(MAX_CARDS)
  integer num_cards, num_lines(MAX_CARDS)
  integer first_line(MAX_CARDS)
  integer current_card, current_line

  private MAX_CARDS, MAX_STRLEN, CARD_BUFLEN, MAX_LINES
  private input_lines
  private extfile
  private card_name
  private num_cards, num_lines
  private first_line
  private current_card, current_line

contains

   !< 
   !! Method:  bufferedread_init
   !! read in input file
   !! @param t_filename: name of input file
   !! @returns true if file is found
   !> 
   logical function bufferedread_init(t_filename) result(t_ok)

#include "global.fh"

     character (len=*) t_filename
     character(MAX_STRLEN) newline
     logical t_read, ok
     integer ierr, i, i_ok, me, nlines, nlen, icnt

     ! Just in case somebody really needs lots of lines
     MAX_LINES = 200000
#ifdef MAXLINES
     MAX_LINES = MAXLINES
#endif
   
     me = ga_nodeid()
     num_cards = 0
     nlines = 0
     num_lines = 0
     first_line = 0
     i_ok = 0
     icnt = 0
     allocate(input_lines(MAX_LINES))
     t_ok = .false.

     !  Scan through file and find all input blocks
     if (me.eq.0) then
       open(unit=5,file=t_filename,iostat=ierr,status='old')
       t_read = .false.
       if (ierr.eq.0) i_ok = 1
       do while (ierr.eq.0)
         read(5,'(a)',iostat=ierr) newline
         icnt = icnt + 1
         if (icnt .gt. MAX_LINES) then
            call ga_error('Maximum number of lines in input file exceeded. You can set it in the makefile.',MAX_LINES)
         endif

         if (ierr.eq.0) then
            call bufferedread_rmwhtspc(newline)
            if (newline(1:1).eq.'~') then
               if (num_cards.gt.0) then
                  num_lines(num_cards) = nlines
               endif
               call bufferedread_lcase(newline)
               num_cards = num_cards + 1
!              newline(1:1) = ' '
               call bufferedread_rmwhtspc(newline)
               card_name(num_cards)(1:) = newline
               input_lines(icnt)(1:) = newline
               first_line(num_cards) = icnt
               t_read = .true.
               nlines = 1
            else if (t_read) then
               nlines = nlines + 1
               input_lines(icnt) = newline
            endif
         else
            if (num_cards.gt.0) then
               num_lines(num_cards) = nlines
            endif
         endif
       end do
       close(5)

       !  finish broadcasting data to other processes
       call ga_igop(1, i_ok, 1, '+')
       call ga_igop(2, num_cards, 1, '+')
       call ga_igop(3, num_lines, num_cards, '+')
       call ga_igop(4, first_line, num_cards, '+')
       do i = 1, num_cards
         nlen = MAX_STRLEN*num_lines(i)
         call ga_brdcst(i,input_lines(first_line(i)),nlen,0)
       end do
       nlen = MAX_STRLEN*num_cards
       call ga_brdcst(1,card_name(1),nlen,0)
     else
       call ga_igop(1, i_ok, 1, '+')
       call ga_igop(2, num_cards, 1, '+')
       call ga_igop(3, num_lines, num_cards, '+')
       call ga_igop(4, first_line, num_cards, '+')
       do i = 1, num_cards
         nlen = MAX_STRLEN*num_lines(i)
         call ga_brdcst(i,input_lines(first_line(i)),nlen,0)
       end do
       nlen = MAX_STRLEN*num_cards
       call ga_brdcst(1,card_name(1),nlen,0)
     endif

     if (i_ok.gt.0) t_ok = .true.

   end function bufferedread_init



   !< 
   !! Find input corresponding to a particular input field
   !! @param t_field: field or card name
   !! @returns false if target string not found
   !> 
   logical function bufferedread_find(t_field) result(t_ok)
     implicit none
     character (len=*) t_field
     integer idx, nlen
     character (512) field

     ! Rather than a reset call, always start over
     current_card = 0

     idx = 0
     t_ok = .false.
     field = t_field
     call bufferedread_lcase(field)
     call bufferedread_rmwhtspc(field)
     nlen = len_trim(field)
     current_card = 0
     current_line = 0
     do while (idx.lt.num_cards)
       idx = idx + 1
       if (field(1:nlen).eq.card_name(idx)(1:nlen)) then
         current_card = idx
         idx = num_cards
       endif
     end do
     if (current_card.gt.0) t_ok = .true.

   end function bufferedread_find



   !< 
   !! Get next line of input
   !! @param t_line: next line of input in file
   !! @param t_ok: returns true if next line found
   !>
   logical function bufferedread_getline(t_line) result(t_ok)
     implicit none
     character (len=*) t_line
     integer idx

     t_ok = .false.
     if (current_card.gt.0) then
       current_line = current_line + 1
       idx = first_line(current_card) + current_line
       do while(current_line.lt.num_lines(current_card).and. &
                (input_lines(idx)(1:1).eq.'#'.or. &
                input_lines(idx)(1:1).eq.'!'))
         current_line = current_line + 1
         idx = first_line(current_card) + current_line
       end do
       if (current_line.lt.num_lines(current_card)) then
         t_ok = .true.
         t_line = input_lines(idx)
       else
         t_ok = .false.
       endif
     endif
     if (.not.t_ok) t_line(1:) = ""
   end function bufferedread_getline


   !< 
   !! Clean up memory.
   !>
   logical function bufferedread_term() result(t_ok)
      implicit none
      deallocate(input_lines)
      t_ok = .true.
   end function bufferedread_term



   !<
   !! Utility subroutine to remove leading and trailing white space from line
   !>
   subroutine bufferedread_rmwhtspc(t_line)
     implicit none
     character(*) t_line
     integer t_len, i, j, istart
     t_len = len_trim(t_line)
     istart = 0
     i = 0
     if (t_len.eq.0) return
     do while (i.lt.t_len)
       i = i + 1
       istart = i
       if (t_line(i:i).ne.' ') then
         i = t_len
       endif
     end do
     j = 1
     do i = istart, t_len
       t_line(j:j) = t_line(i:i)
       j = j + 1
     end do
     t_len = len(t_line)
     do i = j, t_len
       t_line(i:i) = ""
     end do
   end subroutine bufferedread_rmwhtspc



   !<
   !! Utility routine to change all alphabetical characters in a line to lower
   !! case characters
   !>
   subroutine bufferedread_lcase(chdum)
     implicit none
     character(len=*) chdum
     integer i, nlen, ichng
     nlen = len_trim(chdum)
     ichng = ichar('a') - ichar('A')
     do i = 1, nlen
       if (ichar(chdum(i:i)).ge.ichar('A').and. &
           ichar(chdum(i:i)).le.ichar('Z')) then
         chdum(i:i) = char(ichar(chdum(i:i)) + ichng)
       endif
     end do
   end subroutine bufferedread_lcase




   !<
   !! Utility routine to change all alphabetical characters in a line to upper
   !! case characters
   !>
   subroutine bufferedread_ucase(chdum)
     implicit none
     character(len=*) chdum
     integer i, nlen, ichng
     nlen = len_trim(chdum)
     ichng = ichar('A') - ichar('a')
     do i = 1, nlen
       if (ichar(chdum(i:i)).ge.ichar('a').and. &
           ichar(chdum(i:i)).le.ichar('z')) then
         chdum(i:i) = char(ichar(chdum(i:i)) + ichng)
       endif
     end do
   end subroutine bufferedread_ucase


   !< 
   !! Read number of lines in external input file
   !! Data is not preserved.  We are just counting lines.
   !>
   integer function bufferedread_nlines(t_filename) result(number_lines)

      implicit none

#include "global.fh"

      character (len=*) t_filename
      character(MAX_STRLEN) newline
      logical t_read, ok
      integer ierr, i, i_ok, me
      me = ga_nodeid()
      ierr = 0
      number_lines = 0
      if (me.eq.0) then
         open(unit=27,file=t_filename,iostat=ierr,status='old',form='formatted')
         t_read = .false.
         if (ierr.eq.0) i_ok = 1
         do while (ierr.eq.0)
           read(27,'(a)',iostat=ierr) newline
           number_lines = number_lines + 1
         end do
         number_lines = number_lines-1
         close(27)
       endif
       call ga_brdcst(1,number_lines,sizeof(number_lines),0)
       call ga_sync
!      write(*,*) "done readlines", number_lines
   end function bufferedread_nlines


   !< 
   !! Read first line in external input file
   !>
   integer function bufferedread_firstline(t_filename) result(firstline)

      implicit none

#include "global.fh"

      character (len=*) t_filename
      character(MAX_STRLEN) newline
      integer ierr, i, me
      me = ga_nodeid()
      firstline = 0
      if (me.eq.0) then
         open(unit=27,file=t_filename,iostat=ierr,status='old',form='formatted')
         read(27,*,iostat=ierr) firstline
         if (ierr.ne.0) then
           call ga_error('Error reading external input file: ',trim(t_filename))
         endif
         close(27)
       endif
       call ga_brdcst(1,firstline,4,0)
   end function bufferedread_firstline

   !< 
   !! Read first line in external input file
   !>
   logical function bufferedread_extfile(t_filename,iunit,ict) result(t_ok)

      implicit none

#include "global.fh"

      character (len=*) t_filename
      integer ierr, i, me, ict, iunit, nlen

     MAX_LINES = 100000
#ifdef MAXLINES
     MAX_LINES = MAXLINES
#endif
      me = ga_nodeid()
      allocate(extfile(MAX_LINES))
      t_ok = .false.
      ict = 0
      nlen = 0
      ierr = 0
      if (me.eq.0) then
        do while (ierr.eq.0)
          ict = ict + 1
          read(iunit,'(a)',iostat=ierr) extfile(ict)
          if (ict .gt. MAX_LINES) then
             call ga_error('Maximum number of lines in external input file exceeded. You can set it in the makefile.',MAX_LINES)
          endif
        enddo
        ict = ict-1
        nlen = MAX_STRLEN*ict
      endif
      call ga_igop(1,ict,1,'+')
      call ga_igop(2,nlen,1,'+')
      call ga_brdcst(3,extfile,nlen,0)
      if(ierr.eq.0)t_ok = .TRUE.
      if(me.eq.0)close(iunit)
   end function bufferedread_extfile

   !<
   !! Get next line of input
   !! @param t_line: next line of input in file
   !! @param t_ok: returns true if next line found
   !>
   logical function bufferedread_getline_ext(t_line,i,nlines) result(t_ok)
     implicit none
     character (len=*) t_line
     integer nlines,i

     if(i.le.nlines)then
       t_ok = .true.
       t_line = extfile(i)
     else
       t_ok = .false.
       if (.not.t_ok) t_line(1:) = ""
     endif
   end function bufferedread_getline_ext

   logical function bufferedread_term_ext() result(t_ok)
      implicit none
      deallocate(extfile)
      t_ok = .true.
   end function bufferedread_term_ext


end module
