SUBROUTINE sort(n,a)
 implicit none
 INTEGER n
 integer a(n)
!Sorts an array a(1:n) into ascending numerical order by Shell’s method (diminishing increment
!sort). n is input; a is replaced on output by its sorted rearrangement.
 INTEGER i,j,inc
 integer v
 inc=1 !Determine the starting increment.
 1 inc=3*inc+1
 if(inc.le.n)goto 1
 2 continue !Loop over the partial sorts.
 inc=inc/3
 do 11 i=inc+1,n !Outer loop of straight insertion.
  v=a(i)
  j=i
  3 if(a(j-inc).gt.v)then !Inner loop of straight insertion.
   a(j)=a(j-inc)
   j=j-inc
   if(j.le.inc)goto 4
   goto 3
  endif
  4 a(j)=v
11 continue
 if(inc.gt.1)goto 2
 return
!
END
