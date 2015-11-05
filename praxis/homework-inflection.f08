! Copyright 2015 Dennis Decker Jensen
!
! Two small homwwork problems from Programming Praxis
!
! http://programmingpraxis.com/2015/08/21/two-homework-problems/
!
! Tectonics: gfortran -g -Wall -Wextra -std=f2008ts homework-inflection.f08
!
! 1. Find the inflection point of an array
! 2. Write the last N lines of a file.

program homework_inflection
   implicit none
   integer a(7)

   a = (/ 3, 7, 9, 8, 2, 5, 6 /)
   print '((A),*(I2))', 'Inflection point of array:', a
   print '("1:",2I4)', inflection1(a)
   print '("2:",2I4)', inflection2(a)

   print *, "Tail of file:"
   call tail('color', 10)

contains

   function inflection1(a) result(res)
      integer, dimension(:), intent(in) :: a
      integer res(2)
      integer b, e, bsum, esum

      b = 1; e = size(a); bsum = 0; esum = 0
      do while (b <= e)
         if (bsum < esum) then
            bsum = bsum + a(b)
            b = b + 1
         else
            esum = esum + a(e)
            e = e - 1
         end if
      end do
      res = (/ b - 1, abs(esum - bsum) /)
   end function
 
   function inflection2(a) result(res) ! Slow and easy
      integer, dimension(:), intent(in) :: a
      integer res(2)
      integer m(size(a))
      integer b(1), i

      m = (/ (abs(sum(a(:i)) - sum(a(i + 1:))), i = 1, size(a)) /)
      !print '(*(I3))', m
      b = minloc(m); b = b(1)
      res = (/ b, m(b) /)
   end function

   subroutine tail(filename, nlines)
      use, intrinsic :: iso_fortran_env, only: iostat_end
      implicit none
      character(len=*), intent(in) :: filename
      integer, intent(in) :: nlines
      integer :: u, j, ios, rlines = 0, i = 0
      character(len=79) msg
      character(len=100) buffer(nlines)

      open(newunit=u, file=filename, action='read', iostat=ios, iomsg=msg)
      if (ios .ne. 0) then
         print *, "Error opening '", filename, "'; error code: ", ios
         print *, msg
      end if

      do
         i = modulo(i + 1, nlines)
         read (unit=u, iostat=ios, iomsg=msg, fmt='(A100)') buffer(i)
         if (ios .ne. 0) exit
         rlines = rlines + 1
      end do

      if (ios .ne. iostat_end) then
         print *, 'Error reading "', filename, '"; error code: ', ios
         print *, msg
      else
         do j = 1, min(nlines, rlines)
            print *, trim(buffer(i))
            i = modulo(i + 1, nlines)
         end do
      end if
      close(u)
   end subroutine
   
end program homework_inflection
