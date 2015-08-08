! Copyright 2015 Dennis Decker Jensen
!
! Three typical homwwork problems from Programming Praxis
!
! http://programmingpraxis.com/2015/08/04/three-homework-problems/
!
! Tectonics: gfortran -Wall -Wextra -std=f2008ts homework.f08
!
!   1. Write a function that takes as input three positive
!      integers and finds the sum of the squares of the two
!      largest of the three
!
!    2. Write a function that a positive integer as input and
!       determines if it is a base-10 palindrome
!
!    3. Write a function that takes a positive integer as input and
!       determines the number of trailing zeroes in the output of
!       that number's factorial.
!
program homework
   implicit none

   write (*,10) 'one(2,3,4) = ', one(2,3,4)
   write (*,10) 'one(2,4,3) = ', one(2,4,3)
   write (*,10) 'one(3,2,4) = ', one(3,2,4)
   write (*,10) 'one(3,4,2) = ', one(3,4,3)
   write (*,10) 'one(4,2,3) = ', one(4,2,3)
   write (*,10) 'one(4,3,2) = ', one(4,3,2)
10 format (A,I2)

   write (*,20) 'two(121)        = ', two(121)
   write (*,20) 'two(333)        = ', two(333)
   write (*,20) 'two(433)        = ', two(433)
   write (*,20) 'two(3333)       = ', two(3333)
   write (*,20) 'two(3433)       = ', two(3433)
   write (*,20) 'two(32)         = ', two(32)
   write (*,20) 'two(12344321)   = ', two(12344321)
   write (*,20) 'two(11)         = ', two(11)
   write (*,20) 'two(1239321)    = ', two(1239321)
   write (*,20) 'two(1239999321) = ', two(1239999321)
   write (*,20) 'two(0012332100) = ', two(0012332100)
   write (*,20) 'two(0)          = ', two(0)
20 format (A,L1)

   write (*,30) 'tree(9)    = ', three(9)
   write (*,30) 'tree(11)   = ', three(11)
   write (*,30) 'tree(27)   = ', three(27)
   write (*,30) 'tree(35)   = ', three(35)
   write (*,30) 'tree(80)   = ', three(80)
   write (*,30) 'tree(1200) = ', three(1200)
30 format (A,I3)

contains

   ! Square sum of 2 largest numbers
   integer function one(x, y, z)
      integer, intent(in) :: x, y, z
      integer hold, a, b, c

      a = x; b = y; c = z
      if (a < b) then
         hold = a
         a = b
         b = hold
      end if
      if (b < c) then
         hold = b
         b = c
         c = hold
      end if
      one = a * a + b * b
   end function one

   ! Is the number a palindrome?
   ! Examples of number palindroms:
   !    11, 121, 1239321, 1239999321, 0, 0012332100
   logical function two(num)
      integer, intent(in) :: num
      integer a, b, m

      ! Drop trailing zeroes
      a = num; m = 0
      if (num > 0) then
         do while(m == 0)
            m = modulo(a, 10)
            a = a / 10
         end do
      end if

      ! Move digits one by one into b
      b = m
      do while(a > b)
         m = modulo(a, 10)
         a = a / 10
         b = b * 10 + m
      end do

      two = a == b .or. a == b / 10
   end function two

   ! Number of trailing zeroes in factorial(n)
   integer function three(n)
      integer, intent(in) :: n
      integer f

      three = 0; f = 5
      do while(f <= n)
         three = three + n / f
         f = f * 5
      end do
   end function three

end program homework
