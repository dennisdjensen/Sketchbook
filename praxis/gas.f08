! Copyright 2015 Dennis Decker Jensen
! The gas station problem. A classic computer sicence problem.
! http://programmingpraxis.com/2015/07/17/the-gas-station-problem/

! There is a circular route for a truck.
! The truck has a gas tank with infinite capacity.
! Along the route are gas stations with varying amount of gas
! available: You are given a list of gas stations with amounts
! of gas available, and another list with gas required to go
! to the next gas station. You must find the start gas station
! that makes it possible to complete a circular trip back to start
! without the truck running out of gas.
program gas
   implicit none
   integer, dimension(8) :: trip, gasoline, distance
   integer start

   gasoline = (/15, 08, 02, 06, 18, 09, 21, 30/)
   distance = (/08, 06, 30, 09, 15, 21, 02, 18/)
   call station(gasoline, distance, trip, start)
   print 100, start, trip
   call station2(gasoline, distance, start)
   print '(A,I2)', "Trip start (solution 2): ", start

   gasoline(8) = gasoline(8) - 1 ! Make trip impossible
   call station(gasoline, distance, trip, start)
   print 100, start, trip
   call station2(gasoline, distance, start)
   print '(A,I2)', "Trip start (solution 2): ", start
100 format ('Station start: ',I2,'; trip:',8I4)

contains

   ! Take the cumulative sum of the excess fuel.
   ! The solution is the station after the station
   !  where the excess fuel is the most negative.
   subroutine station(gas, distance, trip, start)
      integer, intent(in) :: gas(:), distance(size(gas))
      integer, intent(out) :: trip(size(gas)), start
      integer i, m(1)

      trip = gas - distance
      call accumulate(trip)

      if (trip(size(trip)) < 0) then
         start = 0 ! There is not enough fuel for the trip
         ! Leave trip alone for debugging
      else
         m = minloc(trip)
         start = modulo(m(1) + 1, size(trip)) ! Right after the least
         trip = gas - distance
         trip = (/trip(start:), trip(1:start - 1)/)
         call accumulate(trip)
      end if
   end subroutine station

   subroutine accumulate(a)
      integer a(:), i
      do i = 2, size(a)
         a(i) = a(i) + a(i - 1)
      end do
   end subroutine accumulate

   ! Sum up the addition of excess fuel,
   ! and move start backwards if there is not enough gas in tank.
   subroutine station2(gas, distance, start)
      integer, intent(in) :: gas(:), distance(size(gas))
      integer, intent(out) :: start
      integer i, tank

      start = size(gas) + 1
      i = 1
      tank = 0
      do while (i < start)
         if (tank + gas(i) >= distance(i)) then
            tank = tank + gas(i) - distance(i)
            i = i + 1
         else
            start = start - 1
            tank = tank + gas(start) - distance(start)
         end if
      end do
      if (tank >= 0) then
         start = modulo(start, size(gas))
      else
         start = 0
      end if
   end subroutine station2
end program gas

