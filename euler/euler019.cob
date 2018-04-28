      * Author: Dennis Decker Jensen
      * Date: 9 July 2015
      * Purpose: Number of Sundays the 1st every month
      *          from 1 Jan 1900 to 31 Dec 2000 inclusive.
      * Tectonics: cobc -x euler019.cob
      * Method:
      *  This was fun. I inspected some really old BASIC programs I did
      *  when in puperty, where this sort of thing was a classic,
      *  and I also got to inspect some of the history of calendars
      *  once more.
      *
      *  This is a lot more complicated than people think. I suspect
      *  most people would use astronomical time calculations, when
      *  a few machine code instructions is all that it takes.
      *
      *  You can still find old publications with this stuff,
      *  e.g. find the old magazines of Astronomical Computing,
      *  Sky & Telescope, in the mid eighties. They published BASIC
      *  programs right up until 1998 inclusive. It is a shame they
      *  don't do that anymore. It was very educational for a whole
      *  generation. You can find the algorithms on the net.
      *  Wikipedia is reliable in a few instances, and in this instance
      *  provides a good enough overview:
      *http://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
      *
      *  Pope Gregor of Rome changed the calendar system annum 1582
      *  from the Julian calendar to the Gregorian calendar.
      *
      *  Different countries modified their calendars at different times
      *  and in different ways. Some days and weeks were taken out of
      *  the calendar in the transition, different for each country.
      *  For example, Denmark did this in 1700, United Kingdom in 1752,
      *  and Sweden in 1755.
      *  If we needed to go further back in time, we could use
      *  the proleptic Gregorian calendar beginning at 01 Jan 0001.
      *
      *  The Gregorian calendar introduced spring days and years.
      *
      *  Since we need to calculate this for the 20th century only,
      *  and do not need astronomical time calculations, but strictly
      *  calendrical calculations, we use the Rata Die (Latin ablative:
      *  "with fixed date") of IBM, instead of Julian days.
      *  This is based on Base = 7N + K, where Base it in days,
      *  N is weeks, and K is the day of the week.
      *  COBOL (and Rexx) has built-in functions for doing this.
      *  OpenCobol/GnuCOBOL has a rata die of 01 Jan 1601 as day 1.
      *  The functions are really simple and easy to do yourself, if
      *  your programming language doesn't provide them.
       identification division.
       program-id. euler019.
       environment division.
       configuration section.
       input-output section.
      ******************************************************************
       data division.
       file section.
       working-storage section.

       01  the-date                pic 9(8) usage is display.
      *    05  the-year            pic 9999 usage is display.
      *    05  the-month           pic 99   usage display.
      *    05  the-day             pic 99   usage display.
       01  date-num.
           05  day-num                 pic 9(6).
           05  week-num                pic 9(6).
           05  day-of-week-num         pic 9.
               88  is-sunday           value zero.
               88  is-monday           value 1.
               88  is-tuesday          value 2.
               88  is-wednesday        value 3.
               88  is-thursday         value 4.
               88  is-friday           value 5.
               88  is-saturday         value 6.
       01  count-of-sundays        pic 999  value is zero.
       01  start-date.
           05  start-year          pic 9999 usage display value 1901.
           05  start-month         pic 99   usage display value 01.
           05  start-day           pic 99   usage display value 01.
       01  end-date.
           05  end-year            pic 9999 usage display value 2000.
           05  end-month           pic 99   usage display value 12.
           05  end-day             pic 99   usage display value 31.
       01  date-counters.
           05  year-num            pic 9999.
           05  month-num           pic 99.
      ******************************************************************
       procedure division.
       main-procedure.
           perform varying year-num from start-year by 1
                   until year-num > end-year
               perform varying month-num from 1 by 1
                       until month-num > 12
                   string year-num month-num "01" into the-date
                   compute day-num = function integer-of-date(the-date)
                   divide day-num by 7
                       giving week-num remainder day-of-week-num
                   display the-date ": "
                       week-num " * 7 + " day-of-week-num " = " day-num
                   if is-sunday
                       add 1 to count-of-sundays
               end-perform
           end-perform
           display "No Sundays the 1st every month: " count-of-sundays.
           stop run.
       end program euler019.
