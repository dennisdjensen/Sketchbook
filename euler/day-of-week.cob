      * Author: Dennis Decker Jensen
      * Spike for Project Euler 19
       identification division.
       program-id. dayname.
       data division.
       working-storage section.
       01  ws-day-of-week         pic 9.
       01  names                  pic X(21)
            values "SunMonTueWedThuFriSat".
       procedure division.
       this-day.
           accept ws-day-of-week from day-of-week.
           display ws-day-of-week ": "
                names(1 + 3 * ws-day-of-week:3).
           stop run.
       end program dayname.
