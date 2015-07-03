      * Author: Dennis Decker Jensen
      * Date: 01 July 2015
      * Purpose: Find largest product of 13 adjacent digits in a series.
      * Tectonics: cobc -x euler0008.cob
       IDENTIFICATION DIVISION.
       PROGRAM-ID. euler0008.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  thousand-digits         pic x(1000)  values   "73167176531330
      -"6249192251196744265747423553491949349698352031277450632623957831
      -"8016984801869478851843858615607891129494954595017379583319528532
      -"0880551112540698747158523863050715693290963295227443043557668966
      -"4895044524452316173185640309871112172238311362229893423380308135
      -"3362766142828064444866452387493035890729629049156044077239071381
      -"0515859307960866701724271218839987979087922749219016997208880937
      -"7665727333001053367881220235421809751254540594752243525849077116
      -"7055601360483958644670632441572215539753697817977846174064955149
      -"2908625693219784686224828397224137565705605749026140797296865241
      -"4535100474821663704844031998900088952434506585412275886668811642
      -"7171479924442928230863465674813919123162824586178664583591245665
      -"2947654568284891288314260769004224219022671055626321111109370544
      -"2175069416589604080719840385096245544436298123098787992724428490
      -"9188845801561660979191338754992005240636899125607176060588611646
      -"7109405077541002256983155200055935729725716362695618826704282524
      -"83600823257530420752963450".
       01  digit-table redefines thousand-digits.
           05 digits                   pic 9 occurs 1000 times.
       01  product-digit-table.
           05 product-digits           pic 9 occurs 13 times.
       01  product                     pic 9(13) usage is computational.
       01  max-product                 pic 9(13) usage is computational.
       01  i                           pic 999 usage is computational.
       01  j                           pic 999 usage is computational.
       01  product-where               pic 999 usage is computational.
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           perform varying i from 1 by 1 until i > 1000 - 13 + 1
               move 1 to product
               perform varying j from 0 by 1 until j = 13
                   compute product = product * digits(i + j)
               end-perform
               if product > max-product
                   move product to max-product
                   move i to product-where
                   move thousand-digits(i:13) to product-digit-table
               end-if
           end-perform
           display
               "Maximum product of 13 adjacent digits: "
               max-product " at " product-where
               " -> " product-digit-table
           end-display
           STOP RUN.
       END PROGRAM euler0008.
