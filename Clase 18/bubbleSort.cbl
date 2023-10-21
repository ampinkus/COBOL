      ******************************************************************
      * Author: Alfredo
      * Date:
      * Purpose:Buuble sortng implementation
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BubbleSort.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Array  OCCURS 10 TIMES.
          02 Element PIC 9(5).

       01 Temp PIC 9(5).
       01 Swapped PIC X VALUE 'N'.
       77 ArraySize PIC 9(5) VALUE 10.
       77 I PIC 9(5).
       77 J PIC 9(5).
       77 K PIC 9(5).

       PROCEDURE DIVISION.

           PERFORM Bubble-Sort
           DISPLAY "Sorted Array:"
           PERFORM Display-Array
           STOP RUN.



       Bubble-Sort.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= ArraySize
              PERFORM VARYING J FROM 1 BY 1 UNTIL J >= ArraySize - I + 1
                  IF Element(J) > Element(J + 1)
                      MOVE Element(J) TO Temp
                      MOVE Element(J + 1) TO Element(J)
                      MOVE Temp TO Element(J + 1)
                      MOVE 'Y' TO Swapped
                  END-IF
              END-PERFORM
              IF Swapped = 'N'
                  EXIT PERFORM
              END-IF
           END-PERFORM.

       Display-Array.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K <= ArraySize
              DISPLAY Element(K)
           END-PERFORM.
