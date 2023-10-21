      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QuickSortProgram.

      *-----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variable que guarda el vector a ordenar
          01 WS-VECTOR.
               05 WS-NUMBERS PIC 9(02) OCCURS 10 TIMES.
      *=================================================================*
      * Variables para el ordenamiento QuickSort
      *=================================================================*

      * El PIC(N) de WS-ARRAY-SIZE y WS-I donde N permita guardar el
      * número de elementos del arreglo.
          77 WS-ARRAY-SIZE     PIC 9(02).
          77 WS-I              PIC 9(02).

      * WS-LOW, WS-HIGH, WS-LEFT y WS-RIGHT tienen que tener un PIC(N)
      * donde N permita guardar el número de elementos del arreglo.
          77 WS-LOW            PIC 9(02).
          77 WS-HIGH           PIC 9(02).
          77 WS-LEFT           PIC 9(02).
          77 WS-RIGHT          PIC 9(02).

      * WS-PIVOT y WS-TEMP tienen que tener el mismo formato PIC
      * que el arreglo a ordenar
          77 WS-PIVOT          PIC 9(02).
          77 WS-TEMP           PIC 9(02).

      * Indica que el arreglo ya está ordenado
          77 WS-SORTED         PIC 9(01) VALUE 0.
      *=================================================================*
      * Fin variables para el ordenamiento QuickSort
      *=================================================================*
      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.
           MOVE 10     TO WS-ARRAY-SIZE
           MOVE 14     TO WS-NUMBERS(1)
           MOVE 1      TO WS-NUMBERS(2)
           MOVE 5      TO WS-NUMBERS(3)
           MOVE 16     TO WS-NUMBERS(4)
           MOVE 3      TO WS-NUMBERS(5)
           MOVE 10     TO WS-NUMBERS(6)
           MOVE 9      TO WS-NUMBERS(7)
           MOVE 7      TO WS-NUMBERS(8)
           MOVE 12     TO WS-NUMBERS(9)
           MOVE 11     TO WS-NUMBERS(10)

      * Muestro el arreglo a ordenar
           DISPLAY "Unsorted Array:"
           PERFORM 1000-DISPLAY-NUMBERS
              THRU 1000-DISPLAY-NUMBERS-EXIT

      * Pongo WS-LOW en 1 para efectuar el ordenamiento
           MOVE 1 TO WS-LOW
      * Tengo que pasar al proceso el tamaño del arreglo a ordenar
           MOVE WS-ARRAY-SIZE TO WS-HIGH

      * Procedo a ordenar el arreglo llamando el procedimiento
      * QUICK-SORT
           PERFORM 9000-START-QUICK-SORT
              THRU 9000-START-QUICK-SORT-EXIT.

      * Muestro el arreglo ordenado
           DISPLAY "Sorted Array:"
           PERFORM 1000-DISPLAY-NUMBERS
              THRU 1000-DISPLAY-NUMBERS-EXIT

           STOP RUN.

      *-----------------------------------------------------------------*
       1000-DISPLAY-NUMBERS.
           DISPLAY "NUMBERS: "
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ARRAY-SIZE
               DISPLAY WS-NUMBERS(WS-I)
           END-PERFORM.
       1000-DISPLAY-NUMBERS-EXIT.
           EXIT.

      *-----------------------------------------------------------------*
      *    A PARTIR DE AQUÍ ES LA RUTINA DE QUICK SORT.  USO 9000       *
      *    PARA QUE SEA MAS FACIL USARLO EN UN PROGRAMA EXISTENTE      *
      *-----------------------------------------------------------------*
       9000-START-QUICK-SORT.
           SET WS-SORTED TO 1.
           IF WS-LOW >= WS-HIGH
               MOVE 0 TO WS-SORTED
           END-IF

           PERFORM 9100-START-SORT-ROUTINE
              THRU 9100-START-SORT-ROUTINE-EXIT
               UNTIL WS-SORTED = 0.

       9000-START-QUICK-SORT-EXIT.
           EXIT.

      *-----------------------------------------------------------------*
       9100-START-SORT-ROUTINE.
           MOVE WS-NUMBERS(WS-LOW) TO WS-PIVOT
           MOVE WS-LOW TO WS-LEFT
           MOVE WS-HIGH TO WS-RIGHT
           SET WS-SORTED TO 0.

           PERFORM UNTIL WS-LEFT >= WS-RIGHT
               PERFORM UNTIL WS-NUMBERS(WS-LEFT) >= WS-PIVOT
                   ADD 1 TO WS-LEFT
               END-PERFORM

               PERFORM UNTIL WS-NUMBERS(WS-RIGHT) <= WS-PIVOT
                   SUBTRACT 1 FROM WS-RIGHT
               END-PERFORM

               IF WS-LEFT <= WS-RIGHT
                   PERFORM 9200-START-SWAP
                      THRU 9200-START-SWAP-EXIT
               ELSE
                   SET WS-SORTED TO 1
               END-IF
           END-PERFORM

           IF WS-LOW < WS-RIGHT
               MOVE WS-RIGHT TO WS-HIGH
               PERFORM 9000-START-QUICK-SORT
                THRU 9000-START-QUICK-SORT-EXIT
           END-IF

           MOVE WS-LEFT TO WS-LOW
           ADD 1 TO WS-RIGHT
           IF WS-RIGHT < WS-HIGH
               PERFORM 9000-START-QUICK-SORT
                THRU 9000-START-QUICK-SORT-EXIT
           END-IF.
       9100-START-SORT-ROUTINE-EXIT.
           EXIT.
      *-----------------------------------------------------------------*
       9200-START-SWAP.
           MOVE WS-NUMBERS(WS-LEFT) TO WS-TEMP
           MOVE WS-NUMBERS(WS-RIGHT) TO WS-NUMBERS(WS-LEFT)
           MOVE WS-TEMP TO WS-NUMBERS(WS-RIGHT).
       9200-START-SWAP-EXIT.
           EXIT.
      *-----------------------------------------------------------------*
           END PROGRAM QuickSortProgram.
