      ************************************************************
      * Component : BTSTSORT (COBOL batch)
      * Author : Cyril Coquilleau
      * Date : 2018-04-01
      * Purpose : Fills an array and launches Quicksort module
      ******************************************************************
      *-----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BTSTSORT.

      *-----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Working data items
       01 ARRAY-SIZE   PIC 9(9) VALUE 10000.
       01 I            PIC 9(9).

      * Copy with the array to sort, defined as ELEMENT in this example
       COPY "CARRSORT.cpy".

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
      *   Fills the array with random values
          PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
             COMPUTE ELEMENT(I) = FUNCTION RANDOM * 10000000
          END-PERFORM

      *   Displays the unsorted array
      *     DISPLAY "Displays the unsorted array:"
      *    PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
      *       DISPLAY "Indice: " I "  Valor: " ELEMENT(I)
      *    END-PERFORM

      * Start timer
           DISPLAY FUNCTION CURRENT-DATE(11:6).

      *   Calls Quicksort module
          MOVE 1 TO LOW
          MOVE ARRAY-SIZE TO HIGH
          CALL "MQCKSORT" USING REFERENCE ARRAY
                                CONTENT LOW
                                CONTENT HIGH
      * Stop timer
           DISPLAY FUNCTION CURRENT-DATE(11:6).

      *   Displays the sorted array
      *     DISPLAY "  ".
      *     DISPLAY "Displays the sorted array:"
      *    PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
      *       DISPLAY "Indice: " I "  Valor: " ELEMENT(I)
      *    END-PERFORM

          STOP RUN.
      *-----------------------------------------------------------------*

       END PROGRAM BTSTSORT.
