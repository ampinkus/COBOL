      ******************************************************************
      * Component : BTSTSORT (COBOL batch)
      * Author : Cyril Coquilleau
      * Date : 2018-04-01
      * Purpose : Fills an array and launches Quicksort module
      ******************************************************************
      *-----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MQCKSORT RECURSIVE.

      *-----------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Partition Index
       01 PI             PIC 9(9).
       01 PI-MINUS1      PIC 9(9).
       01 PI-PLUS1       PIC 9(9).

      * Temporary data for PARTITION procedure
       01 PIVOT          PIC 9(9).
       01 I              PIC 9(9).
       01 I-PLUS1        PIC 9(9).
       01 J              PIC 9(9).
       01 ELEMENT-TEMP   PIC 9(9).

       LINKAGE SECTION.
       COPY 'CARRSORT.cpy'.

      *-----------------------------------------------------------------*
       PROCEDURE DIVISION USING ARRAY LOW HIGH.

       MAIN-PROCEDURE.
           IF LOW < HIGH
      *      Partitionning (fills PI data item)
             MOVE 0 TO PI
             PERFORM PARTITION
             COMPUTE PI-PLUS1 = PI + 1
             COMPUTE PI-MINUS1 = PI - 1

      *      Quicksort recursive calls
             CALL 'MQCKSORT' USING REFERENCE ARRAY
                                   CONTENT LOW
                                   CONTENT PI-MINUS1
             CALL 'MQCKSORT' USING REFERENCE ARRAY
                                   CONTENT PI-PLUS1
                                   CONTENT HIGH
           END-IF

           GOBACK.

       PARTITION.
           MOVE ELEMENT(HIGH) TO PIVOT

      *   Index of smaller element
           COMPUTE I = LOW - 1
           PERFORM VARYING J FROM LOW BY 1 UNTIL J >= HIGH
      *      If current element is smaller than or equal to pivot
             IF ELEMENT(J) <= PIVOT
                COMPUTE I = I + 1

      *         Swap ELEMENT(I) and ELEMENT(J)
                MOVE ELEMENT(I)   TO ELEMENT-TEMP
                MOVE ELEMENT(J)   TO ELEMENT(I)
                MOVE ELEMENT-TEMP TO ELEMENT(J)
             END-IF
           END-PERFORM

      *   Swap ELEMENT(I+1) and ELEMENT(HIGH) (or pivot)
           COMPUTE I-PLUS1 = I + 1
           MOVE ELEMENT(I-PLUS1) TO ELEMENT-TEMP
           MOVE ELEMENT(HIGH)    TO ELEMENT(I-PLUS1)
           MOVE ELEMENT-TEMP     TO ELEMENT(HIGH)

      *   Returning the pivot position to the main procedure
           MOVE I-PLUS1 TO PI.
      *-----------------------------------------------------------------*

       END PROGRAM MQCKSORT.
