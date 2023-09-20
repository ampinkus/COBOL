       IDENTIFICATION DIVISION.
       PROGRAM-ID. VectorPrinter.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  vector OCCURS 11 TIMES PIC 9(4) VALUE 0.
       01  output-string PIC X(50) VALUE SPACES.
       01 idx PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.
           *> Initialize the vector with some values
                MOVE 100 TO vector(1)
                MOVE 200 TO vector(2)
                MOVE 300 TO vector(3)
                MOVE 400 TO vector(4)
                MOVE 500 TO vector(5)
                MOVE 600 TO vector(6)
                MOVE 700 TO vector(7)
                MOVE 800 TO vector(8)
                MOVE 900 TO vector(9)
                MOVE 1000 TO vector(10)


           *> Concatenate vector elements into output string
           MOVE SPACES TO output-string
           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > 10
             MOVE vector(idx) TO output-string(((idx - 1) * 4 + 1):(3 * idx))
           END-PERFORM.

           *> Display the vector as a row
           DISPLAY "Vector elements: " output-string

           STOP RUN.
