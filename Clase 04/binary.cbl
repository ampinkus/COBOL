       IDENTIFICATION DIVISION.
       PROGRAM-ID. BaseConverter.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  base10-number      PIC 9(9) VALUE 0.
       01  binary-digits      OCCURS 0 TO 32 TIMES
                               DEPENDING ON idx
                               PIC 9 VALUE 0.
       01  quotient           PIC 9(9) VALUE 0.
       01  rem                PIC 9(4) VALUE 0.
       01  idx                PIC 9(4) VALUE 1.

       PROCEDURE DIVISION.
       *> Input the base 10 number
       DISPLAY "Enter a base 10 number: "
       ACCEPT base10-number

       *> Convert to base 2
       PERFORM UNTIL base10-number = 0
           COMPUTE quotient = base10-number / 2
           COMPUTE rem = FUNCTION MOD(base10-number, 2)
           MOVE rem TO binary-digits(idx)
           ADD 1 TO idx
           MOVE quotient TO base10-number
       END-PERFORM.

       *> Output the base 2 number
       DISPLAY "Base 2 representation: "
           MOVE idx TO rem.
           PERFORM VARYING idx FROM rem BY -1 UNTIL idx = 0
           DISPLAY binary-digits(idx)  WITH NO ADVANCING
       END-PERFORM.

       STOP RUN.
