      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 PEPE PIC X.
       PROCEDURE DIVISION.
           PERFORM DISPLAY-AND-ACCEPT UNTIL PEPE = "s" OR PEPE = "S"
           IF PEPE = "a" OR PEPE = "A"
               DISPLAY "You entered A"
           ELSE IF PEPE = "b" OR PEPE = "B"
               DISPLAY "You entered B"
           ELSE
               DISPLAY "You entered a letter other than A or B"
           END-IF
           STOP RUN.

       DISPLAY-AND-ACCEPT.
           DISPLAY "Enter a letter: "
           ACCEPT PEPE.
