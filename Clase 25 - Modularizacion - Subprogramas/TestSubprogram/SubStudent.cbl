      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTIL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 PARAMETRES.
           02 PA-RETURN-CODE PIC 99 VALUE 0.
       PROCEDURE DIVISION USING PARAMETRES.
       MAIN-PROCEDURE.
           DISPLAY "Hello world"
           MOVE 0 TO PA-RETURN-CODE
           STOP RUN.
       END PROGRAM UTIL.
