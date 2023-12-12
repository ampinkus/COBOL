      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       ENVIRONMENT DIVISION.
          CONFIGURATION SECTION.
      *-----------------------
          INPUT-OUTPUT SECTION.
      *-----------------------
          DATA DIVISION.
          FILE SECTION.
      *-----------------------
          WORKING-STORAGE SECTION.
          01 PARAMETRES.
              02 PA-RETURN-CODE PIC 99 VALUE 0.
              02 WS-STUDENT-ID PIC 9(4) VALUE 1000.
              02 WS-STUDENT-NAME PIC A(15) VALUE 'Tim'.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
          PROCEDURE DIVISION.
          MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            CALL "MYMODULE" USING PARAMETRES
            STOP RUN.
      ** add other procedures here
          END PROGRAM YOUR-PROGRAM-NAME.
