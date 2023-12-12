      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
          PROGRAM-ID. MYMODULE.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       ENVIRONMENT DIVISION.
          CONFIGURATION SECTION.
      *-----------------------
          INPUT-OUTPUT SECTION.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       DATA DIVISION.
          FILE SECTION.
      *-----------------------
          WORKING-STORAGE SECTION.
      *-----------------------
          LINKAGE SECTION.
          01 PARAMETRES.
              02 PA-RETURN-CODE PIC 99 VALUE 0.
              02 LS-STUDENT-ID PIC 9(4).
              02 LS-STUDENT-NAME PIC A(15).

      *-*-*-*-*-*-*-*-*-*-*-*-*-*
       PROCEDURE DIVISION USING PARAMETRES.
          MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
            DISPLAY LS-STUDENT-ID
            DISPLAY LS-STUDENT-NAME
            MOVE 0 TO PA-RETURN-CODE
           GOBACK.
      ** add other procedures here
       END PROGRAM MYMODULE.
