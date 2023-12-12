      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      * Debe tener el mismo nombre que el programa en disco, si no hay error de DLL
          PROGRAM-ID. AREA.

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
           05 WS-ENTRADA.
              10 PA-RETURN-CODE    PIC 99 VALUE 0.
              10 WS-BASE           PIC 99 VALUE 0.
              10 WS-ALTURA         PIC 99 VALUE 0.

           05 WS-SALIDA.
              10 WS-AREA           PIC 9999 VALUE 0.
              01 WS-PERIMETRO      PIC 9999 VALUE 0.

      *-*-*-*-*-*-*-*-*-*-*-*-*-*
       PROCEDURE DIVISION USING PARAMETRES.
          MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **

           DISPLAY "WS-BASE: " WS-BASE
           DISPLAY "WS-ALTURA: " WS-ALTURA
      *      MULTIPLY WS-BASE BY WS-ALTURA GIVING WS-AREA.
      *      ADD WS-BASE TO WS-ALTURA GIVING WS-PERIMETRO.
      *      MULTIPLY WS-PERIMETRO BY 2 GIVING WS-PERIMETRO.
            MOVE 0 TO PA-RETURN-CODE
           GOBACK.
      ** add other procedures here
       END PROGRAM AREA.
