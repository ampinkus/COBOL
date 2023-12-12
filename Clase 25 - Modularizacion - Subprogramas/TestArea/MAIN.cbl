      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECTANGULO.

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
           05 WS-ENTRADA.
              10 PA-RETURN-CODE    PIC 99 VALUE 0.
              10 WS-BASE           PIC 99.
              10 WS-ALTURA   PIC 99.

           05 WS-SALIDA.
              10 WS-AREA      PIC 9999.
              01 WS-PERIMETRO PIC 9999.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           DISPLAY "Ingrese el valor de la base del rectangulo: "
                   ACCEPT WS-BASE.
           DISPLAY "Ingrese el valor de la altura del rectangulo: "
                   ACCEPT WS-ALTURA.
           CALL "AREA" USING PARAMETRES

           DISPLAY "El perimetro es: " WS-PERIMETRO.
           DISPLAY "El area es: " WS-AREA.

           STOP RUN.
      ** add other procedures here
          END PROGRAM RECTANGULO.
