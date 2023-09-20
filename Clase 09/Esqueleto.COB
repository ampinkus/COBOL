 ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMA.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-VAR-AUXILIARES.
              05 WS-VARIASBLE01            PIC X(20).
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 0000-INICIAR-PROGRAMA
              THRU 0000-INICIAR-PROGRAMA-EXIT.

           PERFORM 2000-PROCESAR-PROGRAMA
              THRU 2000-PROCESAR-PROGRAMA-EXIT.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       0000-INICIAR-PROGRAMA.
           DISPLAY "Iniciar".
       0000-INICIAR-PROGRAMA-EXIT.
           EXIT.

       2000-PROCESAR-PROGRAMA.
           DISPLAY "Procesar".
       2000-PROCESAR-PROGRAMA-EXIT.
           EXIT.

       3000-FINALIZAR-PROGRAMA.
           DISPLAY "Finalizar".
       3000-FINALIZAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
           END PROGRAM PROGRAMA.
