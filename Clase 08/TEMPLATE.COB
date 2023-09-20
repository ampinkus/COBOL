 ******************************************************************
      * Author: Fredi
      * Date:
      * Purpose: Template
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Cl08EJ01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * Variables generales.
       77 WS-FECHA PIC 9(6).


       PROCEDURE DIVISION.
           PERFORM 0000-INICIAR-PROGRAMA

              THRU 0000-INICIAR-PROGRAMAL-FIN.

           PERFORM 2000-PROCESAR-PROGRAMA

              THRU 2000-PROCESAR-PROGRAMA-FIN.

           PERFORM 3000-FINALIZAR-PROGRAMA


           THRU 3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.

      *-----------------------------------------------------------------
      * 4 DIGITOS-VERBO-NOMBRE
       0000-INICIAR-PROGRAMA.


           STOP RUN.
       0000-INICIAR-PROGRAMAL-FIN.
           EXIT.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       2000-PROCESAR-PROGRAMA.


       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       3000-FINALIZAR-PROGRAMA.


       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *-----------------------------------------------------------------
       END PROGRAM Cl08EJ01.
