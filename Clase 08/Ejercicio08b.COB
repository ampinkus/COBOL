 ******************************************************************
      * Author: Fredi
      * Date:
      * Purpose: Ejercicio 01 clase 08 tomando la fecha del sistema
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
       77 WS-NOMBRE PIC X(20).
       77 WS-APELLIDO PIC X(20).
      * Para que el DNI salga con puntos
       77 WS-DNI PIC 99.999.999.
       77 WS-ANIO PIC 9(04).
      * Para que la edad no tenga ceros delante del número
       77 WS-EDAD PIC ZZZ9.
      * Para tomar la fecha del sistema
       77 WS-CURRENT-DATE PIC 9(8) VALUE ZEROS.


       PROCEDURE DIVISION.
           PERFORM 0000-INICIAR-PROGRAMA
              THRU 0000-INICIAR-PROGRAMA-FIN.

           PERFORM 2000-PROCESAR-PROGRAMA
              THRU 2000-PROCESAR-PROGRAMA-FIN.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           STOP RUN.

      *-----------------------------------------------------------------
      * 4 DIGITOS-VERBO-NOMBRE
       0000-INICIAR-PROGRAMA.
           MOVE SPACES TO WS-NOMBRE.
           MOVE SPACES TO WS-APELLIDO.
           MOVE ZEROS TO WS-DNI.
           MOVE ZEROS TO WS-ANIO.
           MOVE ZEROS TO WS-EDAD.

       0000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       2000-PROCESAR-PROGRAMA.
           DISPLAY "INGRESAR NOMBRE: ".
           ACCEPT WS-NOMBRE.

           DISPLAY "INGRESAR APELLIDO: ".
           ACCEPT WS-APELLIDO.

           DISPLAY "INGRESAR DNI: ".
           ACCEPT WS-DNI.

           DISPLAY "INGRESAR ANIO NACIMIENTO: ".
           ACCEPT WS-ANIO.

           MOVE FUNCTION CURRENT-DATE (1:4) TO WS-CURRENT-DATE.
           DISPLAY WS-CURRENT-DATE.
      * Resto de la fecha actual la fecha de naciemiento
           SUBTRACT WS-CURRENT-DATE FROM WS-ANIO GIVING WS-EDAD.



       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       3000-FINALIZAR-PROGRAMA.
           DISPLAY " ".
           DISPLAY "+---+---+---+---+---+---+---+---+---+---+---+---+".
           DISPLAY "EL NOMBRE ES: " WS-NOMBRE.
           DISPLAY "EL APELLIDO ES: " WS-APELLIDO.
           DISPLAY "EL DNI ES: " WS-DNI.
           DISPLAY "LA EDAD ES: " WS-EDAD " ANIOS".

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *-----------------------------------------------------------------

       END PROGRAM Cl08EJ01.
