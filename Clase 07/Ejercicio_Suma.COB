 ******************************************************************
      * Author: Fredi
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Suma.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
      * Variables generales.
       01 WS-VARIABLES.
          02 WS-NUMERO1       PIC S9(5)V99 VALUE ZERO .
          02 WS-NUMERO2       PIC S9(5)V99 VALUE ZERO.
          02 WS-RESULTADO     PIC S9(6)V99 VALUE ZERO.

       PROCEDURE DIVISION.
       0000-PROCESO-PRINCIPAL.

           PERFORM 1000-CARGAR-DATOS
              THRU 1000-CARGAR-DATOS-FIN

           PERFORM 2000-PROCESAR-DATOS
              THRU 2000-PROCESAR-DATOS-EXIT.

           PERFORM 3000-IMPRIMIR
              THRU 3000-IMPRIMIR-EXIT.

           STOP RUN.
       0000-PROCESO-PRINCIPAL-FIN.
           EXIT.

      * Carga de datos
       1000-CARGAR-DATOS.
           DISPLAY "Ingresar el primer numero de la suma : "
           NO ADVANCING
           ACCEPT WS-NUMERO1.

           DISPLAY "Ingresar el segundo numero de la suma : "
           NO ADVANCING
           ACCEPT WS-NUMERO2.

       1000-CARGAR-DATOS-FIN.
           EXIT.


      * Procesar datos
       2000-PROCESAR-DATOS.
           ADD WS-NUMERO1 TO WS-NUMERO2 GIVING WS-RESULTADO.

       2000-PROCESAR-DATOS-EXIT.
           EXIT.


      * Imprimir datos
       3000-IMPRIMIR.
           DISPLAY "El resultado de la suma es: " WS-RESULTADO.

       3000-IMPRIMIR-EXIT.
           EXIT.

       END PROGRAM Suma.
