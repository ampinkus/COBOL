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
       01 WS-PERSON.
           03 NOMBRE PIC X(40) VALUE SPACES.
           03 APELLIDO PIC X(40) VALUE SPACES.
           03 CALLE PIC X(40) VALUE SPACES.
           03 CIUDAD PIC X(40) VALUE SPACES.
           03 ZIP PIC X(6) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-PROCESO-PRINCIPAL.


           STOP RUN.
       0000-PROCESO-PRINCIPAL-FIN.
           EXIT.

      * Carga de datos
       1000-CARGAR-DATOS.


       1000-CARGAR-DATOS-FIN.
           EXIT.


      * Procesar datos
       2000-PROCESAR-DATOS.


       2000-PROCESAR-DATOS-EXIT.
           EXIT.


      * Imprimir datos
       3000-IMPRIMIR.


       3000-IMPRIMIR-EXIT.
           EXIT.

       END PROGRAM Suma.
