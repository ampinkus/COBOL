    ******************************************************************
      * Author: Gauchos con COBOL
      * Date: 10/11/2023
      * Purpose: TestMAESTARJ para probar el funcionamiento de MAESTARJ
      * Tectonics: cobc
      * NOTA DE ERRORES:
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTMAESTARJ.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
      * ESTRUCTURA DE DATOS PARA COMUNICARSE CON LA RUTINA MAESTARJ
       01 LK-TARJETA.
          COPY MAESTARJ.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAIN-PROCEDURE.
           PERFORM 1000-BUSCAR-DATOS
              THRU 1000-BUSCAR-DATOS-EXIT.

           STOP RUN.

      *----------------------------------------------------------------*
       1000-BUSCAR-DATOS.

           MOVE "9012-3456-1234-5678" TO LK-NUMERO-I.
           CALL 'MAESTARJ' USING LK-TARJETA.

       1000-BUSCAR-DATOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
           END PROGRAM TESTMAESTARJ.
