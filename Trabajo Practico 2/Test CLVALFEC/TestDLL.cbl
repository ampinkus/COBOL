    ******************************************************************
      * Author: Gauchos con COBOL
      * Date: 10/11/2023
      * Purpose: TestDLL para probar el funcionamiento de CLVALFEC.dylib
      * Tectonics: cobc
      * NOTA DE ERRORES:
      ******************************************************************
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTDLL.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       SPECIAL-NAMES.
        DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.


      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01 WS-VALFECIO.
          05 WS-ENTRADA.
             10 WS-FEC-I.
                15 WS-DD-I                     PIC 9(02).
                15 WS-MM-I                     PIC 9(02).
                15 WS-AAAA-I                   PIC 9(04).

          05 WS-SALIDA.
             10 WS-DESCUENTO-O                 PIC X(01).
             10 WS-VALIDACION-O                PIC X(01).
             10 WS-MOTIVO-ERROR-O.
                15 WS-COD-ERROR-O              PIC X(20).
                15 WS-DES-ERROR-O              PIC X(100).

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       MAIN-PROCEDURE.

            PERFORM 1000-PROGRAMA
               THRU 1000-PROGRAMA-EXIT.

           STOP RUN.

      *----------------------------------------------------------------*
       1000-PROGRAMA.
           MOVE 10 TO WS-DD-I
           MOVE 12 TO WS-MM-I
           MOVE 2023 TO WS-AAAA-I

      *     DISPLAY "Ingrese el dia: (DD) "     ACCEPT WS-DD-I.
      *     DISPLAY "Ingrese el mes: (MM)"      ACCEPT WS-MM-I
      *     DISPLAY "Ingrese el anio: (AAAA"    ACCEPT WS-AAAA-I.

           CALL "CLVALFEC" USING WS-VALFECIO.
      *    Aplica descuento?
           DISPLAY WS-DESCUENTO-O
      *    Fecha válida?
           DISPLAY WS-VALIDACION-O
           DISPLAY WS-DES-ERROR-O
           DISPLAY WS-COD-ERROR-O.

       1000-PROGRAMA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
           END PROGRAM TESTDLL.
