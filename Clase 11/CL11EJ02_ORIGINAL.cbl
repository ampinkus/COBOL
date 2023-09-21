      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 07/09/2023
      * Purpose: CLASE 11 - EJERCICIO 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL11EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENTRADA
           ASSIGN TO 'E:\COBOL\PARCIAL2023.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.
      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENTRADA.
       01 ENT-ARCHIVO.
          05 ENT-FECHA                      PIC X(10).
          05 ENT-ID-EMPLEADO                PIC 9(05).
          05 ENT-NOMBRE-APELLIDO            PIC X(40).
          05 ENT-CATEGORIA                  PIC X(20).
          05 ENT-IMPORTE                    PIC 9(8)V9(2).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                    VALUE '00'.
             88 FS-ENTRADA-EOF                   VALUE '10'.
             88 FS-ENTRADA-NFD                   VALUE '35'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-ENTRADA             PIC 9(5) VALUE 0.

       01 WS-CORTE-CONTROL.
          05 WS-CC-FECHA-ANT                PIC X(10).
          05 WS-CC-CATEGORIA-ANT            PIC X(20).

       01 WS-ACUMULADORES.
          05 WS-CC-IMPORTE-ACUM             PIC 9(8)V9(2).
          05 WS-CC-CANT-VENTAS-ACUM         PIC 9(04).

       01 WS-LISTADO.
          05 WS-LIS-SEPARADOR-1             PIC X(39) VALUE ALL '-'.
          05 WS-LIS-SEPARADOR-2             PIC X(39) VALUE ALL '#'.
          05 WS-LIS-DETALLE.
             10 WS-LIS-D-CATEGORIA          PIC X(12).
             10 FILLER                      PIC X(07) VALUE ' |     '.
             10 WS-LIS-D-CANTIDAD           PIC ZZZ9.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-IMPORTE            PIC ZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-ENTRADA-OK

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-ENTRADA-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-ARCHIVO
              THRU 1100-ABRIR-ARCHIVO-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO.

           OPEN INPUT ENTRADA.

           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                    PERFORM 1500-LEER-ARCHIVO
                       THRU 1500-LEER-ARCHIVO-EXIT
               WHEN FS-ENTRADA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.

           READ ENTRADA.

            EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                    ADD 1                   TO WS-CONT-REG-ENTRADA
               WHEN FS-ENTRADA-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1500-LEER-ARCHIVO-EXIT.
       EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           MOVE ZEROS TO WS-CC-IMPORTE-ACUM.
           MOVE ZEROS TO WS-CC-CANT-VENTAS-ACUM.
           MOVE ENT-FECHA TO WS-CC-FECHA-ANT.

           DISPLAY WS-LIS-SEPARADOR-2.
           DISPLAY 'FECHA: ' WS-CC-FECHA-ANT
           DISPLAY WS-LIS-SEPARADOR-1.

           PERFORM 2100-PROCESAR-CORTE-X-DIA
              THRU 2100-PROCESAR-CORTE-X-DIA-FIN
             UNTIL FS-ENTRADA-EOF
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESAR-CORTE-X-DIA.

           MOVE ZEROS                       TO WS-CC-IMPORTE-ACUM.
           MOVE ZEROS                       TO WS-CC-CANT-VENTAS-ACUM.
           MOVE ENT-CATEGORIA               TO WS-CC-CATEGORIA-ANT.

           PERFORM 2200-PROCESAR-CORTE-X-CATEG
              THRU 2200-PROCESAR-CORTE-X-CATEG-FIN
             UNTIL FS-ENTRADA-EOF
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT
                OR ENT-CATEGORIA NOT EQUAL WS-CC-CATEGORIA-ANT.

           MOVE WS-CC-CATEGORIA-ANT         TO WS-LIS-D-CATEGORIA.
           MOVE WS-CC-CANT-VENTAS-ACUM      TO WS-LIS-D-CANTIDAD.
           MOVE WS-CC-IMPORTE-ACUM          TO WS-LIS-D-IMPORTE.

           DISPLAY WS-LIS-DETALLE.


       2100-PROCESAR-CORTE-X-DIA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-CORTE-X-CATEG.

           ADD ENT-IMPORTE                  TO WS-CC-IMPORTE-ACUM.
           ADD 1                            TO WS-CC-CANT-VENTAS-ACUM.

           MOVE ENT-FECHA                   TO WS-CC-FECHA-ANT.
           MOVE ENT-CATEGORIA               TO WS-CC-CATEGORIA-ANT.

           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

       2200-PROCESAR-CORTE-X-CATEG-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY WS-LIS-SEPARADOR-2.

           DISPLAY 'CANTIDAD DE REGISTROS LEIDOS: ' WS-CONT-REG-ENTRADA.

           PERFORM 3200-CERRAR-ARCHIVO
              THRU 3200-CERRAR-ARCHIVO-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO.

           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR AL CERRAR ARCHUIVO ENTRADA: ' FS-ENTRADA
           END-IF.

       3200-CERRAR-ARCHIVO-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL11EJ02.
