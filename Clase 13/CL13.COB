      *----------------------------------------------------------------*
      * Purpose: CLASE 13 - EJERCICIO 1
      *          Merge - Corte control
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CL13EJ01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT SUCURSA
           ASSIGN TO '../SUCURSAL-A.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSA.

       SELECT SUCURSB
           ASSIGN TO '../SUCURSAL-B.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SUCURSB.







       SELECT VENTATOT
           ASSIGN TO '../VENTATOT.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTATOT.

       DATA DIVISION.

       FILE SECTION.

       FD SUCURSA.
       01 ENTRADA-A.
          05 ENTRADA-A-FECHA               PIC X(10).
          05 ENTRADA-A-COD-VENTA           PIC X(04).
          05 ENTRADA-A-PRODUCTO            PIC X(30).
          05 ENTRADA-A-IMPORTE             PIC 9(8)V9(2).

       FD SUCURSB.
       01 ENTRADA-B.
          05 ENTRADA-B-FECHA               PIC X(10).
          05 ENTRADA-B-COD-VENTA           PIC X(04).
          05 ENTRADA-B-PRODUCTO            PIC X(30).
          05 ENTRADA-B-IMPORTE             PIC 9(8)V9(2).











       FD VENTATOT.
       01 SALIDA-TOT.
          05 SALIDA-FECHA                  PIC X(10).
          05 SALIDA-COD-VENTA              PIC X(04).
          05 SALIDA-PRODUCTO               PIC X(30).
          05 SALIDA-IMPORTE                PIC 9(8)V9(2).



       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-SUCURSA                      PIC X(2).
             88 FS-SUCURSA-OK                VALUE '00'.
             88 FS-SUCURSA-EOF               VALUE '10'.
          05 FS-SUCURSB                      PIC X(2).
             88 FS-SUCURSB-OK                VALUE '00'.
             88 FS-SUCURSB-EOF               VALUE '10'.


          05 FS-VENTATOT                     PIC X(2).
             88 FS-VENTATOT-OK               VALUE '00'.

       01 WS-SUCURSA-EOF                     PIC X(1).
          88 WS-SUCURSA-EOF-YES              VALUE 'Y'.
          88 WS-SUCURSA-EOF-NO               VALUE 'N'.
       01 WS-SUCURSB-EOF                     PIC X(1).
          88 WS-SUCURSB-EOF-YES              VALUE 'Y'.
          88 WS-SUCURSB-EOF-NO               VALUE 'N'.


       01 WS-PROCESAR                        PIC X(1).
          88 SW-PROCESAR-A                   VALUE 'A'.
          88 SW-PROCESAR-B                   VALUE 'B'.


       01 WS-VAR-AUX.
          05 WS-CLAVE-FECHA                  PIC X(10)     VALUE SPACES.
          05 WS-FECHA-ANT                    PIC X(10)     VALUE SPACES.

          05 WS-SUCURSA-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-SUCURSB-CANT-REG             PIC 9(5)      VALUE 0.
          05 WS-SALIDA-CANT-REG              PIC 9(5)      VALUE 0.
		  

       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-ABRIR-ARCHIVOS
              THRU 1000-ABRIR-ARCHIVOS-EXIT.

           IF FS-SUCURSA-OK OR FS-SUCURSB-OK

                PERFORM 3000-CLAVE-MENOR
                   THRU 3000-CLAVE-MENOR-EXIT



              PERFORM 2000-PROCESAR-ARCHIVO
                 THRU 2000-PROCESAR-ARCHIVO-EXIT
                UNTIL WS-SUCURSA-EOF-YES AND
                      WS-SUCURSB-EOF-YES






              PERFORM 8000-CERRAR-ARCHIVOS
                 THRU 8000-CERRAR-ARCHIVOS-EXIT

              PERFORM 9000-TOTALES-ARCHIVO
                 THRU 9000-TOTALES-ARCHIVO-EXIT

           END-IF.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVOS.

           PERFORM 4100-ABRIR-ARCHIVO-SUCURSA
              THRU 4100-ABRIR-ARCHIVO-SUCURSA-EXIT.

           PERFORM 4200-ABRIR-ARCHIVO-SUCURSB
              THRU 4200-ABRIR-ARCHIVO-SUCURSB-EXIT.




           PERFORM 4400-ABRIR-ARCHIVO-SALIDA
              THRU 4400-ABRIR-ARCHIVO-SALIDA-EXIT.




       1000-ABRIR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

      *----SI AUN LOS DOS ARCHIVOS TIENEN REGISTROS POR LEER
                PERFORM 3000-CLAVE-MENOR
                   THRU 3000-CLAVE-MENOR-EXIT

                EVALUATE TRUE
                  WHEN SW-PROCESAR-A
                       PERFORM 3100-PROCESAR-SUCURSA
                          THRU 3100-PROCESAR-SUCURSA-EXIT
                          UNTIL NOT FS-SUCURSA-OK
                             OR WS-CLAVE-FECHA < ENTRADA-A-FECHA
                  WHEN SW-PROCESAR-B
                        PERFORM 3200-PROCESAR-SUCURSB
                          THRU 3200-PROCESAR-SUCURSB-EXIT
                         UNTIL NOT FS-SUCURSB-OK
                            OR WS-CLAVE-FECHA < ENTRADA-B-FECHA

              END-EVALUATE.


       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       3000-CLAVE-MENOR.
      *----SELECCIONO LA FECHA MENOR

           IF ENTRADA-A-FECHA <= ENTRADA-B-FECHA

                MOVE ENTRADA-A-FECHA    TO WS-CLAVE-FECHA
                SET  SW-PROCESAR-A      TO TRUE
           ELSE

                MOVE ENTRADA-B-FECHA TO WS-CLAVE-FECHA
                SET  SW-PROCESAR-B   TO TRUE
           END-IF.

       3000-CLAVE-MENOR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-PROCESAR-SUCURSA.


           MOVE ENTRADA-A             TO SALIDA-TOT.




           PERFORM 7000-GRABAR-ARCHIVO-SAL
              THRU 7000-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 6100-LEER-SUCURSA
              THRU 6100-LEER-SUCURSA-EXIT.


       3100-PROCESAR-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-PROCESAR-SUCURSB.

           MOVE ENTRADA-B             TO SALIDA-TOT.



           PERFORM 7000-GRABAR-ARCHIVO-SAL
              THRU 7000-GRABAR-ARCHIVO-SAL-EXIT.

           PERFORM 6200-LEER-SUCURSB
              THRU 6200-LEER-SUCURSB-EXIT.

       3200-PROCESAR-SUCURSB-EXIT.
           EXIT.




      *----------------------------------------------------------------*
       4100-ABRIR-ARCHIVO-SUCURSA.

           OPEN INPUT SUCURSA.

           EVALUATE FS-SUCURSA
               WHEN '00'
                    PERFORM 6100-LEER-SUCURSA
                       THRU 6100-LEER-SUCURSA-EXIT
               WHEN '35'
                    SET WS-SUCURSA-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSA
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    SET WS-SUCURSA-EOF-YES       TO TRUE
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSA
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4100-ABRIR-ARCHIVO-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       4200-ABRIR-ARCHIVO-SUCURSB.

           OPEN INPUT SUCURSB.

           EVALUATE FS-SUCURSB
               WHEN '00'
                    PERFORM 6200-LEER-SUCURSB
                       THRU 6200-LEER-SUCURSB-EXIT
               WHEN '35'
                    SET WS-SUCURSB-EOF-YES       TO TRUE
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSB
      * SI NO EXISTE EL ARCHIVO B DETENGO EL PROCESO
                    STOP RUN
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSB
      * SI NO ABRE EL ARCHIVO DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4200-ABRIR-ARCHIVO-SUCURSB-EXIT.
           EXIT.













      *----------------------------------------------------------------*
       4400-ABRIR-ARCHIVO-SALIDA.

           OPEN OUTPUT VENTATOT.

           EVALUATE FS-VENTATOT
               WHEN '00'
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTATOT'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
      * SI NO ABRE EL ARCHIVO DE SALIDA DETENGO EL PROCESO
                    STOP RUN
           END-EVALUATE.

       4400-ABRIR-ARCHIVO-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*














      *----------------------------------------------------------------*
       6100-LEER-SUCURSA.

           READ SUCURSA.

           EVALUATE FS-SUCURSA
               WHEN '00'
                    ADD 1                      TO WS-SUCURSA-CANT-REG
               WHEN '10'
                    SET WS-SUCURSA-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-A-FECHA
               WHEN OTHER
                    SET WS-SUCURSA-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-A-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSA'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSA
           END-EVALUATE.
       6100-LEER-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       6200-LEER-SUCURSB.

           READ SUCURSB.

           EVALUATE FS-SUCURSB
               WHEN '00'

                    ADD 1                      TO WS-SUCURSB-CANT-REG
               WHEN '10'
                    SET WS-SUCURSB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-B-FECHA
               WHEN OTHER
                    SET WS-SUCURSB-EOF-YES     TO TRUE
                    MOVE HIGH-VALUES           TO ENTRADA-B-FECHA
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SUCURSB'
                    DISPLAY 'FILE STATUS: ' FS-SUCURSB
           END-EVALUATE.
       6200-LEER-SUCURSB-EXIT.
           EXIT.








      *----------------------------------------------------------------*
       7000-GRABAR-ARCHIVO-SAL.

           WRITE SALIDA-TOT.

           EVALUATE FS-VENTATOT
               WHEN '00'
                    ADD 1 TO WS-SALIDA-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-VENTATOT
           END-EVALUATE.

       7000-GRABAR-ARCHIVO-SAL-EXIT.
           EXIT.
      *----------------------------------------------------------------*











      *----------------------------------------------------------------*
       8000-CERRAR-ARCHIVOS.

           PERFORM 8100-CERRAR-ARCH-SAL
              THRU 8100-CERRAR-ARCH-SAL-EXIT.




           PERFORM 8300-CERRAR-ARCH-SUCURSA
              THRU 8300-CERRAR-ARCH-SUCURSA-EXIT.

           PERFORM 8400-CERRAR-ARCH-SUCURSB
              THRU 8400-CERRAR-ARCH-SUCURSB-EXIT.



       8000-CERRAR-ARCHIVOS-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8100-CERRAR-ARCH-SAL.

           CLOSE VENTATOT.

           IF NOT FS-VENTATOT-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-VENTATOT
           END-IF.

       8100-CERRAR-ARCH-SAL-EXIT.
           EXIT.
      *----------------------------------------------------------------*






      *----------------------------------------------------------------*
       8300-CERRAR-ARCH-SUCURSA.

           CLOSE SUCURSA.

           IF NOT FS-SUCURSA-OK
              DISPLAY 'ERROR EN CLOSE DE SUCURSA: ' FS-SUCURSA
           END-IF.

       8300-CERRAR-ARCH-SUCURSA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       8400-CERRAR-ARCH-SUCURSB.

           CLOSE SUCURSB.

           IF NOT FS-SUCURSB-OK
              DISPLAY 'ERROR EN CLOSE DE SUCURSB: ' FS-SUCURSB
           END-IF.

       8400-CERRAR-ARCH-SUCURSB-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       9000-TOTALES-ARCHIVO.

           DISPLAY '================================================'.
           DISPLAY 'CANT. REG. LEIDOS SUCURSAL A: ' WS-SUCURSA-CANT-REG.
           DISPLAY 'CANT. REG. LEIDOS SUCURSAL B: ' WS-SUCURSB-CANT-REG.

           DISPLAY 'CANT. REG. GRABADOS         : ' WS-SALIDA-CANT-REG.

           DISPLAY '================================================'.
       9000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL13EJ01.
