      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 25/09/2023
      * Purpose: CLASE 14 - EJERCICIO 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL14EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-EMPLEADOS
           ASSIGN TO '../EMPLEADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-EMPLEADOS.

       SELECT ENT-DIRECCIONES
           ASSIGN TO '../DIRECCIONES.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-DIRECCIONES.

       SELECT ENT-DATOS-EXTRA
           ASSIGN TO '../DATOS-EXTRA.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-DATOS-EXTRA.

       SELECT SAL-APAREO
           ASSIGN TO '../APAREO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-APAREO.

       SELECT SAL-ERROR
           ASSIGN TO '../ERROR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ERROR.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-EMPLEADOS.
       01 WS-ENT-EMPLEADOS.
          05 WS-ENT-EMP-ID-EMPLEADO         PIC 9(08).
          05 WS-ENT-EMP-NOMBRE              PIC X(25).
          05 WS-ENT-EMP-APELLIDO            PIC X(25).
          05 WS-ENT-EMP-ESTADO              PIC X(01).

       FD ENT-DIRECCIONES.
       01 WS-ENT-DIRECCIONES.
          05 WS-ENT-DIR-ID-EMPLEADO         PIC 9(08).
          05 WS-ENT-DIR-DIRECCION           PIC X(50).
          05 WS-ENT-DIR-COD-POSTAL          PIC 9(04).

       FD ENT-DATOS-EXTRA.
       01 WS-ENT-DATOS-EXTRA.
          05 WS-ENT-EXT-ID-EMPLEADO         PIC 9(08).
          05 WS-ENT-EXT-TIPO-DOC            PIC X(03).
          05 WS-ENT-EXT-NRO-DOC             PIC 9(08).
          05 WS-ENT-EXT-TELEFONO            PIC X(09).

       FD SAL-APAREO.
       01 WS-SAL-APAREO                     PIC X(166).

       FD SAL-ERROR.
       01 WS-SAL-ERROR                      PIC X(100).

       WORKING-STORAGE SECTION.

      * FORMATO DEL ARCHIVO DE SALIDA "APAREO.TXT"
          COPY APAREO.

      * FORMATO DEL ARCHIVO DE SALIDA "ERROR.TXT"
          COPY ERROR.

       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-OK                 VALUE '00'.
             88 FS-EMPLEADOS-EOF                VALUE '10'.
             88 FS-EMPLEADOS-NFD                VALUE '35'.
          05 FS-DIRECCIONES                 PIC X(2).
             88 FS-DIRECCIONES-OK               VALUE '00'.
             88 FS-DIRECCIONES-EOF              VALUE '10'.
             88 FS-DIRECCIONES-NFD              VALUE '35'.
          05 FS-DATOS-EXTRA                 PIC X(2).
             88 FS-DATOS-EXTRA-OK               VALUE '00'.
             88 FS-DATOS-EXTRA-EOF              VALUE '10'.
             88 FS-DATOS-EXTRA-NFD              VALUE '35'.
          05 FS-APAREO                      PIC X(2).
             88 FS-APAREO-OK                    VALUE '00'.
             88 FS-APAREO-EOF                   VALUE '10'.
          05 FS-ERROR                       PIC X(2).
             88 FS-ERROR-OK                     VALUE '00'.
             88 FS-ERROR-EOF                    VALUE '10'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-EMPLEADOS          PIC 9(04) VALUE 0.
          05 WS-CONT-REG-DIRECCIONES        PIC 9(04) VALUE 0.
          05 WS-CONT-REG-DATOS-EXTRA        PIC 9(04) VALUE 0.
          05 WS-CONT-REG-APAREO             PIC 9(04) VALUE 0.
          05 WS-CONT-REG-ERROR              PIC 9(04) VALUE 0.

       01 WS-DATOS-EXTRA                    PIC X(2).
          88 WS-DATOS-EXTRA-SI                  VALUE 'SI'.
          88 WS-DATOS-EXTRA-NO                  VALUE 'NO'.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-EMPLEADOS-OK AND FS-DIRECCIONES-OK AND FS-APAREO-OK
              AND FS-ERROR-OK AND FS-DATOS-EXTRA-OK

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-EMPLEADOS-EOF
                  AND FS-DIRECCIONES-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-FIN.

           PERFORM 1200-ABRIR-DIRECCIONES
              THRU 1200-ABRIR-DIRECCIONES-FIN.

           PERFORM 1300-ABRIR-APAREO
              THRU 1300-ABRIR-APAREO-FIN.

           MOVE WS-SAL-APA-SEPARADOR        TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

           MOVE WS-SAL-APA-TITULOS          TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

           MOVE WS-SAL-APA-SEPARADOR        TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

           PERFORM 1400-ABRIR-ERROR
              THRU 1400-ABRIR-ERROR-FIN.

           PERFORM 1500-ABRIR-DATOS-EXTRA
              THRU 1500-ABRIR-DATOS-EXTRA-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.

           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    PERFORM 1110-LEER-EMPLEADOS
                       THRU 1110-LEER-EMPLEADOS-FIN
               WHEN FS-EMPLEADOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1110-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
                    ADD 1                   TO WS-CONT-REG-EMPLEADOS
               WHEN FS-EMPLEADOS-EOF
                    MOVE 99999999           TO WS-ENT-EMP-ID-EMPLEADO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1110-LEER-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-DIRECCIONES.

           OPEN INPUT ENT-DIRECCIONES.

           EVALUATE TRUE
               WHEN FS-DIRECCIONES-OK
                    PERFORM 1210-LEER-DIRECCIONES
                       THRU 1210-LEER-DIRECCIONES-FIN
               WHEN FS-DIRECCIONES-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE DIRECCIONES'
                    DISPLAY 'FILE STATUS: ' FS-DIRECCIONES
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE DIRECCIONES'
                    DISPLAY 'FILE STATUS: ' FS-DIRECCIONES
           END-EVALUATE.

       1200-ABRIR-DIRECCIONES-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1210-LEER-DIRECCIONES.

           READ ENT-DIRECCIONES.

            EVALUATE TRUE
               WHEN FS-DIRECCIONES-OK
                    ADD 1                   TO WS-CONT-REG-DIRECCIONES
               WHEN FS-DIRECCIONES-EOF
                    MOVE 99999999           TO WS-ENT-DIR-ID-EMPLEADO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE DIRECCIONES'
                    DISPLAY 'FILE STATUS: ' FS-DIRECCIONES
           END-EVALUATE.

       1210-LEER-DIRECCIONES-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1300-ABRIR-APAREO.

           OPEN OUTPUT SAL-APAREO.

           EVALUATE FS-APAREO
               WHEN '00'
                    CONTINUE
               WHEN '35'
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE APAREO'
                    DISPLAY 'FILE STATUS: ' FS-APAREO
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE APAREO'
                    DISPLAY 'FILE STATUS: ' FS-APAREO
           END-EVALUATE.

       1300-ABRIR-APAREO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1400-ABRIR-ERROR.

           OPEN OUTPUT SAL-ERROR.

           EVALUATE FS-ERROR
               WHEN '00'
                    CONTINUE
               WHEN '35'
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ERROR'
                    DISPLAY 'FILE STATUS: ' FS-ERROR
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ERROR'
                    DISPLAY 'FILE STATUS: ' FS-ERROR
           END-EVALUATE.

       1400-ABRIR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1500-ABRIR-DATOS-EXTRA.

           OPEN INPUT ENT-DATOS-EXTRA.

           EVALUATE TRUE
               WHEN FS-DATOS-EXTRA-OK
                    PERFORM 1510-LEER-DATOS-EXTRA
                       THRU 1510-LEER-DATOS-EXTRA-FIN
               WHEN FS-DATOS-EXTRA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
           END-EVALUATE.

       1500-ABRIR-DATOS-EXTRA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1510-LEER-DATOS-EXTRA.

           READ ENT-DATOS-EXTRA.

            EVALUATE TRUE
               WHEN FS-DATOS-EXTRA-OK
                    ADD 1                   TO WS-CONT-REG-DATOS-EXTRA
               WHEN FS-DATOS-EXTRA-EOF
                    MOVE 99999999           TO WS-ENT-EXT-ID-EMPLEADO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
           END-EVALUATE.

       1510-LEER-DATOS-EXTRA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           EVALUATE TRUE

               WHEN WS-ENT-EMP-ID-EMPLEADO > WS-ENT-DIR-ID-EMPLEADO
      *             GRABAR ARCHIVO DE ERRORES DE DIRECCION
                    PERFORM 2400-GRABAR-ERROR-SIN-DIR
                       THRU 2400-GRABAR-ERROR-SIN-DIR-FIN

                    PERFORM 1210-LEER-DIRECCIONES
                       THRU 1210-LEER-DIRECCIONES-FIN

               WHEN WS-ENT-EMP-ID-EMPLEADO < WS-ENT-DIR-ID-EMPLEADO
      *             GRABAR ARCHIVO DE ERRORES DE EMPLEADOS
                    PERFORM 2410-GRABAR-ERROR-SIN-EMP
                       THRU 2410-GRABAR-ERROR-SIN-EMP-FIN

                    PERFORM 1110-LEER-EMPLEADOS
                       THRU 1110-LEER-EMPLEADOS-FIN

               WHEN WS-ENT-EMP-ID-EMPLEADO = WS-ENT-DIR-ID-EMPLEADO
                    IF WS-ENT-EMP-ESTADO EQUAL 'A'
                       SET WS-DATOS-EXTRA-NO TO TRUE

                       PERFORM 2200-PROCESAR-APAREO
                          THRU 2200-PROCESAR-APAREO-FIN
                         UNTIL FS-DATOS-EXTRA-EOF
                            OR WS-DATOS-EXTRA-SI
                            OR WS-ENT-EMP-ID-EMPLEADO
                             < WS-ENT-EXT-ID-EMPLEADO

                       IF WS-DATOS-EXTRA-SI
                          PERFORM 2205-PROCESAR-APAREO
                             THRU 2205-PROCESAR-APAREO-FIN
                       ELSE
      *                    GRABAR ARCHIVO DE ERRORES DE DATOS-EXTRA
                           PERFORM 2430-GRABAR-ERROR-SIN-EXTRA
                              THRU 2430-GRABAR-ERROR-SIN-EXTRA-FIN
                       END-IF
                   END-IF

                   PERFORM 1110-LEER-EMPLEADOS
                      THRU 1110-LEER-EMPLEADOS-FIN

                   PERFORM 1210-LEER-DIRECCIONES
                      THRU 1210-LEER-DIRECCIONES-FIN

           END-EVALUATE.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-APAREO.

           IF WS-ENT-EMP-ID-EMPLEADO = WS-ENT-EXT-ID-EMPLEADO
              SET WS-DATOS-EXTRA-SI         TO TRUE
           ELSE
              PERFORM 1510-LEER-DATOS-EXTRA
                 THRU 1510-LEER-DATOS-EXTRA-FIN
           END-IF.

       2200-PROCESAR-APAREO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2205-PROCESAR-APAREO.

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-APA-ID-EMPLEADO.
           MOVE WS-ENT-EMP-NOMBRE           TO WS-SAL-APA-NOMBRE.
           MOVE WS-ENT-EMP-APELLIDO         TO WS-SAL-APA-APELLIDO.
           MOVE WS-ENT-DIR-DIRECCION        TO WS-SAL-APA-DIRECCION.
           MOVE WS-ENT-DIR-COD-POSTAL       TO WS-SAL-APA-COD-POSTAL.
           MOVE WS-ENT-EXT-TIPO-DOC         TO WS-SAL-APA-TIPO-DOC.
           MOVE WS-ENT-EXT-NRO-DOC          TO WS-SAL-APA-NRO-DOC.
           MOVE WS-ENT-EXT-TELEFONO         TO WS-SAL-APA-TELEFONO.

           MOVE WS-SAL-APA-DETALLE          TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

       2205-PROCESAR-APAREO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2210-ESCRIBIR-APAREO.

           WRITE WS-SAL-APAREO.

           IF FS-APAREO-OK
              ADD 1                         TO  WS-CONT-REG-APAREO
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR APAREO.TXT: ' FS-APAREO
           END-IF.

       2210-ESCRIBIR-APAREO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2400-GRABAR-ERROR-SIN-DIR.

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-ERR-ID-EMPLEADO
                                            OF WS-SAL-ERR-SIN-DIRECCION.

           MOVE WS-SAL-ERR-SIN-DIRECCION    TO WS-SAL-ERROR.
           PERFORM 2420-ESCRIBIR-ERROR
              THRU 2420-ESCRIBIR-ERROR-FIN.

       2400-GRABAR-ERROR-SIN-DIR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2410-GRABAR-ERROR-SIN-EMP.

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-ERR-ID-EMPLEADO
                                            OF WS-SAL-ERR-SIN-EMPLEADO.

           MOVE WS-SAL-ERR-SIN-EMPLEADO     TO WS-SAL-ERROR.
           PERFORM 2420-ESCRIBIR-ERROR
              THRU 2420-ESCRIBIR-ERROR-FIN.

       2410-GRABAR-ERROR-SIN-EMP-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2420-ESCRIBIR-ERROR.

           WRITE WS-SAL-ERROR.

           IF FS-ERROR-OK
              ADD 1                         TO  WS-CONT-REG-ERROR
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR ERROR.TXT: ' FS-ERROR
           END-IF.

       2420-ESCRIBIR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2430-GRABAR-ERROR-SIN-EXTRA.

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-ERR-ID-EMPLEADO
                                            OF WS-SAL-ERR-SIN-EXTRA.

           MOVE WS-SAL-ERR-SIN-EXTRA        TO WS-SAL-ERROR.
           PERFORM 2420-ESCRIBIR-ERROR
              THRU 2420-ESCRIBIR-ERROR-FIN.

       2430-GRABAR-ERROR-SIN-EXTRA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS   : '
                   WS-CONT-REG-EMPLEADOS.
           DISPLAY 'CANTIDAD DE REGISTROS DIRECCIONES : '
                   WS-CONT-REG-DIRECCIONES.
           DISPLAY 'CANTIDAD DE REGISTROS DATOS-EXTRA : '
                   WS-CONT-REG-DATOS-EXTRA
           DISPLAY 'CANTIDAD DE REGISTROS APAREADOS   : '
                   WS-CONT-REG-APAREO.
           DISPLAY 'CANTIDAD DE REGISTROS CON ERROR   : '
                   WS-CONT-REG-ERROR.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS
                 ENT-DIRECCIONES
                 ENT-DATOS-EXTRA
                 SAL-APAREO
                 SAL-ERROR.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-DIRECCIONES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DIRECCIONES: '
                      FS-DIRECCIONES
           END-IF.

           IF NOT FS-DATOS-EXTRA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DATOS-EXTRA: '
                      FS-DATOS-EXTRA
           END-IF.

           IF NOT FS-APAREO-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO APAREO: ' FS-APAREO
           END-IF.

           IF NOT FS-ERROR-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ERROR: ' FS-ERROR
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL14EJ02.
