      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 25/09/2023
      * Purpose: CLASE 15 - EJERCICIO 1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL15EJ01.
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

       SELECT ENT-VENTAS
           ASSIGN TO '../VENTAS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-VENTAS.

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

       FD ENT-VENTAS.
       01 WS-ENT-VENTAS.
          05 WS-ENT-VEN-ID-TICKET           PIC 9(15).
          05 WS-ENT-VEN-ID-EMPLEADO         PIC 9(08).
          05 WS-ENT-VEN-SECTOR              PIC X(20).
          05 WS-ENT-VEN-IMP-VENTA           PIC 9(08)V9(02).

       FD SAL-APAREO.
       01 WS-SAL-APAREO                     PIC X(86).

       FD SAL-ERROR.
       01 WS-SAL-ERROR                      PIC X(50).

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
          05 FS-VENTAS                      PIC X(2).
             88 FS-VENTAS-OK                    VALUE '00'.
             88 FS-VENTAS-EOF                   VALUE '10'.
             88 FS-VENTAS-NFD                   VALUE '35'.
          05 FS-APAREO                      PIC X(2).
             88 FS-APAREO-OK                    VALUE '00'.
             88 FS-APAREO-EOF                   VALUE '10'.
          05 FS-ERROR                       PIC X(2).
             88 FS-ERROR-OK                     VALUE '00'.
             88 FS-ERROR-EOF                    VALUE '10'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-EMPLEADOS          PIC 9(04) VALUE 0.
          05 WS-CONT-REG-VENTAS             PIC 9(06) VALUE 0.
          05 WS-CONT-REG-APAREO             PIC 9(04) VALUE 0.
          05 WS-CONT-REG-ERROR              PIC 9(04) VALUE 0.

       77 WS-IMP-ACUM                       PIC 9(10)V9(02) VALUE 0.
       77 WS-FORMAT-IMPORTE                 PIC ZZZ.ZZ9.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-EMPLEADOS-OK AND FS-VENTAS-OK AND FS-APAREO-OK
              AND FS-ERROR-OK

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-EMPLEADOS-EOF
                  AND FS-VENTAS-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-FIN.

           PERFORM 1200-ABRIR-VENTAS
              THRU 1200-ABRIR-VENTAS-FIN.

           PERFORM 1300-ABRIR-APAREO
              THRU 1300-ABRIR-APAREO-FIN.

           MOVE WS-SAL-APA-SEPARADOR        TO WS-SAL-APAREO.
           PERFORM 2220-ESCRIBIR-APAREO
              THRU 2220-ESCRIBIR-APAREO-FIN.

           MOVE WS-SAL-APA-TITULOS          TO WS-SAL-APAREO.
           PERFORM 2220-ESCRIBIR-APAREO
              THRU 2220-ESCRIBIR-APAREO-FIN.

           MOVE WS-SAL-APA-SEPARADOR        TO WS-SAL-APAREO.
           PERFORM 2220-ESCRIBIR-APAREO
              THRU 2220-ESCRIBIR-APAREO-FIN.

           PERFORM 1400-ABRIR-ERROR
              THRU 1400-ABRIR-ERROR-FIN.

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
       1200-ABRIR-VENTAS.

           OPEN INPUT ENT-VENTAS.

           EVALUATE TRUE
               WHEN FS-VENTAS-OK
                    PERFORM 1210-LEER-VENTAS
                       THRU 1210-LEER-VENTAS-FIN
               WHEN FS-VENTAS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE VENTAS'
                    DISPLAY 'FILE STATUS: ' FS-VENTAS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE VENTAS'
                    DISPLAY 'FILE STATUS: ' FS-VENTAS
           END-EVALUATE.

       1200-ABRIR-VENTAS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1210-LEER-VENTAS.

           READ ENT-VENTAS.

            EVALUATE TRUE
               WHEN FS-VENTAS-OK
                    ADD 1                   TO WS-CONT-REG-VENTAS
               WHEN FS-VENTAS-EOF
                    MOVE 99999999           TO WS-ENT-VEN-ID-EMPLEADO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE VENTAS'
                    DISPLAY 'FILE STATUS: ' FS-VENTAS
           END-EVALUATE.

       1210-LEER-VENTAS-FIN.
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
       2000-PROCESAR-PROGRAMA.
      * Tanto el archivo de empleados como el de ventas está indexado
      * de menor a mayor.
      * Si el ID de empleado es menor que el ID de ventas
      * significa que el empleado no hizo ninguna venta.
      * Si el ID del empleado es mayor que el ID de ventas tengo que
      * leer otro registro de ventas para ver si hay una concidencia.
      * Si el ID del empleado coincide con el ID de ventas tengo que
      * revisar si se trata de un empleaod activo:
      *    Si es un empleado activo sumo el importe de la venta al empleado.
      *    Si no es un empleado activo es un error en la venbta ya que no
      *      fue realizada por un empleado activo.

      *---- Uso un evaluate True para contemplar las distintas
      *---- casuisticas

           EVALUATE TRUE
Caso1 *----En este caso tengo un empleado sin ventas
  |   *----Es un error. Grabo el error y leo un empleado
  |             WHEN WS-ENT-EMP-ID-EMPLEADO < WS-ENT-VEN-ID-EMPLEADO
  |   *         Grabo error en de la venta .
  |               PERFORM 2410-GRABAR-ERROR-EN-EMP
  |                  THRU 2410-GRABAR-ERROR-EN-EMP-FIN
                  PERFORM 1110-LEER-EMPLEADOS
                     THRU 1110-LEER-EMPLEADOS-FIN
  |
Caso1
Caso2 *----En este caso tengo que leer otro registro de ventas para
  |   *----  ver si hay una coincidencia
  |            WHEN WS-ENT-EMP-ID-EMPLEADO > WS-ENT-VEN-ID-EMPLEADO
  |              PERFORM 1210-LEER-VENTAS
  |                 THRU 1210-LEER-VENTAS-FIN
  |
Caso2
Caso3 *----En este caso tengo un empleado con una venta, tengo que ver
  |   *----  si es un empleado activo o no.
  |            WHEN WS-ENT-EMP-ID-EMPLEADO = WS-ENT-VEN-ID-EMPLEADO
  |                 IF WS-ENT-EMP-ESTADO EQUAL 'A'
  |   *----Calculo las ventas.
  |                    ADD WS-ENT-VEN-IMP-VENTA  TO WS-IMP-ACUM
                    ELSE
      *----La venta la hizo un empleado no activo es un error
                       PERFORM 2400-GRABAR-ERROR-EN-VENTA
                       THRU 2400-GRABAR-ERROR-EN-VENTA-FIN


  |
  |
  |
  |
  |
  |   *---- Grabo la salida con el resultado de las ventas.
  |                    PERFORM 2215-GRABAR-DATOS
  |                       THRU 2215-GRABAR-DATOS-FIN
  |                    PERFORM 1110-LEER-EMPLEADOS
  |                       THRU 1110-LEER-EMPLEADOS-FIN
  |                 ELSE
  |   *----Si hay ventas para un empleado inactivo, grabo error
  |
  |
  |   *----Descarto las ventas del empleado inactivo. Todas.
  |
  |
  |
  |
  |
  |                    PERFORM 1110-LEER-EMPLEADOS
  |                       THRU 1110-LEER-EMPLEADOS-FIN
  |
  |
  |                 END-IF
  |   *----Avanzo los archivos para continuar con las ventas del
  |   *----siguiente empleado.
Caso3
           END-EVALUATE.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2210-PROCESAR-VENTAS.

           IF WS-ENT-EMP-ID-EMPLEADO = WS-ENT-VEN-ID-EMPLEADO
              ADD WS-ENT-VEN-IMP-VENTA      TO WS-IMP-ACUM
           END-IF.

           PERFORM 1210-LEER-VENTAS
              THRU 1210-LEER-VENTAS-FIN.

       2210-PROCESAR-VENTAS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2215-GRABAR-DATOS.

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-APA-ID-EMPLEADO.
           MOVE WS-ENT-EMP-NOMBRE           TO WS-SAL-APA-NOMBRE.
           MOVE WS-ENT-EMP-APELLIDO         TO WS-SAL-APA-APELLIDO.
           MOVE WS-IMP-ACUM                 TO WS-SAL-APA-IMPORTE.

           MOVE WS-SAL-APA-DETALLE          TO WS-SAL-APAREO.
           PERFORM 2220-ESCRIBIR-APAREO
              THRU 2220-ESCRIBIR-APAREO-FIN.

       2215-GRABAR-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2220-ESCRIBIR-APAREO.

           WRITE WS-SAL-APAREO.

           IF FS-APAREO-OK
              ADD 1                         TO  WS-CONT-REG-APAREO
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR APAREO.TXT: ' FS-APAREO
           END-IF.

       2220-ESCRIBIR-APAREO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2400-GRABAR-ERROR-EN-VENTA.

           MOVE WS-ENT-VEN-ID-EMPLEADO      TO WS-SAL-ERR-ID-EMPLEADO
                                            OF WS-SAL-ERR-VENTAS.

           MOVE WS-SAL-ERR-VENTAS           TO WS-SAL-ERROR.
           PERFORM 2420-ESCRIBIR-ERROR
              THRU 2420-ESCRIBIR-ERROR-FIN.

       2400-GRABAR-ERROR-EN-VENTA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2410-GRABAR-ERROR-EN-EMP.

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-ERR-ID-EMPLEADO
                                            OF WS-SAL-ERR-EMPLEADO.

           MOVE WS-SAL-ERR-EMPLEADO         TO WS-SAL-ERROR.
           PERFORM 2420-ESCRIBIR-ERROR
              THRU 2420-ESCRIBIR-ERROR-FIN.

       2410-GRABAR-ERROR-EN-EMP-FIN.
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
       2430-GRABAR-ERROR-EN-VENTA2.

           MOVE WS-ENT-VEN-ID-EMPLEADO      TO WS-SAL-ERR-ID-EMPLEADO
                                            OF WS-SAL-ERR-VENTAS2.

           MOVE WS-SAL-ERR-VENTAS2          TO WS-SAL-ERROR.
           PERFORM 2420-ESCRIBIR-ERROR
              THRU 2420-ESCRIBIR-ERROR-FIN.

       2430-GRABAR-ERROR-EN-VENTA2-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           MOVE WS-CONT-REG-EMPLEADOS       TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS   : '
                   WS-FORMAT-IMPORTE.

           MOVE WS-CONT-REG-VENTAS          TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS VENTAS      : '
                   WS-FORMAT-IMPORTE.

           MOVE WS-CONT-REG-APAREO          TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS APAREADOS   : '
                   WS-FORMAT-IMPORTE.

           MOVE WS-CONT-REG-ERROR           TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS CON ERROR   : '
                   WS-FORMAT-IMPORTE.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS
                 ENT-VENTAS
                 SAL-APAREO
                 SAL-ERROR.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-VENTAS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO VENTAS: ' FS-VENTAS
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

       END PROGRAM CL15EJ01.
