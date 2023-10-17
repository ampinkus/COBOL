      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 25/09/2023
      * Purpose: CLASE 14 - EJERCICIO 1
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL14EJ01.
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

       FD SAL-APAREO.
       01 WS-SAL-APAREO                     PIC X(133).

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
          05 FS-APAREO                      PIC X(2).
             88 FS-APAREO-OK                    VALUE '00'.
             88 FS-APAREO-EOF                   VALUE '10'.
          05 FS-ERROR                       PIC X(2).
             88 FS-ERROR-OK                     VALUE '00'.
             88 FS-ERROR-EOF                    VALUE '10'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-EMPLEADOS          PIC 9(04) VALUE 0.
          05 WS-CONT-REG-DIRECCIONES        PIC 9(04) VALUE 0.
          05 WS-CONT-REG-APAREO             PIC 9(04) VALUE 0.
          05 WS-CONT-REG-ERROR              PIC 9(04) VALUE 0.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

      * Si los 3 archivos se puedieron abrir correctamente
           IF FS-EMPLEADOS-OK AND FS-DIRECCIONES-OK AND FS-APAREO-OK
      * Proceso el programa hasta que alguno de los dos archivos de entrada
      * llego hasta su final.
              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                UNTIL FS-EMPLEADOS-EOF
                   OR FS-DIRECCIONES-EOF

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.
      * Donde abro los archivos de empleados, direcciones y apareo.
      * En la apertura d empleados y direcciones elo el primer registro.
      * En el archivo apareo guardo los separadores y el título.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-FIN.

           PERFORM 1200-ABRIR-DIRECCIONES
              THRU 1200-ABRIR-DIRECCIONES-FIN.

           PERFORM 1300-ABRIR-APAREO
              THRU 1300-ABRIR-APAREO-FIN.

      *  Es:  WS-SAL-APA-SEPARADOR     PIC X(133) VALUES ALL "-".
      *  escribo la línea de separacion superior
           MOVE WS-SAL-APA-SEPARADOR        TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

      * Escribo los títulos del archivo Apareo
           MOVE WS-SAL-APA-TITULOS          TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

      * Escribo la línea de separacion
           MOVE WS-SAL-APA-SEPARADOR        TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

           PERFORM 1400-ABRIR-ERROR
              THRU 1400-ABRIR-ERROR-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.

           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
      * Si pude abrir el archivo de empleados leo el primer registro
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
                    CONTINUE
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
      * Si pude abrir direcciones leo el primer registro
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
                    CONTINUE
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
       2000-PROCESAR-PROGRAMA.
      * Este proceso se repite hasta que uno de los dos archivos que
      * estoy apareando llegue a su fin.
      * Tanto el archivo de empleados como el de direcciones está indexado
      * de menor a mayor, si el ID de direcciones es myor que el del
      * empleado significa que el empledo ya no tiene una dirección
           EVALUATE TRUE
      * Si el ID del empleado es mayor que el ID de direcciones leo un
      * registro del archivo de direcciones para ver si hay una coincidencia
                 WHEN WS-ENT-EMP-ID-EMPLEADO > WS-ENT-DIR-ID-EMPLEADO
                      PERFORM 1210-LEER-DIRECCIONES
                         THRU 1210-LEER-DIRECCIONES-FIN
      * Si el ID del empleado es menor que el ID de direcciones
      * no hay dirección para ese empleado y grabo el error
                 WHEN WS-ENT-EMP-ID-EMPLEADO < WS-ENT-DIR-ID-EMPLEADO
                      PERFORM 2400-GRABAR-ERROR-SIN-DIR
                         THRU 2400-GRABAR-ERROR-SIN-DIR-FIN
      * Luego de grabar el error porque el empleado no tiene una
      * dirección tengo que volver a ler otro registro de empleado
                      PERFORM 1110-LEER-EMPLEADOS
                         THRU 1110-LEER-EMPLEADOS-FIN
      * Si el ID del empleado coincide con el ID de la dirección y si
      * es un empleado activo grabo el registro en Apareo
                 WHEN WS-ENT-EMP-ID-EMPLEADO = WS-ENT-DIR-ID-EMPLEADO
                      IF WS-ENT-EMP-ESTADO EQUAL 'A'
                         PERFORM 2200-GRABAR-APAREO
                            THRU 2200-GRABAR-APAREO-FIN
                      END-IF
      * Un vez grabado el registro de apareo tengo que leeer un
      * registro de empleado nuevo
                      PERFORM 1110-LEER-EMPLEADOS
                         THRU 1110-LEER-EMPLEADOS-FIN

           END-EVALUATE.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-GRABAR-APAREO.
      * Muevo los detalles del archivo de empleados y direccciónes
      * al archivo de apareo
      *    05 WS-SAL-APA-DETALLE.
      *       10 WS-SAL-APA-ID-EMPLEADO      PIC 9(08).
      *       10 FILLER                      PIC X(06) VALUE '    | '.
      *       10 WS-SAL-APA-NOMBRE           PIC X(25).
      *       10 FILLER                      PIC X(03) VALUE ' | '.
      *       10 WS-SAL-APA-APELLIDO         PIC X(25).
      *       10 FILLER                      PIC X(03) VALUE ' | '.
      *       10 WS-SAL-APA-DIRECCION        PIC X(50).
      *       10 FILLER                      PIC X(09) VALUE ' |       '.
      *       10 WS-SAL-APA-COD-POSTAL       PIC 9(04).

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-APA-ID-EMPLEADO.
           MOVE WS-ENT-EMP-NOMBRE           TO WS-SAL-APA-NOMBRE.
           MOVE WS-ENT-EMP-APELLIDO         TO WS-SAL-APA-APELLIDO.
           MOVE WS-ENT-DIR-DIRECCION        TO WS-SAL-APA-DIRECCION.
           MOVE WS-ENT-DIR-COD-POSTAL       TO WS-SAL-APA-COD-POSTAL.

      * Muevo todo el grupo 05 WS-SAL-APA-DETALLE a la variable
      * 01 WS-SAL-APAREO     PIC X(133).
           MOVE WS-SAL-APA-DETALLE          TO WS-SAL-APAREO.
           PERFORM 2210-ESCRIBIR-APAREO
              THRU 2210-ESCRIBIR-APAREO-FIN.

       2200-GRABAR-APAREO-FIN.
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
      * Envío el ID del empleado al archivo de error: error.cpy
      * 01 WS-SALIDA-ERROR.
      *    05 WS-SAL-ERR-SIN-DIRECCION.
      *       10 FILLER       PIC X(15)   VALUE 'El empleado ID '.
      *       10 WS-SAL-ERR-ID-EMPLEADO      PIC X(08).
      *       10 FILLER       PIC X(19)   VALUE ' no posee direccion'.

           MOVE WS-ENT-EMP-ID-EMPLEADO      TO WS-SAL-ERR-ID-EMPLEADO.
      * Envio todo el grupo 05 WS-SAL-ERR-SIN-DIRECCION a la variable
      * 01 WS-SAL-ERROR   PIC X(100).
           MOVE WS-SAL-ERR-SIN-DIRECCION    TO WS-SAL-ERROR.

           PERFORM 2410-ESCRIBIR-ERROR
              THRU 2410-ESCRIBIR-ERROR-FIN.

       2400-GRABAR-ERROR-SIN-DIR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2410-ESCRIBIR-ERROR.
      * Escribo todo el grupo 01 de error.cpy
           WRITE WS-SAL-ERROR.

           IF FS-ERROR-OK
              ADD 1                         TO  WS-CONT-REG-ERROR
           ELSE
              DISPLAY 'ERROR AL ESCRIBIR ERROR.TXT: ' FS-ERROR
           END-IF.

       2410-ESCRIBIR-ERROR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS   : '
                   WS-CONT-REG-EMPLEADOS.
           DISPLAY 'CANTIDAD DE REGISTROS DIRECCIONES : '
                   WS-CONT-REG-DIRECCIONES.
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
                 SAL-APAREO
                 SAL-ERROR.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-DIRECCIONES-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DIRECCIONES: '
                      FS-DIRECCIONES
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

       END PROGRAM CL14EJ01.
