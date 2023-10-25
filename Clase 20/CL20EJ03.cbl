      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 20/10/2023
      * Purpose: CLASE 20 - EJERCICIO 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL20EJ03.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENT-EMPLEADOS
           ASSIGN TO '../EMPLEADOS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-EMPLEADOS
           RECORD KEY IS ENT-EMP-ID-EMPLEADO.

       SELECT ENT-DATOS-EXTRA
           ASSIGN TO '../DATOS-EXTRA.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-DATOS-EXTRA
           RECORD KEY IS ENT-EXT-ID-EMPLEADO.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-EMPLEADOS.
       01 ENT-EMPLEADOS-REG.
          05 ENT-EMP-ID-EMPLEADO            PIC 9(08).
          05 ENT-EMP-NOMBRE                 PIC X(25).
          05 ENT-EMP-APELLIDO               PIC X(25).
          05 ENT-EMP-ESTADO                 PIC X(01).
          05 ENT-EMP-DIRECCION              PIC X(50).
          05 ENT-EMP-COD-POSTAL             PIC 9(04).

       FD ENT-DATOS-EXTRA.
       01 ENT-DATOS-EXTRA-REG.
          05 ENT-EXT-ID-EMPLEADO            PIC 9(08).
          05 ENT-EXT-TIPO-DOC               PIC X(03).
          05 ENT-EXT-NRO-DOC                PIC X(08).
          05 ENT-EXT-TELEFONO               PIC X(09).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-FILE-OK            VALUE '00'.
             88 FS-EMPLEADOS-FILE-EOF           VALUE '10'.
             88 FS-EMPLEADOS-FILE-NFD           VALUE '35'.
             88 FS-EMPLEADOS-CLAVE-INV          VALUE '21'.
             88 FS-EMPLEADOS-CLAVE-DUP          VALUE '22'.
             88 FS-EMPLEADOS-CLAVE-NFD          VALUE '23'.
          05 FS-DATOS-EXTRA                 PIC X(2).
             88 FS-DATOS-EXTRA-FILE-OK          VALUE '00'.
             88 FS-DATOS-EXTRA-FILE-EOF         VALUE '10'.
             88 FS-DATOS-EXTRA-FILE-NFD         VALUE '35'.
             88 FS-DATOS-EXTRA-CLAVE-INV        VALUE '21'.
             88 FS-DATOS-EXTRA-CLAVE-DUP        VALUE '22'.
             88 FS-DATOS-EXTRA-CLAVE-NFD        VALUE '23'.

       77 WS-ID-EMPLEADO                    PIC 9(08).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-EMPLEADOS-FILE-OK

               DISPLAY 'INGRESA UN ID-EMPLEADO:'
               ACCEPT WS-ID-EMPLEADO
               DISPLAY " "

              PERFORM 2000-BUSCAR-EMPLEADO
                 THRU 2000-BUSCAR-EMPLEADO-FIN

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-FIN.

           PERFORM 1200-ABRIR-DIRECCIONES
              THRU 1200-ABRIR-DIRECCIONES-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.

           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
                    CONTINUE
               WHEN FS-EMPLEADOS-FILE-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-DIRECCIONES.

           OPEN INPUT ENT-DATOS-EXTRA.

           EVALUATE FS-DATOS-EXTRA
               WHEN '00'
                    CONTINUE
               WHEN '35'
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
           END-EVALUATE.

       1200-ABRIR-DIRECCIONES-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-BUSCAR-EMPLEADO.

           DISPLAY "--------------------------------------------------".

           MOVE WS-ID-EMPLEADO              TO ENT-EMP-ID-EMPLEADO.

           PERFORM 2100-LEER-EMPLEADOS
              THRU 2100-LEER-EMPLEADOS-FIN.

           IF (ENT-EMP-ESTADO EQUAL 'A')

               PERFORM 2105-MOSTRAR-EMPLEADO
                  THRU 2105-MOSTRAR-EMPLEADO-FIN

               MOVE WS-ID-EMPLEADO          TO ENT-EXT-ID-EMPLEADO

               PERFORM 2200-LEER-DATOS-EXTRA
                  THRU 2200-LEER-DATOS-EXTRA-FIN

           ELSE
                DISPLAY "EL EMPLEADO SE ENCUENTRA DADO DE BAJA"
           END-IF.

           DISPLAY "--------------------------------------------------".

       2000-BUSCAR-EMPLEADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS KEY IS ENT-EMP-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
                    CONTINUE
               WHEN FS-EMPLEADOS-CLAVE-INV
                   DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO"
               WHEN FS-EMPLEADOS-CLAVE-DUP
                   DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                           "DUPLICADO"
               WHEN FS-EMPLEADOS-CLAVE-NFD
                   DISPLAY "ERROR: EL ID INGRESADO NO EXISTE"
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       2100-LEER-EMPLEADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2105-MOSTRAR-EMPLEADO.

           DISPLAY " ID EMPLEADO: " ENT-EMP-ID-EMPLEADO.
           DISPLAY " NOMBRE     : " ENT-EMP-NOMBRE.
           DISPLAY " APELLIDO   : " ENT-EMP-APELLIDO.
           DISPLAY " ESTADO     : " ENT-EMP-ESTADO.
           DISPLAY " DIRECCION  : " ENT-EMP-DIRECCION.
           DISPLAY " COD. POSTAL: " ENT-EMP-COD-POSTAL.

       2105-MOSTRAR-EMPLEADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-LEER-DATOS-EXTRA.

           READ ENT-DATOS-EXTRA KEY IS ENT-EXT-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-DATOS-EXTRA-FILE-OK
                    PERFORM 2205-MOSTRAR-DATOS-EXTRA
                       THRU 2205-MOSTRAR-DATOS-EXTRA-FIN
               WHEN FS-DATOS-EXTRA-CLAVE-INV
                    DISPLAY "ERROR: EL ID INGRESADO ES INVALIDO "-
                             "EN ARCHIVO DE DATOS EXTRA"
               WHEN FS-DATOS-EXTRA-CLAVE-DUP
                    DISPLAY "ERROR: EL ID INGRESADO SE ENCUENTRA "-
                            "DUPLICADO EN ARCHIVO DE DATOS EXTRA"
               WHEN FS-DATOS-EXTRA-CLAVE-NFD
                    DISPLAY "ERROR: NO POSEE DATOS EXTRA"
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       2200-LEER-DATOS-EXTRA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2205-MOSTRAR-DATOS-EXTRA.

           DISPLAY " DOCUMENT   : " ENT-EXT-TIPO-DOC
                  " " ENT-EXT-NRO-DOC.
           DISPLAY " TELEFONO   : " ENT-EXT-TELEFONO.

       2205-MOSTRAR-DATOS-EXTRA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

           DISPLAY " ".
           DISPLAY '### FIN DEL PROGRAMA ###'.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS
                 ENT-DATOS-EXTRA.

           IF NOT FS-EMPLEADOS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-DATOS-EXTRA-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DATOS-EXTRA: '
                      FS-DATOS-EXTRA
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL20EJ03.
