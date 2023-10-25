      ******************************************************************
      * Author: EMILIANO TOMASI
      * Date: 20/10/2023
      * Purpose: CLASE 20 - EJERCICIO 2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL20EJ02.
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

       SELECT SAL-RESULTADO
           ASSIGN TO '../RESULTADO.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-RESULTADO
           RECORD KEY IS SAL-RES-ID-EMPLEADO.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENT-EMPLEADOS.
       01 ENT-EMPLEADOS-REG.
          05 ENT-EMP-ID-EMPLEADO            PIC 9(08).
          05 ENT-EMP-APELLIDO               PIC X(25).
          05 ENT-EMP-NOMBRE                 PIC X(25).
          05 ENT-EMP-ESTADO                 PIC X(01).
          05 ENT-EMP-DIRECCION              PIC X(50).
          05 ENT-EMP-COD-POSTAL             PIC 9(04).

       FD SAL-RESULTADO.
       01 SAL-RESULTADO-REG.
          05 SAL-RES-ID-EMPLEADO            PIC 9(08).
          05 SAL-RES-APELLIDO               PIC X(25).
          05 SAL-RES-NOMBRE                 PIC X(25).
          05 SAL-RES-ESTADO                 PIC X(01).
          05 SAL-RES-DIRECCION              PIC X(50).
          05 SAL-RES-COD-POSTAL             PIC 9(04).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-FILE-OK            VALUE '00'.
             88 FS-EMPLEADOS-FILE-EOF           VALUE '10'.
             88 FS-EMPLEADOS-FILE-NFD           VALUE '35'.
             88 FS-EMPLEADOS-CLAVE-INV          VALUE '21'.
             88 FS-EMPLEADOS-CLAVE-DUP          VALUE '22'.
             88 FS-EMPLEADOS-CLAVE-NFD          VALUE '23'.
          05 FS-RESULTADO                   PIC X(2).
             88 FS-RESULTADO-FILE-OK            VALUE '00'.
             88 FS-RESULTADO-FILE-EOF           VALUE '10'.
             88 FS-RESULTADO-FILE-NFD           VALUE '35'.
             88 FS-RESULTADO-CLAVE-INV          VALUE '21'.
             88 FS-RESULTADO-CLAVE-DUP          VALUE '22'.
             88 FS-RESULTADO-CLAVE-NFD          VALUE '23'.

       77 WS-ID-EMPLEADO                    PIC 9(08).
       77 WS-CONT-REG-RESULTADO             PIC 9(04) VALUE 0.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-EMPLEADOS-FILE-OK AND FS-RESULTADO-FILE-OK

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

           PERFORM 1200-ABRIR-RESULTADO
              THRU 1200-ABRIR-RESULTADO-FIN.

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
       1200-ABRIR-RESULTADO.

           OPEN OUTPUT SAL-RESULTADO.

           EVALUATE FS-RESULTADO
               WHEN '00'
                    CONTINUE
               WHEN '35'
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE RESULTADO'
                    DISPLAY 'FILE STATUS: ' FS-RESULTADO
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE RESULTADO'
                    DISPLAY 'FILE STATUS: ' FS-RESULTADO
           END-EVALUATE.

       1200-ABRIR-RESULTADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2000-BUSCAR-EMPLEADO.

           MOVE WS-ID-EMPLEADO          TO ENT-EMP-ID-EMPLEADO.

           PERFORM 2100-LEER-EMPLEADOS
              THRU 2100-LEER-EMPLEADOS-FIN.

       2000-BUSCAR-EMPLEADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS KEY IS ENT-EMP-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
                    PERFORM 2105-MOSTRAR-DATOS
                       THRU 2105-MOSTRAR-DATOS-FIN
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
       2105-MOSTRAR-DATOS.

           DISPLAY "--------------------------------------------------".

           DISPLAY " ID EMPLEADO: " ENT-EMP-ID-EMPLEADO.
           DISPLAY " APELLIDO   : " ENT-EMP-APELLIDO.
           DISPLAY " NOMBRE     : " ENT-EMP-NOMBRE.
           DISPLAY " ESTADO     : " ENT-EMP-ESTADO.
           DISPLAY " DIRECCION  : " ENT-EMP-DIRECCION.
           DISPLAY " COD. POSTAL: " ENT-EMP-COD-POSTAL.

           DISPLAY "--------------------------------------------------".

           IF (ENT-EMP-ESTADO EQUAL 'A')

               MOVE ENT-EMPLEADOS-REG       TO SAL-RESULTADO-REG
               PERFORM 2210-ESCRIBIR-RESULTADO
                  THRU 2210-ESCRIBIR-RESULTADO-FIN

           END-IF.

       2105-MOSTRAR-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2210-ESCRIBIR-RESULTADO.

           WRITE SAL-RESULTADO-REG.

           EVALUATE TRUE
               WHEN FS-RESULTADO-FILE-OK
                    ADD 1                   TO  WS-CONT-REG-RESULTADO
               WHEN FS-RESULTADO-CLAVE-DUP
                    DISPLAY 'EL EMPLADO YA EXISTE EN EL '-
                    'ARCHIVO RESULTADO'
              WHEN OTHER
                   DISPLAY 'ERROR AL ESCRIBIR RESULTADO.VSAM - '-
                  'FILE-STATUS: ' FS-RESULTADO ' - '-
                  'ID-EMPLEADO: ' SAL-RES-ID-EMPLEADO
           END-EVALUATE.

       2210-ESCRIBIR-RESULTADO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY " ".
           DISPLAY 'CANTIDAD DE REGISTROS RESULTADO: '
                   WS-CONT-REG-RESULTADO.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

           DISPLAY " ".
           DISPLAY '### FIN DEL PROGRAMA ###'.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS
                 SAL-RESULTADO.

           IF NOT FS-EMPLEADOS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-RESULTADO-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO RESULTADO: ' FS-RESULTADO
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL20EJ02.
