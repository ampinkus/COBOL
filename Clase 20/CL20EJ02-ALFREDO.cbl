      ******************************************************************
      * Author: Alfredo
      * Date: 20/10/2023
      * Purpose: CLASE 20 - EJERCICIO 1: Se solicita un ID de empleado
      * y si el mismo existe y su estatus es A se graba en el archivo
      * SAL-EMPLEADOS.VSAM
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL20EJ01.
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

       SELECT SAL-EMPLEADOS
          ASSIGN TO '../SAL-EMPLEADOS.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-SAL-EMPLEADOS
           RECORD KEY IS SAL-EMP-ID-EMPLEADO.

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

       FD SAL-EMPLEADOS.
       01 SAL-EMPLEADOS-REG.
          05 SAL-EMP-ID-EMPLEADO            PIC 9(08).
          05 SAL-EMP-APELLIDO               PIC X(25).
          05 SAL-EMP-NOMBRE                 PIC X(25).
          05 SAL-EMP-ESTADO                 PIC X(01).
          05 SAL-EMP-DIRECCION              PIC X(50).
          05 SAL-EMP-COD-POSTAL             PIC 9(04).

       WORKING-STORAGE SECTION.
       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-FILE-OK            VALUE '00'.
             88 FS-EMPLEADOS-FILE-EOF           VALUE '10'.
             88 FS-EMPLEADOS-FILE-NFD           VALUE '35'.
             88 FS-EMPLEADOS-CLAVE-INV          VALUE '21'.
             88 FS-EMPLEADOS-CLAVE-DUP          VALUE '22'.
             88 FS-EMPLEADOS-CLAVE-NFD          VALUE '23'.

          05 FS-SAL-EMPLEADOS                   PIC X(2).
             88 FS-SAL-EMPLEADOS-FILE-OK           VALUE '00'.
             88 FS-SAL-EMPLEADOS-FILE-EOF          VALUE '10'.
             88 FS-SAL-EMPLEADOS-FILE-NFD          VALUE '35'.
             88 FS-SAL-EMPLEADOS-CLAVE-INV         VALUE '21'.
             88 FS-SAL-EMPLEADOS-CLAVE-DUP         VALUE '22'.
             88 FS-SAL-EMPLEADOS-CLAVE-NFD         VALUE '23'.

       77 WS-ID-EMPLEADO                    PIC 9(08).
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

      * Si la apertura del archivo fue exitosa
           IF FS-EMPLEADOS-FILE-OK AND FS-SAL-EMPLEADOS-FILE-OK

      * Ingresamos el ID de un ampleado
               DISPLAY 'INGRESA UN ID-EMPLEADO:'
               ACCEPT WS-ID-EMPLEADO
               DISPLAY " "
      * Buscamos al empleado, si el estado es A lo grabamos en un
      * archivo indexado VSAM
              PERFORM 2000-BUSCAR-EMPLEADO
                 THRU 2000-BUSCAR-EMPLEADO-FIN

           END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.
      * Abro los archivos archivo de ENT-EMPLEADOS y SAL-EMPLEADOS
           PERFORM 1100-ABRIR-ARCHIVOS
              THRU 1100-ABRIR-ARCHIVOS-FIN.

       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVOS.
           OPEN INPUT ENT-EMPLEADOS.
      * Chequeo si la apertura ENT-EMPLEADOS fue exitosa
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

           OPEN OUTPUT SAL-EMPLEADOS.
      * Chequeo si la apertura SAL-EMPLEADOS  fue exitosa
           EVALUATE TRUE
               WHEN FS-SAL-EMPLEADOS-FILE-OK
                    CONTINUE
               WHEN OTHER
                 DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SAL-EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-SAL-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-ARCHIVOS-FIN.
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
                    PERFORM 2200-MOSTRAR-DATOS
                       THRU 2200-MOSTRAR-DATOS-FIN
      * Si el estado del empleado es A voy a grabar el empleado
      * en el archivo de salida
                IF  ENT-EMP-ESTADO EQUAL "A"
                    PERFORM 2300-GRABAR-DATOS
                       THRU 2300-GRABAR-DATOS-EXIT
                END-IF

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
       2200-MOSTRAR-DATOS.

           DISPLAY "--------------------------------------------------".

           DISPLAY " ID EMPLEADO: " ENT-EMP-ID-EMPLEADO.
           DISPLAY " APELLIDO   : " ENT-EMP-APELLIDO.
           DISPLAY " NOMBRE     : " ENT-EMP-NOMBRE.
           DISPLAY " ESTADO     : " ENT-EMP-ESTADO.
           DISPLAY " DIRECCION  : " ENT-EMP-DIRECCION.
           DISPLAY " COD. POSTAL: " ENT-EMP-COD-POSTAL.

           DISPLAY "--------------------------------------------------".

       2200-MOSTRAR-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2300-GRABAR-DATOS.
      *    Muevo el registro de entrada al de salida
           MOVE ENT-EMPLEADOS-REG TO SAL-EMPLEADOS-REG.

      *    Grabo el registro de salida
           WRITE SAL-EMPLEADOS-REG.
           IF NOT FS-SAL-EMPLEADOS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO SAL-EMPLEADOS: '
            FS-SAL-EMPLEADOS
           END-IF.

       2300-GRABAR-DATOS-EXIT.
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

           CLOSE ENT-EMPLEADOS.
           IF NOT FS-EMPLEADOS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.


           CLOSE SAL-EMPLEADOS.
           IF NOT FS-SAL-EMPLEADOS-FILE-OK
            DISPLAY 'ERROR AL CERRAR ARCHIVO SAL-EMPLEADOS: '
            FS-SAL-EMPLEADOS
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL20EJ01.
