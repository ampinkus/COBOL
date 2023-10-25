      ******************************************************************
      * Author: Alfredo
      * Date: 20/10/2023
      * Purpose: CLASE 20 - EJERCICIO 1: Se solicita un ID de empleado
      * y si el mismo existe y su estatus es A se busca si tiene
      * información extra en el archivo DATOS-EXTRA.VSAM y se graba
      * la información original junto a la extra en el archivo
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

       SELECT ENT-DATOS-EXTRA
           ASSIGN TO '../DATOS-EXTRA.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           FILE STATUS IS FS-DATOS-EXTRA
           RECORD KEY IS ENT-EXT-ID-EMPLEADO.

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
      * Los datos del archivo de empleados
       01 SAL-EMPLEADOS-REG.
           05 SAL-EMP-ID-EMPLEADO            PIC 9(08).
           05 SAL-EMP-APELLIDO               PIC X(25).
           05 SAL-EMP-NOMBRE                 PIC X(25).
           05 SAL-EMP-ESTADO                 PIC X(01).
           05 SAL-EMP-DIRECCION              PIC X(50).
           05 SAL-EMP-COD-POSTAL             PIC 9(04).
      * Agrego los datos extra por si existen
           05 SAL-EMP-TIPO-DOC               PIC X(03).
           05 SAL-EMP-NRO-DOC                PIC X(08).
           05 SAL-EMP-TELEFONO               PIC X(09).

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

          05 FS-SAL-EMPLEADOS                PIC X(2).
             88 FS-SAL-EMPLEADOS-FILE-OK        VALUE '00'.
             88 FS-SAL-EMPLEADOS-FILE-EOF       VALUE '10'.
             88 FS-SAL-EMPLEADOS-FILE-NFD       VALUE '35'.
             88 FS-SAL-EMPLEADOS-CLAVE-INV      VALUE '21'.
             88 FS-SAL-EMPLEADOS-CLAVE-DUP      VALUE '22'.
             88 FS-SAL-EMPLEADOS-CLAVE-NFD      VALUE '23'.

          05 FS-DATOS-EXTRA                  PIC X(2).
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

      * Si la apertura de los archivos fue exitosa
           IF FS-EMPLEADOS-FILE-OK AND FS-SAL-EMPLEADOS-FILE-OK AND
              FS-DATOS-EXTRA-FILE-OK
      * Ingresamos el ID de un ampleado
               DISPLAY 'INGRESA UN ID-EMPLEADO:'
               ACCEPT WS-ID-EMPLEADO
               DISPLAY " "
      * Buscamos al empleado, si el estado es A lo grabamos en un
      * archivo indexado VSAM junto a los datos extra si existen
               PERFORM 2000-BUSCAR-EMPLEADO
                  THRU 2000-BUSCAR-EMPLEADO-FIN
           END-IF.

           PERFORM 2400-GRABAR-DATOS
               THRU 2400-GRABAR-DATOS-EXIT

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

            STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.
      * Abro los archivos archivo de ENT-EMPLEADOS, SAL-EMPLEADOS y
      * DATOS-EXTRA
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

           OPEN INPUT ENT-DATOS-EXTRA.
      * Chequeo si la apertura DATOS-EXTRA fue exitosa
           EVALUATE TRUE
               WHEN FS-DATOS-EXTRA-FILE-OK
                    CONTINUE
               WHEN FS-DATOS-EXTRA-FILE-NFD
                 DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-DATOS-EXTRA
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
           MOVE WS-ID-EMPLEADO TO ENT-EMP-ID-EMPLEADO.

           PERFORM 2100-LEER-EMPLEADOS
              THRU 2100-LEER-EMPLEADOS-FIN.

       2000-BUSCAR-EMPLEADO-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2100-LEER-EMPLEADOS.
           READ ENT-EMPLEADOS KEY IS ENT-EMP-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-FILE-OK
      * Hay un empleado con el id buscado
                    PERFORM 2200-MOSTRAR-DATOS
                       THRU 2200-MOSTRAR-DATOS-FIN
      * Si el estado del empleado es A  busco si hay datos extra para
      *  grabar en el archivo de salida
                IF  ENT-EMP-ESTADO EQUAL "A"
                    PERFORM 2300-PROCESAR-DATOS-EXTRA
                       THRU 2300-PROCESAR-DATOS-EXTRA-EXIT
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
       2150-LEER-DATOS-EXTRA.
           READ ENT-DATOS-EXTRA KEY IS ENT-EXT-ID-EMPLEADO.

           EVALUATE TRUE
               WHEN FS-DATOS-EXTRA-FILE-OK
      * Se encontró un registro con datos extra
                   CONTINUE
               WHEN FS-DATOS-EXTRA-CLAVE-INV
                   DISPLAY "ERROR: EL ID DATOS-EXTRA ES INVALIDO"

               WHEN FS-DATOS-EXTRA-CLAVE-DUP
                   DISPLAY "ERROR: EL ID DATOS-EXTRA SE ENCUENTRA "-
                           "DUPLICADO"

               WHEN FS-DATOS-EXTRA-CLAVE-NFD
                   DISPLAY "ERROR: EL ID DATOS-EXTRA NO EXISTE"

               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE DATOS-EXTRA'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.


       2150-LEER-DATOS-EXTRA-EXIT.
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

      * DEBUGGING
           DISPLAY "SAL-EMPLEADOS-REG: "SAL-EMPLEADOS-REG.
       2200-MOSTRAR-DATOS-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2300-PROCESAR-DATOS-EXTRA.
      * Guardo el ID del empleado en el indice de DATOS-EXTRA
           MOVE WS-ID-EMPLEADO TO ENT-EXT-ID-EMPLEADO.
      * Leo el registro de datos extra
           PERFORM 2150-LEER-DATOS-EXTRA
              THRU 2150-LEER-DATOS-EXTRA-EXIT

      * Muevo los datos extra al archivo de salida
           MOVE ENT-EXT-TIPO-DOC  TO SAL-EMP-TIPO-DOC.
           MOVE ENT-EXT-NRO-DOC   TO SAL-EMP-NRO-DOC.
           MOVE ENT-EXT-TELEFONO  TO SAL-EMP-TELEFONO.

       2300-PROCESAR-DATOS-EXTRA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2400-GRABAR-DATOS.
      *    Muevo el registro de entrada y de datos extras al de salida
           MOVE ENT-EMP-ID-EMPLEADO TO SAL-EMP-ID-EMPLEADO
           MOVE ENT-EMP-APELLIDO    TO SAL-EMP-APELLIDO
           MOVE ENT-EMP-NOMBRE      TO SAL-EMP-NOMBRE
           MOVE ENT-EMP-ESTADO      TO SAL-EMP-ESTADO
           MOVE ENT-EMP-DIRECCION   TO SAL-EMP-DIRECCION
           MOVE ENT-EMP-COD-POSTAL  TO SAL-EMP-COD-POSTAL
           MOVE ENT-EXT-TIPO-DOC    TO SAL-EMP-TIPO-DOC
           MOVE ENT-EXT-NRO-DOC     TO SAL-EMP-NRO-DOC
           MOVE ENT-EXT-TELEFONO    TO SAL-EMP-TELEFONO.

      *    Grabo el registro de salida
           WRITE SAL-EMPLEADOS-REG.
           IF NOT FS-SAL-EMPLEADOS-FILE-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO SAL-EMPLEADOS: '
            FS-SAL-EMPLEADOS
           END-IF.

       2400-GRABAR-DATOS-EXIT.
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

           CLOSE ENT-DATOS-EXTRA.
           IF NOT FS-DATOS-EXTRA-FILE-OK
            DISPLAY 'ERROR AL CERRAR ARCHIVO DATOS EXTRA: '
            FS-DATOS-EXTRA
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.

      *----------------------------------------------------------------*

       END PROGRAM CL20EJ01.
