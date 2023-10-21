      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 25/09/2023
      * Purpose: CLASE 18 - EJERCICIO 3
      * DESCRIPCION: ORDENAR ARCHIVOS DE EMPLEADOS POR ESTADO.
      * EN EL CL18EJ03 CONTAMOS CUANTOS EMPLEADOS HAY POR ESTADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL15EJ03.
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

      *----------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      * El achivo de texto de empleados
       FD ENT-EMPLEADOS.
       01 REG-ENT-EMPLEADOS.
          05 ENT-EMP-ID-EMPLEADO         PIC 9(08).
          05 ENT-EMP-NOMBRE              PIC X(25).
          05 ENT-EMP-APELLIDO            PIC X(25).
          05 ENT-EMP-ESTADO              PIC X(01).


       WORKING-STORAGE SECTION.
       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-OK                 VALUE '00'.
             88 FS-EMPLEADOS-EOF                VALUE '10'.
             88 FS-EMPLEADOS-NFD                VALUE '35'.

       01 WS-CONTADORES.
           05 WS-CONT-REG-EMPLEADOS          PIC 9(06) VALUE 0.
           05 WS-CONT-REG-SALIDA             PIC 9(06) VALUE 0.
           05 WS-FORMAT-SALIDA               PIC ZZZ.ZZ9.

      * Arreglo para contar estados
      *  WS-DATO-ESTADO(1) = A, WS-DATO-ESTADO(2) = B  ....
       01 WS-ESTADO.
          05 WS-DATO-ESTADO PIC 9(06) OCCURS 6 TIMES.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-INICIAR
              THRU 1000-INICIAR-EXIT.

      *---- AHORA VAMOS A CONTAR LOS ESTADOS
           PERFORM 2200-CONTAR-ESTADOS
              THRU 2200-CONTAR-ESTADOS-EXIT
              UNTIL FS-EMPLEADOS-EOF

           PERFORM 3000-FINALIZAR
              THRU 3000-FINALIZAR-EXIT.

           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-EXIT.

       1000-INICIAR-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1100-ABRIR-EMPLEADOS.
           OPEN INPUT ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
      * Al abrir el archivo de empleados ya leo el primer registro.
                    PERFORM 1110-LEER-EMPLEADOS
                       THRU 1110-LEER-EMPLEADOS-EXIT
               WHEN FS-EMPLEADOS-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1100-ABRIR-EMPLEADOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       1110-LEER-EMPLEADOS.

           READ ENT-EMPLEADOS.

           EVALUATE TRUE
               WHEN FS-EMPLEADOS-OK
      * Cada vez que la lectura de un empleado fue exitosa agrego 1
      * a la cantidad de empleados leídos
                    ADD 1 TO WS-CONT-REG-EMPLEADOS
               WHEN FS-EMPLEADOS-EOF
      * Pongo un número alto en el ID de empleado si leí el último registro
                   MOVE 99999999           TO ENT-EMP-ID-EMPLEADO
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE EMPLEADOS'
                    DISPLAY 'FILE STATUS: ' FS-EMPLEADOS
           END-EVALUATE.

       1110-LEER-EMPLEADOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2200-CONTAR-ESTADOS.

           EVALUATE TRUE
               WHEN ENT-EMP-ESTADO EQUAL "A"
                   ADD 1 TO WS-DATO-ESTADO(1)

               WHEN ENT-EMP-ESTADO EQUAL "B"
                   ADD 1 TO WS-DATO-ESTADO(2)

               WHEN ENT-EMP-ESTADO EQUAL "C"
                   ADD 1 TO WS-DATO-ESTADO(3)

               WHEN ENT-EMP-ESTADO EQUAL "D"
                   ADD 1 TO WS-DATO-ESTADO(4)

               WHEN ENT-EMP-ESTADO EQUAL "E"
                   ADD 1 TO WS-DATO-ESTADO(5)

           END-EVALUATE.

           PERFORM 1110-LEER-EMPLEADOS
              THRU 1110-LEER-EMPLEADOS-EXIT.

       2200-CONTAR-ESTADOS-EXIT.
           EXIT.


      *----------------------------------------------------------------*
       3000-FINALIZAR.

           MOVE WS-DATO-ESTADO(1)       TO WS-FORMAT-SALIDA.
           DISPLAY 'CANTIDAD DE EMPLEADOS ESTADO A  : '
                   WS-FORMAT-SALIDA.

           MOVE WS-DATO-ESTADO(2)       TO WS-FORMAT-SALIDA.
           DISPLAY 'CANTIDAD DE EMPLEADOS ESTADO B  : '
                   WS-FORMAT-SALIDA.

           MOVE WS-DATO-ESTADO(3)       TO WS-FORMAT-SALIDA.
           DISPLAY 'CANTIDAD DE EMPLEADOS ESTADO C  : '
                   WS-FORMAT-SALIDA.

           MOVE WS-DATO-ESTADO(4)       TO WS-FORMAT-SALIDA.
           DISPLAY 'CANTIDAD DE EMPLEADOS ESTADO D  : '
                   WS-FORMAT-SALIDA.

           MOVE WS-DATO-ESTADO(5)       TO WS-FORMAT-SALIDA.
           DISPLAY 'CANTIDAD DE EMPLEADOS ESTADO E  : '
                   WS-FORMAT-SALIDA.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL15EJ03.
