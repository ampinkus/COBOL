      ******************************************************************
      * Author: Alfredo Pinkus
      * Date: 25/09/2023
      * Purpose: CLASE 18 - EJERCICIO 2
      * DESCRIPCION: ORDENAR ARCHIVOS DE EMPLEADOS POR ESTADO.
      * EN EL CL18EJ03 CONTAMOS CUANTOS EMPLEADOS HAY POR ESTADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL15EJ02.
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


       SELECT SAL-SALIDA
           ASSIGN TO '../SALIDA.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA.

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

      * El archivo de texto de salida
       FD SAL-SALIDA.
       01 REG-SALIDA                        PIC X(59).


       WORKING-STORAGE SECTION.
       01 FS-STATUS.
          05 FS-EMPLEADOS                   PIC X(2).
             88 FS-EMPLEADOS-OK                 VALUE '00'.
             88 FS-EMPLEADOS-EOF                VALUE '10'.
             88 FS-EMPLEADOS-NFD                VALUE '35'.
          05 FS-SALIDA                      PIC X(2).
             88 FS-SALIDA-OK                    VALUE '00'.
             88 FS-SALIDA-EOF                   VALUE '10'.
             88 FS-SALIDA-NFD                   VALUE '35'.

       01 WS-CONTADORES.
           05 WS-CONT-REG-EMPLEADOS          PIC 9(06) VALUE 0.
           05 WS-CONT-REG-SALIDA             PIC 9(06) VALUE 0.

       01 WS-VARIABLES-GENERALES.
           05 WS-IMP-ACUM                    PIC 9(10)V9(02) VALUE 0.
           05 WS-FORMAT-IMPORTE              PIC ZZZ.ZZ9.
           05 WS-I                           PIC 9(2) VALUE 0.
           05 WS-J                           PIC 9(2) VALUE 0.
           05 WS-VAR-AUXILIAR                PIC 9.

           05 WS-VALIDAR-ORDEN               PIC X(2).
               88 WS-ORDENADO-SI                      VALUE 'SI'.
               88 WS-ORDENADO-NO                      VALUE 'NO'.

      * El listado para probar el algoritmo de sorting
           05 WS-LISTA.
               10 WS-ITEM OCCURS 10 TIMES.
                  15 WS-ITEM-VALOR PIC 9.

           05 WS-II                          PIC 9(4) VALUE 0.
           05 WS-JJ                          PIC 9(4) VALUE 0.
      * Variable para el intercambio de registros de empleados
           05 WS-VAR-AUX2                    PIC X(59).

      * Variable de la lista de empleados
           05 WS-LISTA-EMP.
               10 WS-ITEM OCCURS 1217 TIMES.
                  15 WS-ITEM-EMP.
      * Separamos el ESTADO del resto
                     20 WS-ITEM-RESTO        PIC X(58).
                     20 WS-ITEM-ESTADO       PIC X(1).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
           PERFORM 1000-INICIAR
              THRU 1000-INICIAR-EXIT.

      *---- AHORA VAMOS A ORDENAR EL ARCHIVO DE ENTRADA.
           PERFORM 2200-PROCESAR-ARCHIVO
              THRU 2200-PROCESAR-ARCHIVO-EXIT.

           PERFORM 3000-FINALIZAR
              THRU 3000-FINALIZAR-EXIT.

           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-EMPLEADOS
              THRU 1100-ABRIR-EMPLEADOS-EXIT.

           PERFORM 1200-ABRIR-SALIDA
              THRU 1200-ABRIR-SALIDA-EXIT.

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
       1200-ABRIR-SALIDA.
           OPEN OUTPUT  SAL-SALIDA.

           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                    CONTINUE
               WHEN FS-SALIDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       1200-ABRIR-SALIDA-EXIT.
           EXIT.


      *----------------------------------------------------------------*
       2200-PROCESAR-ARCHIVO.
      *---- LEO TODO EL ARCHIVO Y LO GUARDO EN WS-ITEM-EMP A FIN DE
      * SEPARAR EL ESTADO DEL RESTO DEL REGISTRO
      * Ahora tengo el ESTADO en WS-ITEM-ESTADO que es el que uso
      * para ordenar
           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL FS-EMPLEADOS-EOF
              MOVE REG-ENT-EMPLEADOS TO WS-ITEM-EMP(WS-II)

              PERFORM 1110-LEER-EMPLEADOS
                 THRU 1110-LEER-EMPLEADOS-EXIT

           END-PERFORM.

      *----REALIZO EL PROCEDIMIENTO DE ORDENAMIENTO DE LA LISTA EMPLEADO
      *----SETEAR EN NO ORDENADO PARA QUE ENTRE EN EL BUCLE
           MOVE 'NO' TO WS-VALIDAR-ORDEN.

      *----VOY A REPETIR HASTA QUE ESTÈ ORDENADA LA LISTA EMPLEADOS
      *----USAR VARIABLES WS-II PARA INDICE Y WS-VAR-AUX2 COMO AUXILIAR

      *----VOY A REPETIR HASTA QUE ESTÈ ORDENADA LA LISTA
      * Recorro la lista hasta que llego al último elemento o hasta
      * que no hago mas intercambios de elementos
           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL WS-II > 1217
                   OR  WS-ORDENADO-SI
      *----PARA UNA PASADA ASUMO QUE ESTA ORDENADA
              MOVE 'SI' TO WS-VALIDAR-ORDEN
      * Recorro la lista hasta: (ultimo elemento - WS-I)
      * en la primer pasada de 1 a 9, en la segunda de 1 a 8....
              PERFORM VARYING WS-JJ FROM 1 BY 1 UNTIL WS-JJ >
                         (1217 - WS-II)
      *----SI EN UNA PASADA COMPLETA NO ENTRA EN EL IF, ESTA ORDENADA
                IF WS-ITEM-ESTADO(WS-JJ) >  WS-ITEM-ESTADO(WS-JJ + 1)
      *---- AL DETECTAR UN DESORDEN SETEO EN NO, PORQUE SEGURO TENGO
      *---- QUE HACER OTRA PASADA.
                   MOVE 'NO' TO WS-VALIDAR-ORDEN
      * Intercambio los datos del indice (WS-JJ + 1) y (WS-JJ)
                   MOVE WS-ITEM-EMP(WS-JJ) TO WS-VAR-AUX2
                   MOVE WS-ITEM-EMP(WS-JJ + 1) TO WS-ITEM-EMP(WS-JJ)
                   MOVE WS-VAR-AUX2 TO WS-ITEM-EMP(WS-JJ + 1)
                END-IF
              END-PERFORM
           END-PERFORM.

      *---- EL SIGUIENTE PARRAFO LEE TODO EL OCCURS DE EMPLEADOS Y LO
      *---- GRABA EN EL ARCHIVO DE SALIDA.
           PERFORM 2300-MOVER-SALIDA
              THRU 2300-MOVER-SALIDA-EXIT.

       2200-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
       2300-MOVER-SALIDA.

           PERFORM VARYING WS-II FROM 1 BY 1 UNTIL WS-II >
                                                 WS-CONT-REG-EMPLEADOS

              MOVE WS-ITEM-EMP (WS-II) TO REG-SALIDA

              PERFORM 2400-GRABAR-SALIDA
                 THRU 2400-GRABAR-SALIDA-EXIT

           END-PERFORM.
       2300-MOVER-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2400-GRABAR-SALIDA.

           WRITE REG-SALIDA.

           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                    ADD 1 TO WS-CONT-REG-SALIDA
               WHEN FS-SALIDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       2400-GRABAR-SALIDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR.

           MOVE WS-CONT-REG-EMPLEADOS       TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS EMPLEADOS   : '
                   WS-FORMAT-IMPORTE.

           MOVE WS-CONT-REG-SALIDA          TO WS-FORMAT-IMPORTE.
           DISPLAY 'CANTIDAD DE REGISTROS SALIDA      : '
                   WS-FORMAT-IMPORTE.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3000-FINALIZAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE ENT-EMPLEADOS
                 SAL-SALIDA.

           IF NOT FS-EMPLEADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO EMPLEADOS: ' FS-EMPLEADOS
           END-IF.

           IF NOT FS-SALIDA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO SALIDA: ' FS-SALIDA
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL15EJ02.
