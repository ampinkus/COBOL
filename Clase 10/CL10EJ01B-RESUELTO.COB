      *----------------------------------------------------------------*
      * Purpose: CLASE 10 - EJERCICIO 1B
      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.

       PROGRAM-ID. CL10EJ01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

      ****ARCHIVO DE ENTRADA
      ******* Nombre logico del archivo: ENTRADA
       FILE-CONTROL.
      *----------------------------------------------------------------*
      ****ARCHIVO DE ENTRADA
      *       Nombre logico del archivo: ENTRADA
       SELECT ENTRADA
      *    Indicamos donde esta alocado el archivo de entrada
           ASSIGN TO 'E:\COBOL\SUELDOS.TXT'
      *    Indicamos tiop de organizacion del  archivo de entrada
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.

      *----------------------------------------------------------------*
      ****ARCHIVO DE SALIDA
      ******** Nombre logico del archivo: SALIDA
       SELECT SALIDA
           ASSIGN TO 'E:\COBOL\AGUINALDOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA.

      *----------------------------------------------------------------*

       DATA DIVISION.
       FILE SECTION.
      *----------------------------------------------------------------*
      ***** Declaramos la estructura de dato del archivo de entrada
       FD ENTRADA.
       01 ENT-ARCHIVOS.
          05 ENT-ID-EMPLEADO                PIC 9(05).
          05 ENT-NOMBRE                     PIC X(15).
          05 ENT-APELLIDO                   PIC X(15).
          05 ENT-DIAS-TRABAJADO             PIC 9(02).
          05 ENT-SUELDO                     PIC 9(08)V9(02).

      *----------------------------------------------------------------*
      ***** Declaramos la estructura de dato del archivo de salida
       FD SALIDA.
       01 SAL-ARCHIVOS.
          05 SAL-ID-EMPLEADO                PIC 9(05).
          05 SAL-NOMBRE                     PIC X(15).
          05 SAL-APELLIDO                   PIC X(15).
          05 SAL-DIAS-TRABAJADO             PIC 9(02).
          05 SAL-SUELDO                     PIC 9(08)V9(02).
          05 SAL-AGUINALDO                  PIC 9(08)V9(02).

      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      ***** Declaramos la variables de nuestro programa COBOL
      *----------------------------------------------------------------*
      *    Varaibles FILE STATUS  ENTRADA/SALIDA                        *
      *----------------------------------------------------------------*
       01 FS-STATUS.
      *----------------------------------------------------------------*
      *   ** FILE STATUT DE ENTRADA                                    *
      *----------------------------------------------------------------*
          05 FS-ENTRADA                      PIC X(02).
             88 FS-ENTRADA-OK                VALUE '00'.
             88 FS-ENTRADA-EOF               VALUE '10'.
      *----------------------------------------------------------------*
      *   ** FILE STATUT DE SALIDA                                     *
      *----------------------------------------------------------------*
          05 FS-SALIDA                       PIC X(02).
             88 FS-SALIDA-OK                 VALUE '00'.
             88 FS-SALIDA-EOF                VALUE '10'.

      *----------------------------------------------------------------*
      *    Declaracion de variables
      *----------------------------------------------------------------*
       01 WS-VAR-AUX.
          05 WS-ENTRADA-CANT-REG             PIC 9(05)      VALUE 0.
          05 WS-SALIDA-CANT-REG              PIC 9(05)      VALUE 0.

      *   Variable para acumular sueldo
          05 WS-SUELDO-TOTAL-N          PIC 9(08)V9(02) VALUE 0.
          05 WS-SUELDO-TOTAL-Z          PIC $ZZ.ZZZ.ZZ9,99.

      *   Variable para acumular aguinaldo
          05 WS-TOT-AGUINALDOS-N             PIC 9(08)v99  VALUE 0.
          05 WS-TOT-AGUINALDOS-Z             PIC $ZZ.ZZZ.ZZ9,99.

      *   Variable para calcular promedio de sueldo
          05 WS-SUELDO-PROM-N                PIC 9(08)v99  VALUE 0.
          05 WS-SUELDO-PROM-Z                PIC $ZZ.ZZZ.ZZ9,99.

      *   Variable para guardar sueldo mayor
          05 WS-SUELDO-MAYOR-N               PIC 9(08)v99  VALUE 0.
          05 WS-SUELDO-MAYOR-Z               PIC $ZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

           PERFORM 1000-ABRIR-ARCHIVOS
              THRU 1000-ABRIR-ARCHIVOS-EXIT.

           PERFORM 2000-PROCESAR-ARCHIVO
              THRU 2000-PROCESAR-ARCHIVO-EXIT
             UNTIL FS-ENTRADA-EOF

           PERFORM 3000-CERRAR-ARCHIVOS
              THRU 3000-CERRAR-ARCHIVOS-EXIT

      *    Impimir totales
           PERFORM 4000-TOTALES-ARCHIVO
              THRU 4000-TOTALES-ARCHIVO-EXIT

           STOP RUN.

      *----------------------------------------------------------------*
      *    PROCESO PARA ABRIR ARCHIVO DE ENTRADA Y SALIDA              *
      *----------------------------------------------------------------*
       1000-ABRIR-ARCHIVOS.

      *    ABRIR ARCHIVO DE ENTRADA
           PERFORM 1100-ABRIR-ARCHIVO-ENTRADA
              THRU 1100-ABRIR-ARCHIVO-ENTRADA-EXIT.

      *    ABRIR ARCHIVO DE SALIDA
           PERFORM 1200-ABRIR-ARCHIVO-SALIDA
              THRU 1200-ABRIR-ARCHIVO-SALIDA-EXIT.

       1000-ABRIR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA ABRIR ARCHIVO DE ENTRADA                       *
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO-ENTRADA.

      *    Abro archivo  de entrada  : ENTRADA
           OPEN INPUT ENTRADA.

           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                    PERFORM 1500-LEER-ARCHIVO
                       THRU 1500-LEER-ARCHIVO-EXIT
               WHEN FS-ENTRADA-EOF
                    DISPLAY 'EL ARCHIVO DE ENTRADA ESTA VACIO'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                    DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-ENTRADA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA ABRIR ARCHIVO DE SALIDA                        *
      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-SALIDA.

      *    Abro archivo  de salida  : SALIDA
           OPEN OUTPUT SALIDA.

           EVALUATE TRUE
               WHEN FS-SALIDA-OK
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       1200-ABRIR-ARCHIVO-SALIDA-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA LEER ARCHIVO DE ENTRADA                        *
      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.

           READ ENTRADA
             AT END
                 CONTINUE
             NOT AT END
      *         CONTADOR DE REGISTROS LEIDOS
                ADD 1                      TO WS-ENTRADA-CANT-REG
           END-READ.

       1500-LEER-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA LEER ARCHIVO DE SALIDA                         *
      *----------------------------------------------------------------*
       2000-PROCESAR-ARCHIVO.

           IF ENT-SUELDO IS NUMERIC
      *       La variable sueldo  es numerica
      *----  Muevo variables del registro de entrada al regstro de salida
             PERFORM 2200-MOVER-DATOS-SAL
                THRU 2200-MOVER-DATOS-SAL-EXIT

      *----  LLlamar al parrafo para Calculo totales
             PERFORM 2300-PROCESO-TOTALES
                THRU 2300-PROCESO-TOTALES-EXIT

      *----  Grabar registro de salida
             PERFORM 2500-GRABAR-ARCHIVO-SAL
                THRU 2500-GRABAR-ARCHIVO-SAL-EXIT

           END-IF.

           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

       2000-PROCESAR-ARCHIVO-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    MUEVO DATOS DE LA ENTRADA A LA SALIDA                       *
      *----------------------------------------------------------------*
       2200-MOVER-DATOS-SAL.
      *-----MUEVO CAMPOS AL ARCHIVO DE SALIDA Y CALCULO SAL-AGUINALDO

      *    Muevo la variable ENT-ID-EMPLEADO     a la Salida
           MOVE ENT-ID-EMPLEADO      TO SAL-ID-EMPLEADO

      *    Muevo la variable ENT-NOMBRE          a la Salida
           MOVE ENT-NOMBRE           TO SAL-NOMBRE

      *    Muevo la variable ENT-APELLIDO        a la Salida
           MOVE ENT-APELLIDO         TO SAL-APELLIDO

      *    Muevo la variable ENT-DIAS-TRABAJADO  a la Salida
           MOVE ENT-DIAS-TRABAJADO   TO SAL-DIAS-TRABAJADO

      *    Muevo la variable ENT-SUELDO          a la Salida
           MOVE ENT-SUELDO           TO SAL-SUELDO

      *    Calcular aguinaldo  SAL-AGUINALDO = ENT-SUELDO /2
      *    Guardo la variable aguinaldo en el registro Aguinaldo de salida
           DIVIDE 2 INTO  SAL-SUELDO GIVING SAL-AGUINALDO

           .
       2200-MOVER-DATOS-SAL-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESAR TOTALES                                            *
      *----------------------------------------------------------------*
       2300-PROCESO-TOTALES.

      *----  ACUMULO IMPORTE SUELDO
           ADD ENT-SUELDO TO WS-SUELDO-TOTAL-N

      *----  ME QUEDO CON EL MAYOR SUELDO
           IF ENT-SUELDO > WS-SUELDO-MAYOR-N
              MOVE ENT-SUELDO TO WS-SUELDO-MAYOR-N
           END-IF

      *----  ACUMULO IMPORTE AGUINALDO
           ADD SAL-AGUINALDO         TO WS-TOT-AGUINALDOS-N.

       2300-PROCESO-TOTALES-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    PROCESO PARA ESCRIBIR  ARCHIVO DE SALIDA                    *
      *----------------------------------------------------------------*
       2500-GRABAR-ARCHIVO-SAL.

           WRITE SAL-ARCHIVOS.

           EVALUATE FS-SALIDA
               WHEN '00'
                    ADD 1 TO WS-SALIDA-CANT-REG
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.

       2500-GRABAR-ARCHIVO-SAL-EXIT.
           EXIT.


      *----------------------------------------------------------------*
      *    PROCESO PARA CERRRAR ARCHIVO DE ENTRADA Y SALIDA            *
      *----------------------------------------------------------------*

       3000-CERRAR-ARCHIVOS.
      *    Cerrar archivo de entrada
           PERFORM 3100-CERRAR-ARCHIVO-ENT
              THRU 3100-CERRAR-ARCHIVO-ENT-EXIT.

      *    Cerrar archivo de salida
           PERFORM 3200-CERRAR-ARCHIVO-SAL
              THRU 3200-CERRAR-ARCHIVO-SAL-EXIT.

       3000-CERRAR-ARCHIVOS-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA CERRRAR ARCHIVO DE ENTRADA                     *
      *----------------------------------------------------------------*
       3100-CERRAR-ARCHIVO-ENT.

           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR EN CLOSE DE ENTRADA: ' FS-ENTRADA
           END-IF.

       3100-CERRAR-ARCHIVO-ENT-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      *    PROCESO PARA CERRRAR ARCHIVO DE SALIDA                      *
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO-SAL.

           CLOSE SALIDA.

           IF NOT FS-SALIDA-OK
              DISPLAY 'ERROR EN CLOSE DE SALIDA: ' FS-SALIDA
           END-IF.

       3200-CERRAR-ARCHIVO-SAL-EXIT.
           EXIT.


      *----------------------------------------------------------------*
      *    PROCESO PARA MOSTRAR ESTADISTICAS DEL PROCESO               *
      *----------------------------------------------------------------*
       4000-TOTALES-ARCHIVO.

           DISPLAY '================================================'.

      *    Muestro Sueldo acumulado
           MOVE WS-SUELDO-TOTAL-N      TO  WS-SUELDO-TOTAL-Z.
           DISPLAY 'TOTAL SUELDOS            : '  WS-SUELDO-TOTAL-Z.

      *    Muestro Aguinaldo acumulado
           MOVE WS-TOT-AGUINALDOS-N          TO  WS-TOT-AGUINALDOS-Z.
           DISPLAY 'TOTAL AGUINALDOS         : ' WS-TOT-AGUINALDOS-Z.

      *    Calculo Sueldo Promedio   SUELDO-TOTAL  / CANT-REG PROCESADO
           DIVIDE WS-SALIDA-CANT-REG  INTO WS-SUELDO-TOTAL-N
                  GIVING WS-SUELDO-PROM-N.

      *    Muestro Sueldo promedio
           MOVE WS-SUELDO-PROM-N             TO  WS-SUELDO-PROM-Z.
           DISPLAY 'SUELDO PROMEDIO          : ' WS-SUELDO-PROM-Z.

      *    Muestro Sueldo mayor
           MOVE WS-SUELDO-MAYOR-N            TO  WS-SUELDO-MAYOR-Z.
           DISPLAY 'SUELDO MAS ALTO          : ' WS-SUELDO-MAYOR-Z.

      *    Muestro Cantidad de registros procesados
           DISPLAY '================================================'.
           DISPLAY 'CANT. DE REGISTROS LEIDOS: ' WS-ENTRADA-CANT-REG.
           DISPLAY 'CANT. DE REG. GRABADOS   : ' WS-SALIDA-CANT-REG.
           DISPLAY '================================================'.
       4000-TOTALES-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       END PROGRAM CL10EJ01.
